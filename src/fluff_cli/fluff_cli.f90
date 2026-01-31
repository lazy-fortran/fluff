module fluff_cli
    ! Command-line interface
    use fluff_core, only: fluff_version_t, get_fluff_version
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use fluff_formatter, only: create_formatter_engine, formatter_engine_t
    use fluff_config, only: create_default_config, fluff_config_t
    use fluff_diagnostics, only: diagnostic_t
    use fluff_json, only: json_array_get_element_json, json_escape_string, &
                          json_get_int_member, json_get_member_json, &
                          json_get_string_member, json_get_string_value, json_parse
    use fluff_json_rpc, only: create_json_error_response, create_json_response, &
                              parse_lsp_message
    implicit none
    private

    ! CLI argument types
    type, public :: cli_args_t
        character(len=:), allocatable :: command     ! check, format, server
        character(len=:), allocatable :: files(:)    ! Files to process
        logical :: fix = .false.
        logical :: diff = .false.
        logical :: watch = .false.
        logical :: version = .false.
        logical :: help = .false.
        logical :: help_requested = .false.         ! Alias for help
        logical :: version_requested = .false.       ! Alias for version
        logical :: show_fixes = .false.
        logical :: quiet = .false.
        logical :: verbose = .false.
        character(len=:), allocatable :: config_file
        character(len=:), allocatable :: output_format  ! text, json, sarif
        character(len=:), allocatable :: error_msg    ! Parse error message
    contains
        procedure :: parse => args_parse
        procedure :: validate => args_validate
    end type cli_args_t

    ! CLI application
    type, public :: cli_app_t
        type(cli_args_t) :: args
        type(linter_engine_t) :: linter
        type(formatter_engine_t) :: formatter
    contains
        procedure :: run => app_run
        procedure :: print_help => app_print_help
        procedure :: print_version => app_print_version
    end type cli_app_t

    ! Public procedures
    public :: create_cli_app

contains

    ! Create CLI application
    function create_cli_app() result(app)
        type(cli_app_t) :: app

        app%linter = create_linter_engine()
        app%formatter = create_formatter_engine()

    end function create_cli_app

    ! Parse command line arguments
    subroutine args_parse(this, argc, argv)
        class(cli_args_t), intent(inout) :: this
        integer, intent(in) :: argc
        character(len=*), intent(in) :: argv(:)

        integer :: i
        logical :: expecting_value
        character(len=:), allocatable :: current_flag

        ! Initialize
        this%command = "check"  ! Default command
        this%error_msg = ""
        expecting_value = .false.
        i = 1

        do while (i <= argc)
            if (expecting_value) then
                ! Handle flag values
                select case (current_flag)
                case ("--config")
                    this%config_file = trim(argv(i))
                case ("--output-format")
                    this%output_format = trim(argv(i))
                end select
                expecting_value = .false.

            else if (len_trim(argv(i)) >= 2 .and. argv(i) (1:2) == "--") then
                ! Long flags
                select case (trim(argv(i)))
                case ("--fix")
                    this%fix = .true.
                case ("--diff")
                    this%diff = .true.
                case ("--watch")
                    this%watch = .true.
                case ("--version")
                    this%version = .true.
                    this%version_requested = .true.
                case ("--help")
                    this%help = .true.
                    this%help_requested = .true.
                case ("--show-fixes")
                    this%show_fixes = .true.
                case ("--quiet")
                    this%quiet = .true.
                case ("--verbose")
                    this%verbose = .true.
                case ("--config")
                    current_flag = "--config"
                    expecting_value = .true.
                case ("--output-format")
                    current_flag = "--output-format"
                    expecting_value = .true.
                end select

            else if (len_trim(argv(i)) >= 1 .and. argv(i) (1:1) == "-") then
                ! Short flags
                select case (trim(argv(i)))
                case ("-h")
                    this%help = .true.
                    this%help_requested = .true.
                case ("-v")
                    this%version = .true.
                    this%version_requested = .true.
                case ("-q")
                    this%quiet = .true.
                end select

            else if (i == 1) then
                ! First non-flag argument is the command
                select case (trim(argv(i)))
                case ("check", "format", "server")
                    this%command = trim(argv(i))
                case default
                    ! If not a valid command, treat as file
                    call add_file_to_list(this%files, trim(argv(i)))
                end select

            else
                ! File arguments
                call add_file_to_list(this%files, trim(argv(i)))
            end if

            i = i + 1
        end do

    end subroutine args_parse

    ! Helper to add file to list
    subroutine add_file_to_list(files, new_file)
        character(len=:), allocatable, intent(inout) :: files(:)
        character(len=*), intent(in) :: new_file

        character(len=:), allocatable :: temp(:)
        integer :: n

        if (.not. allocated(files)) then
            allocate (character(len=len(new_file)) :: files(1))
            files(1) = new_file
        else
            n = size(files)
            allocate (character(len=max(len(files), len(new_file))) :: temp(n + 1))
            temp(1:n) = files
            temp(n + 1) = new_file
            call move_alloc(temp, files)
        end if

    end subroutine add_file_to_list

    ! Validate arguments
    function args_validate(this) result(valid)
        class(cli_args_t), intent(in) :: this
        logical :: valid

        valid = .true.

        ! Check if command is valid
        if (allocated(this%command)) then
            select case (this%command)
            case ("check", "format", "server")
                ! Valid commands
            case default
                valid = .false.
            end select
        end if

    end function args_validate

    ! Run the CLI application
    subroutine app_run(this, exit_code)
        class(cli_app_t), intent(inout) :: this
        integer, intent(out) :: exit_code

        exit_code = 0

        ! Handle help and version requests
        if (this%args%help .or. this%args%help_requested) then
            call this%print_help()
            return
        end if

        if (this%args%version .or. this%args%version_requested) then
            call this%print_version()
            return
        end if

        ! Validate arguments
        if (.not. this%args%validate()) then
            print *, "Error: Invalid arguments"
            exit_code = 1
            return
        end if

        ! Execute command
        select case (this%args%command)
        case ("check")
            call run_check_command(this, exit_code)
        case ("format")
            call run_format_command(this, exit_code)
        case ("server")
            call run_server_command(this, exit_code)
        case default
            print *, "Error: Unknown command '", this%args%command, "'"
            exit_code = 1
        end select

    end subroutine app_run

    ! Print help message
    subroutine app_print_help(this)
        class(cli_app_t), intent(in) :: this

        print *, "fluff - A linting and formatting tool for Fortran"
        print *, ""
        print *, "Usage: fluff [COMMAND] [OPTIONS] [FILES...]"
        print *, ""
        print *, "Commands:"
        print *, "  check    Check code for issues (default)"
        print *, "  format   Format code"
        print *, "  server   Run LSP server"
        print *, ""
        print *, "Options:"
        print *, "  -h, --help         Show this help message"
        print *, "  -v, --version      Show version"
        print *, "  --fix              Apply fixes automatically"
        print *, "  --diff             Show diffs instead of rewriting files"
        print *, "  --watch            Watch files for changes"
        print *, "  --config FILE      Use configuration file"
        print *, "  --output-format    Output format (text, json, sarif)"

    end subroutine app_print_help

    ! Print version
    subroutine app_print_version(this)
        class(cli_app_t), intent(in) :: this
        type(fluff_version_t) :: version

        version = get_fluff_version()
        print '(a,i0,".",i0,".",i0)', "fluff ", version%major, version%minor, &
            version%patch

    end subroutine app_print_version

    ! Run check command
    subroutine run_check_command(app, exit_code)
        use fluff_fix_applicator, only: apply_fixes_to_file
        type(cli_app_t), intent(inout) :: app
        integer, intent(out) :: exit_code

        type(fluff_config_t) :: config
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        integer :: i, fixes_applied

        exit_code = 0

        ! Load configuration
        if (allocated(app%args%config_file)) then
            call config%from_file(app%args%config_file)
        else
            config = create_default_config()
        end if

        ! Initialize linter
        call app%linter%initialize()
        call app%linter%set_config(config)

        ! Process files
        if (allocated(app%args%files)) then
            do i = 1, size(app%args%files)
                call app%linter%lint_file(app%args%files(i), diagnostics, error_msg)

                if (error_msg /= "") then
                    print *, "Error linting file: ", error_msg
                    exit_code = 1
                else if (allocated(diagnostics)) then
                    if (size(diagnostics) > 0) exit_code = 1

                    if (app%args%fix) then
                        call apply_fixes_to_file(app%args%files(i), diagnostics, &
                                                 fixes_applied, error_msg)
                        if (error_msg /= "") then
                            print *, "Error applying fixes: ", error_msg
                        else if (fixes_applied > 0 .and. .not. app%args%quiet) then
                            print '(A,I0,A,A)', "Applied ", fixes_applied, &
                                " fix(es) to ", trim(app%args%files(i))
                        end if
                    end if

                    call print_diagnostics(diagnostics, app%args%output_format)
                end if
            end do
        else
            print *, "No files specified"
            exit_code = 1
        end if

    end subroutine run_check_command

    ! Run format command
    subroutine run_format_command(app, exit_code)
        use fluff_fix_applicator, only: read_text_file, write_text_file
        type(cli_app_t), intent(inout) :: app
        integer, intent(out) :: exit_code

        character(len=:), allocatable :: formatted_code
        character(len=:), allocatable :: original_code
        character(len=:), allocatable :: error_msg
        integer :: i

        exit_code = 0

        ! Initialize formatter
        call app%formatter%initialize()

        ! Process files
        if (allocated(app%args%files)) then
            do i = 1, size(app%args%files)
                call app%formatter%format_file(app%args%files(i), &
                                               formatted_code, error_msg)

                if (error_msg /= "") then
                    print *, "Error formatting file: ", error_msg
                    exit_code = 1
                else if (app%args%diff) then
                    call read_text_file(app%args%files(i), original_code, error_msg)
                    if (error_msg /= "") then
                        print *, "Error reading file: ", error_msg
                        exit_code = 1
                    else
                        call print_unified_diff(app%args%files(i), original_code, &
                                                formatted_code)
                    end if
                else if (app%args%fix) then
                    call write_text_file(app%args%files(i), formatted_code, error_msg)
                    if (error_msg /= "") then
                        print *, "Error writing file: ", error_msg
                        exit_code = 1
                    end if
                else
                    print *, formatted_code
                end if
            end do
        else
            print *, "No files specified"
            exit_code = 1
        end if

    end subroutine run_format_command

    ! Run server command
    subroutine run_server_command(app, exit_code)
        use fluff_lsp_server, only: fluff_lsp_server_t
        use fluff_lsp_framing, only: lsp_read_framed_message, lsp_write_framed_message
        use iso_fortran_env, only: error_unit
        type(cli_app_t), intent(inout) :: app
        integer, intent(out) :: exit_code

        type(fluff_lsp_server_t) :: lsp_server
        character(len=:), allocatable :: outgoing
        character(len=:), allocatable :: message, error_msg
        character(len=:), allocatable :: message_type, method
        integer :: message_id
        logical :: success, server_running
        logical :: found_outgoing

        exit_code = 0
        server_running = .true.

        ! Initialize LSP server
        call lsp_server%initialize("")

        ! Main server loop - read from stdin, write to stdout
        do while (server_running)
            ! Read framed JSON-RPC message from stdin
            call lsp_read_framed_message(message, success, error_msg)

            if (.not. success) then
                ! End of input or read error - exit gracefully
                if (app%args%verbose) then
                    write (error_unit, '(A)') "Read error: "//error_msg
                end if
                server_running = .false.
                cycle
            end if

            ! Skip empty messages
            if (len_trim(message) == 0) cycle

            ! Parse JSON-RPC message
            call parse_lsp_message(message, message_type, message_id, &
                                   method, success)

            if (.not. success) then
                if (app%args%verbose) then
                    write (error_unit, '(A)') "Failed to parse message: "//message
                end if
                cycle
            end if

            ! Handle different message types
            select case (message_type)
            case ("request")
                call handle_lsp_request_framed(lsp_server, message_id, method, &
                                               message, success)
                if (method == "shutdown") then
                    server_running = .false.
                end if

            case ("notification")
                call handle_lsp_notification(lsp_server, method, message, success)
                call lsp_server%pop_notification(outgoing, found_outgoing)
                if (found_outgoing) call lsp_write_framed_message(outgoing)

            case default
                if (app%args%verbose) then
                    write (error_unit, '(A)') "Unknown message type: "//message_type
                end if
            end select
        end do

    end subroutine run_server_command

    ! Handle LSP requests
    subroutine handle_lsp_request(server, id, method, message, success)
        use fluff_lsp_server, only: fluff_lsp_server_t
        use iso_fortran_env, only: output_unit, error_unit
        type(fluff_lsp_server_t), intent(inout) :: server
        integer, intent(in) :: id
        character(len=*), intent(in) :: method, message
        logical, intent(out) :: success

        character(len=:), allocatable :: response, capabilities

        success = .true.

        select case (method)
        case ("initialize")
            ! Return server capabilities
            capabilities = server%get_server_capabilities()
            response = create_json_response(id, capabilities)
            write (output_unit, '(A)') response

        case ("shutdown")
            ! Acknowledge shutdown
            response = create_json_response(id, 'null')
            write (output_unit, '(A)') response

        case ("textDocument/formatting")
            call handle_formatting_request(server, id, message, success)

        case default
            ! Method not found error
            response = create_json_error_response(id, -32601, "Method not found")
            write (output_unit, '(A)') response
            write (error_unit, '(A)') "Unknown method: "//method
        end select

    end subroutine handle_lsp_request

    ! Handle LSP requests with Content-Length framing
    subroutine handle_lsp_request_framed(server, id, method, message, success)
        use fluff_lsp_server, only: fluff_lsp_server_t
        use fluff_lsp_framing, only: lsp_write_framed_message
        use iso_fortran_env, only: error_unit
        type(fluff_lsp_server_t), intent(inout) :: server
        integer, intent(in) :: id
        character(len=*), intent(in) :: method, message
        logical, intent(out) :: success

        character(len=:), allocatable :: response, capabilities

        success = .true.

        select case (method)
        case ("initialize")
            ! Return server capabilities
            capabilities = server%get_server_capabilities()
            response = create_json_response(id, capabilities)
            call lsp_write_framed_message(response)

        case ("shutdown")
            ! Acknowledge shutdown
            response = create_json_response(id, 'null')
            call lsp_write_framed_message(response)

        case ("textDocument/formatting")
            call handle_formatting_request_framed(server, id, message, success)

        case default
            ! Method not found error
            response = create_json_error_response(id, -32601, "Method not found")
            call lsp_write_framed_message(response)
            write (error_unit, '(A)') "Unknown method: "//method
        end select

    end subroutine handle_lsp_request_framed

    ! Handle LSP notifications
    subroutine handle_lsp_notification(server, method, message, success)
        use fluff_lsp_server, only: fluff_lsp_server_t
        use iso_fortran_env, only: error_unit
        type(fluff_lsp_server_t), intent(inout) :: server
        character(len=*), intent(in) :: method, message
        logical, intent(out) :: success

        success = .true.

        select case (method)
        case ("initialized")
            ! Server initialization complete
            write (error_unit, '(A)') "LSP server initialized"

        case ("textDocument/didOpen")
            call handle_did_open_notification(server, message, success)

        case ("textDocument/didChange")
            call handle_did_change_notification(server, message, success)

        case ("textDocument/didSave")
            call handle_did_save_notification(server, message, success)

        case ("textDocument/didClose")
            call handle_did_close_notification(server, message, success)

        case ("exit")
            ! Exit notification
            write (error_unit, '(A)') "Exit notification received"

        case default
            write (error_unit, '(A)') "Unknown notification: "//method
        end select

    end subroutine handle_lsp_notification

    subroutine handle_did_open_notification(server, message, success)
        use fluff_lsp_server, only: fluff_lsp_server_t
        type(fluff_lsp_server_t), intent(inout) :: server
        character(len=*), intent(in) :: message
        logical, intent(out) :: success
        character(len=:), allocatable :: err
        character(len=:), allocatable :: params_json, text_document_json
        character(len=:), allocatable :: uri, language_id, text
        integer :: version
        logical :: ok, found

        call json_parse(message, ok, err)
        if (.not. ok) then
            success = .false.
            return
        end if

        call json_get_member_json(message, "params", params_json, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_member_json(params_json, "textDocument", text_document_json, &
                                  found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_string_member(text_document_json, "uri", uri, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_string_member(text_document_json, "languageId", language_id, &
                                    found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_int_member(text_document_json, "version", version, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_string_member(text_document_json, "text", text, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call server%handle_text_document_did_open(uri, language_id, version, text, &
                                                  success)
    end subroutine handle_did_open_notification

    subroutine handle_did_change_notification(server, message, success)
        use fluff_lsp_server, only: fluff_lsp_server_t
        type(fluff_lsp_server_t), intent(inout) :: server
        character(len=*), intent(in) :: message
        logical, intent(out) :: success
        character(len=:), allocatable :: err
        character(len=:), allocatable :: params_json, text_document_json
        character(len=:), allocatable :: changes_json, change0_json
        character(len=:), allocatable :: uri, text
        integer :: version
        logical :: ok, found

        call json_parse(message, ok, err)
        if (.not. ok) then
            success = .false.
            return
        end if

        call json_get_member_json(message, "params", params_json, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_member_json(params_json, "textDocument", text_document_json, &
                                  found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_string_member(text_document_json, "uri", uri, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_int_member(text_document_json, "version", version, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_member_json(params_json, "contentChanges", changes_json, found, &
                                  ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_array_get_element_json(changes_json, 1, change0_json, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_string_member(change0_json, "text", text, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call server%handle_text_document_did_change(uri, version, text, success)
    end subroutine handle_did_change_notification

    subroutine handle_did_save_notification(server, message, success)
        use fluff_lsp_server, only: fluff_lsp_server_t
        type(fluff_lsp_server_t), intent(inout) :: server
        character(len=*), intent(in) :: message
        logical, intent(out) :: success

        character(len=:), allocatable :: err
        character(len=:), allocatable :: params_json, text_document_json
        character(len=:), allocatable :: uri
        logical :: ok, found

        call json_parse(message, ok, err)
        if (.not. ok) then
            success = .false.
            return
        end if

        call json_get_member_json(message, "params", params_json, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_member_json(params_json, "textDocument", text_document_json, &
                                  found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_string_member(text_document_json, "uri", uri, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call server%handle_text_document_did_save(uri, success)
    end subroutine handle_did_save_notification

    subroutine handle_did_close_notification(server, message, success)
        use fluff_lsp_server, only: fluff_lsp_server_t
        type(fluff_lsp_server_t), intent(inout) :: server
        character(len=*), intent(in) :: message
        logical, intent(out) :: success

        character(len=:), allocatable :: err
        character(len=:), allocatable :: params_json, text_document_json
        character(len=:), allocatable :: uri
        logical :: ok, found

        call json_parse(message, ok, err)
        if (.not. ok) then
            success = .false.
            return
        end if

        call json_get_member_json(message, "params", params_json, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_member_json(params_json, "textDocument", text_document_json, &
                                  found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call json_get_string_member(text_document_json, "uri", uri, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            return
        end if

        call server%handle_text_document_did_close(uri, success)
    end subroutine handle_did_close_notification

    subroutine handle_formatting_request(server, id, message, success)
        use fluff_lsp_server, only: fluff_lsp_server_t
        use iso_fortran_env, only: output_unit
        type(fluff_lsp_server_t), intent(inout) :: server
        integer, intent(in) :: id
        character(len=*), intent(in) :: message
        logical, intent(out) :: success

        character(len=:), allocatable :: formatted_content, response

        character(len=:), allocatable :: err
        character(len=:), allocatable :: params_json, text_document_json
        character(len=:), allocatable :: uri, formatted_json
        logical :: ok, found

        call json_parse(message, ok, err)
        if (.not. ok) then
            success = .false.
            response = create_json_error_response(id, -32700, "Parse error")
            write (output_unit, '(A)') response
            return
        end if

        call json_get_member_json(message, "params", params_json, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            response = create_json_error_response(id, -32602, "Invalid params")
            write (output_unit, '(A)') response
            return
        end if

        call json_get_member_json(params_json, "textDocument", text_document_json, &
                                  found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            response = create_json_error_response(id, -32602, "Invalid params")
            write (output_unit, '(A)') response
            return
        end if

        call json_get_string_member(text_document_json, "uri", uri, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            response = create_json_error_response(id, -32602, "Invalid params")
            write (output_unit, '(A)') response
            return
        end if

        call server%format_document(uri, formatted_content, success)

        if (success) then
            call json_escape_string(formatted_content, formatted_json)
            response = create_json_response(id, formatted_json)
        else
            response = create_json_error_response(id, -32603, "Formatting failed")
        end if

        write (output_unit, '(A)') response
    end subroutine handle_formatting_request

    subroutine handle_formatting_request_framed(server, id, message, success)
        use fluff_lsp_server, only: fluff_lsp_server_t
        use fluff_lsp_framing, only: lsp_write_framed_message
        type(fluff_lsp_server_t), intent(inout) :: server
        integer, intent(in) :: id
        character(len=*), intent(in) :: message
        logical, intent(out) :: success

        character(len=:), allocatable :: formatted_content, response

        character(len=:), allocatable :: err
        character(len=:), allocatable :: params_json, text_document_json
        character(len=:), allocatable :: uri, formatted_json
        logical :: ok, found

        call json_parse(message, ok, err)
        if (.not. ok) then
            success = .false.
            response = create_json_error_response(id, -32700, "Parse error")
            call lsp_write_framed_message(response)
            return
        end if

        call json_get_member_json(message, "params", params_json, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            response = create_json_error_response(id, -32602, "Invalid params")
            call lsp_write_framed_message(response)
            return
        end if

        call json_get_member_json(params_json, "textDocument", text_document_json, &
                                  found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            response = create_json_error_response(id, -32602, "Invalid params")
            call lsp_write_framed_message(response)
            return
        end if

        call json_get_string_member(text_document_json, "uri", uri, found, ok)
        if (.not. ok .or. .not. found) then
            success = .false.
            response = create_json_error_response(id, -32602, "Invalid params")
            call lsp_write_framed_message(response)
            return
        end if

        call server%format_document(uri, formatted_content, success)

        if (success) then
            call json_escape_string(formatted_content, formatted_json)
            response = create_json_response(id, formatted_json)
        else
            response = create_json_error_response(id, -32603, "Formatting failed")
        end if

        call lsp_write_framed_message(response)
    end subroutine handle_formatting_request_framed

    subroutine print_unified_diff(file_path, original, formatted)
        character(len=*), intent(in) :: file_path
        character(len=*), intent(in) :: original
        character(len=*), intent(in) :: formatted

        character(len=:), allocatable :: a_lines(:)
        character(len=:), allocatable :: b_lines(:)

        call split_lines(original, a_lines)
        call split_lines(formatted, b_lines)

        print *, "--- a/"//trim(file_path)
        print *, "+++ b/"//trim(file_path)
        print *, "@@ -"//trim(int_to_str(size(a_lines)))//" +"// &
            trim(int_to_str(size(b_lines)))//" @@"

        call print_prefixed_lines("-", a_lines)
        call print_prefixed_lines("+", b_lines)
    end subroutine print_unified_diff

    subroutine split_lines(text, lines)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: lines(:)

        integer :: i, n, start
        integer :: max_len
        character(len=1) :: nl

        nl = new_line('a')
        n = 1
        do i = 1, len(text)
            if (text(i:i) == nl) n = n + 1
        end do

        max_len = 0
        start = 1
        do i = 1, len(text)
            if (text(i:i) == nl) then
                max_len = max(max_len, i - start)
                start = i + 1
            end if
        end do
        max_len = max(max_len, len(text) - start + 1)
        max_len = max(1, max_len)

        allocate (character(len=max_len) :: lines(n))
        start = 1
        n = 0
        do i = 1, len(text)
            if (text(i:i) == nl) then
                n = n + 1
                lines(n) = text(start:i - 1)
                start = i + 1
            end if
        end do
        n = n + 1
        lines(n) = text(start:)
    end subroutine split_lines

    subroutine print_prefixed_lines(prefix, lines)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in) :: lines(:)

        integer :: i

        do i = 1, size(lines)
            print *, trim(prefix)//trim(lines(i))
        end do
    end subroutine print_prefixed_lines

    pure function int_to_str(i) result(str)
        integer, intent(in) :: i
        character(len=:), allocatable :: str
        character(len=32) :: buf

        write (buf, '(I0)') i
        str = trim(buf)
    end function int_to_str

    ! Print diagnostics
    subroutine print_diagnostics(diagnostics, format)
        use fluff_diagnostics, only: diagnostic_collection_t
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=*), intent(in), optional :: format

        character(len=:), allocatable :: output_format, json_output, sarif_output
        type(diagnostic_collection_t) :: collection
        integer :: i

        if (present(format)) then
            output_format = format
        else
            output_format = "text"
        end if

        select case (output_format)
        case ("text")
            do i = 1, size(diagnostics)
                call diagnostics(i)%print()
            end do
        case ("json")
            ! Create collection and output as JSON
            call collection%clear()
            do i = 1, size(diagnostics)
                call collection%add(diagnostics(i))
            end do
            json_output = collection%to_json()
            print *, json_output
        case ("sarif")
            ! Create collection and output as SARIF
            call collection%clear()
            do i = 1, size(diagnostics)
                call collection%add(diagnostics(i))
            end do
            sarif_output = collection%to_sarif()
            print *, sarif_output
        end select
    end subroutine print_diagnostics

end module fluff_cli
