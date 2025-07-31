module fluff_cli
    ! Command-line interface
    use fluff_core
    use fluff_linter
    use fluff_formatter
    use fluff_config
    use fluff_diagnostics
    use fluff_json_rpc, only: parse_lsp_message
    implicit none
    private
    
    ! CLI argument types
    type, public :: cli_args_t
        character(len=:), allocatable :: command     ! "check", "format", "server"
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
        character(len=:), allocatable :: output_format  ! "text", "json", "sarif"
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
                
            else if (len_trim(argv(i)) >= 2 .and. argv(i)(1:2) == "--") then
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
                
            else if (len_trim(argv(i)) >= 1 .and. argv(i)(1:1) == "-") then
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
            allocate(character(len=len(new_file)) :: files(1))
            files(1) = new_file
        else
            n = size(files)
            allocate(character(len=max(len(files), len(new_file))) :: temp(n+1))
            temp(1:n) = files
            temp(n+1) = new_file
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
        print '(a,i0,".",i0,".",i0)', "fluff ", version%major, version%minor, version%patch
        
    end subroutine app_print_version
    
    ! Run check command
    subroutine run_check_command(app, exit_code)
        type(cli_app_t), intent(inout) :: app
        integer, intent(out) :: exit_code
        
        type(fluff_config_t) :: config
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        integer :: i
        
        exit_code = 0
        
        ! Load configuration
        if (allocated(app%args%config_file)) then
            call config%from_file(app%args%config_file)
        else
            config = create_default_config()
        end if
        
        ! Initialize linter
        call app%linter%initialize()
        
        ! Process files
        if (allocated(app%args%files)) then
            do i = 1, size(app%args%files)
                call app%linter%lint_file(app%args%files(i), diagnostics, error_msg)
                
                if (error_msg /= "") then
                    print *, "Error linting file: ", error_msg
                    exit_code = 1
                else if (allocated(diagnostics)) then
                    if (size(diagnostics) > 0) exit_code = 1
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
        type(cli_app_t), intent(inout) :: app
        integer, intent(out) :: exit_code
        
        character(len=:), allocatable :: formatted_code
        character(len=:), allocatable :: error_msg
        integer :: i
        
        exit_code = 0
        
        ! Initialize formatter
        call app%formatter%initialize()
        
        ! Process files
        if (allocated(app%args%files)) then
            do i = 1, size(app%args%files)
                call app%formatter%format_file(app%args%files(i), formatted_code, error_msg)
                
                if (error_msg /= "") then
                    print *, "Error formatting file: ", error_msg
                    exit_code = 1
                else if (app%args%diff) then
                    ! TODO: Show diff
                    print *, "Diff not yet implemented"
                else
                    ! TODO: Write formatted code back to file
                    print *, "Writing formatted code not yet implemented"
                end if
            end do
        else
            print *, "No files specified"
            exit_code = 1
        end if
        
    end subroutine run_format_command
    
    ! Run server command
    subroutine run_server_command(app, exit_code)
        use fluff_lsp_server
        use iso_fortran_env, only: input_unit, output_unit, error_unit
        type(cli_app_t), intent(inout) :: app
        integer, intent(out) :: exit_code
        
        type(fluff_lsp_server_t) :: lsp_server
        character(len=10000) :: line
        character(len=:), allocatable :: message_type, method
        integer :: message_id, iostat
        logical :: success, server_running
        
        exit_code = 0
        server_running = .true.
        
        ! Initialize LSP server
        call lsp_server%initialize("")
        
        ! Write initialization message to stderr for debugging
        write(error_unit, '(A)') "Fluff LSP server starting..."
        
        ! Main server loop - read from stdin, write to stdout
        do while (server_running)
            ! Read JSON-RPC message from stdin
            read(input_unit, '(A)', iostat=iostat) line
            
            if (iostat /= 0) then
                ! End of input - exit gracefully
                server_running = .false.
                cycle
            end if
            
            ! Skip empty lines
            if (len_trim(line) == 0) cycle
            
            ! Parse JSON-RPC message
            call parse_lsp_message(trim(line), message_type, message_id, method, success)
            
            if (.not. success) then
                write(error_unit, '(A)') "Failed to parse message: " // trim(line)
                cycle
            end if
            
            ! Handle different message types
            select case (message_type)
            case ("request")
                call handle_lsp_request(lsp_server, message_id, method, trim(line), success)
                if (method == "shutdown") then
                    server_running = .false.
                end if
                
            case ("notification")
                call handle_lsp_notification(lsp_server, method, trim(line), success)
                
            case default
                write(error_unit, '(A)') "Unknown message type: " // message_type
            end select
        end do
        
        write(error_unit, '(A)') "Fluff LSP server shutting down..."
        
    end subroutine run_server_command
    
    ! Handle LSP requests
    subroutine handle_lsp_request(server, id, method, message, success)
        use fluff_lsp_server
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
            write(output_unit, '(A)') response
            
        case ("shutdown")
            ! Acknowledge shutdown
            response = create_json_response(id, 'null')
            write(output_unit, '(A)') response
            
        case ("textDocument/formatting")
            call handle_formatting_request(server, id, message, success)
            
        case default
            ! Method not found error
            response = create_json_error_response(id, -32601, "Method not found")
            write(output_unit, '(A)') response
            write(error_unit, '(A)') "Unknown method: " // method
        end select
        
    end subroutine handle_lsp_request
    
    ! Handle LSP notifications
    subroutine handle_lsp_notification(server, method, message, success)
        use fluff_lsp_server
        use iso_fortran_env, only: error_unit
        type(fluff_lsp_server_t), intent(inout) :: server
        character(len=*), intent(in) :: method, message
        logical, intent(out) :: success
        
        success = .true.
        
        select case (method)
        case ("initialized")
            ! Server initialization complete
            write(error_unit, '(A)') "LSP server initialized"
            
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
            write(error_unit, '(A)') "Exit notification received"
            
        case default
            write(error_unit, '(A)') "Unknown notification: " // method
        end select
        
    end subroutine handle_lsp_notification
    
    ! Placeholder implementations for document lifecycle
    subroutine handle_did_open_notification(server, message, success)
        use fluff_lsp_server
        type(fluff_lsp_server_t), intent(inout) :: server
        character(len=*), intent(in) :: message
        logical, intent(out) :: success
        
        ! Basic placeholder - would extract URI, languageId, version, text from JSON
        call server%handle_text_document_did_open("file:///placeholder.f90", "fortran", 1, "program test\nend program", success)
        
    end subroutine handle_did_open_notification
    
    subroutine handle_did_change_notification(server, message, success)
        use fluff_lsp_server
        type(fluff_lsp_server_t), intent(inout) :: server
        character(len=*), intent(in) :: message
        logical, intent(out) :: success
        
        ! Basic placeholder
        call server%handle_text_document_did_change("file:///placeholder.f90", 2, "program modified\nend program", success)
        
    end subroutine handle_did_change_notification
    
    subroutine handle_did_save_notification(server, message, success)
        use fluff_lsp_server
        type(fluff_lsp_server_t), intent(inout) :: server
        character(len=*), intent(in) :: message
        logical, intent(out) :: success
        
        ! Basic placeholder
        call server%handle_text_document_did_save("file:///placeholder.f90", success)
        
    end subroutine handle_did_save_notification
    
    subroutine handle_did_close_notification(server, message, success)
        use fluff_lsp_server
        type(fluff_lsp_server_t), intent(inout) :: server
        character(len=*), intent(in) :: message
        logical, intent(out) :: success
        
        ! Basic placeholder
        call server%handle_text_document_did_close("file:///placeholder.f90", success)
        
    end subroutine handle_did_close_notification
    
    subroutine handle_formatting_request(server, id, message, success)
        use fluff_lsp_server
        use iso_fortran_env, only: output_unit
        type(fluff_lsp_server_t), intent(inout) :: server
        integer, intent(in) :: id
        character(len=*), intent(in) :: message
        logical, intent(out) :: success
        
        character(len=:), allocatable :: formatted_content, response
        
        ! Basic placeholder - would extract URI from JSON
        call server%format_document("file:///placeholder.f90", formatted_content, success)
        
        if (success) then
            response = create_json_response(id, '"' // formatted_content // '"')
        else
            response = create_json_error_response(id, -32603, "Formatting failed")
        end if
        
        write(output_unit, '(A)') response
        
    end subroutine handle_formatting_request
    
    ! Create JSON-RPC response
    function create_json_response(id, result) result(json_response)
        integer, intent(in) :: id
        character(len=*), intent(in) :: result
        character(len=:), allocatable :: json_response
        
        character(len=20) :: id_str
        
        write(id_str, '(I0)') id
        json_response = '{"jsonrpc":"2.0","id":' // trim(id_str) // ',"result":' // result // '}'
        
    end function create_json_response
    
    ! Create JSON-RPC error response
    function create_json_error_response(id, error_code, message) result(json_response)
        integer, intent(in) :: id, error_code
        character(len=*), intent(in) :: message
        character(len=:), allocatable :: json_response
        
        character(len=20) :: id_str, code_str
        
        write(id_str, '(I0)') id
        write(code_str, '(I0)') error_code
        
        json_response = '{"jsonrpc":"2.0","id":' // trim(id_str) // &
                       ',"error":{"code":' // trim(code_str) // &
                       ',"message":"' // message // '"}}'
        
    end function create_json_error_response
    
    ! Print diagnostics
    subroutine print_diagnostics(diagnostics, format)
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=*), intent(in), optional :: format
        
        character(len=:), allocatable :: output_format
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
            print *, "JSON output not yet implemented"
        case ("sarif")
            print *, "SARIF output not yet implemented"
        end select
        
    end subroutine print_diagnostics
    
end module fluff_cli