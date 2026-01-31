module fluff_lsp_dispatcher
    use fluff_json, only: json_array_get_element_json, json_array_length, &
                          json_get_int_member, json_get_member_json, &
                          json_get_string_member, json_is_array, json_parse
    use fluff_json_rpc, only: create_json_error_response, create_json_response, &
                              parse_lsp_message
    use fluff_lsp_server, only: fluff_lsp_server_t, document_t
    use fluff_lsp_hover, only: get_hover_info
    implicit none
    private

    public :: lsp_dispatcher_t

    integer, parameter :: LSP_ERROR_PARSE_ERROR = -32700
    integer, parameter :: LSP_ERROR_INVALID_REQUEST = -32600
    integer, parameter :: LSP_ERROR_METHOD_NOT_FOUND = -32601
    integer, parameter :: LSP_ERROR_INVALID_PARAMS = -32602
    integer, parameter :: LSP_ERROR_INTERNAL_ERROR = -32603
    integer, parameter :: LSP_ERROR_SERVER_NOT_INITIALIZED = -32002

    type :: lsp_dispatcher_t
        type(fluff_lsp_server_t) :: server
        logical :: should_exit = .false.
    contains
        procedure :: dispatch
        procedure :: handle_request
        procedure :: handle_notification
    end type lsp_dispatcher_t

contains

    subroutine dispatch(this, json_message, response, has_response)
        class(lsp_dispatcher_t), intent(inout) :: this
        character(len=*), intent(in) :: json_message
        character(len=:), allocatable, intent(out) :: response
        logical, intent(out) :: has_response

        character(len=:), allocatable :: message_type, method
        integer :: message_id
        logical :: success

        has_response = .false.
        response = ""

        call parse_lsp_message(json_message, message_type, message_id, method, &
                               success)

        if (.not. success) then
            response = create_json_error_response(0, LSP_ERROR_PARSE_ERROR, &
                                                  "Parse error")
            has_response = .true.
            return
        end if

        select case (message_type)
        case ("request")
            call this%handle_request(json_message, method, message_id, response)
            has_response = .true.
        case ("notification")
            call this%handle_notification(json_message, method)
            has_response = .false.
        case default
            has_response = .false.
        end select
    end subroutine dispatch

    subroutine handle_request(this, json_message, method, message_id, response)
        class(lsp_dispatcher_t), intent(inout) :: this
        character(len=*), intent(in) :: json_message, method
        integer, intent(in) :: message_id
        character(len=:), allocatable, intent(out) :: response

        character(len=:), allocatable :: result_json
        character(len=:), allocatable :: params_json, root_path
        logical :: found, ok

        select case (method)
        case ("initialize")
            call json_get_member_json(json_message, "params", params_json, &
                                      found, ok)
            if (ok .and. found) then
                call json_get_string_member(params_json, "rootPath", root_path, &
                                            found, ok)
                if (.not. found .or. .not. ok) root_path = "."
            else
                root_path = "."
            end if

            call this%server%initialize(root_path)
            result_json = this%server%get_initialize_result()
            response = create_json_response(message_id, result_json)

        case ("shutdown")
            if (.not. this%server%is_initialized) then
                response = create_json_error_response(message_id, &
                             LSP_ERROR_SERVER_NOT_INITIALIZED, "Server not initialized")
                return
            end if
            call this%server%shutdown()
            response = create_json_response(message_id, "null")

        case ("textDocument/formatting")
            if (.not. this%server%is_initialized) then
                response = create_json_error_response(message_id, &
                             LSP_ERROR_SERVER_NOT_INITIALIZED, "Server not initialized")
                return
            end if
            call handle_formatting_request(this, json_message, message_id, &
                                           response)

        case ("textDocument/hover")
            if (.not. this%server%is_initialized) then
                response = create_json_error_response(message_id, &
                             LSP_ERROR_SERVER_NOT_INITIALIZED, "Server not initialized")
                return
            end if
            call handle_hover_request(this, json_message, message_id, response)

        case default
            response = create_json_error_response(message_id, &
                               LSP_ERROR_METHOD_NOT_FOUND, "Method not found: "//method)
        end select
    end subroutine handle_request

    subroutine handle_notification(this, json_message, method)
        class(lsp_dispatcher_t), intent(inout) :: this
        character(len=*), intent(in) :: json_message, method

        character(len=:), allocatable :: params_json, text_doc_json
        character(len=:), allocatable :: uri, language_id, content
        character(len=:), allocatable :: changes_json, change_json
        integer :: version, n_changes
        logical :: found, ok, success

        select case (method)
        case ("initialized")
            return

        case ("exit")
            this%should_exit = .true.

        case ("textDocument/didOpen")
            call json_get_member_json(json_message, "params", params_json, &
                                      found, ok)
            if (.not. ok .or. .not. found) return

            call json_get_member_json(params_json, "textDocument", &
                                      text_doc_json, found, ok)
            if (.not. ok .or. .not. found) return

            call json_get_string_member(text_doc_json, "uri", uri, found, ok)
            if (.not. ok .or. .not. found) return

            call json_get_string_member(text_doc_json, "languageId", &
                                        language_id, found, ok)
            if (.not. ok .or. .not. found) language_id = "fortran"

            call json_get_int_member(text_doc_json, "version", version, &
                                     found, ok)
            if (.not. ok .or. .not. found) version = 1

            call json_get_string_member(text_doc_json, "text", content, &
                                        found, ok)
            if (.not. ok .or. .not. found) content = ""

            call this%server%handle_text_document_did_open(uri, language_id, &
                                                           version, content, &
                                                           success)

        case ("textDocument/didChange")
            call json_get_member_json(json_message, "params", params_json, &
                                      found, ok)
            if (.not. ok .or. .not. found) return

            call json_get_member_json(params_json, "textDocument", &
                                      text_doc_json, found, ok)
            if (.not. ok .or. .not. found) return

            call json_get_string_member(text_doc_json, "uri", uri, found, ok)
            if (.not. ok .or. .not. found) return

            call json_get_int_member(text_doc_json, "version", version, &
                                     found, ok)
            if (.not. ok .or. .not. found) version = 1

            call json_get_member_json(params_json, "contentChanges", &
                                      changes_json, found, ok)
            if (.not. ok .or. .not. found) return

            call json_is_array(changes_json, ok, success)
            if (.not. ok .or. .not. success) return

            call json_array_length(changes_json, n_changes, ok)
            if (.not. ok .or. n_changes < 1) return

            call json_array_get_element_json(changes_json, 1, change_json, &
                                             found, ok)
            if (.not. ok .or. .not. found) return

            call json_get_string_member(change_json, "text", content, found, ok)
            if (.not. ok .or. .not. found) return

            call this%server%handle_text_document_did_change(uri, version, &
                                                             content, success)

        case ("textDocument/didSave")
            call json_get_member_json(json_message, "params", params_json, &
                                      found, ok)
            if (.not. ok .or. .not. found) return

            call json_get_member_json(params_json, "textDocument", &
                                      text_doc_json, found, ok)
            if (.not. ok .or. .not. found) return

            call json_get_string_member(text_doc_json, "uri", uri, found, ok)
            if (.not. ok .or. .not. found) return

            call this%server%handle_text_document_did_save(uri, success)

        case ("textDocument/didClose")
            call json_get_member_json(json_message, "params", params_json, &
                                      found, ok)
            if (.not. ok .or. .not. found) return

            call json_get_member_json(params_json, "textDocument", &
                                      text_doc_json, found, ok)
            if (.not. ok .or. .not. found) return

            call json_get_string_member(text_doc_json, "uri", uri, found, ok)
            if (.not. ok .or. .not. found) return

            call this%server%handle_text_document_did_close(uri, success)

        case default
            return
        end select
    end subroutine handle_notification

    subroutine handle_formatting_request(this, json_message, message_id, &
                                         response)
        class(lsp_dispatcher_t), intent(inout) :: this
        character(len=*), intent(in) :: json_message
        integer, intent(in) :: message_id
        character(len=:), allocatable, intent(out) :: response

        character(len=:), allocatable :: params_json, text_doc_json, uri
        character(len=:), allocatable :: formatted_content
        logical :: found, ok, success

        call json_get_member_json(json_message, "params", params_json, &
                                  found, ok)
        if (.not. ok .or. .not. found) then
            response = create_json_error_response(message_id, &
                                             LSP_ERROR_INVALID_PARAMS, "Missing params")
            return
        end if

        call json_get_member_json(params_json, "textDocument", text_doc_json, &
                                  found, ok)
        if (.not. ok .or. .not. found) then
            response = create_json_error_response(message_id, &
                                       LSP_ERROR_INVALID_PARAMS, "Missing textDocument")
            return
        end if

        call json_get_string_member(text_doc_json, "uri", uri, found, ok)
        if (.not. ok .or. .not. found) then
            response = create_json_error_response(message_id, &
                                                LSP_ERROR_INVALID_PARAMS, "Missing uri")
            return
        end if

        call this%server%format_document(uri, formatted_content, success)
        if (.not. success) then
            response = create_json_response(message_id, "[]")
            return
        end if

        response = create_json_response(message_id, "[]")
    end subroutine handle_formatting_request

    subroutine handle_hover_request(this, json_message, message_id, response)
        class(lsp_dispatcher_t), intent(inout) :: this
        character(len=*), intent(in) :: json_message
        integer, intent(in) :: message_id
        character(len=:), allocatable, intent(out) :: response

        character(len=:), allocatable :: params_json, text_doc_json
        character(len=:), allocatable :: position_json, uri
        character(len=:), allocatable :: hover_content, escaped_content
        type(document_t) :: doc
        integer :: line, character
        logical :: found, ok, success

        call json_get_member_json(json_message, "params", params_json, found, ok)
        if (.not. ok .or. .not. found) then
            response = create_json_error_response(message_id, &
                                         LSP_ERROR_INVALID_PARAMS, "Missing params")
            return
        end if

        call json_get_member_json(params_json, "textDocument", text_doc_json, &
                                  found, ok)
        if (.not. ok .or. .not. found) then
            response = create_json_error_response(message_id, &
                                   LSP_ERROR_INVALID_PARAMS, "Missing textDocument")
            return
        end if

        call json_get_string_member(text_doc_json, "uri", uri, found, ok)
        if (.not. ok .or. .not. found) then
            response = create_json_error_response(message_id, &
                                            LSP_ERROR_INVALID_PARAMS, "Missing uri")
            return
        end if

        call json_get_member_json(params_json, "position", position_json, found, ok)
        if (.not. ok .or. .not. found) then
            response = create_json_error_response(message_id, &
                                         LSP_ERROR_INVALID_PARAMS, "Missing position")
            return
        end if

        call json_get_int_member(position_json, "line", line, found, ok)
        if (.not. ok .or. .not. found) then
            response = create_json_error_response(message_id, &
                                            LSP_ERROR_INVALID_PARAMS, "Missing line")
            return
        end if

        call json_get_int_member(position_json, "character", character, found, ok)
        if (.not. ok .or. .not. found) then
            response = create_json_error_response(message_id, &
                                       LSP_ERROR_INVALID_PARAMS, "Missing character")
            return
        end if

        doc = this%server%workspace%get_document_by_uri(uri)
        if (.not. doc%is_open) then
            response = create_json_response(message_id, "null")
            return
        end if

        call get_hover_info(doc%content, line + 1, character, hover_content, success)

        if (.not. success .or. len_trim(hover_content) == 0) then
            response = create_json_response(message_id, "null")
            return
        end if

        escaped_content = escape_json_string(hover_content)
        response = create_json_response(message_id, &
                   '{"contents":{"kind":"markdown","value":"'//escaped_content//'"}}')

    end subroutine handle_hover_request

    function escape_json_string(str) result(escaped)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: escaped
        integer :: i, j
        character(len=1) :: ch

        allocate (character(len=len(str)*2) :: escaped)
        j = 0
        do i = 1, len(str)
            ch = str(i:i)
            select case (ch)
            case ('"')
                j = j + 1
                escaped(j:j) = '\'
                j = j + 1
                escaped(j:j) = '"'
            case ('\')
                j = j + 1
                escaped(j:j) = '\'
                j = j + 1
                escaped(j:j) = '\'
            case (achar(10))
                j = j + 1
                escaped(j:j) = '\'
                j = j + 1
                escaped(j:j) = 'n'
            case (achar(13))
                j = j + 1
                escaped(j:j) = '\'
                j = j + 1
                escaped(j:j) = 'r'
            case (achar(9))
                j = j + 1
                escaped(j:j) = '\'
                j = j + 1
                escaped(j:j) = 't'
            case default
                j = j + 1
                escaped(j:j) = ch
            end select
        end do
        escaped = escaped(1:j)
    end function escape_json_string

end module fluff_lsp_dispatcher
