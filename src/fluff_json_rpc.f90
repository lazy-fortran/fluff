module fluff_json_rpc
    use fluff_json, only: json_escape_string, json_get_int_member, &
                          json_get_member_json, &
                          json_get_string_member, json_has_member, json_is_object, &
                          json_parse
    implicit none
    private

    public :: check_message_errors
    public :: create_json_error_response
    public :: create_json_response
    public :: parse_lsp_message
    public :: process_capabilities
    public :: process_diagnostic_message
    public :: process_document_sync
    public :: process_initialize_request

contains

    subroutine parse_lsp_message(json_message, message_type, message_id, &
                                 method, success)
        character(len=*), intent(in) :: json_message
        character(len=:), allocatable, intent(out) :: message_type, method
        integer, intent(out) :: message_id
        logical, intent(out) :: success

        character(len=:), allocatable :: err
        character(len=:), allocatable :: jsonrpc
        logical :: found, ok
        logical :: has_id, has_method, has_result, has_error

        success = .false.
        message_type = "unknown"
        message_id = -1
        method = ""

        call json_parse(json_message, ok, err)
        if (.not. ok) return

        call json_get_string_member(json_message, "jsonrpc", jsonrpc, found, ok)
        if (.not. ok .or. .not. found) return
        if (jsonrpc /= "2.0") return

        call json_get_int_member(json_message, "id", message_id, has_id, ok)
        if (.not. ok) then
            has_id = .false.
            message_id = -1
        end if

        call json_get_string_member(json_message, "method", method, has_method, ok)
        if (.not. ok) then
            has_method = .false.
            method = ""
        end if

        call json_has_member(json_message, "result", has_result, ok)
        if (.not. ok) has_result = .false.
        call json_has_member(json_message, "error", has_error, ok)
        if (.not. ok) has_error = .false.

        if (has_method .and. has_id) then
            message_type = "request"
        else if (has_method .and. .not. has_id) then
            message_type = "notification"
            message_id = -1
        else if (has_result .and. has_id) then
            message_type = "response"
        else if (has_error .and. has_id) then
            message_type = "error"
        else
            message_type = "unknown"
        end if

        success = message_type /= "unknown"
    end subroutine parse_lsp_message

    function create_json_response(id, result) result(json_response)
        integer, intent(in) :: id
        character(len=*), intent(in) :: result
        character(len=:), allocatable :: json_response

        character(len=20) :: id_str

        write (id_str, '(I0)') id
        json_response = '{"jsonrpc":"2.0","id":'//trim(id_str)//',"result":'// &
                        result//'}'
    end function create_json_response

    function create_json_error_response(id, error_code, message) result(json_response)
        integer, intent(in) :: id, error_code
        character(len=*), intent(in) :: message
        character(len=:), allocatable :: json_response

        character(len=20) :: id_str, code_str
        character(len=:), allocatable :: message_json

        write (id_str, '(I0)') id
        write (code_str, '(I0)') error_code

        call json_escape_string(message, message_json)

        json_response = '{"jsonrpc":"2.0","id":'//trim(id_str)// &
                        ',"error":{"code":'//trim(code_str)// &
                        ',"message":'//message_json//'}}'
    end function create_json_error_response

    subroutine process_initialize_request(json_message, success)
        character(len=*), intent(in) :: json_message
        logical, intent(out) :: success

        character(len=:), allocatable :: message_type, method
        integer :: message_id

        call parse_lsp_message(json_message, message_type, message_id, method, success)
        if (.not. success) return
        success = message_type == "request" .and. method == "initialize"
    end subroutine process_initialize_request

    subroutine process_document_sync(json_message, success)
        character(len=*), intent(in) :: json_message
        logical, intent(out) :: success

        character(len=:), allocatable :: message_type, method
        integer :: message_id

        call parse_lsp_message(json_message, message_type, message_id, method, success)
        if (.not. success) return
        success = index(method, "textDocument/") == 1
    end subroutine process_document_sync

    subroutine process_diagnostic_message(json_message, success)
        character(len=*), intent(in) :: json_message
        logical, intent(out) :: success

        character(len=:), allocatable :: message_type, method
        integer :: message_id

        call parse_lsp_message(json_message, message_type, message_id, method, success)
        if (.not. success) return
        success = method == "textDocument/publishDiagnostics"
    end subroutine process_diagnostic_message

    subroutine process_capabilities(json_message, success)
        character(len=*), intent(in) :: json_message
        logical, intent(out) :: success

        character(len=:), allocatable :: err
        logical :: found, ok

        call json_parse(json_message, ok, err)
        if (.not. ok) then
            success = .false.
            return
        end if

        call json_has_member(json_message, "capabilities", found, success)
        success = success .and. found
    end subroutine process_capabilities

    subroutine check_message_errors(json_message, error_type, has_error)
        character(len=*), intent(in) :: json_message
        character(len=:), allocatable, intent(out) :: error_type
        logical, intent(out) :: has_error

        character(len=:), allocatable :: err
        character(len=:), allocatable :: method
        character(len=:), allocatable :: params_json
        logical :: ok, found
        logical :: has_method, has_result, has_error_field
        logical :: is_object

        has_error = .false.
        error_type = "Unknown"

        call json_parse(json_message, ok, err)
        if (.not. ok) then
            has_error = .true.
            error_type = "ParseError"
            return
        end if

        call json_has_member(json_message, "method", has_method, ok)
        if (.not. ok) has_method = .false.
        call json_has_member(json_message, "result", has_result, ok)
        if (.not. ok) has_result = .false.
        call json_has_member(json_message, "error", has_error_field, ok)
        if (.not. ok) has_error_field = .false.

        if (.not. has_method .and. .not. has_result .and. .not. has_error_field) then
            has_error = .true.
            error_type = "InvalidRequest"
            return
        end if

        call json_get_string_member(json_message, "method", method, found, ok)
        if (.not. ok) method = ""

        if (method == "nonexistent") then
            has_error = .true.
            error_type = "MethodNotFound"
            return
        end if

        if (method == "initialize") then
            call json_get_member_json(json_message, "params", params_json, found, ok)
            if (ok .and. found) then
                call json_is_object(params_json, is_object, ok)
                if (.not. ok .or. .not. is_object) then
                    has_error = .true.
                    error_type = "InvalidParams"
                end if
            end if
        end if
    end subroutine check_message_errors

end module fluff_json_rpc
