module fluff_lsp_protocol
    use fluff_diagnostics, only: diagnostic_t, SEVERITY_ERROR, SEVERITY_HINT, &
                                 SEVERITY_INFO, &
                                 SEVERITY_WARNING
    use fluff_json, only: json_escape_string
    implicit none
    private

    public :: lsp_clear_diagnostics_notification
    public :: lsp_format_diagnostic
    public :: lsp_publish_diagnostics_notification

contains

    subroutine lsp_format_diagnostic(diagnostic, formatted, success)
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=:), allocatable, intent(out) :: formatted
        logical, intent(out) :: success

        character(len=:), allocatable :: message_json, code_json
        character(len=:), allocatable :: start_line, start_char, end_line, end_char
        character(len=:), allocatable :: severity_str
        integer :: lsp_severity

        success = .false.
        formatted = ""

        lsp_severity = lsp_severity_from_diagnostic(diagnostic%severity)
        call json_escape_string(diagnostic%message, message_json)
        call json_escape_string(diagnostic%code, code_json)

        start_line = int_to_str(max(0, diagnostic%location%start%line - 1))
        start_char = int_to_str(max(0, diagnostic%location%start%column - 1))
        end_line = int_to_str(max(0, diagnostic%location%end%line - 1))
        end_char = int_to_str(max(0, diagnostic%location%end%column - 1))
        severity_str = int_to_str(lsp_severity)

        formatted = '{"range":{"start":{"line":'//start_line//',"character":'// &
                    start_char//'},"end":{"line":'//end_line//',"character":'// &
                    end_char//'}},"severity":'//severity_str//',"code":'// &
                    code_json//',"source":"fluff","message":'//message_json//'}'
        success = .true.
    end subroutine lsp_format_diagnostic

    subroutine lsp_publish_diagnostics_notification(uri, diagnostics, &
                                                    notification, success)
        character(len=*), intent(in) :: uri
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable, intent(out) :: notification
        logical, intent(out) :: success

        character(len=:), allocatable :: uri_json
        character(len=:), allocatable :: diagnostics_json
        character(len=:), allocatable :: one_diag
        logical :: ok
        integer :: i

        success = .false.
        notification = ""

        call json_escape_string(uri, uri_json)

        diagnostics_json = "["
        do i = 1, size(diagnostics)
            call lsp_format_diagnostic(diagnostics(i), one_diag, ok)
            if (.not. ok) return
            if (i > 1) diagnostics_json = diagnostics_json//","
            diagnostics_json = diagnostics_json//one_diag
        end do
        diagnostics_json = diagnostics_json//"]"

        notification = &
            '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics",'// &
            '"params":{"uri":'//uri_json//',"diagnostics":'// &
            diagnostics_json//'}}'
        success = .true.
    end subroutine lsp_publish_diagnostics_notification

    subroutine lsp_clear_diagnostics_notification(uri, notification, success)
        character(len=*), intent(in) :: uri
        character(len=:), allocatable, intent(out) :: notification
        logical, intent(out) :: success

        type(diagnostic_t), allocatable :: empty(:)

        allocate (empty(0))
        call lsp_publish_diagnostics_notification(uri, empty, notification, success)
    end subroutine lsp_clear_diagnostics_notification

    pure integer function lsp_severity_from_diagnostic(diagnostic_severity) &
        result(lsp_severity)
        integer, intent(in) :: diagnostic_severity

        select case (diagnostic_severity)
        case (SEVERITY_ERROR)
            lsp_severity = 1
        case (SEVERITY_WARNING)
            lsp_severity = 2
        case (SEVERITY_INFO)
            lsp_severity = 3
        case (SEVERITY_HINT)
            lsp_severity = 4
        case default
            lsp_severity = 3
        end select
    end function lsp_severity_from_diagnostic

    pure function int_to_str(i) result(str)
        integer, intent(in) :: i
        character(len=:), allocatable :: str
        character(len=32) :: buf

        write (buf, '(I0)') i
        str = trim(buf)
    end function int_to_str

end module fluff_lsp_protocol
