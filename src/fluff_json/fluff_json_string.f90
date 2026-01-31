module fluff_json_string
    use fluff_json_lexer, only: skip_ws
    implicit none
    private

    public :: json_escape_string
    public :: parse_string_value
    public :: json_get_string_value

contains

    subroutine json_escape_string(raw, escaped)
        character(len=*), intent(in) :: raw
        character(len=:), allocatable, intent(out) :: escaped

        integer :: i
        character(len=2) :: hex
        character(len=1) :: c

        escaped = '"'
        do i = 1, len(raw)
            c = raw(i:i)
            select case (c)
            case ('"')
                escaped = escaped//'\"'
            case ('\')
                escaped = escaped//'\\'
            case (char(8))
                escaped = escaped//'\b'
            case (char(9))
                escaped = escaped//'\t'
            case (char(10))
                escaped = escaped//'\n'
            case (char(12))
                escaped = escaped//'\f'
            case (char(13))
                escaped = escaped//'\r'
            case default
                if (iachar(c) < 32) then
                    write (hex, '(Z2.2)') iachar(c)
                    escaped = escaped//'\u00'//hex
                else
                    escaped = escaped//c
                end if
            end select
        end do
        escaped = escaped//'"'
    end subroutine json_escape_string

    subroutine parse_string_value(text, pos, out, success, error_message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message

        character(len=:), allocatable :: buf
        character(len=1) :: c
        character(len=4) :: hex4
        integer :: codepoint
        integer :: iostat_val

        success = .false.
        error_message = ""

        if (pos > len(text) .or. text(pos:pos) /= '"') then
            error_message = "Expected quote to start string"
            return
        end if

        pos = pos + 1
        buf = ""

        do while (pos <= len(text))
            c = text(pos:pos)
            if (c == '"') then
                pos = pos + 1
                out = buf
                success = .true.
                return
            end if

            if (c /= '\') then
                buf = buf//c
                pos = pos + 1
                cycle
            end if

            pos = pos + 1
            if (pos > len(text)) then
                error_message = "Unterminated escape sequence"
                return
            end if

            c = text(pos:pos)
            select case (c)
            case ('"', '\', '/')
                buf = buf//c
                pos = pos + 1
            case ('b')
                buf = buf//char(8)
                pos = pos + 1
            case ('f')
                buf = buf//char(12)
                pos = pos + 1
            case ('n')
                buf = buf//char(10)
                pos = pos + 1
            case ('r')
                buf = buf//char(13)
                pos = pos + 1
            case ('t')
                buf = buf//char(9)
                pos = pos + 1
            case ('u')
                if (pos + 4 > len(text)) then
                    error_message = "Invalid unicode escape"
                    return
                end if
                hex4 = text(pos + 1:pos + 4)
                read (hex4, '(Z4)', iostat=iostat_val) codepoint
                if (iostat_val == 0 .and. codepoint >= 0 .and. codepoint <= 127) then
                    buf = buf//achar(codepoint)
                else
                    buf = buf//'?'
                end if
                pos = pos + 5
            case default
                error_message = "Invalid escape in string"
                return
            end select
        end do

        error_message = "Unterminated string"
    end subroutine parse_string_value

    subroutine json_get_string_value(json_string_value, out, success)
        character(len=*), intent(in) :: json_string_value
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: success

        integer :: pos
        character(len=:), allocatable :: err

        pos = 1
        err = ""

        call skip_ws(json_string_value, pos)
        call parse_string_value(json_string_value, pos, out, success, err)
        if (.not. success) return

        call skip_ws(json_string_value, pos)
        success = pos > len(json_string_value)
    end subroutine json_get_string_value

end module fluff_json_string
