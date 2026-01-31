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

        character(len=:), allocatable :: buffer
        integer :: i, pos, buffer_size
        character(len=2) :: hex
        character(len=1) :: c

        buffer_size = len(raw) * 2 + 2
        allocate (character(len=buffer_size) :: buffer)
        buffer(1:1) = '"'
        pos = 2

        do i = 1, len(raw)
            c = raw(i:i)
            select case (c)
            case ('"')
                buffer(pos:pos + 1) = '\"'
                pos = pos + 2
            case ('\')
                buffer(pos:pos + 1) = '\\'
                pos = pos + 2
            case (char(8))
                buffer(pos:pos + 1) = '\b'
                pos = pos + 2
            case (char(9))
                buffer(pos:pos + 1) = '\t'
                pos = pos + 2
            case (char(10))
                buffer(pos:pos + 1) = '\n'
                pos = pos + 2
            case (char(12))
                buffer(pos:pos + 1) = '\f'
                pos = pos + 2
            case (char(13))
                buffer(pos:pos + 1) = '\r'
                pos = pos + 2
            case default
                if (iachar(c) < 32) then
                    write (hex, '(Z2.2)') iachar(c)
                    buffer(pos:pos + 5) = '\u00' // hex
                    pos = pos + 6
                else
                    buffer(pos:pos) = c
                    pos = pos + 1
                end if
            end select
        end do

        buffer(pos:pos) = '"'
        escaped = buffer(1:pos)
    end subroutine json_escape_string

    subroutine parse_string_value(text, pos, out, success, error_message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message

        character(len=:), allocatable :: buffer
        character(len=1) :: c
        character(len=4) :: hex4
        integer :: codepoint, iostat_val, buf_pos

        success = .false.
        error_message = ""

        if (pos > len(text) .or. text(pos:pos) /= '"') then
            error_message = "Expected quote to start string"
            return
        end if

        pos = pos + 1
        allocate (character(len=len(text)) :: buffer)
        buf_pos = 1

        do while (pos <= len(text))
            c = text(pos:pos)
            if (c == '"') then
                pos = pos + 1
                out = buffer(1:buf_pos - 1)
                success = .true.
                return
            end if

            if (c /= '\') then
                buffer(buf_pos:buf_pos) = c
                buf_pos = buf_pos + 1
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
                buffer(buf_pos:buf_pos) = c
                buf_pos = buf_pos + 1
                pos = pos + 1
            case ('b')
                buffer(buf_pos:buf_pos) = char(8)
                buf_pos = buf_pos + 1
                pos = pos + 1
            case ('f')
                buffer(buf_pos:buf_pos) = char(12)
                buf_pos = buf_pos + 1
                pos = pos + 1
            case ('n')
                buffer(buf_pos:buf_pos) = char(10)
                buf_pos = buf_pos + 1
                pos = pos + 1
            case ('r')
                buffer(buf_pos:buf_pos) = char(13)
                buf_pos = buf_pos + 1
                pos = pos + 1
            case ('t')
                buffer(buf_pos:buf_pos) = char(9)
                buf_pos = buf_pos + 1
                pos = pos + 1
            case ('u')
                if (pos + 4 > len(text)) then
                    error_message = "Invalid unicode escape"
                    return
                end if
                hex4 = text(pos + 1:pos + 4)
                read (hex4, '(Z4)', iostat=iostat_val) codepoint
                if (iostat_val == 0 .and. codepoint >= 0 .and. codepoint <= 127) then
                    buffer(buf_pos:buf_pos) = achar(codepoint)
                else
                    buffer(buf_pos:buf_pos) = '?'
                end if
                buf_pos = buf_pos + 1
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
