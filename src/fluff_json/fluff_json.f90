module fluff_json
    implicit none
    private

    public :: json_escape_string
    public :: json_has_member
    public :: json_parse
    public :: json_get_int_member
    public :: json_get_member_json
    public :: json_get_string_member
    public :: json_get_string_value
    public :: json_array_get_element_json
    public :: json_array_length
    public :: json_is_array
    public :: json_is_null
    public :: json_is_object

contains

    subroutine json_parse(text, success, error_message)
        character(len=*), intent(in) :: text
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message

        integer :: pos

        success = .false.
        error_message = ""
        pos = 1

        call skip_value(text, pos, success, error_message)
        if (.not. success) return

        call skip_ws(text, pos)
        if (pos <= len(text)) then
            success = .false.
            error_message = "Trailing data after JSON value"
        end if
    end subroutine json_parse

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

    subroutine json_has_member(obj_json, key, found, success)
        character(len=*), intent(in) :: obj_json
        character(len=*), intent(in) :: key
        logical, intent(out) :: found
        logical, intent(out) :: success

        character(len=:), allocatable :: tmp

        call json_get_member_json(obj_json, key, tmp, found, success)
    end subroutine json_has_member

    subroutine json_get_string_member(obj_json, key, out, found, success)
        character(len=*), intent(in) :: obj_json
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: found
        logical, intent(out) :: success

        character(len=:), allocatable :: value_json

        out = ""
        call json_get_member_json(obj_json, key, value_json, found, success)
        if (.not. success .or. .not. found) return
        call json_get_string_value(value_json, out, success)
        if (.not. success) then
            found = .false.
            out = ""
        end if
    end subroutine json_get_string_member

    subroutine json_get_int_member(obj_json, key, out, found, success)
        character(len=*), intent(in) :: obj_json
        character(len=*), intent(in) :: key
        integer, intent(out) :: out
        logical, intent(out) :: found
        logical, intent(out) :: success

        character(len=:), allocatable :: value_json
        integer :: pos
        integer :: start_pos, end_pos
        integer :: iostat_val

        out = 0
        call json_get_member_json(obj_json, key, value_json, found, success)
        if (.not. success .or. .not. found) return

        pos = 1
        call skip_ws(value_json, pos)

        if (json_starts_with_literal(value_json, pos, "null")) then
            out = 0
            found = .false.
            success = .true.
            return
        end if

        start_pos = pos
        if (pos <= len(value_json) .and. value_json(pos:pos) == '-') pos = pos + 1
        if (pos > len(value_json)) then
            found = .false.
            success = .false.
            return
        end if

        if (value_json(pos:pos) < '0' .or. value_json(pos:pos) > '9') then
            found = .false.
            success = .false.
            return
        end if

        do while (pos <= len(value_json))
            if (value_json(pos:pos) < '0' .or. value_json(pos:pos) > '9') exit
            pos = pos + 1
        end do
        end_pos = pos - 1

        call skip_ws(value_json, pos)
        if (pos <= len(value_json)) then
            found = .false.
            success = .false.
            return
        end if

        read (value_json(start_pos:end_pos), *, iostat=iostat_val) out
        success = iostat_val == 0
        if (.not. success) found = .false.
    end subroutine json_get_int_member

    subroutine json_get_member_json(obj_json, key, value_json, found, success)
        character(len=*), intent(in) :: obj_json
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: value_json
        logical, intent(out) :: found
        logical, intent(out) :: success

        integer :: pos
        character(len=:), allocatable :: err
        character(len=:), allocatable :: member_key
        integer :: start_pos, end_pos

        value_json = ""
        found = .false.
        success = .false.
        err = ""
        pos = 1

        call skip_ws(obj_json, pos)
        if (pos > len(obj_json) .or. obj_json(pos:pos) /= '{') then
            return
        end if
        pos = pos + 1

        call skip_ws(obj_json, pos)
        if (pos <= len(obj_json) .and. obj_json(pos:pos) == '}') then
            success = .true.
            return
        end if

        do
            call skip_ws(obj_json, pos)
            call parse_string_value(obj_json, pos, member_key, success, err)
            if (.not. success) return

            call skip_ws(obj_json, pos)
            if (pos > len(obj_json) .or. obj_json(pos:pos) /= ':') then
                success = .false.
                return
            end if
            pos = pos + 1

            call skip_ws(obj_json, pos)
            start_pos = pos
            call skip_value(obj_json, pos, success, err)
            if (.not. success) return
            end_pos = pos - 1

            if (member_key == key) then
                value_json = obj_json(start_pos:end_pos)
                found = .true.
            end if

            call skip_ws(obj_json, pos)
            if (pos > len(obj_json)) then
                success = .false.
                return
            end if

            if (obj_json(pos:pos) == ',') then
                pos = pos + 1
                cycle
            end if
            if (obj_json(pos:pos) == '}') then
                pos = pos + 1
                exit
            end if

            success = .false.
            return
        end do

        success = .true.
    end subroutine json_get_member_json

    subroutine json_array_get_element_json(array_json, idx, element_json, &
                                           found, success)
        character(len=*), intent(in) :: array_json
        integer, intent(in) :: idx
        character(len=:), allocatable, intent(out) :: element_json
        logical, intent(out) :: found
        logical, intent(out) :: success

        integer :: pos
        integer :: i
        integer :: start_pos, end_pos
        character(len=:), allocatable :: err

        element_json = ""
        found = .false.
        success = .false.
        err = ""
        pos = 1

        if (idx < 1) return

        call skip_ws(array_json, pos)
        if (pos > len(array_json) .or. array_json(pos:pos) /= '[') return
        pos = pos + 1
        call skip_ws(array_json, pos)

        if (pos <= len(array_json) .and. array_json(pos:pos) == ']') then
            success = .true.
            return
        end if

        do i = 1, idx
            call skip_ws(array_json, pos)
            start_pos = pos
            call skip_value(array_json, pos, success, err)
            if (.not. success) return
            end_pos = pos - 1

            if (i == idx) then
                element_json = array_json(start_pos:end_pos)
                found = .true.
            end if

            call skip_ws(array_json, pos)
            if (pos > len(array_json)) then
                success = .false.
                return
            end if

            if (array_json(pos:pos) == ',') then
                pos = pos + 1
                cycle
            end if
            if (array_json(pos:pos) == ']') then
                pos = pos + 1
                exit
            end if

            success = .false.
            return
        end do

        success = .true.
    end subroutine json_array_get_element_json

    subroutine json_array_length(array_json, length, success)
        character(len=*), intent(in) :: array_json
        integer, intent(out) :: length
        logical, intent(out) :: success

        integer :: pos
        character(len=:), allocatable :: err

        length = 0
        success = .false.
        err = ""
        pos = 1

        call skip_ws(array_json, pos)
        if (pos > len(array_json) .or. array_json(pos:pos) /= '[') return
        pos = pos + 1
        call skip_ws(array_json, pos)

        if (pos <= len(array_json) .and. array_json(pos:pos) == ']') then
            pos = pos + 1
            call skip_ws(array_json, pos)
            success = pos > len(array_json)
            return
        end if

        do
            call skip_value(array_json, pos, success, err)
            if (.not. success) return
            length = length + 1

            call skip_ws(array_json, pos)
            if (pos > len(array_json)) then
                success = .false.
                return
            end if

            if (array_json(pos:pos) == ',') then
                pos = pos + 1
                cycle
            end if
            if (array_json(pos:pos) == ']') then
                pos = pos + 1
                exit
            end if

            success = .false.
            return
        end do

        call skip_ws(array_json, pos)
        success = pos > len(array_json)
    end subroutine json_array_length

    subroutine json_is_object(value_json, is_object, success)
        character(len=*), intent(in) :: value_json
        logical, intent(out) :: is_object
        logical, intent(out) :: success

        integer :: pos

        pos = 1
        call skip_ws(value_json, pos)
        if (pos > len(value_json)) then
            is_object = .false.
            success = .false.
            return
        end if

        is_object = value_json(pos:pos) == '{'
        success = .true.
    end subroutine json_is_object

    subroutine json_is_array(value_json, is_array, success)
        character(len=*), intent(in) :: value_json
        logical, intent(out) :: is_array
        logical, intent(out) :: success

        integer :: pos

        pos = 1
        call skip_ws(value_json, pos)
        if (pos > len(value_json)) then
            is_array = .false.
            success = .false.
            return
        end if

        is_array = value_json(pos:pos) == '['
        success = .true.
    end subroutine json_is_array

    subroutine json_is_null(value_json, is_null, success)
        character(len=*), intent(in) :: value_json
        logical, intent(out) :: is_null
        logical, intent(out) :: success

        integer :: pos

        pos = 1
        call skip_ws(value_json, pos)
        is_null = json_starts_with_literal(value_json, pos, "null")
        success = .true.
    end subroutine json_is_null

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

    subroutine skip_ws(text, pos)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos

        do while (pos <= len(text))
            select case (text(pos:pos))
            case (' ', char(9), char(10), char(13))
                pos = pos + 1
            case default
                exit
            end select
        end do
    end subroutine skip_ws

    recursive subroutine skip_value(text, pos, success, error_message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message

        character(len=1) :: c
        character(len=:), allocatable :: dummy_string

        success = .false.
        error_message = ""
        call skip_ws(text, pos)

        if (pos > len(text)) then
            error_message = "Unexpected end of input"
            return
        end if

        c = text(pos:pos)
        select case (c)
        case ('{')
            call skip_object(text, pos, success, error_message)
        case ('[')
            call skip_array(text, pos, success, error_message)
        case ('"')
            call parse_string_value(text, pos, dummy_string, success, error_message)
        case ('t', 'f')
            call skip_bool(text, pos, success, error_message)
        case ('n')
            call skip_null(text, pos, success, error_message)
        case ('-', '0':'9')
            call skip_number(text, pos, success, error_message)
        case default
            error_message = "Unexpected character in JSON value"
        end select
    end subroutine skip_value

    subroutine skip_object(text, pos, success, error_message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message

        character(len=:), allocatable :: key

        success = .false.
        error_message = ""

        if (text(pos:pos) /= '{') return
        pos = pos + 1
        call skip_ws(text, pos)

        if (pos <= len(text) .and. text(pos:pos) == '}') then
            pos = pos + 1
            success = .true.
            return
        end if

        do
            call skip_ws(text, pos)
            call parse_string_value(text, pos, key, success, error_message)
            if (.not. success) return

            call skip_ws(text, pos)
            if (pos > len(text) .or. text(pos:pos) /= ':') return
            pos = pos + 1

            call skip_value(text, pos, success, error_message)
            if (.not. success) return

            call skip_ws(text, pos)
            if (pos > len(text)) return

            if (text(pos:pos) == ',') then
                pos = pos + 1
                cycle
            end if
            if (text(pos:pos) == '}') then
                pos = pos + 1
                success = .true.
                return
            end if
            return
        end do
    end subroutine skip_object

    subroutine skip_array(text, pos, success, error_message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message

        success = .false.
        error_message = ""

        if (text(pos:pos) /= '[') return
        pos = pos + 1
        call skip_ws(text, pos)

        if (pos <= len(text) .and. text(pos:pos) == ']') then
            pos = pos + 1
            success = .true.
            return
        end if

        do
            call skip_value(text, pos, success, error_message)
            if (.not. success) return

            call skip_ws(text, pos)
            if (pos > len(text)) return

            if (text(pos:pos) == ',') then
                pos = pos + 1
                cycle
            end if
            if (text(pos:pos) == ']') then
                pos = pos + 1
                success = .true.
                return
            end if
            return
        end do
    end subroutine skip_array

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

    subroutine skip_number(text, pos, success, error_message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message

        character(len=1) :: c

        success = .false.
        error_message = ""

        if (text(pos:pos) == '-') pos = pos + 1
        if (pos > len(text)) then
            error_message = "Invalid number"
            return
        end if

        if (text(pos:pos) == '0') then
            pos = pos + 1
        else
            if (text(pos:pos) < '0' .or. text(pos:pos) > '9') then
                error_message = "Invalid number"
                return
            end if
            do while (pos <= len(text))
                c = text(pos:pos)
                if (c < '0' .or. c > '9') exit
                pos = pos + 1
            end do
        end if

        if (pos <= len(text) .and. text(pos:pos) == '.') then
            pos = pos + 1
            if (pos > len(text) .or. text(pos:pos) < '0' .or. text(pos:pos) > '9') then
                error_message = "Invalid number"
                return
            end if
            do while (pos <= len(text))
                c = text(pos:pos)
                if (c < '0' .or. c > '9') exit
                pos = pos + 1
            end do
        end if

        if (pos <= len(text)) then
            c = text(pos:pos)
            if (c == 'e' .or. c == 'E') then
                pos = pos + 1
                if (pos <= len(text)) then
                    c = text(pos:pos)
                    if (c == '+' .or. c == '-') pos = pos + 1
                end if
                if (pos > len(text) .or. text(pos:pos) < '0' .or. text(pos:pos) &
                    > '9') then
                    error_message = "Invalid number"
                    return
                end if
                do while (pos <= len(text))
                    c = text(pos:pos)
                    if (c < '0' .or. c > '9') exit
                    pos = pos + 1
                end do
            end if
        end if

        success = .true.
    end subroutine skip_number

    subroutine skip_bool(text, pos, success, error_message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message

        success = .false.
        error_message = ""

        if (pos + 3 <= len(text) .and. text(pos:pos + 3) == "true") then
            pos = pos + 4
            success = .true.
            return
        end if

        if (pos + 4 <= len(text) .and. text(pos:pos + 4) == "false") then
            pos = pos + 5
            success = .true.
            return
        end if

        error_message = "Invalid boolean literal"
    end subroutine skip_bool

    subroutine skip_null(text, pos, success, error_message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message

        success = .false.
        error_message = ""

        if (pos + 3 <= len(text) .and. text(pos:pos + 3) == "null") then
            pos = pos + 4
            success = .true.
            return
        end if

        error_message = "Invalid null literal"
    end subroutine skip_null

    pure logical function json_starts_with_literal(text, pos, literal) result(matches)
        character(len=*), intent(in) :: text
        integer, intent(in) :: pos
        character(len=*), intent(in) :: literal

        integer :: last

        last = pos + len(literal) - 1
        if (pos < 1 .or. last > len(text)) then
            matches = .false.
            return
        end if

        matches = text(pos:last) == literal
    end function json_starts_with_literal

end module fluff_json
