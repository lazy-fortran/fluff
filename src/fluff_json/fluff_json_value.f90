module fluff_json_value
    use fluff_json_lexer, only: skip_ws, skip_number, skip_bool, skip_null
    use fluff_json_string, only: parse_string_value
    implicit none
    private

    public :: skip_value
    public :: skip_object
    public :: skip_array

contains

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

end module fluff_json_value
