module fluff_json_lexer
    implicit none
    private

    public :: skip_ws
    public :: skip_number
    public :: skip_bool
    public :: skip_null
    public :: json_starts_with_literal

contains

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

end module fluff_json_lexer
