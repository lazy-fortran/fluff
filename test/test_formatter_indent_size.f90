program test_formatter_indent_size
    !! format_source has to indent by options%indent_size. It used to indent
    !! by four columns whatever was configured, because fortfront's program
    !! and module body emitters build their prefix from a literal four spaces
    !! and only its nested-statement path consults the requested width. That
    !! produced output mixing both widths, so a single-width oracle is what
    !! this checks: every indented line must be a whole number of levels of
    !! the configured width.
    use fluff_formatter, only: formatter_engine_t
    implicit none

    integer :: failures

    failures = 0

    call check_width(1)
    call check_width(2)
    call check_width(3)
    call check_width(4)
    call check_width(8)
    call check_tab_indent()

    if (failures /= 0) error stop "formatter ignores the configured indent size"

    print *, "[OK] format_source honours options%indent_size"

contains

    subroutine check_width(width)
        integer, intent(in) :: width

        character(len=:), allocatable :: formatted

        call format_sample(width, ' ', formatted)

        ! The shallowest indented line is one level deep, so it fixes the
        ! width; every other indent must be a multiple of it.
        call expect(smallest_positive_indent(formatted) == width, &
            "shallowest indent is not the configured width", width, formatted)
        call expect(all_indents_are_multiples(formatted, width), &
            "an indent is not a multiple of the configured width", width, &
            formatted)
    end subroutine check_width

    subroutine check_tab_indent()
        character(len=:), allocatable :: formatted

        call format_sample(1, achar(9), formatted)

        call expect(index(formatted, achar(9)//"implicit none") > 0, &
            "declarations are not indented with the configured tab", 1, &
            formatted)
        ! The print sits three levels in: program body, do body, if body.
        call expect(index(formatted, repeat(achar(9), 3)//"print") > 0, &
            "nested statements are not indented with the configured tab", 1, &
            formatted)
    end subroutine check_tab_indent

    subroutine format_sample(width, indent_char, formatted)
        integer, intent(in) :: width
        character(len=1), intent(in) :: indent_char
        character(len=:), allocatable, intent(out) :: formatted

        type(formatter_engine_t) :: formatter
        character(len=:), allocatable :: error_msg

        call formatter%initialize()
        formatter%options%indent_size = width
        formatter%options%indent_char = indent_char
        formatter%options%use_tabs = indent_char == achar(9)

        call formatter%format_source(sample_source(), formatted, error_msg)

        if (error_msg /= "") error stop "formatting the sample failed"
        if (len_trim(formatted) == 0) error stop "formatting produced no output"
    end subroutine format_sample

    ! Top level declarations and a nested statement, so that both fortfront
    ! emission paths are exercised.
    function sample_source() result(source)
        character(len=:), allocatable :: source

        source = "program p"//new_line('a')// &
            "implicit none"//new_line('a')// &
            "integer :: i"//new_line('a')// &
            "do i = 1, 3"//new_line('a')// &
            "if (i > 1) then"//new_line('a')// &
            "print *, i"//new_line('a')// &
            "end if"//new_line('a')// &
            "end do"//new_line('a')// &
            "end program p"
    end function sample_source

    subroutine expect(condition, what, width, formatted)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: what
        integer, intent(in) :: width
        character(len=*), intent(in) :: formatted

        if (condition) return

        failures = failures + 1
        print *, "[FAIL] indent_size ", width, ": ", what
        print *, formatted
    end subroutine expect

    function smallest_positive_indent(code) result(width)
        character(len=*), intent(in) :: code
        integer :: width

        integer :: line_start, line_end, lead

        width = 0
        line_start = 1
        do while (line_start <= len(code))
            call next_line(code, line_start, line_end)
            lead = leading_blanks(code, line_start, line_end)
            if (lead > 0) then
                if (width == 0) then
                    width = lead
                else if (lead < width) then
                    width = lead
                end if
            end if
            line_start = line_end + 2
        end do
    end function smallest_positive_indent

    function all_indents_are_multiples(code, width) result(ok)
        character(len=*), intent(in) :: code
        integer, intent(in) :: width
        logical :: ok

        integer :: line_start, line_end, lead

        ok = .true.
        line_start = 1
        do while (line_start <= len(code))
            call next_line(code, line_start, line_end)
            lead = leading_blanks(code, line_start, line_end)
            if (lead > 0) then
                if (mod(lead, width) /= 0) ok = .false.
            end if
            line_start = line_end + 2
        end do
    end function all_indents_are_multiples

    subroutine next_line(code, line_start, line_end)
        character(len=*), intent(in) :: code
        integer, intent(in) :: line_start
        integer, intent(out) :: line_end

        integer :: newline_offset

        newline_offset = index(code(line_start:), new_line('a'))
        if (newline_offset == 0) then
            line_end = len(code)
        else
            line_end = line_start + newline_offset - 2
        end if
    end subroutine next_line

    ! Number of leading blanks on a line that has content, zero otherwise.
    function leading_blanks(code, line_start, line_end) result(lead)
        character(len=*), intent(in) :: code
        integer, intent(in) :: line_start, line_end
        integer :: lead

        lead = 0
        if (line_end < line_start) return
        if (len_trim(code(line_start:line_end)) == 0) return

        lead = verify(code(line_start:line_end), " ") - 1
        if (lead < 0) lead = 0
    end function leading_blanks

end program test_formatter_indent_size
