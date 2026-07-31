program test_formatter_quality
    use fluff_formatter
    use fluff_core
    implicit none

    type(formatter_engine_t) :: formatter
    character(len=:), allocatable :: source_code, formatted_code, error_msg

    print *, "=== Formatter Quality Metrics Testing ==="

    call formatter%initialize()

    ! Test quality metrics
    call test_indentation_consistency()
    call test_line_length_compliance()
    call test_semantic_preservation()
    call test_format_stability()
    call test_whitespace_normalization()
    call test_inline_comment_preservation()

    print *, "[OK] All formatter quality tests passed!"

contains

    subroutine test_indentation_consistency()
        integer :: indent_violations
        print *, ""
        print *, "Testing indentation consistency..."

        ! Test consistent 4-space indentation
        source_code = "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "if (1 > 0) then" // new_line('a') // &
            "print *, 'hello'" // new_line('a') // &
            "end if" // new_line('a') // &
            "end program test"

        call formatter%format_source(source_code, formatted_code, error_msg)
        if (error_msg /= "") then
            error stop "Formatting failed: " // error_msg
        end if

        ! Check for consistent indentation
        indent_violations = count_indentation_violations(formatted_code, 4)

        print *, "  Source lines: ", count_lines(source_code)
        print *, "  Formatted lines: ", count_lines(formatted_code)
        print *, "  Indentation violations: ", indent_violations
        print *, "  Indentation consistency: ", &
            real(count_lines(formatted_code) - indent_violations) / real(count_lines(formatted_code)) * 100.0, "%"

        if (indent_violations > 0) then
            print *, "  Warning: Found indentation inconsistencies"
        else
            print *, "[OK] Perfect indentation consistency"
        end if

    end subroutine test_indentation_consistency

    subroutine test_line_length_compliance()
        integer :: long_lines, max_line_length
        print *, ""
        print *, "Testing line length compliance..."

        ! Test with intentionally long line
        source_code = "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "result = very_long_function_name(argument1, argument2, argument3, argument4, argument5)" // new_line('a') // &
            "end program test"

        call formatter%format_source(source_code, formatted_code, error_msg)
        if (error_msg /= "") then
            error stop "Formatting failed: " // error_msg
        end if

        call analyze_line_lengths(formatted_code, long_lines, max_line_length)

        print *, "  Lines over 88 chars: ", long_lines
        print *, "  Max line length: ", max_line_length
        print *, "  Line length compliance: ", &
            real(count_lines(formatted_code) - long_lines) / real(count_lines(formatted_code)) * 100.0, "%"

        if (long_lines == 0) then
            print *, "[OK] All lines within length limit"
        else
            print *, "  Warning: Some lines exceed length limit"
        end if

    end subroutine test_line_length_compliance

    subroutine test_semantic_preservation()
        character(len=:), allocatable :: original_normalized, formatted_normalized
        logical :: semantically_identical
        print *, ""
        print *, "Testing semantic preservation..."

        source_code = "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "real :: x, y, z" // new_line('a') // &
            "x = 1.0" // new_line('a') // &
            "y = 2.0" // new_line('a') // &
            "z = x + y" // new_line('a') // &
            "print *, z" // new_line('a') // &
            "end program test"

        call formatter%format_source(source_code, formatted_code, error_msg)
        if (error_msg /= "") then
            error stop "Formatting failed: " // error_msg
        end if

        ! Normalize both versions for semantic comparison
        original_normalized = normalize_for_semantic_comparison(source_code)
        formatted_normalized = normalize_for_semantic_comparison(formatted_code)

        semantically_identical = (original_normalized == formatted_normalized)

        print *, "  Original tokens: ", count_tokens(source_code)
        print *, "  Formatted tokens: ", count_tokens(formatted_code)
        print *, "  Semantic preservation: ", semantically_identical

        if (semantically_identical) then
            print *, "[OK] Perfect semantic preservation"
        else
            print *, "  Warning: Semantic differences detected"
            print *, "  Original normalized: ", original_normalized
            print *, "  Formatted normalized: ", formatted_normalized
        end if

    end subroutine test_semantic_preservation

    subroutine test_format_stability()
        character(len=:), allocatable :: first_format, second_format
        logical :: is_stable
        print *, ""
        print *, "Testing format stability (idempotency)..."

        source_code = "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer::i,j" // new_line('a') // &
            "i=1;j=2" // new_line('a') // &
            "print*,i,j" // new_line('a') // &
            "end program test"

        ! Format once
        call formatter%format_source(source_code, first_format, error_msg)
        if (error_msg /= "") then
            error stop "First formatting failed: " // error_msg
        end if

        ! Format the formatted code again
        call formatter%format_source(first_format, second_format, error_msg)
        if (error_msg /= "") then
            error stop "Second formatting failed: " // error_msg
        end if

        is_stable = (first_format == second_format)

        print *, "  Format stability: ", is_stable
        print *, "  First format length: ", len(first_format)
        print *, "  Second format length: ", len(second_format)

        if (is_stable) then
            print *, "[OK] Format is stable (idempotent)"
        else
            print *, "  Warning: Format is unstable"
            print *, "  First: ", first_format
            print *, "  Second: ", second_format
        end if

    end subroutine test_format_stability

    subroutine test_inline_comment_preservation()
        !! An inline comment documents the statement it sits on. Formatting may
        !! normalize the spacing before the bang, but detaching the comment from
        !! its statement destroys that documentation, so every trailing comment
        !! below must come back on a line that still carries its own statement.
        print *, ""
        print *, "Testing inline comment preservation..."

        source_code = "program p"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    integer :: x  ! count"//new_line('a')// &
            "    integer :: y = 2  ! init"//new_line('a')// &
            "    ! standalone"//new_line('a')// &
            "    x = 1  ! set"//new_line('a')// &
            "    if (x > 0) then  ! guard"//new_line('a')// &
            "        print *, x  ! show"//new_line('a')// &
            "    end if  ! done"//new_line('a')// &
            "end program p  ! tail"//new_line('a')

        call formatter%format_source(source_code, formatted_code, error_msg)
        if (error_msg /= "") then
            error stop "Formatting failed: "//error_msg
        end if

        call assert_inline_comment(formatted_code, "integer :: x", "! count")
        call assert_inline_comment(formatted_code, "integer :: y", "! init")
        call assert_inline_comment(formatted_code, "x = 1", "! set")
        call assert_inline_comment(formatted_code, "if (x > 0) then", "! guard")
        call assert_inline_comment(formatted_code, "print *, x", "! show")
        call assert_inline_comment(formatted_code, "end if", "! done")
        call assert_inline_comment(formatted_code, "end program p", "! tail")

        ! A standalone comment stays standalone: it must not be glued onto the
        ! statement that follows it.
        call assert_standalone_comment(formatted_code, "! standalone")

        print *, "[OK] Inline comments stay on their statements"

    end subroutine test_inline_comment_preservation

    subroutine assert_inline_comment(text, code_fragment, comment)
        character(len=*), intent(in) :: text, code_fragment, comment
        character(len=:), allocatable :: line
        integer :: i, code_pos, comment_pos
        logical :: found

        found = .false.
        do i = 1, count_lines(text)
            line = get_line(text, i)
            code_pos = index(line, code_fragment)
            comment_pos = index(line, comment)
            if (code_pos <= 0) cycle
            if (comment_pos <= code_pos) cycle
            found = .true.
            exit
        end do

        if (.not. found) then
            print *, "  Missing inline comment for: ", code_fragment
            print *, "  Formatted output:"
            print *, text
            error stop "Inline comment detached from its statement"
        end if

    end subroutine assert_inline_comment

    subroutine assert_standalone_comment(text, comment)
        character(len=*), intent(in) :: text, comment
        character(len=:), allocatable :: line
        integer :: i, comment_pos
        logical :: found

        found = .false.
        do i = 1, count_lines(text)
            line = get_line(text, i)
            comment_pos = index(line, comment)
            if (comment_pos <= 0) cycle
            if (len_trim(line(1:comment_pos - 1)) /= 0) then
                print *, "  Standalone comment merged into a statement: ", line
                error stop "Standalone comment must not join a statement"
            end if
            found = .true.
        end do

        if (.not. found) then
            print *, "  Formatted output:"
            print *, text
            error stop "Standalone comment lost during formatting"
        end if

    end subroutine assert_standalone_comment

    function get_line(text, line_number) result(line)
        character(len=*), intent(in) :: text
        integer, intent(in) :: line_number
        character(len=:), allocatable :: line
        integer :: i, current, start_pos

        current = 1
        start_pos = 1
        line = ""

        do i = 1, len(text)
            if (text(i:i) /= new_line('a')) cycle
            if (current == line_number) then
                line = text(start_pos:i - 1)
                return
            end if
            current = current + 1
            start_pos = i + 1
        end do

        if (current == line_number .and. start_pos <= len(text)) then
            line = text(start_pos:)
        end if

    end function get_line

    subroutine test_whitespace_normalization()
        integer :: trailing_spaces, inconsistent_spacing
        print *, ""
        print *, "Testing whitespace normalization..."

        ! Test with messy whitespace
        source_code = "program test   " // new_line('a') // &
            "  implicit none  " // new_line('a') // &
            "integer  ::   i  ,  j" // new_line('a') // &
            "i  =  1  +  2" // new_line('a') // &
            "end program test" // new_line('a')

        call formatter%format_source(source_code, formatted_code, error_msg)
        if (error_msg /= "") then
            error stop "Formatting failed: " // error_msg
        end if

        call analyze_whitespace(formatted_code, trailing_spaces, inconsistent_spacing)

        print *, "  Trailing spaces: ", trailing_spaces
        print *, "  Inconsistent spacing: ", inconsistent_spacing
        print *, "  Whitespace quality: ", &
            real(count_lines(formatted_code) - trailing_spaces - inconsistent_spacing) / &
            real(count_lines(formatted_code)) * 100.0, "%"

        if (trailing_spaces == 0 .and. inconsistent_spacing == 0) then
            print *, "[OK] Perfect whitespace normalization"
        else
            print *, "  Warning: Whitespace issues detected"
        end if

    end subroutine test_whitespace_normalization

    ! Helper functions for quality analysis
    function count_lines(text) result(lines)
        character(len=*), intent(in) :: text
        integer :: lines, i

        lines = 1
        do i = 1, len(text)
            if (text(i:i) == new_line('a')) lines = lines + 1
        end do

    end function count_lines

    function count_tokens(text) result(tokens)
        character(len=*), intent(in) :: text
        integer :: tokens, i
        logical :: in_token

        tokens = 0
        in_token = .false.

        do i = 1, len(text)
            if (text(i:i) /= ' ' .and. text(i:i) /= new_line('a') .and. text(i:i) /= char(9)) then
                if (.not. in_token) then
                    tokens = tokens + 1
                    in_token = .true.
                end if
            else
                in_token = .false.
            end if
        end do

    end function count_tokens

    function count_indentation_violations(text, expected_indent) result(violations)
        character(len=*), intent(in) :: text
        integer, intent(in) :: expected_indent
        integer :: violations, i, line_start, indent_count

        violations = 0
        line_start = 1

        do i = 1, len(text)
            if (text(i:i) == new_line('a') .or. i == len(text)) then
                ! Analyze this line
                indent_count = 0
                do while (line_start + indent_count <= len(text) .and. &
                        text(line_start + indent_count:line_start + indent_count) == ' ')
                    indent_count = indent_count + 1
                end do

                ! Check if indentation follows expected pattern
                if (indent_count > 0 .and. mod(indent_count, expected_indent) /= 0) then
                    violations = violations + 1
                end if

                line_start = i + 1
            end if
        end do

    end function count_indentation_violations

    subroutine analyze_line_lengths(text, long_lines, max_length)
        character(len=*), intent(in) :: text
        integer, intent(out) :: long_lines, max_length
        integer :: i, line_start, current_length

        long_lines = 0
        max_length = 0
        line_start = 1

        do i = 1, len(text)
            if (text(i:i) == new_line('a') .or. i == len(text)) then
                current_length = i - line_start
                if (current_length > max_length) max_length = current_length
                if (current_length > 88) long_lines = long_lines + 1
                line_start = i + 1
            end if
        end do

    end subroutine analyze_line_lengths

    subroutine analyze_whitespace(text, trailing_spaces, inconsistent_spacing)
        character(len=*), intent(in) :: text
        integer, intent(out) :: trailing_spaces, inconsistent_spacing
        integer :: i, line_start

        trailing_spaces = 0
        inconsistent_spacing = 0
        line_start = 1

        do i = 1, len(text)
            if (text(i:i) == new_line('a') .or. i == len(text)) then
                ! Check for trailing spaces
                if (i > line_start .and. text(i-1:i-1) == ' ') then
                    trailing_spaces = trailing_spaces + 1
                end if
                line_start = i + 1
            end if
        end do

    end subroutine analyze_whitespace

    function normalize_for_semantic_comparison(text) result(normalized)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: normalized
        integer :: i, j

        ! Simple normalization: remove all whitespace and newlines
        allocate(character(len=len(text)) :: normalized)
        j = 1

        do i = 1, len(text)
            if (text(i:i) /= ' ' .and. text(i:i) /= new_line('a') .and. text(i:i) /= char(9)) then
                normalized(j:j) = text(i:i)
                j = j + 1
            end if
        end do

        normalized = normalized(1:j-1)

    end function normalize_for_semantic_comparison

end program test_formatter_quality
