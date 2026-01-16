module fluff_format_quality_assess
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fluff_format_quality_types, only: format_quality_t
    implicit none
    private

    public :: assess_format_quality

contains

    subroutine assess_format_quality(code, quality)
        character(len=*), intent(in) :: code
        type(format_quality_t), intent(out) :: quality

        call assess_indentation(code, quality%indentation_score)
        call assess_spacing(code, quality%spacing_score)
        call assess_readability(code, quality%readability_score)
        call assess_structure(code, quality%structure_score)
        call assess_consistency(code, quality%consistency_score)
        call assess_line_length(code, quality%line_length_score)

        quality%total_lines = count_lines(code)
        quality%blank_lines = count_blank_lines(code)
        quality%long_lines = count_long_lines(code, 88)

        call quality%calculate_overall_score()
        call quality%generate_recommendations()

    end subroutine assess_format_quality

    subroutine assess_indentation(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score

        integer :: consistent_lines, total_lines
        integer :: expected_indent, actual_indent
        integer :: code_index, line_start, line_end
        logical :: in_procedure, in_if, in_do

        total_lines = count_lines(code)
        consistent_lines = 0
        expected_indent = 0

        in_procedure = .false.
        in_if = .false.
        in_do = .false.

        line_start = 1
        do code_index = 1, len(code)
            if (code(code_index:code_index) == new_line("a") .or. &
                code_index == len(code)) then
                line_end = code_index - 1
                if (code_index == len(code)) line_end = code_index

                call analyze_line_indentation(code(line_start:line_end), &
                                              expected_indent, actual_indent, &
                                              in_procedure, in_if, in_do)

                if (abs(actual_indent - expected_indent) <= 1) then
                    consistent_lines = consistent_lines + 1
                end if

                line_start = code_index + 1
            end if
        end do

        if (total_lines > 0) then
            score = real(consistent_lines, dp) / real(total_lines, dp) * 10.0_dp
        else
            score = 10.0_dp
        end if

    end subroutine assess_indentation

    subroutine assess_spacing(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score

        integer :: good_spacing, total_operators
        integer :: code_index

        good_spacing = 0
        total_operators = 0

        do code_index = 1, len(code) - 2
            select case (code(code_index:code_index))
            case ("=")
                total_operators = total_operators + 1
                if (code_index > 1 .and. code_index < len(code)) then
                    if (code(code_index-1:code_index-1) == " " .and. &
                        code(code_index+1:code_index+1) == " ") then
                        good_spacing = good_spacing + 1
                    end if
                end if
            case ("+", "-", "*", "/")
                if (code(code_index:code_index) == "*" .and. &
                    code_index < len(code)) then
                    if (code(code_index+1:code_index+1) == "*") cycle
                end if

                total_operators = total_operators + 1
                if (code_index > 1 .and. code_index < len(code)) then
                    if (code(code_index-1:code_index-1) == " " .and. &
                        code(code_index+1:code_index+1) == " ") then
                        good_spacing = good_spacing + 1
                    end if
                end if
            end select
        end do

        if (total_operators > 0) then
            score = real(good_spacing, dp) / real(total_operators, dp) * 10.0_dp
        else
            score = 9.0_dp
        end if

    end subroutine assess_spacing

    subroutine assess_readability(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score

        integer :: total_lines, blank_lines
        real(dp) :: blank_line_ratio, ideal_ratio

        total_lines = count_lines(code)
        blank_lines = count_blank_lines(code)
        ideal_ratio = 0.15_dp

        if (total_lines > 0) then
            blank_line_ratio = real(blank_lines, dp) / real(total_lines, dp)

            if (blank_line_ratio >= ideal_ratio) then
                score = 10.0_dp - min(5.0_dp, &
                                      (blank_line_ratio - ideal_ratio) * 20.0_dp)
            else
                score = 5.0_dp + (blank_line_ratio / ideal_ratio) * 5.0_dp
            end if
        else
            score = 8.0_dp
        end if

    end subroutine assess_readability

    subroutine assess_structure(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score

        logical :: has_implicit_none, has_proper_end, has_contains

        has_implicit_none = index(code, "implicit none") > 0
        has_proper_end = index(code, "end program") > 0 .or. &
                         index(code, "end module") > 0 .or. &
                         index(code, "end subroutine") > 0 .or. &
                         index(code, "end function") > 0
        has_contains = index(code, "contains") > 0

        score = 6.0_dp
        if (has_implicit_none) score = score + 2.0_dp
        if (has_proper_end) score = score + 1.5_dp
        if (has_contains) score = score + 0.5_dp

    end subroutine assess_structure

    subroutine assess_consistency(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score

        logical :: consistent_case, consistent_spacing

        consistent_case = check_case_consistency(code)
        consistent_spacing = check_spacing_consistency(code)

        score = 6.0_dp
        if (consistent_case) score = score + 2.0_dp
        if (consistent_spacing) score = score + 2.0_dp

    end subroutine assess_consistency

    subroutine assess_line_length(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score

        integer :: total_lines, long_lines

        total_lines = count_lines(code)
        long_lines = count_long_lines(code, 88)

        if (total_lines > 0) then
            score = real(total_lines - long_lines, dp) / real(total_lines, dp) * 10.0_dp
        else
            score = 10.0_dp
        end if

    end subroutine assess_line_length

    function check_case_consistency(code) result(consistent)
        character(len=*), intent(in) :: code
        logical :: consistent

        consistent = index(code, "program") > 0 .or. index(code, "module") > 0 .or. &
                     index(code, "subroutine") > 0 .or. index(code, "function") > 0

    end function check_case_consistency

    function check_spacing_consistency(code) result(consistent)
        character(len=*), intent(in) :: code
        logical :: consistent

        consistent = index(code, " = ") > 0 .or. index(code, " :: ") > 0

    end function check_spacing_consistency

    subroutine analyze_line_indentation(line, expected_indent, actual_indent, &
                                        in_procedure, in_if, in_do)
        character(len=*), intent(in) :: line
        integer, intent(inout) :: expected_indent
        integer, intent(out) :: actual_indent
        logical, intent(inout) :: in_procedure, in_if, in_do

        character(len=:), allocatable :: trimmed_line
        integer :: line_index

        trimmed_line = trim(line)

        actual_indent = 0
        do line_index = 1, len(line)
            if (line(line_index:line_index) == " ") then
                actual_indent = actual_indent + 1
            else
                exit
            end if
        end do

        if (len(trimmed_line) == 0) return

        if (index(trimmed_line, "subroutine ") == 1 .or. &
            index(trimmed_line, "function ") == 1) then
            in_procedure = .true.
            expected_indent = 0
        else if (index(trimmed_line, "end subroutine") == 1 .or. &
                 index(trimmed_line, "end function") == 1) then
            in_procedure = .false.
            expected_indent = 0
        else if (in_procedure) then
            expected_indent = 4
        end if

        if (index(trimmed_line, "if ") == 1) then
            in_if = .true.
        else if (index(trimmed_line, "end if") == 1) then
            in_if = .false.
        end if

        if (index(trimmed_line, "do ") == 1) then
            in_do = .true.
        else if (index(trimmed_line, "end do") == 1) then
            in_do = .false.
        end if

        if (in_if) expected_indent = expected_indent + 4
        if (in_do) expected_indent = expected_indent + 4

    end subroutine analyze_line_indentation

    function count_lines(code) result(count)
        character(len=*), intent(in) :: code
        integer :: count, code_index

        count = 1
        do code_index = 1, len(code)
            if (code(code_index:code_index) == new_line("a")) count = count + 1
        end do
    end function count_lines

    function count_blank_lines(code) result(count)
        character(len=*), intent(in) :: code
        integer :: count, code_index
        logical :: line_is_blank, prev_was_newline

        count = 0
        line_is_blank = .true.
        prev_was_newline = .true.

        do code_index = 1, len(code)
            if (code(code_index:code_index) == new_line("a")) then
                if (line_is_blank .and. .not. prev_was_newline) then
                    count = count + 1
                end if
                line_is_blank = .true.
                prev_was_newline = .true.
            else if (code(code_index:code_index) /= " " .and. &
                     code(code_index:code_index) /= char(9)) then
                line_is_blank = .false.
                prev_was_newline = .false.
            else
                prev_was_newline = .false.
            end if
        end do
    end function count_blank_lines

    function count_long_lines(code, max_length) result(count)
        character(len=*), intent(in) :: code
        integer, intent(in) :: max_length
        integer :: count, line_length, code_index

        count = 0
        line_length = 0

        do code_index = 1, len(code)
            if (code(code_index:code_index) == new_line("a")) then
                if (line_length > max_length) count = count + 1
                line_length = 0
            else
                line_length = line_length + 1
            end if
        end do

        if (line_length > max_length) count = count + 1
    end function count_long_lines

end module fluff_format_quality_assess
