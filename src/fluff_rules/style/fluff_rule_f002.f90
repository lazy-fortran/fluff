module fluff_rule_f002
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename
    use fluff_rule_diagnostic_utils, only: push_diagnostic
    implicit none
    private

    public :: check_f002_indentation

contains

    subroutine check_f002_indentation(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        allocate (tmp(0))
        violation_count = 0

        call analyze_indentation(ctx, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f002_indentation

    subroutine analyze_indentation(ctx, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: indent_unit
        integer :: line_num
        integer :: indent_cols
        logical :: found
        character(len=:), allocatable :: line_text

        indent_unit = 0
        line_num = 1
        do
            call ctx%get_source_line(line_num, line_text, found)
            if (.not. found) exit
            if (.not. is_code_line(line_text)) then
                line_num = line_num + 1
                cycle
            end if

            indent_cols = count_leading_indent_columns(line_text)
            if (indent_cols > 0 .and. indent_unit == 0) indent_unit = indent_cols

            if (indent_unit > 0 .and. indent_cols > 0) then
                if (mod(indent_cols, indent_unit) /= 0) then
                    call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                         code="F002", &
                                   message="Inconsistent indentation levels detected", &
                                         file_path=current_filename, &
                                         location=line_location(line_num), &
                                         severity=SEVERITY_WARNING))
                    return
                end if
            end if
            line_num = line_num + 1
        end do
    end subroutine analyze_indentation

    pure logical function is_code_line(line) result(is_code)
        character(len=*), intent(in) :: line

        integer :: i
        character(len=1) :: ch

        is_code = .false.
        do i = 1, len(line)
            ch = line(i:i)
            if (ch == " " .or. ch == achar(9) .or. ch == achar(13)) cycle
            is_code = (ch /= "!")
            return
        end do
    end function is_code_line

    pure integer function count_leading_indent_columns(line) result(cols)
        character(len=*), intent(in) :: line

        integer :: i
        character(len=1) :: ch

        cols = 0
        do i = 1, len(line)
            ch = line(i:i)
            if (ch == " ") then
                cols = cols + 1
            else if (ch == achar(9)) then
                cols = cols + 4
            else
                exit
            end if
        end do
    end function count_leading_indent_columns

    pure function line_location(line_num) result(location)
        integer, intent(in) :: line_num
        type(source_range_t) :: location

        location%start%line = line_num
        location%start%column = 1
        location%end%line = line_num
        location%end%column = 1
    end function line_location

end module fluff_rule_f002
