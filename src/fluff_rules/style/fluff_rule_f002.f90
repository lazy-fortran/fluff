module fluff_rule_f002
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename, current_source_text
    implicit none
    private

    public :: check_f002_indentation

contains

    subroutine check_f002_indentation(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count

        if (.not. allocated(current_source_text)) then
            allocate (violations(0))
            return
        end if

        allocate (temp_violations(100))
        violation_count = 0

        call analyze_indentation_from_text(current_source_text, temp_violations, &
                                           violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = temp_violations(1:violation_count)
        end if
    end subroutine check_f002_indentation

    integer function count_leading_spaces(line) result(count)
        character(len=*), intent(in) :: line
        integer :: i

        count = 0
        do i = 1, len(line)
            if (line(i:i) == " ") then
                count = count + 1
            else
                exit
            end if
        end do
    end function count_leading_spaces

    subroutine analyze_indentation_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        character(len=1000) :: line
        integer :: pos, next_pos, line_num
        integer :: indent_levels(1000)
        integer :: line_count
        integer :: i, j
        logical :: has_inconsistency
        type(source_range_t) :: location

        pos = 1
        line_num = 0
        line_count = 0

        do while (pos <= len(source_text))
            next_pos = index(source_text(pos:), new_line("a"))
            if (next_pos == 0) then
                line = source_text(pos:)
                pos = len(source_text) + 1
            else
                line = source_text(pos:pos + next_pos - 2)
                pos = pos + next_pos
            end if

            line_num = line_num + 1
            if (len_trim(line) > 0 .and. line(1:1) /= "!") then
                line_count = line_count + 1
                indent_levels(line_count) = count_leading_spaces(line)
            end if
        end do

        has_inconsistency = .false.
        if (line_count >= 2) then
            do i = 1, line_count - 1
                do j = i + 1, line_count
                    if (indent_levels(i) > 0 .and. indent_levels(j) > 0) then
                        if (indent_levels(i) /= indent_levels(j)) then
                            if (mod(indent_levels(i), 4) /= &
                                mod(indent_levels(j), 4)) then
                                has_inconsistency = .true.
                                exit
                            end if
                        end if
                    end if
                end do
                if (has_inconsistency) exit
            end do
        end if

        if (.not. has_inconsistency) then
            return
        end if

        violation_count = violation_count + 1
        if (violation_count <= size(violations)) then
            location%start%line = 1
            location%start%column = 1
            location%end%line = 1
            location%end%column = 1

            violations(violation_count) = create_diagnostic( &
                                          code="F002", &
                                          message="Inconsistent indentation levels "// &
                                          "detected", &
                                          file_path=current_filename, &
                                          location=location, &
                                          severity=SEVERITY_WARNING &
                                          )
        end if
    end subroutine analyze_indentation_from_text

end module fluff_rule_f002
