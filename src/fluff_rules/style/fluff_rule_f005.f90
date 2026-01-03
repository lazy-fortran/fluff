module fluff_rule_f005
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename, current_source_text
    implicit none
    private

    public :: check_f005_mixed_tabs_spaces

contains

    subroutine check_f005_mixed_tabs_spaces(ctx, node_index, violations)
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

        call analyze_mixed_tabs_spaces_from_text(current_source_text, temp_violations, &
                                                 violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = temp_violations(1:violation_count)
        end if
    end subroutine check_f005_mixed_tabs_spaces

    subroutine analyze_mixed_tabs_spaces_from_text(source_text, violations, &
                                                   violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        integer :: i, spaces, tabs
        logical :: has_spaces, has_tabs

        pos = 1
        line_num = 0

        do while (pos <= len(source_text))
            line_num = line_num + 1

            next_pos = index(source_text(pos:), new_line("a"))
            if (next_pos == 0) then
                line_start = pos
                line_end = len(source_text)
                pos = len(source_text) + 1
            else
                line_start = pos
                line_end = pos + next_pos - 2
                pos = pos + next_pos
            end if

            if (line_end < line_start) then
                if (next_pos == 0) exit
                cycle
            end if

            line_content = source_text(line_start:line_end)

            spaces = 0
            tabs = 0
            has_spaces = .false.
            has_tabs = .false.

            do i = 1, len(line_content)
                if (line_content(i:i) == " ") then
                    spaces = spaces + 1
                    has_spaces = .true.
                else if (line_content(i:i) == achar(9)) then
                    tabs = tabs + 1
                    has_tabs = .true.
                else
                    exit
                end if
            end do

            if (has_spaces .and. has_tabs) then
                violation_count = violation_count + 1
                if (violation_count <= size(violations)) then
                    location%start%line = line_num
                    location%start%column = 1
                    location%end%line = line_num
                    location%end%column = i

                    violations(violation_count) = create_diagnostic( &
                                                  code="F005", &
                                                  message="Mixed tabs and spaces", &
                                                  file_path=current_filename, &
                                                  location=location, &
                                                  severity=SEVERITY_WARNING &
                                                  )
                end if
            end if

            if (next_pos == 0) exit
        end do
    end subroutine analyze_mixed_tabs_spaces_from_text

end module fluff_rule_f005
