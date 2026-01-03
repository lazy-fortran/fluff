module fluff_rule_f013
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename, current_source_text
    implicit none
    private

    public :: check_f013_multiple_statements

contains

    subroutine check_f013_multiple_statements(ctx, node_index, violations)
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

        call analyze_multiple_statements_from_text(current_source_text, &
                                                   temp_violations, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = temp_violations(1:violation_count)
        end if
    end subroutine check_f013_multiple_statements

    subroutine analyze_multiple_statements_from_text(source_text, violations, &
                                                     violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        integer :: i, semicolon_count, semicolon_pos
        logical :: in_string
        character :: quote_char

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
            if (is_comment_line(line_content)) then
                if (next_pos == 0) exit
                cycle
            end if

            semicolon_count = 0
            semicolon_pos = 0
            in_string = .false.
            quote_char = " "

            i = 1
            do while (i <= len(line_content))
                if (.not. in_string) then
                    if (line_content(i:i) == '"' .or. line_content(i:i) == "'") then
                        in_string = .true.
                        quote_char = line_content(i:i)
                    else if (line_content(i:i) == ";") then
                        semicolon_count = semicolon_count + 1
                        if (semicolon_pos == 0) semicolon_pos = i
                    else if (line_content(i:i) == "!") then
                        exit
                    end if
                else
                    if (line_content(i:i) == quote_char) then
                        if (i < len(line_content)) then
                            if (line_content(i + 1:i + 1) == quote_char) then
                                i = i + 1
                            else
                                in_string = .false.
                            end if
                        else
                            in_string = .false.
                        end if
                    end if
                end if

                i = i + 1
            end do

            if (semicolon_count > 0) then
                violation_count = violation_count + 1
                if (violation_count <= size(violations)) then
                    location%start%line = line_num
                    location%start%column = semicolon_pos
                    location%end%line = line_num
                    location%end%column = semicolon_pos
                    violations(violation_count) = create_diagnostic( &
                                                  code="F013", &
                                                  message="Multiple statements "// &
                                                  "per line", &
                                                  file_path=current_filename, &
                                                  location=location, &
                                                  severity=SEVERITY_WARNING &
                                                  )
                end if
            end if

            if (next_pos == 0) exit
            end do
            end subroutine analyze_multiple_statements_from_text

            logical function is_comment_line(line) result(is_comment)
                character(len=*), intent(in) :: line
                character(len=:), allocatable :: trimmed

                trimmed = adjustl(line)
                if (len_trim(trimmed) == 0) then
                    is_comment = .false.
                else
                    is_comment = trimmed(1:1) == "!"
                end if
            end function is_comment_line

        end module fluff_rule_f013
