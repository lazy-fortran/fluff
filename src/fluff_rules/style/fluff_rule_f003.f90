module fluff_rule_f003
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, fix_suggestion_t, text_edit_t, &
                                 create_diagnostic, create_fix_suggestion, &
                                 SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename, current_source_text
    implicit none
    private

    public :: check_f003_line_length

contains

    subroutine check_f003_line_length(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        integer, parameter :: MAX_LINE_LENGTH = 88

        if (.not. allocated(current_source_text)) then
            allocate (violations(0))
            return
        end if

        allocate (temp_violations(100))
        violation_count = 0

        call analyze_line_lengths_with_ast_context(ctx, current_source_text, &
                                                   temp_violations, violation_count, &
                                                   MAX_LINE_LENGTH)

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = temp_violations(1:violation_count)
        end if
    end subroutine check_f003_line_length

    subroutine analyze_line_lengths_with_ast_context(ctx, source_text, violations, &
                                                     violation_count, max_length)
        type(fluff_ast_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        integer, intent(in) :: max_length

        character(len=1000) :: line
        integer :: pos, next_pos, line_num
        integer :: line_length
        type(source_range_t) :: location
        type(fix_suggestion_t), allocatable :: fix_suggestions(:)
        character(len=:), allocatable :: suggested_fix
        type(text_edit_t) :: text_edit

        pos = 1
        line_num = 0

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
            line_length = len_trim(line)

            if (line_length > max_length .and. .not. is_comment_line(line)) then
                violation_count = violation_count + 1

                location%start%line = line_num
                location%start%column = max_length + 1
                location%end%line = line_num
                location%end%column = line_length

                call generate_line_break_suggestions(line, suggested_fix)

                text_edit%range = location
                text_edit%new_text = suggested_fix

                allocate (fix_suggestions(1))
                fix_suggestions(1) = create_fix_suggestion( &
                                     description="Break line at logical points", &
                                     edits=[text_edit])

                violations(violation_count) = create_diagnostic( &
                                              code="F003", &
                   message="Line too long ("//trim(adjustl(int_to_str(line_length)))// &
                                        " > "//trim(adjustl(int_to_str(max_length)))// &
                                              " characters)", &
                                              file_path=current_filename, &
                                              location=location, &
                                              severity=SEVERITY_WARNING)

                allocate (violations(violation_count)%fixes, source=fix_suggestions)
            end if
        end do
    end subroutine analyze_line_lengths_with_ast_context

    subroutine generate_line_break_suggestions(line, suggested_fix)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: suggested_fix

        character(len=:), allocatable :: trimmed_line
        integer :: break_pos, i
        logical :: found_break_point

        trimmed_line = trim(line)
        found_break_point = .false.

        do i = len(trimmed_line), 50, -1
            if (i <= len(trimmed_line) .and. trimmed_line(i:i) == ",") then
                break_pos = i
                found_break_point = .true.
                exit
            end if
        end do

        if (.not. found_break_point) then
            do i = len(trimmed_line), 50, -1
                if (i <= len(trimmed_line)) then
                    select case (trimmed_line(i:i))
                    case ("+", "-", "*", "/")
                        break_pos = i
                        found_break_point = .true.
                        exit
                    end select
                end if
            end do
        end if

        if (.not. found_break_point) then
            i = index(trimmed_line, "::")
            if (i > 0 .and. i < 80) then
                break_pos = i + 1
                found_break_point = .true.
            end if
        end if

        if (found_break_point) then
            suggested_fix = trimmed_line(1:break_pos)//" &"//new_line("a")// &
                            "        "//trim(adjustl(trimmed_line(break_pos + 1:)))
        else
            suggested_fix = trimmed_line(1:80)//" &"//new_line("a")// &
                            "        "//trim(adjustl(trimmed_line(81:)))
        end if
    end subroutine generate_line_break_suggestions

    logical function is_comment_line(line) result(is_comment)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: trimmed

        trimmed = adjustl(line)
        is_comment = len_trim(trimmed) > 0 .and. trimmed(1:1) == "!"
    end function is_comment_line

    function int_to_str(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str

        write (str, "(I0)") i
    end function int_to_str

end module fluff_rule_f003
