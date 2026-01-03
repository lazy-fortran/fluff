module fluff_rule_f008
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename, current_source_text
    implicit none
    private

    public :: check_f008_missing_intent

contains

    subroutine check_f008_missing_intent(ctx, node_index, violations)
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

        call analyze_missing_intent_from_text(current_source_text, temp_violations, &
                                              violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = temp_violations(1:violation_count)
        end if
    end subroutine check_f008_missing_intent

    subroutine analyze_missing_intent_from_text(source_text, violations, &
                                                violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_trimmed
        character(len=:), allocatable :: line_lower
        logical :: in_procedure
        logical :: in_interface
        integer :: proc_start_line
        integer :: i, arg_start, arg_end
        character(len=:), allocatable :: proc_args
        logical, allocatable :: arg_has_intent(:)
        integer :: num_args

        pos = 1
        line_num = 0
        in_procedure = .false.
        in_interface = .false.
        allocate (arg_has_intent(100))
        arg_has_intent = .false.
        num_args = 0

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

            line_trimmed = adjustl(line_content)
            line_lower = to_lower(line_trimmed)

            if (index(line_lower, "interface") == 1) then
                in_interface = .true.
            else if (index(line_lower, "end interface") == 1) then
                in_interface = .false.
            end if

            if (.not. in_interface) then
                if (index(line_lower, "subroutine ") == 1 .or. &
                    (index(line_lower, "function ") > 0 .and. &
                     index(line_lower, "function ") <= 20)) then
                    in_procedure = .true.
                    proc_start_line = line_num

                    arg_start = index(line_lower, "(")
                    arg_end = index(line_lower, ")")
                    if (arg_start > 0 .and. arg_end > arg_start) then
                        proc_args = line_lower(arg_start + 1:arg_end - 1)
                        num_args = 1
                        do i = 1, len(proc_args)
                            if (proc_args(i:i) == ",") num_args = num_args + 1
                        end do
                        arg_has_intent = .false.
                    else
                        num_args = 0
                    end if
                else if (index(line_lower, "end subroutine") == 1 .or. &
                         index(line_lower, "end function") == 1) then
                    if (in_procedure .and. num_args > 0) then
                        do i = 1, num_args
                            if (.not. arg_has_intent(i)) then
                                violation_count = violation_count + 1
                                if (violation_count <= size(violations)) then
                                    location%start%line = proc_start_line
                                    location%start%column = 1
                                    location%end%line = proc_start_line
                                    location%end%column = 1
                                    violations(violation_count) = create_diagnostic( &
                                                                  code="F008", &
                         message="Missing intent declaration for procedure arguments", &
                                                           file_path=current_filename, &
                                                                  location=location, &
                                                              severity=SEVERITY_WARNING)
                                end if
                                exit
                            end if
                        end do
                    end if
                    in_procedure = .false.
                    num_args = 0
                else if (in_procedure) then
                    if (index(line_lower, "intent(") > 0) then
                        do i = 1, num_args
                            arg_has_intent(i) = .true.
                        end do
                    end if
                end if
            end if

            if (next_pos == 0) exit
        end do
    end subroutine analyze_missing_intent_from_text

    logical function is_comment_line(line) result(is_comment)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: trimmed

        trimmed = adjustl(line)
        is_comment = len_trim(trimmed) > 0 .and. trimmed(1:1) == "!"
    end function is_comment_line

    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str

        integer :: i
        integer :: ascii_val

        lower_str = str
        do i = 1, len(str)
            ascii_val = iachar(str(i:i))
            if (ascii_val >= iachar("A") .and. ascii_val <= iachar("Z")) then
                lower_str(i:i) = achar(ascii_val + 32)
            end if
        end do
    end function to_lower

end module fluff_rule_f008
