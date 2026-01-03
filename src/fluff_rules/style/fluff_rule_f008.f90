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

        integer :: pos, line_num
        logical :: done
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_trimmed
        character(len=:), allocatable :: line_lower
        logical :: in_procedure
        logical :: in_interface
        integer :: proc_start_line
        logical, allocatable :: arg_has_intent(:)
        integer :: num_args

        pos = 1
        line_num = 0
        in_procedure = .false.
        in_interface = .false.
        proc_start_line = 0
        allocate (arg_has_intent(100))
        arg_has_intent = .false.
        num_args = 0

        do
            call next_source_line(source_text, pos, line_num, line_content, done)
            if (done) exit
            if (is_comment_line(line_content)) cycle

            line_trimmed = adjustl(line_content)
            line_lower = to_lower(line_trimmed)

            call update_interface_state(line_lower, in_interface)
            if (in_interface) cycle

            if (is_procedure_start(line_lower)) then
                call start_procedure_from_signature(line_lower, line_num, &
                                                    in_procedure, proc_start_line, &
                                                    num_args, arg_has_intent)
                cycle
            end if

            if (is_procedure_end(line_lower)) then
                call finish_procedure_if_needed(violations, violation_count, &
                                                proc_start_line, in_procedure, &
                                                num_args, arg_has_intent)
                in_procedure = .false.
                num_args = 0
                cycle
            end if

            if (in_procedure) then
                if (index(line_lower, "intent(") > 0) then
                    call mark_all_args_have_intent(num_args, arg_has_intent)
                end if
            end if
        end do
    end subroutine analyze_missing_intent_from_text

    subroutine next_source_line(source_text, pos, line_num, line_content, done)
        character(len=*), intent(in) :: source_text
        integer, intent(inout) :: pos
        integer, intent(inout) :: line_num
        character(len=:), allocatable, intent(out) :: line_content
        logical, intent(out) :: done

        integer :: next_pos
        integer :: line_start, line_end

        if (pos > len(source_text)) then
            done = .true.
            line_content = ""
            return
        end if

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
            line_content = ""
        else
            line_content = source_text(line_start:line_end)
        end if

        done = .false.
    end subroutine next_source_line

    subroutine update_interface_state(line_lower, in_interface)
        character(len=*), intent(in) :: line_lower
        logical, intent(inout) :: in_interface

        if (index(line_lower, "interface") == 1) then
            in_interface = .true.
        else if (index(line_lower, "end interface") == 1) then
            in_interface = .false.
        end if
    end subroutine update_interface_state

    logical function is_procedure_start(line_lower) result(is_start)
        character(len=*), intent(in) :: line_lower

        is_start = index(line_lower, "subroutine ") == 1 .or. &
                   (index(line_lower, "function ") > 0 .and. &
                    index(line_lower, "function ") <= 20)
    end function is_procedure_start

    logical function is_procedure_end(line_lower) result(is_end)
        character(len=*), intent(in) :: line_lower

        is_end = index(line_lower, "end subroutine") == 1 .or. &
                 index(line_lower, "end function") == 1
    end function is_procedure_end

    subroutine start_procedure_from_signature(line_lower, line_num, in_procedure, &
                                              proc_start_line, num_args, &
                                              arg_has_intent)
        character(len=*), intent(in) :: line_lower
        integer, intent(in) :: line_num
        logical, intent(inout) :: in_procedure
        integer, intent(inout) :: proc_start_line
        integer, intent(inout) :: num_args
        logical, intent(inout) :: arg_has_intent(:)

        in_procedure = .true.
        proc_start_line = line_num
        call parse_argument_count(line_lower, num_args)

        arg_has_intent = .false.
    end subroutine start_procedure_from_signature

    subroutine parse_argument_count(line_lower, num_args)
        character(len=*), intent(in) :: line_lower
        integer, intent(out) :: num_args

        integer :: arg_start, arg_end
        integer :: i
        character(len=:), allocatable :: proc_args

        arg_start = index(line_lower, "(")
        arg_end = index(line_lower, ")")
        if (arg_start > 0 .and. arg_end > arg_start) then
            proc_args = line_lower(arg_start + 1:arg_end - 1)
            num_args = 1
            do i = 1, len(proc_args)
                if (proc_args(i:i) == ",") num_args = num_args + 1
            end do
        else
            num_args = 0
        end if
    end subroutine parse_argument_count

    subroutine finish_procedure_if_needed(violations, violation_count, &
                                          proc_start_line, in_procedure, &
                                          num_args, arg_has_intent)
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        integer, intent(in) :: proc_start_line
        logical, intent(in) :: in_procedure
        integer, intent(in) :: num_args
        logical, intent(in) :: arg_has_intent(:)

        integer :: i

        if (.not. in_procedure) return
        if (num_args <= 0) return

        do i = 1, min(num_args, size(arg_has_intent))
            if (.not. arg_has_intent(i)) then
                call push_missing_intent_violation(violations, violation_count, &
                                                   proc_start_line)
                exit
            end if
        end do
    end subroutine finish_procedure_if_needed

    subroutine mark_all_args_have_intent(num_args, arg_has_intent)
        integer, intent(in) :: num_args
        logical, intent(inout) :: arg_has_intent(:)

        integer :: i

        do i = 1, min(num_args, size(arg_has_intent))
            arg_has_intent(i) = .true.
        end do
    end subroutine mark_all_args_have_intent

    subroutine push_missing_intent_violation(violations, violation_count, line_num)
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        integer, intent(in) :: line_num

        type(source_range_t) :: location

        violation_count = violation_count + 1
        if (violation_count <= size(violations)) then
            location%start%line = line_num
            location%start%column = 1
            location%end%line = line_num
            location%end%column = 1
            violations(violation_count) = create_diagnostic( &
                                          code="F008", &
                                          message="Missing intent declaration for " // &
                                                  "procedure arguments", &
                                          file_path=current_filename, &
                                          location=location, &
                                          severity=SEVERITY_WARNING)
        end if
    end subroutine push_missing_intent_violation

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
