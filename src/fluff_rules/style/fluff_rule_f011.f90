module fluff_rule_f011
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_file_context, only: current_filename, current_source_text
    implicit none
    private

    public :: check_f011_missing_end_labels

contains

    subroutine check_f011_missing_end_labels(ctx, node_index, violations)
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

        call analyze_missing_end_labels_from_text(current_source_text, &
                                                  temp_violations, &
                                                  violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = temp_violations(1:violation_count)
        end if
    end subroutine check_f011_missing_end_labels

    subroutine analyze_missing_end_labels_from_text(source_text, violations, &
                                                    violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        integer :: pos, line_num
        logical :: done
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_trimmed
        character(len=:), allocatable :: line_lower
        character(len=64) :: stack_names(100)
        integer :: stack_depth
        integer :: name_start, name_end

        pos = 1
        line_num = 0
        stack_depth = 0
        stack_names = ""

        do
            call next_source_line(source_text, pos, line_num, line_content, done)
            if (done) exit
            if (is_comment_line(line_content)) cycle

            line_trimmed = adjustl(line_content)
            line_lower = to_lower(line_trimmed)

            call handle_start_construct(line_lower, line_trimmed, stack_names, &
                                        stack_depth, name_start, name_end)
            call handle_end_construct(line_lower, line_num, violations, &
                                      violation_count, stack_depth)
        end do
    end subroutine analyze_missing_end_labels_from_text

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

    subroutine handle_start_construct(line_lower, line_trimmed, stack_names, &
                                      stack_depth, name_start, name_end)
        character(len=*), intent(in) :: line_lower
        character(len=*), intent(in) :: line_trimmed
        character(len=*), intent(inout) :: stack_names(:)
        integer, intent(inout) :: stack_depth
        integer, intent(inout) :: name_start
        integer, intent(inout) :: name_end

        if (index(line_lower, "program ") == 1) then
            call push_construct_name(stack_names, stack_depth, line_trimmed, 9)
        else if (index(line_lower, "module ") == 1 .and. &
                 index(line_lower, "module procedure") /= 1) then
            call push_construct_name(stack_names, stack_depth, line_trimmed, 8)
        else if (index(line_lower, "subroutine ") == 1) then
            call push_construct_name(stack_names, stack_depth, line_trimmed, 12)
        else if (is_function_start(line_lower)) then
            name_start = index(line_lower, "function ") + 9
            name_end = len(line_trimmed)
            stack_depth = stack_depth + 1
            if (stack_depth <= size(stack_names)) then
                stack_names(stack_depth) = &
                    extract_name(line_trimmed(name_start:name_end))
            end if
        end if
    end subroutine handle_start_construct

    subroutine push_construct_name(stack_names, stack_depth, line_trimmed, start_pos)
        character(len=*), intent(inout) :: stack_names(:)
        integer, intent(inout) :: stack_depth
        character(len=*), intent(in) :: line_trimmed
        integer, intent(in) :: start_pos

        stack_depth = stack_depth + 1
        if (stack_depth <= size(stack_names)) then
            stack_names(stack_depth) = extract_name(line_trimmed(start_pos:))
        end if
    end subroutine push_construct_name

    logical function is_function_start(line_lower) result(is_start)
        character(len=*), intent(in) :: line_lower

        is_start = index(line_lower, "function ") > 0 .and. &
                   (index(line_lower, "function ") == 1 .or. &
                    index(line_lower, "pure function ") == 1 .or. &
                    index(line_lower, "elemental function ") == 1)
    end function is_function_start

    subroutine handle_end_construct(line_lower, line_num, violations, &
                                    violation_count, stack_depth)
        character(len=*), intent(in) :: line_lower
        integer, intent(in) :: line_num
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        integer, intent(inout) :: stack_depth

        if (index(line_lower, "end program") == 1) then
            if (len_trim(line_lower) == 11) then
                call push_end_label_violation(violations, violation_count, &
                                              line_num, "program", 11)
            end if
            if (stack_depth > 0) stack_depth = stack_depth - 1
        else if (is_end_module(line_lower)) then
            if (len_trim(line_lower) == 10) then
                call push_end_label_violation(violations, violation_count, &
                                              line_num, "module", 10)
            end if
            if (stack_depth > 0) stack_depth = stack_depth - 1
        else if (index(line_lower, "end subroutine") == 1) then
            if (len_trim(line_lower) == 14) then
                call push_end_label_violation(violations, violation_count, &
                                              line_num, "subroutine", 14)
            end if
            if (stack_depth > 0) stack_depth = stack_depth - 1
        else if (index(line_lower, "end function") == 1) then
            if (len_trim(line_lower) == 12) then
                call push_end_label_violation(violations, violation_count, &
                                              line_num, "function", 12)
            end if
            if (stack_depth > 0) stack_depth = stack_depth - 1
        end if
    end subroutine handle_end_construct

    logical function is_end_module(line_lower) result(is_end)
        character(len=*), intent(in) :: line_lower

        is_end = index(line_lower, "end module") == 1 .and. &
                 index(line_lower, "end module procedure") /= 1
    end function is_end_module

    subroutine push_end_label_violation(violations, violation_count, line_num, &
                                        kind_name, end_len)
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        integer, intent(in) :: line_num
        character(len=*), intent(in) :: kind_name
        integer, intent(in) :: end_len

        type(source_range_t) :: location

        violation_count = violation_count + 1
        if (violation_count <= size(violations)) then
            location%start%line = line_num
            location%start%column = 1
            location%end%line = line_num
            location%end%column = end_len
            violations(violation_count) = create_diagnostic( &
                                          code="F011", &
                                          message="Missing end label for " // &
                                                  trim(kind_name), &
                                          file_path=current_filename, &
                                          location=location, &
                                          severity=SEVERITY_INFO)
        end if
    end subroutine push_end_label_violation

    function extract_name(str) result(name)
        character(len=*), intent(in) :: str
        character(len=64) :: name
        integer :: paren_pos, space_pos
        character(len=:), allocatable :: trimmed

        name = ""
        trimmed = adjustl(str)

        paren_pos = index(trimmed, "(")
        space_pos = index(trimmed, " ")

        if (paren_pos > 0) then
            name = trimmed(1:paren_pos - 1)
        else if (space_pos > 0) then
            name = trimmed(1:space_pos - 1)
        else
            name = trimmed
        end if
    end function extract_name

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

end module fluff_rule_f011
