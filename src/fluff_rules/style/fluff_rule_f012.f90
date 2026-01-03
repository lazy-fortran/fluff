module fluff_rule_f012
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_file_context, only: current_filename, current_source_text
    implicit none
    private

    public :: check_f012_naming_conventions

contains

    subroutine check_f012_naming_conventions(ctx, node_index, violations)
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

        call analyze_naming_conventions_from_text(current_source_text, &
                                                  temp_violations, &
                                                  violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = temp_violations(1:violation_count)
        end if
    end subroutine check_f012_naming_conventions

    subroutine analyze_naming_conventions_from_text(source_text, violations, &
                                                    violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        character(len=64) :: var_names(1000)
        integer :: num_vars
        integer :: snake_count, camel_count, pascal_count

        var_names = ""
        num_vars = 0
        call collect_declared_variable_names(source_text, var_names, num_vars)
        if (num_vars <= 0) return

        call count_naming_styles(var_names, num_vars, snake_count, camel_count, &
                                 pascal_count)

        if (has_mixed_naming_styles(snake_count, camel_count, pascal_count)) then
            call push_inconsistent_naming_violation(violations, violation_count)
        end if
    end subroutine analyze_naming_conventions_from_text

    subroutine collect_declared_variable_names(source_text, var_names, num_vars)
        character(len=*), intent(in) :: source_text
        character(len=*), intent(inout) :: var_names(:)
        integer, intent(inout) :: num_vars

        integer :: pos, line_num
        logical :: done
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_trimmed
        character(len=:), allocatable :: line_lower
        integer :: double_colon_pos

        pos = 1
        line_num = 0

        do
            call next_source_line(source_text, pos, line_num, line_content, done)
            if (done) exit
            if (is_comment_line(line_content)) cycle

            line_trimmed = adjustl(line_content)
            line_lower = to_lower(line_trimmed)
            double_colon_pos = index(line_trimmed, "::")
            if (double_colon_pos <= 0) cycle

            if (has_type_keyword(line_lower(1:double_colon_pos))) then
                call extract_variable_names(line_trimmed(double_colon_pos + 2:), &
                                            var_names, num_vars)
            end if
        end do
    end subroutine collect_declared_variable_names

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

    subroutine count_naming_styles(var_names, num_vars, snake_count, camel_count, &
                                   pascal_count)
        character(len=*), intent(in) :: var_names(:)
        integer, intent(in) :: num_vars
        integer, intent(out) :: snake_count
        integer, intent(out) :: camel_count
        integer, intent(out) :: pascal_count

        integer :: i
        character(len=64) :: var_name

        snake_count = 0
        camel_count = 0
        pascal_count = 0

        do i = 1, min(num_vars, size(var_names))
            var_name = trim(var_names(i))
            if (len_trim(var_name) == 0) cycle
            if (is_snake_case(var_name)) snake_count = snake_count + 1
            if (is_camel_case(var_name)) camel_count = camel_count + 1
            if (is_pascal_case(var_name)) pascal_count = pascal_count + 1
        end do
    end subroutine count_naming_styles

    logical function has_mixed_naming_styles(snake_count, camel_count, &
                                            pascal_count) result(has_mixed)
        integer, intent(in) :: snake_count
        integer, intent(in) :: camel_count
        integer, intent(in) :: pascal_count

        has_mixed = (snake_count > 0 .and. camel_count > 0) .or. &
                    (snake_count > 0 .and. pascal_count > 0) .or. &
                    (camel_count > 0 .and. pascal_count > 0)
    end function has_mixed_naming_styles

    subroutine push_inconsistent_naming_violation(violations, violation_count)
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        type(source_range_t) :: location

        violation_count = violation_count + 1
        if (violation_count <= size(violations)) then
            location%start%line = 1
            location%start%column = 1
            location%end%line = 1
            location%end%column = 1
            violations(violation_count) = create_diagnostic( &
                                          code="F012", &
                                          message="Inconsistent naming convention", &
                                          file_path=current_filename, &
                                          location=location, &
                                          severity=SEVERITY_INFO)
        end if
    end subroutine push_inconsistent_naming_violation

    logical function has_type_keyword(str) result(has_type)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: trimmed

        trimmed = adjustl(str)
        has_type = index(trimmed, "integer") > 0 .or. &
                   index(trimmed, "real") > 0 .or. &
                   index(trimmed, "character") > 0 .or. &
                   index(trimmed, "logical") > 0 .or. &
                   index(trimmed, "complex") > 0 .or. &
                   index(trimmed, "type(") > 0 .or. &
                   index(trimmed, "class(") > 0 .or. &
                   index(trimmed, "double precision") > 0
    end function has_type_keyword

    subroutine extract_variable_names(decl_str, var_names, num_vars)
        character(len=*), intent(in) :: decl_str
        character(len=*), intent(inout) :: var_names(:)
        integer, intent(inout) :: num_vars

        character(len=:), allocatable :: trimmed
        integer :: start_pos, comma_pos, eq_pos, paren_pos
        character(len=:), allocatable :: var_part

        trimmed = adjustl(decl_str)
        start_pos = 1

        do while (start_pos <= len(trimmed))
            comma_pos = index(trimmed(start_pos:), ",")
            if (comma_pos == 0) then
                var_part = trimmed(start_pos:)
            else
                var_part = trimmed(start_pos:start_pos + comma_pos - 2)
            end if

            eq_pos = index(var_part, "=")
            if (eq_pos > 0) then
                var_part = var_part(1:eq_pos - 1)
            end if

            paren_pos = index(var_part, "(")
            if (paren_pos > 0) then
                var_part = var_part(1:paren_pos - 1)
            end if

            var_part = adjustl(trim(var_part))
            if (len(var_part) > 0) then
                num_vars = num_vars + 1
                if (num_vars <= size(var_names)) then
                    var_names(num_vars) = var_part
                end if
            end if

            if (comma_pos == 0) exit
            start_pos = start_pos + comma_pos
        end do
    end subroutine extract_variable_names

    logical function is_snake_case(name) result(is_snake)
        character(len=*), intent(in) :: name
        integer :: i

        is_snake = .true.
        do i = 1, len(name)
            if (name(i:i) >= "A" .and. name(i:i) <= "Z") then
                is_snake = .false.
                return
            end if
        end do

        is_snake = index(name, "_") > 0 .or. len(name) <= 4
    end function is_snake_case

    logical function is_camel_case(name) result(is_camel)
        character(len=*), intent(in) :: name
        logical :: has_upper, has_lower
        integer :: i

        is_camel = .false.
        has_upper = .false.
        has_lower = .false.

        if (name(1:1) >= "A" .and. name(1:1) <= "Z") return

        do i = 1, len(name)
            if (name(i:i) >= "a" .and. name(i:i) <= "z") has_lower = .true.
            if (name(i:i) >= "A" .and. name(i:i) <= "Z") has_upper = .true.
        end do

        is_camel = has_upper .and. has_lower .and. index(name, "_") == 0
    end function is_camel_case

    logical function is_pascal_case(name) result(is_pascal)
        character(len=*), intent(in) :: name
        logical :: has_upper, has_lower
        integer :: i

        is_pascal = .false.
        has_upper = .false.
        has_lower = .false.

        if (name(1:1) < "A" .or. name(1:1) > "Z") return

        do i = 1, len(name)
            if (name(i:i) >= "a" .and. name(i:i) <= "z") has_lower = .true.
            if (name(i:i) >= "A" .and. name(i:i) <= "Z") has_upper = .true.
        end do

        is_pascal = has_upper .and. has_lower .and. index(name, "_") == 0
    end function is_pascal_case

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

end module fluff_rule_f012
