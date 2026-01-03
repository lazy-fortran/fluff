module fluff_rule_f014
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_file_context, only: current_filename, current_source_text
    implicit none
    private

    public :: check_f014_unnecessary_parentheses

contains

    subroutine check_f014_unnecessary_parentheses(ctx, node_index, violations)
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

        call analyze_unnecessary_parentheses_from_text(current_source_text, &
                                                       temp_violations, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = temp_violations(1:violation_count)
        end if
    end subroutine check_f014_unnecessary_parentheses

    subroutine analyze_unnecessary_parentheses_from_text(source_text, violations, &
                                                         violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_trimmed
        integer :: i, paren_end
        character(len=:), allocatable :: inner_content

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
            if (.not. is_comment_line(line_content)) then
                line_trimmed = adjustl(line_content)

                i = 1
                do while (i <= len(line_trimmed) - 2)
                    if (line_trimmed(i:i) == "(") then
                        paren_end = find_matching_paren(line_trimmed, i)
                        if (paren_end > 0) then
                            inner_content = line_trimmed(i + 1:paren_end - 1)
                            inner_content = adjustl(trim(inner_content))

                            if (is_simple_expression(inner_content)) then
                                if (i == 1 .or. .not. &
                                    is_alnum_or_underscore(line_trimmed(i - 1:i &
                                                                        - 1))) then
                                    if (.not. &
                                        is_in_control_structure(line_trimmed, i)) then
                                        if (.not. needs_precedence_parentheses( &
                                            line_trimmed, i, paren_end)) then
                                            violation_count = violation_count + 1
                                            if (violation_count <= &
                                                size(violations)) then
                                                location%start%line = line_num
                                                location%start%column = i
                                                location%end%line = line_num
                                                location%end%column = paren_end
                                                violations(violation_count) = &
                                                    create_diagnostic( &
                                                    code="F014", &
                           message="Unnecessary parentheses around simple expression", &
                                                    file_path=current_filename, &
                                                    location=location, &
                                                    severity=SEVERITY_INFO)
                                            end if
                                        end if
                                    end if
                                end if
                            end if
                            i = paren_end + 1
                        else
                            i = i + 1
                        end if
                    else
                        i = i + 1
                    end if
                end do
            end if

            if (next_pos == 0) exit
        end do
    end subroutine analyze_unnecessary_parentheses_from_text

    integer function find_matching_paren(str, start_pos) result(end_pos)
        character(len=*), intent(in) :: str
        integer, intent(in) :: start_pos
        integer :: depth, i

        depth = 1
        end_pos = 0

        do i = start_pos + 1, len(str)
            if (str(i:i) == "(") then
                depth = depth + 1
            else if (str(i:i) == ")") then
                depth = depth - 1
                if (depth == 0) then
                    end_pos = i
                    return
                end if
            end if
        end do
    end function find_matching_paren

    logical function is_simple_expression(expr) result(is_simple)
        character(len=*), intent(in) :: expr
        integer :: i
        logical :: has_operator
        character(len=:), allocatable :: trimmed_expr

        trimmed_expr = adjustl(trim(expr))
        has_operator = .false.

        if (len(trimmed_expr) == 0) then
            is_simple = .false.
            return
        end if

        do i = 1, len(trimmed_expr)
            if (trimmed_expr(i:i) == "+" .or. trimmed_expr(i:i) == "-" .or. &
                trimmed_expr(i:i) == "*" .or. trimmed_expr(i:i) == "/" .or. &
                trimmed_expr(i:i) == "(" .or. trimmed_expr(i:i) == ")" .or. &
                trimmed_expr(i:i) == "=" .or. trimmed_expr(i:i) == "<" .or. &
                trimmed_expr(i:i) == ">" .or. trimmed_expr(i:i) == "." .or. &
                trimmed_expr(i:i) == "&") then
                has_operator = .true.
                exit
            end if
        end do

        is_simple = .not. has_operator .and. len(trimmed_expr) > 0
    end function is_simple_expression

    logical function is_alnum_or_underscore(ch) result(is_alnum)
        character, intent(in) :: ch

        is_alnum = (ch >= "a" .and. ch <= "z") .or. &
                   (ch >= "A" .and. ch <= "Z") .or. &
                   (ch >= "0" .and. ch <= "9") .or. &
                   (ch == "_")
    end function is_alnum_or_underscore

    logical function is_in_control_structure(line, pos) result(in_control)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        character(len=:), allocatable :: line_lower

        line_lower = to_lower(adjustl(line))
        in_control = index(line_lower, "if") > 0 .or. &
                     index(line_lower, "while") > 0 .or. &
                     index(line_lower, "select") > 0 .or. &
                     index(line_lower, "case") > 0
    end function is_in_control_structure

    integer function get_operator_precedence(op) result(prec)
        character(len=*), intent(in) :: op
        character(len=:), allocatable :: op_lower

        op_lower = to_lower(adjustl(trim(op)))
        select case (op_lower)
        case ("**")
            prec = 10
        case ("*", "/")
            prec = 9
        case ("+", "-")
            prec = 7
        case ("//")
            prec = 6
        case ("==", "/=", "<", "<=", ">", ">=", ".eq.", ".ne.", ".lt.", ".le.", &
              ".gt.", ".ge.")
            prec = 5
        case (".not.")
            prec = 4
        case (".and.")
            prec = 3
        case (".or.")
            prec = 2
        case (".eqv.", ".neqv.")
            prec = 1
        case default
            prec = 0
        end select
    end function get_operator_precedence

    function get_operator_at(line, pos, direction) result(op)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        integer, intent(in) :: direction
        character(len=10) :: op
        integer :: i, start_i, end_i
        character(len=:), allocatable :: line_lower

        op = ""
        line_lower = to_lower(line)

        if (direction < 0) then
            if (pos < 1) return
            if (pos >= 2) then
                if (line(pos - 1:pos) == "**") then
                    op = "**"
                    return
                else if (line(pos - 1:pos) == "//" .or. line(pos - 1:pos) == "==" .or. &
                         line(pos - 1:pos) == "/=" .or. line(pos - 1:pos) == "<=" .or. &
                         line(pos - 1:pos) == ">=") then
                    op = line(pos - 1:pos)
                    return
                end if
            end if

            start_i = pos
            do i = pos, 1, -1
                if (line(i:i) == ".") then
                    start_i = i
                    exit
                else if (line(i:i) == " ") then
                    cycle
                else if (.not. ((line(i:i) >= "a" .and. line(i:i) <= "z") .or. &
                                (line(i:i) >= "A" .and. line(i:i) <= "Z"))) then
                    exit
                end if
            end do
            if (start_i < pos) then
                end_i = index(line(start_i + 1:), ".") + start_i
                if (end_i > start_i) then
                    op = line_lower(start_i:end_i)
                    if (get_operator_precedence(op) > 0) return
                end if
            end if

            if (line(pos:pos) == "*" .or. line(pos:pos) == "/" .or. &
                line(pos:pos) == "+" .or. line(pos:pos) == "-" .or. &
                line(pos:pos) == "<" .or. line(pos:pos) == ">") then
                op = line(pos:pos)
            end if
        else
            if (pos > len(line)) return
            if (pos <= len(line) - 1) then
                if (line(pos:pos + 1) == "**") then
                    op = "**"
                    return
                else if (line(pos:pos + 1) == "//" .or. line(pos:pos + 1) == "==" .or. &
                         line(pos:pos + 1) == "/=" .or. line(pos:pos + 1) == "<=" .or. &
                         line(pos:pos + 1) == ">=") then
                    op = line(pos:pos + 1)
                    return
                end if
            end if
            if (line(pos:pos) == ".") then
                end_i = index(line(pos + 1:), ".") + pos
                if (end_i > pos) then
                    op = line_lower(pos:end_i)
                    if (get_operator_precedence(op) > 0) return
                end if
            end if
            if (line(pos:pos) == "*" .or. line(pos:pos) == "/" .or. &
                line(pos:pos) == "+" .or. line(pos:pos) == "-" .or. &
                line(pos:pos) == "<" .or. line(pos:pos) == ">") then
                op = line(pos:pos)
            end if
        end if
    end function get_operator_at

    integer function get_inner_operator_precedence(expr) result(prec)
        character(len=*), intent(in) :: expr
        integer :: i, depth, min_prec, op_prec
        character(len=:), allocatable :: expr_lower
        character(len=10) :: current_op

        prec = 100
        min_prec = 100
        depth = 0
        expr_lower = to_lower(adjustl(trim(expr)))

        i = 1
        do while (i <= len(expr_lower))
            if (expr_lower(i:i) == "(") then
                depth = depth + 1
                i = i + 1
            else if (expr_lower(i:i) == ")") then
                depth = depth - 1
                i = i + 1
            else if (depth == 0) then
                current_op = ""
                if (i <= len(expr_lower) - 1) then
                    if (expr_lower(i:i + 1) == "**") then
                        current_op = "**"
                        op_prec = 10
                        if (op_prec < min_prec) min_prec = op_prec
                        i = i + 2
                        cycle
                    else if (expr_lower(i:i + 1) == "//" .or. expr_lower(i:i + 1) == &
                             "==" .or. expr_lower(i:i + 1) == "/=" .or. &
                             expr_lower(i:i + 1) == "<=" .or. expr_lower(i:i + 1) == &
                             ">=") then
                        op_prec = get_operator_precedence(expr_lower(i:i + 1))
                        if (op_prec < min_prec) min_prec = op_prec
                        i = i + 2
                        cycle
                    end if
                end if

                if (expr_lower(i:i) == ".") then
                    do while (i <= len(expr_lower))
                        if (i > 1) then
                            if (expr_lower(i:i) == "." .and. &
                                index(expr_lower(1:i - 1), ".") > 0) exit
                        end if
                        i = i + 1
                    end do
                    cycle
                end if

                if (expr_lower(i:i) == "*" .or. expr_lower(i:i) == "/") then
                    op_prec = 9
                    if (op_prec < min_prec) min_prec = op_prec
                else if (expr_lower(i:i) == "+" .or. expr_lower(i:i) == "-") then
                    op_prec = 7
                    if (op_prec < min_prec) min_prec = op_prec
                else if (expr_lower(i:i) == "<" .or. expr_lower(i:i) == ">") then
                    op_prec = 5
                    if (op_prec < min_prec) min_prec = op_prec
                end if
                i = i + 1
            else
                i = i + 1
            end if
        end do

        prec = min_prec
    end function get_inner_operator_precedence

    logical function needs_precedence_parentheses(line, start_pos, end_pos) &
        result(needs_precedence)
        character(len=*), intent(in) :: line
        integer, intent(in) :: start_pos, end_pos
        integer :: before_pos, after_pos, inner_prec, outer_prec
        character(len=:), allocatable :: inner_expr
        character(len=10) :: op_before, op_after

        needs_precedence = .false.

        if (end_pos > start_pos + 1) then
            inner_expr = line(start_pos + 1:end_pos - 1)
        else
            inner_expr = ""
        end if

        inner_prec = get_inner_operator_precedence(inner_expr)
        if (inner_prec >= 100) return

        before_pos = start_pos - 1
        do while (before_pos >= 1 .and. line(before_pos:before_pos) == " ")
            before_pos = before_pos - 1
        end do

        if (before_pos >= 1) then
            op_before = get_operator_at(line, before_pos, -1)
            if (len_trim(op_before) > 0) then
                outer_prec = get_operator_precedence(op_before)
                if (outer_prec > inner_prec) then
                    needs_precedence = .true.
                    return
                end if
            end if
        end if

        after_pos = end_pos + 1
        do while (after_pos <= len(line) .and. line(after_pos:after_pos) == " ")
            after_pos = after_pos + 1
        end do

        if (after_pos <= len(line)) then
            op_after = get_operator_at(line, after_pos, 1)
            if (len_trim(op_after) > 0) then
                outer_prec = get_operator_precedence(op_after)
                if (outer_prec > inner_prec) then
                    needs_precedence = .true.
                    return
                end if
            end if
        end if
    end function needs_precedence_parentheses

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
        is_comment = len_trim(trimmed) > 0 .and. trimmed(1:1) == "!"
    end function is_comment_line

end module fluff_rule_f014
