module fluff_rule_f015
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: token_t, tokenize_core_with_trivia
    use lexer_token_types, only: TK_KEYWORD, TK_NEWLINE, TK_NUMBER, TK_OPERATOR, &
                                 TK_WHITESPACE
    implicit none
    private

    public :: check_f015_redundant_continue

contains

    subroutine check_f015_redundant_continue(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        character(len=:), allocatable :: source_text
        logical :: found
        type(token_t), allocatable :: tokens(:)
        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count
        integer, allocatable :: referenced_labels(:)

        allocate (tmp(0))
        violation_count = 0

        call ctx%get_source_text(source_text, found)
        if (.not. found) then
            allocate (violations(0))
            return
        end if

        call tokenize_core_with_trivia(source_text, tokens)

        call collect_label_references(tokens, referenced_labels)
        call scan_continue_statements(tokens, referenced_labels, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f015_redundant_continue

    subroutine collect_label_references(tokens, referenced_labels)
        type(token_t), allocatable, intent(in) :: tokens(:)
        integer, allocatable, intent(out) :: referenced_labels(:)

        integer :: i
        character(len=:), allocatable :: keyword

        if (allocated(referenced_labels)) deallocate (referenced_labels)
        allocate (referenced_labels(0))

        if (.not. allocated(tokens)) return
        if (size(tokens) <= 0) return

        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_KEYWORD) cycle
            if (.not. allocated(tokens(i)%text)) cycle
            keyword = to_lower_ascii(trim(tokens(i)%text))

            if (keyword == "goto") then
                call collect_goto_targets(tokens, i, referenced_labels)
            else if (keyword == "go") then
                call collect_go_to_targets(tokens, i, referenced_labels)
            else if (keyword == "do") then
                call collect_do_label(tokens, i, referenced_labels)
            else if (keyword == "if") then
                call collect_arithmetic_if_targets(tokens, i, referenced_labels)
            end if
        end do
    end subroutine collect_label_references

    subroutine collect_goto_targets(tokens, goto_idx, referenced_labels)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: goto_idx
        integer, allocatable, intent(inout) :: referenced_labels(:)

        integer :: line
        integer :: i
        integer :: label
        logical :: ok
        logical :: in_list

        line = tokens(goto_idx)%line
        in_list = .false.

        do i = goto_idx + 1, size(tokens)
            if (tokens(i)%line /= line) exit
            if (tokens(i)%kind == TK_OPERATOR) then
                if (allocated(tokens(i)%text)) then
                    if (tokens(i)%text == "(") then
                        in_list = .true.
                    else if (tokens(i)%text == ")") then
                        in_list = .false.
                        exit
                    end if
                end if
            end if

            if (tokens(i)%kind /= TK_NUMBER) cycle
            if (.not. allocated(tokens(i)%text)) cycle
            call parse_label(tokens(i)%text, label, ok)
            if (.not. ok) cycle
            call push_int_unique(referenced_labels, label)
            if (.not. in_list) exit
        end do
    end subroutine collect_goto_targets

    subroutine collect_go_to_targets(tokens, go_idx, referenced_labels)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: go_idx
        integer, allocatable, intent(inout) :: referenced_labels(:)

        integer :: i
        character(len=:), allocatable :: keyword

        i = next_nontrivia_same_line(tokens, go_idx + 1)
        if (i <= 0) return
        if (tokens(i)%kind /= TK_KEYWORD) return
        if (.not. allocated(tokens(i)%text)) return
        keyword = to_lower_ascii(trim(tokens(i)%text))
        if (keyword /= "to") return

        call collect_goto_targets(tokens, i, referenced_labels)
    end subroutine collect_go_to_targets

    subroutine collect_do_label(tokens, do_idx, referenced_labels)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: do_idx
        integer, allocatable, intent(inout) :: referenced_labels(:)

        integer :: i
        integer :: label
        logical :: ok

        i = next_nontrivia_same_line(tokens, do_idx + 1)
        if (i <= 0) return
        if (tokens(i)%kind /= TK_NUMBER) return
        if (.not. allocated(tokens(i)%text)) return
        call parse_label(tokens(i)%text, label, ok)
        if (.not. ok) return
        call push_int_unique(referenced_labels, label)
    end subroutine collect_do_label

    subroutine collect_arithmetic_if_targets(tokens, if_idx, referenced_labels)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: if_idx
        integer, allocatable, intent(inout) :: referenced_labels(:)

        integer :: line
        integer :: i
        integer :: last_close_paren
        integer :: commas_found
        integer :: candidate(3)
        integer :: candidate_count
        integer :: label
        logical :: ok

        line = tokens(if_idx)%line
        last_close_paren = 0

        do i = if_idx + 1, size(tokens)
            if (tokens(i)%line /= line) exit
            if (tokens(i)%kind /= TK_OPERATOR) cycle
            if (.not. allocated(tokens(i)%text)) cycle
            if (tokens(i)%text == ")") last_close_paren = i
        end do
        if (last_close_paren <= 0) return

        commas_found = 0
        candidate = 0
        candidate_count = 0
        do i = last_close_paren + 1, size(tokens)
            if (tokens(i)%line /= line) exit
            if (tokens(i)%kind == TK_OPERATOR) then
                if (allocated(tokens(i)%text)) then
                    if (tokens(i)%text == ",") commas_found = commas_found + 1
                end if
            else if (tokens(i)%kind == TK_NUMBER) then
                if (.not. allocated(tokens(i)%text)) cycle
                call parse_label(tokens(i)%text, label, ok)
                if (.not. ok) cycle
                candidate_count = candidate_count + 1
                if (candidate_count <= 3) candidate(candidate_count) = label
            end if
        end do

        if (candidate_count >= 3 .and. commas_found >= 2) then
            do i = 1, 3
                call push_int_unique(referenced_labels, candidate(i))
            end do
        end if
    end subroutine collect_arithmetic_if_targets

    subroutine scan_continue_statements(tokens, referenced_labels, tmp, &
                                        violation_count)
        type(token_t), allocatable, intent(in) :: tokens(:)
        integer, intent(in) :: referenced_labels(:)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: first_idx
        integer :: current_line

        if (.not. allocated(tokens)) return
        if (size(tokens) <= 0) return

        current_line = -1
        first_idx = 0
        do first_idx = 1, size(tokens)
            if (tokens(first_idx)%line /= current_line) then
                current_line = tokens(first_idx)%line
                call check_continue_line(tokens, first_idx, referenced_labels, tmp, &
                                         violation_count)
            end if
        end do
    end subroutine scan_continue_statements

    subroutine check_continue_line(tokens, line_start, referenced_labels, tmp, &
                                   violation_count)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: line_start
        integer, intent(in) :: referenced_labels(:)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: line
        integer :: idx0
        integer :: idx1
        integer :: label
        logical :: ok
        character(len=:), allocatable :: keyword

        line = tokens(line_start)%line
        idx0 = first_nontrivia_in_line(tokens, line_start)
        if (idx0 <= 0) return

        idx1 = idx0
        if (tokens(idx0)%kind == TK_NUMBER) then
            idx1 = next_nontrivia_same_line(tokens, idx0 + 1)
            if (idx1 <= 0) return
        end if

        if (tokens(idx1)%kind /= TK_KEYWORD) return
        if (.not. allocated(tokens(idx1)%text)) return
        keyword = to_lower_ascii(trim(tokens(idx1)%text))
        if (keyword /= "continue") return

        if (tokens(idx0)%kind /= TK_NUMBER) then
            call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                 code="F015", &
                                 message="Redundant CONTINUE statement", &
                                 file_path=current_filename, &
                                 location=token_location(tokens(idx1)), &
                                 severity=SEVERITY_INFO))
            return
        end if

        call parse_label(tokens(idx0)%text, label, ok)
        if (.not. ok) return

        if (.not. is_label_referenced(referenced_labels, label)) then
            call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                 code="F015", &
                                 message="Redundant CONTINUE statement", &
                                 file_path=current_filename, &
                                 location=token_location(tokens(idx1)), &
                                 severity=SEVERITY_INFO))
        end if
    end subroutine check_continue_line

    pure logical function is_label_referenced(referenced_labels, label) result(ok)
        integer, intent(in) :: referenced_labels(:)
        integer, intent(in) :: label

        integer :: i

        ok = .false.
        do i = 1, size(referenced_labels)
            if (referenced_labels(i) == label) then
                ok = .true.
                return
            end if
        end do
    end function is_label_referenced

    integer function first_nontrivia_in_line(tokens, start_idx) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx

        integer :: line
        integer :: i

        idx = 0
        line = tokens(start_idx)%line
        do i = start_idx, size(tokens)
            if (tokens(i)%line /= line) exit
            if (tokens(i)%kind == TK_NEWLINE) cycle
            if (tokens(i)%kind == TK_WHITESPACE) cycle
            idx = i
            return
        end do
    end function first_nontrivia_in_line

    integer function next_nontrivia_same_line(tokens, start_idx) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx

        integer :: line
        integer :: i

        idx = 0
        if (start_idx <= 0) return
        if (start_idx > size(tokens)) return
        line = tokens(start_idx)%line

        do i = start_idx, size(tokens)
            if (tokens(i)%line /= line) exit
            if (tokens(i)%kind == TK_NEWLINE) cycle
            if (tokens(i)%kind == TK_WHITESPACE) cycle
            idx = i
            return
        end do
    end function next_nontrivia_same_line

    subroutine parse_label(text, label, ok)
        character(len=*), intent(in) :: text
        integer, intent(out) :: label
        logical, intent(out) :: ok

        integer :: ios

        label = 0
        ok = .false.
        read (text, *, iostat=ios) label
        ok = (ios == 0)
    end subroutine parse_label

    subroutine push_int_unique(values, value)
        integer, allocatable, intent(inout) :: values(:)
        integer, intent(in) :: value

        integer, allocatable :: tmp(:)
        integer :: i

        do i = 1, size(values)
            if (values(i) == value) return
        end do

        allocate (tmp(size(values) + 1))
        if (size(values) > 0) tmp(1:size(values)) = values
        tmp(size(values) + 1) = value
        call move_alloc(tmp, values)
    end subroutine push_int_unique

    pure function token_location(tok) result(location)
        type(token_t), intent(in) :: tok
        type(source_range_t) :: location
        integer :: end_col

        location%start%line = tok%line
        location%start%column = tok%column
        location%end%line = tok%line
        end_col = tok%column
        if (allocated(tok%text)) end_col = end_col + len(tok%text) - 1
        location%end%column = end_col
    end function token_location

end module fluff_rule_f015
