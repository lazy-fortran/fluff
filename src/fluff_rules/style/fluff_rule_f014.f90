module fluff_rule_f014
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_diagnostic_utils, only: push_diagnostic
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: token_t, tokenize_core_with_trivia
    use lexer_token_types, only: TK_IDENTIFIER, TK_NUMBER, TK_OPERATOR, TK_STRING
    implicit none
    private

    public :: check_f014_unnecessary_parentheses

contains

    subroutine check_f014_unnecessary_parentheses(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        character(len=:), allocatable :: source_text
        logical :: found
        type(token_t), allocatable :: tokens(:)
        integer, allocatable :: close_for_open(:)
        integer, allocatable :: open_for_close(:)
        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        call ctx%get_source_text(source_text, found)
        if (.not. found) then
            allocate (violations(0))
            return
        end if

        call tokenize_core_with_trivia(source_text, tokens)

        allocate (tmp(0))
        violation_count = 0

        call build_paren_pairs(tokens, close_for_open, open_for_close)
        call find_unnecessary_parens(tokens, close_for_open, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f014_unnecessary_parentheses

    subroutine build_paren_pairs(tokens, close_for_open, open_for_close)
        type(token_t), allocatable, intent(in) :: tokens(:)
        integer, allocatable, intent(out) :: close_for_open(:)
        integer, allocatable, intent(out) :: open_for_close(:)

        integer, allocatable :: stack(:)
        integer :: top
        integer :: i
        integer :: open_idx

        if (allocated(close_for_open)) deallocate (close_for_open)
        if (allocated(open_for_close)) deallocate (open_for_close)
        if (.not. allocated(tokens)) then
            allocate (close_for_open(0))
            allocate (open_for_close(0))
            return
        end if
        if (size(tokens) <= 0) then
            allocate (close_for_open(0))
            allocate (open_for_close(0))
            return
        end if

        allocate (close_for_open(size(tokens)))
        allocate (open_for_close(size(tokens)))
        close_for_open = 0
        open_for_close = 0

        allocate (stack(size(tokens)))
        top = 0

        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_OPERATOR) cycle
            if (.not. allocated(tokens(i)%text)) cycle
            if (tokens(i)%text == "(") then
                top = top + 1
                stack(top) = i
            else if (tokens(i)%text == ")") then
                if (top <= 0) cycle
                open_idx = stack(top)
                top = top - 1
                close_for_open(open_idx) = i
                open_for_close(i) = open_idx
            end if
        end do
        deallocate (stack)
    end subroutine build_paren_pairs

    subroutine find_unnecessary_parens(tokens, close_for_open, tmp, violation_count)
        type(token_t), allocatable, intent(in) :: tokens(:)
        integer, intent(in) :: close_for_open(:)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: open_idx
        integer :: close_idx

        if (.not. allocated(tokens)) return
        if (size(tokens) <= 0) return

        do open_idx = 1, size(tokens)
            close_idx = close_for_open(open_idx)
            if (close_idx <= 0) cycle
            if (tokens(open_idx)%line /= tokens(close_idx)%line) cycle
            if (open_idx > 1) then
                if (tokens(open_idx - 1)%kind == TK_IDENTIFIER) cycle
            end if
            if (is_double_parens(tokens, close_for_open, open_idx, close_idx)) then
                call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                     code="F014", &
                                  message="Unnecessary parentheses around expression", &
                                     file_path=current_filename, &
                                     location=paren_location(tokens(open_idx), &
                                                             tokens(close_idx)), &
                                     severity=SEVERITY_INFO))
            else if (is_simple_parens(tokens, open_idx, close_idx)) then
                call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                     code="F014", &
                           message="Unnecessary parentheses around simple expression", &
                                     file_path=current_filename, &
                                     location=paren_location(tokens(open_idx), &
                                                             tokens(close_idx)), &
                                     severity=SEVERITY_INFO))
            end if
        end do
    end subroutine find_unnecessary_parens

    pure logical function is_simple_parens(tokens, open_idx, close_idx) result(ok)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: open_idx
        integer, intent(in) :: close_idx

        integer :: inner_idx

        ok = .false.
        if (close_idx /= open_idx + 2) return
        inner_idx = open_idx + 1
        ok = (tokens(inner_idx)%kind == TK_IDENTIFIER .or. &
              tokens(inner_idx)%kind == TK_NUMBER .or. &
              tokens(inner_idx)%kind == TK_STRING)
    end function is_simple_parens

    pure logical function is_double_parens(tokens, close_for_open, open_idx, &
                                           close_idx) result(ok)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: close_for_open(:)
        integer, intent(in) :: open_idx
        integer, intent(in) :: close_idx

        integer :: inner_open
        integer :: inner_close

        ok = .false.
        if (open_idx + 1 >= close_idx) return
        if (tokens(open_idx + 1)%kind /= TK_OPERATOR) return
        if (.not. allocated(tokens(open_idx + 1)%text)) return
        if (tokens(open_idx + 1)%text /= "(") return

        inner_open = open_idx + 1
        inner_close = close_for_open(inner_open)
        if (inner_close <= 0) return
        if (inner_close /= close_idx - 1) return

        if (tokens(close_idx - 1)%kind /= TK_OPERATOR) return
        if (.not. allocated(tokens(close_idx - 1)%text)) return
        if (tokens(close_idx - 1)%text /= ")") return

        ok = .true.
    end function is_double_parens

    pure function paren_location(open_tok, close_tok) result(location)
        type(token_t), intent(in) :: open_tok
        type(token_t), intent(in) :: close_tok
        type(source_range_t) :: location

        location%start%line = open_tok%line
        location%start%column = open_tok%column
        location%end%line = close_tok%line
        location%end%column = close_tok%column
    end function paren_location

end module fluff_rule_f014
