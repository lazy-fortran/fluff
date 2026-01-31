module fluff_rule_f010
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fluff_text_helpers, only: starts_with
    use fluff_token_helpers, only: token_location, first_nontrivia_in_line, &
                                   next_nontrivia_same_line
    use fortfront, only: comment_node, goto_node, token_t, tokenize_core_with_trivia
    use lexer_token_types, only: TK_KEYWORD, TK_NEWLINE, TK_NUMBER, TK_OPERATOR, &
                                 TK_WHITESPACE, TK_COMMENT
    implicit none
    private

    public :: check_f010_obsolete_features

contains

    subroutine check_f010_obsolete_features(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        character(len=:), allocatable :: source_text
        logical :: found
        type(token_t), allocatable :: tokens(:)
        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count
        integer :: i

        allocate (tmp(0))
        violation_count = 0

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (n => ctx%arena%entries(i)%node)
            type is (goto_node)
                call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                     code="F010", &
                                     message="Obsolete feature: GOTO", &
                                     file_path=current_filename, &
                                     location=ctx%get_node_location(i), &
                                     severity=SEVERITY_WARNING))
            type is (comment_node)
                call check_legacy_comment(ctx, i, n%text, tmp, violation_count)
            end select
        end do

        call ctx%get_source_text(source_text, found)
        if (found) then
            call tokenize_core_with_trivia(source_text, tokens)
            call scan_arithmetic_if(tokens, tmp, violation_count)
        end if

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f010_obsolete_features

    subroutine check_legacy_comment(ctx, node_index, text, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(in) :: text
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        character(len=:), allocatable :: trimmed
        character(len=:), allocatable :: lowered

        if (.not. allocated(text)) return
        trimmed = adjustl(text)
        if (len_trim(trimmed) <= 0) return
        if (trimmed(1:1) == "!") return

        lowered = to_lower_ascii(trimmed)
        if (starts_with(lowered, "common")) then
            call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                 code="F010", &
                                 message="Obsolete feature: COMMON", &
                                 file_path=current_filename, &
                                 location=ctx%get_node_location(node_index), &
                                 severity=SEVERITY_WARNING))
        else if (starts_with(lowered, "equivalence")) then
            call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                 code="F010", &
                                 message="Obsolete feature: EQUIVALENCE", &
                                 file_path=current_filename, &
                                 location=ctx%get_node_location(node_index), &
                                 severity=SEVERITY_WARNING))
        end if
    end subroutine check_legacy_comment

    subroutine scan_arithmetic_if(tokens, tmp, violation_count)
        type(token_t), allocatable, intent(in) :: tokens(:)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: idx
        integer :: line
        integer :: line_start
        integer :: first_idx
        integer :: stmt_idx
        character(len=:), allocatable :: keyword

        if (.not. allocated(tokens)) return
        if (size(tokens) <= 0) return

        line = -1
        do idx = 1, size(tokens)
            if (tokens(idx)%line == line) cycle
            line = tokens(idx)%line
            line_start = idx

            first_idx = first_nontrivia_in_line(tokens, line_start)
            if (first_idx <= 0) cycle

            stmt_idx = first_idx
            if (tokens(first_idx)%kind == TK_NUMBER) then
                stmt_idx = next_nontrivia_same_line(tokens, first_idx + 1)
                if (stmt_idx <= 0) cycle
            end if

            if (tokens(stmt_idx)%kind /= TK_KEYWORD) cycle
            if (.not. allocated(tokens(stmt_idx)%text)) cycle
            keyword = to_lower_ascii(trim(tokens(stmt_idx)%text))
            if (keyword /= "if") cycle

            if (.not. is_arithmetic_if_tail(tokens, stmt_idx)) cycle

            call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                 code="F010", &
                                 message="Obsolete feature: arithmetic IF", &
                                 file_path=current_filename, &
                                 location=token_location(tokens(stmt_idx)), &
                                 severity=SEVERITY_WARNING))
        end do
    end subroutine scan_arithmetic_if

    logical function is_arithmetic_if_tail(tokens, stmt_idx) result(ok)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_idx

        integer :: line
        integer :: last_close_paren
        integer :: idx_label1
        integer :: idx_comma1
        integer :: idx_label2
        integer :: idx_comma2
        integer :: idx_label3
        integer :: j

        ok = .false.
        line = tokens(stmt_idx)%line

        last_close_paren = find_last_close_paren(tokens, stmt_idx, line)
        if (last_close_paren <= 0) return

        idx_label1 = next_nontrivia_same_line(tokens, last_close_paren + 1)
        if (idx_label1 <= 0) return
        if (tokens(idx_label1)%kind /= TK_NUMBER) return

        idx_comma1 = next_nontrivia_same_line(tokens, idx_label1 + 1)
        if (idx_comma1 <= 0) return
        if (tokens(idx_comma1)%kind /= TK_OPERATOR) return
        if (.not. allocated(tokens(idx_comma1)%text)) return
        if (tokens(idx_comma1)%text /= ",") return

        idx_label2 = next_nontrivia_same_line(tokens, idx_comma1 + 1)
        if (idx_label2 <= 0) return
        if (tokens(idx_label2)%kind /= TK_NUMBER) return

        idx_comma2 = next_nontrivia_same_line(tokens, idx_label2 + 1)
        if (idx_comma2 <= 0) return
        if (tokens(idx_comma2)%kind /= TK_OPERATOR) return
        if (.not. allocated(tokens(idx_comma2)%text)) return
        if (tokens(idx_comma2)%text /= ",") return

        idx_label3 = next_nontrivia_same_line(tokens, idx_comma2 + 1)
        if (idx_label3 <= 0) return
        if (tokens(idx_label3)%kind /= TK_NUMBER) return

        do j = idx_label3 + 1, size(tokens)
            if (tokens(j)%line /= line) exit
            if (tokens(j)%kind == TK_NEWLINE) cycle
            if (tokens(j)%kind == TK_WHITESPACE) cycle
            if (tokens(j)%kind == TK_COMMENT) cycle
            return
        end do

        ok = .true.
    end function is_arithmetic_if_tail

    integer function find_last_close_paren(tokens, stmt_idx, line) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_idx
        integer, intent(in) :: line

        integer :: j

        idx = 0
        do j = stmt_idx + 1, size(tokens)
            if (tokens(j)%line /= line) exit
            if (tokens(j)%kind /= TK_OPERATOR) cycle
            if (.not. allocated(tokens(j)%text)) cycle
            if (tokens(j)%text == ")") idx = j
        end do
    end function find_last_close_paren

end module fluff_rule_f010
