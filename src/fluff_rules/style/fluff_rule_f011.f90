module fluff_rule_f011
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: token_t, tokenize_core_with_trivia
    use lexer_token_types, only: TK_IDENTIFIER, TK_KEYWORD
    implicit none
    private

    public :: check_f011_missing_end_labels

contains

    subroutine check_f011_missing_end_labels(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        character(len=:), allocatable :: source_text
        logical :: found
        type(token_t), allocatable :: tokens(:)
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

        call scan_end_statements(tokens, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f011_missing_end_labels

    subroutine scan_end_statements(tokens, tmp, violation_count)
        type(token_t), allocatable, intent(in) :: tokens(:)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: i
        integer :: first_idx
        integer :: current_line

        if (.not. allocated(tokens)) return
        if (size(tokens) <= 0) return

        current_line = -1
        first_idx = 0
        do i = 1, size(tokens)
            if (tokens(i)%line /= current_line) then
                if (first_idx > 0) then
                    call check_line_end_statement(tokens, first_idx, tmp, &
                                                  violation_count)
                end if
                current_line = tokens(i)%line
                first_idx = i
            end if
        end do
        if (first_idx > 0) then
            call check_line_end_statement(tokens, first_idx, tmp, violation_count)
        end if
    end subroutine scan_end_statements

    subroutine check_line_end_statement(tokens, first_idx, tmp, violation_count)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: first_idx
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: line
        integer :: i
        character(len=:), allocatable :: t0
        character(len=:), allocatable :: kind
        logical :: has_name

        line = tokens(first_idx)%line
        i = first_idx

        if (tokens(i)%kind /= TK_KEYWORD) return
        if (.not. allocated(tokens(i)%text)) return
        t0 = to_lower_ascii(trim(tokens(i)%text))
        if (t0 /= "end") return

        i = i + 1
        if (i > size(tokens)) return
        if (tokens(i)%line /= line) return
        if (tokens(i)%kind /= TK_KEYWORD) return
        if (.not. allocated(tokens(i)%text)) return
        kind = to_lower_ascii(trim(tokens(i)%text))

        if (kind /= "program" .and. kind /= "module" .and. kind /= "subroutine" .and. &
            kind /= "function") then
            return
        end if

        if (kind == "module") then
            if (i + 1 <= size(tokens)) then
                if (tokens(i + 1)%line == line) then
                    if (tokens(i + 1)%kind == TK_KEYWORD) then
                        if (allocated(tokens(i + 1)%text)) then
                            if (to_lower_ascii(trim(tokens(i + 1)%text)) == &
                                "procedure") then
                                return
                            end if
                        end if
                    end if
                end if
            end if
        end if

        has_name = .false.
        if (i + 1 <= size(tokens)) then
            if (tokens(i + 1)%line == line) then
                if (allocated(tokens(i + 1)%text)) then
                    has_name = (tokens(i + 1)%kind == TK_IDENTIFIER)
                end if
            end if
        end if

        if (has_name) return

        call push_diagnostic(tmp, violation_count, create_diagnostic( &
                             code="F011", &
                             message="Missing end label for "//trim(kind), &
                             file_path=current_filename, &
                             location=end_phrase_location(tokens(first_idx), &
                                                          tokens(i)), &
                             severity=SEVERITY_INFO))
    end subroutine check_line_end_statement

    pure function end_phrase_location(end_tok, kind_tok) result(location)
        type(token_t), intent(in) :: end_tok
        type(token_t), intent(in) :: kind_tok
        type(source_range_t) :: location
        integer :: end_col

        location%start%line = end_tok%line
        location%start%column = end_tok%column
        location%end%line = end_tok%line

        end_col = kind_tok%column
        if (allocated(kind_tok%text)) end_col = end_col + len(kind_tok%text) - 1
        location%end%column = end_col
    end function end_phrase_location

end module fluff_rule_f011
