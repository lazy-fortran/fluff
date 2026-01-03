module fluff_rule_f004
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, &
                                 create_fix_suggestion, SEVERITY_WARNING, text_edit_t
    use fluff_rule_file_context, only: current_filename, current_source_text
    use fortfront, only: token_t, tokenize_core_with_trivia, trivia_token_t
    use lexer_token_types, only: TK_NEWLINE, TK_WHITESPACE
    implicit none
    private

    public :: check_f004_trailing_whitespace

contains

    subroutine check_f004_trailing_whitespace(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(token_t), allocatable :: tokens(:)
        type(source_range_t), allocatable :: locations(:)
        integer :: i

        if (.not. allocated(current_source_text)) then
            allocate (violations(0))
            return
        end if

        call tokenize_core_with_trivia(current_source_text, tokens)

        call collect_trailing_whitespace(tokens, locations)
        if (size(locations) <= 0) then
            allocate (violations(0))
            return
        end if

        allocate (violations(size(locations)))
        do i = 1, size(locations)
            violations(i) = make_f004_diagnostic(locations(i))
        end do
    end subroutine check_f004_trailing_whitespace

    subroutine collect_trailing_whitespace(tokens, locations)
        type(token_t), allocatable, intent(in) :: tokens(:)
        type(source_range_t), allocatable, intent(out) :: locations(:)
        integer :: i

        allocate (locations(0))
        if (.not. allocated(tokens)) return
        if (size(tokens) <= 0) return

        do i = 1, size(tokens)
            call collect_from_trivia(tokens(i)%leading_trivia, locations)
        end do
        call collect_from_trivia(tokens(size(tokens))%trailing_trivia, locations)
    end subroutine collect_trailing_whitespace

    subroutine collect_from_trivia(trivia, locations)
        type(trivia_token_t), allocatable, intent(in) :: trivia(:)
        type(source_range_t), allocatable, intent(inout) :: locations(:)

        integer :: i
        integer :: start_col, end_col
        type(source_range_t) :: location

        if (.not. allocated(trivia)) return
        if (size(trivia) <= 0) return

        do i = 2, size(trivia)
            if (trivia(i)%kind /= TK_NEWLINE) cycle
            if (trivia(i - 1)%kind /= TK_WHITESPACE) cycle
            if (.not. allocated(trivia(i - 1)%text)) cycle
            if (len(trivia(i - 1)%text) <= 0) cycle

            start_col = trivia(i - 1)%column
            end_col = start_col + len(trivia(i - 1)%text) - 1

            location%start%line = trivia(i - 1)%line
            location%start%column = start_col
            location%end%line = trivia(i - 1)%line
            location%end%column = end_col

            call append_location(locations, location)
        end do
    end subroutine collect_from_trivia

    subroutine append_location(locations, location)
        type(source_range_t), allocatable, intent(inout) :: locations(:)
        type(source_range_t), intent(in) :: location
        type(source_range_t), allocatable :: tmp(:)
        integer :: n

        if (.not. allocated(locations)) then
            allocate (locations(1))
            locations(1) = location
            return
        end if

        n = size(locations)
        allocate (tmp(n + 1))
        if (n > 0) tmp(1:n) = locations
        tmp(n + 1) = location
        call move_alloc(tmp, locations)
    end subroutine append_location

    function make_f004_diagnostic(location) result(diag)
        type(source_range_t), intent(in) :: location
        type(diagnostic_t) :: diag
        type(text_edit_t) :: edits(1)

        diag = create_diagnostic(code="F004", message="Trailing whitespace", &
                                 file_path=current_filename, location=location, &
                                 severity=SEVERITY_WARNING)

        edits(1)%range = location
        edits(1)%new_text = ""

        allocate (diag%fixes(1))
        diag%fixes(1) = create_fix_suggestion("Remove trailing whitespace", edits)
    end function make_f004_diagnostic

end module fluff_rule_f004
