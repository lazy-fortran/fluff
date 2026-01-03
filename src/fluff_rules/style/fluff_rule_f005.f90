module fluff_rule_f005
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename, current_source_text
    use fortfront, only: token_t, tokenize_core_with_trivia, trivia_token_t
    use lexer_token_types, only: TK_WHITESPACE
    implicit none
    private

    public :: check_f005_mixed_tabs_spaces

contains

    subroutine check_f005_mixed_tabs_spaces(ctx, node_index, violations)
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

        call collect_mixed_indentation(tokens, locations)
        if (size(locations) <= 0) then
            allocate (violations(0))
            return
        end if

        allocate (violations(size(locations)))
        do i = 1, size(locations)
            violations(i) = create_diagnostic( &
                            code="F005", &
                            message="Mixed tabs and spaces", &
                            file_path=current_filename, &
                            location=locations(i), &
                            severity=SEVERITY_WARNING)
        end do
    end subroutine check_f005_mixed_tabs_spaces

    subroutine collect_mixed_indentation(tokens, locations)
        type(token_t), allocatable, intent(in) :: tokens(:)
        type(source_range_t), allocatable, intent(out) :: locations(:)
        integer :: i

        allocate (locations(0))
        if (.not. allocated(tokens)) return
        if (size(tokens) <= 0) return

        do i = 1, size(tokens)
            call collect_from_trivia(tokens(i)%leading_trivia, locations)
        end do
    end subroutine collect_mixed_indentation

    subroutine collect_from_trivia(trivia, locations)
        type(trivia_token_t), allocatable, intent(in) :: trivia(:)
        type(source_range_t), allocatable, intent(inout) :: locations(:)

        integer :: i
        integer :: end_col
        type(source_range_t) :: location

        if (.not. allocated(trivia)) return
        if (size(trivia) <= 0) return

        do i = 1, size(trivia)
            if (trivia(i)%kind /= TK_WHITESPACE) cycle
            if (trivia(i)%column /= 1) cycle
            if (.not. allocated(trivia(i)%text)) cycle
            if (.not. has_space_and_tab(trivia(i)%text)) cycle

            end_col = trivia(i)%column + len(trivia(i)%text) - 1
            location%start%line = trivia(i)%line
            location%start%column = trivia(i)%column
            location%end%line = trivia(i)%line
            location%end%column = end_col
            call append_location(locations, location)
        end do
    end subroutine collect_from_trivia

    logical function has_space_and_tab(text) result(has_both)
        character(len=*), intent(in) :: text

        has_both = (index(text, " ") > 0 .and. index(text, achar(9)) > 0)
    end function has_space_and_tab

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

end module fluff_rule_f005
