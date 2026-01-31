module fluff_rule_f013
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_diagnostic_utils, only: push_diagnostic
    use fluff_rule_file_context, only: current_filename
    use fluff_token_helpers, only: token_location_point
    use fortfront, only: token_t, tokenize_core_with_trivia
    use lexer_token_types, only: TK_OPERATOR
    implicit none
    private

    public :: check_f013_multiple_statements

contains

    subroutine check_f013_multiple_statements(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        character(len=:), allocatable :: source_text
        logical :: found
        type(token_t), allocatable :: tokens(:)
        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count
        integer :: i

        call ctx%get_source_text(source_text, found)
        if (.not. found) then
            allocate (violations(0))
            return
        end if

        call tokenize_core_with_trivia(source_text, tokens)

        allocate (tmp(0))
        violation_count = 0
        if (allocated(tokens)) then
            do i = 1, size(tokens)
                if (tokens(i)%kind /= TK_OPERATOR) cycle
                if (.not. allocated(tokens(i)%text)) cycle
                if (tokens(i)%text /= ";") cycle
                call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                     code="F013", &
                                     message="Multiple statements per line", &
                                     file_path=current_filename, &
                                     location=token_location_point(tokens(i)), &
                                     severity=SEVERITY_WARNING))
            end do
        end if

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f013_multiple_statements

end module fluff_rule_f013
