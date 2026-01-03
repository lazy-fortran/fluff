module fluff_rule_f015
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_diagnostic_utils, only: push_diagnostic
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: continue_node
    implicit none
    private

    public :: check_f015_redundant_continue

contains

    subroutine check_f015_redundant_continue(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count
        integer :: i

        allocate (tmp(0))
        violation_count = 0

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (n => ctx%arena%entries(i)%node)
            type is (continue_node)
                call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                     code="F015", &
                                     message="Redundant CONTINUE statement", &
                                     file_path=current_filename, &
                                     location=ctx%get_node_location(i), &
                                     severity=SEVERITY_INFO))
            end select
        end do

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f015_redundant_continue

end module fluff_rule_f015
