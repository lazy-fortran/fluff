module fluff_rule_p001
    use fluff_ast, only: fluff_ast_context_t, NODE_DO_LOOP
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    implicit none
    private

    public :: check_p001_array_access

contains

    subroutine check_p001_array_access(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        call check_p001_array_access_ast_based(ctx, node_index, violations)
    end subroutine check_p001_array_access

    subroutine check_p001_array_access_ast_based(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count

        allocate (temp_violations(50))
        violation_count = 0

        call analyze_array_access_patterns(ctx, node_index, temp_violations, &
                                           violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = temp_violations(1:violation_count)
        end if
    end subroutine check_p001_array_access_ast_based

    recursive subroutine analyze_array_access_patterns(ctx, node_index, violations, &
                                                       violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        integer, allocatable :: children(:)
        integer :: node_type, i

        node_type = ctx%get_node_type(node_index)
        if (node_type == NODE_DO_LOOP) then
            call check_loop_array_access(ctx, node_index, violations, violation_count)
        end if

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) > 0) then
                call analyze_array_access_patterns(ctx, children(i), violations, &
                                                   violation_count)
            end if
        end do
        if (allocated(children)) deallocate (children)
    end subroutine analyze_array_access_patterns

    subroutine check_loop_array_access(ctx, loop_node, violations, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: loop_node
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        if (has_array_like_accesses(ctx, loop_node)) then
            if (violation_count < size(violations)) then
                violation_count = violation_count + 1
                violations(violation_count) = create_diagnostic( &
                                              code="P001", &
                            message="Consider memory-efficient array access patterns", &
                                              file_path="", &
                                            location=ctx%get_node_location(loop_node), &
                                              severity=SEVERITY_INFO)
            end if
        end if
    end subroutine check_loop_array_access

    logical function has_array_like_accesses(ctx, node_index) result(has_arrays)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index

        integer, allocatable :: children(:)

        has_arrays = .false.
        children = ctx%get_children(node_index)
        if (size(children) > 2) then
            has_arrays = .true.
        end if
        if (allocated(children)) deallocate (children)
    end function has_array_like_accesses

end module fluff_rule_p001
