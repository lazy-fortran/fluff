module fluff_rule_p006
    use fluff_ast, only: fluff_ast_context_t, NODE_ALLOCATE_STATEMENT, &
                         NODE_DEALLOCATE_STATEMENT
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_diagnostic_utils, only: push_diagnostic
    use fortfront, only: allocate_statement_node, assignment_node, binary_op_node, &
                         call_or_subscript_node, deallocate_statement_node, do_loop_node
    implicit none
    private

    public :: check_p006_loop_allocations

contains

    subroutine check_p006_loop_allocations(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        allocate (tmp(16))
        violation_count = 0

        call analyze_p006(ctx, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p006_loop_allocations

    subroutine analyze_p006(ctx, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: i, j
        integer :: alloc_node, dealloc_node
        logical :: found_alloc, found_dealloc

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (loop => ctx%arena%entries(i)%node)
            type is (do_loop_node)
                if (.not. allocated(loop%body_indices)) cycle
                found_alloc = .false.
                found_dealloc = .false.
                alloc_node = 0
                dealloc_node = 0
                do j = 1, size(loop%body_indices)
                    if (.not. found_alloc) then
                        alloc_node = find_allocate_node(ctx, loop%body_indices(j))
                        if (alloc_node > 0) found_alloc = .true.
                    end if
                    if (.not. found_dealloc) then
                        dealloc_node = find_deallocate_node(ctx, loop%body_indices(j))
                        if (dealloc_node > 0) found_dealloc = .true.
                    end if
                end do
                if (found_alloc) then
                    call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                         code="P006", &
                                         message="Avoid allocate inside loops", &
                                         file_path="", &
                                         location=ctx%get_node_location(alloc_node), &
                                         severity=SEVERITY_WARNING))
                end if
                if (found_dealloc) then
                    call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                         code="P006", &
                                         message="Avoid deallocate inside loops", &
                                         file_path="", &
                                         location=ctx%get_node_location(dealloc_node), &
                                         severity=SEVERITY_WARNING))
                end if
            end select
        end do
    end subroutine analyze_p006

    recursive integer function find_allocate_node(ctx, node_index) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        integer :: i, child_result

        found = 0
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        if (ctx%get_node_type(node_index) == NODE_ALLOCATE_STATEMENT) then
            found = node_index
            return
        end if

        select type (n => ctx%arena%entries(node_index)%node)
        type is (allocate_statement_node)
            found = node_index
            return
        type is (do_loop_node)
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    child_result = find_allocate_node(ctx, n%body_indices(i))
                    if (child_result > 0) then
                        found = child_result
                        return
                    end if
                end do
            end if
        type is (assignment_node)
            child_result = find_allocate_node(ctx, n%target_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
            child_result = find_allocate_node(ctx, n%value_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
        type is (binary_op_node)
            child_result = find_allocate_node(ctx, n%left_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
            child_result = find_allocate_node(ctx, n%right_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
        type is (call_or_subscript_node)
            if (allocated(n%arg_indices)) then
                do i = 1, size(n%arg_indices)
                    child_result = find_allocate_node(ctx, n%arg_indices(i))
                    if (child_result > 0) then
                        found = child_result
                        return
                    end if
                end do
            end if
        end select
    end function find_allocate_node

    recursive integer function find_deallocate_node(ctx, node_index) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        integer :: i, child_result

        found = 0
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        if (ctx%get_node_type(node_index) == NODE_DEALLOCATE_STATEMENT) then
            found = node_index
            return
        end if

        select type (n => ctx%arena%entries(node_index)%node)
        type is (deallocate_statement_node)
            found = node_index
            return
        type is (do_loop_node)
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    child_result = find_deallocate_node(ctx, n%body_indices(i))
                    if (child_result > 0) then
                        found = child_result
                        return
                    end if
                end do
            end if
        type is (assignment_node)
            child_result = find_deallocate_node(ctx, n%target_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
            child_result = find_deallocate_node(ctx, n%value_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
        type is (binary_op_node)
            child_result = find_deallocate_node(ctx, n%left_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
            child_result = find_deallocate_node(ctx, n%right_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
        type is (call_or_subscript_node)
            if (allocated(n%arg_indices)) then
                do i = 1, size(n%arg_indices)
                    child_result = find_deallocate_node(ctx, n%arg_indices(i))
                    if (child_result > 0) then
                        found = child_result
                        return
                    end if
                end do
            end if
        end select
    end function find_deallocate_node

end module fluff_rule_p006
