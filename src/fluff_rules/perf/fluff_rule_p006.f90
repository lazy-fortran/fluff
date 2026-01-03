module fluff_rule_p006
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_diagnostic_utils, only: push_diagnostic
    use fortfront, only: allocate_statement_node, assignment_node, binary_op_node, &
                         call_or_subscript_node, do_loop_node
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

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (loop => ctx%arena%entries(i)%node)
            type is (do_loop_node)
                if (.not. allocated(loop%body_indices)) cycle
                do j = 1, size(loop%body_indices)
                    if (node_has_allocate(ctx, loop%body_indices(j))) then
                        call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                            code="P006", &
                                            message="Avoid allocate inside loops", &
                                            file_path="", &
                                            location=ctx%get_node_location(i), &
                                            severity=SEVERITY_WARNING))
                        exit
                    end if
                end do
            end select
        end do
    end subroutine analyze_p006

    recursive logical function node_has_allocate(ctx, node_index) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        integer :: i

        found = .false.
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (allocate_statement_node)
            found = .true.
            return
        type is (do_loop_node)
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    if (node_has_allocate(ctx, n%body_indices(i))) then
                        found = .true.
                        return
                    end if
                end do
            end if
        type is (assignment_node)
            if (node_has_allocate(ctx, n%target_index)) then
                found = .true.
                return
            end if
            if (node_has_allocate(ctx, n%value_index)) then
                found = .true.
                return
            end if
        type is (binary_op_node)
            if (node_has_allocate(ctx, n%left_index)) then
                found = .true.
                return
            end if
            if (node_has_allocate(ctx, n%right_index)) then
                found = .true.
                return
            end if
        type is (call_or_subscript_node)
            if (allocated(n%arg_indices)) then
                do i = 1, size(n%arg_indices)
                    if (node_has_allocate(ctx, n%arg_indices(i))) then
                        found = .true.
                        return
                    end if
                end do
            end if
        end select
    end function node_has_allocate

end module fluff_rule_p006
