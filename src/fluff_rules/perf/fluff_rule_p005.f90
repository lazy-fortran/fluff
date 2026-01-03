module fluff_rule_p005
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_diagnostic_utils, only: push_diagnostic
    use fortfront, only: assignment_node, binary_op_node, call_or_subscript_node, &
                         do_loop_node
    implicit none
    private

    public :: check_p005_string_operations

contains

    subroutine check_p005_string_operations(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        allocate (tmp(16))
        violation_count = 0

        call analyze_p005(ctx, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p005_string_operations

    subroutine analyze_p005(ctx, tmp, violation_count)
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
                    if (node_has_concat(ctx, loop%body_indices(j))) then
                        call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                            code="P005", &
                             message="String concatenation in loops can be expensive", &
                                            file_path="", &
                                            location=ctx%get_node_location(i), &
                                            severity=SEVERITY_INFO))
                        exit
                    end if
                end do
            end select
        end do
    end subroutine analyze_p005

    recursive logical function node_has_concat(ctx, node_index) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        integer :: i

        found = .false.
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (binary_op_node)
            if (allocated(n%operator)) then
                if (trim(n%operator) == "//") then
                    found = .true.
                    return
                end if
            end if
            if (node_has_concat(ctx, n%left_index)) then
                found = .true.
                return
            end if
            if (node_has_concat(ctx, n%right_index)) then
                found = .true.
                return
            end if
        type is (assignment_node)
            if (node_has_concat(ctx, n%target_index)) then
                found = .true.
                return
            end if
            if (node_has_concat(ctx, n%value_index)) then
                found = .true.
                return
            end if
        type is (call_or_subscript_node)
            if (allocated(n%arg_indices)) then
                do i = 1, size(n%arg_indices)
                    if (node_has_concat(ctx, n%arg_indices(i))) then
                        found = .true.
                        return
                    end if
                end do
            end if
        type is (do_loop_node)
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    if (node_has_concat(ctx, n%body_indices(i))) then
                        found = .true.
                        return
                    end if
                end do
            end if
        end select
    end function node_has_concat

end module fluff_rule_p005
