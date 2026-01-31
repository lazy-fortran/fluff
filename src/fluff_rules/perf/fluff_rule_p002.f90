module fluff_rule_p002
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: assignment_node, binary_op_node, call_or_subscript_node, &
                         do_loop_node, identifier_node
    implicit none
    private

    public :: check_p002_loop_ordering

contains

    subroutine check_p002_loop_ordering(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        allocate (tmp(16))
        violation_count = 0

        call analyze_p002(ctx, node_index, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p002_loop_ordering

    subroutine analyze_p002(ctx, node_index, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: i, j
        integer :: inner_index
        character(len=:), allocatable :: v_outer, v_inner

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (outer_loop => ctx%arena%entries(i)%node)
            type is (do_loop_node)
                if (.not. allocated(outer_loop%var_name)) cycle
                if (.not. allocated(outer_loop%body_indices)) cycle
                v_outer = to_lower_ascii(trim(outer_loop%var_name))

                do j = 1, size(outer_loop%body_indices)
                    inner_index = outer_loop%body_indices(j)
                    if (.not. allocated(ctx%arena%entries(inner_index)%node)) then
                        cycle
                    end if
                    select type (inner_loop => &
                                 ctx%arena%entries(inner_index)%node)
                    type is (do_loop_node)
                        if (.not. allocated(inner_loop%var_name)) cycle
                        v_inner = to_lower_ascii(trim(inner_loop%var_name))
                        if (loop_has_inefficient_access(ctx, &
                                                        inner_index, &
                                                        v_outer, v_inner)) then
                            call push_diagnostic(tmp, violation_count, &
                                                create_diagnostic( &
                                                code="P002", &
                                             message="Consider swapping nested loops", &
                                                file_path=current_filename, &
                                                location=ctx%get_node_location( &
                                                inner_index), &
                                                severity=SEVERITY_INFO &
                                                ))
                            exit
                        end if
                    end select
                end do
            end select
        end do
    end subroutine analyze_p002

    function loop_has_inefficient_access(ctx, loop_index, outer_var, inner_var) &
        result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: loop_index
        character(len=*), intent(in) :: outer_var
        character(len=*), intent(in) :: inner_var
        logical :: found

        integer :: i

        found = .false.
        if (loop_index <= 0) return
        if (.not. allocated(ctx%arena%entries(loop_index)%node)) return

        select type (loop => ctx%arena%entries(loop_index)%node)
        type is (do_loop_node)
            if (.not. allocated(loop%body_indices)) return
            do i = 1, size(loop%body_indices)
                if (node_has_inefficient_access(ctx, loop%body_indices(i), &
                                                outer_var, inner_var)) then
                    found = .true.
                    exit
                end if
            end do
        end select
    end function loop_has_inefficient_access

    recursive function node_has_inefficient_access(ctx, node_index, outer_var, &
                                                   inner_var) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: outer_var
        character(len=*), intent(in) :: inner_var
        logical :: found

        integer :: i

        found = .false.
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (call_or_subscript_node)
            if (is_inefficient_2d_access(ctx, node_index, outer_var, inner_var)) then
                found = .true.
                return
            end if
            if (allocated(n%arg_indices)) then
                do i = 1, size(n%arg_indices)
                    if (node_has_inefficient_access(ctx, n%arg_indices(i), &
                                                    outer_var, inner_var)) then
                        found = .true.
                        return
                    end if
                end do
            end if
        type is (assignment_node)
            if (node_has_inefficient_access(ctx, n%target_index, outer_var, &
                                            inner_var)) then
                found = .true.
                return
            end if
            if (node_has_inefficient_access(ctx, n%value_index, outer_var, &
                                            inner_var)) then
                found = .true.
                return
            end if
        type is (binary_op_node)
            if (node_has_inefficient_access(ctx, n%left_index, outer_var, &
                                            inner_var)) then
                found = .true.
                return
            end if
            if (node_has_inefficient_access(ctx, n%right_index, outer_var, &
                                            inner_var)) then
                found = .true.
                return
            end if
        type is (do_loop_node)
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    if (node_has_inefficient_access(ctx, n%body_indices(i), &
                                                    outer_var, inner_var)) then
                        found = .true.
                        return
                    end if
                end do
            end if
        end select
    end function node_has_inefficient_access

    function is_inefficient_2d_access(ctx, node_index, outer_var, inner_var) result(bad)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: outer_var
        character(len=*), intent(in) :: inner_var
        logical :: bad

        integer :: a1, a2
        character(len=:), allocatable :: n1, n2

        bad = .false.
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (c => ctx%arena%entries(node_index)%node)
        type is (call_or_subscript_node)
            if (.not. allocated(c%arg_indices)) return
            if (size(c%arg_indices) < 2) return
            a1 = c%arg_indices(1)
            a2 = c%arg_indices(2)
        class default
            return
        end select

        call get_identifier_arg_name(ctx, a1, n1)
        call get_identifier_arg_name(ctx, a2, n2)
        if (.not. allocated(n1) .or. .not. allocated(n2)) return

        if (n1 == outer_var .and. n2 == inner_var) then
            bad = .true.
        end if
    end function is_inefficient_2d_access

    subroutine get_identifier_arg_name(ctx, node_index, name)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name

        name = ""
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (identifier_node)
            if (allocated(n%name)) name = to_lower_ascii(trim(n%name))
        end select
    end subroutine get_identifier_arg_name

end module fluff_rule_p002
