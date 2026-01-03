module fluff_rule_p001
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fortfront, only: assignment_node, call_or_subscript_node, do_loop_node, &
                         identifier_node, if_node, select_case_node, &
                         case_block_node, case_default_node, where_node
    implicit none
    private

    public :: check_p001_array_access

contains

    subroutine check_p001_array_access(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer, allocatable :: reported(:)
        integer :: reported_count
        integer :: violation_count

        violation_count = 0
        reported_count = 0

        call analyze_nested_loops(ctx, reported, reported_count, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p001_array_access

    subroutine analyze_nested_loops(ctx, reported, reported_count, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, allocatable, intent(inout) :: reported(:)
        integer, intent(inout) :: reported_count
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: i, j
        integer :: inner_count
        integer :: inner_index
        integer, allocatable :: inner_loops(:)
        character(len=:), allocatable :: v_outer, v_inner

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (outer_loop => ctx%arena%entries(i)%node)
            type is (do_loop_node)
                if (.not. allocated(outer_loop%var_name)) cycle
                if (.not. allocated(outer_loop%body_indices)) cycle
                v_outer = to_lower_ascii(trim(outer_loop%var_name))

                inner_count = 0
                do j = 1, size(outer_loop%body_indices)
                    if (outer_loop%body_indices(j) <= 0) cycle
                    call collect_inner_loops(ctx, outer_loop%body_indices(j), &
                                             inner_loops, inner_count)
                end do

                do j = 1, inner_count
                    inner_index = inner_loops(j)
                    if (inner_index <= 0) cycle
                    if (.not. allocated(ctx%arena%entries(inner_index)%node)) cycle
                    select type (inner_loop => ctx%arena%entries(inner_index)%node)
                    type is (do_loop_node)
                        if (.not. allocated(inner_loop%var_name)) cycle
                        v_inner = to_lower_ascii(trim(inner_loop%var_name))
                        call collect_inefficient_accesses(ctx, inner_index, v_outer, &
                                                          v_inner, reported, &
                                                          reported_count, tmp, &
                                                          violation_count)
                    end select
                end do
            end select
        end do
    end subroutine analyze_nested_loops

    recursive subroutine collect_inner_loops(ctx, node_index, inner_loops, inner_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        integer, allocatable, intent(inout) :: inner_loops(:)
        integer, intent(inout) :: inner_count

        integer, allocatable :: children(:)
        integer :: i
        integer :: j
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (do_loop_node)
            call push_unique_index(inner_loops, inner_count, node_index)
            return
        type is (if_node)
            if (allocated(n%then_body_indices)) then
                do i = 1, size(n%then_body_indices)
                    call collect_inner_loops(ctx, n%then_body_indices(i), inner_loops, &
                                             inner_count)
                end do
            end if
            if (allocated(n%elseif_blocks)) then
                do i = 1, size(n%elseif_blocks)
                    if (allocated(n%elseif_blocks(i)%body_indices)) then
                        associate (elseif_body => n%elseif_blocks(i)%body_indices)
                            do j = 1, size(elseif_body)
                                call collect_inner_loops(ctx, elseif_body(j), &
                                                         inner_loops, inner_count)
                            end do
                        end associate
                    end if
                end do
            end if
            if (allocated(n%else_body_indices)) then
                do i = 1, size(n%else_body_indices)
                    call collect_inner_loops(ctx, n%else_body_indices(i), inner_loops, &
                                             inner_count)
                end do
            end if
            return
        type is (select_case_node)
            if (allocated(n%case_indices)) then
                do i = 1, size(n%case_indices)
                    call collect_inner_loops(ctx, n%case_indices(i), inner_loops, &
                                             inner_count)
                end do
            end if
            if (n%default_index > 0) then
                call collect_inner_loops(ctx, n%default_index, inner_loops, inner_count)
            end if
            return
        type is (case_block_node)
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    call collect_inner_loops(ctx, n%body_indices(i), inner_loops, &
                                             inner_count)
                end do
            end if
            return
        type is (case_default_node)
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    call collect_inner_loops(ctx, n%body_indices(i), inner_loops, &
                                             inner_count)
                end do
            end if
            return
        type is (where_node)
            if (allocated(n%where_body_indices)) then
                do i = 1, size(n%where_body_indices)
                    call collect_inner_loops(ctx, n%where_body_indices(i), &
                                             inner_loops, &
                                             inner_count)
                end do
            end if
            if (allocated(n%elsewhere_clauses)) then
                do i = 1, size(n%elsewhere_clauses)
                    if (allocated(n%elsewhere_clauses(i)%body_indices)) then
                        associate (elsewhere_body => &
                                   n%elsewhere_clauses(i)%body_indices)
                            do j = 1, size(elsewhere_body)
                                call collect_inner_loops(ctx, elsewhere_body(j), &
                                                         inner_loops, inner_count)
                            end do
                        end associate
                    end if
                end do
            end if
            return
        end select

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) > 0) then
                call collect_inner_loops(ctx, children(i), inner_loops, inner_count)
            end if
        end do
    end subroutine collect_inner_loops

    recursive subroutine collect_inefficient_accesses(ctx, node_index, outer_var, &
                                                      inner_var, reported, &
                                                      reported_count, tmp, &
                                                      violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: outer_var
        character(len=*), intent(in) :: inner_var
        integer, allocatable, intent(inout) :: reported(:)
        integer, intent(inout) :: reported_count
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer, allocatable :: children(:)
        integer :: i

        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (do_loop_node)
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    if (n%body_indices(i) > 0) then
                        call collect_inefficient_accesses(ctx, n%body_indices(i), &
                                                          outer_var, inner_var, &
                                                          reported, reported_count, &
                                                          tmp, &
                                                          violation_count)
                    end if
                end do
            end if
            return
        type is (assignment_node)
            call collect_target_accesses(ctx, n%target_index, outer_var, inner_var, &
                                         reported, reported_count, tmp, &
                                         violation_count)
        end select

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) > 0) then
                call collect_inefficient_accesses(ctx, children(i), outer_var, &
                                                  inner_var, reported, &
                                                  reported_count, tmp, &
                                                  violation_count)
            end if
        end do
    end subroutine collect_inefficient_accesses

    recursive subroutine collect_target_accesses(ctx, node_index, outer_var, &
                                                 inner_var, reported, reported_count, &
                                                 tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: outer_var
        character(len=*), intent(in) :: inner_var
        integer, allocatable, intent(inout) :: reported(:)
        integer, intent(inout) :: reported_count
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer, allocatable :: children(:)
        integer :: i

        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (call_or_subscript_node)
            if (.not. reported_contains(reported, reported_count, node_index)) then
                if (is_inefficient_access(ctx, node_index, outer_var, inner_var)) then
                    call push_reported(reported, reported_count, node_index)
                    call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                         code="P001", &
                                         message= &
                                         "Leftmost array index varies in an outer " &
                                         //"loop; consider swapping loop order", &
                                         file_path="", &
                                         location=ctx%get_node_location(node_index), &
                                         severity=SEVERITY_WARNING))
                end if
            end if

            if (n%base_expr_index > 0) then
                call collect_target_accesses(ctx, n%base_expr_index, outer_var, &
                                             inner_var, reported, reported_count, &
                                             tmp, &
                                             violation_count)
            end if
            if (allocated(n%arg_indices)) then
                do i = 1, size(n%arg_indices)
                    if (n%arg_indices(i) > 0) then
                        call collect_target_accesses(ctx, n%arg_indices(i), outer_var, &
                                                     inner_var, reported, &
                                                     reported_count, tmp, &
                                                     violation_count)
                    end if
                end do
            end if
            return
        end select

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) > 0) then
                call collect_target_accesses(ctx, children(i), outer_var, inner_var, &
                                             reported, reported_count, tmp, &
                                             violation_count)
            end if
        end do
    end subroutine collect_target_accesses

    logical function is_inefficient_access(ctx, node_index, outer_var, inner_var) &
        result(bad)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: outer_var
        character(len=*), intent(in) :: inner_var

        integer :: i
        integer :: a1
        character(len=:), allocatable :: n1
        character(len=:), allocatable :: nk

        bad = .false.
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (c => ctx%arena%entries(node_index)%node)
        type is (call_or_subscript_node)
            if (.not. allocated(c%arg_indices)) return
            if (size(c%arg_indices) < 2) return
            a1 = c%arg_indices(1)
        class default
            return
        end select

        call get_identifier_arg_name(ctx, a1, n1)
        if (.not. allocated(n1)) return
        if (n1 /= outer_var) return

        select type (c => ctx%arena%entries(node_index)%node)
        type is (call_or_subscript_node)
            do i = 2, size(c%arg_indices)
                call get_identifier_arg_name(ctx, c%arg_indices(i), nk)
                if (allocated(nk)) then
                    if (nk == inner_var) then
                        bad = .true.
                        return
                    end if
                end if
            end do
        end select
    end function is_inefficient_access

    subroutine get_identifier_arg_name(ctx, node_index, name)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name

        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (identifier_node)
            if (allocated(n%name)) name = to_lower_ascii(trim(n%name))
        end select
    end subroutine get_identifier_arg_name

    subroutine push_reported(reported, reported_count, idx)
        integer, allocatable, intent(inout) :: reported(:)
        integer, intent(inout) :: reported_count
        integer, intent(in) :: idx

        integer, allocatable :: grown(:)
        integer :: new_size

        if (.not. allocated(reported)) then
            new_size = max(16, reported_count + 1)
            allocate (reported(new_size))
        end if

        if (reported_count >= size(reported)) then
            new_size = max(2*size(reported), reported_count + 1)
            allocate (grown(new_size))
            if (reported_count > 0) grown(1:reported_count) = reported(1:reported_count)
            call move_alloc(grown, reported)
        end if

        reported_count = reported_count + 1
        reported(reported_count) = idx
    end subroutine push_reported

    subroutine push_unique_index(indices, index_count, idx)
        integer, allocatable, intent(inout) :: indices(:)
        integer, intent(inout) :: index_count
        integer, intent(in) :: idx

        integer, allocatable :: grown(:)
        integer :: new_size

        if (idx <= 0) return
        if (reported_contains(indices, index_count, idx)) return

        if (.not. allocated(indices)) then
            new_size = max(16, index_count + 1)
            allocate (indices(new_size))
        end if

        if (index_count >= size(indices)) then
            new_size = max(2*size(indices), index_count + 1)
            allocate (grown(new_size))
            if (index_count > 0) grown(1:index_count) = indices(1:index_count)
            call move_alloc(grown, indices)
        end if

        index_count = index_count + 1
        indices(index_count) = idx
    end subroutine push_unique_index

    logical function reported_contains(reported, reported_count, idx) result(found)
        integer, allocatable, intent(in) :: reported(:)
        integer, intent(in) :: reported_count
        integer, intent(in) :: idx

        integer :: i

        found = .false.
        if (reported_count <= 0) return
        do i = 1, reported_count
            if (reported(i) == idx) then
                found = .true.
                return
            end if
        end do
    end function reported_contains

end module fluff_rule_p001
