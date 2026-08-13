module fluff_rule_c003
    ! C003: Loop with no proven bound.
    !
    ! Reports a `do while` loop where no monotone quantity decreasing toward a
    ! compared-against bound can be found, and a regular `do` loop whose step is
    ! not a non-zero constant (a constant non-zero step always yields a finite
    ! trip count). A `do while` is proven bounded when its condition compares
    ! an induction variable against a bound and that variable moves strictly
    ! toward the bound in the body (an iteration counter bounded by a constant,
    ! or an induction variable moving monotonically toward a compared-against
    ! bound). Everything else reports. Conservative by design: over-reporting is
    ! sound, and the rule ships off by default.
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: binary_op_node, do_loop_node, do_while_node, &
        literal_node, identifier_node, assignment_node
    implicit none
    private

    public :: check_c003_unbounded_loop

contains

    subroutine check_c003_unbounded_loop(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count
        integer :: i
        logical :: bounded

        allocate (tmp(0))
        violation_count = 0

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (n => ctx%arena%entries(i)%node)
                type is (do_while_node)
                bounded = do_while_bounded(ctx, n)
                type is (do_loop_node)
                bounded = do_loop_bounded(ctx, n)
            class default
                cycle
            end select

            if (.not. bounded) then
                call push_diagnostic(tmp, violation_count, create_diagnostic( &
                    code="C003", &
                    message="Loop with no proven bound", &
                    file_path=current_filename, &
                    location=ctx%get_node_location(i), &
                    severity=SEVERITY_WARNING))
            end if
        end do

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_c003_unbounded_loop

    ! A regular DO loop with a constant non-zero step always has a finite trip
    ! count regardless of start/end values. A zero step (or a variable step,
    ! which may be zero) is not provably bounded.
    logical function do_loop_bounded(ctx, n) result(bounded)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(do_loop_node), intent(in) :: n

        bounded = .false.
        ! step_expr_index == 0 means the default step of 1
        if (n%step_expr_index == 0) then
            bounded = .true.
            return
        end if
        if (n%step_expr_index <= 0) return
        if (.not. allocated(ctx%arena%entries(n%step_expr_index)%node)) return

        select type (s => ctx%arena%entries(n%step_expr_index)%node)
            type is (literal_node)
            bounded = literal_is_nonzero_step(s)
        end select
    end function do_loop_bounded

    logical function literal_is_nonzero_step(node) result(nonzero)
        type(literal_node), intent(in) :: node

        character(len=:), allocatable :: v
        real :: rval
        integer :: ios

        nonzero = .false.
        if (.not. allocated(node%value)) return
        v = trim(node%value)
        if (len_trim(v) == 0) return
        if (v(1:1) == '"' .or. v(1:1) == "'") return
        read (v, *, iostat=ios) rval
        if (ios /= 0) return
        nonzero = (rval /= 0.0)
    end function literal_is_nonzero_step

    ! A do while is proven bounded when its condition compares an induction
    ! variable against a bound and the variable moves strictly toward that
    ! bound in the body.
    logical function do_while_bounded(ctx, n) result(bounded)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(do_while_node), intent(in) :: n

        character(len=:), allocatable :: ind_var
        integer :: bound_idx
        logical :: increasing

        bounded = .false.
        if (n%condition_index <= 0) return
        if (.not. allocated(ctx%arena%entries(n%condition_index)%node)) return

        select type (c => ctx%arena%entries(n%condition_index)%node)
            type is (binary_op_node)
            if (extract_induction_comparison(ctx, c, ind_var, bound_idx, increasing)) then
                if (.not. bound_reassigned_in_body(ctx, n, bound_idx)) then
                    if (body_moves_var_toward_bound(ctx, n%body_indices, ind_var, &
                        increasing)) then
                        bounded = .true.
                    end if
                end if
            end if
        end select
    end function do_while_bounded

    ! From a comparison `v < b`, `v <= b`, `b > v` (increasing) or
    ! `v > b`, `v >= b`, `b < v` (decreasing), extract the induction variable
    ! and the bound expression index.
    logical function extract_induction_comparison(ctx, c, ind_var, bound_idx, &
            increasing) result(ok)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(binary_op_node), intent(in) :: c
        character(len=:), allocatable, intent(out) :: ind_var
        integer, intent(out) :: bound_idx
        logical, intent(out) :: increasing

        character(len=:), allocatable :: op

        ok = .false.
        if (.not. allocated(c%operator)) return
        op = trim(c%operator)

        ! v < b  or  v <= b : v increases toward b
        if (op == "<" .or. op == "<=") then
            if (expr_is_identifier(ctx, c%left_index, ind_var)) then
                bound_idx = c%right_index
                increasing = .true.
                ok = .true.
            end if
            return
        end if
        ! v > b  or  v >= b : v decreases toward b
        if (op == ">" .or. op == ">=") then
            if (expr_is_identifier(ctx, c%left_index, ind_var)) then
                bound_idx = c%right_index
                increasing = .false.
                ok = .true.
            end if
            return
        end if
        ! b < v  or  b <= v : v increases toward b
        if (op == "<" .or. op == "<=") then
            if (expr_is_identifier(ctx, c%right_index, ind_var)) then
                bound_idx = c%left_index
                increasing = .true.
                ok = .true.
            end if
            return
        end if
        ! b > v  or  b >= v : v decreases toward b
        if (op == ">" .or. op == ">=") then
            if (expr_is_identifier(ctx, c%right_index, ind_var)) then
                bound_idx = c%left_index
                increasing = .false.
                ok = .true.
            end if
        end if
    end function extract_induction_comparison

    logical function expr_is_identifier(ctx, idx, name) result(match)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: idx
        character(len=:), allocatable, intent(out) :: name

        match = .false.
        if (idx <= 0) return
        if (.not. allocated(ctx%arena%entries(idx)%node)) return
        select type (n => ctx%arena%entries(idx)%node)
            type is (identifier_node)
            if (allocated(n%name)) then
                name = to_lower_ascii(trim(n%name))
                match = .true.
            end if
        end select
    end function expr_is_identifier

    ! True if the bound expression is an identifier that is reassigned anywhere
    ! in the loop body (a bound that changes in the body can chase the counter).
    logical function bound_reassigned_in_body(ctx, n, bound_idx) result(reassigned)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(do_while_node), intent(in) :: n
        integer, intent(in) :: bound_idx

        character(len=:), allocatable :: bound_name

        reassigned = .false.
        if (bound_idx <= 0) return
        if (expr_is_identifier(ctx, bound_idx, bound_name)) then
            reassigned = subtree_assigns_var(ctx, n%body_indices, bound_name)
        end if
    end function bound_reassigned_in_body

    ! True if the loop body contains an assignment `v = v +/- c` with a
    ! positive constant step, moving `v` strictly in the given direction.
    logical function body_moves_var_toward_bound(ctx, body_indices, var_name, &
            increasing) result(moves)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: var_name
        logical, intent(in) :: increasing

        integer :: i

        moves = .false.
        if (size(body_indices) <= 0) return
        do i = 1, size(body_indices)
            if (subtree_moves_var(ctx, body_indices(i), var_name, increasing)) then
                moves = .true.
                return
            end if
        end do
    end function body_moves_var_toward_bound

    recursive logical function subtree_moves_var(ctx, node_idx, var_name, &
            increasing) result(moves)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_idx
        character(len=*), intent(in) :: var_name
        logical, intent(in) :: increasing

        integer, allocatable :: children(:)
        integer :: i

        moves = .false.
        if (node_idx <= 0) return
        if (.not. allocated(ctx%arena%entries(node_idx)%node)) return

        select type (n => ctx%arena%entries(node_idx)%node)
            type is (assignment_node)
            if (assignment_is_monotonic_step(ctx, n, var_name, increasing)) then
                moves = .true.
                return
            end if
        end select

        children = ctx%get_children(node_idx)
        do i = 1, size(children)
            if (children(i) <= 0) cycle
            if (subtree_moves_var(ctx, children(i), var_name, increasing)) then
                moves = .true.
                return
            end if
        end do
    end function subtree_moves_var

    ! True if `a` is `var = var + c` (increasing) or `var = var - c`
    ! (decreasing) with a positive constant step.
    logical function assignment_is_monotonic_step(ctx, a, var_name, increasing) &
            result(is_step)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(assignment_node), intent(in) :: a
        character(len=*), intent(in) :: var_name
        logical, intent(in) :: increasing

        character(len=:), allocatable :: target_name
        integer :: lidx, ridx
        real :: step_val

        is_step = .false.
        if (.not. expr_is_identifier(ctx, a%target_index, target_name)) return
        if (to_lower_ascii(trim(target_name)) /= to_lower_ascii(trim(var_name))) return
        if (a%value_index <= 0) return
        if (.not. allocated(ctx%arena%entries(a%value_index)%node)) return

        select type (v => ctx%arena%entries(a%value_index)%node)
            type is (binary_op_node)
            if (.not. allocated(v%operator)) return
            lidx = v%left_index
            ridx = v%right_index
            if (trim(v%operator) == "+") then
                ! var = var + c  or  c + var
                if (expr_is_identifier(ctx, lidx, target_name)) then
                    if (literal_positive(ctx, ridx, step_val)) then
                        is_step = increasing .and. step_val > 0.0
                    end if
                else if (expr_is_identifier(ctx, ridx, target_name)) then
                    if (literal_positive(ctx, lidx, step_val)) then
                        is_step = increasing .and. step_val > 0.0
                    end if
                end if
            else if (trim(v%operator) == "-") then
                ! var = var - c
                if (expr_is_identifier(ctx, lidx, target_name)) then
                    if (literal_positive(ctx, ridx, step_val)) then
                        is_step = (.not. increasing) .and. step_val > 0.0
                    end if
                end if
            end if
        end select
    end function assignment_is_monotonic_step

    logical function literal_positive(ctx, idx, rval) result(positive)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: idx
        real, intent(out) :: rval

        character(len=:), allocatable :: v
        integer :: ios

        positive = .false.
        rval = 0.0
        if (idx <= 0) return
        if (.not. allocated(ctx%arena%entries(idx)%node)) return
        select type (n => ctx%arena%entries(idx)%node)
            type is (literal_node)
            if (.not. allocated(n%value)) return
            if (len_trim(n%value) == 0) return
            if (n%value(1:1) == '"' .or. n%value(1:1) == "'") return
            v = trim(n%value)
            read (v, *, iostat=ios) rval
            if (ios /= 0) return
            positive = (rval > 0.0)
        end select
    end function literal_positive

    recursive logical function subtree_assigns_var(ctx, body_indices, var_name) &
            result(assigns)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: var_name

        integer :: i

        assigns = .false.
        if (size(body_indices) <= 0) return
        do i = 1, size(body_indices)
            if (subtree_assigns_var_one(ctx, body_indices(i), var_name)) then
                assigns = .true.
                return
            end if
        end do
    end function subtree_assigns_var

    recursive logical function subtree_assigns_var_one(ctx, node_idx, var_name) &
            result(assigns)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_idx
        character(len=*), intent(in) :: var_name

        integer, allocatable :: children(:)
        integer :: i
        character(len=:), allocatable :: target_name

        assigns = .false.
        if (node_idx <= 0) return
        if (.not. allocated(ctx%arena%entries(node_idx)%node)) return

        select type (n => ctx%arena%entries(node_idx)%node)
            type is (assignment_node)
            if (expr_is_identifier(ctx, n%target_index, target_name)) then
                if (to_lower_ascii(trim(target_name)) == to_lower_ascii(trim(var_name))) then
                    assigns = .true.
                    return
                end if
            end if
        end select

        children = ctx%get_children(node_idx)
        do i = 1, size(children)
            if (children(i) <= 0) cycle
            if (subtree_assigns_var_one(ctx, children(i), var_name)) then
                assigns = .true.
                return
            end if
        end do
    end function subtree_assigns_var_one

end module fluff_rule_c003
