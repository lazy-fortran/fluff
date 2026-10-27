module fluff_rule_c002
    ! C002: Division by a value not proven non-zero.
    !
    ! Reports a division (binary op "/") whose denominator cannot be shown
    ! non-zero on all paths reaching it, using conservative interval/sign
    ! reasoning local to the enclosing procedure. Non-zero is established by a
    ! literal, by a parameter (named constant), by a dominating guard, or by
    ! all reaching definitions assigning a provably non-zero value. Anything
    ! else reports. This rule is deliberately conservative: soundness in this
    ! class means over-reporting, and the rule ships off by default.
    use fluff_ast, only: fluff_ast_context_t, &
        NODE_FUNCTION_DEF, NODE_SUBROUTINE_DEF, NODE_PROGRAM
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: binary_op_node, if_node, literal_node, identifier_node, &
        call_or_subscript_node, assignment_node, declaration_node
    implicit none
    private

    public :: check_c002_unguarded_division

contains

    subroutine check_c002_unguarded_division(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        integer, allocatable :: parent(:)
        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count
        integer :: i
        logical :: proven

        allocate (tmp(0))
        violation_count = 0

        call build_parent_map(ctx, ctx%root_index, parent)

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (b => ctx%arena%entries(i)%node)
                type is (binary_op_node)
                if (.not. allocated(b%operator)) cycle
                if (trim(b%operator) /= "/") cycle
                if (b%right_index <= 0) cycle
                proven = denom_proven_nonzero(ctx, b%right_index, i, parent)
                if (.not. proven) then
                    call push_diagnostic(tmp, violation_count, create_diagnostic( &
                        code="C002", &
                        message="Division by a value not proven non-zero", &
                        file_path=current_filename, &
                        location=ctx%get_node_location(i), &
                        severity=SEVERITY_WARNING))
                end if
            end select
        end do

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_c002_unguarded_division

    ! Build a map from each node index to its parent index via DFS from root.
    recursive subroutine build_parent_map(ctx, cur, parent)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: cur
        integer, allocatable, intent(inout) :: parent(:)

        integer, allocatable :: children(:)
        integer :: i

        if (cur <= 0) return
        if (.not. allocated(ctx%arena%entries(cur)%node)) return

        children = ctx%get_children(cur)
        do i = 1, size(children)
            if (children(i) <= 0) cycle
            if (.not. allocated(parent)) then
                allocate (parent(0:ctx%arena%size))
                parent = 0
            end if
            parent(children(i)) = cur
            call build_parent_map(ctx, children(i), parent)
        end do
    end subroutine build_parent_map

    recursive logical function denom_proven_nonzero(ctx, denom_idx, div_idx, parent) &
            result(proven)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: denom_idx
        integer, intent(in) :: div_idx
        integer, allocatable, intent(in) :: parent(:)

        integer :: proc_root

        proven = .false.
        if (denom_idx <= 0) return
        if (.not. allocated(ctx%arena%entries(denom_idx)%node)) return

        proc_root = find_enclosing_procedure(ctx, div_idx, parent)

        select type (n => ctx%arena%entries(denom_idx)%node)
            type is (literal_node)
            proven = literal_is_nonzero(n)
            type is (identifier_node)
            if (.not. allocated(n%name)) return
            if (is_parameter_nonzero(ctx, n%name, proc_root)) then
                proven = .true.
                return
            end if
            if (guarded_nonzero(ctx, n%name, div_idx, parent)) then
                proven = .true.
                return
            end if
            if (all_defs_nonzero(ctx, n%name, proc_root, div_idx, parent)) then
                proven = .true.
                return
            end if
            type is (binary_op_node)
            ! product/quotient is non-zero if every factor is non-zero
            if (allocated(n%operator)) then
                if (trim(n%operator) == "*") then
                    proven = denom_proven_nonzero(ctx, n%left_index, div_idx, parent) .and. &
                        denom_proven_nonzero(ctx, n%right_index, div_idx, parent)
                else if (trim(n%operator) == "**") then
                    proven = denom_proven_nonzero(ctx, n%left_index, div_idx, parent)
                else if (trim(n%operator) == "/") then
                    proven = denom_proven_nonzero(ctx, n%left_index, div_idx, parent) .and. &
                        denom_proven_nonzero(ctx, n%right_index, div_idx, parent)
                end if
            end if
            type is (call_or_subscript_node)
            if (.not. n%is_array_access) then
                if (allocated(n%name)) then
                    if (to_lower_ascii(trim(n%name)) == "abs") then
                        if (allocated(n%arg_indices)) then
                            if (size(n%arg_indices) >= 1) then
                                proven = denom_proven_nonzero(ctx, n%arg_indices(1), &
                                    div_idx, parent)
                            end if
                        end if
                    end if
                end if
            end if
        end select
    end function denom_proven_nonzero

    ! Walk up ancestors until a program/function/subroutine boundary.
    recursive integer function find_enclosing_procedure(ctx, node_idx, parent) result(root)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_idx
        integer, allocatable, intent(in) :: parent(:)

        integer :: p, t

        root = node_idx
        if (node_idx <= 0) return
        if (.not. allocated(parent)) return
        if (node_idx > ubound(parent, 1)) return

        p = node_idx
        do
            t = ctx%get_node_type(p)
            if (t == NODE_PROGRAM .or. t == NODE_FUNCTION_DEF .or. &
                t == NODE_SUBROUTINE_DEF) then
                root = p
                return
            end if
            if (p <= 0 .or. p > ubound(parent, 1)) return
            if (parent(p) <= 0) return
            p = parent(p)
        end do
    end function find_enclosing_procedure

    logical function literal_is_nonzero(node) result(nonzero)
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
    end function literal_is_nonzero

    ! True if `name` is a named constant (parameter) in the procedure with a
    ! non-zero literal initializer.
    logical function is_parameter_nonzero(ctx, name, proc_root) result(nonzero)
        type(fluff_ast_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name
        integer, intent(in) :: proc_root

        integer :: i
        character(len=:), allocatable :: lname

        nonzero = .false.
        lname = to_lower_ascii(trim(name))

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            if (proc_root > 0) then
                if (i == proc_root) cycle
            end if
            select type (d => ctx%arena%entries(i)%node)
                type is (declaration_node)
                if (.not. d%is_parameter) cycle
                if (.not. d%has_initializer) cycle
                if (d%initializer_index <= 0) cycle
                if (declares_name(d, lname)) then
                    select type (lit => ctx%arena%entries(d%initializer_index)%node)
                        type is (literal_node)
                        if (literal_is_nonzero(lit)) then
                            nonzero = .true.
                            return
                        end if
                    end select
                end if
            end select
        end do
    end function is_parameter_nonzero

    logical function declares_name(d, lname) result(declared)
        type(declaration_node), intent(in) :: d
        character(len=*), intent(in) :: lname

        integer :: j

        declared = .false.
        if (allocated(d%var_name)) then
            if (to_lower_ascii(trim(d%var_name)) == lname) then
                declared = .true.
                return
            end if
        end if
        if (allocated(d%var_names)) then
            do j = 1, size(d%var_names)
                if (to_lower_ascii(trim(d%var_names(j))) == lname) then
                    declared = .true.
                    return
                end if
            end do
        end if
    end function declares_name

    ! True if every assignment to `name` in the enclosing procedure that
    ! precedes the division assigns a provably non-zero value.
    logical function all_defs_nonzero(ctx, name, proc_root, div_idx, parent) result(ok)
        type(fluff_ast_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name
        integer, intent(in) :: proc_root
        integer, intent(in) :: div_idx
        integer, allocatable, intent(in) :: parent(:)

        integer :: i, found_count
        character(len=:), allocatable :: lname
        logical :: def_ok

        ok = .false.
        lname = to_lower_ascii(trim(name))
        found_count = 0

        do i = 1, ctx%arena%size
            if (i >= div_idx) exit
            if (proc_root > 0) then
                if (.not. is_descendant(ctx, i, proc_root, parent)) cycle
            end if
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (a => ctx%arena%entries(i)%node)
                type is (assignment_node)
                if (.not. assignment_targets_name(ctx, a, lname)) cycle
                found_count = found_count + 1
                if (a%value_index <= 0) then
                    return
                end if
                def_ok = denom_proven_nonzero(ctx, a%value_index, div_idx, parent)
                if (.not. def_ok) return
            end select
        end do

        ok = (found_count > 0)
    end function all_defs_nonzero

    logical function assignment_targets_name(ctx, a, lname) result(targeted)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(assignment_node), intent(in) :: a
        character(len=*), intent(in) :: lname

        targeted = .false.
        if (a%target_index <= 0) return
        if (.not. allocated(ctx%arena%entries(a%target_index)%node)) return
        select type (t => ctx%arena%entries(a%target_index)%node)
            type is (identifier_node)
            if (allocated(t%name)) then
                targeted = (to_lower_ascii(trim(t%name)) == lname)
            end if
        end select
    end function assignment_targets_name

    ! True if `idx` is within the subtree rooted at `root`.
    recursive logical function is_descendant(ctx, idx, root, parent) result(desc)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: idx
        integer, intent(in) :: root
        integer, allocatable, intent(in) :: parent(:)

        integer :: cur

        desc = .false.
        if (idx == root) then
            desc = .true.
            return
        end if
        if (idx <= 0) return
        if (.not. allocated(parent)) return
        if (idx > ubound(parent, 1)) return

        cur = idx
        do
            if (cur == root) then
                desc = .true.
                return
            end if
            if (cur <= 0 .or. cur > ubound(parent, 1)) return
            if (parent(cur) <= 0) return
            cur = parent(cur)
        end do
    end function is_descendant

    ! True if a dominating guard proves `name` non-zero on the path to div_idx.
    logical function guarded_nonzero(ctx, name, div_idx, parent) result(guarded)
        type(fluff_ast_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name
        integer, intent(in) :: div_idx
        integer, allocatable, intent(in) :: parent(:)

        integer :: cur, p
        character(len=*), parameter :: BRANCH_NONE = "", BRANCH_THEN = "then", &
            BRANCH_ELSE = "else"

        guarded = .false.
        if (.not. allocated(parent)) return
        if (div_idx <= 0 .or. div_idx > ubound(parent, 1)) return

        cur = div_idx
        do
            if (cur <= 0 .or. cur > ubound(parent, 1)) return
            p = parent(cur)
            if (p <= 0 .or. p > ubound(parent, 1)) return
            if (.not. allocated(ctx%arena%entries(p)%node)) return

            select type (ifn => ctx%arena%entries(p)%node)
                type is (if_node)
                block
                    character(len=:), allocatable :: branch
                    branch = branch_of_if(ctx, p, cur)
                    if (branch /= BRANCH_NONE) then
                        if (if_condition_proves_nonzero(ctx, ifn, branch, name)) then
                            guarded = .true.
                            return
                        end if
                    end if
                end block
            end select

            cur = p
        end do
    end function guarded_nonzero

    ! Determine which branch of the if_node `cur` belongs to.
    function branch_of_if(ctx, if_idx, cur) result(branch)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: if_idx
        integer, intent(in) :: cur
        character(len=:), allocatable :: branch

        integer :: j

        branch = ""
        if (.not. allocated(ctx%arena%entries(if_idx)%node)) return
        select type (ifn => ctx%arena%entries(if_idx)%node)
            type is (if_node)
            if (allocated(ifn%then_body_indices)) then
                do j = 1, size(ifn%then_body_indices)
                    if (ifn%then_body_indices(j) == cur) then
                        branch = "then"
                        return
                    end if
                end do
            end if
            if (allocated(ifn%else_body_indices)) then
                do j = 1, size(ifn%else_body_indices)
                    if (ifn%else_body_indices(j) == cur) then
                        branch = "else"
                        return
                    end if
                end do
            end if
        end select
    end function branch_of_if

    ! True if the if condition proves `name` non-zero on the given branch.
    function if_condition_proves_nonzero(ctx, ifn, branch, name) result(proven)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(if_node), intent(in) :: ifn
        character(len=*), intent(in) :: branch
        character(len=*), intent(in) :: name
        logical :: proven

        proven = .false.
        if (ifn%condition_index <= 0) return
        if (.not. allocated(ctx%arena%entries(ifn%condition_index)%node)) return

        select type (c => ctx%arena%entries(ifn%condition_index)%node)
            type is (binary_op_node)
            proven = comparison_proves_nonzero(ctx, c, branch, name)
        end select
    end function if_condition_proves_nonzero

    logical function comparison_proves_nonzero(ctx, c, branch, name) result(proven)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(binary_op_node), intent(in) :: c
        character(len=*), intent(in) :: branch
        character(len=*), intent(in) :: name

        character(len=:), allocatable :: op
        integer :: lidx, ridx
        logical :: l_is_name, r_is_name
        logical :: l_is_abs_name, r_is_abs_name

        proven = .false.
        if (.not. allocated(c%operator)) return
        op = trim(c%operator)
        lidx = c%left_index
        ridx = c%right_index

        l_is_name = expr_is_identifier(ctx, lidx, name)
        r_is_name = expr_is_identifier(ctx, ridx, name)
        l_is_abs_name = expr_is_abs_of_identifier(ctx, lidx, name)
        r_is_abs_name = expr_is_abs_of_identifier(ctx, ridx, name)

        select case (op)
        case ("/=")
            ! name /= 0  (or 0 /= name) proves non-zero on the then branch
            if (branch == "then") then
                if ((l_is_name .and. expr_literal_is_zero(ctx, ridx)) .or. &
                    (r_is_name .and. expr_literal_is_zero(ctx, lidx)) .or. &
                    (l_is_abs_name .and. expr_literal_is_zero(ctx, ridx)) .or. &
                    (r_is_abs_name .and. expr_literal_is_zero(ctx, lidx))) then
                    proven = .true.
                end if
            end if
        case (">", ">=")
            ! name > 0 / name >= positive proves non-zero on then branch
            if (branch == "then") then
                if ((l_is_name .or. l_is_abs_name) .and. &
                    expr_literal_gte(ctx, ridx, 0.0)) then
                    proven = .true.
                end if
            end if
        case ("<", "<=")
            ! 0 < name / negative <= name proves non-zero on then branch
            if (branch == "then") then
                if ((r_is_name .or. r_is_abs_name) .and. &
                    expr_literal_lte(ctx, lidx, 0.0)) then
                    proven = .true.
                end if
            end if
        case ("==")
            ! name == 0 -> the else branch proves non-zero
            if (branch == "else") then
                if ((l_is_name .and. expr_literal_is_zero(ctx, ridx)) .or. &
                    (r_is_name .and. expr_literal_is_zero(ctx, lidx))) then
                    proven = .true.
                end if
            end if
        end select
    end function comparison_proves_nonzero

    logical function expr_is_identifier(ctx, idx, name) result(match)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name

        match = .false.
        if (idx <= 0) return
        if (.not. allocated(ctx%arena%entries(idx)%node)) return
        select type (n => ctx%arena%entries(idx)%node)
            type is (identifier_node)
            if (allocated(n%name)) then
                match = (to_lower_ascii(trim(n%name)) == to_lower_ascii(trim(name)))
            end if
        end select
    end function expr_is_identifier

    logical function expr_is_abs_of_identifier(ctx, idx, name) result(match)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: idx
        character(len=*), intent(in) :: name

        match = .false.
        if (idx <= 0) return
        if (.not. allocated(ctx%arena%entries(idx)%node)) return
        select type (n => ctx%arena%entries(idx)%node)
            type is (call_or_subscript_node)
            if (n%is_array_access) return
            if (.not. allocated(n%name)) return
            if (to_lower_ascii(trim(n%name)) /= "abs") return
            if (.not. allocated(n%arg_indices)) return
            if (size(n%arg_indices) < 1) return
            match = expr_is_identifier(ctx, n%arg_indices(1), name)
        end select
    end function expr_is_abs_of_identifier

    logical function expr_literal_is_zero(ctx, idx) result(is_zero)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: idx

        real :: rval

        is_zero = .false.
        if (idx <= 0) return
        if (.not. allocated(ctx%arena%entries(idx)%node)) return
        select type (n => ctx%arena%entries(idx)%node)
            type is (literal_node)
            if (.not. literal_is_nonzero(n)) then
                if (parse_literal_real(n, rval)) then
                    is_zero = (rval == 0.0)
                end if
            end if
        end select
    end function expr_literal_is_zero

    logical function expr_literal_gte(ctx, idx, bound) result(gte)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: idx
        real, intent(in) :: bound

        real :: rval

        gte = .false.
        if (idx <= 0) return
        if (.not. allocated(ctx%arena%entries(idx)%node)) return
        select type (n => ctx%arena%entries(idx)%node)
            type is (literal_node)
            if (parse_literal_real(n, rval)) then
                gte = (rval >= bound)
            end if
        end select
    end function expr_literal_gte

    logical function expr_literal_lte(ctx, idx, bound) result(lte)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: idx
        real, intent(in) :: bound

        real :: rval

        lte = .false.
        if (idx <= 0) return
        if (.not. allocated(ctx%arena%entries(idx)%node)) return
        select type (n => ctx%arena%entries(idx)%node)
            type is (literal_node)
            if (parse_literal_real(n, rval)) then
                lte = (rval <= bound)
            end if
        end select
    end function expr_literal_lte

    logical function parse_literal_real(node, rval) result(ok)
        type(literal_node), intent(in) :: node
        real, intent(out) :: rval

        character(len=:), allocatable :: v
        integer :: ios

        ok = .false.
        if (.not. allocated(node%value)) return
        v = trim(node%value)
        if (len_trim(v) == 0) return
        if (v(1:1) == '"' .or. v(1:1) == "'") return
        read (v, *, iostat=ios) rval
        ok = (ios == 0)
    end function parse_literal_real

end module fluff_rule_c002
