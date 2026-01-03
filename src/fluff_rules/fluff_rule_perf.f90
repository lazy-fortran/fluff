module fluff_rule_perf
    use fluff_ast, only: fluff_ast_context_t, NODE_FUNCTION_DEF, NODE_SUBROUTINE_DEF
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO, &
                                 SEVERITY_WARNING
    use fluff_core, only: source_range_t
    use fortfront, only: assignment_node, binary_op_node, call_or_subscript_node, &
                         declaration_node, do_loop_node, function_def_node, &
                         identifier_node, literal_node, subroutine_def_node, &
                         allocate_statement_node, subroutine_call_node, &
                         print_statement_node, write_statement_node, &
                         read_statement_node, stop_node, error_stop_node
    implicit none
    private

    public :: check_p002_loop_ordering_impl
    public :: check_p003_array_temporaries_impl
    public :: check_p004_pure_elemental_impl
    public :: check_p005_string_operations_impl
    public :: check_p006_loop_allocations_impl
    public :: check_p007_mixed_precision_impl

    type :: var_prop_t
        character(len=:), allocatable :: name
        logical :: is_array = .false.
        integer :: real_kind = -1
    end type var_prop_t

contains

    subroutine check_p002_loop_ordering_impl(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        allocate (tmp(16))
        violation_count = 0

        call analyze_p002(ctx, node_index, "", tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p002_loop_ordering_impl

    recursive subroutine analyze_p002(ctx, node_index, outer_var, tmp, &
                                      violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: outer_var
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: i, j
        character(len=:), allocatable :: v_outer, v_inner

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (outer_loop => ctx%arena%entries(i)%node)
            type is (do_loop_node)
                if (.not. allocated(outer_loop%var_name)) cycle
                if (.not. allocated(outer_loop%body_indices)) cycle
                v_outer = to_lower_ascii(trim(outer_loop%var_name))

                do j = 1, size(outer_loop%body_indices)
              if (.not. allocated(ctx%arena%entries(outer_loop%body_indices(j))%node)) &
                        cycle
                    select type (inner_loop => &
                                 ctx%arena%entries(outer_loop%body_indices(j))%node)
                    type is (do_loop_node)
                        if (.not. allocated(inner_loop%var_name)) cycle
                        v_inner = to_lower_ascii(trim(inner_loop%var_name))
                        if (loop_has_inefficient_access(ctx, &
                                                        outer_loop%body_indices(j), &
                                                        v_outer, v_inner)) then
                            call push_violation(tmp, violation_count, &
                                                create_diagnostic( &
                                                code="P002", &
                     message="Consider swapping nested loops for column-major access", &
                                                file_path="", &
                           location=ctx%get_node_location(outer_loop%body_indices(j)), &
                                                severity=SEVERITY_INFO))
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
                                                   inner_var) &
        result(found)
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
            if (is_inefficient_2d_access(ctx, node_index, outer_var, &
                                         inner_var)) then
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

    subroutine check_p003_array_temporaries_impl(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(var_prop_t), allocatable :: props(:)
        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        allocate (props(0))
        allocate (tmp(16))
        violation_count = 0

        call collect_var_props(ctx, node_index, props)
        call analyze_p003(ctx, node_index, props, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p003_array_temporaries_impl

    recursive subroutine analyze_p003(ctx, node_index, props, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(var_prop_t), allocatable, intent(in) :: props(:)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: i

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (n => ctx%arena%entries(i)%node)
            type is (assignment_node)
                if (is_whole_array_binary_assignment(ctx, i, props)) then
                    call push_violation(tmp, violation_count, create_diagnostic( &
                                        code="P003", &
                              message="Whole-array expression may create temporaries", &
                                        file_path="", &
                                        location=ctx%get_node_location(i), &
                                        severity=SEVERITY_INFO))
                end if
            end select
        end do
    end subroutine analyze_p003

    function is_whole_array_binary_assignment(ctx, node_index, props) result(is_bad)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(var_prop_t), allocatable, intent(in) :: props(:)
        logical :: is_bad

        integer :: target_idx, value_idx
        character(len=:), allocatable :: tname, l1, l2
        character(len=:), allocatable :: op

        is_bad = .false.
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (a => ctx%arena%entries(node_index)%node)
        type is (assignment_node)
            target_idx = a%target_index
            value_idx = a%value_index
        class default
            return
        end select

        call get_identifier_arg_name(ctx, target_idx, tname)
        if (.not. allocated(tname)) return
        if (.not. prop_is_array(props, tname)) return

        if (value_idx <= 0) return
        if (.not. allocated(ctx%arena%entries(value_idx)%node)) return

        select type (b => ctx%arena%entries(value_idx)%node)
        type is (binary_op_node)
            if (.not. allocated(b%operator)) return
            op = trim(b%operator)
            if (op /= "+" .and. op /= "-" .and. op /= "*" .and. op /= "/") return
            call get_identifier_arg_name(ctx, b%left_index, l1)
            call get_identifier_arg_name(ctx, b%right_index, l2)
        class default
            return
        end select

        if (.not. allocated(l1) .or. .not. allocated(l2)) return
        if (prop_is_array(props, l1) .and. prop_is_array(props, l2)) then
            is_bad = .true.
        end if
    end function is_whole_array_binary_assignment

    subroutine check_p004_pure_elemental_impl(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count
        integer :: i

        allocate (tmp(16))
        violation_count = 0

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            if (ctx%get_node_type(i) == NODE_FUNCTION_DEF .or. &
                ctx%get_node_type(i) == NODE_SUBROUTINE_DEF) then
                if (procedure_is_pure_candidate(ctx, i)) then
                    call push_violation(tmp, violation_count, create_diagnostic( &
                                        code="P004", &
                            message="Consider adding pure attribute for optimization", &
                                        file_path="", &
                                        location=ctx%get_node_location(i), &
                                        severity=SEVERITY_INFO))
                end if
            end if
        end do

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p004_pure_elemental_impl

    function procedure_is_pure_candidate(ctx, node_index) result(ok)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        logical :: ok

        ok = .false.
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (p => ctx%arena%entries(node_index)%node)
        type is (function_def_node)
            if (has_prefix(p%prefix_keywords, "pure")) return
            if (procedure_has_side_effects(ctx, node_index)) return
            ok = .true.
        type is (subroutine_def_node)
            if (has_prefix(p%prefix_keywords, "pure")) return
            if (procedure_has_side_effects(ctx, node_index)) return
            ok = .true.
        end select
    end function procedure_is_pure_candidate

    recursive function procedure_has_side_effects(ctx, node_index) result(has)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        logical :: has

        integer, allocatable :: children(:)
        integer :: i

        has = .false.
        if (node_index <= 0) return

        if (allocated(ctx%arena%entries(node_index)%node)) then
            select type (n => ctx%arena%entries(node_index)%node)
            type is (print_statement_node)
                has = .true.
                return
            type is (write_statement_node)
                has = .true.
                return
            type is (read_statement_node)
                has = .true.
                return
            type is (allocate_statement_node)
                has = .true.
                return
            type is (subroutine_call_node)
                has = .true.
                return
            type is (stop_node)
                has = .true.
                return
            type is (error_stop_node)
                has = .true.
                return
            end select
        end if

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) > 0) then
                if (procedure_has_side_effects(ctx, children(i))) then
                    has = .true.
                    exit
                end if
            end if
        end do
        if (allocated(children)) deallocate (children)
    end function procedure_has_side_effects

    subroutine check_p005_string_operations_impl(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        allocate (tmp(16))
        violation_count = 0

        call analyze_p005(ctx, node_index, .false., tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p005_string_operations_impl

    recursive subroutine analyze_p005(ctx, node_index, in_loop, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        logical, intent(in) :: in_loop
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
                        call push_violation(tmp, violation_count, create_diagnostic( &
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

    function binary_op_is_concat(ctx, node_index) result(is_concat)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        logical :: is_concat

        is_concat = .false.
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return
        select type (b => ctx%arena%entries(node_index)%node)
        type is (binary_op_node)
            if (allocated(b%operator)) then
                is_concat = (trim(b%operator) == "//")
            end if
        end select
    end function binary_op_is_concat

    subroutine check_p006_loop_allocations_impl(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        allocate (tmp(16))
        violation_count = 0

        call analyze_p006(ctx, node_index, .false., tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p006_loop_allocations_impl

    recursive subroutine analyze_p006(ctx, node_index, in_loop, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        logical, intent(in) :: in_loop
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
                        call push_violation(tmp, violation_count, create_diagnostic( &
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

    subroutine check_p007_mixed_precision_impl(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(var_prop_t), allocatable :: props(:)
        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        allocate (props(0))
        allocate (tmp(16))
        violation_count = 0

        call collect_var_props(ctx, node_index, props)
        call analyze_p007(ctx, node_index, props, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p007_mixed_precision_impl

    recursive subroutine analyze_p007(ctx, node_index, props, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(var_prop_t), allocatable, intent(in) :: props(:)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: i

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (b => ctx%arena%entries(i)%node)
            type is (binary_op_node)
                if (binary_op_is_mixed_precision(ctx, i, props)) then
                    call push_violation(tmp, violation_count, create_diagnostic( &
                                        code="P007", &
                            message="Mixed precision arithmetic can hurt performance", &
                                        file_path="", &
                                        location=ctx%get_node_location(i), &
                                        severity=SEVERITY_INFO))
                end if
            end select
        end do
    end subroutine analyze_p007

    function binary_op_is_mixed_precision(ctx, node_index, props) result(is_mixed)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(var_prop_t), allocatable, intent(in) :: props(:)
        logical :: is_mixed

        integer :: k1, k2
        character(len=:), allocatable :: op
        integer :: lidx, ridx

        is_mixed = .false.
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (b => ctx%arena%entries(node_index)%node)
        type is (binary_op_node)
            if (.not. allocated(b%operator)) return
            op = trim(b%operator)
            if (op /= "+" .and. op /= "-" .and. op /= "*" .and. op /= "/") return
            lidx = b%left_index
            ridx = b%right_index
        class default
            return
        end select

        k1 = expr_real_kind(ctx, lidx, props)
        k2 = expr_real_kind(ctx, ridx, props)
        if (k1 < 0 .or. k2 < 0) return
        is_mixed = (k1 /= k2)
    end function binary_op_is_mixed_precision

    function expr_real_kind(ctx, node_index, props) result(kind_val)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(var_prop_t), allocatable, intent(in) :: props(:)
        integer :: kind_val

        character(len=:), allocatable :: name

        kind_val = -1
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (identifier_node)
            if (.not. allocated(n%name)) return
            name = to_lower_ascii(trim(n%name))
            kind_val = prop_real_kind(props, name)
        type is (call_or_subscript_node)
            if (.not. allocated(n%name)) return
            if (allocated(n%arg_indices)) then
                if (size(n%arg_indices) > 0) return
            end if
            name = to_lower_ascii(trim(n%name))
            kind_val = prop_real_kind(props, name)
        type is (literal_node)
            if (.not. allocated(n%value)) return
            if (index(n%value, "d") > 0 .or. index(n%value, "D") > 0) then
                kind_val = 8
            else
                kind_val = 0
            end if
        end select
    end function expr_real_kind

    recursive subroutine collect_var_props(ctx, node_index, props)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(var_prop_t), allocatable, intent(inout) :: props(:)
        integer :: i

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (d => ctx%arena%entries(i)%node)
            type is (declaration_node)
                call add_decl_props(ctx, i, props)
            end select
        end do
    end subroutine collect_var_props

    recursive function node_has_concat(ctx, node_index) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        logical :: found
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

    recursive function node_has_allocate(ctx, node_index) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        logical :: found
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

    subroutine add_decl_props(ctx, node_index, props)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(var_prop_t), allocatable, intent(inout) :: props(:)

        character(len=:), allocatable :: tname
        character(len=:), allocatable :: vname
        integer :: rk
        integer :: i

        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (d => ctx%arena%entries(node_index)%node)
        type is (declaration_node)
            if (.not. allocated(d%type_name)) return
            tname = to_lower_ascii(trim(d%type_name))
            rk = -1
            if (tname == "real") then
                if (d%has_kind) then
                    rk = d%kind_value
                else
                    rk = 0
                end if
            else if (tname == "double precision" .or. tname == "doubleprecision") then
                rk = 8
            else if (index(tname, "real(") == 1) then
                rk = parse_kind_from_type_name(tname)
            end if
            if (d%is_multi_declaration .and. allocated(d%var_names)) then
                do i = 1, size(d%var_names)
                    vname = to_lower_ascii(trim(d%var_names(i)))
                    call upsert_prop(props, vname, d%is_array, rk)
                end do
            else if (allocated(d%var_name)) then
                vname = to_lower_ascii(trim(d%var_name))
                call upsert_prop(props, vname, d%is_array, rk)
            end if
        end select
    end subroutine add_decl_props

    function parse_kind_from_type_name(type_name) result(kind_val)
        character(len=*), intent(in) :: type_name
        integer :: kind_val

        integer :: lpar, rpar, ios
        character(len=32) :: buf

        kind_val = -1
        lpar = index(type_name, "(")
        rpar = index(type_name, ")")
        if (lpar <= 0 .or. rpar <= lpar) return

        buf = ""
        if (rpar - lpar - 1 > 0) then
            buf = type_name(lpar + 1:rpar - 1)
            read (buf, *, iostat=ios) kind_val
            if (ios /= 0) kind_val = -1
        end if
    end function parse_kind_from_type_name

    subroutine upsert_prop(props, name, is_array, real_kind)
        type(var_prop_t), allocatable, intent(inout) :: props(:)
        character(len=*), intent(in) :: name
        logical, intent(in) :: is_array
        integer, intent(in) :: real_kind

        integer :: i

        do i = 1, size(props)
            if (props(i)%name == name) then
                props(i)%is_array = props(i)%is_array .or. is_array
                if (real_kind >= 0) props(i)%real_kind = real_kind
                return
            end if
        end do

        props = [props, var_prop_t(name=name, is_array=is_array, real_kind=real_kind)]
    end subroutine upsert_prop

    function prop_is_array(props, name) result(is_array)
        type(var_prop_t), allocatable, intent(in) :: props(:)
        character(len=*), intent(in) :: name
        logical :: is_array

        integer :: i

        is_array = .false.
        do i = 1, size(props)
            if (props(i)%name == name) then
                is_array = props(i)%is_array
                return
            end if
        end do
    end function prop_is_array

    function prop_real_kind(props, name) result(kind_val)
        type(var_prop_t), allocatable, intent(in) :: props(:)
        character(len=*), intent(in) :: name
        integer :: kind_val

        integer :: i

        kind_val = -1
        do i = 1, size(props)
            if (props(i)%name == name) then
                kind_val = props(i)%real_kind
                return
            end if
        end do
    end function prop_real_kind

    function has_prefix(prefix_keywords, key) result(found)
        character(len=16), allocatable, intent(in) :: prefix_keywords(:)
        character(len=*), intent(in) :: key
        logical :: found

        integer :: i
        character(len=:), allocatable :: kw

        found = .false.
        if (.not. allocated(prefix_keywords)) return
        do i = 1, size(prefix_keywords)
            kw = to_lower_ascii(trim(prefix_keywords(i)))
            if (kw == to_lower_ascii(trim(key))) then
                found = .true.
                exit
            end if
        end do
    end function has_prefix

    subroutine push_violation(tmp, count, diag)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: count
        type(diagnostic_t), intent(in) :: diag

        type(diagnostic_t), allocatable :: grown(:)

        if (count >= size(tmp)) then
            allocate (grown(max(2*size(tmp), 8)))
            if (count > 0) grown(1:count) = tmp(1:count)
            call move_alloc(grown, tmp)
        end if

        count = count + 1
        tmp(count) = diag
    end subroutine push_violation

    pure function to_lower_ascii(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out

        integer :: i, c

        out = s
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) then
                out(i:i) = achar(c + 32)
            end if
        end do
    end function to_lower_ascii

end module fluff_rule_perf
