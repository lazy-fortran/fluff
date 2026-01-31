module fluff_rule_p004
    use fluff_ast, only: fluff_ast_context_t, NODE_FUNCTION_DEF, NODE_SUBROUTINE_DEF
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: allocate_statement_node, call_or_subscript_node, &
                         declaration_node, error_stop_node, function_def_node, &
                         identifier_node, print_statement_node, read_statement_node, &
                         stop_node, subroutine_call_node, subroutine_def_node, &
                         symbol_info_t, write_statement_node
    implicit none
    private

    public :: check_p004_pure_elemental

contains

    subroutine check_p004_pure_elemental(ctx, node_index, violations)
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
                    call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                         code="P004", &
                            message="Consider adding pure attribute for optimization", &
                                         file_path=current_filename, &
                                         location=ctx%get_node_location(i), &
                                         severity=SEVERITY_INFO))
                end if
            end if
        end do

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p004_pure_elemental

    logical function procedure_is_pure_candidate(ctx, node_index) result(ok)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index

        ok = .false.
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (p => ctx%arena%entries(node_index)%node)
        type is (function_def_node)
            if (has_prefix(p%prefix_keywords, "pure")) return
            if (has_prefix(p%prefix_keywords, "elemental")) return
            if (procedure_has_side_effects(ctx, node_index)) return
            if (procedure_accesses_global_state(ctx, node_index)) return
            if (procedure_has_intent_out_args(ctx, node_index)) return
            ok = .true.
        type is (subroutine_def_node)
            if (has_prefix(p%prefix_keywords, "pure")) return
            if (has_prefix(p%prefix_keywords, "elemental")) return
            if (procedure_has_side_effects(ctx, node_index)) return
            if (procedure_accesses_global_state(ctx, node_index)) return
            ok = .true.
        end select
    end function procedure_is_pure_candidate

    recursive logical function procedure_has_side_effects(ctx, node_index) result(has)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index

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
                if (.not. is_pure_intrinsic_call(ctx, n%name)) then
                    has = .true.
                    return
                end if
            type is (call_or_subscript_node)
                if (is_impure_procedure_call(ctx, n)) then
                    has = .true.
                    return
                end if
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

    logical function procedure_accesses_global_state(ctx, proc_index) result(has)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: proc_index

        has = .false.
    end function procedure_accesses_global_state

    logical function procedure_has_intent_out_args(ctx, proc_index) result(has)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: proc_index

        integer :: i
        character(len=:), allocatable :: intent_str

        has = .false.

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            if (ctx%arena%entries(i)%parent_index /= proc_index) cycle

            select type (n => ctx%arena%entries(i)%node)
            type is (declaration_node)
                if (n%has_intent .and. allocated(n%intent)) then
                    intent_str = to_lower_ascii(trim(n%intent))
                    if (intent_str == "out" .or. intent_str == "inout") then
                        has = .true.
                        return
                    end if
                end if
            end select
        end do
    end function procedure_has_intent_out_args

    logical function is_pure_intrinsic_call(ctx, name) result(is_pure)
        type(fluff_ast_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name

        character(len=:), allocatable :: lname

        is_pure = .false.
        if (len_trim(name) == 0) return

        lname = to_lower_ascii(trim(name))

        select case (lname)
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", &
              "sinh", "cosh", "tanh", "sqrt", "exp", "log", "log10", &
              "abs", "mod", "modulo", "min", "max", "floor", "ceiling", &
              "nint", "sign", "real", "int", "dble", "cmplx", "char", &
              "ichar", "len", "len_trim", "trim", "adjustl", "adjustr", &
              "index", "scan", "verify", "repeat", "size", "shape", &
              "lbound", "ubound", "sum", "product", "maxval", "minval", &
              "count", "any", "all", "merge", "pack", "unpack", "reshape", &
              "transpose", "matmul", "dot_product", "spread", &
              "kind", "huge", "tiny", "epsilon", "precision", "range", &
              "digits", "bit_size", "iand", "ior", "ieor", "not", "btest", &
              "ibset", "ibclr", "ishft", "transfer", "logical", &
              "allocated", "present", "associated")
            is_pure = .true.
        end select
    end function is_pure_intrinsic_call

    logical function is_impure_procedure_call(ctx, node) result(is_impure)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(call_or_subscript_node), intent(in) :: node

        character(len=:), allocatable :: lname

        is_impure = .false.
        if (.not. allocated(node%name)) return

        lname = to_lower_ascii(trim(node%name))

        if (is_pure_intrinsic_call(ctx, lname)) return

        if (ctx%is_symbol_defined(lname)) then
            is_impure = .true.
        end if
    end function is_impure_procedure_call

    logical function has_prefix(prefix_keywords, key) result(found)
        character(len=16), allocatable, intent(in) :: prefix_keywords(:)
        character(len=*), intent(in) :: key

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

end module fluff_rule_p004
