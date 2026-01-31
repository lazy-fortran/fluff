module fluff_rule_p007
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fortfront, only: binary_op_node, call_or_subscript_node, declaration_node, &
                         identifier_node, literal_node, symbol_info_t, TREAL
    implicit none
    private

    public :: check_p007_mixed_precision

    type :: var_prop_t
        character(len=:), allocatable :: name
        logical :: is_array = .false.
        integer :: real_kind = -1
    end type var_prop_t

contains

    subroutine check_p007_mixed_precision(ctx, node_index, violations)
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
        call analyze_p007(ctx, props, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p007_mixed_precision

    subroutine analyze_p007(ctx, props, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(var_prop_t), allocatable, intent(in) :: props(:)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: i

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (b => ctx%arena%entries(i)%node)
            type is (binary_op_node)
                if (binary_op_is_mixed_precision(ctx, i, props)) then
                    call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                         code="P007", &
                                         message="Mixed precision arithmetic can "// &
                                         "hurt performance", &
                                         file_path="", &
                                         location=ctx%get_node_location(i), &
                                         severity=SEVERITY_INFO &
                                         ))
                end if
            end select
        end do
    end subroutine analyze_p007

    logical function binary_op_is_mixed_precision(ctx, node_index, props) &
        result(is_mixed)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(var_prop_t), allocatable, intent(in) :: props(:)

        integer :: k1, k2
        character(len=:), allocatable :: op
        integer :: lidx, ridx

        is_mixed = .false.
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (b => ctx%arena%entries(node_index)%node)
        type is (binary_op_node)
            if (.not. allocated(b%operator)) return
            op = trim(b%operator)
            if (op /= "+" .and. op /= "-" .and. op /= "*" .and. op /= "/" .and. &
                op /= "**") return
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

    recursive integer function expr_real_kind(ctx, node_index, props) result(kind_val)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(var_prop_t), allocatable, intent(in) :: props(:)

        character(len=:), allocatable :: name
        type(symbol_info_t) :: sym
        integer :: sub_kind

        kind_val = -1
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (identifier_node)
            if (.not. allocated(n%name)) return
            name = to_lower_ascii(trim(n%name))

            sym = ctx%lookup_symbol(name)
            if (sym%is_defined .and. sym%type_info%kind == TREAL) then
                kind_val = prop_real_kind(props, name)
                if (kind_val > 0) return
            end if

            kind_val = prop_real_kind(props, name)

        type is (call_or_subscript_node)
            if (.not. allocated(n%name)) return
            name = to_lower_ascii(trim(n%name))

            if (is_real_returning_intrinsic(name)) then
                kind_val = infer_intrinsic_kind(ctx, n, props)
                return
            end if

            sym = ctx%lookup_symbol(name)
            if (sym%is_defined .and. sym%type_info%kind == TREAL) then
                kind_val = prop_real_kind(props, name)
                if (kind_val > 0) return
            end if

            if (allocated(n%arg_indices)) then
                if (size(n%arg_indices) == 0) then
                    kind_val = prop_real_kind(props, name)
                end if
            else
                kind_val = prop_real_kind(props, name)
            end if

        type is (literal_node)
            if (.not. allocated(n%value)) return
            kind_val = infer_literal_kind(n%value)

        type is (binary_op_node)
            sub_kind = expr_real_kind(ctx, n%left_index, props)
            if (sub_kind >= 0) then
                kind_val = sub_kind
                return
            end if
            kind_val = expr_real_kind(ctx, n%right_index, props)
        end select
    end function expr_real_kind

    integer function infer_literal_kind(value) result(kind_val)
        character(len=*), intent(in) :: value

        integer :: underscore_pos, ios
        character(len=32) :: kind_str

        kind_val = -1

        if (index(value, "d") > 0 .or. index(value, "D") > 0) then
            kind_val = 8
            return
        end if

        if (index(value, "e") > 0 .or. index(value, "E") > 0) then
            kind_val = 4
        end if

        underscore_pos = index(value, "_")
        if (underscore_pos > 0 .and. underscore_pos < len_trim(value)) then
            kind_str = value(underscore_pos + 1:)
            read (kind_str, *, iostat=ios) kind_val
            if (ios /= 0) kind_val = -1
            return
        end if

        if (index(value, ".") > 0 .and. kind_val < 0) then
            kind_val = 4
        end if
    end function infer_literal_kind

    logical function is_real_returning_intrinsic(name) result(is_real)
        character(len=*), intent(in) :: name

        is_real = .false.

        select case (name)
        case ("real", "dble", "float", "sngl", "sin", "cos", "tan", &
              "asin", "acos", "atan", "atan2", "sinh", "cosh", "tanh", &
              "sqrt", "exp", "log", "log10", "abs", "mod", "modulo", &
              "floor", "ceiling", "nint", "sign", "min", "max", &
              "sum", "product", "maxval", "minval", "dot_product", &
              "huge", "tiny", "epsilon")
            is_real = .true.
        end select
    end function is_real_returning_intrinsic

    integer function infer_intrinsic_kind(ctx, node, props) result(kind_val)
        type(fluff_ast_context_t), intent(in) :: ctx
        type(call_or_subscript_node), intent(in) :: node
        type(var_prop_t), allocatable, intent(in) :: props(:)

        character(len=:), allocatable :: name

        kind_val = -1
        if (.not. allocated(node%name)) return

        name = to_lower_ascii(trim(node%name))

        select case (name)
        case ("dble")
            kind_val = 8
        case ("sngl", "float")
            kind_val = 4
        case ("real")
            if (allocated(node%arg_indices)) then
                if (size(node%arg_indices) >= 2) then
                    kind_val = expr_real_kind(ctx, node%arg_indices(2), props)
                else if (size(node%arg_indices) == 1) then
                    kind_val = 4
                end if
            end if
        case default
            if (allocated(node%arg_indices)) then
                if (size(node%arg_indices) >= 1) then
                    kind_val = expr_real_kind(ctx, node%arg_indices(1), props)
                end if
            end if
        end select
    end function infer_intrinsic_kind

    subroutine collect_var_props(ctx, node_index, props)
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
                    rk = 4
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

    integer function parse_kind_from_type_name(type_name) result(kind_val)
        character(len=*), intent(in) :: type_name
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

    integer function prop_real_kind(props, name) result(kind_val)
        type(var_prop_t), allocatable, intent(in) :: props(:)
        character(len=*), intent(in) :: name
        integer :: i

        kind_val = -1
        do i = 1, size(props)
            if (props(i)%name == name) then
                kind_val = props(i)%real_kind
                return
            end if
        end do
    end function prop_real_kind

end module fluff_rule_p007
