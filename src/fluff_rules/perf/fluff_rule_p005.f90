module fluff_rule_p005
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: assignment_node, binary_op_node, call_or_subscript_node, &
                         declaration_node, do_loop_node, identifier_node, &
                         symbol_info_t, TCHAR
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
        integer :: concat_node

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (loop => ctx%arena%entries(i)%node)
            type is (do_loop_node)
                if (.not. allocated(loop%body_indices)) cycle
                do j = 1, size(loop%body_indices)
                    concat_node = find_string_concat_node(ctx, loop%body_indices(j))
                    if (concat_node > 0) then
                        call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                             code="P005", &
                             message="String concatenation in loops can be expensive", &
                                             file_path=current_filename, &
                                          location=ctx%get_node_location(concat_node), &
                                             severity=SEVERITY_INFO))
                        exit
                    end if
                end do
            end select
        end do
    end subroutine analyze_p005

    recursive integer function find_string_concat_node(ctx, node_index) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        integer :: i, child_result

        found = 0
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (binary_op_node)
            if (allocated(n%operator)) then
                if (trim(n%operator) == "//") then
                    if (involves_character_operands(ctx, node_index)) then
                        found = node_index
                        return
                    end if
                end if
            end if
            child_result = find_string_concat_node(ctx, n%left_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
            child_result = find_string_concat_node(ctx, n%right_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
        type is (assignment_node)
            child_result = find_string_concat_node(ctx, n%target_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
            child_result = find_string_concat_node(ctx, n%value_index)
            if (child_result > 0) then
                found = child_result
                return
            end if
        type is (call_or_subscript_node)
            if (allocated(n%arg_indices)) then
                do i = 1, size(n%arg_indices)
                    child_result = find_string_concat_node(ctx, n%arg_indices(i))
                    if (child_result > 0) then
                        found = child_result
                        return
                    end if
                end do
            end if
        type is (do_loop_node)
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    child_result = find_string_concat_node(ctx, n%body_indices(i))
                    if (child_result > 0) then
                        found = child_result
                        return
                    end if
                end do
            end if
        end select
    end function find_string_concat_node

    logical function involves_character_operands(ctx, bin_op_index) result(is_char)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: bin_op_index

        integer :: left_idx, right_idx

        is_char = .true.

        if (.not. allocated(ctx%arena%entries(bin_op_index)%node)) return

        select type (n => ctx%arena%entries(bin_op_index)%node)
        type is (binary_op_node)
            left_idx = n%left_index
            right_idx = n%right_index
        class default
            return
        end select

       if (is_character_expr(ctx, left_idx) .or. is_character_expr(ctx, right_idx)) then
            is_char = .true.
        end if
    end function involves_character_operands

    recursive logical function is_character_expr(ctx, node_index) result(is_char)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index

        type(symbol_info_t) :: sym
        character(len=:), allocatable :: name

        is_char = .false.
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (identifier_node)
            if (.not. allocated(n%name)) return
            name = to_lower_ascii(trim(n%name))
            sym = ctx%lookup_symbol(name)
            if (sym%is_defined .and. sym%type_info%kind == TCHAR) then
                is_char = .true.
                return
            end if
            if (is_declared_as_character(ctx, name)) then
                is_char = .true.
                return
            end if
        type is (binary_op_node)
            if (allocated(n%operator)) then
                if (trim(n%operator) == "//") then
                    is_char = .true.
                    return
                end if
            end if
            is_char = is_character_expr(ctx, n%left_index) .or. &
                      is_character_expr(ctx, n%right_index)
        type is (call_or_subscript_node)
            if (.not. allocated(n%name)) return
            name = to_lower_ascii(trim(n%name))
            if (is_string_intrinsic(name)) then
                is_char = .true.
                return
            end if
            sym = ctx%lookup_symbol(name)
            if (sym%is_defined .and. sym%type_info%kind == TCHAR) then
                is_char = .true.
                return
            end if
            if (is_declared_as_character(ctx, name)) then
                is_char = .true.
                return
            end if
        end select
    end function is_character_expr

    logical function is_declared_as_character(ctx, name) result(is_char)
        type(fluff_ast_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name

        integer :: i, j
        character(len=:), allocatable :: tname, vname

        is_char = .false.

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle

            select type (n => ctx%arena%entries(i)%node)
            type is (declaration_node)
                if (.not. allocated(n%type_name)) cycle
                tname = to_lower_ascii(trim(n%type_name))
                if (index(tname, "character") /= 1) cycle

                if (n%is_multi_declaration .and. allocated(n%var_names)) then
                    do j = 1, size(n%var_names)
                        vname = to_lower_ascii(trim(n%var_names(j)))
                        if (vname == name) then
                            is_char = .true.
                            return
                        end if
                    end do
                else if (allocated(n%var_name)) then
                    vname = to_lower_ascii(trim(n%var_name))
                    if (vname == name) then
                        is_char = .true.
                        return
                    end if
                end if
            end select
        end do
    end function is_declared_as_character

    logical function is_string_intrinsic(name) result(is_str)
        character(len=*), intent(in) :: name

        is_str = .false.

        select case (name)
        case ("trim", "adjustl", "adjustr", "repeat", "char", "achar", &
              "new_line", "lge", "lgt", "lle", "llt")
            is_str = .true.
        end select
    end function is_string_intrinsic

end module fluff_rule_p005
