module fluff_rule_identifier_checks
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic
    use fluff_rule_file_context, only: current_filename
    use fluff_rule_symbol_collect, only: collect_declared_names, is_in_same_unit, &
                                         is_in_subtree, name_in_list
    use fortfront, only: identifier_node
    implicit none
    private

    public :: check_undefined_variable_usage

contains

    subroutine check_undefined_variable_usage(ctx, node_index, code, severity, &
                                              message_prefix, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: code
        integer, intent(in) :: severity
        character(len=*), intent(in) :: message_prefix
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        character(len=:), allocatable :: declared(:)
        integer, allocatable :: decl_lines(:)
        integer, allocatable :: decl_columns(:)
        logical, allocatable :: decl_is_parameter(:)
        type(diagnostic_t), allocatable :: all_violations(:)
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: i, violation_count
        character(len=:), allocatable :: identifier_name
        integer :: identifier_line, identifier_column
        type(source_range_t) :: location

        violation_count = 0
        allocate (all_violations(100))

        call collect_declared_names(ctx, node_index, declared, decl_lines, &
                                    decl_columns, decl_is_parameter)

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            if (.not. is_in_subtree(ctx%arena, node_index, i)) cycle
            if (.not. is_in_same_unit(ctx%arena, node_index, i)) cycle
            if (ctx%arena%entries(i)%node_type /= "identifier") cycle

            select type (node => ctx%arena%entries(i)%node)
            type is (identifier_node)
                if (.not. allocated(node%name)) cycle
                identifier_name = node%name
                identifier_line = node%line
                identifier_column = node%column
            class default
                cycle
            end select

            if (len_trim(identifier_name) == 0) cycle
            if (name_in_list(declared, identifier_name)) cycle

            select case (trim(identifier_name))
            case ("sin", "cos", "tan", "sqrt", "exp", "log", "abs", &
                  "asin", "acos", "atan", "atan2", "sinh", "cosh", "tanh", &
                  "int", "real", "nint", "floor", "ceiling", &
                  "min", "max", "mod", "modulo", "sign", &
                  "len", "len_trim", "trim", "adjustl", "adjustr", &
                  "size", "shape", "sum", "product", "maxval", "minval", &
                  "allocated", "present", "associated", &
                  "print", "write", "read", "open", "close")
                cycle
            end select

            violation_count = violation_count + 1
            if (violation_count > size(all_violations)) then
                allocate (temp_violations(size(all_violations)*2))
                temp_violations = all_violations
                call move_alloc(temp_violations, all_violations)
            end if

            location%start%line = identifier_line
            location%start%column = identifier_column
            location%end%line = identifier_line
            location%end%column = identifier_column + max(0, &
                                                          len_trim(identifier_name) - 1)

            all_violations(violation_count) = create_diagnostic( &
                                              code=code, &
                                              message=trim(message_prefix)// &
                                              trim(identifier_name), &
                                              file_path=current_filename, &
                                              location=location, &
                                              severity=severity)
        end do

        if (violation_count == 0) then
            deallocate (all_violations)
            allocate (violations(0))
        else
            allocate (violations(violation_count))
            violations = all_violations(1:violation_count)
        end if
    end subroutine check_undefined_variable_usage

end module fluff_rule_identifier_checks
