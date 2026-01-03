module fluff_rule_f006
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename
    use fluff_rule_symbol_collect, only: collect_declared_names, collect_used_names, &
                                         name_in_list
    implicit none
    private

    public :: check_f006_unused_variable

contains

    subroutine check_f006_unused_variable(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        character(len=:), allocatable :: declared(:)
        integer, allocatable :: decl_lines(:)
        integer, allocatable :: decl_columns(:)
        logical, allocatable :: decl_is_parameter(:)
        character(len=:), allocatable :: used(:)
        integer :: i, violation_count
        type(source_range_t) :: location
        logical :: is_intrinsic
        type(diagnostic_t), allocatable :: tmp(:)

        violation_count = 0
        allocate (tmp(100))

        call collect_declared_names(ctx, node_index, declared, decl_lines, &
                                    decl_columns, decl_is_parameter)
        call collect_used_names(ctx, node_index, used)

        do i = 1, size(declared)
            if (violation_count >= size(tmp)) exit
            if (len_trim(declared(i)) == 0) cycle
            if (decl_is_parameter(i)) cycle
            if (name_in_list(used, declared(i))) cycle

            is_intrinsic = .false.
            select case (trim(declared(i)))
            case ("sin", "cos", "tan", "sqrt", "exp", "log", "abs", &
                  "asin", "acos", "atan", "atan2", "sinh", "cosh", "tanh", &
                  "int", "real", "nint", "floor", "ceiling", &
                  "min", "max", "mod", "modulo", "sign", &
                  "len", "len_trim", "trim", "adjustl", "adjustr", &
                  "size", "shape", "sum", "product", "maxval", "minval", &
                  "allocated", "present", "associated")
                is_intrinsic = .true.
            end select
            if (is_intrinsic) cycle

            violation_count = violation_count + 1

            location%start%line = decl_lines(i)
            location%start%column = decl_columns(i)
            location%end%line = decl_lines(i)
            location%end%column = decl_columns(i) + len_trim(declared(i)) - 1

            tmp(violation_count) = create_diagnostic( &
                                   code="F006", &
                                   message="Unused variable: "//trim(declared(i)), &
                                   file_path=current_filename, &
                                   location=location, &
                                   severity=SEVERITY_WARNING)
        end do

        if (violation_count == 0) then
            allocate (violations(0))
            return
        end if

        allocate (violations(violation_count))
        violations = tmp(1:violation_count)
    end subroutine check_f006_unused_variable

end module fluff_rule_f006
