module fluff_rule_c001
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, SEVERITY_ERROR
    use fluff_rule_identifier_checks, only: check_undefined_variable_usage
    implicit none
    private

    public :: check_c001_undefined_var

contains

    subroutine check_c001_undefined_var(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        call check_undefined_variable_usage(ctx, node_index, "C001", SEVERITY_ERROR, &
                                            "Use of undefined variable: ", violations)
    end subroutine check_c001_undefined_var

end module fluff_rule_c001
