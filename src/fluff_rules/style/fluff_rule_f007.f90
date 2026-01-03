module fluff_rule_f007
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, SEVERITY_ERROR
    use fluff_rule_identifier_checks, only: check_undefined_variable_usage
    implicit none
    private

    public :: check_f007_undefined_variable

contains

    subroutine check_f007_undefined_variable(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        call check_undefined_variable_usage(ctx, node_index, "F007", SEVERITY_ERROR, &
                                            "Undefined variable: ", violations)
    end subroutine check_f007_undefined_variable

end module fluff_rule_f007
