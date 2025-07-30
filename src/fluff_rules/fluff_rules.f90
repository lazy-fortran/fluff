module fluff_rules
    ! Built-in rule implementations
    use fluff_core
    use fluff_ast
    use fluff_linter
    use fluff_diagnostics
    implicit none
    private
    
    ! Rule categories
    character(len=*), parameter :: CATEGORY_STYLE = "style"
    character(len=*), parameter :: CATEGORY_PERFORMANCE = "performance"
    character(len=*), parameter :: CATEGORY_CORRECTNESS = "correctness"
    
    ! Public procedures for rule registration
    public :: register_all_rules
    public :: register_style_rules
    public :: register_performance_rules
    public :: register_correctness_rules
    
contains
    
    ! Register all built-in rules
    subroutine register_all_rules(registry)
        type(rule_registry_t), intent(inout) :: registry
        
        call register_style_rules(registry)
        call register_performance_rules(registry)
        call register_correctness_rules(registry)
        
    end subroutine register_all_rules
    
    ! Register style rules (F001-F050)
    subroutine register_style_rules(registry)
        type(rule_registry_t), intent(inout) :: registry
        type(rule_info_t) :: rule
        
        ! F001: Missing implicit none
        rule%code = "F001"
        rule%name = "missing-implicit-none"
        rule%description = "Missing 'implicit none' statement"
        rule%category = CATEGORY_STYLE
        ! rule%check => check_f001_implicit_none
        ! TODO: Set check procedure when type is fixed
        ! call registry%register_rule(rule)
        
        ! F002: Inconsistent indentation
        rule%code = "F002"
        rule%name = "inconsistent-indentation"
        rule%description = "Inconsistent indentation detected"
        rule%category = CATEGORY_STYLE
        ! rule%check => check_f002_indentation
        ! call registry%register_rule(rule)
        
        ! F003: Line too long
        rule%code = "F003"
        rule%name = "line-too-long"
        rule%description = "Line exceeds maximum length"
        rule%category = CATEGORY_STYLE
        ! rule%check => check_f003_line_length
        ! call registry%register_rule(rule)
        
        ! TODO: Add more style rules
        
    end subroutine register_style_rules
    
    ! Register performance rules (P001-P025)
    subroutine register_performance_rules(registry)
        type(rule_registry_t), intent(inout) :: registry
        type(rule_info_t) :: rule
        
        ! P001: Non-contiguous array access
        rule%code = "P001"
        rule%name = "non-contiguous-array-access"
        rule%description = "Non-contiguous array access pattern detected"
        rule%category = CATEGORY_PERFORMANCE
        ! rule%check => check_p001_array_access
        ! call registry%register_rule(rule)
        
        ! TODO: Add more performance rules
        
    end subroutine register_performance_rules
    
    ! Register correctness rules
    subroutine register_correctness_rules(registry)
        type(rule_registry_t), intent(inout) :: registry
        type(rule_info_t) :: rule
        
        ! C001: Undefined variable
        rule%code = "C001"
        rule%name = "undefined-variable"
        rule%description = "Use of undefined variable"
        rule%category = CATEGORY_CORRECTNESS
        ! rule%check => check_c001_undefined_var
        ! call registry%register_rule(rule)
        
        ! TODO: Add more correctness rules
        
    end subroutine register_correctness_rules
    
    ! Rule implementations
    
    ! F001: Check for missing implicit none
    subroutine check_f001_implicit_none(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement implicit none check
        allocate(violations(0))
        
    end subroutine check_f001_implicit_none
    
    ! F002: Check indentation consistency
    subroutine check_f002_indentation(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement indentation check
        allocate(violations(0))
        
    end subroutine check_f002_indentation
    
    ! F003: Check line length
    subroutine check_f003_line_length(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement line length check
        allocate(violations(0))
        
    end subroutine check_f003_line_length
    
    ! P001: Check array access patterns
    subroutine check_p001_array_access(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement array access pattern check
        allocate(violations(0))
        
    end subroutine check_p001_array_access
    
    ! C001: Check for undefined variables
    subroutine check_c001_undefined_var(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement undefined variable check
        allocate(violations(0))
        
    end subroutine check_c001_undefined_var
    
end module fluff_rules