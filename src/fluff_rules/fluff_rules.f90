module fluff_rules
    ! Built-in rule implementations
    use fluff_core
    use fluff_ast
    use fluff_diagnostics
    use fluff_rule_types
    implicit none
    private
    
    ! Rule categories
    character(len=*), parameter :: CATEGORY_STYLE = "style"
    character(len=*), parameter :: CATEGORY_PERFORMANCE = "performance"
    character(len=*), parameter :: CATEGORY_CORRECTNESS = "correctness"
    
    ! Public procedures for rule registration
    public :: get_all_builtin_rules
    public :: get_style_rules
    public :: get_performance_rules
    public :: get_correctness_rules
    
contains
    
    ! Get all built-in rules
    function get_all_builtin_rules() result(rules)
        type(rule_info_t), allocatable :: rules(:)
        type(rule_info_t), allocatable :: style_rules(:)
        type(rule_info_t), allocatable :: perf_rules(:)
        type(rule_info_t), allocatable :: correct_rules(:)
        
        style_rules = get_style_rules()
        perf_rules = get_performance_rules()
        correct_rules = get_correctness_rules()
        
        allocate(rules(size(style_rules) + size(perf_rules) + size(correct_rules)))
        rules(1:size(style_rules)) = style_rules
        rules(size(style_rules)+1:size(style_rules)+size(perf_rules)) = perf_rules
        rules(size(style_rules)+size(perf_rules)+1:) = correct_rules
        
    end function get_all_builtin_rules
    
    ! Get style rules (F001-F050)
    function get_style_rules() result(rules)
        type(rule_info_t), allocatable :: rules(:)
        type(rule_info_t) :: rule
        integer :: rule_count
        
        ! F001: Missing implicit none
        rule%code = "F001"
        rule%name = "missing-implicit-none"
        rule%description = "Missing 'implicit none' statement"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "best-practices"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_WARNING
        rule%check => check_f001_implicit_none
        ! Registration handled by caller
        
        ! F002: Inconsistent indentation
        rule%code = "F002"
        rule%name = "inconsistent-indentation"
        rule%description = "Inconsistent indentation detected"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "formatting"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_WARNING
        rule%check => check_f002_indentation
        ! Registration handled by caller
        
        ! F003: Line too long
        rule%code = "F003"
        rule%name = "line-too-long"
        rule%description = "Line exceeds maximum length"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "formatting"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_INFO
        rule%check => check_f003_line_length
        ! Registration handled by caller
        
        ! TODO: Add more style rules
        
        ! Allocate result
        rule_count = 3  ! Number of style rules defined above
        allocate(rules(rule_count))
        
        ! F001
        rules(1)%code = "F001"
        rules(1)%name = "missing-implicit-none"
        rules(1)%description = "Missing 'implicit none' statement"
        rules(1)%category = CATEGORY_STYLE
        rules(1)%subcategory = "best-practices"
        rules(1)%default_enabled = .true.
        rules(1)%fixable = .true.
        rules(1)%severity = SEVERITY_WARNING
        rules(1)%check => check_f001_implicit_none
        
        ! F002
        rules(2)%code = "F002"
        rules(2)%name = "inconsistent-indentation"
        rules(2)%description = "Inconsistent indentation detected"
        rules(2)%category = CATEGORY_STYLE
        rules(2)%subcategory = "formatting"
        rules(2)%default_enabled = .true.
        rules(2)%fixable = .true.
        rules(2)%severity = SEVERITY_WARNING
        rules(2)%check => check_f002_indentation
        
        ! F003
        rules(3)%code = "F003"
        rules(3)%name = "line-too-long"
        rules(3)%description = "Line exceeds maximum length"
        rules(3)%category = CATEGORY_STYLE
        rules(3)%subcategory = "formatting"
        rules(3)%default_enabled = .true.
        rules(3)%fixable = .false.
        rules(3)%severity = SEVERITY_INFO
        rules(3)%check => check_f003_line_length
        
    end function get_style_rules
    
    ! Get performance rules (P001-P025)
    function get_performance_rules() result(rules)
        type(rule_info_t), allocatable :: rules(:)
        type(rule_info_t) :: rule
        integer :: rule_count
        
        ! P001: Non-contiguous array access
        rule%code = "P001"
        rule%name = "non-contiguous-array-access"
        rule%description = "Non-contiguous array access pattern detected"
        rule%category = CATEGORY_PERFORMANCE
        rule%subcategory = "memory"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_WARNING
        rule%check => check_p001_array_access
        ! Registration handled by caller
        
        ! TODO: Add more performance rules
        
        ! Allocate result
        rule_count = 1  ! Number of performance rules defined above
        allocate(rules(rule_count))
        
        ! P001
        rules(1)%code = "P001"
        rules(1)%name = "non-contiguous-array-access"
        rules(1)%description = "Non-contiguous array access pattern detected"
        rules(1)%category = CATEGORY_PERFORMANCE
        rules(1)%subcategory = "memory"
        rules(1)%default_enabled = .true.
        rules(1)%fixable = .false.
        rules(1)%severity = SEVERITY_WARNING
        rules(1)%check => check_p001_array_access
        
    end function get_performance_rules
    
    ! Get correctness rules
    function get_correctness_rules() result(rules)
        type(rule_info_t), allocatable :: rules(:)
        type(rule_info_t) :: rule
        integer :: rule_count
        
        ! C001: Undefined variable
        rule%code = "C001"
        rule%name = "undefined-variable"
        rule%description = "Use of undefined variable"
        rule%category = CATEGORY_CORRECTNESS
        rule%subcategory = "semantic"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_ERROR
        rule%check => check_c001_undefined_var
        ! Registration handled by caller
        
        ! TODO: Add more correctness rules
        
        ! Allocate result
        rule_count = 1  ! Number of correctness rules defined above
        allocate(rules(rule_count))
        
        ! C001
        rules(1)%code = "C001"
        rules(1)%name = "undefined-variable"
        rules(1)%description = "Use of undefined variable"
        rules(1)%category = CATEGORY_CORRECTNESS
        rules(1)%subcategory = "semantic"
        rules(1)%default_enabled = .true.
        rules(1)%fixable = .false.
        rules(1)%severity = SEVERITY_ERROR
        rules(1)%check => check_c001_undefined_var
        
    end function get_correctness_rules
    
    ! Rule implementations
    
    ! F001: Check for missing implicit none
    subroutine check_f001_implicit_none(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f001_implicit_none
    
    ! F002: Check indentation consistency
    subroutine check_f002_indentation(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f002_indentation
    
    ! F003: Check line length
    subroutine check_f003_line_length(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
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