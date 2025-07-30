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
        
        ! F004: Trailing whitespace
        rule%code = "F004"
        rule%name = "trailing-whitespace"
        rule%description = "Trailing whitespace detected"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "formatting"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_WARNING
        rule%check => check_f004_trailing_whitespace
        ! Registration handled by caller
        
        ! F005: Mixed tabs and spaces
        rule%code = "F005"
        rule%name = "mixed-tabs-spaces"
        rule%description = "Mixed tabs and spaces in indentation"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "formatting"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_WARNING
        rule%check => check_f005_mixed_tabs_spaces
        ! Registration handled by caller
        
        ! F006: Unused variable declaration
        rule%code = "F006"
        rule%name = "unused-variable"
        rule%description = "Unused variable declaration"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "best-practices"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_WARNING
        rule%check => check_f006_unused_variable
        ! Registration handled by caller
        
        ! F007: Undefined variable usage
        rule%code = "F007"
        rule%name = "undefined-variable"
        rule%description = "Undefined variable usage"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "best-practices"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_ERROR
        rule%check => check_f007_undefined_variable
        ! Registration handled by caller
        
        ! F008: Missing intent declarations
        rule%code = "F008"
        rule%name = "missing-intent"
        rule%description = "Missing intent declarations"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "best-practices"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_WARNING
        rule%check => check_f008_missing_intent
        ! Registration handled by caller
        
        ! TODO: Add more style rules
        
        ! Allocate result
        rule_count = 8  ! Number of style rules defined above
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
        
        ! F004
        rules(4)%code = "F004"
        rules(4)%name = "trailing-whitespace"
        rules(4)%description = "Trailing whitespace detected"
        rules(4)%category = CATEGORY_STYLE
        rules(4)%subcategory = "formatting"
        rules(4)%default_enabled = .true.
        rules(4)%fixable = .true.
        rules(4)%severity = SEVERITY_WARNING
        rules(4)%check => check_f004_trailing_whitespace
        
        ! F005
        rules(5)%code = "F005"
        rules(5)%name = "mixed-tabs-spaces"
        rules(5)%description = "Mixed tabs and spaces in indentation"
        rules(5)%category = CATEGORY_STYLE
        rules(5)%subcategory = "formatting"
        rules(5)%default_enabled = .true.
        rules(5)%fixable = .true.
        rules(5)%severity = SEVERITY_WARNING
        rules(5)%check => check_f005_mixed_tabs_spaces
        
        ! F006
        rules(6)%code = "F006"
        rules(6)%name = "unused-variable"
        rules(6)%description = "Unused variable declaration"
        rules(6)%category = CATEGORY_STYLE
        rules(6)%subcategory = "best-practices"
        rules(6)%default_enabled = .true.
        rules(6)%fixable = .false.
        rules(6)%severity = SEVERITY_WARNING
        rules(6)%check => check_f006_unused_variable
        
        ! F007
        rules(7)%code = "F007"
        rules(7)%name = "undefined-variable"
        rules(7)%description = "Undefined variable usage"
        rules(7)%category = CATEGORY_STYLE
        rules(7)%subcategory = "best-practices"
        rules(7)%default_enabled = .true.
        rules(7)%fixable = .false.
        rules(7)%severity = SEVERITY_ERROR
        rules(7)%check => check_f007_undefined_variable
        
        ! F008
        rules(8)%code = "F008"
        rules(8)%name = "missing-intent"
        rules(8)%description = "Missing intent declarations"
        rules(8)%category = CATEGORY_STYLE
        rules(8)%subcategory = "best-practices"
        rules(8)%default_enabled = .true.
        rules(8)%fixable = .true.
        rules(8)%severity = SEVERITY_WARNING
        rules(8)%check => check_f008_missing_intent
        
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
    
    ! F004: Check trailing whitespace
    subroutine check_f004_trailing_whitespace(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f004_trailing_whitespace
    
    ! F005: Check mixed tabs and spaces
    subroutine check_f005_mixed_tabs_spaces(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f005_mixed_tabs_spaces
    
    ! F006: Check unused variable declarations
    subroutine check_f006_unused_variable(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f006_unused_variable
    
    ! F007: Check undefined variable usage
    subroutine check_f007_undefined_variable(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f007_undefined_variable
    
    ! F008: Check missing intent declarations
    subroutine check_f008_missing_intent(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f008_missing_intent
    
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