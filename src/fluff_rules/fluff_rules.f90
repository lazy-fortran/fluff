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
        
        ! F009: Inconsistent intent usage
        rule%code = "F009"
        rule%name = "inconsistent-intent"
        rule%description = "Inconsistent intent usage"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "best-practices"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_ERROR
        rule%check => check_f009_inconsistent_intent
        ! Registration handled by caller
        
        ! F010: Obsolete language features
        rule%code = "F010"
        rule%name = "obsolete-features"
        rule%description = "Obsolete language features (GOTO, computed GOTO)"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "modernization"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_WARNING
        rule%check => check_f010_obsolete_features
        ! Registration handled by caller
        
        ! F011: Missing end statement labels
        rule%code = "F011"
        rule%name = "missing-end-labels"
        rule%description = "Missing end statement labels"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "clarity"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_INFO
        rule%check => check_f011_missing_end_labels
        ! Registration handled by caller
        
        ! F012: Inconsistent naming conventions
        rule%code = "F012"
        rule%name = "inconsistent-naming"
        rule%description = "Inconsistent naming conventions"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "consistency"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_WARNING
        rule%check => check_f012_naming_conventions
        ! Registration handled by caller
        
        ! F013: Multiple statements per line
        rule%code = "F013"
        rule%name = "multiple-statements"
        rule%description = "Multiple statements per line"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "formatting"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_WARNING
        rule%check => check_f013_multiple_statements
        ! Registration handled by caller
        
        ! F014: Unnecessary parentheses
        rule%code = "F014"
        rule%name = "unnecessary-parentheses"
        rule%description = "Unnecessary parentheses"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "simplification"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_INFO
        rule%check => check_f014_unnecessary_parentheses
        ! Registration handled by caller
        
        ! F015: Redundant continue statements
        rule%code = "F015"
        rule%name = "redundant-continue"
        rule%description = "Redundant continue statements"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "simplification"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_INFO
        rule%check => check_f015_redundant_continue
        ! Registration handled by caller
        
        ! TODO: Add more style rules
        
        ! Allocate result
        rule_count = 15  ! Number of style rules defined above
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
        
        ! F009
        rules(9)%code = "F009"
        rules(9)%name = "inconsistent-intent"
        rules(9)%description = "Inconsistent intent usage"
        rules(9)%category = CATEGORY_STYLE
        rules(9)%subcategory = "best-practices"
        rules(9)%default_enabled = .true.
        rules(9)%fixable = .false.
        rules(9)%severity = SEVERITY_ERROR
        rules(9)%check => check_f009_inconsistent_intent
        
        ! F010
        rules(10)%code = "F010"
        rules(10)%name = "obsolete-features"
        rules(10)%description = "Obsolete language features (GOTO, computed GOTO)"
        rules(10)%category = CATEGORY_STYLE
        rules(10)%subcategory = "modernization"
        rules(10)%default_enabled = .true.
        rules(10)%fixable = .false.
        rules(10)%severity = SEVERITY_WARNING
        rules(10)%check => check_f010_obsolete_features
        
        ! F011
        rules(11)%code = "F011"
        rules(11)%name = "missing-end-labels"
        rules(11)%description = "Missing end statement labels"
        rules(11)%category = CATEGORY_STYLE
        rules(11)%subcategory = "clarity"
        rules(11)%default_enabled = .true.
        rules(11)%fixable = .true.
        rules(11)%severity = SEVERITY_INFO
        rules(11)%check => check_f011_missing_end_labels
        
        ! F012
        rules(12)%code = "F012"
        rules(12)%name = "inconsistent-naming"
        rules(12)%description = "Inconsistent naming conventions"
        rules(12)%category = CATEGORY_STYLE
        rules(12)%subcategory = "consistency"
        rules(12)%default_enabled = .true.
        rules(12)%fixable = .false.
        rules(12)%severity = SEVERITY_WARNING
        rules(12)%check => check_f012_naming_conventions
        
        ! F013
        rules(13)%code = "F013"
        rules(13)%name = "multiple-statements"
        rules(13)%description = "Multiple statements per line"
        rules(13)%category = CATEGORY_STYLE
        rules(13)%subcategory = "formatting"
        rules(13)%default_enabled = .true.
        rules(13)%fixable = .true.
        rules(13)%severity = SEVERITY_WARNING
        rules(13)%check => check_f013_multiple_statements
        
        ! F014
        rules(14)%code = "F014"
        rules(14)%name = "unnecessary-parentheses"
        rules(14)%description = "Unnecessary parentheses"
        rules(14)%category = CATEGORY_STYLE
        rules(14)%subcategory = "simplification"
        rules(14)%default_enabled = .true.
        rules(14)%fixable = .true.
        rules(14)%severity = SEVERITY_INFO
        rules(14)%check => check_f014_unnecessary_parentheses
        
        ! F015
        rules(15)%code = "F015"
        rules(15)%name = "redundant-continue"
        rules(15)%description = "Redundant continue statements"
        rules(15)%category = CATEGORY_STYLE
        rules(15)%subcategory = "simplification"
        rules(15)%default_enabled = .true.
        rules(15)%fixable = .true.
        rules(15)%severity = SEVERITY_INFO
        rules(15)%check => check_f015_redundant_continue
        
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
        
        ! P002: Inefficient loop ordering
        rule%code = "P002"
        rule%name = "inefficient-loop-ordering"
        rule%description = "Inefficient loop ordering"
        rule%category = CATEGORY_PERFORMANCE
        rule%subcategory = "memory"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_WARNING
        rule%check => check_p002_loop_ordering
        ! Registration handled by caller
        
        ! P003: Unnecessary array temporaries
        rule%code = "P003"
        rule%name = "unnecessary-array-temporaries"
        rule%description = "Unnecessary array temporaries"
        rule%category = CATEGORY_PERFORMANCE
        rule%subcategory = "memory"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_WARNING
        rule%check => check_p003_array_temporaries
        ! Registration handled by caller
        
        ! P004: Missing pure/elemental declarations
        rule%code = "P004"
        rule%name = "missing-pure-elemental"
        rule%description = "Missing `pure`/`elemental` declarations"
        rule%category = CATEGORY_PERFORMANCE
        rule%subcategory = "optimization"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_INFO
        rule%check => check_p004_pure_elemental
        ! Registration handled by caller
        
        ! P005: Inefficient string operations
        rule%code = "P005"
        rule%name = "inefficient-string-operations"
        rule%description = "Inefficient string operations"
        rule%category = CATEGORY_PERFORMANCE
        rule%subcategory = "memory"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_WARNING
        rule%check => check_p005_string_operations
        ! Registration handled by caller
        
        ! P006: Unnecessary allocations in loops
        rule%code = "P006"
        rule%name = "unnecessary-allocations-in-loops"
        rule%description = "Unnecessary allocations in loops"
        rule%category = CATEGORY_PERFORMANCE
        rule%subcategory = "memory"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_WARNING
        rule%check => check_p006_loop_allocations
        ! Registration handled by caller
        
        ! P007: Mixed precision arithmetic
        rule%code = "P007"
        rule%name = "mixed-precision-arithmetic"
        rule%description = "Mixed precision arithmetic"
        rule%category = CATEGORY_PERFORMANCE
        rule%subcategory = "precision"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_INFO
        rule%check => check_p007_mixed_precision
        ! Registration handled by caller
        
        ! TODO: Add more performance rules
        
        ! Allocate result
        rule_count = 7  ! Number of performance rules defined above
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
        
        ! P002
        rules(2)%code = "P002"
        rules(2)%name = "inefficient-loop-ordering"
        rules(2)%description = "Inefficient loop ordering"
        rules(2)%category = CATEGORY_PERFORMANCE
        rules(2)%subcategory = "memory"
        rules(2)%default_enabled = .true.
        rules(2)%fixable = .false.
        rules(2)%severity = SEVERITY_WARNING
        rules(2)%check => check_p002_loop_ordering
        
        ! P003
        rules(3)%code = "P003"
        rules(3)%name = "unnecessary-array-temporaries"
        rules(3)%description = "Unnecessary array temporaries"
        rules(3)%category = CATEGORY_PERFORMANCE
        rules(3)%subcategory = "memory"
        rules(3)%default_enabled = .true.
        rules(3)%fixable = .false.
        rules(3)%severity = SEVERITY_WARNING
        rules(3)%check => check_p003_array_temporaries
        
        ! P004
        rules(4)%code = "P004"
        rules(4)%name = "missing-pure-elemental"
        rules(4)%description = "Missing `pure`/`elemental` declarations"
        rules(4)%category = CATEGORY_PERFORMANCE
        rules(4)%subcategory = "optimization"
        rules(4)%default_enabled = .true.
        rules(4)%fixable = .true.
        rules(4)%severity = SEVERITY_INFO
        rules(4)%check => check_p004_pure_elemental
        
        ! P005
        rules(5)%code = "P005"
        rules(5)%name = "inefficient-string-operations"
        rules(5)%description = "Inefficient string operations"
        rules(5)%category = CATEGORY_PERFORMANCE
        rules(5)%subcategory = "memory"
        rules(5)%default_enabled = .true.
        rules(5)%fixable = .false.
        rules(5)%severity = SEVERITY_WARNING
        rules(5)%check => check_p005_string_operations
        
        ! P006
        rules(6)%code = "P006"
        rules(6)%name = "unnecessary-allocations-in-loops"
        rules(6)%description = "Unnecessary allocations in loops"
        rules(6)%category = CATEGORY_PERFORMANCE
        rules(6)%subcategory = "memory"
        rules(6)%default_enabled = .true.
        rules(6)%fixable = .false.
        rules(6)%severity = SEVERITY_WARNING
        rules(6)%check => check_p006_loop_allocations
        
        ! P007
        rules(7)%code = "P007"
        rules(7)%name = "mixed-precision-arithmetic"
        rules(7)%description = "Mixed precision arithmetic"
        rules(7)%category = CATEGORY_PERFORMANCE
        rules(7)%subcategory = "precision"
        rules(7)%default_enabled = .true.
        rules(7)%fixable = .false.
        rules(7)%severity = SEVERITY_INFO
        rules(7)%check => check_p007_mixed_precision
        
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
    
    ! F009: Check inconsistent intent usage
    subroutine check_f009_inconsistent_intent(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f009_inconsistent_intent
    
    ! F010: Check obsolete language features
    subroutine check_f010_obsolete_features(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f010_obsolete_features
    
    ! F011: Check missing end statement labels
    subroutine check_f011_missing_end_labels(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f011_missing_end_labels
    
    ! F012: Check naming conventions
    subroutine check_f012_naming_conventions(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f012_naming_conventions
    
    ! F013: Check multiple statements per line
    subroutine check_f013_multiple_statements(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f013_multiple_statements
    
    ! F014: Check unnecessary parentheses
    subroutine check_f014_unnecessary_parentheses(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f014_unnecessary_parentheses
    
    ! F015: Check redundant continue statements
    subroutine check_f015_redundant_continue(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement when fortfront AST API is available
        ! For now, just return empty violations
        allocate(violations(0))
        
    end subroutine check_f015_redundant_continue
    
    ! P001: Check array access patterns
    subroutine check_p001_array_access(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement array access pattern check
        allocate(violations(0))
        
    end subroutine check_p001_array_access
    
    ! P002: Check loop ordering efficiency
    subroutine check_p002_loop_ordering(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement loop ordering efficiency check
        allocate(violations(0))
        
    end subroutine check_p002_loop_ordering
    
    ! P003: Check array temporaries
    subroutine check_p003_array_temporaries(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement array temporaries check
        allocate(violations(0))
        
    end subroutine check_p003_array_temporaries
    
    ! P004: Check pure/elemental declarations
    subroutine check_p004_pure_elemental(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement pure/elemental declaration check
        allocate(violations(0))
        
    end subroutine check_p004_pure_elemental
    
    ! P005: Check string operations efficiency
    subroutine check_p005_string_operations(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement string operations efficiency check
        allocate(violations(0))
        
    end subroutine check_p005_string_operations
    
    ! P006: Check loop allocations
    subroutine check_p006_loop_allocations(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement loop allocations check
        allocate(violations(0))
        
    end subroutine check_p006_loop_allocations
    
    ! P007: Check mixed precision arithmetic
    subroutine check_p007_mixed_precision(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement mixed precision arithmetic check
        allocate(violations(0))
        
    end subroutine check_p007_mixed_precision
    
    ! C001: Check for undefined variables
    subroutine check_c001_undefined_var(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! TODO: Implement undefined variable check
        allocate(violations(0))
        
    end subroutine check_c001_undefined_var
    
end module fluff_rules