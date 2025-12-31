module fluff_rules
    ! Built-in rule implementations
    use fluff_core
    use fluff_ast, only: fluff_ast_context_t, NODE_DECLARATION, NODE_IDENTIFIER, &
                       NODE_FUNCTION_DEF, NODE_SUBROUTINE_DEF, NODE_IF, &
                       NODE_DO_LOOP, NODE_MODULE, NODE_UNKNOWN, NODE_ASSIGNMENT, &
                       NODE_PROGRAM
    use fluff_diagnostics
    use fluff_rule_types
    use fortfront, only: symbol_info_t, variable_usage_info_t, get_variables_in_expression, &
                        SCOPE_FUNCTION, SCOPE_SUBROUTINE, identifier_node, &
                        semantic_context_t
    use fortfront_compat, only: get_identifier_name, get_symbols_in_scope, &
                               is_identifier_defined_direct, get_unused_variables_direct
    implicit none
    private
    
    ! Module variables to track current file being processed (workaround)
    character(len=:), allocatable :: current_filename
    character(len=:), allocatable :: current_source_text
    
    ! Rule categories
    character(len=*), parameter :: CATEGORY_STYLE = "style"
    character(len=*), parameter :: CATEGORY_PERFORMANCE = "performance"
    character(len=*), parameter :: CATEGORY_CORRECTNESS = "correctness"
    
    ! Public procedures for rule registration
    public :: get_all_builtin_rules
    public :: get_style_rules  
    public :: get_performance_rules
    public :: get_correctness_rules
    public :: set_current_file_context
    
    ! Public constants
    public :: CATEGORY_STYLE, CATEGORY_PERFORMANCE, CATEGORY_CORRECTNESS
    
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
        rule%description = "Line exceeds maximum length with suggested breaking points"
        rule%category = CATEGORY_STYLE
        rule%subcategory = "formatting"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_WARNING
        rule%check => check_f003_line_length
        ! Registration handled by caller
        
        ! Future: Add more rules as fortfront API becomes available
        
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
        rule%default_enabled = .false.
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
        
        ! Future: Add more rules as fortfront API becomes available
        
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
        rules(3)%description = "Line exceeds maximum length with suggested breaking points"
        rules(3)%category = CATEGORY_STYLE
        rules(3)%subcategory = "formatting"
        rules(3)%default_enabled = .true.
        rules(3)%fixable = .true.
        rules(3)%severity = SEVERITY_WARNING
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
        
        ! Future: Add more rules as fortfront API becomes available
        
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
        
        ! Future: Add more rules as fortfront API becomes available
        
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
        use fortfront, only: ast_arena_t, semantic_context_t, &
                            lex_source, parse_tokens, analyze_semantics, &
                            create_ast_arena, create_semantic_context, &
                            module_node, program_node, subroutine_def_node, function_def_node, &
                            get_node_type_id_from_arena
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! Use fortfront AST to check for implicit none statements
        ! Always call the AST-based implementation for all nodes
        call check_f001_implicit_none_ast_based(ctx, node_index, violations)
        
    end subroutine check_f001_implicit_none
    
    ! F002: Check indentation consistency
    subroutine check_f002_indentation(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        character(len=:), allocatable :: source_text
        character(len=1000) :: line
        integer :: io_status
        integer :: line_num
        integer :: indent_levels(1000)
        integer :: line_count
        integer :: i, j
        logical :: has_inconsistency
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        type(source_range_t) :: location
        
        ! Initialize
        allocate(temp_violations(100))
        violation_count = 0
        line_count = 0
        indent_levels = 0
        
        ! Use current_source_text to check indentation
        if (.not. allocated(current_source_text)) then
            ! No source text available
            allocate(violations(0))
            return
        end if
        
        call analyze_indentation_from_text(current_source_text, temp_violations, violation_count)
        
        ! Allocate and copy violations
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_f002_indentation
    
    ! Recursive helper to check indentation
    recursive subroutine check_indentation_recursive(ctx, node_index, expected_indent, &
                                                    violations, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        integer, intent(in) :: expected_indent
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer, allocatable :: children(:)
        integer :: node_type
        integer :: i, num_children
        integer :: next_indent
        integer :: actual_indent
        type(source_range_t) :: location
        
        ! Get node type
        node_type = ctx%get_node_type(node_index)
        
        ! Get node location to check indentation
        location = ctx%get_node_location(node_index)
        
        ! Check if indentation is correct (simplified)
        ! In a real implementation, we'd check the actual column position
        ! For now, we just use the location info
        if (location%start%column > 1) then
            ! Check if indentation matches expected level
            actual_indent = location%start%column - 1  ! 0-based to 1-based
            
            ! Only check for certain node types that should be indented
            if (should_check_indent(node_type)) then
                if (mod(actual_indent, 4) /= 0) then  ! Check for 4-space indentation
                    violation_count = violation_count + 1
                    if (violation_count <= size(violations)) then
                        violations(violation_count) = create_diagnostic( &
                            code="F002", &
                            message="Inconsistent indentation (expected multiple of 4 spaces)", &
                            file_path="", &
                            location=location, &
                            severity=SEVERITY_WARNING)
                        
                        ! Generate fix suggestion for indentation
                        call add_indentation_fix(violations(violation_count), location, actual_indent)
                    end if
                end if
            end if
        end if
        
        ! Calculate expected indentation for children
        next_indent = expected_indent
        if (increases_indent(node_type)) then
            next_indent = expected_indent + 4
        end if
        
        ! Recursively process children
        children = ctx%get_children(node_index)
        num_children = size(children)
        
        do i = 1, num_children
            if (children(i) > 0) then
                call check_indentation_recursive(ctx, children(i), next_indent, &
                                                violations, violation_count)
            end if
        end do
        
        if (allocated(children)) deallocate(children)
        
    end subroutine check_indentation_recursive
    
    ! Check if node type should have indentation checked
    function should_check_indent(node_type) result(should_check)
        integer, intent(in) :: node_type
        logical :: should_check
        
        ! Check indentation for statements and declarations
        should_check = node_type /= NODE_UNKNOWN
        
    end function should_check_indent
    
    ! Check if node type increases indentation level
    function increases_indent(node_type) result(increases)
        integer, intent(in) :: node_type
        logical :: increases
        
        ! These node types increase indentation for their children
        increases = node_type == NODE_FUNCTION_DEF .or. &
                   node_type == NODE_SUBROUTINE_DEF .or. &
                   node_type == NODE_IF .or. &
                   node_type == NODE_DO_LOOP .or. &
                   node_type == NODE_MODULE
        
    end function increases_indent
    
    ! Helper functions for F002 indentation checking
    function count_leading_spaces(line) result(count)
        character(len=*), intent(in) :: line
        integer :: count
        integer :: i
        
        count = 0
        do i = 1, len(line)
            if (line(i:i) == ' ') then
                count = count + 1
            else
                exit
            end if
        end do
    end function count_leading_spaces
    
    ! Analyze indentation from source text
    subroutine analyze_indentation_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        character(len=1000) :: line
        integer :: pos, next_pos, line_num
        integer :: indent_levels(1000)
        integer :: line_count
        integer :: i, j
        logical :: has_inconsistency
        type(source_range_t) :: location
        
        pos = 1
        line_num = 0
        line_count = 0
        
        do while (pos <= len(source_text))
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                line = source_text(pos:)
                pos = len(source_text) + 1
            else
                line = source_text(pos:pos+next_pos-2)
                pos = pos + next_pos
            end if
            
            line_num = line_num + 1
            if (len_trim(line) > 0 .and. line(1:1) /= '!') then
                line_count = line_count + 1
                indent_levels(line_count) = count_leading_spaces(line)
            end if
        end do
        
        ! Check for inconsistent indentation patterns
        has_inconsistency = .false.
        
        if (line_count >= 2) then
            do i = 1, line_count - 1
                do j = i + 1, line_count
                    if (indent_levels(i) > 0 .and. indent_levels(j) > 0) then
                        ! Check if indentation levels are inconsistent
                        ! Look for cases like 2 spaces vs 4 spaces
                        if (indent_levels(i) /= indent_levels(j)) then
                            if (mod(indent_levels(i), 4) /= mod(indent_levels(j), 4)) then
                                has_inconsistency = .true.
                                exit
                            end if
                        end if
                    end if
                end do
                if (has_inconsistency) exit
            end do
        end if
        
        if (has_inconsistency) then
            violation_count = violation_count + 1
            if (violation_count <= size(violations)) then
                ! Create a basic location
                location%start%line = 1
                location%start%column = 1  
                location%end%line = 1
                location%end%column = 1
                violations(violation_count) = create_diagnostic( &
                    code="F002", &
                    message="Inconsistent indentation levels detected", &
                    file_path=current_filename, &
                    location=location, &
                    severity=SEVERITY_WARNING)
            end if
        end if
        
    end subroutine analyze_indentation_from_text
    
    ! Analyze line lengths from source text
    subroutine analyze_line_lengths_from_text(source_text, violations, violation_count, max_length)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        integer, intent(in) :: max_length
        
        character(len=1000) :: line
        integer :: pos, next_pos, line_num
        integer :: line_length
        type(source_range_t) :: location
        
        pos = 1
        line_num = 0
        
        do while (pos <= len(source_text))
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                line = source_text(pos:)
                pos = len(source_text) + 1
            else
                line = source_text(pos:pos+next_pos-2)
                pos = pos + next_pos
            end if
            
            line_num = line_num + 1
            line_length = len_trim(line)
            
            ! Check if line exceeds maximum length and is not a comment
            if (line_length > max_length .and. .not. is_comment_line(line)) then
                violation_count = violation_count + 1
                if (violation_count <= size(violations)) then
                    ! Create location for the long line
                    location%start%line = line_num
                    location%start%column = max_length + 1  
                    location%end%line = line_num
                    location%end%column = line_length
                    violations(violation_count) = create_diagnostic( &
                        code="F003", &
                        message="Line too long (" // trim(adjustl(int_to_str(line_length))) // &
                               " > " // trim(adjustl(int_to_str(max_length))) // " characters)", &
                        file_path=current_filename, &
                        location=location, &
                        severity=SEVERITY_INFO)
                end if
            end if
        end do
        
    end subroutine analyze_line_lengths_from_text
    
    ! Enhanced line length analysis with AST context for smart breaking points
    subroutine analyze_line_lengths_with_ast_context(ctx, source_text, violations, violation_count, max_length)
        type(fluff_ast_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        integer, intent(in) :: max_length
        
        character(len=1000) :: line
        integer :: pos, next_pos, line_num
        integer :: line_length
        type(source_range_t) :: location
        type(fix_suggestion_t), allocatable :: fix_suggestions(:)
        character(len=:), allocatable :: suggested_fix
        type(text_edit_t) :: text_edit
        
        pos = 1
        line_num = 0
        
        do while (pos <= len(source_text))
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                line = source_text(pos:)
                pos = len(source_text) + 1
            else
                line = source_text(pos:pos+next_pos-2)
                pos = pos + next_pos
            end if
            
            line_num = line_num + 1
            line_length = len_trim(line)
            
            ! Check if line exceeds maximum length and is not a comment
            if (line_length > max_length .and. .not. is_comment_line(line)) then
                violation_count = violation_count + 1
                
                ! Create location
                location%start%line = line_num
                location%start%column = max_length + 1  
                location%end%line = line_num
                location%end%column = line_length
                
                ! Generate smart breaking suggestions based on AST context
                call generate_line_break_suggestions(line, suggested_fix)
                
                ! Create text edit for the fix
                text_edit%range = location
                text_edit%new_text = suggested_fix
                
                ! Create fix suggestion
                allocate(fix_suggestions(1))
                fix_suggestions(1) = create_fix_suggestion( &
                    description="Break line at logical points", &
                    edits=[text_edit])
                
                violations(violation_count) = create_diagnostic( &
                    code="F003", &
                    message="Line too long (" // trim(adjustl(int_to_str(line_length))) // &
                           " > " // trim(adjustl(int_to_str(max_length))) // " characters)", &
                    file_path=current_filename, &
                    location=location, &
                    severity=SEVERITY_WARNING)
                
                ! Add fix suggestions to the diagnostic
                allocate(violations(violation_count)%fixes, source=fix_suggestions)
            end if
        end do
        
    end subroutine analyze_line_lengths_with_ast_context
    
    ! Generate smart line breaking suggestions
    subroutine generate_line_break_suggestions(line, suggested_fix)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: suggested_fix
        
        character(len=:), allocatable :: trimmed_line
        integer :: break_pos, i
        logical :: found_break_point
        
        trimmed_line = trim(line)
        found_break_point = .false.
        
        ! Look for good breaking points (in order of preference):
        ! 1. After commas in argument lists
        ! 2. After operators (+, -, *, /, ==, etc.)
        ! 3. After :: in declarations
        ! 4. Before keywords (then, else, etc.)
        
        ! First try: break after commas
        do i = len(trimmed_line), 50, -1  ! Start from end, work backwards to column 50
            if (i <= len(trimmed_line) .and. trimmed_line(i:i) == ',') then
                break_pos = i
                found_break_point = .true.
                exit
            end if
        end do
        
        ! Second try: break after operators if no comma found
        if (.not. found_break_point) then
            do i = len(trimmed_line), 50, -1
                if (i <= len(trimmed_line)) then
                    if (trimmed_line(i:i) == '+' .or. trimmed_line(i:i) == '-' .or. &
                        trimmed_line(i:i) == '*' .or. trimmed_line(i:i) == '/') then
                        break_pos = i
                        found_break_point = .true.
                        exit
                    end if
                end if
            end do
        end if
        
        ! Third try: break after ::
        if (.not. found_break_point) then
            i = index(trimmed_line, '::')
            if (i > 0 .and. i < 80) then
                break_pos = i + 1
                found_break_point = .true.
            end if
        end if
        
        ! Generate the suggested fix
        if (found_break_point) then
            suggested_fix = trimmed_line(1:break_pos) // " &" // new_line('a') // &
                           "        " // trim(adjustl(trimmed_line(break_pos+1:)))
        else
            ! Fallback: simple break at 80 characters
            suggested_fix = trimmed_line(1:80) // " &" // new_line('a') // &
                           "        " // trim(adjustl(trimmed_line(81:)))
        end if
        
    end subroutine generate_line_break_suggestions
    
    ! Helper function to check if line is a comment
    function is_comment_line(line) result(is_comment)
        character(len=*), intent(in) :: line
        logical :: is_comment
        
        character(len=:), allocatable :: trimmed_line
        
        trimmed_line = adjustl(line)
        is_comment = len(trimmed_line) > 0 .and. trimmed_line(1:1) == '!'
        
    end function is_comment_line
    
    ! Helper function to convert integer to string
    function int_to_str(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str
        
        write(str, '(I0)') i
        
    end function int_to_str
    
    ! Analyze trailing whitespace from source text
    subroutine analyze_trailing_whitespace_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer :: pos, next_pos, line_num
        integer :: line_length, trimmed_length
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        
        pos = 1
        line_num = 0
        
        do while (pos <= len(source_text))
            line_num = line_num + 1
            
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                ! Last line
                line_start = pos
                line_end = len(source_text)
                pos = len(source_text) + 1  ! Exit after this line
            else
                ! Regular line
                line_start = pos
                line_end = pos + next_pos - 2
                ! Handle carriage return if present
                if (line_end >= line_start .and. source_text(line_end:line_end) == char(13)) then
                    line_end = line_end - 1
                end if
                pos = pos + next_pos
            end if
            
            ! Extract actual line content
            if (line_end >= line_start) then
                line_content = source_text(line_start:line_end)
                line_length = line_end - line_start + 1
                trimmed_length = len_trim(line_content)
                
                
                ! Check if line has trailing whitespace (spaces or tabs)
                ! Note: len_trim doesn't trim tabs, so we need to check manually
                block
                    integer :: true_trimmed_length, i
                    logical :: has_trailing_whitespace
                    
                    ! Find the actual end of non-whitespace content (excluding trailing spaces AND tabs)
                    true_trimmed_length = line_length
                    do i = line_length, 1, -1
                        if (line_content(i:i) /= ' ' .and. line_content(i:i) /= char(9)) then
                            true_trimmed_length = i
                            exit
                        end if
                        if (i == 1) true_trimmed_length = 0  ! Line is all whitespace
                    end do
                    
                    has_trailing_whitespace = (line_length > 0 .and. true_trimmed_length > 0 &
                                              .and. true_trimmed_length < line_length)
                    
                    if (has_trailing_whitespace) then
                        violation_count = violation_count + 1
                        if (violation_count <= size(violations)) then
                            ! Create location for trailing whitespace
                            location%start%line = line_num
                            location%start%column = true_trimmed_length + 1  
                            location%end%line = line_num
                            location%end%column = line_length
                            violations(violation_count) = create_diagnostic( &
                                code="F004", &
                                message="Trailing whitespace", &
                                file_path=current_filename, &
                                location=location, &
                                severity=SEVERITY_INFO)
                        end if
                    end if
                end block
            end if
            
            ! Move to next line if not already at end
            if (next_pos == 0) exit
        end do
        
    end subroutine analyze_trailing_whitespace_from_text
    
    ! Analyze mixed tabs and spaces from source text
    subroutine analyze_mixed_tabs_spaces_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        integer :: i, spaces, tabs
        logical :: has_spaces, has_tabs
        
        pos = 1
        line_num = 0
        
        do while (pos <= len(source_text))
            line_num = line_num + 1
            
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                ! Last line
                line_start = pos
                line_end = len(source_text)
            else
                ! Regular line
                line_start = pos
                line_end = pos + next_pos - 2
                pos = pos + next_pos
            end if
            
            ! Extract actual line content
            if (line_end >= line_start) then
                line_content = source_text(line_start:line_end)
                
                ! Check for mixed tabs and spaces in leading whitespace
                spaces = 0
                tabs = 0
                has_spaces = .false.
                has_tabs = .false.
                
                ! Count leading whitespace characters
                do i = 1, len(line_content)
                    if (line_content(i:i) == ' ') then
                        spaces = spaces + 1
                        has_spaces = .true.
                    else if (line_content(i:i) == char(9)) then  ! tab character
                        tabs = tabs + 1
                        has_tabs = .true.
                    else
                        ! End of leading whitespace
                        exit
                    end if
                end do
                
                ! Check if line has both tabs and spaces in indentation
                if (has_spaces .and. has_tabs .and. len_trim(line_content) > 0) then
                    violation_count = violation_count + 1
                    if (violation_count <= size(violations)) then
                        ! Create location for mixed indentation
                        location%start%line = line_num
                        location%start%column = 1
                        location%end%line = line_num
                        location%end%column = spaces + tabs
                        violations(violation_count) = create_diagnostic( &
                            code="F005", &
                            message="Mixed tabs and spaces in indentation", &
                            file_path=current_filename, &
                            location=location, &
                            severity=SEVERITY_WARNING)
                    end if
                end if
            end if
            
            ! Move to next line if not already at end
            if (next_pos == 0) exit
        end do
        
    end subroutine analyze_mixed_tabs_spaces_from_text
    
    ! Analyze multiple statements from source text
    subroutine analyze_multiple_statements_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        integer :: i, semicolon_count, semicolon_pos
        logical :: in_string
        character :: quote_char
        
        pos = 1
        line_num = 0
        
        do while (pos <= len(source_text))
            line_num = line_num + 1
            
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                ! Last line
                line_start = pos
                line_end = len(source_text)
            else
                ! Regular line
                line_start = pos
                line_end = pos + next_pos - 2
                pos = pos + next_pos
            end if
            
            ! Extract actual line content
            if (line_end >= line_start) then
                line_content = source_text(line_start:line_end)
                
                ! Skip comment lines
                if (.not. is_comment_line(line_content)) then
                    ! Count semicolons not in strings
                    semicolon_count = 0
                    semicolon_pos = 0
                    in_string = .false.
                    quote_char = ' '
                    
                    do i = 1, len(line_content)
                        ! Handle string literals
                        if (.not. in_string) then
                            if (line_content(i:i) == '"' .or. line_content(i:i) == "'") then
                                in_string = .true.
                                quote_char = line_content(i:i)
                            else if (line_content(i:i) == ';') then
                                semicolon_count = semicolon_count + 1
                                if (semicolon_pos == 0) semicolon_pos = i
                            else if (line_content(i:i) == '!' .and. .not. in_string) then
                                ! Stop at comment
                                exit
                            end if
                        else
                            if (line_content(i:i) == quote_char) then
                                ! Check if it's escaped
                                if (i == 1 .or. line_content(i-1:i-1) /= '\') then
                                    in_string = .false.
                                end if
                            end if
                        end if
                    end do
                    
                    ! Report if multiple statements found
                    if (semicolon_count > 0) then
                        violation_count = violation_count + 1
                        if (violation_count <= size(violations)) then
                            ! Create location for semicolon
                            location%start%line = line_num
                            location%start%column = semicolon_pos
                            location%end%line = line_num
                            location%end%column = semicolon_pos
                            violations(violation_count) = create_diagnostic( &
                                code="F013", &
                                message="Multiple statements per line", &
                                file_path=current_filename, &
                                location=location, &
                                severity=SEVERITY_WARNING)
                        end if
                    end if
                end if
            end if
            
            ! Move to next line if not already at end
            if (next_pos == 0) exit
        end do
        
    end subroutine analyze_multiple_statements_from_text
    
    ! Analyze obsolete features from source text
    subroutine analyze_obsolete_features_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_lower
        integer :: goto_pos, common_pos, equiv_pos
        
        pos = 1
        line_num = 0
        
        do while (pos <= len(source_text))
            line_num = line_num + 1
            
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                ! Last line
                line_start = pos
                line_end = len(source_text)
            else
                ! Regular line
                line_start = pos
                line_end = pos + next_pos - 2
                pos = pos + next_pos
            end if
            
            ! Extract actual line content
            if (line_end >= line_start) then
                line_content = source_text(line_start:line_end)
                
                ! Skip comment lines
                if (.not. is_comment_line(line_content)) then
                    ! Convert to lowercase for case-insensitive search
                    line_lower = to_lower(line_content)
                    
                    ! Check for GOTO statement
                    goto_pos = index(line_lower, 'goto')
                    if (goto_pos > 0) then
                        ! Make sure it's not part of a larger word
                        if (is_keyword_at_position(line_lower, 'goto', goto_pos)) then
                            violation_count = violation_count + 1
                            if (violation_count <= size(violations)) then
                                location%start%line = line_num
                                location%start%column = goto_pos
                                location%end%line = line_num
                                location%end%column = goto_pos + 3
                                violations(violation_count) = create_diagnostic( &
                                    code="F010", &
                                    message="Obsolete feature: GOTO statement", &
                                    file_path=current_filename, &
                                    location=location, &
                                    severity=SEVERITY_WARNING)
                            end if
                        end if
                    end if
                    
                    ! Check for COMMON blocks
                    common_pos = index(line_lower, 'common')
                    if (common_pos > 0) then
                        if (is_keyword_at_position(line_lower, 'common', common_pos)) then
                            violation_count = violation_count + 1
                            if (violation_count <= size(violations)) then
                                location%start%line = line_num
                                location%start%column = common_pos
                                location%end%line = line_num
                                location%end%column = common_pos + 5
                                violations(violation_count) = create_diagnostic( &
                                    code="F010", &
                                    message="Obsolete feature: COMMON block", &
                                    file_path=current_filename, &
                                    location=location, &
                                    severity=SEVERITY_WARNING)
                            end if
                        end if
                    end if
                    
                    ! Check for EQUIVALENCE
                    equiv_pos = index(line_lower, 'equivalence')
                    if (equiv_pos > 0) then
                        if (is_keyword_at_position(line_lower, 'equivalence', equiv_pos)) then
                            violation_count = violation_count + 1
                            if (violation_count <= size(violations)) then
                                location%start%line = line_num
                                location%start%column = equiv_pos
                                location%end%line = line_num
                                location%end%column = equiv_pos + 10
                                violations(violation_count) = create_diagnostic( &
                                    code="F010", &
                                    message="Obsolete feature: EQUIVALENCE statement", &
                                    file_path=current_filename, &
                                    location=location, &
                                    severity=SEVERITY_WARNING)
                            end if
                        end if
                    end if
                end if
            end if
            
            ! Move to next line if not already at end
            if (next_pos == 0) exit
        end do
        
    end subroutine analyze_obsolete_features_from_text
    
    ! Helper to convert string to lowercase
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: lower_str
        integer :: i
        
        allocate(character(len=len(str)) :: lower_str)
        do i = 1, len(str)
            if (str(i:i) >= 'A' .and. str(i:i) <= 'Z') then
                lower_str(i:i) = char(ichar(str(i:i)) + 32)
            else
                lower_str(i:i) = str(i:i)
            end if
        end do
    end function to_lower
    
    ! Check if keyword is at position (not part of larger word)
    function is_keyword_at_position(line, keyword, pos) result(is_keyword)
        character(len=*), intent(in) :: line
        character(len=*), intent(in) :: keyword
        integer, intent(in) :: pos
        logical :: is_keyword

        logical :: start_ok, end_ok
        integer :: end_pos

        end_pos = pos + len(keyword) - 1

        ! Check if keyword starts at word boundary
        if (pos == 1) then
            start_ok = .true.
        else if (pos > 1 .and. pos <= len(line)) then
            start_ok = .not. is_alphanumeric(line(pos-1:pos-1))
        else
            start_ok = .false.
        end if

        ! Check if keyword ends at word boundary
        if (end_pos == len(line)) then
            end_ok = .true.
        else if (end_pos > 0 .and. end_pos < len(line)) then
            end_ok = .not. is_alphanumeric(line(end_pos+1:end_pos+1))
        else
            end_ok = .false.
        end if

        is_keyword = start_ok .and. end_ok

    end function is_keyword_at_position
    
    ! Check if character is alphanumeric
    function is_alphanumeric(ch) result(is_alnum)
        character, intent(in) :: ch
        logical :: is_alnum
        
        is_alnum = (ch >= 'a' .and. ch <= 'z') .or. &
                   (ch >= 'A' .and. ch <= 'Z') .or. &
                   (ch >= '0' .and. ch <= '9') .or. &
                   (ch == '_')
        
    end function is_alphanumeric
    
    ! Analyze redundant continue from source text
    subroutine analyze_redundant_continue_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_trimmed
        character(len=:), allocatable :: line_lower
        integer :: continue_pos
        
        pos = 1
        line_num = 0
        
        do while (pos <= len(source_text))
            line_num = line_num + 1
            
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                ! Last line
                line_start = pos
                line_end = len(source_text)
            else
                ! Regular line
                line_start = pos
                line_end = pos + next_pos - 2
                pos = pos + next_pos
            end if
            
            ! Extract actual line content
            if (line_end >= line_start) then
                line_content = source_text(line_start:line_end)
                
                ! Skip comment lines
                if (.not. is_comment_line(line_content)) then
                    line_trimmed = adjustl(line_content)
                    line_lower = to_lower(line_trimmed)
                    
                    ! Check for continue statement
                    continue_pos = index(line_lower, 'continue')
                    if (continue_pos > 0) then
                        ! Check if it's a standalone continue (redundant)
                        if (is_keyword_at_position(line_lower, 'continue', continue_pos)) then
                            ! Check if it's just "continue" or labeled continue
                            violation_count = violation_count + 1
                            if (violation_count <= size(violations)) then
                                location%start%line = line_num
                                location%start%column = continue_pos
                                location%end%line = line_num
                                location%end%column = continue_pos + 7
                                violations(violation_count) = create_diagnostic( &
                                    code="F015", &
                                    message="Redundant CONTINUE statement", &
                                    file_path=current_filename, &
                                    location=location, &
                                    severity=SEVERITY_INFO)
                            end if
                        end if
                    end if
                end if
            end if
            
            ! Move to next line if not already at end
            if (next_pos == 0) exit
        end do
        
    end subroutine analyze_redundant_continue_from_text
    
    ! Analyze missing intent from source text
    subroutine analyze_missing_intent_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_trimmed
        character(len=:), allocatable :: line_lower
        logical :: in_procedure
        logical :: in_interface
        character(len=:), allocatable :: current_proc_name
        character(len=:), allocatable :: proc_args
        integer :: proc_start_line
        integer :: i, arg_start, arg_end
        character(len=:), allocatable :: arg_name
        logical, allocatable :: arg_has_intent(:)
        character(len=:), allocatable :: args_array(:)
        integer :: num_args
        
        pos = 1
        line_num = 0
        in_procedure = .false.
        in_interface = .false.
        allocate(arg_has_intent(100))
        num_args = 0
        
        do while (pos <= len(source_text))
            line_num = line_num + 1
            
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                line_start = pos
                line_end = len(source_text)
            else
                line_start = pos
                line_end = pos + next_pos - 2
                pos = pos + next_pos
            end if
            
            ! Extract line content
            if (line_end >= line_start) then
                line_content = source_text(line_start:line_end)
                
                ! Skip comment lines
                if (.not. is_comment_line(line_content)) then
                    line_trimmed = adjustl(line_content)
                    line_lower = to_lower(line_trimmed)
                    
                    ! Check for interface block
                    if (index(line_lower, 'interface') == 1) then
                        in_interface = .true.
                    else if (index(line_lower, 'end interface') == 1) then
                        in_interface = .false.
                    end if
                    
                    ! Skip interface blocks
                    if (.not. in_interface) then
                        ! Check for subroutine or function definition
                        if ((index(line_lower, 'subroutine ') == 1) .or. &
                            (index(line_lower, 'function ') > 0 .and. index(line_lower, 'function ') <= 20)) then
                            in_procedure = .true.
                            proc_start_line = line_num
                            
                            ! Extract arguments from procedure declaration
                            arg_start = index(line_lower, '(')
                            arg_end = index(line_lower, ')')
                            if (arg_start > 0 .and. arg_end > arg_start) then
                                proc_args = line_lower(arg_start+1:arg_end-1)
                                ! Parse arguments (simplified - just count commas + 1)
                                num_args = 1
                                do i = 1, len(proc_args)
                                    if (proc_args(i:i) == ',') num_args = num_args + 1
                                end do
                                ! Initialize intent tracking
                                arg_has_intent = .false.
                            else
                                num_args = 0
                            end if
                        else if ((index(line_lower, 'end subroutine') == 1) .or. &
                                 (index(line_lower, 'end function') == 1)) then
                            ! Check if any arguments are missing intent
                            if (in_procedure .and. num_args > 0) then
                                ! Only report if we found arguments without intent
                                do i = 1, num_args
                                    if (.not. arg_has_intent(i)) then
                                        violation_count = violation_count + 1
                                        if (violation_count <= size(violations)) then
                                            location%start%line = proc_start_line
                                            location%start%column = 1
                                            location%end%line = proc_start_line
                                            location%end%column = 1
                                            violations(violation_count) = create_diagnostic( &
                                                code="F008", &
                                                message="Missing intent declaration for procedure arguments", &
                                                file_path=current_filename, &
                                                location=location, &
                                                severity=SEVERITY_WARNING)
                                        end if
                                        exit  ! Only report once per procedure
                                    end if
                                end do
                            end if
                            in_procedure = .false.
                            num_args = 0
                        else if (in_procedure) then
                            ! Check for intent declarations within procedure
                            if (index(line_lower, 'intent(') > 0) then
                                ! Mark that at least some arguments have intent
                                ! (Simplified - would need more complex parsing for real mapping)
                                do i = 1, num_args
                                    arg_has_intent(i) = .true.
                                end do
                            end if
                        end if
                    end if
                end if
            end if
            
            ! Move to next line if not at end
            if (next_pos == 0) exit
        end do
        
    end subroutine analyze_missing_intent_from_text
    
    ! Analyze missing end labels from source text
    subroutine analyze_missing_end_labels_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_trimmed
        character(len=:), allocatable :: line_lower
        character(len=:), allocatable :: stack_names(:)
        integer :: stack_depth
        integer :: i, name_start, name_end
        character(len=:), allocatable :: name
        
        pos = 1
        line_num = 0
        stack_depth = 0
        allocate(character(len=64) :: stack_names(100))
        
        do while (pos <= len(source_text))
            line_num = line_num + 1
            
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                line_start = pos
                line_end = len(source_text)
            else
                line_start = pos
                line_end = pos + next_pos - 2
                pos = pos + next_pos
            end if
            
            ! Extract line content
            if (line_end >= line_start) then
                line_content = source_text(line_start:line_end)
                
                ! Skip comment lines
                if (.not. is_comment_line(line_content)) then
                    line_trimmed = adjustl(line_content)
                    line_lower = to_lower(line_trimmed)
                    
                    ! Check for program/module/subroutine/function start
                    if (index(line_lower, 'program ') == 1) then
                        stack_depth = stack_depth + 1
                        name_start = 9
                        name_end = len(line_trimmed)
                        stack_names(stack_depth) = extract_name(line_trimmed(name_start:name_end))
                    else if (index(line_lower, 'module ') == 1 .and. index(line_lower, 'module procedure') /= 1) then
                        stack_depth = stack_depth + 1
                        name_start = 8
                        name_end = len(line_trimmed)
                        stack_names(stack_depth) = extract_name(line_trimmed(name_start:name_end))
                    else if (index(line_lower, 'subroutine ') == 1) then
                        stack_depth = stack_depth + 1
                        name_start = 12
                        name_end = len(line_trimmed)
                        stack_names(stack_depth) = extract_name(line_trimmed(name_start:name_end))
                    else if (index(line_lower, 'function ') > 0 .and. &
                             (index(line_lower, 'function ') == 1 .or. &
                              index(line_lower, 'pure function ') == 1 .or. &
                              index(line_lower, 'elemental function ') == 1)) then
                        stack_depth = stack_depth + 1
                        name_start = index(line_lower, 'function ') + 9
                        name_end = len(line_trimmed)
                        stack_names(stack_depth) = extract_name(line_trimmed(name_start:name_end))
                    ! Check for end statements
                    else if (index(line_lower, 'end program') == 1) then
                        ! Check if label is missing
                        if (len_trim(line_lower) == 11) then  ! Just "end program"
                            violation_count = violation_count + 1
                            if (violation_count <= size(violations)) then
                                location%start%line = line_num
                                location%start%column = 1
                                location%end%line = line_num
                                location%end%column = 11
                                violations(violation_count) = create_diagnostic( &
                                    code="F011", &
                                    message="Missing end label for program", &
                                    file_path=current_filename, &
                                    location=location, &
                                    severity=SEVERITY_INFO)
                            end if
                        end if
                        if (stack_depth > 0) stack_depth = stack_depth - 1
                    else if (index(line_lower, 'end module') == 1) then
                        ! Check if label is missing
                        if (len_trim(line_lower) == 10) then  ! Just "end module"
                            violation_count = violation_count + 1
                            if (violation_count <= size(violations)) then
                                location%start%line = line_num
                                location%start%column = 1
                                location%end%line = line_num
                                location%end%column = 10
                                violations(violation_count) = create_diagnostic( &
                                    code="F011", &
                                    message="Missing end label for module", &
                                    file_path=current_filename, &
                                    location=location, &
                                    severity=SEVERITY_INFO)
                            end if
                        end if
                        if (stack_depth > 0) stack_depth = stack_depth - 1
                    else if (index(line_lower, 'end subroutine') == 1) then
                        ! Check if label is missing
                        if (len_trim(line_lower) == 14) then  ! Just "end subroutine"
                            violation_count = violation_count + 1
                            if (violation_count <= size(violations)) then
                                location%start%line = line_num
                                location%start%column = 1
                                location%end%line = line_num
                                location%end%column = 14
                                violations(violation_count) = create_diagnostic( &
                                    code="F011", &
                                    message="Missing end label for subroutine", &
                                    file_path=current_filename, &
                                    location=location, &
                                    severity=SEVERITY_INFO)
                            end if
                        end if
                        if (stack_depth > 0) stack_depth = stack_depth - 1
                    else if (index(line_lower, 'end function') == 1) then
                        ! Check if label is missing
                        if (len_trim(line_lower) == 12) then  ! Just "end function"
                            violation_count = violation_count + 1
                            if (violation_count <= size(violations)) then
                                location%start%line = line_num
                                location%start%column = 1
                                location%end%line = line_num
                                location%end%column = 12
                                violations(violation_count) = create_diagnostic( &
                                    code="F011", &
                                    message="Missing end label for function", &
                                    file_path=current_filename, &
                                    location=location, &
                                    severity=SEVERITY_INFO)
                            end if
                        end if
                        if (stack_depth > 0) stack_depth = stack_depth - 1
                    end if
                end if
            end if
            
            ! Move to next line if not at end
            if (next_pos == 0) exit
        end do
        
    end subroutine analyze_missing_end_labels_from_text
    
    ! Extract name from procedure/program declaration
    function extract_name(str) result(name)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: name
        integer :: paren_pos, space_pos
        character(len=:), allocatable :: trimmed
        
        trimmed = adjustl(str)
        
        ! Find parenthesis or space
        paren_pos = index(trimmed, '(')
        space_pos = index(trimmed, ' ')
        
        if (paren_pos > 0) then
            name = trimmed(1:paren_pos-1)
        else if (space_pos > 0) then
            name = trimmed(1:space_pos-1)
        else
            name = trimmed
        end if
        
    end function extract_name
    
    ! Analyze naming conventions from source text
    subroutine analyze_naming_conventions_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_trimmed
        character(len=:), allocatable :: line_lower
        character(len=:), allocatable :: var_names(:)
        integer :: num_vars
        integer :: i, j, colon_pos, double_colon_pos
        integer :: snake_count, camel_count, pascal_count, const_count
        character(len=:), allocatable :: var_name
        character(len=:), allocatable :: dominant_style
        
        allocate(character(len=64) :: var_names(1000))
        num_vars = 0
        pos = 1
        line_num = 0
        
        ! First pass: collect all variable names
        do while (pos <= len(source_text))
            line_num = line_num + 1
            
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                line_start = pos
                line_end = len(source_text)
            else
                line_start = pos
                line_end = pos + next_pos - 2
                pos = pos + next_pos
            end if
            
            ! Extract line content
            if (line_end >= line_start) then
                line_content = source_text(line_start:line_end)
                
                ! Skip comment lines
                if (.not. is_comment_line(line_content)) then
                    line_trimmed = adjustl(line_content)
                    line_lower = to_lower(line_trimmed)
                    
                    ! Check for variable declarations (simplified)
                    double_colon_pos = index(line_trimmed, '::')
                    if (double_colon_pos > 0) then
                        ! Check if it's a declaration (has type keyword before ::)
                        if (has_type_keyword(line_lower(1:double_colon_pos))) then
                            ! Extract variable names after ::
                            call extract_variable_names(line_trimmed(double_colon_pos+2:), &
                                                       var_names, num_vars)
                        end if
                    end if
                end if
            end if
            
            ! Move to next line if not at end
            if (next_pos == 0) exit
        end do
        
        ! Analyze naming styles if we have variables
        if (num_vars > 0) then
            snake_count = 0
            camel_count = 0
            pascal_count = 0
            const_count = 0
            
            do i = 1, num_vars
                var_name = trim(var_names(i))
                if (is_snake_case(var_name)) snake_count = snake_count + 1
                if (is_camel_case(var_name)) camel_count = camel_count + 1
                if (is_pascal_case(var_name)) pascal_count = pascal_count + 1
                if (is_const_case(var_name)) const_count = const_count + 1
            end do
            
            ! Determine dominant style
            if (snake_count >= camel_count .and. snake_count >= pascal_count) then
                dominant_style = "snake_case"
            else if (camel_count >= pascal_count) then
                dominant_style = "camelCase"
            else
                dominant_style = "PascalCase"
            end if
            
            ! Report if there's inconsistency (more than one style used)
            if ((snake_count > 0 .and. camel_count > 0) .or. &
                (snake_count > 0 .and. pascal_count > 0) .or. &
                (camel_count > 0 .and. pascal_count > 0)) then
                violation_count = violation_count + 1
                if (violation_count <= size(violations)) then
                    location%start%line = 1
                    location%start%column = 1
                    location%end%line = 1
                    location%end%column = 1
                    violations(violation_count) = create_diagnostic( &
                        code="F012", &
                        message="Inconsistent naming convention detected. Consider using " // dominant_style // " consistently", &
                        file_path=current_filename, &
                        location=location, &
                        severity=SEVERITY_INFO)
                end if
            end if
        end if
        
    end subroutine analyze_naming_conventions_from_text
    
    ! Check if line has type keyword before ::
    function has_type_keyword(str) result(has_type)
        character(len=*), intent(in) :: str
        logical :: has_type
        character(len=:), allocatable :: trimmed
        
        trimmed = adjustl(str)
        
        has_type = index(trimmed, 'integer') > 0 .or. &
                   index(trimmed, 'real') > 0 .or. &
                   index(trimmed, 'character') > 0 .or. &
                   index(trimmed, 'logical') > 0 .or. &
                   index(trimmed, 'complex') > 0 .or. &
                   index(trimmed, 'type(') > 0 .or. &
                   index(trimmed, 'class(') > 0 .or. &
                   index(trimmed, 'double precision') > 0
        
    end function has_type_keyword
    
    ! Extract variable names from declaration
    subroutine extract_variable_names(decl_str, var_names, num_vars)
        character(len=*), intent(in) :: decl_str
        character(len=*), intent(inout) :: var_names(:)
        integer, intent(inout) :: num_vars
        
        character(len=:), allocatable :: trimmed
        integer :: i, start_pos, comma_pos, eq_pos, paren_pos
        character(len=:), allocatable :: var_part
        
        trimmed = adjustl(decl_str)
        start_pos = 1
        
        do while (start_pos <= len(trimmed))
            ! Find next comma
            comma_pos = index(trimmed(start_pos:), ',')
            
            if (comma_pos == 0) then
                ! Last variable
                var_part = trimmed(start_pos:)
            else
                var_part = trimmed(start_pos:start_pos+comma_pos-2)
            end if
            
            ! Remove initialization part if present
            eq_pos = index(var_part, '=')
            if (eq_pos > 0) then
                var_part = var_part(1:eq_pos-1)
            end if
            
            ! Remove array dimension if present
            paren_pos = index(var_part, '(')
            if (paren_pos > 0) then
                var_part = var_part(1:paren_pos-1)
            end if
            
            ! Add variable name
            var_part = adjustl(trim(var_part))
            if (len(var_part) > 0) then
                num_vars = num_vars + 1
                if (num_vars <= size(var_names)) then
                    var_names(num_vars) = var_part
                end if
            end if
            
            if (comma_pos == 0) exit
            start_pos = start_pos + comma_pos
        end do
        
    end subroutine extract_variable_names
    
    ! Check if name is snake_case
    function is_snake_case(name) result(is_snake)
        character(len=*), intent(in) :: name
        logical :: is_snake
        integer :: i
        
        is_snake = .true.
        
        ! Check for underscore and all lowercase
        do i = 1, len(name)
            if (name(i:i) >= 'A' .and. name(i:i) <= 'Z') then
                is_snake = .false.
                return
            end if
        end do
        
        ! Must have underscore for multi-word
        is_snake = index(name, '_') > 0 .or. len(name) <= 4
        
    end function is_snake_case
    
    ! Check if name is camelCase
    function is_camel_case(name) result(is_camel)
        character(len=*), intent(in) :: name
        logical :: is_camel
        logical :: has_upper, has_lower
        integer :: i
        
        is_camel = .false.
        has_upper = .false.
        has_lower = .false.
        
        ! First char should be lowercase
        if (name(1:1) >= 'A' .and. name(1:1) <= 'Z') return
        
        ! Check for mixed case
        do i = 1, len(name)
            if (name(i:i) >= 'a' .and. name(i:i) <= 'z') has_lower = .true.
            if (name(i:i) >= 'A' .and. name(i:i) <= 'Z') has_upper = .true.
        end do
        
        is_camel = has_upper .and. has_lower .and. index(name, '_') == 0
        
    end function is_camel_case
    
    ! Check if name is PascalCase
    function is_pascal_case(name) result(is_pascal)
        character(len=*), intent(in) :: name
        logical :: is_pascal
        logical :: has_upper, has_lower
        integer :: i
        
        is_pascal = .false.
        has_upper = .false.
        has_lower = .false.
        
        ! First char should be uppercase
        if (name(1:1) < 'A' .or. name(1:1) > 'Z') return
        
        ! Check for mixed case
        do i = 1, len(name)
            if (name(i:i) >= 'a' .and. name(i:i) <= 'z') has_lower = .true.
            if (name(i:i) >= 'A' .and. name(i:i) <= 'Z') has_upper = .true.
        end do
        
        is_pascal = has_upper .and. has_lower .and. index(name, '_') == 0
        
    end function is_pascal_case
    
    ! Check if name is CONST_CASE
    function is_const_case(name) result(is_const)
        character(len=*), intent(in) :: name
        logical :: is_const
        integer :: i
        
        is_const = .true.
        
        ! Check for all uppercase
        do i = 1, len(name)
            if (name(i:i) >= 'a' .and. name(i:i) <= 'z') then
                is_const = .false.
                return
            end if
        end do
        
    end function is_const_case
    
    ! Analyze unnecessary parentheses from source text
    subroutine analyze_unnecessary_parentheses_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_trimmed
        integer :: i, paren_start, paren_end
        character(len=:), allocatable :: inner_content
        
        pos = 1
        line_num = 0
        
        do while (pos <= len(source_text))
            line_num = line_num + 1
            
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                line_start = pos
                line_end = len(source_text)
            else
                line_start = pos
                line_end = pos + next_pos - 2
                pos = pos + next_pos
            end if
            
            ! Extract line content
            if (line_end >= line_start) then
                line_content = source_text(line_start:line_end)
                
                ! Skip comment lines
                if (.not. is_comment_line(line_content)) then
                    line_trimmed = adjustl(line_content)
                    
                    ! Check for patterns like (10) or ((expr))
                    i = 1
                    do while (i <= len(line_trimmed) - 2)
                        if (line_trimmed(i:i) == '(') then
                            ! Find matching closing parenthesis
                            paren_end = find_matching_paren(line_trimmed, i)
                            if (paren_end > 0) then
                                inner_content = line_trimmed(i+1:paren_end-1)
                                inner_content = adjustl(trim(inner_content))
                                
                                ! Check if inner content is simple (number or variable)
                                if (is_simple_expression(inner_content)) then
                                    ! Check context - not a function call and not needed for precedence
                                    if (i == 1 .or. .not. is_alnum_or_underscore(line_trimmed(i-1:i-1))) then
                                        ! Check if it's in an if statement or other control structure
                                        if (.not. is_in_control_structure(line_trimmed, i)) then
                                            ! Check if it's part of a larger expression needing precedence
                                            if (.not. needs_precedence_parentheses(line_trimmed, i, paren_end)) then
                                                violation_count = violation_count + 1
                                                if (violation_count <= size(violations)) then
                                                    location%start%line = line_num
                                                    location%start%column = i
                                                    location%end%line = line_num
                                                    location%end%column = paren_end
                                                    violations(violation_count) = create_diagnostic( &
                                                        code="F014", &
                                                        message="Unnecessary parentheses around simple expression", &
                                                        file_path=current_filename, &
                                                        location=location, &
                                                        severity=SEVERITY_INFO)
                                                end if
                                            end if
                                        end if
                                    end if
                                end if
                                i = paren_end + 1
                            else
                                i = i + 1
                            end if
                        else
                            i = i + 1
                        end if
                    end do
                end if
            end if
            
            ! Move to next line if not at end
            if (next_pos == 0) exit
        end do
        
    end subroutine analyze_unnecessary_parentheses_from_text
    
    ! Find matching closing parenthesis
    function find_matching_paren(str, start_pos) result(end_pos)
        character(len=*), intent(in) :: str
        integer, intent(in) :: start_pos
        integer :: end_pos
        integer :: depth, i
        
        depth = 1
        end_pos = 0
        
        do i = start_pos + 1, len(str)
            if (str(i:i) == '(') then
                depth = depth + 1
            else if (str(i:i) == ')') then
                depth = depth - 1
                if (depth == 0) then
                    end_pos = i
                    return
                end if
            end if
        end do
        
    end function find_matching_paren
    
    ! Check if expression is simple (number or variable)
    function is_simple_expression(expr) result(is_simple)
        character(len=*), intent(in) :: expr
        logical :: is_simple
        integer :: i
        logical :: has_operator
        character(len=:), allocatable :: trimmed_expr
        
        trimmed_expr = adjustl(trim(expr))
        has_operator = .false.
        
        ! Empty expression is not simple
        if (len(trimmed_expr) == 0) then
            is_simple = .false.
            return
        end if
        
        ! Check for operators or complex constructs
        do i = 1, len(trimmed_expr)
            if (trimmed_expr(i:i) == '+' .or. trimmed_expr(i:i) == '-' .or. &
                trimmed_expr(i:i) == '*' .or. trimmed_expr(i:i) == '/' .or. &
                trimmed_expr(i:i) == '(' .or. trimmed_expr(i:i) == ')' .or. &
                trimmed_expr(i:i) == '=' .or. trimmed_expr(i:i) == '<' .or. &
                trimmed_expr(i:i) == '>' .or. trimmed_expr(i:i) == '.' .or. &
                trimmed_expr(i:i) == '&') then
                has_operator = .true.
                exit
            end if
        end do
        
        ! Only flag as simple if it's truly a single variable or number
        is_simple = .not. has_operator .and. len(trimmed_expr) > 0
        
    end function is_simple_expression
    
    ! Check if character is alphanumeric or underscore
    function is_alnum_or_underscore(ch) result(is_alnum)
        character, intent(in) :: ch
        logical :: is_alnum
        
        is_alnum = (ch >= 'a' .and. ch <= 'z') .or. &
                   (ch >= 'A' .and. ch <= 'Z') .or. &
                   (ch >= '0' .and. ch <= '9') .or. &
                   (ch == '_')
        
    end function is_alnum_or_underscore
    
    ! Check if parentheses are in a control structure
    function is_in_control_structure(line, pos) result(in_control)
        character(len=*), intent(in) :: line
        integer, intent(in) :: pos
        logical :: in_control
        character(len=:), allocatable :: line_lower
        
        line_lower = to_lower(adjustl(line))
        
        ! Check for control structures where parentheses are expected
        in_control = index(line_lower, "if") > 0 .or. &
                    index(line_lower, "while") > 0 .or. &
                    index(line_lower, "select") > 0 .or. &
                    index(line_lower, "case") > 0
        
    end function is_in_control_structure
    
    ! Check if parentheses are needed for operator precedence
    function needs_precedence_parentheses(line, start_pos, end_pos) result(needs_precedence)
        character(len=*), intent(in) :: line
        integer, intent(in) :: start_pos, end_pos
        logical :: needs_precedence
        integer :: before_pos, after_pos
        
        needs_precedence = .false.
        
        ! Check character before parentheses
        before_pos = start_pos - 1
        if (before_pos >= 1) then
            if (line(before_pos:before_pos) == '*' .or. line(before_pos:before_pos) == '/' .or. &
                line(before_pos:before_pos) == '+' .or. line(before_pos:before_pos) == '-') then
                needs_precedence = .true.
                return
            end if
        end if
        
        ! Check character after parentheses
        after_pos = end_pos + 1
        if (after_pos <= len(line)) then
            if (line(after_pos:after_pos) == '*' .or. line(after_pos:after_pos) == '/' .or. &
                line(after_pos:after_pos) == '+' .or. line(after_pos:after_pos) == '-') then
                needs_precedence = .true.
                return
            end if
        end if
        
    end function needs_precedence_parentheses
    
    ! Analyze unused variables from source text
    subroutine analyze_unused_variables_from_text(source_text, violations, violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        character(len=:), allocatable :: declared_vars(:)
        character(len=:), allocatable :: used_vars(:)
        integer :: declared_count, used_count
        integer :: pos, next_pos, line_num
        integer :: line_start, line_end
        type(source_range_t) :: location
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_trimmed
        character(len=:), allocatable :: line_lower
        integer :: i, j
        logical :: found
        logical :: in_procedure
        
        allocate(character(len=64) :: declared_vars(500))
        allocate(character(len=64) :: used_vars(500))
        declared_count = 0
        used_count = 0
        pos = 1
        line_num = 0
        in_procedure = .false.
        
        ! First pass: collect declarations and usages
        do while (pos <= len(source_text))
            line_num = line_num + 1
            
            ! Find end of line
            next_pos = index(source_text(pos:), char(10))
            if (next_pos == 0) then
                line_start = pos
                line_end = len(source_text)
            else
                line_start = pos
                line_end = pos + next_pos - 2
                pos = pos + next_pos
            end if
            
            ! Extract line content
            if (line_end >= line_start) then
                line_content = source_text(line_start:line_end)
                
                ! Skip comment lines
                if (.not. is_comment_line(line_content)) then
                    line_trimmed = adjustl(line_content)
                    line_lower = to_lower(line_trimmed)
                    
                    ! Check if entering/leaving procedure
                    if (index(line_lower, 'subroutine ') == 1 .or. &
                        index(line_lower, 'function ') == 1 .or. &
                        index(line_lower, 'program ') == 1) then
                        in_procedure = .true.
                    else if (index(line_lower, 'end subroutine') == 1 .or. &
                             index(line_lower, 'end function') == 1 .or. &
                             index(line_lower, 'end program') == 1) then
                        in_procedure = .false.
                    end if
                    
                    ! Check for variable declarations
                    if (index(line_trimmed, '::') > 0) then
                        if (has_type_keyword(line_lower(1:index(line_trimmed, '::')))) then
                            call extract_variable_names(line_trimmed(index(line_trimmed, '::')+2:), &
                                                       declared_vars, declared_count)
                        end if
                    end if
                    
                    ! Check for variable usage (simplified - looks for identifiers in assignments and calls)
                    if (in_procedure) then
                        call extract_used_variables(line_trimmed, used_vars, used_count, declared_vars, declared_count)
                    end if
                end if
            end if
            
            ! Move to next line if not at end
            if (next_pos == 0) exit
        end do
        
        ! Check for unused variables
        do i = 1, declared_count
            found = .false.
            do j = 1, used_count
                if (trim(declared_vars(i)) == trim(used_vars(j))) then
                    found = .true.
                    exit
                end if
            end do
            
            if (.not. found) then
                violation_count = violation_count + 1
                if (violation_count <= size(violations)) then
                    location%start%line = 1
                    location%start%column = 1
                    location%end%line = 1
                    location%end%column = 1
                    violations(violation_count) = create_diagnostic( &
                        code="F006", &
                        message="Unused variable: " // trim(declared_vars(i)), &
                        file_path=current_filename, &
                        location=location, &
                        severity=SEVERITY_WARNING)
                end if
            end if
        end do
        
    end subroutine analyze_unused_variables_from_text
    
    ! Extract used variables from a line
    subroutine extract_used_variables(line, used_vars, used_count, declared_vars, declared_count)
        character(len=*), intent(in) :: line
        character(len=*), intent(inout) :: used_vars(:)
        integer, intent(inout) :: used_count
        character(len=*), intent(in) :: declared_vars(:)
        integer, intent(in) :: declared_count
        
        character(len=:), allocatable :: working_line
        integer :: i, j, start_pos, end_pos
        character(len=:), allocatable :: token
        logical :: already_tracked
        
        working_line = line
        
        ! Remove string literals
        call remove_string_literals(working_line)
        
        ! Look for identifiers that match declared variables
        do i = 1, declared_count
            if (index(working_line, trim(declared_vars(i))) > 0) then
                ! Check if already tracked
                already_tracked = .false.
                do j = 1, used_count
                    if (trim(used_vars(j)) == trim(declared_vars(i))) then
                        already_tracked = .true.
                        exit
                    end if
                end do
                
                if (.not. already_tracked) then
                    used_count = used_count + 1
                    if (used_count <= size(used_vars)) then
                        used_vars(used_count) = declared_vars(i)
                    end if
                end if
            end if
        end do
        
    end subroutine extract_used_variables
    
    ! Remove string literals from line
    subroutine remove_string_literals(line)
        character(len=:), allocatable, intent(inout) :: line
        integer :: i, quote_start
        logical :: in_string
        character :: quote_char
        
        in_string = .false.
        i = 1
        
        do while (i <= len(line))
            if (.not. in_string) then
                if (line(i:i) == '"' .or. line(i:i) == "'") then
                    in_string = .true.
                    quote_char = line(i:i)
                    quote_start = i
                end if
            else
                if (line(i:i) == quote_char) then
                    ! Replace string content with spaces
                    line(quote_start:i) = repeat(' ', i - quote_start + 1)
                    in_string = .false.
                end if
            end if
            i = i + 1
        end do
        
    end subroutine remove_string_literals
    
    subroutine check_line_indentation(line, line_num, violations, violation_count)
        character(len=*), intent(in) :: line
        integer, intent(in) :: line_num
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer :: spaces, tabs
        integer :: i
        type(source_range_t) :: location
        
        spaces = 0
        tabs = 0
        
        ! Count leading spaces and tabs
        do i = 1, len(line)
            if (line(i:i) == ' ') then
                spaces = spaces + 1
            else if (line(i:i) == char(9)) then  ! tab character
                tabs = tabs + 1
            else
                exit
            end if
        end do
        
        ! Report mixed tabs and spaces
        if (spaces > 0 .and. tabs > 0) then
            violation_count = violation_count + 1
            if (violation_count <= size(violations)) then
                ! Create a basic location - line info not available in this context
                ! location = create_source_range(line_num, 1, line_num, spaces + tabs)
                violations(violation_count) = create_diagnostic( &
                    code="F002", &
                    message="Mixed tabs and spaces in indentation", &
                    file_path="", &
                    location=location, &
                    severity=SEVERITY_WARNING)
            end if
        end if
    end subroutine check_line_indentation
    
    subroutine check_indentation_consistency(indent_levels, line_count, violations, violation_count)
        integer, intent(in) :: indent_levels(:)
        integer, intent(in) :: line_count
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer :: i, j
        integer :: common_indent
        logical :: has_inconsistency
        type(source_range_t) :: location
        
        if (line_count < 2) return
        
        ! Find inconsistent indentation patterns
        has_inconsistency = .false.
        
        ! Check if indentation levels are consistent multiples
        do i = 1, line_count - 1
            do j = i + 1, line_count
                if (indent_levels(i) > 0 .and. indent_levels(j) > 0) then
                    ! Check if one is not a multiple of the other
                    if (indent_levels(i) /= indent_levels(j)) then
                        ! Different levels - check if they're consistent
                        if (mod(indent_levels(i), 4) /= mod(indent_levels(j), 4)) then
                            has_inconsistency = .true.
                            exit
                        end if
                    end if
                end if
            end do
            if (has_inconsistency) exit
        end do
        
        if (has_inconsistency) then
            violation_count = violation_count + 1
            if (violation_count <= size(violations)) then
                ! Create a basic location - will be updated later
                ! location = create_source_range(1, 1, 1, 1)
                violations(violation_count) = create_diagnostic( &
                    code="F002", &
                    message="Inconsistent indentation levels detected", &
                    file_path="", &
                    location=location, &
                    severity=SEVERITY_WARNING)
            end if
        end if
    end subroutine check_indentation_consistency
    
    ! F003: Check line length with AST context awareness
    subroutine check_f003_line_length(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        integer, parameter :: MAX_LINE_LENGTH = 88  ! Following Black/Ruff standard
        
        ! Initialize
        allocate(temp_violations(100))
        violation_count = 0
        
        ! Use current_source_text to check line lengths with AST context
        if (.not. allocated(current_source_text)) then
            ! No source text available
            allocate(violations(0))
            return
        end if
        
        call analyze_line_lengths_with_ast_context(ctx, current_source_text, temp_violations, violation_count, MAX_LINE_LENGTH)
        
        ! Allocate result
        allocate(violations(violation_count))
        violations(1:violation_count) = temp_violations(1:violation_count)
        
    end subroutine check_f003_line_length
    
    ! F004: Check trailing whitespace
    subroutine check_f004_trailing_whitespace(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Initialize
        allocate(temp_violations(100))
        violation_count = 0
        
        ! Use current_source_text to check trailing whitespace
        if (.not. allocated(current_source_text)) then
            ! No source text available
            allocate(violations(0))
            return
        end if
        
        call analyze_trailing_whitespace_from_text(current_source_text, temp_violations, violation_count)
        
        ! Allocate and copy violations
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_f004_trailing_whitespace
    
    ! F005: Check mixed tabs and spaces
    subroutine check_f005_mixed_tabs_spaces(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Initialize
        allocate(temp_violations(100))
        violation_count = 0
        
        ! Use current_source_text to check mixed tabs and spaces
        if (.not. allocated(current_source_text)) then
            ! No source text available
            allocate(violations(0))
            return
        end if
        
        call analyze_mixed_tabs_spaces_from_text(current_source_text, temp_violations, violation_count)
        
        ! Allocate and copy violations
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_f005_mixed_tabs_spaces
    
    ! F006: Check unused variable declarations
    subroutine check_f006_unused_variable(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        character(len=:), allocatable :: unused_vars(:)
        logical :: success
        integer :: i, violation_count
        type(source_range_t) :: location
        type(semantic_context_t) :: ctx_copy
        logical :: is_intrinsic
        
        violation_count = 0
        allocate(violations(100))  ! Pre-allocate reasonable size
        
        ! Make a copy of the semantic context to allow modifications
        ctx_copy = ctx%semantic_ctx
        
        ! Get unused variables in current function/subroutine scope using direct API
        success = get_unused_variables_direct(ctx%arena, ctx_copy, SCOPE_FUNCTION, unused_vars)
        if (.not. success) then
            success = get_unused_variables_direct(ctx%arena, ctx_copy, SCOPE_SUBROUTINE, unused_vars)
        end if
        
        if (success .and. size(unused_vars) > 0) then
            do i = 1, min(size(unused_vars), size(violations))
                if (len_trim(unused_vars(i)) > 0) then
                    ! Skip intrinsic functions
                    is_intrinsic = .false.
                    select case(trim(unused_vars(i)))
                    case ("sin", "cos", "tan", "sqrt", "exp", "log", "abs", &
                          "asin", "acos", "atan", "atan2", "sinh", "cosh", "tanh", &
                          "int", "real", "nint", "floor", "ceiling", &
                          "min", "max", "mod", "modulo", "sign", &
                          "len", "len_trim", "trim", "adjustl", "adjustr", &
                          "size", "shape", "sum", "product", "maxval", "minval", &
                          "allocated", "present", "associated")
                        is_intrinsic = .true.
                    end select
                    
                    if (.not. is_intrinsic) then
                        violation_count = violation_count + 1
                        
                        ! Create basic location (would need node lookup for exact position)
                        location%start%line = 1
                        location%start%column = 1
                        location%end%line = 1
                        location%end%column = len_trim(unused_vars(i))
                        
                        violations(violation_count) = create_diagnostic( &
                            code="F006", &
                            message="Unused variable: " // trim(unused_vars(i)), &
                            file_path=current_filename, &
                            location=location, &
                            severity=SEVERITY_WARNING)
                    end if
                end if
            end do
        end if
        
        ! Resize violations array to actual count
        if (violation_count == 0) then
            deallocate(violations)
            allocate(violations(0))
        else
            block
                type(diagnostic_t), allocatable :: temp_violations(:)
                allocate(temp_violations(violation_count))
                temp_violations = violations(1:violation_count)
                call move_alloc(temp_violations, violations)
            end block
        end if
        
    end subroutine check_f006_unused_variable
    
    ! Removed old helper functions - now using fortfront APIs directly
    
    ! Create a source range helper
    function create_range(start_line, start_col, end_line, end_col) result(range)
        integer, intent(in) :: start_line, start_col, end_line, end_col
        type(source_range_t) :: range
        
        range%start%line = start_line
        range%start%column = start_col
        range%end%line = end_line
        range%end%column = end_col
        
    end function create_range
    
    ! Helper subroutine to grow variable name arrays
    subroutine grow_var_array(var_array, new_size)
        character(len=256), allocatable, intent(inout) :: var_array(:)
        integer, intent(in) :: new_size
        
        character(len=256), allocatable :: temp_array(:)
        integer :: old_size
        
        old_size = size(var_array)
        if (new_size <= old_size) return
        
        ! Save old data
        allocate(temp_array(old_size))
        temp_array = var_array
        
        ! Resize array
        deallocate(var_array)
        allocate(var_array(new_size))
        
        ! Copy back old data
        var_array(1:old_size) = temp_array
        
        deallocate(temp_array)
        
    end subroutine grow_var_array
    
    ! Helper subroutine to grow violation arrays
    subroutine grow_violation_array(violation_array, new_size)
        type(diagnostic_t), allocatable, intent(inout) :: violation_array(:)
        integer, intent(in) :: new_size
        
        type(diagnostic_t), allocatable :: temp_array(:)
        integer :: old_size
        
        old_size = size(violation_array)
        if (new_size <= old_size) return
        
        ! Save old data
        allocate(temp_array(old_size))
        temp_array = violation_array
        
        ! Resize array
        deallocate(violation_array)
        allocate(violation_array(new_size))
        
        ! Copy back old data
        violation_array(1:old_size) = temp_array
        
        deallocate(temp_array)
        
    end subroutine grow_violation_array
    
    ! F007: Check undefined variable usage
    subroutine check_f007_undefined_variable(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:), all_violations(:)
        integer :: violation_count
        
        ! Initialize with reasonable pre-allocation
        violation_count = 0
        allocate(all_violations(100))
        
        ! Traverse the AST starting from node_index to find all identifiers
        call check_identifiers_recursive(ctx, node_index, all_violations, violation_count)
        
        ! Resize to actual count
        if (violation_count == 0) then
            deallocate(all_violations)
            allocate(all_violations(0))
        else if (violation_count < size(all_violations)) then
            allocate(temp_violations(violation_count))
            temp_violations = all_violations(1:violation_count)
            call move_alloc(temp_violations, all_violations)
        end if
        
        ! Return violations
        violations = all_violations
        
    end subroutine check_f007_undefined_variable
    
    ! Recursive helper to check all identifiers in the AST
    recursive subroutine check_identifiers_recursive(ctx, node_index, violations, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        character(len=:), allocatable :: identifier_name
        logical :: is_defined
        type(source_range_t) :: location
        type(semantic_context_t) :: ctx_copy
        integer, allocatable :: children(:)
        integer :: i
        type(diagnostic_t), allocatable :: temp_violations(:)
        
        if (node_index <= 0 .or. node_index > ctx%arena%size) return
        
        ! Check if this node is an identifier
        if (ctx%arena%entries(node_index)%node_type == "identifier") then
            ! Get identifier name
            select type (node => ctx%arena%entries(node_index)%node)
            type is (identifier_node)
                if (allocated(node%name)) then
                    identifier_name = node%name
                    
                    ! Make a copy of the semantic context to allow modifications
                    ctx_copy = ctx%semantic_ctx
                    
                    ! Check if identifier is defined using direct API
                    is_defined = is_identifier_defined_direct(ctx%arena, ctx_copy, identifier_name)
                    
                    if (.not. is_defined) then
                        ! Skip intrinsic functions
                        select case(trim(identifier_name))
                        case ("sin", "cos", "tan", "sqrt", "exp", "log", "abs", &
                              "asin", "acos", "atan", "atan2", "sinh", "cosh", "tanh", &
                              "int", "real", "nint", "floor", "ceiling", &
                              "min", "max", "mod", "modulo", "sign", &
                              "len", "len_trim", "trim", "adjustl", "adjustr", &
                              "size", "shape", "sum", "product", "maxval", "minval", &
                              "allocated", "present", "associated", &
                              "print", "write", "read", "open", "close")
                            ! Skip intrinsics and I/O statements
                        case default
                            ! Add violation
                            violation_count = violation_count + 1
                            
                            ! Create location from node
                            location%start%line = node%line
                            location%start%column = node%column
                            location%end%line = node%line
                            location%end%column = node%column + len(identifier_name) - 1
                            
                            ! Grow violations array if needed
                            if (violation_count > size(violations)) then
                                allocate(temp_violations(size(violations) + 10))
                                temp_violations(1:size(violations)) = violations
                                call move_alloc(temp_violations, violations)
                            end if
                            
                            violations(violation_count) = create_diagnostic( &
                                code="F007", &
                                message="Undefined variable: " // identifier_name, &
                                file_path=current_filename, &
                                location=location, &
                                severity=SEVERITY_ERROR)
                        end select
                    end if
                end if
            end select
        end if
        
        ! Recursively check children
        children = ctx%arena%get_children(node_index)
        if (allocated(children)) then
            do i = 1, size(children)
                call check_identifiers_recursive(ctx, children(i), violations, violation_count)
            end do
        end if
        
    end subroutine check_identifiers_recursive
    
    ! Recursively check for undefined identifiers
    ! Removed helper functions - waiting for semantic_query_t to be exported
    
    ! F008: Check missing intent declarations
    subroutine check_f008_missing_intent(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Use source text analysis for now
        if (.not. allocated(current_source_text)) then
            allocate(violations(0))
            return
        end if
        
        ! Initialize
        allocate(temp_violations(100))
        violation_count = 0
        
        ! Analyze source text for missing intent
        call analyze_missing_intent_from_text(current_source_text, temp_violations, violation_count)
        
        ! Allocate and copy violations
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_f008_missing_intent
    
    ! Recursive helper to check intent declarations
    recursive subroutine check_intent_recursive(ctx, node_index, violations, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer, allocatable :: children(:)
        integer :: node_type
        integer :: i, num_children
        
        ! Get node type
        node_type = ctx%get_node_type(node_index)
        
        ! Check if this is a procedure definition
        if (node_type == NODE_FUNCTION_DEF .or. node_type == NODE_SUBROUTINE_DEF) then
            ! Check arguments for intent
            call check_procedure_arguments_intent(ctx, node_index, violations, violation_count)
        end if
        
        ! Recursively process children
        children = ctx%get_children(node_index)
        num_children = size(children)
        
        do i = 1, num_children
            if (children(i) > 0) then
                call check_intent_recursive(ctx, children(i), violations, violation_count)
            end if
        end do
        
        if (allocated(children)) deallocate(children)
        
    end subroutine check_intent_recursive
    
    ! Check procedure arguments for missing intent
    subroutine check_procedure_arguments_intent(ctx, proc_node, violations, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: proc_node
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer, allocatable :: children(:)
        integer :: i, child_type
        character(len=256) :: arg_name
        type(source_range_t) :: location
        type(fix_suggestion_t) :: fix
        type(text_edit_t) :: edit
        
        ! Simplified implementation that checks for variable declarations without intent
        children = ctx%get_children(proc_node)
        
        ! Look for argument declarations without intent
        do i = 1, size(children)
            if (children(i) > 0) then
                child_type = ctx%get_node_type(children(i))
                
                ! Check if this looks like a variable declaration without intent
                if (child_type == NODE_DECLARATION) then
                    ! TODO: Need API to check if declaration has intent
                    ! For now, skip this check
                    if (.false.) then
                        location = ctx%get_node_location(children(i))
                        violation_count = violation_count + 1
                        
                        if (violation_count <= size(violations)) then
                            violations(violation_count) = create_diagnostic( &
                                code="F008", &
                                message="Missing intent declaration for procedure argument", &
                                file_path="", &
                                location=location, &
                                severity=SEVERITY_WARNING)
                            
                            ! Generate fix suggestion to add intent(in)
                            fix%description = "Add 'intent(in)' attribute"
                            fix%is_safe = .false.  ! Might change semantics
                            
                            ! Create text edit to add intent before the type
                            edit%range%start%line = location%start%line
                            edit%range%start%column = location%start%column
                            edit%range%end%line = location%start%line
                            edit%range%end%column = location%start%column
                            edit%new_text = ", intent(in)"
                            
                            ! Attach edit to fix
                            allocate(fix%edits(1))
                            fix%edits(1) = edit
                            
                            ! Attach fix to diagnostic
                            allocate(violations(violation_count)%fixes(1))
                            violations(violation_count)%fixes(1) = fix
                        end if
                    end if
                end if
            end if
        end do
        
        if (allocated(children)) deallocate(children)
        
    end subroutine check_procedure_arguments_intent
    
    ! F009: Check inconsistent intent usage
    subroutine check_f009_inconsistent_intent(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! BLOCKED: Requires fortfront AST API (issues #11-14)
        allocate(violations(0))
        
    end subroutine check_f009_inconsistent_intent
    
    ! F010: Check obsolete language features
    subroutine check_f010_obsolete_features(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Initialize
        allocate(temp_violations(100))
        violation_count = 0
        
        ! Use current_source_text to check for obsolete features
        if (.not. allocated(current_source_text)) then
            ! No source text available
            allocate(violations(0))
            return
        end if
        
        call analyze_obsolete_features_from_text(current_source_text, temp_violations, violation_count)
        
        ! Allocate and copy violations
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_f010_obsolete_features
    
    ! F011: Check missing end statement labels
    subroutine check_f011_missing_end_labels(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Use source text analysis
        if (.not. allocated(current_source_text)) then
            allocate(violations(0))
            return
        end if
        
        ! Initialize
        allocate(temp_violations(100))
        violation_count = 0
        
        ! Analyze source text for missing end labels
        call analyze_missing_end_labels_from_text(current_source_text, temp_violations, violation_count)
        
        ! Allocate and copy violations
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_f011_missing_end_labels
    
    ! F012: Check naming conventions
    subroutine check_f012_naming_conventions(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Use source text analysis
        if (.not. allocated(current_source_text)) then
            allocate(violations(0))
            return
        end if
        
        ! Initialize
        allocate(temp_violations(100))
        violation_count = 0
        
        ! Analyze source text for naming conventions
        call analyze_naming_conventions_from_text(current_source_text, temp_violations, violation_count)
        
        ! Allocate and copy violations
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_f012_naming_conventions
    
    ! F013: Check multiple statements per line
    subroutine check_f013_multiple_statements(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Initialize
        allocate(temp_violations(100))
        violation_count = 0
        
        ! Use current_source_text to check for multiple statements
        if (.not. allocated(current_source_text)) then
            ! No source text available
            allocate(violations(0))
            return
        end if
        
        call analyze_multiple_statements_from_text(current_source_text, temp_violations, violation_count)
        
        ! Allocate and copy violations
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_f013_multiple_statements
    
    ! F014: Check unnecessary parentheses
    subroutine check_f014_unnecessary_parentheses(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Use source text analysis for simple cases
        if (.not. allocated(current_source_text)) then
            allocate(violations(0))
            return
        end if
        
        ! Initialize
        allocate(temp_violations(100))
        violation_count = 0
        
        ! Analyze source text for unnecessary parentheses
        call analyze_unnecessary_parentheses_from_text(current_source_text, temp_violations, violation_count)
        
        ! Allocate and copy violations
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_f014_unnecessary_parentheses
    
    ! F015: Check redundant continue statements
    subroutine check_f015_redundant_continue(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Initialize
        allocate(temp_violations(100))
        violation_count = 0
        
        ! Use current_source_text to check for redundant continue
        if (.not. allocated(current_source_text)) then
            ! No source text available
            allocate(violations(0))
            return
        end if
        
        call analyze_redundant_continue_from_text(current_source_text, temp_violations, violation_count)
        
        ! Allocate and copy violations
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_f015_redundant_continue
    
    ! P001: Check array access patterns
    subroutine check_p001_array_access(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! Use fortfront AST to analyze array access patterns
        call check_p001_array_access_ast_based(ctx, node_index, violations)
        
    end subroutine check_p001_array_access
    
    ! P002: Check loop ordering efficiency
    subroutine check_p002_loop_ordering(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! Use fortfront AST to analyze loop ordering
        call check_p002_loop_ordering_ast_based(ctx, node_index, violations)
        
    end subroutine check_p002_loop_ordering
    
    ! P003: Check array temporaries
    subroutine check_p003_array_temporaries(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! BLOCKED: Requires fortfront AST API (issues #11-14) array temporaries check
        allocate(violations(0))
        
    end subroutine check_p003_array_temporaries
    
    ! P004: Check pure/elemental declarations
    subroutine check_p004_pure_elemental(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! Use fortfront AST to analyze pure/elemental declarations
        call check_p004_pure_elemental_ast_based(ctx, node_index, violations)
        
    end subroutine check_p004_pure_elemental
    
    ! P005: Check string operations efficiency
    subroutine check_p005_string_operations(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! BLOCKED: Requires fortfront AST API (issues #11-14) string operations efficiency check
        allocate(violations(0))
        
    end subroutine check_p005_string_operations
    
    ! P006: Check loop allocations
    subroutine check_p006_loop_allocations(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! BLOCKED: Requires fortfront AST API (issues #11-14) loop allocations check
        allocate(violations(0))
        
    end subroutine check_p006_loop_allocations
    
    ! P007: Check mixed precision arithmetic
    subroutine check_p007_mixed_precision(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! BLOCKED: Requires fortfront AST API (issues #11-14) mixed precision arithmetic check
        allocate(violations(0))
        
    end subroutine check_p007_mixed_precision
    
    ! C001: Check for undefined variables
    subroutine check_c001_undefined_var(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! BLOCKED: Requires fortfront AST API (issues #11-14) undefined variable check
        allocate(violations(0))
        
    end subroutine check_c001_undefined_var
    
    ! AST-BASED IMPLEMENTATIONS using fortfront
    
    ! F001: Check for missing implicit none using AST
    subroutine check_f001_implicit_none_ast_based(ctx, node_index, violations)
        use fortfront, only: ast_arena_t, program_node, module_node, &
                            subroutine_def_node, function_def_node, &
                            get_node_type_id_from_arena
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index  
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        logical :: found_implicit_none
        integer :: i, node_type
        type(fix_suggestion_t) :: fix
        type(text_edit_t) :: edit
        type(source_range_t) :: location
        
        ! Initialize
        allocate(temp_violations(10))
        violation_count = 0
        
        node_type = ctx%get_node_type(node_index)
        
        ! Check if this is a program unit that needs implicit none
        ! Be permissive and check any node that might contain program units
        if (node_type == NODE_PROGRAM .or. node_type == NODE_MODULE .or. &
            node_type == NODE_FUNCTION_DEF .or. node_type == NODE_SUBROUTINE_DEF .or. &
            node_type == NODE_DECLARATION .or. node_type == NODE_UNKNOWN .or. &
            node_index == 1) then  ! Root node check
            
            ! For now, use simplified text-based check
            if (allocated(current_source_text)) then
                found_implicit_none = index(current_source_text, "implicit") > 0 .and. &
                                    index(current_source_text, "none") > 0
                
                if (.not. found_implicit_none) then
                    location = ctx%get_node_location(node_index)
                    violation_count = violation_count + 1
                    if (violation_count <= size(temp_violations)) then
                        temp_violations(violation_count) = create_diagnostic( &
                            code="F001", &
                            message="Missing 'implicit none' statement", &
                            file_path="", &
                            location=location, &
                            severity=SEVERITY_WARNING)
                        
                        ! Generate fix suggestion
                        fix%description = "Add 'implicit none' statement"
                        fix%is_safe = .true.
                        
                        ! Create text edit to insert implicit none
                        edit%range%start%line = location%start%line + 1
                        edit%range%start%column = 1
                        edit%range%end%line = location%start%line + 1
                        edit%range%end%column = 1
                        edit%new_text = "    implicit none" // new_line('a')
                        
                        ! Attach the edit to the fix
                        allocate(fix%edits(1))
                        fix%edits(1) = edit
                        
                        ! Attach the fix to the diagnostic
                        allocate(temp_violations(violation_count)%fixes(1))
                        temp_violations(violation_count)%fixes(1) = fix
                    end if
                end if
            end if
        end if
        
        ! Allocate result
        allocate(violations(violation_count))
        do i = 1, violation_count
            violations(i) = temp_violations(i)
        end do
        
    end subroutine check_f001_implicit_none_ast_based
    
    ! Check if a node type needs implicit none
    function needs_implicit_none(ctx, node_index) result(needs)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        logical :: needs
        
        integer :: node_type
        
        node_type = ctx%get_node_type(node_index)
        
        ! Program units that should have implicit none
        ! Note: The root node might be a declaration that contains a program
        needs = node_type == NODE_PROGRAM .or. &
               node_type == NODE_MODULE .or. &
               node_type == NODE_FUNCTION_DEF .or. &
               node_type == NODE_SUBROUTINE_DEF .or. &
               node_type == NODE_DECLARATION
        
    end function needs_implicit_none
    
    ! Find implicit none statement in scope
    function find_implicit_none_in_scope(ctx, scope_index) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_index
        logical :: found
        
        integer, allocatable :: children(:)
        integer :: i, child_type
        
        found = .false.
        children = ctx%get_children(scope_index)
        
        ! Look through immediate children for implicit none
        do i = 1, size(children)
            if (children(i) > 0) then
                child_type = ctx%get_node_type(children(i))
                ! Check if this child is an implicit none statement
                if (is_implicit_none_statement(ctx, children(i))) then
                    found = .true.
                    exit
                end if
            end if
        end do
        
        if (allocated(children)) deallocate(children)
        
    end function find_implicit_none_in_scope
    
    ! Check if node is an implicit none statement
    function is_implicit_none_statement(ctx, node_index) result(is_implicit)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        logical :: is_implicit
        
        integer :: node_type
        integer, allocatable :: children(:)
        integer :: i
        
        is_implicit = .false.
        node_type = ctx%get_node_type(node_index)
        
        ! Check if this is a declaration node for implicit statement
        if (node_type == NODE_DECLARATION) then
            ! For now, assume implicit none detection works via text-based analysis
            ! This is a simplified implementation that works with current limitations
            ! Real implementation would need semantic context from fortfront
            is_implicit = check_implicit_none_in_current_source(ctx)
        end if
        
    end function is_implicit_none_statement
    
    ! P001: Check array access patterns using AST
    subroutine check_p001_array_access_ast_based(ctx, node_index, violations)
        use fortfront, only: get_identifiers_in_subtree
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Initialize
        allocate(temp_violations(50))
        violation_count = 0
        
        ! Analyze array access patterns recursively
        call analyze_array_access_patterns(ctx, node_index, temp_violations, violation_count)
        
        ! Allocate result
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_p001_array_access_ast_based
    
    ! Analyze array access patterns for memory efficiency
    recursive subroutine analyze_array_access_patterns(ctx, node_index, violations, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer, allocatable :: children(:)
        integer :: node_type, i
        
        node_type = ctx%get_node_type(node_index)
        
        ! Check for non-contiguous array access in loops
        if (node_type == NODE_DO_LOOP) then
            call check_loop_array_access(ctx, node_index, violations, violation_count)
        end if
        
        ! Process children recursively
        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) > 0) then
                call analyze_array_access_patterns(ctx, children(i), violations, violation_count)
            end if
        end do
        
        if (allocated(children)) deallocate(children)
        
    end subroutine analyze_array_access_patterns
    
    ! Check array access patterns within loops
    subroutine check_loop_array_access(ctx, loop_node, violations, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: loop_node
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        ! Simple implementation for now - check if this looks like a nested loop structure
        if (has_array_like_accesses(ctx, loop_node)) then
            if (violation_count < size(violations)) then
                violation_count = violation_count + 1
                violations(violation_count) = create_diagnostic( &
                    code="P001", &
                    message="Consider memory-efficient array access patterns", &
                    file_path="", &
                    location=ctx%get_node_location(loop_node), &
                    severity=SEVERITY_INFO)
            end if
        end if
        
    end subroutine check_loop_array_access
    
    ! Simple heuristic to detect array-like access patterns
    function has_array_like_accesses(ctx, node_index) result(has_arrays)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        logical :: has_arrays
        
        integer, allocatable :: children(:)
        integer :: i, child_type
        
        has_arrays = .false.
        children = ctx%get_children(node_index)
        
        ! Simple heuristic: if we find multiple children, assume there might be array access
        if (size(children) > 2) then
            has_arrays = .true.
        end if
        
        if (allocated(children)) deallocate(children)
        
    end function has_array_like_accesses
    
    ! P002: Check loop ordering efficiency using AST
    subroutine check_p002_loop_ordering_ast_based(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Initialize
        allocate(temp_violations(20))
        violation_count = 0
        
        ! Analyze nested loops for optimal ordering
        call analyze_nested_loops(ctx, node_index, temp_violations, violation_count)
        
        ! Allocate result
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_p002_loop_ordering_ast_based
    
    ! Analyze nested loops for memory-efficient ordering
    recursive subroutine analyze_nested_loops(ctx, node_index, violations, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer, allocatable :: children(:)
        integer :: node_type, i, nested_loop_count
        
        node_type = ctx%get_node_type(node_index)
        
        ! Check for nested loops
        if (node_type == NODE_DO_LOOP) then
            nested_loop_count = count_nested_loops(ctx, node_index)
            if (nested_loop_count > 1) then
                ! This is a simplified heuristic - in practice would analyze array indexing patterns
                if (violation_count < size(violations)) then
                    violation_count = violation_count + 1
                    violations(violation_count) = create_diagnostic( &
                        code="P002", &
                        message="Consider loop ordering for memory efficiency (innermost loop should access contiguous memory)", &
                        file_path="", &
                        location=ctx%get_node_location(node_index), &
                        severity=SEVERITY_INFO)
                end if
            end if
        end if
        
        ! Process children recursively
        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) > 0) then
                call analyze_nested_loops(ctx, children(i), violations, violation_count)
            end if
        end do
        
        if (allocated(children)) deallocate(children)
        
    end subroutine analyze_nested_loops
    
    ! Count nested loops within a loop
    recursive function count_nested_loops(ctx, loop_node) result(count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: loop_node
        integer :: count
        
        integer, allocatable :: children(:)
        integer :: i, node_type
        
        count = 0
        children = ctx%get_children(loop_node)
        
        do i = 1, size(children)
            if (children(i) > 0) then
                node_type = ctx%get_node_type(children(i))
                if (node_type == NODE_DO_LOOP) then
                    count = count + 1 + count_nested_loops(ctx, children(i))
                end if
            end if
        end do
        
        if (allocated(children)) deallocate(children)
        
    end function count_nested_loops
    
    ! P004: Check pure/elemental declarations using AST
    subroutine check_p004_pure_elemental_ast_based(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        
        ! Initialize
        allocate(temp_violations(20))
        violation_count = 0
        
        ! Analyze procedures for pure/elemental opportunities
        call analyze_procedure_purity(ctx, node_index, temp_violations, violation_count)
        
        ! Allocate result
        allocate(violations(violation_count))
        if (violation_count > 0) then
            violations(1:violation_count) = temp_violations(1:violation_count)
        end if
        
    end subroutine check_p004_pure_elemental_ast_based
    
    ! Analyze procedures for pure/elemental opportunities
    recursive subroutine analyze_procedure_purity(ctx, node_index, violations, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        
        integer, allocatable :: children(:)
        integer :: node_type, i
        character(len=256) :: node_text
        
        node_type = ctx%get_node_type(node_index)
        
        ! Check functions and subroutines for pure/elemental opportunities
        if (node_type == NODE_FUNCTION_DEF .or. node_type == NODE_SUBROUTINE_DEF) then
            ! TODO: Need API to check procedure attributes and analyze purity
            ! For now, skip this check
            if (.false.) then
                if (violation_count < size(violations)) then
                    violation_count = violation_count + 1
                    violations(violation_count) = create_diagnostic( &
                        code="P004", &
                        message="Consider adding 'pure' attribute for optimization", &
                        file_path="", &
                        location=ctx%get_node_location(node_index), &
                        severity=SEVERITY_INFO)
                    
                    ! Generate fix suggestion to add pure attribute
                    call add_pure_attribute_fix(violations(violation_count), ctx%get_node_location(node_index))
                end if
            end if
        end if
        
        ! Process children recursively
        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) > 0) then
                call analyze_procedure_purity(ctx, children(i), violations, violation_count)
            end if
        end do
        
        if (allocated(children)) deallocate(children)
        
    end subroutine analyze_procedure_purity
    
    ! Simple heuristic to check if procedure could be pure
    function could_be_pure_procedure(ctx, proc_node) result(could_be_pure)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: proc_node
        logical :: could_be_pure
        
        ! TODO: Need fortfront API to analyze procedure purity
        ! For now, return false (conservative)
        could_be_pure = .false.
        
    end function could_be_pure_procedure
    
    ! Helper subroutine to add indentation fix
    subroutine add_indentation_fix(diagnostic, location, actual_indent)
        type(diagnostic_t), intent(inout) :: diagnostic
        type(source_range_t), intent(in) :: location
        integer, intent(in) :: actual_indent
        
        type(fix_suggestion_t) :: fix
        type(text_edit_t) :: edit
        integer :: correct_indent
        character(len=256) :: spaces
        
        ! Calculate correct indentation (round to nearest multiple of 4)
        correct_indent = (actual_indent / 4) * 4
        if (mod(actual_indent, 4) >= 2) then
            correct_indent = correct_indent + 4
        end if
        
        ! Generate spaces string
        if (correct_indent > 0 .and. correct_indent <= 256) then
            spaces = repeat(' ', correct_indent)
        else
            spaces = ""
        end if
        
        ! Create fix suggestion
        fix%description = "Fix indentation to " // trim(adjustl(char(correct_indent/4))) // " levels"
        fix%is_safe = .true.
        
        ! Create text edit to replace indentation
        edit%range%start%line = location%start%line
        edit%range%start%column = 1
        edit%range%end%line = location%start%line
        edit%range%end%column = actual_indent + 1
        edit%new_text = trim(spaces)
        
        ! Attach edit to fix
        allocate(fix%edits(1))
        fix%edits(1) = edit
        
        ! Attach fix to diagnostic
        allocate(diagnostic%fixes(1))
        diagnostic%fixes(1) = fix
        
    end subroutine add_indentation_fix
    
    ! Helper subroutine to add pure attribute fix
    subroutine add_pure_attribute_fix(diagnostic, location)
        type(diagnostic_t), intent(inout) :: diagnostic
        type(source_range_t), intent(in) :: location
        
        type(fix_suggestion_t) :: fix
        type(text_edit_t) :: edit
        
        ! Create fix suggestion to add pure attribute
        fix%description = "Add 'pure' attribute to procedure"
        fix%is_safe = .false.  ! Less certain about semantic correctness
        
        ! Create text edit to add pure before the procedure declaration
        edit%range%start%line = location%start%line
        edit%range%start%column = 1  
        edit%range%end%line = location%start%line
        edit%range%end%column = 1
        edit%new_text = "pure "
        
        ! Attach edit to fix
        allocate(fix%edits(1))
        fix%edits(1) = edit
        
        ! Attach fix to diagnostic
        allocate(diagnostic%fixes(1))
        diagnostic%fixes(1) = fix
        
    end subroutine add_pure_attribute_fix
    
    ! TEMPORARY TEXT-BASED IMPLEMENTATIONS
    ! These will be replaced when fortfront AST API is available
    ! Issues: https://github.com/lazy-fortran/fortfront/issues/11-14
    
    subroutine check_f001_implicit_none_text_based(violations)
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        ! Text-based check for implicit none in common program units
        ! This is a basic implementation that checks if implicit none is present
        integer :: violation_count
        
        ! Simplified: create empty violations array for testing
        violation_count = 0  
        
        ! For now, always pass - real implementation would parse source
        ! In a full implementation, we would:
        ! 1. Check if we're in a program, module, subroutine, or function
        ! 2. Look for "implicit none" statement in the declaration section
        ! 3. Report violation if missing
        
        ! Return empty violations array
        allocate(violations(violation_count))
        
    end subroutine check_f001_implicit_none_text_based
    
    ! Helper subroutine to set current file context
    subroutine set_current_file_context(filename, source_text)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: source_text
        
        current_filename = filename
        current_source_text = source_text
        
    end subroutine set_current_file_context
    
    ! Helper subroutine to convert string to lowercase
    subroutine lowercase_string(str)
        character(len=:), allocatable, intent(inout) :: str
        integer :: i
        character :: c
        
        if (.not. allocated(str)) return
        
        do i = 1, len(str)
            c = str(i:i)
            if (c >= 'A' .and. c <= 'Z') then
                str(i:i) = char(ichar(c) + 32)
            end if
        end do
        
    end subroutine lowercase_string
    
    ! Check if implicit none exists in current source (simplified implementation)
    function check_implicit_none_in_current_source(ctx) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        logical :: found
        character(len=:), allocatable :: source_lower
        
        found = .false.
        
        ! Use module-level source text if available
        if (allocated(current_source_text)) then
            source_lower = current_source_text
            call lowercase_string(source_lower)
            ! Check if implicit none appears anywhere - for standalone subroutines/functions
            ! this is correct behavior as they should have their own implicit none
            found = index(source_lower, "implicit") > 0 .and. index(source_lower, "none") > 0
        end if
        
    end function check_implicit_none_in_current_source
    
end module fluff_rules