module fluff_rules
    use fluff_diagnostics, only: SEVERITY_ERROR, SEVERITY_INFO, SEVERITY_WARNING
    use fluff_rule_c001, only: check_c001_undefined_var
    use fluff_rule_f009, only: check_f009_inconsistent_intent_impl
    use fluff_rule_file_context, only: set_current_file_context
    use fluff_rule_f001, only: check_f001_implicit_none
    use fluff_rule_f002, only: check_f002_indentation
    use fluff_rule_f003, only: check_f003_line_length
    use fluff_rule_f004, only: check_f004_trailing_whitespace
    use fluff_rule_f005, only: check_f005_mixed_tabs_spaces
    use fluff_rule_f006, only: check_f006_unused_variable
    use fluff_rule_f007, only: check_f007_undefined_variable
    use fluff_rule_f008, only: check_f008_missing_intent
    use fluff_rule_f010, only: check_f010_obsolete_features
    use fluff_rule_f011, only: check_f011_missing_end_labels
    use fluff_rule_f012, only: check_f012_naming_conventions
    use fluff_rule_f013, only: check_f013_multiple_statements
    use fluff_rule_f014, only: check_f014_unnecessary_parentheses
    use fluff_rule_f015, only: check_f015_redundant_continue
    use fluff_rule_p001, only: check_p001_array_access
    use fluff_rule_p002, only: check_p002_loop_ordering
    use fluff_rule_p003, only: check_p003_array_temporaries
    use fluff_rule_p004, only: check_p004_pure_elemental
    use fluff_rule_p005, only: check_p005_string_operations
    use fluff_rule_p006, only: check_p006_loop_allocations
    use fluff_rule_p007, only: check_p007_mixed_precision
    use fluff_rule_types, only: rule_info_t
    implicit none
    private

    character(len=*), parameter :: CATEGORY_STYLE = "style"
    character(len=*), parameter :: CATEGORY_PERFORMANCE = "performance"
    character(len=*), parameter :: CATEGORY_CORRECTNESS = "correctness"

    public :: CATEGORY_STYLE, CATEGORY_PERFORMANCE, CATEGORY_CORRECTNESS
    public :: get_all_builtin_rules
    public :: get_style_rules
    public :: get_performance_rules
    public :: get_correctness_rules
    public :: set_current_file_context

contains

    function get_all_builtin_rules() result(rules)
        type(rule_info_t), allocatable :: rules(:)
        type(rule_info_t), allocatable :: style_rules(:)
        type(rule_info_t), allocatable :: perf_rules(:)
        type(rule_info_t), allocatable :: correct_rules(:)

        style_rules = get_style_rules()
        perf_rules = get_performance_rules()
        correct_rules = get_correctness_rules()

        allocate (rules(size(style_rules) + size(perf_rules) + size(correct_rules)))
        rules(1:size(style_rules)) = style_rules
        rules(size(style_rules) + 1:size(style_rules) + size(perf_rules)) = perf_rules
        rules(size(style_rules) + size(perf_rules) + 1:) = correct_rules
    end function get_all_builtin_rules

    function get_style_rules() result(rules)
        type(rule_info_t), allocatable :: rules(:)

        allocate (rules(15))

        call populate_style_rules(rules)
    end function get_style_rules

    function get_performance_rules() result(rules)
        type(rule_info_t), allocatable :: rules(:)

        allocate (rules(7))

        rules(1) = rule_info_t(code="P001", name="non-contiguous-array-access", &
                               description="Non-contiguous array access pattern", &
                               category=CATEGORY_PERFORMANCE, subcategory="memory", &
                               default_enabled=.true., fixable=.false., &
                               severity=SEVERITY_WARNING, &
                               check=check_p001_array_access)

        rules(2) = rule_info_t(code="P002", name="inefficient-loop-ordering", &
                               description="Inefficient loop ordering", &
                               category=CATEGORY_PERFORMANCE, subcategory="memory", &
                               default_enabled=.true., fixable=.false., &
                               severity=SEVERITY_WARNING, &
                               check=check_p002_loop_ordering)

        rules(3) = rule_info_t(code="P003", name="unnecessary-array-temporaries", &
                               description="Unnecessary array temporaries", &
                               category=CATEGORY_PERFORMANCE, subcategory="memory", &
                               default_enabled=.true., fixable=.false., &
                               severity=SEVERITY_WARNING, &
                               check=check_p003_array_temporaries)

        rules(4) = rule_info_t(code="P004", name="missing-pure-elemental", &
                               description="Missing pure and elemental declarations", &
                               category=CATEGORY_PERFORMANCE, &
                               subcategory="optimization", &
                               default_enabled=.true., fixable=.true., &
                               severity=SEVERITY_INFO, &
                               check=check_p004_pure_elemental)

        rules(5) = rule_info_t(code="P005", name="inefficient-string-operations", &
                               description="Inefficient string operations", &
                               category=CATEGORY_PERFORMANCE, subcategory="memory", &
                               default_enabled=.true., fixable=.false., &
                               severity=SEVERITY_WARNING, &
                               check=check_p005_string_operations)

        rules(6) = rule_info_t(code="P006", name="unnecessary-allocations-in-loops", &
                               description="Unnecessary allocations in loops", &
                               category=CATEGORY_PERFORMANCE, subcategory="memory", &
                               default_enabled=.true., fixable=.false., &
                               severity=SEVERITY_WARNING, &
                               check=check_p006_loop_allocations)

        rules(7) = rule_info_t(code="P007", name="mixed-precision-arithmetic", &
                               description="Mixed precision arithmetic", &
                               category=CATEGORY_PERFORMANCE, subcategory="precision", &
                               default_enabled=.true., fixable=.false., &
                               severity=SEVERITY_INFO, &
                               check=check_p007_mixed_precision)
    end function get_performance_rules

    function get_correctness_rules() result(rules)
        type(rule_info_t), allocatable :: rules(:)

        allocate (rules(1))

        rules(1) = rule_info_t(code="C001", name="undefined-variable", &
                               description="Use of undefined variable", &
                               category=CATEGORY_CORRECTNESS, subcategory="semantic", &
                               default_enabled=.true., fixable=.false., &
                               severity=SEVERITY_ERROR, &
                               check=check_c001_undefined_var)
    end function get_correctness_rules

    subroutine populate_style_rules(rules)
        type(rule_info_t), intent(inout) :: rules(:)

        call populate_style_rules_1_5(rules)
        call populate_style_rules_6_10(rules)
        call populate_style_rules_11_15(rules)
    end subroutine populate_style_rules

    subroutine populate_style_rules_1_5(rules)
        type(rule_info_t), intent(inout) :: rules(:)

        rules(1) = rule_info_t(code="F001", name="missing-implicit-none", &
                               description="Missing implicit none statement", &
                               category=CATEGORY_STYLE, subcategory="best-practices", &
                               default_enabled=.true., fixable=.true., &
                               severity=SEVERITY_WARNING, &
                               check=check_f001_implicit_none)

        rules(2) = rule_info_t(code="F002", name="inconsistent-indentation", &
                               description="Inconsistent indentation detected", &
                               category=CATEGORY_STYLE, subcategory="formatting", &
                               default_enabled=.true., fixable=.true., &
                               severity=SEVERITY_WARNING, &
                               check=check_f002_indentation)

        rules(3) = rule_info_t(code="F003", name="line-too-long", &
                               description="Line exceeds maximum length", &
                               category=CATEGORY_STYLE, subcategory="formatting", &
                               default_enabled=.true., fixable=.true., &
                               severity=SEVERITY_WARNING, &
                               check=check_f003_line_length)

        rules(4) = rule_info_t(code="F004", name="trailing-whitespace", &
                               description="Trailing whitespace detected", &
                               category=CATEGORY_STYLE, subcategory="formatting", &
                               default_enabled=.true., fixable=.true., &
                               severity=SEVERITY_WARNING, &
                               check=check_f004_trailing_whitespace)

        rules(5) = rule_info_t(code="F005", name="mixed-tabs-spaces", &
                               description="Mixed tabs and spaces in indentation", &
                               category=CATEGORY_STYLE, subcategory="formatting", &
                               default_enabled=.true., fixable=.true., &
                               severity=SEVERITY_WARNING, &
                               check=check_f005_mixed_tabs_spaces)
    end subroutine populate_style_rules_1_5

    subroutine populate_style_rules_6_10(rules)
        type(rule_info_t), intent(inout) :: rules(:)

        rules(6) = rule_info_t(code="F006", name="unused-variable", &
                               description="Unused variable declaration", &
                               category=CATEGORY_STYLE, subcategory="best-practices", &
                               default_enabled=.true., fixable=.false., &
                               severity=SEVERITY_WARNING, &
                               check=check_f006_unused_variable)

        rules(7) = rule_info_t(code="F007", name="undefined-variable", &
                               description="Undefined variable usage", &
                               category=CATEGORY_STYLE, subcategory="best-practices", &
                               default_enabled=.true., fixable=.false., &
                               severity=SEVERITY_ERROR, &
                               check=check_f007_undefined_variable)

        rules(8) = rule_info_t(code="F008", name="missing-intent", &
                               description="Missing intent declarations", &
                               category=CATEGORY_STYLE, subcategory="best-practices", &
                               default_enabled=.true., fixable=.true., &
                               severity=SEVERITY_WARNING, &
                               check=check_f008_missing_intent)

        rules(9) = rule_info_t(code="F009", name="inconsistent-intent", &
                               description="Inconsistent intent usage", &
                               category=CATEGORY_STYLE, subcategory="best-practices", &
                               default_enabled=.true., fixable=.false., &
                               severity=SEVERITY_ERROR, &
                               check=check_f009_inconsistent_intent_impl)

        rules(10) = rule_info_t(code="F010", name="obsolete-features", &
                                description="Obsolete language features", &
                                category=CATEGORY_STYLE, subcategory="modernization", &
                                default_enabled=.true., fixable=.false., &
                                severity=SEVERITY_WARNING, &
                                check=check_f010_obsolete_features)
    end subroutine populate_style_rules_6_10

    subroutine populate_style_rules_11_15(rules)
        type(rule_info_t), intent(inout) :: rules(:)

        rules(11) = rule_info_t(code="F011", name="missing-end-labels", &
                                description="Missing end statement labels", &
                                category=CATEGORY_STYLE, subcategory="clarity", &
                                default_enabled=.true., fixable=.true., &
                                severity=SEVERITY_INFO, &
                                check=check_f011_missing_end_labels)

        rules(12) = rule_info_t(code="F012", name="inconsistent-naming", &
                                description="Inconsistent naming conventions", &
                                category=CATEGORY_STYLE, subcategory="consistency", &
                                default_enabled=.true., fixable=.false., &
                                severity=SEVERITY_WARNING, &
                                check=check_f012_naming_conventions)

        rules(13) = rule_info_t(code="F013", name="multiple-statements", &
                                description="Multiple statements per line", &
                                category=CATEGORY_STYLE, subcategory="formatting", &
                                default_enabled=.true., fixable=.true., &
                                severity=SEVERITY_WARNING, &
                                check=check_f013_multiple_statements)

        rules(14) = rule_info_t(code="F014", name="unnecessary-parentheses", &
                                description="Unnecessary parentheses", &
                                category=CATEGORY_STYLE, subcategory="simplification", &
                                default_enabled=.true., fixable=.true., &
                                severity=SEVERITY_INFO, &
                                check=check_f014_unnecessary_parentheses)

        rules(15) = rule_info_t(code="F015", name="redundant-continue", &
                                description="Redundant continue statements", &
                                category=CATEGORY_STYLE, subcategory="simplification", &
                                default_enabled=.true., fixable=.true., &
                                severity=SEVERITY_INFO, &
                                check=check_f015_redundant_continue)
    end subroutine populate_style_rules_11_15

end module fluff_rules
