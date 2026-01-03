module test_core_basics
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use fluff_core
    use fluff_diagnostics, only: SEVERITY_ERROR, SEVERITY_WARNING, SEVERITY_INFO, &
        SEVERITY_HINT
    use fluff_rules, only: CATEGORY_STYLE, CATEGORY_PERFORMANCE, CATEGORY_CORRECTNESS
    implicit none
    private
    
    public :: collect_core_basics
    
contains
    
    !> Collect all tests
    subroutine collect_core_basics(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        
        testsuite = [ &
            new_unittest("source_location", test_source_location), &
            new_unittest("source_range", test_source_range), &
            new_unittest("severity_levels", test_severity_levels), &
            new_unittest("rule_categories", test_rule_categories) &
        ]
        
    end subroutine collect_core_basics
    
    subroutine test_source_location(error)
        type(error_type), allocatable, intent(out) :: error
        type(source_location_t) :: loc
        
        loc%line = 10
        loc%column = 5
        
        call check(error, loc%line == 10, "Line number should be 10")
        if (allocated(error)) return
        
        call check(error, loc%column == 5, "Column number should be 5")
        
    end subroutine test_source_location
    
    subroutine test_source_range(error)
        type(error_type), allocatable, intent(out) :: error
        type(source_range_t) :: range
        
        range%start%line = 1
        range%start%column = 1
        range%end%line = 2
        range%end%column = 10
        
        call check(error, range%start%line == 1, "Start line should be 1")
        if (allocated(error)) return
        
        call check(error, range%end%column == 10, "End column should be 10")
        
    end subroutine test_source_range
    
    subroutine test_severity_levels(error)
        type(error_type), allocatable, intent(out) :: error
        
        call check(error, SEVERITY_ERROR > SEVERITY_WARNING, &
            "Error severity should be higher than warning")
        if (allocated(error)) return
        
        call check(error, SEVERITY_WARNING > SEVERITY_INFO, &
            "Warning severity should be higher than info")
        if (allocated(error)) return
        
        call check(error, SEVERITY_INFO > SEVERITY_HINT, &
            "Info severity should be higher than hint")
        
    end subroutine test_severity_levels
    
    subroutine test_rule_categories(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, CATEGORY_STYLE == "style", "Style category constant")
        if (allocated(error)) return

        call check(error, CATEGORY_PERFORMANCE == "performance", &
            "Performance category constant")
        if (allocated(error)) return

        call check(error, CATEGORY_CORRECTNESS == "correctness", &
            "Correctness category constant")
        if (allocated(error)) return

        call check(error, CATEGORY_STYLE /= CATEGORY_PERFORMANCE, &
            "Category constants should be distinct")
        
    end subroutine test_rule_categories
    
end module test_core_basics
