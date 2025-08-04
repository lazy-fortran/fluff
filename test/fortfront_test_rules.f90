module fortfront_test_rules
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none
    private
    
    public :: collect_fortfront_test_rules
    
contains
    
    !> Collect all tests
    subroutine collect_fortfront_test_rules(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        
        testsuite = [ &
            new_unittest("rule_test_syntax", test_rule_test_syntax) &
        ]
        
    end subroutine collect_fortfront_test_rules
    
    subroutine test_rule_test_syntax(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i, unused_var, undefined_var
        
        ! This test validates that basic rule test code compiles
        i = 42
        undefined_var = i  ! Initialize to avoid undefined usage
        
        call check(error, i == 42, "Variable assignment should work")
        
    end subroutine test_rule_test_syntax
    
end module fortfront_test_rules
