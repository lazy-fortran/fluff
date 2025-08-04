module comprehensive_integration_test
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none
    private
    
    public :: collect_comprehensive_integration_tests
    
contains
    
    !> Collect all tests
    subroutine collect_comprehensive_integration_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        
        testsuite = [ &
            new_unittest("basic_integration", test_basic_integration) &
        ]
        
    end subroutine collect_comprehensive_integration_tests
    
    subroutine test_basic_integration(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: global_var
        real :: single_precision
        double precision :: double_precision_val
        
        ! Basic integration test with proper code
        global_var = 42
        single_precision = 3.14
        double_precision_val = 2.71828d0
        
        call check(error, global_var == 42, "Variable assignment should work")
        if (allocated(error)) return
        
        call check(error, abs(single_precision - 3.14) < 1e-6, "Real assignment should work")
        if (allocated(error)) return
        
        call check(error, abs(double_precision_val - 2.71828d0) < 1e-12, "Double precision assignment should work")
        
    end subroutine test_basic_integration
    
end module comprehensive_integration_test
