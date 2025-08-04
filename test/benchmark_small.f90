module benchmark_small
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none
    private
    
    public :: collect_benchmark_small_tests
    
contains
    
    !> Collect all tests
    subroutine collect_benchmark_small_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        
        testsuite = [ &
            new_unittest("simple_arithmetic", test_simple_arithmetic) &
        ]
        
    end subroutine collect_benchmark_small_tests
    
    subroutine test_simple_arithmetic(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i, n
        real :: result, expected
        
        n = 10
        result = 0.0
        expected = 55.0  ! Sum of 1 to 10
        
        do i = 1, n
            result = result + real(i)
        end do
        
        call check(error, abs(result - expected) < 1e-6, &
            "Sum of 1 to 10 should be 55.0")
        
    end subroutine test_simple_arithmetic
    
end module benchmark_small
