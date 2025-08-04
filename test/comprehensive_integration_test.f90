! Comprehensive integration test for all fluff rules
program comprehensive_integration_test
    ! F001: Missing implicit none (intentionally missing)
integer :: global_var  ! No implicit none
  real :: poorly_indented_var  ! F002: bad indentation
    character(len=200) :: very_long_var_name_that_exceeds_line_length = 'test'  ! F003
    integer :: trailing_spaces_var     
	    integer :: mixed_tabs_var
    integer :: unused_variable  ! F006: unused
    integer :: undefined_var   ! Declare to avoid compilation error
    real :: matrix(1000, 1000)
    real, allocatable :: temp_array(:)
    real :: single_precision
    double precision :: double_precision_val
    integer :: i, j, k
    !
    global_var = 42
    single_precision = 3.14
    double_precision_val = 2.71828d0
    !
    ! P001: Non-contiguous array access
    do i = 1, 1000
        do j = 1, 1000
            matrix(j, i) = real(i * j)  ! Column-major (bad)
        end do
    end do
    !
    ! P006: Allocations in loops
    do k = 1, 100
        allocate(temp_array(100))  ! Bad: in loop
        temp_array = real(k)
        ! P007: Mixed precision arithmetic
        single_precision = single_precision + double_precision_val
        deallocate(temp_array)
    end do
    !
    ! F007 & C001: Undefined variable
    print *, undefined_var  ! Error: not declared
    !
    call test_subroutine(global_var)
    !
contains
    !
    ! F008: Missing intent declarations
    subroutine test_subroutine(param)
        integer :: param  ! Missing intent
        param = param * 2
    end subroutine test_subroutine
    !
    ! P004: Missing pure/elemental
    function square(x) result(y)
        real :: x, y  ! Could be pure elemental
        y = x * x
    end function square
    !
end program comprehensive_integration_test
