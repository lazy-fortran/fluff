program test_rules
    ! This file tests various linting rules
    
    integer :: used_var
    integer :: unused_var  ! F006: This should trigger unused variable warning
    real :: x, y
    
    ! Missing implicit none - F001 should catch this
    
    used_var = 10
    x = 5.0
    
    ! F007: Using undefined variable
    z = x + y  
    
    call test_subroutine(used_var, x)
    
contains
    
    ! F008: Missing intent declarations
    subroutine test_subroutine(a, b)
        integer :: a  ! Missing intent
        real :: b     ! Missing intent
        
        ! F002: Bad indentation (3 spaces instead of 4)
       a = a + 1
        
        if (a > 10) then
            print *, "Value is large"
         b = b * 2.0  ! F002: Bad indentation (5 spaces)
        end if
        
    end subroutine test_subroutine
    
    function test_function(input) result(output)
        integer :: input  ! F008: Missing intent
        integer :: output
        integer :: temp_var  ! F006: Unused variable
        
        output = input * 2
        
    end function test_function
    
end program test_rules