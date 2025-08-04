program small_test
    implicit none
    integer :: i, n
    real :: result
    
    n = 10
    result = 0.0
    
    do i = 1, n
        result = result + real(i)
    end do
    
    print *, 'Result:', result
end program small_test
