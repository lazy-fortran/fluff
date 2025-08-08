program test_formatter_basic
    use fluff_formatter
    use fluff_ast
    implicit none
    
    type(formatter_engine_t) :: formatter
    character(len=:), allocatable :: source_code, formatted_code, error_msg
    
    print *, "=== Testing Basic Formatter Functionality ==="
    
    ! Initialize formatter
    call formatter%initialize()
    
    ! Test 1: Simple program formatting
    source_code = "program test" // new_line('a') // &
                 "implicit none" // new_line('a') // &
                 "integer :: i" // new_line('a') // &
                 "i = 42" // new_line('a') // &
                 "end program test"
    
    call formatter%format_source(source_code, formatted_code, error_msg)
    
    if (error_msg /= "") then
        print *, "ERROR: " // error_msg
        error stop
    end if
    
    print *, "Input:"
    print *, source_code
    print *, ""
    print *, "Output:"
    print *, formatted_code
    print *, ""
    
    ! Test 2: With variables
    source_code = "program vars" // new_line('a') // &
                 "real :: x, y, z" // new_line('a') // &
                 "x = 1.0" // new_line('a') // &
                 "y = 2.0" // new_line('a') // &
                 "z = x + y" // new_line('a') // &
                 "end program vars"
    
    call formatter%format_source(source_code, formatted_code, error_msg)
    
    if (error_msg /= "") then
        print *, "ERROR: " // error_msg
        error stop
    end if
    
    print *, "Test 2 Output:"
    print *, formatted_code
    
    print *, ""
    print *, "Basic formatter tests completed successfully!"
    
end program test_formatter_basic