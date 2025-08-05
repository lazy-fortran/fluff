program test_minimal_sqrt
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=:), allocatable :: source_code, formatted_code, error_msg
    type(format_options_t) :: options
    
    ! Minimal code with sqrt that might cause the error
    source_code = "program test" // new_line('a') // &
                  "implicit none" // new_line('a') // &
                  "real :: a, result" // new_line('a') // &
                  "a = 1.0" // new_line('a') // &
                  "result = sqrt(a)" // new_line('a') // &
                  "end program"
    
    options = format_options_t()
    
    print *, "Testing minimal sqrt..."
    call transform_lazy_fortran_string_with_format(source_code, formatted_code, error_msg, options)
    
    if (error_msg /= "") then
        print *, "Error: ", error_msg
        error stop 1
    else
        print *, "Success!"
        print *, "Formatted:", formatted_code
    end if
end program test_minimal_sqrt