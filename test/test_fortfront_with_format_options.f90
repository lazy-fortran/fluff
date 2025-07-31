program test_fortfront_with_format_options
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=:), allocatable :: source_code, formatted_code, error_msg
    type(format_options_t) :: format_opts
    
    print *, "=== Testing fortfront with format options ==="
    
    ! Test with different indentation
    source_code = "program test" // new_line('a') // &
                 "implicit none" // new_line('a') // &
                 "real :: a = 1.0" // new_line('a') // &
                 "real :: b = 2.0" // new_line('a') // &
                 "real :: result" // new_line('a') // &
                 "result = a * b" // new_line('a') // &
                 "end program test"
    
    print *, "Input code:"
    print *, source_code
    print *, ""
    
    ! Test with 2-space indentation
    format_opts%indent_size = 2
    format_opts%use_tabs = .false.
    format_opts%indent_char = ' '
    format_opts%standardize_types = .false.
    
    call transform_lazy_fortran_string_with_format(source_code, formatted_code, error_msg, format_opts)
    
    if (error_msg /= "") then
        error stop "Formatting failed: " // error_msg
    end if
    
    print *, "Output with 2-space indent:"
    print *, formatted_code
    print *, ""
    
    ! Test with tabs
    format_opts%indent_size = 1
    format_opts%use_tabs = .true.
    format_opts%indent_char = char(9)  ! Tab character
    format_opts%standardize_types = .false.
    
    call transform_lazy_fortran_string_with_format(source_code, formatted_code, error_msg, format_opts)
    
    if (error_msg /= "") then
        error stop "Formatting with tabs failed: " // error_msg
    end if
    
    print *, "Output with tabs:"
    print *, formatted_code
    print *, ""
    
    print *, "✓ Format options API working!"
    
end program test_fortfront_with_format_options