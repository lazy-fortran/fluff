program test_standardize_types
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=:), allocatable :: source_code, formatted_code, error_msg
    type(format_options_t) :: format_opts
    
    print *, "=== Testing standardize_types field ==="
    
    source_code = "program test" // new_line('a') // &
                 "implicit none" // new_line('a') // &
                 "real :: x = 1.0" // new_line('a') // &
                 "end program test"
    
    print *, "Input:"
    print *, source_code
    print *, ""
    
    ! Test with standardize_types = .false.
    format_opts%indent_size = 4
    format_opts%use_tabs = .false.
    format_opts%indent_char = ' '
    format_opts%standardize_types = .false.
    
    call transform_lazy_fortran_string_with_format(source_code, formatted_code, error_msg, format_opts)
    
    if (error_msg /= "") then
        error stop "Formatting failed: " // error_msg
    end if
    
    print *, "Output with standardize_types=.false.:"
    print *, formatted_code
    print *, ""
    
    ! Test with standardize_types = .true.
    format_opts%standardize_types = .true.
    
    call transform_lazy_fortran_string_with_format(source_code, formatted_code, error_msg, format_opts)
    
    if (error_msg /= "") then
        error stop "Formatting failed: " // error_msg
    end if
    
    print *, "Output with standardize_types=.true.:"
    print *, formatted_code
    
end program test_standardize_types