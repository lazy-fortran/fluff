module test_standardize_types
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    private
    
    public :: collect_standardize_types_tests
    
contains
    
    !> Collect all tests
    subroutine collect_standardize_types_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        
        testsuite = [ &
            new_unittest("standardize_types_false", test_standardize_types_false), &
            new_unittest("standardize_types_true", test_standardize_types_true) &
        ]
        
    end subroutine collect_standardize_types_tests
    
    subroutine test_standardize_types_false(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: source_code, formatted_code, error_msg
        type(format_options_t) :: format_opts
        
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "real :: x = 1.0" // new_line('a') // &
                     "end program test"
        
        ! Test with standardize_types = .false.
        format_opts%indent_size = 4
        format_opts%use_tabs = .false.
        format_opts%indent_char = ' '
        format_opts%standardize_types = .false.
        
        call transform_lazy_fortran_string_with_format(source_code, formatted_code, error_msg, format_opts)
        
        call check(error, error_msg == "", "Formatting should succeed: " // error_msg)
        
    end subroutine test_standardize_types_false
    
    subroutine test_standardize_types_true(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: source_code, formatted_code, error_msg
        type(format_options_t) :: format_opts
        
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "real :: x = 1.0" // new_line('a') // &
                     "end program test"
        
        ! Test with standardize_types = .true.
        format_opts%indent_size = 4
        format_opts%use_tabs = .false.
        format_opts%indent_char = ' '
        format_opts%standardize_types = .true.
        
        call transform_lazy_fortran_string_with_format(source_code, formatted_code, error_msg, format_opts)
        
        call check(error, error_msg == "", "Formatting should succeed: " // error_msg)
        
    end subroutine test_standardize_types_true
    
end module test_standardize_types