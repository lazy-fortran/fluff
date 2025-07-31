program test_formatter_simple
    use fluff_formatter
    use fluff_ast
    implicit none
    
    type(formatter_engine_t) :: formatter
    character(len=:), allocatable :: input_code, formatted_code, error_msg
    
    ! Initialize formatter
    call formatter%initialize()
    
    ! Simple test code
    input_code = "program test" // new_line('a') // &
                "    implicit none" // new_line('a') // &
                "    integer :: i,j,k" // new_line('a') // &
                "    i=1" // new_line('a') // &
                "    j=i+2" // new_line('a') // &
                "end program test"
    
    print *, "Input:"
    print *, input_code
    print *, ""
    
    ! Format
    call formatter%format_source(input_code, formatted_code, error_msg)
    
    if (error_msg /= "") then
        print *, "Error:", error_msg
    else
        print *, "Formatted:"
        print *, formatted_code
    end if
    
end program test_formatter_simple