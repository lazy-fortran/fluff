program debug_hover
    use fluff_lsp_hover
    implicit none
    
    character(len=:), allocatable :: hover_content
    logical :: success
    character(len=100) :: code
    
    ! Test 1: Hover over "x" in "integer :: x = 42"
    code = "integer :: x = 42"
    print *, "Test 1: '", trim(code), "'"
    print *, "Position: line=1, char=11 (should be on 'x')"
    
    call get_hover_info(code, 1, 11, hover_content, success)
    
    print *, "Success: ", success
    if (allocated(hover_content)) then
        print *, "Hover content: '", hover_content, "'"
    else
        print *, "Hover content: NOT ALLOCATED"
    end if
    
    print *, ""
    
    ! Test 2: Test format_hover_message directly
    print *, "Test 2: format_hover_message"
    call format_hover_message("integer :: x", "Variable declaration", hover_content, success)
    print *, "Success: ", success
    if (allocated(hover_content)) then
        print *, "Formatted: '", hover_content, "'"
    end if
    
end program debug_hover