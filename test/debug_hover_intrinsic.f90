program debug_hover_intrinsic
    use fluff_lsp_hover
    implicit none
    
    character(len=:), allocatable :: hover_content
    logical :: success
    character(len=100) :: code
    
    ! Test: Hover over "sin" in "x = sin(angle)"
    code = "x = sin(angle)"
    print *, "Test: '", trim(code), "'"
    print *, "Position: line=1, char=4 (0-based, should be on 'sin')"
    
    call get_hover_info(code, 1, 4, hover_content, success)
    
    print *, "Success: ", success
    if (allocated(hover_content)) then
        print *, "Hover content: '", hover_content, "'"
    else
        print *, "Hover content: NOT ALLOCATED"
    end if
    
end program debug_hover_intrinsic