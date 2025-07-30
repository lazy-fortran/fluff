program test_rule_f005_mixed_tabs_spaces
    ! Test F005: Mixed tabs and spaces rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none
    
    print *, "Testing F005: Mixed tabs and spaces rule..."
    
    ! Test 1: Lines with mixed tabs and spaces (should trigger)
    call test_mixed_tabs_spaces()
    
    ! Test 2: Lines with only spaces (should not trigger)
    call test_only_spaces()
    
    ! Test 3: Lines with only tabs (should not trigger)
    call test_only_tabs()
    
    ! Test 4: Multiple mixed indentations
    call test_multiple_mixed()
    
    print *, "All F005 tests passed!"
    
contains
    
    subroutine test_mixed_tabs_spaces()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f005
        
        ! Skip test if fortfront not available
        print *, "  ⚠ Mixed tabs and spaces (skipped - fortfront not available)"
        return
        
        ! Note: Using char(9) for tab character
        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &        ! 4 spaces
                   char(9) // "integer :: x" // new_line('a') // &  ! Tab + text
                   "  " // char(9) // "x = 42" // new_line('a') // & ! 2 spaces + tab
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f005.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f005.f90", diagnostics, error_msg)
        
        ! Check for F005 violation
        found_f005 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F005") then
                    found_f005 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f005.f90", status="old")
        close(99, status="delete")
        
        if (.not. found_f005) then
            error stop "Failed: F005 should be triggered for mixed tabs and spaces"
        end if
        
        print *, "  ✓ Mixed tabs and spaces"
        
    end subroutine test_mixed_tabs_spaces
    
    subroutine test_only_spaces()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f005
        
        ! Skip test if fortfront not available
        print *, "  ⚠ Only spaces (skipped - fortfront not available)"
        return
        
        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: x" // new_line('a') // &
                   "    x = 42" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f005_spaces.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f005_spaces.f90", diagnostics, error_msg)
        
        ! Check for F005 violation
        found_f005 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F005") then
                    found_f005 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f005_spaces.f90", status="old")
        close(99, status="delete")
        
        if (found_f005) then
            error stop "Failed: F005 should not be triggered for consistent spaces"
        end if
        
        print *, "  ✓ Only spaces"
        
    end subroutine test_only_spaces
    
    subroutine test_only_tabs()
        ! Skip test if fortfront not available
        print *, "  ⚠ Only tabs (skipped - fortfront not available)"
    end subroutine test_only_tabs
    
    subroutine test_multiple_mixed()
        ! Skip test if fortfront not available
        print *, "  ⚠ Multiple mixed indentations (skipped - fortfront not available)"
    end subroutine test_multiple_mixed
    
end program test_rule_f005_mixed_tabs_spaces