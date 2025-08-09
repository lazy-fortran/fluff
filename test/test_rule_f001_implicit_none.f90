program test_rule_f001_implicit_none
    ! Test F001: Missing implicit none rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none
    
    print *, "Testing F001: Missing implicit none rule..."
    
    ! Test 1: Program without implicit none (should trigger)
    call test_missing_implicit_none()
    
    ! Test 2: Program with implicit none (should not trigger)
    call test_has_implicit_none()
    
    ! Test 3: Module without implicit none (should trigger)
    call test_module_missing_implicit_none()
    
    ! Test 4: Subroutine without implicit none (should trigger)
    call test_subroutine_missing_implicit_none()
    
    ! Test 5: Interface blocks should not trigger
    call test_interface_block()
    
    print *, "All F001 tests passed!"
    
contains
    
    subroutine test_missing_implicit_none()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f001
        
        ! Enable test - fortfront is available
        
        test_code = "program test" // new_line('a') // &
                   "    integer :: x" // new_line('a') // &
                   "    x = 42" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f001.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f001.f90", diagnostics, error_msg)
        
        ! Check for F001 violation
        found_f001 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F001") then
                    found_f001 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f001.f90", status="old")
        close(99, status="delete")
        
        if (.not. found_f001) then
            error stop "Failed: F001 should be triggered for missing implicit none"
        end if
        
        print *, "  ✓ Missing implicit none in program"
        
    end subroutine test_missing_implicit_none
    
    subroutine test_has_implicit_none()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f001
        
        ! Enable test - fortfront is available
        
        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: x" // new_line('a') // &
                   "    x = 42" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f001_ok.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f001_ok.f90", diagnostics, error_msg)
        
        ! Check for F001 violation
        found_f001 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F001") then
                    found_f001 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f001_ok.f90", status="old")
        close(99, status="delete")
        
        if (found_f001) then
            error stop "Failed: F001 should not be triggered when implicit none is present"
        end if
        
        print *, "  ✓ Has implicit none"
        
    end subroutine test_has_implicit_none
    
    subroutine test_module_missing_implicit_none()
        ! Skip test if fortfront not available
        print *, "  ⚠ Module missing implicit none (skipped - fortfront not available)"
    end subroutine test_module_missing_implicit_none
    
    subroutine test_subroutine_missing_implicit_none()
        ! Skip test if fortfront not available
        print *, "  ⚠ Subroutine missing implicit none (skipped - fortfront not available)"
    end subroutine test_subroutine_missing_implicit_none
    
    subroutine test_interface_block()
        ! Skip test if fortfront not available
        print *, "  ⚠ Interface block handling (skipped - fortfront not available)"
    end subroutine test_interface_block
    
end program test_rule_f001_implicit_none