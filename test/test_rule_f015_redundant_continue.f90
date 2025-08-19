program test_rule_f015_redundant_continue
    ! Test F015: Redundant continue statements rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none
    
    print *, "Testing F015: Redundant continue statements rule..."
    
    ! Test 1: Redundant continue statements (should trigger)
    call test_redundant_continue()
    
    ! Test 2: No continue statements (should not trigger)
    call test_no_continue()
    
    ! Test 3: Necessary continue statements
    call test_necessary_continue()
    
    ! Test 4: Loop labels and continue
    call test_loop_labels_continue()
    
    print *, "All F015 tests passed!"
    
contains
    
    subroutine test_redundant_continue()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f015
        
        ! Test redundant continue statements detection
        
        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: i" // new_line('a') // &
                   "    do i = 1, 10" // new_line('a') // &
                   "        if (i == 5) then" // new_line('a') // &
                   "            continue" // new_line('a') // &  ! Redundant continue
                   "        end if" // new_line('a') // &
                   "        print *, i" // new_line('a') // &
                   "    end do" // new_line('a') // &
                   "    " // new_line('a') // &
                   "10  continue" // new_line('a') // &           ! Redundant labeled continue
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f015.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f015.f90", diagnostics, error_msg)
        
        ! Check for F015 violation
        found_f015 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F015") then
                    found_f015 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f015.f90", status="old")
        close(99, status="delete")
        
        if (.not. found_f015) then
            error stop "Failed: F015 should be triggered for redundant continue"
        end if
        
        print *, "  ✓ Redundant continue statements"
        
    end subroutine test_redundant_continue
    
    subroutine test_no_continue()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f015
        
        ! Test code without continue statements (should not trigger F015)
        
        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: i" // new_line('a') // &
                   "    do i = 1, 10" // new_line('a') // &
                   "        if (i == 5) then" // new_line('a') // &
                   "            exit" // new_line('a') // &
                   "        end if" // new_line('a') // &
                   "        print *, i" // new_line('a') // &
                   "    end do" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f015_ok.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f015_ok.f90", diagnostics, error_msg)
        
        ! Check for F015 violation
        found_f015 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F015") then
                    found_f015 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f015_ok.f90", status="old")
        close(99, status="delete")
        
        if (found_f015) then
            error stop "Failed: F015 should not be triggered when no continue statements"
        end if
        
        print *, "  ✓ No continue statements"
        
    end subroutine test_no_continue
    
    subroutine test_necessary_continue()
        ! Test placeholder for necessary continue statements
        print *, "  ✓ Necessary continue statements (test placeholder)"
    end subroutine test_necessary_continue
    
    subroutine test_loop_labels_continue()
        ! Test placeholder for loop labels and continue
        print *, "  ✓ Loop labels and continue (test placeholder)"
    end subroutine test_loop_labels_continue
    
end program test_rule_f015_redundant_continue