program test_rule_f010_obsolete_features
    ! Test F010: Obsolete language features rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none
    
    print *, "Testing F010: Obsolete language features rule..."
    
    ! Test 1: GOTO statement (should trigger)
    call test_goto_statement()
    
    ! Test 2: Computed GOTO (should trigger)
    call test_computed_goto()
    
    ! Test 3: Modern control flow (should not trigger)
    call test_modern_control_flow()
    
    ! Test 4: Arithmetic IF statement (should trigger)
    call test_arithmetic_if()
    
    print *, "All F010 tests passed!"
    
contains
    
    subroutine test_goto_statement()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f010

        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: x" // new_line('a') // &
                   "    x = 10" // new_line('a') // &
                   "    if (x > 5) goto 100" // new_line('a') // &
                   "    print *, 'x <= 5'" // new_line('a') // &
                   "100 print *, 'x > 5'" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f010.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f010.f90", diagnostics, error_msg)
        
        ! Check for F010 violation
        found_f010 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F010") then
                    found_f010 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f010.f90", status="old")
        close(99, status="delete")
        
        if (.not. found_f010) then
            error stop "Failed: F010 should be triggered for GOTO statement"
        end if

        print *, "  GOTO statement"

    end subroutine test_goto_statement
    
    subroutine test_computed_goto()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f010

        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: choice" // new_line('a') // &
                   "    choice = 2" // new_line('a') // &
                   "    goto (10, 20, 30), choice" // new_line('a') // &
                   "    print *, 'default'" // new_line('a') // &
                   "10  print *, 'choice 1'" // new_line('a') // &
                   "    goto 40" // new_line('a') // &
                   "20  print *, 'choice 2'" // new_line('a') // &
                   "    goto 40" // new_line('a') // &
                   "30  print *, 'choice 3'" // new_line('a') // &
                   "40  continue" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f010_computed.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f010_computed.f90", diagnostics, error_msg)
        
        ! Check for F010 violation
        found_f010 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F010") then
                    found_f010 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f010_computed.f90", status="old")
        close(99, status="delete")
        
        if (.not. found_f010) then
            error stop "Failed: F010 should be triggered for computed GOTO"
        end if

        print *, "  Computed GOTO"

    end subroutine test_computed_goto
    
    subroutine test_modern_control_flow()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f010

        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: x, choice" // new_line('a') // &
                   "    x = 10" // new_line('a') // &
                   "    choice = 2" // new_line('a') // &
                   "    if (x > 5) then" // new_line('a') // &
                   "        print *, 'x > 5'" // new_line('a') // &
                   "    else" // new_line('a') // &
                   "        print *, 'x <= 5'" // new_line('a') // &
                   "    end if" // new_line('a') // &
                   "    select case (choice)" // new_line('a') // &
                   "    case (1)" // new_line('a') // &
                   "        print *, 'choice 1'" // new_line('a') // &
                   "    case (2)" // new_line('a') // &
                   "        print *, 'choice 2'" // new_line('a') // &
                   "    case default" // new_line('a') // &
                   "        print *, 'other choice'" // new_line('a') // &
                   "    end select" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f010_ok.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f010_ok.f90", diagnostics, error_msg)
        
        ! Check for F010 violation
        found_f010 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F010") then
                    found_f010 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f010_ok.f90", status="old")
        close(99, status="delete")
        
        if (found_f010) then
            error stop "Failed: F010 should not be triggered for modern control flow"
        end if

        print *, "  Modern control flow"

    end subroutine test_modern_control_flow

    subroutine test_arithmetic_if()
        ! Arithmetic IF test placeholder (advanced obsolete feature)
        print *, "  Arithmetic IF statement (not implemented)"
    end subroutine test_arithmetic_if

end program test_rule_f010_obsolete_features