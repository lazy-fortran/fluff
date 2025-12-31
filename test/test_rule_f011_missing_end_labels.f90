program test_rule_f011_missing_end_labels
    ! Test F011: Missing end statement labels rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none
    
    print *, "Testing F011: Missing end statement labels rule..."
    
    ! Test 1: Missing end labels (should trigger)
    call test_missing_end_labels()
    
    ! Test 2: Proper end labels (should not trigger)
    call test_proper_end_labels()
    
    ! Test 3: Mixed end labels
    call test_mixed_end_labels()
    
    ! Test 4: Function end labels
    call test_function_end_labels()
    
    print *, "All F011 tests passed!"
    
contains
    
    subroutine test_missing_end_labels()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f011

        test_code = "program test_prog" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "" // new_line('a') // &
                   "contains" // new_line('a') // &
                   "" // new_line('a') // &
                   "    subroutine calc_values(x, y)" // new_line('a') // &
                   "        real, intent(in) :: x, y" // new_line('a') // &
                   "        print *, x + y" // new_line('a') // &
                   "    end subroutine" // new_line('a') // &  ! Missing label
                   "" // new_line('a') // &
                   "    function compute(a) result(b)" // new_line('a') // &
                   "        real, intent(in) :: a" // new_line('a') // &
                   "        real :: b" // new_line('a') // &
                   "        b = a * 2.0" // new_line('a') // &
                   "    end function" // new_line('a') // &  ! Missing label
                   "" // new_line('a') // &
                   "end program"  ! Missing label
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f011.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f011.f90", diagnostics, error_msg)
        
        ! Check for F011 violation
        found_f011 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F011") then
                    found_f011 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f011.f90", status="old")
        close(99, status="delete")
        
        if (.not. found_f011) then
            error stop "Failed: F011 should be triggered for missing end labels"
        end if

        print *, "  Missing end labels"

    end subroutine test_missing_end_labels
    
    subroutine test_proper_end_labels()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f011

        test_code = "program test_prog" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "" // new_line('a') // &
                   "contains" // new_line('a') // &
                   "" // new_line('a') // &
                   "    subroutine calc_values(x, y)" // new_line('a') // &
                   "        real, intent(in) :: x, y" // new_line('a') // &
                   "        print *, x + y" // new_line('a') // &
                   "    end subroutine calc_values" // new_line('a') // &
                   "" // new_line('a') // &
                   "    function compute(a) result(b)" // new_line('a') // &
                   "        real, intent(in) :: a" // new_line('a') // &
                   "        real :: b" // new_line('a') // &
                   "        b = a * 2.0" // new_line('a') // &
                   "    end function compute" // new_line('a') // &
                   "" // new_line('a') // &
                   "end program test_prog"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f011_ok.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f011_ok.f90", diagnostics, error_msg)
        
        ! Check for F011 violation
        found_f011 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F011") then
                    found_f011 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f011_ok.f90", status="old")
        close(99, status="delete")
        
        if (found_f011) then
            error stop "Failed: F011 should not be triggered for proper end labels"
        end if

        print *, "  Proper end labels"

    end subroutine test_proper_end_labels

    subroutine test_mixed_end_labels()
        ! Mixed end labels test (advanced)
        print *, "  Mixed end labels (not implemented)"
    end subroutine test_mixed_end_labels

    subroutine test_function_end_labels()
        ! Function end labels test (advanced)
        print *, "  Function end labels (not implemented)"
    end subroutine test_function_end_labels

end program test_rule_f011_missing_end_labels