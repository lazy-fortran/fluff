program test_rule_f004_trailing_whitespace
    ! Test F004: Trailing whitespace rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none
    
    print *, "Testing F004: Trailing whitespace rule..."
    
    ! Test 1: Lines with trailing whitespace (should trigger)
    call test_trailing_whitespace()
    
    ! Test 2: Clean lines without trailing whitespace (should not trigger)
    call test_no_trailing_whitespace()
    
    ! Test 3: Multiple lines with mixed trailing whitespace
    call test_multiple_trailing_spaces()
    
    ! Test 4: Trailing tabs
    call test_trailing_tabs()
    
    print *, "All F004 tests passed!"
    
contains
    
    subroutine test_trailing_whitespace()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f004
        
        ! Enable test - fortfront is available
        
        ! Note: The spaces after 'none' and 'x' are intentional
        test_code = "program test" // new_line('a') // &
                   "    implicit none   " // new_line('a') // &  ! Trailing spaces
                   "    integer :: x  " // new_line('a') // &     ! Trailing spaces
                   "    x = 42" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file using printf to preserve trailing whitespace
        call system('printf "program test\n" > test_f004.f90')
        call system('printf "    implicit none   \n" >> test_f004.f90')
        call system('printf "    integer :: x  \n" >> test_f004.f90')
        call system('printf "    x = 42\n" >> test_f004.f90')
        call system('printf "end program test\n" >> test_f004.f90')
        
        ! Lint the file
        call linter%lint_file("test_f004.f90", diagnostics, error_msg)
        
        ! Check for F004 violation
        found_f004 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F004") then
                    found_f004 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f004.f90", status="old")
        close(99, status="delete")
        
        if (.not. found_f004) then
            error stop "Failed: F004 should be triggered for trailing whitespace"
        end if
        
        print *, "  ✓ Trailing whitespace"
        
    end subroutine test_trailing_whitespace
    
    subroutine test_no_trailing_whitespace()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f004
        
        ! Enable test - fortfront is available
        
        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: x" // new_line('a') // &
                   "    x = 42" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f004_ok.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f004_ok.f90", diagnostics, error_msg)
        
        ! Check for F004 violation
        found_f004 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F004") then
                    found_f004 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f004_ok.f90", status="old")
        close(99, status="delete")
        
        if (found_f004) then
            error stop "Failed: F004 should not be triggered when no trailing whitespace"
        end if
        
        print *, "  ✓ No trailing whitespace"
        
    end subroutine test_no_trailing_whitespace
    
    subroutine test_multiple_trailing_spaces()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i, j
        integer :: f004_count
        
        ! Enable test - fortfront is available
        
        ! Multiple lines with trailing spaces of different lengths
        test_code = "program test" // new_line('a') // &
                   "    implicit none     " // new_line('a') // &  ! 5 trailing spaces
                   "    integer :: x  " // new_line('a') // &      ! 2 trailing spaces
                   "    real :: y   " // new_line('a') // &        ! 3 trailing spaces
                   "    x = 42" // new_line('a') // &              ! No trailing space
                   "    y = 3.14" // new_line('a') // &            ! No trailing space
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f004_multi.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f004_multi.f90", diagnostics, error_msg)
        
        ! Count F004 violations
        f004_count = 0
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F004") then
                    f004_count = f004_count + 1
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f004_multi.f90", status="old")
        close(99, status="delete")
        
        if (f004_count /= 3) then
            print *, "Expected 3 F004 violations, found", f004_count
            error stop "Failed: F004 should be triggered for each line with trailing whitespace"
        end if
        
        print *, "  ✓ Multiple trailing spaces"
        
    end subroutine test_multiple_trailing_spaces
    
    subroutine test_trailing_tabs()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f004
        
        ! Enable test - fortfront is available
        
        linter = create_linter_engine()
        
        ! Create temporary file with trailing tabs using printf
        call system('printf "program test\n" > test_f004_tabs.f90')
        call system('printf "    implicit none\t\n" >> test_f004_tabs.f90')   ! Trailing tab
        call system('printf "    integer :: x\t\t\n" >> test_f004_tabs.f90') ! Two trailing tabs
        call system('printf "    x = 42\n" >> test_f004_tabs.f90')
        call system('printf "end program test\n" >> test_f004_tabs.f90')
        
        ! Lint the file
        call linter%lint_file("test_f004_tabs.f90", diagnostics, error_msg)
        
        ! Check for F004 violation
        found_f004 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F004") then
                    found_f004 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f004_tabs.f90", status="old")
        close(99, status="delete")
        
        if (.not. found_f004) then
            error stop "Failed: F004 should be triggered for trailing tabs"
        end if
        
        print *, "  ✓ Trailing tabs"
        
    end subroutine test_trailing_tabs
    
end program test_rule_f004_trailing_whitespace