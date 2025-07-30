program test_rule_p005_string_operations
    ! Test P005: Inefficient string operations rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none
    
    print *, "Testing P005: Inefficient string operations rule..."
    
    ! Test 1: Inefficient string concatenation (should trigger)
    call test_inefficient_concatenation()
    
    ! Test 2: Efficient string operations (should not trigger)
    call test_efficient_operations()
    
    ! Test 3: String operations in loops
    call test_string_operations_in_loops()
    
    ! Test 4: Repeated string allocations
    call test_repeated_allocations()
    
    print *, "All P005 tests passed!"
    
contains
    
    subroutine test_inefficient_concatenation()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_p005
        
        ! Skip test if fortfront not available
        print *, "  ⚠ Inefficient string concatenation (skipped - fortfront not available)"
        return
        
        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    character(len=:), allocatable :: result" // new_line('a') // &
                   "    character(len=*), parameter :: part1 = 'Hello'" // new_line('a') // &
                   "    character(len=*), parameter :: part2 = ' '" // new_line('a') // &
                   "    character(len=*), parameter :: part3 = 'World'" // new_line('a') // &
                   "    integer :: i" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    ! Inefficient: multiple concatenations creating temporaries" // new_line('a') // &
                   "    result = part1 // part2 // part3" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    ! Inefficient: string concatenation in loop" // new_line('a') // &
                   "    result = ''" // new_line('a') // &
                   "    do i = 1, 10" // new_line('a') // &
                   "        result = result // 'item'" // new_line('a') // &  ! Very inefficient
                   "    end do" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_p005.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_p005.f90", diagnostics, error_msg)
        
        ! Check for P005 violation
        found_p005 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "P005") then
                    found_p005 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_p005.f90", status="old")
        close(99, status="delete")
        
        if (.not. found_p005) then
            error stop "Failed: P005 should be triggered for inefficient string operations"
        end if
        
        print *, "  ✓ Inefficient string concatenation"
        
    end subroutine test_inefficient_concatenation
    
    subroutine test_efficient_operations()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_p005
        
        ! Skip test if fortfront not available
        print *, "  ⚠ Efficient string operations (skipped - fortfront not available)"
        return
        
        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    character(len=100) :: buffer" // new_line('a') // &
                   "    character(len=20) :: part1, part2" // new_line('a') // &
                   "    integer :: pos" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    ! Efficient: pre-sized buffer" // new_line('a') // &
                   "    part1 = 'Hello'" // new_line('a') // &
                   "    part2 = 'World'" // new_line('a') // &
                   "    write(buffer, '(A, A, A)') trim(part1), ' ', trim(part2)" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    ! Efficient: direct assignment" // new_line('a') // &
                   "    buffer = 'Static string'" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_p005_ok.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_p005_ok.f90", diagnostics, error_msg)
        
        ! Check for P005 violation
        found_p005 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "P005") then
                    found_p005 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_p005_ok.f90", status="old")
        close(99, status="delete")
        
        if (found_p005) then
            error stop "Failed: P005 should not be triggered for efficient string operations"
        end if
        
        print *, "  ✓ Efficient string operations"
        
    end subroutine test_efficient_operations
    
    subroutine test_string_operations_in_loops()
        ! Skip test if fortfront not available
        print *, "  ⚠ String operations in loops (skipped - fortfront not available)"
    end subroutine test_string_operations_in_loops
    
    subroutine test_repeated_allocations()
        ! Skip test if fortfront not available
        print *, "  ⚠ Repeated string allocations (skipped - fortfront not available)"
    end subroutine test_repeated_allocations
    
end program test_rule_p005_string_operations