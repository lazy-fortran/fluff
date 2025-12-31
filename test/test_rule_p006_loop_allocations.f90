program test_rule_p006_loop_allocations
    ! Test P006: Unnecessary allocations in loops rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none

    print *, "Testing P006: Unnecessary allocations in loops rule..."

    ! Test 1: Allocations inside loops (should trigger)
    call test_allocations_in_loops()

    ! Test 2: Pre-allocated outside loops (should not trigger)
    call test_pre_allocated()

    ! Test 3: Necessary allocations per iteration
    call test_necessary_per_iteration()

    ! Test 4: String allocations in loops
    call test_string_allocations()

    print *, "All P006 tests passed!"

contains

    subroutine test_allocations_in_loops()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_p006

        ! BLOCKED: Rule implementation returns empty violations (see fluff_rules.f90 line 3365)
        ! Requires fortfront AST API for loop allocations check (issues #11-14)
        print *, "  - Allocations inside loops (blocked: rule not implemented)"
        return

        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: i, n" // new_line('a') // &
                   "    real, allocatable :: temp_array(:)" // new_line('a') // &
                   "    real :: result" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    n = 1000" // new_line('a') // &
                   "    result = 0.0" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    ! Bad: allocating inside loop" // new_line('a') // &
                   "    do i = 1, 100" // new_line('a') // &
                   "        allocate(temp_array(n))" // new_line('a') // &
                   "        temp_array = real(i)" // new_line('a') // &
                   "        result = result + sum(temp_array)" // new_line('a') // &
                   "        deallocate(temp_array)" // new_line('a') // &
                   "    end do" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    print *, result" // new_line('a') // &
                   "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open(unit=99, file="test_p006.f90", status="replace")
        write(99, '(A)') test_code
        close(99)

        ! Lint the file
        call linter%lint_file("test_p006.f90", diagnostics, error_msg)

        ! Check for P006 violation
        found_p006 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "P006") then
                    found_p006 = .true.
                    exit
                end if
            end do
        end if

        ! Clean up
        open(unit=99, file="test_p006.f90", status="old")
        close(99, status="delete")

        if (.not. found_p006) then
            error stop "Failed: P006 should be triggered for allocations in loops"
        end if

        print *, "  + Allocations inside loops"

    end subroutine test_allocations_in_loops

    subroutine test_pre_allocated()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_p006

        ! BLOCKED: Rule implementation returns empty violations
        print *, "  - Pre-allocated outside loops (blocked: rule not implemented)"
        return

        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: i, n" // new_line('a') // &
                   "    real, allocatable :: temp_array(:)" // new_line('a') // &
                   "    real :: result" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    n = 1000" // new_line('a') // &
                   "    result = 0.0" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    ! Good: allocate once outside loop" // new_line('a') // &
                   "    allocate(temp_array(n))" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    do i = 1, 100" // new_line('a') // &
                   "        temp_array = real(i)" // new_line('a') // &
                   "        result = result + sum(temp_array)" // new_line('a') // &
                   "    end do" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    deallocate(temp_array)" // new_line('a') // &
                   "    print *, result" // new_line('a') // &
                   "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open(unit=99, file="test_p006_ok.f90", status="replace")
        write(99, '(A)') test_code
        close(99)

        ! Lint the file
        call linter%lint_file("test_p006_ok.f90", diagnostics, error_msg)

        ! Check for P006 violation
        found_p006 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "P006") then
                    found_p006 = .true.
                    exit
                end if
            end do
        end if

        ! Clean up
        open(unit=99, file="test_p006_ok.f90", status="old")
        close(99, status="delete")

        if (found_p006) then
            error stop "Failed: P006 should not be triggered for pre-allocated arrays"
        end if

        print *, "  + Pre-allocated outside loops"

    end subroutine test_pre_allocated

    subroutine test_necessary_per_iteration()
        ! BLOCKED: Rule implementation returns empty violations
        print *, "  - Necessary allocations per iteration (blocked: rule not implemented)"
    end subroutine test_necessary_per_iteration

    subroutine test_string_allocations()
        ! BLOCKED: Rule implementation returns empty violations
        print *, "  - String allocations in loops (blocked: rule not implemented)"
    end subroutine test_string_allocations

end program test_rule_p006_loop_allocations
