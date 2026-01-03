program test_rule_f008_missing_intent
    ! Test F008: Missing intent declarations rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none

    print *, "Testing F008: Missing intent declarations rule..."

    ! Test 1: Missing intent declarations (should trigger)
    call test_missing_intent()

    ! Test 2: Proper intent declarations (should not trigger)
    call test_proper_intent()

    ! Test 3: Mixed intent declarations
    call test_mixed_intent()

    ! Test 4: Function parameters without intent
    call test_function_parameters()

    print *, "All F008 tests passed!"

contains

    subroutine test_missing_intent()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f008

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    ""//new_line('a')// &
                    "contains"//new_line('a')// &
                    ""//new_line('a')// &
                    "    subroutine calc(x, y, result)"//new_line('a')// &
                    "        real :: x, y, result"//new_line('a')// &  ! Missing intent
                    "        result = x + y"//new_line('a')// &
                    "    end subroutine calc"//new_line('a')// &
                    ""//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file="test_f008.f90", status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file("test_f008.f90", diagnostics, error_msg)

        ! Check for F008 violation
        found_f008 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F008") then
                    found_f008 = .true.
                    exit
                end if
            end do
        end if

        ! Clean up
        open (unit=99, file="test_f008.f90", status="old")
        close (99, status="delete")

        if (.not. found_f008) then
            error stop "Failed: F008 should be triggered for missing intent"
        end if

        print *, "  ✓ Missing intent declarations"

    end subroutine test_missing_intent

    subroutine test_proper_intent()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f008

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    ""//new_line('a')// &
                    "contains"//new_line('a')// &
                    ""//new_line('a')// &
                    "    subroutine calc(x, y, result)"//new_line('a')// &
                    "        real, intent(in) :: x, y"//new_line('a')// &
                    "        real, intent(out) :: result"//new_line('a')// &
                    "        result = x + y"//new_line('a')// &
                    "    end subroutine calc"//new_line('a')// &
                    ""//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file="test_f008_ok.f90", status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file("test_f008_ok.f90", diagnostics, error_msg)

        ! Check for F008 violation
        found_f008 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F008") then
                    found_f008 = .true.
                    exit
                end if
            end do
        end if

        ! Clean up
        open (unit=99, file="test_f008_ok.f90", status="old")
        close (99, status="delete")

        if (found_f008) then
      error stop "Failed: F008 should not be triggered when intent is properly declared"
        end if

        print *, "  ✓ Proper intent declarations"

    end subroutine test_proper_intent

    subroutine test_mixed_intent()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f008

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "    subroutine calc(x, y)"//new_line('a')// &
                    "        real, intent(in) :: x"//new_line('a')// &
                    "        real :: y"//new_line('a')// &  ! Missing intent
                    "        y = x + 1.0"//new_line('a')// &
                    "    end subroutine calc"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        open (unit=99, file="test_f008_mixed.f90", status="replace")
        write (99, '(A)') test_code
        close (99)

        call linter%lint_file("test_f008_mixed.f90", diagnostics, error_msg)

        found_f008 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F008") then
                    found_f008 = .true.
                    exit
                end if
            end do
        end if

        open (unit=99, file="test_f008_mixed.f90", status="old")
        close (99, status="delete")

        if (.not. found_f008) then
            error stop "Failed: F008 should be triggered for mixed intent declarations"
        end if

        print *, "  ✓ Mixed intent declarations"
    end subroutine test_mixed_intent

    subroutine test_function_parameters()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f008

        test_code = "integer function add_one(x) result(y)"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &  ! Missing intent
                    "    integer :: y"//new_line('a')// &
                    "    y = x + 1"//new_line('a')// &
                    "end function add_one"

        linter = create_linter_engine()

        open (unit=99, file="test_f008_func.f90", status="replace")
        write (99, '(A)') test_code
        close (99)

        call linter%lint_file("test_f008_func.f90", diagnostics, error_msg)

        found_f008 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F008") then
                    found_f008 = .true.
                    exit
                end if
            end do
        end if

        open (unit=99, file="test_f008_func.f90", status="old")
        close (99, status="delete")

        if (.not. found_f008) then
    error stop "Failed: F008 should be triggered for function parameters without intent"
        end if

        print *, "  ✓ Function parameters"
    end subroutine test_function_parameters

end program test_rule_f008_missing_intent
