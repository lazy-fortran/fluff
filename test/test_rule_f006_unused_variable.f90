program test_rule_f006_unused_variable
    ! Test F006: Unused variable declaration rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none

    print *, "Testing F006: Unused variable declaration rule..."

    ! Test 1: Unused variable (should trigger)
    call test_unused_variable()

    ! Test 2: Used variable (should not trigger)
    call test_used_variable()

    ! Test 3: Multiple unused variables
    call test_multiple_unused()

    ! Test 4: Unused parameter vs used variable
    call test_unused_parameter()

    print *, "All F006 tests passed!"

contains

    subroutine test_unused_variable()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=*), parameter :: tmpfile = "/tmp/fluff_test_f006.f90"
        integer :: i
        logical :: found_f006

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y"//new_line('a')// &  ! x is unused
                    "    y = 42"//new_line('a')// &
                    "    print *, y"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file=tmpfile, status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file(tmpfile, diagnostics, error_msg)

        ! Check for F006 violation
        found_f006 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F006") then
                    found_f006 = .true.
                    exit
                end if
            end do
        end if

        ! Clean up
        open (unit=99, file=tmpfile, status="old")
        close (99, status="delete")

        if (.not. found_f006) then
            error stop "Failed: F006 should be triggered for unused variable"
        end if

        print *, "  - Unused variable"

    end subroutine test_unused_variable

    subroutine test_used_variable()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=*), parameter :: tmpfile = "/tmp/fluff_test_f006_ok.f90"
        integer :: i
        logical :: found_f006

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y"//new_line('a')// &
                    "    x = 10"//new_line('a')// &
                    "    y = x + 32"//new_line('a')// &
                    "    print *, x, y"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file=tmpfile, status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file(tmpfile, diagnostics, error_msg)

        ! Check for F006 violation
        found_f006 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F006") then
                    found_f006 = .true.
                    exit
                end if
            end do
        end if

        ! Clean up
        open (unit=99, file=tmpfile, status="old")
        close (99, status="delete")

        if (found_f006) then
            error stop "Failed: F006 should not be triggered when variables are used"
        end if

        print *, "  - Used variable"

    end subroutine test_used_variable

    subroutine test_multiple_unused()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=*), parameter :: tmpfile = "/tmp/fluff_test_f006_multi.f90"
        integer :: i, f006_count

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: a, b, c"//new_line('a')// &
                    "    b = 1"//new_line('a')// &
                    "    print *, b"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        open (unit=99, file=tmpfile, status="replace")
        write (99, '(A)') test_code
        close (99)

        call linter%lint_file(tmpfile, diagnostics, error_msg)

        f006_count = 0
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F006") f006_count = f006_count + 1
            end do
        end if

        open (unit=99, file=tmpfile, status="old")
        close (99, status="delete")

        if (f006_count < 2) then
            error stop &
                "Failed: expected 2+ F006 diagnostics for multiple unused variables"
        end if

        print *, "  - Multiple unused variables"
    end subroutine test_multiple_unused

    subroutine test_unused_parameter()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=*), parameter :: tmpfile = "/tmp/fluff_test_f006_param.f90"
        integer :: i
        logical :: found_f006

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer, parameter :: p = 3"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 1"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        open (unit=99, file=tmpfile, status="replace")
        write (99, '(A)') test_code
        close (99)

        call linter%lint_file(tmpfile, diagnostics, error_msg)

        found_f006 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F006") then
                    found_f006 = .true.
                    exit
                end if
            end do
        end if

        open (unit=99, file=tmpfile, status="old")
        close (99, status="delete")

        if (found_f006) then
            error stop &
                "Failed: F006 should not be triggered for unused parameter-only cases"
        end if

        print *, "  - Unused parameter"
    end subroutine test_unused_parameter

end program test_rule_f006_unused_variable
