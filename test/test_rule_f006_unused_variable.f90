program test_rule_f006_unused_variable
    ! Test F006: Unused variable declaration rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    use test_support, only: lint_file_checked
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

    function make_tmpfile(stem) result(path)
        character(len=*), intent(in) :: stem
        character(len=:), allocatable :: path

        integer :: count, rate, max_count
        integer, save :: seq = 0
        character(len=32) :: stamp, seq_str

        call system_clock(count, rate, max_count)
        seq = seq + 1

        write (stamp, '(I0)') count
        write (seq_str, '(I0)') seq
        path = "/tmp/"//trim(stem)//"_"//trim(stamp)//"_"//trim(seq_str)//".f90"
    end function make_tmpfile

    subroutine test_unused_variable()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: unit
        integer :: i
        logical :: found_f006

        tmpfile = make_tmpfile("fluff_test_f006")

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y"//new_line('a')// &  ! x is unused
                    "    y = 42"//new_line('a')// &
                    "    print *, y"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (newunit=unit, file=tmpfile, status="replace", action="write")
        write (unit, '(A)') test_code
        close (unit)

        ! Lint the file
        call lint_file_checked(linter, tmpfile, diagnostics)

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
        open (newunit=unit, file=tmpfile, status="old", action="read")
        close (unit, status="delete")

        if (.not. found_f006) then
            error stop "Failed: F006 should be triggered for unused variable"
        end if

        print *, "  - Unused variable"

    end subroutine test_unused_variable

    subroutine test_used_variable()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: unit
        integer :: i
        logical :: found_f006

        tmpfile = make_tmpfile("fluff_test_f006_ok")

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y"//new_line('a')// &
                    "    x = 10"//new_line('a')// &
                    "    y = x + 32"//new_line('a')// &
                    "    print *, x, y"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (newunit=unit, file=tmpfile, status="replace", action="write")
        write (unit, '(A)') test_code
        close (unit)

        ! Lint the file
        call lint_file_checked(linter, tmpfile, diagnostics)

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
        open (newunit=unit, file=tmpfile, status="old", action="read")
        close (unit, status="delete")

        if (found_f006) then
            error stop "Failed: F006 should not be triggered when variables are used"
        end if

        print *, "  - Used variable"

    end subroutine test_used_variable

    subroutine test_multiple_unused()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: unit
        integer :: i, f006_count

        tmpfile = make_tmpfile("fluff_test_f006_multi")

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: a, b, c"//new_line('a')// &
                    "    b = 1"//new_line('a')// &
                    "    print *, b"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        open (newunit=unit, file=tmpfile, status="replace", action="write")
        write (unit, '(A)') test_code
        close (unit)

        call lint_file_checked(linter, tmpfile, diagnostics)

        f006_count = 0
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F006") f006_count = f006_count + 1
            end do
        end if

        open (newunit=unit, file=tmpfile, status="old", action="read")
        close (unit, status="delete")

        if (f006_count < 2) then
            error stop &
                "Failed: expected 2+ F006 diagnostics for multiple unused variables"
        end if

        print *, "  - Multiple unused variables"
    end subroutine test_multiple_unused

    subroutine test_unused_parameter()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: unit
        integer :: i
        logical :: found_f006

        tmpfile = make_tmpfile("fluff_test_f006_param")

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer, parameter :: p = 3"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 1"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        open (newunit=unit, file=tmpfile, status="replace", action="write")
        write (unit, '(A)') test_code
        close (unit)

        call lint_file_checked(linter, tmpfile, diagnostics)

        found_f006 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F006") then
                    found_f006 = .true.
                    exit
                end if
            end do
        end if

        open (newunit=unit, file=tmpfile, status="old", action="read")
        close (unit, status="delete")

        if (found_f006) then
            error stop &
                "Failed: F006 should not be triggered for unused parameter-only cases"
        end if

        print *, "  - Unused parameter"
    end subroutine test_unused_parameter

end program test_rule_f006_unused_variable
