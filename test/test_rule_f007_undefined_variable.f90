program test_rule_f007_undefined_variable
    ! Test F007: Undefined variable usage rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    use test_support, only: lint_file_checked
    implicit none

    print *, "Testing F007: Undefined variable usage rule..."

    ! Test 1: Undefined variable usage (should trigger)
    call test_undefined_variable()

    ! Test 2: Defined variable (should not trigger)
    call test_defined_variable()

    ! Test 3: Multiple undefined variables
    call test_multiple_undefined()

    ! Test 4: Variable defined in different scope
    call test_scope_visibility()

    print *, "All F007 tests passed!"

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

    subroutine test_undefined_variable()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: unit
        integer :: i
        logical :: found_f007

        tmpfile = make_tmpfile("fluff_test_f007")

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = y + 10"//new_line('a')// &  ! y is undefined
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (newunit=unit, file=tmpfile, status="replace", action="write")
        write (unit, '(A)') test_code
        close (unit)

        ! Lint the file
        call lint_file_checked(linter, tmpfile, diagnostics)

        ! Check for F007 violation
        found_f007 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F007") then
                    found_f007 = .true.
                    exit
                end if
            end do
        end if

        ! Clean up
        open (newunit=unit, file=tmpfile, status="old", action="read")
        close (unit, status="delete")

        if (.not. found_f007) then
            error stop "Failed: F007 should be triggered for undefined variable"
        end if

        print *, "  - Undefined variable usage"

    end subroutine test_undefined_variable

    subroutine test_defined_variable()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: unit
        integer :: i
        logical :: found_f007

        tmpfile = make_tmpfile("fluff_test_f007_ok")

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y"//new_line('a')// &
                    "    y = 20"//new_line('a')// &
                    "    x = y + 10"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (newunit=unit, file=tmpfile, status="replace", action="write")
        write (unit, '(A)') test_code
        close (unit)

        ! Lint the file
        call lint_file_checked(linter, tmpfile, diagnostics)

        ! Check for F007 violation
        found_f007 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F007") then
                    found_f007 = .true.
                    exit
                end if
            end do
        end if

        ! Clean up
        open (newunit=unit, file=tmpfile, status="old", action="read")
        close (unit, status="delete")

        if (found_f007) then
            error stop "Failed: F007 should not be triggered when variables are defined"
        end if

        print *, "  - Defined variable"

    end subroutine test_defined_variable

    subroutine test_multiple_undefined()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: unit
        integer :: i, f007_count

        tmpfile = make_tmpfile("fluff_test_f007_multi")

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = y + z"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        open (newunit=unit, file=tmpfile, status="replace", action="write")
        write (unit, '(A)') test_code
        close (unit)

        call lint_file_checked(linter, tmpfile, diagnostics)

        f007_count = 0
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F007") f007_count = f007_count + 1
            end do
        end if

        open (newunit=unit, file=tmpfile, status="old", action="read")
        close (unit, status="delete")

        if (f007_count < 2) then
            error stop &
                "Failed: expected 2+ F007 diagnostics for multiple undefined variables"
        end if

        print *, "  - Multiple undefined variables"
    end subroutine test_multiple_undefined

    subroutine test_scope_visibility()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: unit
        integer :: i
        logical :: found_f007

        tmpfile = make_tmpfile("fluff_test_f007_scope")

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    call sub()"//new_line('a')// &
                    "    x = y"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "    subroutine sub()"//new_line('a')// &
                    "        integer :: y"//new_line('a')// &
                    "        y = 1"//new_line('a')// &
                    "    end subroutine sub"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        open (newunit=unit, file=tmpfile, status="replace", action="write")
        write (unit, '(A)') test_code
        close (unit)

        call lint_file_checked(linter, tmpfile, diagnostics)

        found_f007 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F007") then
                    found_f007 = .true.
                    exit
                end if
            end do
        end if

        open (newunit=unit, file=tmpfile, status="old", action="read")
        close (unit, status="delete")

        if (.not. found_f007) then
            error stop "Failed: expected F007 for out-of-scope variable"
        end if

        print *, "  - Variable scope visibility"
    end subroutine test_scope_visibility

end program test_rule_f007_undefined_variable
