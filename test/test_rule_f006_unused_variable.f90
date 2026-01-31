program test_rule_f006_unused_variable
    ! Test F006: Unused variable declaration rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked
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

    ! Test 5: Loop control variables should not trigger F006
    call test_loop_control_variables()

    print *, "[OK] All F006 tests passed!"

contains

    subroutine test_unused_variable()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: i
        logical :: found_f006

        call make_temp_fortran_path("fluff_test_f006", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y"//new_line('a')// &  ! x is unused
                    "    y = 42"//new_line('a')// &
                    "    print *, y"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call write_text_file(tmpfile, test_code)

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

        call delete_file_if_exists(tmpfile)

        if (.not. found_f006) then
            error stop "Failed: F006 should be triggered for unused variable"
        end if

        print *, "[OK] Unused variable"

    end subroutine test_unused_variable

    subroutine test_used_variable()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: i
        logical :: found_f006

        call make_temp_fortran_path("fluff_test_f006_ok", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y"//new_line('a')// &
                    "    x = 10"//new_line('a')// &
                    "    y = x + 32"//new_line('a')// &
                    "    print *, x, y"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call write_text_file(tmpfile, test_code)

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

        call delete_file_if_exists(tmpfile)

        if (found_f006) then
            error stop "Failed: F006 should not be triggered when variables are used"
        end if

        print *, "[OK] Used variable"

    end subroutine test_used_variable

    subroutine test_multiple_unused()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: i, f006_count

        call make_temp_fortran_path("fluff_test_f006_multi", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: a, b, c"//new_line('a')// &
                    "    b = 1"//new_line('a')// &
                    "    print *, b"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call write_text_file(tmpfile, test_code)

        call lint_file_checked(linter, tmpfile, diagnostics)

        f006_count = 0
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F006") f006_count = f006_count + 1
            end do
        end if

        call delete_file_if_exists(tmpfile)

        if (f006_count < 2) then
            error stop &
                "Failed: expected 2+ F006 diagnostics for multiple unused variables"
        end if

        print *, "[OK] Multiple unused variables"
    end subroutine test_multiple_unused

    subroutine test_unused_parameter()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: i
        logical :: found_f006

        call make_temp_fortran_path("fluff_test_f006_param", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer, parameter :: p = 3"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 1"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call write_text_file(tmpfile, test_code)

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

        call delete_file_if_exists(tmpfile)

        if (found_f006) then
            error stop &
                "Failed: F006 should not be triggered for unused parameter-only cases"
        end if

        print *, "[OK] Unused parameter"
    end subroutine test_unused_parameter

    subroutine test_loop_control_variables()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: i
        logical :: found_f006

        call make_temp_fortran_path("fluff_test_f006_loop", tmpfile)

        test_code = "program nested"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i, j, k"//new_line('a')// &
                    "    do i = 1, 10"//new_line('a')// &
                    "        do j = 1, 10"//new_line('a')// &
                    "            do k = 1, 10"//new_line('a')// &
                    "                print *, i, j, k"//new_line('a')// &
                    "            end do"//new_line('a')// &
                    "        end do"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program nested"

        linter = create_linter_engine()

        call write_text_file(tmpfile, test_code)

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

        call delete_file_if_exists(tmpfile)

        if (found_f006) then
            error stop "Failed: F006 should not trigger for loop control variables"
        end if

        print *, "[OK] Loop control variables"
    end subroutine test_loop_control_variables

end program test_rule_f006_unused_variable
