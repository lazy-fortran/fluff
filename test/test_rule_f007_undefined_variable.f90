program test_rule_f007_undefined_variable
    ! Test F007: Undefined variable usage rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked
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

    print *, "[OK] All F007 tests passed!"

contains

    subroutine test_undefined_variable()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: i
        logical :: found_f007

        call make_temp_fortran_path("fluff_test_f007", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = y + 10"//new_line('a')// &  ! y is undefined
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call write_text_file(tmpfile, test_code)

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

        call delete_file_if_exists(tmpfile)

        if (.not. found_f007) then
            error stop "Failed: F007 should be triggered for undefined variable"
        end if

        print *, "[OK] Undefined variable usage"

    end subroutine test_undefined_variable

    subroutine test_defined_variable()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: i
        logical :: found_f007

        call make_temp_fortran_path("fluff_test_f007_ok", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y"//new_line('a')// &
                    "    y = 20"//new_line('a')// &
                    "    x = y + 10"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call write_text_file(tmpfile, test_code)

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

        call delete_file_if_exists(tmpfile)

        if (found_f007) then
            error stop "Failed: F007 should not be triggered when variables are defined"
        end if

        print *, "[OK] Defined variable"

    end subroutine test_defined_variable

    subroutine test_multiple_undefined()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: i, f007_count

        call make_temp_fortran_path("fluff_test_f007_multi", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = y + z"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call write_text_file(tmpfile, test_code)

        call lint_file_checked(linter, tmpfile, diagnostics)

        f007_count = 0
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F007") f007_count = f007_count + 1
            end do
        end if

        call delete_file_if_exists(tmpfile)

        if (f007_count < 2) then
            error stop &
                "Failed: expected 2+ F007 diagnostics for multiple undefined variables"
        end if

        print *, "[OK] Multiple undefined variables"
    end subroutine test_multiple_undefined

    subroutine test_scope_visibility()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: i
        logical :: found_f007

        call make_temp_fortran_path("fluff_test_f007_scope", tmpfile)

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

        call write_text_file(tmpfile, test_code)

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

        call delete_file_if_exists(tmpfile)

        if (.not. found_f007) then
            error stop "Failed: expected F007 for out-of-scope variable"
        end if

        print *, "[OK] Variable scope visibility"
    end subroutine test_scope_visibility

end program test_rule_f007_undefined_variable
