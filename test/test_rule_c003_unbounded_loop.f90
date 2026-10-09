program test_rule_c003_unbounded_loop
    ! C003: Loop with no proven bound.
    ! The rule is off by default, so every test enables it via --select C003.
    use fluff_config, only: fluff_config_t, create_default_config
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
        delete_file_if_exists, assert_has_diagnostic_code, lint_file_checked
    implicit none

    print *, "Testing C003: loop with no proven bound (correctness rule)..."

    call test_do_while_counter_bounded_silent()
    call test_do_loop_constant_step_silent()
    call test_do_while_unbounded_reports()
    call test_do_loop_variable_step_reports()
    call test_rule_runs_when_selected()

    print *, "[OK] All C003 tests passed!"

contains

    function make_c003_linter() result(linter)
        type(linter_engine_t) :: linter
        linter = create_linter_engine()
        allocate (character(len=3) :: linter%config%rules%select(1))
        linter%config%rules%select(1) = "C003"
    end function make_c003_linter

    subroutine test_do_while_counter_bounded_silent()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    integer :: cnt, n, i"//new_line('a')// &
            "    real :: y"//new_line('a')// &
            "    cnt = 0"//new_line('a')// &
            "    do while (cnt < n)"//new_line('a')// &
            "        cnt = cnt + 1"//new_line('a')// &
            "        y = y + 1"//new_line('a')// &
            "    end do"//new_line('a')// &
            "    print *, y"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c003_counter", tmpfile)
        linter = make_c003_linter()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C003", .false., &
            "C003: do while with counter increasing toward bound is proven")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] Counter-bounded do while silent"
    end subroutine test_do_while_counter_bounded_silent

    subroutine test_do_loop_constant_step_silent()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    integer :: i, n"//new_line('a')// &
            "    real :: y"//new_line('a')// &
            "    do i = 1, n"//new_line('a')// &
            "        y = y + 1"//new_line('a')// &
            "    end do"//new_line('a')// &
            "    print *, y"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c003_doloop", tmpfile)
        linter = make_c003_linter()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C003", .false., &
            "C003: regular do loop with default constant step is proven bounded")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] Constant-step do loop silent"
    end subroutine test_do_loop_constant_step_silent

    subroutine test_do_while_unbounded_reports()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    real :: x, tol"//new_line('a')// &
            "    do while (x > tol)"//new_line('a')// &
            "        x = x * 0.5"//new_line('a')// &
            "    end do"//new_line('a')// &
            "    print *, x"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c003_unbounded", tmpfile)
        linter = make_c003_linter()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C003", .true., &
            "C003: do while with no monotone counter toward bound must report")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] Unbounded do while reports"
    end subroutine test_do_while_unbounded_reports

    subroutine test_do_loop_variable_step_reports()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    integer :: i, n, step"//new_line('a')// &
            "    real :: y"//new_line('a')// &
            "    step = 1"//new_line('a')// &
            "    do i = 1, n, step"//new_line('a')// &
            "        y = y + 1"//new_line('a')// &
            "    end do"//new_line('a')// &
            "    print *, y"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c003_varstep", tmpfile)
        linter = make_c003_linter()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C003", .true., &
            "C003: do loop with variable (possibly zero) step must report")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] Variable-step do loop reports"
    end subroutine test_do_loop_variable_step_reports

    subroutine test_rule_runs_when_selected()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    real :: x, tol"//new_line('a')// &
            "    do while (x > tol)"//new_line('a')// &
            "        x = x * 0.5"//new_line('a')// &
            "    end do"//new_line('a')// &
            "    print *, x"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c003_default", tmpfile)
        ! default config: C003 off by default -> no report
        linter = create_linter_engine()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C003", .false., &
            "C003: off by default (must not report without --select)")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] C003 off by default"
    end subroutine test_rule_runs_when_selected

end program test_rule_c003_unbounded_loop
