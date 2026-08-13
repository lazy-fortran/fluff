program test_rule_c002_unguarded_division
    ! C002: Division by a value not proven non-zero.
    ! The rule is off by default, so every test enables it via --select C002.
    use fluff_config, only: fluff_config_t, create_default_config
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
        delete_file_if_exists, assert_has_diagnostic_code, lint_file_checked
    implicit none

    print *, "Testing C002: unguarded division (correctness rule)..."

    call test_literal_denominator_silent()
    call test_parameter_denominator_silent()
    call test_guarded_division_silent()
    call test_error_norm_shape_reports()
    call test_plain_variable_division_reports()
    call test_constant_reaching_definition_silent()
    call test_rule_runs_when_selected()

    print *, "[OK] All C002 tests passed!"

contains

    ! Enable only C002 on a fresh linter.
    function make_c002_linter() result(linter)
        type(linter_engine_t) :: linter
        linter = create_linter_engine()
        allocate (character(len=3) :: linter%config%rules%select(1))
        linter%config%rules%select(1) = "C002"
    end function make_c002_linter

    subroutine test_literal_denominator_silent()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    real :: x, y"//new_line('a')// &
            "    x = 2.0"//new_line('a')// &
            "    y = x / 3.5"//new_line('a')// &
            "    print *, y"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c002_lit", tmpfile)
        linter = make_c002_linter()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C002", .false., &
            "C002: literal non-zero denominator is proven non-zero")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] Literal denominator silent"
    end subroutine test_literal_denominator_silent

    subroutine test_parameter_denominator_silent()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    real, parameter :: pi = 3.14159"//new_line('a')// &
            "    real :: x, y"//new_line('a')// &
            "    x = 2.0"//new_line('a')// &
            "    y = x / pi"//new_line('a')// &
            "    print *, y"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c002_param", tmpfile)
        linter = make_c002_linter()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C002", .false., &
            "C002: parameter with non-zero value is proven non-zero")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] Parameter denominator silent"
    end subroutine test_parameter_denominator_silent

    subroutine test_guarded_division_silent()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    real :: x, y, sc"//new_line('a')// &
            "    x = 2.0"//new_line('a')// &
            "    sc = 0.0"//new_line('a')// &
            "    if (sc /= 0.0) then"//new_line('a')// &
            "        y = x / sc"//new_line('a')// &
            "    end if"//new_line('a')// &
            "    print *, y"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c002_guard", tmpfile)
        linter = make_c002_linter()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C002", .false., &
            "C002: division guarded by sc /= 0 is proven non-zero")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] Guarded division silent"
    end subroutine test_guarded_division_silent

    subroutine test_error_norm_shape_reports()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    real :: atol, rtol, sc, err5"//new_line('a')// &
            "    integer :: i"//new_line('a')// &
            "    real :: y(10)"//new_line('a')// &
            "    sc = atol + rtol"//new_line('a')// &
            "    y(i) = err5 / sc"//new_line('a')// &
            "    print *, y"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c002_errnorm", tmpfile)
        linter = make_c002_linter()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C002", .true., &
            "C002: error_norm shape (sum of args as denominator) must report")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] error_norm shape reports"
    end subroutine test_error_norm_shape_reports

    subroutine test_plain_variable_division_reports()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    real :: x, y, d"//new_line('a')// &
            "    x = 2.0"//new_line('a')// &
            "    y = x / d"//new_line('a')// &
            "    print *, y"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c002_plain", tmpfile)
        linter = make_c002_linter()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C002", .true., &
            "C002: uninitialised denominator must report")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] Plain variable division reports"
    end subroutine test_plain_variable_division_reports

    subroutine test_constant_reaching_definition_silent()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    real :: x, y, d"//new_line('a')// &
            "    x = 2.0"//new_line('a')// &
            "    d = 5.0"//new_line('a')// &
            "    y = x / d"//new_line('a')// &
            "    print *, y"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c002_def", tmpfile)
        linter = make_c002_linter()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C002", .false., &
            "C002: denominator assigned a non-zero constant is proven non-zero")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] Constant reaching definition silent"
    end subroutine test_constant_reaching_definition_silent

    ! The rule ships off by default, so a plain linter must NOT report. This
    ! proves the rule only runs when explicitly selected (and that the enabled
    ! path is what the positive tests exercise).
    subroutine test_rule_runs_when_selected()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        test_code = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    real :: x, y, d"//new_line('a')// &
            "    x = 2.0"//new_line('a')// &
            "    y = x / d"//new_line('a')// &
            "    print *, y"//new_line('a')// &
            "end program test"

        call make_temp_fortran_path("fluff_test_c002_default", tmpfile)
        ! default config: C002 off by default -> no report
        linter = create_linter_engine()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)
        call assert_has_diagnostic_code(diagnostics, "C002", .false., &
            "C002: off by default (must not report without --select)")
        call delete_file_if_exists(tmpfile)

        print *, "[OK] C002 off by default"
    end subroutine test_rule_runs_when_selected

end program test_rule_c002_unguarded_division
