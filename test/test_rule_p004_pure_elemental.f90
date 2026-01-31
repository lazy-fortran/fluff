program test_rule_p004_pure_elemental
    ! Test P004: Missing pure/elemental declarations rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code, &
                            lint_file_checked
    implicit none

    print *, "Testing P004: Missing pure/elemental declarations rule..."

    call test_missing_pure_triggers()
    call test_already_pure_is_ok()
    call test_already_elemental_is_ok()
    call test_io_prevents_pure_suggestion()
    call test_allocate_prevents_pure_suggestion()
    call test_subroutine_call_prevents_pure()
    call test_pure_intrinsic_call_allows_pure()

    print *, "[OK] All P004 tests passed!"

contains

    subroutine test_missing_pure_triggers()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "print *, f(1.0)"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "real function f(x)"//new_line('a')// &
                    "    real, intent(in) :: x"//new_line('a')// &
                    "    f = x + 1.0"//new_line('a')// &
                    "end function f"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p004_bad", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code( &
            diagnostics, &
            "P004", &
            .true., &
            "missing pure on side-effect-free function should be "// &
            "flagged")
        print *, "[OK] Missing pure"
    end subroutine test_missing_pure_triggers

    subroutine test_already_pure_is_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "print *, f(1.0)"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "pure real function f(x)"//new_line('a')// &
                    "    real, intent(in) :: x"//new_line('a')// &
                    "    f = x + 1.0"//new_line('a')// &
                    "end function f"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p004_ok", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P004", .false., &
                                        "pure function should not be flagged")
        print *, "[OK] Already pure"
    end subroutine test_already_pure_is_ok

    subroutine test_already_elemental_is_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "print *, f(1.0)"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "elemental real function f(x)"//new_line('a')// &
                    "    real, intent(in) :: x"//new_line('a')// &
                    "    f = x + 1.0"//new_line('a')// &
                    "end function f"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p004_elem", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P004", .false., &
                                        "elemental function should not be flagged")
        print *, "[OK] Already elemental"
    end subroutine test_already_elemental_is_ok

    subroutine test_io_prevents_pure_suggestion()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "call show(1.0)"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "subroutine show(x)"//new_line('a')// &
                    "    real, intent(in) :: x"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end subroutine show"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p004_io", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P004", .false., &
                                        "procedure with I/O should not be flagged")
        print *, "[OK] I/O prevents pure suggestion"
    end subroutine test_io_prevents_pure_suggestion

    subroutine test_allocate_prevents_pure_suggestion()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "call alloc_arr()"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "subroutine alloc_arr()"//new_line('a')// &
                    "    real, allocatable :: a(:)"//new_line('a')// &
                    "    allocate(a(10))"//new_line('a')// &
                    "end subroutine alloc_arr"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p004_alloc", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P004", .false., &
                                        "procedure with allocate should not be flagged")
        print *, "[OK] Allocate prevents pure suggestion"
    end subroutine test_allocate_prevents_pure_suggestion

    subroutine test_subroutine_call_prevents_pure()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "call wrapper()"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "subroutine wrapper()"//new_line('a')// &
                    "    call external_proc()"//new_line('a')// &
                    "end subroutine wrapper"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p004_call", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P004", .false., &
                                 "procedure with subroutine call should not be flagged")
        print *, "[OK] Subroutine call prevents pure suggestion"
    end subroutine test_subroutine_call_prevents_pure

    subroutine test_pure_intrinsic_call_allows_pure()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "print *, compute(1.0)"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "real function compute(x)"//new_line('a')// &
                    "    real, intent(in) :: x"//new_line('a')// &
                    "    compute = sin(x) + cos(x)"//new_line('a')// &
                    "end function compute"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p004_intr", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P004", .true., &
                                      "function with pure intrinsics should be flagged")
        print *, "[OK] Pure intrinsic calls allow pure suggestion"
    end subroutine test_pure_intrinsic_call_allows_pure

end program test_rule_p004_pure_elemental
