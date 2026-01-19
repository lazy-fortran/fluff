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

end program test_rule_p004_pure_elemental
