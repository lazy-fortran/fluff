program test_rule_f009_inconsistent_intent
    ! Test F009: Inconsistent intent usage rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code, &
                            lint_file_checked
    implicit none

    print *, "Testing F009: Inconsistent intent usage rule..."

    call test_intent_in_modified()
    call test_intent_in_not_modified()
    call test_intent_out_unassigned()
    call test_intent_out_assigned()

    print *, "[OK] All F009 tests passed!"

contains

    subroutine test_intent_in_modified()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "subroutine calc(x)"//new_line('a')// &
                    "    real, intent(in) :: x"//new_line('a')// &
                    "    x = x + 1.0"//new_line('a')// &
                    "end subroutine calc"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_f009_in_bad", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "F009", .true., &
                                        "intent(in) assignment should be flagged")
        print *, "[OK] Intent(in) modified"
    end subroutine test_intent_in_modified

    subroutine test_intent_in_not_modified()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "subroutine calc(x, y)"//new_line('a')// &
                    "    real, intent(in) :: x"//new_line('a')// &
                    "    real, intent(out) :: y"//new_line('a')// &
                    "    y = x + 1.0"//new_line('a')// &
                    "end subroutine calc"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_f009_in_ok", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "F009", .false., &
                                        "no intent violations expected")
        print *, "[OK] Intent(in) not modified"
    end subroutine test_intent_in_not_modified

    subroutine test_intent_out_unassigned()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "subroutine calc(y)"//new_line('a')// &
                    "    real, intent(out) :: y"//new_line('a')// &
                    "end subroutine calc"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_f009_out_bad", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "F009", .true., &
                                        "intent(out) without assignment should be "// &
                                        "flagged")
        print *, "[OK] Intent(out) unassigned"
    end subroutine test_intent_out_unassigned

    subroutine test_intent_out_assigned()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "subroutine calc(y)"//new_line('a')// &
                    "    real, intent(out) :: y"//new_line('a')// &
                    "    y = 1.0"//new_line('a')// &
                    "end subroutine calc"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_f009_out_ok", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "F009", .false., &
                                        "no intent violations expected")
        print *, "[OK] Intent(out) assigned"
    end subroutine test_intent_out_assigned

end program test_rule_f009_inconsistent_intent
