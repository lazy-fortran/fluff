program test_rule_p007_mixed_precision
    ! Test P007: Mixed precision arithmetic rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code
    implicit none

    print *, "Testing P007: Mixed precision arithmetic rule..."

    call test_mixed_precision_triggers()
    call test_consistent_precision_is_ok()

    print *, "All P007 tests passed!"

contains

    subroutine test_mixed_precision_triggers()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "real :: a"//new_line('a')// &
                    "real(8) :: b"//new_line('a')// &
                    "real(8) :: r"//new_line('a')// &
                    "r = a + b"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p007_bad", path)
        call write_text_file(path, test_code)
        call linter%lint_file(path, diagnostics, error_msg)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P007", .true., &
                                        "mixed precision binary op should be flagged")
        print *, "  + Mixed precision"
    end subroutine test_mixed_precision_triggers

    subroutine test_consistent_precision_is_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "real(8) :: a"//new_line('a')// &
                    "real(8) :: b"//new_line('a')// &
                    "real(8) :: r"//new_line('a')// &
                    "r = a + b"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p007_ok", path)
        call write_text_file(path, test_code)
        call linter%lint_file(path, diagnostics, error_msg)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P007", .false., &
                                        "consistent precision should not be flagged")
        print *, "  + Consistent precision"
    end subroutine test_consistent_precision_is_ok

end program test_rule_p007_mixed_precision
