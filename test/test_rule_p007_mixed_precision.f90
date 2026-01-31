program test_rule_p007_mixed_precision
    ! Test P007: Mixed precision arithmetic rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code, &
                            lint_file_checked
    implicit none

    print *, "Testing P007: Mixed precision arithmetic rule..."

    call test_mixed_precision_triggers()
    call test_consistent_precision_is_ok()
    call test_double_precision_consistency()
    call test_literal_kind_detection()
    call test_intrinsic_kind_propagation()

    print *, "[OK] All P007 tests passed!"

contains

    subroutine test_mixed_precision_triggers()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
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
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P007", .true., &
                                        "mixed precision binary op should be flagged")
        print *, "[OK] Mixed precision"
    end subroutine test_mixed_precision_triggers

    subroutine test_consistent_precision_is_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
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
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P007", .false., &
                                        "consistent precision should not be flagged")
        print *, "[OK] Consistent precision"
    end subroutine test_consistent_precision_is_ok

    subroutine test_double_precision_consistency()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "double precision :: a"//new_line('a')// &
                    "double precision :: b"//new_line('a')// &
                    "double precision :: r"//new_line('a')// &
                    "r = a * b"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p007_dp", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P007", .false., &
                                   "double precision consistency should not be flagged")
        print *, "[OK] Double precision consistency"
    end subroutine test_double_precision_consistency

    subroutine test_literal_kind_detection()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "real :: a"//new_line('a')// &
                    "real :: r"//new_line('a')// &
                    "r = a + 1.0d0"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p007_lit", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P007", .true., &
                                "d exponent literal mixing with real should be flagged")
        print *, "[OK] Literal kind detection"
    end subroutine test_literal_kind_detection

    subroutine test_intrinsic_kind_propagation()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "real(8) :: a"//new_line('a')// &
                    "real(8) :: r"//new_line('a')// &
                    "r = sin(a) + cos(a)"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p007_intr", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P007", .false., &
                                  "intrinsic with same kind args should not be flagged")
        print *, "[OK] Intrinsic kind propagation"
    end subroutine test_intrinsic_kind_propagation

end program test_rule_p007_mixed_precision
