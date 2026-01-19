program test_rule_p003_array_temporaries
    ! Test P003: Unnecessary array temporaries rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code, &
                            lint_file_checked
    implicit none

    print *, "Testing P003: Unnecessary array temporaries rule..."

    call test_whole_array_expression_triggers()
    call test_elemental_loop_is_ok()

    print *, "[OK] All P003 tests passed!"

contains

    subroutine test_whole_array_expression_triggers()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "real :: a(10), b(10), c(10)"//new_line('a')// &
                    "c = a + b"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p003_bad", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P003", .true., &
                                        "whole-array expression should be flagged")
        print *, "[OK] Whole-array expression"
    end subroutine test_whole_array_expression_triggers

    subroutine test_elemental_loop_is_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "integer :: i"//new_line('a')// &
                    "real :: a(10), b(10), c(10)"//new_line('a')// &
                    "do i = 1, 10"//new_line('a')// &
                    "    c(i) = a(i) + b(i)"//new_line('a')// &
                    "end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p003_ok", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P003", .false., &
                                        "element-wise loop should not be flagged")
        print *, "[OK] Element-wise loop"
    end subroutine test_elemental_loop_is_ok

end program test_rule_p003_array_temporaries
