program test_rule_p005_string_operations
    ! Test P005: Inefficient string operations rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code
    implicit none

    print *, "Testing P005: Inefficient string operations rule..."

    call test_loop_concatenation_triggers()
    call test_no_concatenation_is_ok()

    print *, "All P005 tests passed!"

contains

    subroutine test_loop_concatenation_triggers()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "integer :: i"//new_line('a')// &
                    "character(len=:), allocatable :: s"//new_line('a')// &
                    "s = ''"//new_line('a')// &
                    "do i = 1, 10"//new_line('a')// &
                    "    s = s // 'a'"//new_line('a')// &
                    "end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p005_bad", path)
        call write_text_file(path, test_code)
        call linter%lint_file(path, diagnostics, error_msg)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P005", .true., &
                                       "string concatenation in loop should be flagged")
        print *, "  + Loop concatenation"
    end subroutine test_loop_concatenation_triggers

    subroutine test_no_concatenation_is_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "integer :: i"//new_line('a')// &
                    "character(len=10) :: s"//new_line('a')// &
                    "s = ''"//new_line('a')// &
                    "do i = 1, 10"//new_line('a')// &
                    "    s(i:i) = 'a'"//new_line('a')// &
                    "end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p005_ok", path)
        call write_text_file(path, test_code)
        call linter%lint_file(path, diagnostics, error_msg)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P005", .false., &
                                        "no concatenation should not be flagged")
        print *, "  + No concatenation"
    end subroutine test_no_concatenation_is_ok

end program test_rule_p005_string_operations
