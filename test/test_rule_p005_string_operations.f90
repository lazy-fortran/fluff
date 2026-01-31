program test_rule_p005_string_operations
    ! Test P005: Inefficient string operations rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code, &
                            lint_file_checked
    implicit none

    print *, "Testing P005: Inefficient string operations rule..."

    call test_loop_concatenation_triggers()
    call test_no_concatenation_is_ok()
    call test_concat_outside_loop_ok()
    call test_declared_char_concat_in_loop()

    print *, "[OK] All P005 tests passed!"

contains

    subroutine test_loop_concatenation_triggers()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
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
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code( &
            diagnostics, &
            "P005", &
            .true., &
            "string concatenation in loop should be flagged")
        print *, "[OK] Loop concatenation"
    end subroutine test_loop_concatenation_triggers

    subroutine test_no_concatenation_is_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
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
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P005", .false., &
                                        "no concatenation should not be flagged")
        print *, "[OK] No concatenation"
    end subroutine test_no_concatenation_is_ok

    subroutine test_concat_outside_loop_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "character(len=:), allocatable :: s"//new_line('a')// &
                    "s = 'hello' // ' world'"//new_line('a')// &
                    "print *, s"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p005_out", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P005", .false., &
                                        "concat outside loop should not be flagged")
        print *, "[OK] Concat outside loop"
    end subroutine test_concat_outside_loop_ok

    subroutine test_declared_char_concat_in_loop()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "integer :: i"//new_line('a')// &
                    "character(len=100) :: msg"//new_line('a')// &
                    "character(len=10) :: prefix"//new_line('a')// &
                    "prefix = 'item '"//new_line('a')// &
                    "do i = 1, 5"//new_line('a')// &
                    "    msg = prefix // 'data'"//new_line('a')// &
                    "end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p005_decl", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P005", .true., &
                                       "declared char concat in loop should be flagged")
        print *, "[OK] Declared char concat in loop"
    end subroutine test_declared_char_concat_in_loop

end program test_rule_p005_string_operations
