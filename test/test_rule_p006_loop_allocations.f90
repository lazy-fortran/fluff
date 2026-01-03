program test_rule_p006_loop_allocations
    ! Test P006: Unnecessary allocations in loops rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code, &
                            lint_file_checked
    implicit none

    print *, "Testing P006: Unnecessary allocations in loops rule..."

    call test_allocate_inside_loop_triggers()
    call test_allocate_outside_loop_is_ok()

    print *, "All P006 tests passed!"

contains

    subroutine test_allocate_inside_loop_triggers()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "integer :: i"//new_line('a')// &
                    "real, allocatable :: a(:)"//new_line('a')// &
                    "do i = 1, 10"//new_line('a')// &
                    "    allocate(a(10))"//new_line('a')// &
                    "    a = real(i)"//new_line('a')// &
                    "    deallocate(a)"//new_line('a')// &
                    "end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p006_bad", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P006", .true., &
                                        "allocate inside loop should be flagged")
        print *, "  + Allocate inside loop"
    end subroutine test_allocate_inside_loop_triggers

    subroutine test_allocate_outside_loop_is_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "integer :: i"//new_line('a')// &
                    "real, allocatable :: a(:)"//new_line('a')// &
                    "allocate(a(10))"//new_line('a')// &
                    "do i = 1, 10"//new_line('a')// &
                    "    a = real(i)"//new_line('a')// &
                    "end do"//new_line('a')// &
                    "deallocate(a)"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p006_ok", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P006", .false., &
                                        "allocate outside loop should not be flagged")
        print *, "  + Allocate outside loop"
    end subroutine test_allocate_outside_loop_is_ok

end program test_rule_p006_loop_allocations
