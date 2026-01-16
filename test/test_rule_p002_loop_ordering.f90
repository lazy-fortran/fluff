program test_rule_p002_loop_ordering
    ! Test P002: Inefficient loop ordering rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code, &
                            lint_file_checked
    implicit none

    print *, "Testing P002: Inefficient loop ordering rule..."

    ! Test 1: Column-major inefficient ordering (should trigger)
    call test_column_major_inefficient()

    ! Test 2: Row-major efficient ordering (should not trigger)
    call test_row_major_efficient()

    print *, "[OK] All P002 tests passed!"

contains

    subroutine test_column_major_inefficient()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer, parameter :: n = 10, m = 10"//new_line('a')// &
                    "    real :: matrix(n, m)"//new_line('a')// &
                    "    integer :: i, j"//new_line('a')// &
                    "    "//new_line('a')// &
                    "    ! Inefficient: accessing by rows in "//new_line('a')// &
                    "    ! column-major Fortran"//new_line('a')// &
                    "    do i = 1, n"//new_line('a')// &
                    "        do j = 1, m"//new_line('a')// &
                    "            matrix(i, j) = real(i * j)"//new_line('a')// &
                    "        end do"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_p002_bad", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P002", .true., &
                                        "inefficient loop ordering should be flagged")
        print *, "[OK] Column-major inefficient ordering"

    end subroutine test_column_major_inefficient

    subroutine test_row_major_efficient()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer, parameter :: n = 10, m = 10"//new_line('a')// &
                    "    real :: matrix(n, m)"//new_line('a')// &
                    "    integer :: i, j"//new_line('a')// &
                    "    "//new_line('a')// &
                    "    ! Efficient: accessing by columns in "//new_line('a')// &
                    "    ! column-major Fortran"//new_line('a')// &
                    "    do j = 1, m"//new_line('a')// &
                    "        do i = 1, n"//new_line('a')// &
                    "            matrix(i, j) = real(i * j)"//new_line('a')// &
                    "        end do"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_p002_ok", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P002", .false., &
                                        "efficient loop ordering should not be flagged")
        print *, "[OK] Row-major efficient ordering"

    end subroutine test_row_major_efficient

end program test_rule_p002_loop_ordering
