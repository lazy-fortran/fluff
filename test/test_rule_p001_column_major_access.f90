program test_rule_p001_column_major_access
    ! Test P001: Column-major array access in nested loops
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code, &
                            assert_diagnostic_location
    implicit none

    print *, "Testing P001: Column-major array access rule..."

    call test_2d_incorrect_order_triggers()
    call test_2d_correct_order_ok()
    call test_3d_incorrect_order_triggers()
    call test_non_array_call_ok()

    print *, "All P001 tests passed!"

contains

    subroutine test_2d_incorrect_order_triggers()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer, parameter :: n = 10, m = 10"//new_line('a')// &
                    "    real :: matrix(n, m)"//new_line('a')// &
                    "    integer :: i, j"//new_line('a')// &
                    "    do i = 1, n"//new_line('a')// &
                    "        do j = 1, m"//new_line('a')// &
                    "            matrix(i, j) = real(i * j)"//new_line('a')// &
                    "        end do"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p001_bad_2d", path)
        call write_text_file(path, test_code)
        call linter%lint_file(path, diagnostics, error_msg)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P001", .true., &
                                        "outer loop varying leftmost index")
        call assert_diagnostic_location(diagnostics, "P001", 8, 24, &
                                        "P001 should point at array reference")
        print *, "  + 2D incorrect ordering triggers"
    end subroutine test_2d_incorrect_order_triggers

    subroutine test_2d_correct_order_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer, parameter :: n = 10, m = 10"//new_line('a')// &
                    "    real :: matrix(n, m)"//new_line('a')// &
                    "    integer :: i, j"//new_line('a')// &
                    "    do j = 1, m"//new_line('a')// &
                    "        do i = 1, n"//new_line('a')// &
                    "            matrix(i, j) = real(i * j)"//new_line('a')// &
                    "        end do"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p001_ok_2d", path)
        call write_text_file(path, test_code)
        call linter%lint_file(path, diagnostics, error_msg)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P001", .false., &
                                        "innermost loop varies leftmost index")
        print *, "  + 2D correct ordering ok"
    end subroutine test_2d_correct_order_ok

    subroutine test_3d_incorrect_order_triggers()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer, parameter :: n = 4, m = 4, p = 4"//new_line('a')// &
                    "    real :: a(n, m, p)"//new_line('a')// &
                    "    integer :: i, j, k"//new_line('a')// &
                    "    do i = 1, n"//new_line('a')// &
                    "        do j = 1, m"//new_line('a')// &
                    "            do k = 1, p"//new_line('a')// &
                    "                a(i, j, k) = real(i + j + k)"//new_line('a')// &
                    "            end do"//new_line('a')// &
                    "        end do"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p001_bad_3d", path)
        call write_text_file(path, test_code)
        call linter%lint_file(path, diagnostics, error_msg)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P001", .true., &
                                        "3D incorrect ordering triggers")
        call assert_diagnostic_location(diagnostics, "P001", 9, 26, &
                                        "P001 should point at array reference")
        print *, "  + 3D incorrect ordering triggers"
    end subroutine test_3d_incorrect_order_triggers

    subroutine test_non_array_call_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer, parameter :: n = 10, m = 10"//new_line('a')// &
                    "    real :: x"//new_line('a')// &
                    "    integer :: i, j"//new_line('a')// &
                    "    do i = 1, n"//new_line('a')// &
                    "        do j = 1, m"//new_line('a')// &
                    "            x = f(i, j)"//new_line('a')// &
                    "        end do"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "    real function f(i, j)"//new_line('a')// &
                    "        integer, intent(in) :: i, j"//new_line('a')// &
                    "        f = real(i + j)"//new_line('a')// &
                    "    end function f"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p001_call_ok", path)
        call write_text_file(path, test_code)
        call linter%lint_file(path, diagnostics, error_msg)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P001", .false., &
                                        "function call should not be flagged")
        print *, "  + Function call not flagged"
    end subroutine test_non_array_call_ok

end program test_rule_p001_column_major_access
