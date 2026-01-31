program test_long_line_performance
    use, intrinsic :: iso_fortran_env, only: int64
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked
    implicit none

    print *, "Testing long line performance (fixes #206)..."
    call test_very_long_line_completes_quickly()
    call test_many_long_lines_completes_quickly()
    print *, "[OK] All long line performance tests passed!"

contains

    subroutine test_very_long_line_completes_quickly()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: temp_path
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: long_comment
        integer(int64) :: start_time, end_time, clock_rate
        real :: elapsed_seconds
        real, parameter :: max_allowed_seconds = 2.0

        call make_temp_fortran_path("fluff_test_long_line", temp_path)
        linter = create_linter_engine()

        long_comment = "! " // repeat("x", 1500)

        test_code = "program test" // new_line('a') // &
                    "    implicit none" // new_line('a') // &
                    long_comment // new_line('a') // &
                    "end program test"

        call write_text_file(temp_path, test_code)

        call system_clock(start_time, clock_rate)
        call lint_file_checked(linter, temp_path, diagnostics)
        call system_clock(end_time)

        elapsed_seconds = real(end_time - start_time) / real(clock_rate)

        call delete_file_if_exists(temp_path)

        if (elapsed_seconds > max_allowed_seconds) then
            print *, "FAILED: Linting took", elapsed_seconds, "seconds"
            print *, "Expected under", max_allowed_seconds, "seconds"
            error stop "Long line processing too slow"
        end if

        print *, "[OK] Very long line (1500 chars) processed in", elapsed_seconds, "seconds"
    end subroutine test_very_long_line_completes_quickly

    subroutine test_many_long_lines_completes_quickly()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: temp_path
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: long_line
        integer(int64) :: start_time, end_time, clock_rate
        real :: elapsed_seconds
        real, parameter :: max_allowed_seconds = 5.0
        integer :: i
        integer, parameter :: num_long_lines = 50

        call make_temp_fortran_path("fluff_test_many_long", temp_path)
        linter = create_linter_engine()

        long_line = "    x = " // repeat("a + ", 200) // "b"

        test_code = "program test" // new_line('a') // &
                    "    implicit none" // new_line('a') // &
                    "    real :: x, a, b" // new_line('a')

        do i = 1, num_long_lines
            test_code = test_code // long_line // new_line('a')
        end do

        test_code = test_code // "end program test"

        call write_text_file(temp_path, test_code)

        call system_clock(start_time, clock_rate)
        call lint_file_checked(linter, temp_path, diagnostics)
        call system_clock(end_time)

        elapsed_seconds = real(end_time - start_time) / real(clock_rate)

        call delete_file_if_exists(temp_path)

        if (elapsed_seconds > max_allowed_seconds) then
            print *, "FAILED: Linting took", elapsed_seconds, "seconds"
            print *, "Expected under", max_allowed_seconds, "seconds"
            error stop "Many long lines processing too slow"
        end if

        if (.not. allocated(diagnostics)) then
            error stop "Expected F003 diagnostics for long lines"
        end if

        if (size(diagnostics) < num_long_lines) then
            print *, "Warning: Expected at least", num_long_lines, "F003 diagnostics"
        end if

        print *, "[OK] Many long lines (", num_long_lines, " lines of 800+ chars)"
        print *, "     processed in", elapsed_seconds, "seconds with", &
                 size(diagnostics), "diagnostics"
    end subroutine test_many_long_lines_completes_quickly

end program test_long_line_performance
