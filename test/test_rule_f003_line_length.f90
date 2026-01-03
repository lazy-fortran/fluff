program test_rule_f003_line_length
    ! Test F003: Line too long rule
    use fluff_config, only: create_default_config, fluff_config_t
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use fluff_visual_columns, only: visual_columns
    implicit none

    print *, "Testing F003: Line too long rule..."

    ! Test 1: Line exceeding default limit (should trigger)
    call test_line_too_long()

    ! Test 2: Line within limit (should not trigger)
    call test_line_within_limit()

    ! Test 3: Continuation lines handled correctly
    call test_continuation_lines()

    ! Test 4: Comments at different lengths
    call test_comment_lines()

    ! Test 5: Custom line length configuration
    call test_custom_line_length()

    ! Test 6: Tabs count toward visual columns
    call test_tabs_column_calculation()

    print *, "All F003 tests passed!"

contains

    subroutine test_line_too_long()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=200) :: long_line
        character(len=*), parameter :: filename = &
                                       "/tmp/fluff_test_f003_line_too_long.f90"
        integer :: i, expected_end_col
        logical :: found_f003, found_location

        ! Create a line that's definitely too long (> 88 characters)
        long_line = "    real :: very_long_variable_name_that_exceeds_the_maximum_"// &
                    "line_length_limit_of_88_characters_in_fortran"
        expected_end_col = len_trim(long_line)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    trim(long_line)//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file=filename, status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file(filename, diagnostics, error_msg)

        ! Check for F003 violation
        found_f003 = .false.
        found_location = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F003") then
                    found_f003 = .true.
                    if (diagnostics(i)%location%start%line == 3 .and. &
                        diagnostics(i)%location%start%column == 89 .and. &
                        diagnostics(i)%location%end%line == 3 .and. &
                        diagnostics(i)%location%end%column == expected_end_col) then
                        found_location = .true.
                    end if
                    exit
                end if
            end do
        end if

        ! Clean up
        open (unit=99, file=filename, status="old")
        close (99, status="delete")

        if (.not. found_f003) then
            error stop "Failed: F003 should be triggered for lines exceeding "// &
                "length limit"
        end if

        if (.not. found_location) then
            error stop "Failed: F003 location should point to the overflow span"
        end if

        print *, "  ✓ Line too long"

    end subroutine test_line_too_long

    subroutine test_line_within_limit()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=*), parameter :: filename = "/tmp/fluff_test_f003_ok.f90"
        integer :: i
        logical :: found_f003

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    real :: x, y, z  ! This line is well within the 88 "// &
                    "character limit"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file=filename, status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file(filename, diagnostics, error_msg)

        ! Check for F003 violation
        found_f003 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F003") then
                    found_f003 = .true.
                    exit
                end if
            end do
        end if

        ! Clean up
        open (unit=99, file=filename, status="old")
        close (99, status="delete")

        if (found_f003) then
            error stop "Failed: F003 should not be triggered for lines within limit"
        end if

        print *, "  ✓ Line within limit"

    end subroutine test_line_within_limit

    subroutine test_continuation_lines()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=*), parameter :: filename = &
                                       "/tmp/fluff_test_f003_continuation.f90"
        integer :: i
        logical :: found_f003, found_location

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    real :: result = very_long_expression_that_exceeds_line_"// &
                    "limit + another_very_long_term"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file=filename, status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file(filename, diagnostics, error_msg)

        ! Check for F003 violation
        found_f003 = .false.
        found_location = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F003") then
                    found_f003 = .true.
                    if (diagnostics(i)%location%start%line == 3 .and. &
                        diagnostics(i)%location%start%column == 89 .and. &
                        diagnostics(i)%location%end%line == 3) then
                        found_location = .true.
                    end if
                    exit
                end if
            end do
        end if

        ! Clean up
        open (unit=99, file=filename, status="old")
        close (99, status="delete")

        if (.not. found_f003) then
            error stop "Failed: F003 should be triggered for long continuation "// &
                "line"
        end if

        if (.not. found_location) then
            error stop "Failed: F003 location should point to the overflow span"
        end if

        print *, "  ✓ Long physical line detected"

    end subroutine test_continuation_lines

    subroutine test_comment_lines()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=*), parameter :: filename = "/tmp/fluff_test_f003_comments.f90"
        integer :: i
        logical :: found_f003

        ! Comments should be ignored by F003
        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    ! This is a very long comment line that definitely "// &
                    "exceeds the 88 character limit but should be ignored by "// &
                    "F003"// &
                    new_line('a')// &
                    "    real :: x = 42"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file=filename, status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file(filename, diagnostics, error_msg)

        ! Check that F003 is NOT triggered for comment lines
        found_f003 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F003") then
                    found_f003 = .true.
                    exit
                end if
            end do
        end if

        ! Clean up
        open (unit=99, file=filename, status="old")
        close (99, status="delete")

        if (found_f003) then
            error stop "Failed: F003 should not be triggered for long comment "// &
                "lines"
        end if

        print *, "  ✓ Comment lines correctly ignored"

    end subroutine test_comment_lines

    subroutine test_custom_line_length()
        type(linter_engine_t) :: linter
        type(fluff_config_t) :: config
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=*), parameter :: filename = "/tmp/fluff_test_f003_custom.f90"
        integer :: i
        logical :: found_f003

        config = create_default_config()
        config%line_length = 20

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    real :: this_is_longer_than_twenty"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call linter%set_config(config)

        open (unit=99, file=filename, status="replace")
        write (99, '(A)') test_code
        close (99)

        call linter%lint_file(filename, diagnostics, error_msg)

        found_f003 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F003") then
                    if (diagnostics(i)%location%start%line == 3 .and. &
                        diagnostics(i)%location%start%column == 21) then
                        found_f003 = .true.
                    end if
                    exit
                end if
            end do
        end if

        open (unit=99, file=filename, status="old")
        close (99, status="delete")

        if (.not. found_f003) then
            error stop "Failed: F003 should respect configured line length"
        end if

        print *, "  ✓ Custom line length respected"
    end subroutine test_custom_line_length

    subroutine test_tabs_column_calculation()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: long_line
        character(len=*), parameter :: filename = "/tmp/fluff_test_f003_tabs.f90"
        integer :: i, expected_end_col
        logical :: found_f003, found_location

        long_line = achar(9)//"real :: x = 0.0 ! "//repeat("a", 90)
        expected_end_col = visual_columns(long_line)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    long_line//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        open (unit=99, file=filename, status="replace")
        write (99, '(A)') test_code
        close (99)

        call linter%lint_file(filename, diagnostics, error_msg)

        found_f003 = .false.
        found_location = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F003") then
                    found_f003 = .true.
                    if (diagnostics(i)%location%start%line == 3 .and. &
                        diagnostics(i)%location%start%column == 89 .and. &
                        diagnostics(i)%location%end%line == 3 .and. &
                        diagnostics(i)%location%end%column == expected_end_col) then
                        found_location = .true.
                    end if
                    exit
                end if
            end do
        end if

        open (unit=99, file=filename, status="old")
        close (99, status="delete")

        if (.not. found_f003) then
            error stop "Failed: F003 should be triggered for lines with tabs "// &
                "exceeding length limit"
        end if

        if (.not. found_location) then
            error stop "Failed: F003 location end column should account for tabs"
        end if

        print *, "  ✓ Tabs counted as visual columns"
    end subroutine test_tabs_column_calculation

end program test_rule_f003_line_length
