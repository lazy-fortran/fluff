program test_rule_f005_mixed_tabs_spaces
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: linter_engine_t, create_linter_engine
    use test_support, only: assert_equal_int
    implicit none

    print *, "Testing F005: Mixed tabs and spaces rule..."

    ! Test 1: Lines with mixed tabs and spaces (should trigger)
    call test_mixed_tabs_spaces()

    ! Test 2: Lines with only spaces (should not trigger)
    call test_only_spaces()

    ! Test 3: Lines with only tabs (should not trigger)
    call test_only_tabs()

    ! Test 4: Multiple mixed indentations
    call test_multiple_mixed()

    print *, "[OK] All F005 tests passed!"

contains

    subroutine test_mixed_tabs_spaces()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: f005_count
        character(len=*), parameter :: path = "/tmp/fluff_test_f005.f90"

        ! Note: Using char(9) for tab character
        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &        ! 4 spaces
                    char(9)//"integer :: x"//new_line('a')// &  ! Tab + text
                    "  "//char(9)//"x = 42"//new_line('a')// & ! 2 spaces + tab
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file=path, status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file(path, diagnostics, error_msg)

        call assert_error_empty(error_msg)
        call count_code(diagnostics, "F005", f005_count)
        ! Line 3: tab-only when file uses spaces -> 1 violation
        ! Line 4: mixed tabs+spaces in same line -> 1 violation
        call assert_equal_int(f005_count, 2, "Expected 2 F005 violations")
        call assert_f005_location(diagnostics, 3, 1, 1)
        call assert_f005_location(diagnostics, 4, 1, 3)

        ! Clean up
        open (unit=99, file=path, status="old")
        close (99, status="delete")

        print *, "[OK] Mixed tabs and spaces"

    end subroutine test_mixed_tabs_spaces

    subroutine test_only_spaces()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: f005_count
        character(len=*), parameter :: path = "/tmp/fluff_test_f005_spaces.f90"

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 42"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file=path, status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file(path, diagnostics, error_msg)

        call assert_error_empty(error_msg)
        call count_code(diagnostics, "F005", f005_count)

        ! Clean up
        open (unit=99, file=path, status="old")
        close (99, status="delete")

        call assert_equal_int(f005_count, 0, "Expected 0 F005 violations")

        print *, "[OK] Only spaces"

    end subroutine test_only_spaces

    subroutine test_only_tabs()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: f005_count
        character(len=*), parameter :: path = "/tmp/fluff_test_f005_tabs.f90"

        ! Use only tabs for indentation
        test_code = "program test"//new_line('a')// &
                    char(9)//"implicit none"//new_line('a')// &
                    char(9)//"integer :: x"//new_line('a')// &
                    char(9)//"x = 42"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file=path, status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file(path, diagnostics, error_msg)

        call assert_error_empty(error_msg)
        call count_code(diagnostics, "F005", f005_count)

        ! Clean up
        open (unit=99, file=path, status="old")
        close (99, status="delete")

        call assert_equal_int(f005_count, 0, "Expected 0 F005 violations")

        print *, "[OK] Only tabs"

    end subroutine test_only_tabs

    subroutine test_multiple_mixed()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: f005_count
        character(len=*), parameter :: path = "/tmp/fluff_test_f005_multi.f90"

        ! Multiple lines with mixed indentation
        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &            ! 4 spaces
                    char(9)//"integer :: x"//new_line('a')// &     ! 1 tab
                    "  "//char(9)//"real :: y"//new_line('a')// &
                    ! 2 spaces + 1 tab (mixed)
                    char(9)//"  x = 42"//new_line('a')// &
                    ! 1 tab + 2 spaces (mixed)
                    "    y = 3.14"//new_line('a')// &                 ! 4 spaces
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file=path, status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file(path, diagnostics, error_msg)

        call assert_error_empty(error_msg)
        call count_code(diagnostics, "F005", f005_count)

        ! Clean up
        open (unit=99, file=path, status="old")
        close (99, status="delete")

        ! Line 3: tab-only when file uses spaces -> 1 violation
        ! Line 4: mixed tabs+spaces in same line -> 1 violation
        ! Line 5: mixed tabs+spaces in same line -> 1 violation
        call assert_equal_int(f005_count, 3, "Expected 3 F005 violations")
        call assert_f005_location(diagnostics, 3, 1, 1)
        call assert_f005_location(diagnostics, 4, 1, 3)
        call assert_f005_location(diagnostics, 5, 1, 3)

        print *, "[OK] Multiple mixed indentations"

    end subroutine test_multiple_mixed

    subroutine assert_error_empty(error_msg)
        character(len=:), allocatable, intent(in) :: error_msg
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) error stop error_msg
        end if
    end subroutine assert_error_empty

    subroutine count_code(diagnostics, code, count)
        type(diagnostic_t), allocatable, intent(in) :: diagnostics(:)
        character(len=*), intent(in) :: code
        integer, intent(out) :: count
        integer :: i

        count = 0
        if (.not. allocated(diagnostics)) return
        do i = 1, size(diagnostics)
            if (allocated(diagnostics(i)%code)) then
                if (diagnostics(i)%code == code) count = count + 1
            end if
        end do
    end subroutine count_code

    subroutine assert_f005_location(diagnostics, line, start_col, end_col)
        type(diagnostic_t), allocatable, intent(in) :: diagnostics(:)
        integer, intent(in) :: line, start_col, end_col
        integer :: i
        logical :: found

        found = .false.
        if (.not. allocated(diagnostics)) error stop "No diagnostics allocated"

        do i = 1, size(diagnostics)
            if (.not. allocated(diagnostics(i)%code)) cycle
            if (diagnostics(i)%code /= "F005") cycle
            if (diagnostics(i)%location%start%line /= line) cycle
            call assert_equal_int(diagnostics(i)%location%start%column, start_col, &
                                  "Bad F005 start column")
            call assert_equal_int(diagnostics(i)%location%end%column, end_col, &
                                  "Bad F005 end column")
            found = .true.
            exit
        end do

        if (.not. found) then
            print *, "Missing F005 at line", line
            error stop "Missing expected F005 location"
        end if
    end subroutine assert_f005_location

end program test_rule_f005_mixed_tabs_spaces
