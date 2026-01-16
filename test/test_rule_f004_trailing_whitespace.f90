program test_rule_f004_trailing_whitespace
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: linter_engine_t, create_linter_engine
    use fortfront, only: token_t, trivia_token_t, tokenize_core_with_trivia
    use lexer_token_types, only: TK_NEWLINE, TK_WHITESPACE
    use test_support, only: assert_equal_int
    implicit none

    print *, "Testing F004: Trailing whitespace rule..."

    ! Test 1: Lines with trailing whitespace (should trigger)
    call test_trailing_whitespace()

    ! Test 2: Clean lines without trailing whitespace (should not trigger)
    call test_no_trailing_whitespace()

    ! Test 3: Multiple lines with mixed trailing whitespace
    call test_multiple_trailing_spaces()

    ! Test 4: Trailing tabs
    call test_trailing_tabs()

    print *, "[OK] All F004 tests passed!"

contains

    subroutine test_trailing_whitespace()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        integer :: f004_count
        character(len=*), parameter :: path = "/tmp/fluff_test_f004.f90"
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: file_content

        linter = create_linter_engine()

        test_code = "program test"//new_line('a')// &
                    "    implicit none   "//new_line('a')// &
                    "    integer :: x  "//new_line('a')// &
                    "    x = 42"//new_line('a')// &
                    "end program test"//new_line('a')

        call write_temp_file(path, test_code)
        call read_temp_file(path, file_content)
        call assert_equal_int(count_trailing_whitespace(file_content), 2, &
                              "Unexpected trivia trailing whitespace count")

        ! Lint the file
        call linter%lint_file(path, diagnostics, error_msg)

        call assert_error_empty(error_msg)
        call count_code(diagnostics, "F004", f004_count)
        call assert_equal_int(f004_count, 2, "Expected 2 F004 violations")

        call assert_f004_location(diagnostics, 2, 18, 20)
        call assert_f004_location(diagnostics, 3, 17, 18)
        call assert_f004_fix_present(diagnostics)

        ! Clean up
        open (unit=99, file=path, status="old")
        close (99, status="delete")

        print *, "[OK] Trailing whitespace"

    end subroutine test_trailing_whitespace

    subroutine test_no_trailing_whitespace()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: f004_count
        character(len=*), parameter :: path = "/tmp/fluff_test_f004_ok.f90"

        linter = create_linter_engine()

        ! Create temporary file
        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 42"//new_line('a')// &
                    "end program test"

        call write_temp_file(path, test_code//new_line('a'))

        ! Lint the file
        call linter%lint_file(path, diagnostics, error_msg)

        call assert_error_empty(error_msg)
        call count_code(diagnostics, "F004", f004_count)

        ! Clean up
        open (unit=99, file=path, status="old")
        close (99, status="delete")

        call assert_equal_int(f004_count, 0, "Expected 0 F004 violations")

        print *, "[OK] No trailing whitespace"

    end subroutine test_no_trailing_whitespace

    subroutine test_multiple_trailing_spaces()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: f004_count
        character(len=*), parameter :: path = "/tmp/fluff_test_f004_multi.f90"

        ! Multiple lines with trailing spaces of different lengths
        test_code = "program test"//new_line('a')// &
                    "    implicit none     "//new_line('a')// &  ! 5 trailing spaces
                    "    integer :: x  "//new_line('a')// &      ! 2 trailing spaces
                    "    real :: y   "//new_line('a')// &        ! 3 trailing spaces
                    "    x = 42"//new_line('a')// &              ! No trailing space
                    "    y = 3.14"//new_line('a')// &            ! No trailing space
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call write_temp_file(path, test_code//new_line('a'))

        ! Lint the file
        call linter%lint_file(path, diagnostics, error_msg)

        call assert_error_empty(error_msg)
        call count_code(diagnostics, "F004", f004_count)

        ! Clean up
        open (unit=99, file=path, status="old")
        close (99, status="delete")

        call assert_equal_int(f004_count, 3, "Expected 3 F004 violations")
        call assert_f004_location(diagnostics, 2, 18, 22)
        call assert_f004_location(diagnostics, 3, 17, 18)
        call assert_f004_location(diagnostics, 4, 14, 16)

        print *, "[OK] Multiple trailing spaces"

    end subroutine test_multiple_trailing_spaces

    subroutine test_trailing_tabs()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        integer :: f004_count
        character(len=*), parameter :: path = "/tmp/fluff_test_f004_tabs.f90"
        character(len=:), allocatable :: test_code

        linter = create_linter_engine()

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//achar(9)//new_line('a')// &
                    "    integer :: x"//achar(9)//achar(9)//new_line('a')// &
                    "    x = 42"//new_line('a')// &
                    "end program test"//new_line('a')

        call write_temp_file(path, test_code)

        ! Lint the file
        call linter%lint_file(path, diagnostics, error_msg)

        call assert_error_empty(error_msg)
        call count_code(diagnostics, "F004", f004_count)
        call assert_equal_int(f004_count, 2, "Expected 2 F004 violations")

        call assert_f004_location(diagnostics, 2, 18, 18)
        call assert_f004_location(diagnostics, 3, 17, 18)
        call assert_f004_fix_present(diagnostics)

        ! Clean up
        open (unit=99, file=path, status="old")
        close (99, status="delete")

        print *, "[OK] Trailing tabs"

    end subroutine test_trailing_tabs

    subroutine assert_error_empty(error_msg)
        character(len=:), allocatable, intent(in) :: error_msg
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) error stop error_msg
        end if
    end subroutine assert_error_empty

    subroutine write_temp_file(path, content)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: content
        integer :: unit, iostat

        open (newunit=unit, file=path, status="replace", action="write", &
              access="stream", form="unformatted", iostat=iostat)
        if (iostat /= 0) error stop "Failed to create temporary file"

        write (unit, iostat=iostat) content
        if (iostat /= 0) error stop "Failed to write temporary file"
        close (unit)
    end subroutine write_temp_file

    subroutine read_temp_file(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, iostat, size_bytes

        open (newunit=unit, file=path, status="old", action="read", &
              access="stream", form="unformatted", iostat=iostat)
        if (iostat /= 0) error stop "Failed to open temporary file for readback"

        inquire (unit=unit, size=size_bytes)
        if (size_bytes <= 0) then
            content = ""
            close (unit)
            return
        end if

        allocate (character(len=size_bytes) :: content)
        read (unit, iostat=iostat) content
        if (iostat /= 0) error stop "Failed to read back temporary file"
        close (unit)
    end subroutine read_temp_file

    integer function count_trailing_whitespace(source) result(count)
        character(len=*), intent(in) :: source
        type(token_t), allocatable :: tokens(:)
        integer :: i

        count = 0
        call tokenize_core_with_trivia(source, tokens)
        if (.not. allocated(tokens)) return

        do i = 1, size(tokens)
            call count_in_trivia(tokens(i)%leading_trivia, count)
        end do
    end function count_trailing_whitespace

    subroutine count_in_trivia(trivia, count)
        type(trivia_token_t), allocatable, intent(in) :: trivia(:)
        integer, intent(inout) :: count
        integer :: i

        if (.not. allocated(trivia)) return
        if (size(trivia) <= 0) return

        do i = 2, size(trivia)
            if (trivia(i)%kind /= TK_NEWLINE) cycle
            if (trivia(i - 1)%kind /= TK_WHITESPACE) cycle
            if (.not. allocated(trivia(i - 1)%text)) cycle
            if (len(trivia(i - 1)%text) <= 0) cycle
            count = count + 1
        end do
    end subroutine count_in_trivia

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

    subroutine assert_f004_location(diagnostics, line, start_col, end_col)
        type(diagnostic_t), allocatable, intent(in) :: diagnostics(:)
        integer, intent(in) :: line, start_col, end_col
        integer :: i
        logical :: found

        found = .false.
        if (.not. allocated(diagnostics)) error stop "No diagnostics allocated"

        do i = 1, size(diagnostics)
            if (.not. allocated(diagnostics(i)%code)) cycle
            if (diagnostics(i)%code /= "F004") cycle
            if (diagnostics(i)%location%start%line /= line) cycle
            call assert_equal_int(diagnostics(i)%location%start%column, start_col, &
                                  "Bad F004 start column")
            call assert_equal_int(diagnostics(i)%location%end%column, end_col, &
                                  "Bad F004 end column")
            found = .true.
            exit
        end do

        if (.not. found) then
            print *, "Missing F004 at line", line
            error stop "Missing expected F004 location"
        end if
    end subroutine assert_f004_location

    subroutine assert_f004_fix_present(diagnostics)
        type(diagnostic_t), allocatable, intent(in) :: diagnostics(:)
        integer :: i
        integer :: f004_seen

        f004_seen = 0
        if (.not. allocated(diagnostics)) error stop "No diagnostics allocated"

        do i = 1, size(diagnostics)
            if (.not. allocated(diagnostics(i)%code)) cycle
            if (diagnostics(i)%code /= "F004") cycle
            f004_seen = f004_seen + 1
            if (.not. allocated(diagnostics(i)%fixes)) error stop "F004 missing fixes"
            call assert_equal_int(size(diagnostics(i)%fixes), 1, "F004 fixes count")
            if (.not. allocated(diagnostics(i)%fixes(1)%edits)) then
                error stop "F004 fix missing edits"
            end if
            call assert_equal_int(size(diagnostics(i)%fixes(1)%edits), 1, &
                                  "F004 edits count")
            if (allocated(diagnostics(i)%fixes(1)%edits(1)%new_text)) then
                if (len(diagnostics(i)%fixes(1)%edits(1)%new_text) /= 0) then
                    error stop "F004 edit new_text not empty"
                end if
            end if
        end do

        if (f004_seen <= 0) error stop "Expected at least one F004 diagnostic"
    end subroutine assert_f004_fix_present

end program test_rule_f004_trailing_whitespace
