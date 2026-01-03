program test_rule_f013_multiple_statements
    ! Test F013: Multiple statements per line rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked
    implicit none

    print *, "Testing F013: Multiple statements per line rule..."

    ! Test 1: Multiple statements per line (should trigger)
    call test_multiple_statements()

    ! Test 2: Single statements per line (should not trigger)
    call test_single_statements()

    ! Test 3: Semicolon separated statements
    call test_semicolon_statements()

    ! Test 4: Complex multi-statement lines
    call test_complex_multi_statements()

    print *, "All F013 tests passed!"

contains

    subroutine test_multiple_statements()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f013

        ! Test multiple statements per line

        ! Multiple statements:
        ! - assignment chain with semicolons
        ! - single-line IF with semicolons
        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y, z"//new_line('a')// &
                    "    x = 10; y = 20; z = 30"//new_line('a')// &
                    "    if (x > 5) print *, x; "// &
                    "print *, y"//new_line('a')// &
                    "    print *, z"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f013", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F013 violation
        found_f013 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F013") then
                    found_f013 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f013) then
            error stop "Failed: F013 should be triggered for multiple statements "// &
                "per line"
        end if

        print *, "  Multiple statements per line"

    end subroutine test_multiple_statements

    subroutine test_single_statements()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f013

        ! Test single statements per line

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y, z"//new_line('a')// &
                    "    x = 10"//new_line('a')// &
                    "    y = 20"//new_line('a')// &
                    "    z = 30"//new_line('a')// &
                    "    if (x > 5) then"//new_line('a')// &
                    "        print *, x"//new_line('a')// &
                    "    end if"//new_line('a')// &
                    "    print *, y"//new_line('a')// &
                    "    print *, z"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f013_ok", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F013 violation
        found_f013 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F013") then
                    found_f013 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f013) then
            error stop "Failed: F013 should not be triggered for "// &
                "single statements per line"
        end if

        print *, "  Single statements per line"

    end subroutine test_single_statements

    subroutine test_semicolon_statements()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f013

        ! Test semicolon separated statements
        ! Multiple statements:
        ! - assignment chain with semicolons
        ! - repeated print with semicolons
        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: a, b, c"//new_line('a')// &
                    "    a = 1; b = 2; c = 3"//new_line('a')// &
                    "    print *, a; "// &
                    "print *, b"//new_line('a')// &
                    "    print *, c"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f013_semi", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F013 violation
        found_f013 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F013") then
                    found_f013 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f013) then
            error stop "Failed: F013 should be triggered for semicolon separated "// &
                "statements"
        end if

        print *, "  Semicolon separated statements"

    end subroutine test_semicolon_statements

    subroutine test_complex_multi_statements()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f013

        ! Test complex multi-statement lines
        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i, j, sum"//new_line('a')// &
                    "    i = 0; j = 0; sum = 0"//new_line('a')// &
                    "    do i = 1, 10"//new_line('a')// &
                    "        j = i * 2; sum = sum + j"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "    print *, sum"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f013_complex", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F013 violation
        found_f013 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F013") then
                    found_f013 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f013) then
            error stop "Failed: F013 should be triggered for "// &
                "complex multi-statement lines"
        end if

        print *, "  Complex multi-statement lines"

    end subroutine test_complex_multi_statements

end program test_rule_f013_multiple_statements
