program test_f010_f015_location_strict
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked
    implicit none

    print *, "Testing F010-F015 with strict location verification..."

    call test_f010_goto_location()
    call test_f010_arithmetic_if_location()
    call test_f011_missing_end_label_location()
    call test_f012_naming_violation_location()
    call test_f013_semicolon_location()
    call test_f014_parens_location()
    call test_f015_continue_location()

    print *, "[OK] All F010-F015 location tests passed!"

contains

    subroutine assert_diagnostic_at(diagnostics, code, expected_line, test_name)
        type(diagnostic_t), allocatable, intent(in) :: diagnostics(:)
        character(len=*), intent(in) :: code
        integer, intent(in) :: expected_line
        character(len=*), intent(in) :: test_name

        integer :: i
        logical :: found

        found = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == code) then
                    if (diagnostics(i)%location%start%line == expected_line) then
                        found = .true.
                        exit
                    end if
                end if
            end do
        end if

        if (.not. found) then
            print *, "  FAIL: ", test_name
            print *, "    Expected: ", code, " at line ", expected_line
            if (allocated(diagnostics)) then
                do i = 1, size(diagnostics)
                    if (diagnostics(i)%code == code) then
                        print *, "    Found: ", diagnostics(i)%code, " at line ", &
                            diagnostics(i)%location%start%line
                    end if
                end do
            end if
            error stop "Location assertion failed"
        end if

        print *, "  [OK] ", test_name
    end subroutine assert_diagnostic_at

    subroutine assert_no_diagnostic(diagnostics, code, test_name)
        type(diagnostic_t), allocatable, intent(in) :: diagnostics(:)
        character(len=*), intent(in) :: code
        character(len=*), intent(in) :: test_name

        integer :: i
        logical :: found

        found = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == code) then
                    found = .true.
                    exit
                end if
            end do
        end if

        if (found) then
            print *, "  FAIL: ", test_name
            print *, "    Unexpected: ", code, " found"
            error stop "No-diagnostic assertion failed"
        end if

        print *, "  [OK] ", test_name
    end subroutine assert_no_diagnostic

    subroutine test_f010_goto_location()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        print *, ""
        print *, "F010: GOTO location verification"

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 1"//new_line('a')// &
                    "    goto 100"//new_line('a')// &
                    "100 print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_f010_loc", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_diagnostic_at(diagnostics, "F010", 5, "GOTO on line 5")
    end subroutine test_f010_goto_location

    subroutine test_f010_arithmetic_if_location()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        print *, ""
        print *, "F010: Arithmetic IF location verification"

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 0"//new_line('a')// &
                    "    if (x) 10, 20, 30"//new_line('a')// &
                    "10  print *, x"//new_line('a')// &
                    "20  print *, x"//new_line('a')// &
                    "30  print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_f010_arith", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_diagnostic_at(diagnostics, "F010", 5, "Arithmetic IF on line 5")
    end subroutine test_f010_arithmetic_if_location

    subroutine test_f011_missing_end_label_location()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        print *, ""
        print *, "F011: Missing end label location verification"

        test_code = "program test_prog"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "    subroutine helper()"//new_line('a')// &
                    "    end subroutine"//new_line('a')// &
                    "end program test_prog"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_f011_loc", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_diagnostic_at(diagnostics, "F011", 5, "Missing subroutine label on line 5")
    end subroutine test_f011_missing_end_label_location

    subroutine test_f012_naming_violation_location()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found

        print *, ""
        print *, "F012: Naming convention detection verification"

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: first_value"//new_line('a')// &
                    "    integer :: secondValue"//new_line('a')// &
                    "    first_value = 1"//new_line('a')// &
                    "    secondValue = 2"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_f012_loc", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        found = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F012") then
                    found = .true.
                    exit
                end if
            end do
        end if

        if (.not. found) then
            error stop "F012: Expected naming inconsistency diagnostic"
        end if

        print *, "  [OK] Naming inconsistency detected"
    end subroutine test_f012_naming_violation_location

    subroutine test_f013_semicolon_location()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        print *, ""
        print *, "F013: Multiple statements location verification"

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y"//new_line('a')// &
                    "    x = 1; y = 2"//new_line('a')// &
                    "    print *, x, y"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_f013_loc", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_diagnostic_at(diagnostics, "F013", 4, "Semicolon on line 4")
    end subroutine test_f013_semicolon_location

    subroutine test_f014_parens_location()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        print *, ""
        print *, "F014: Unnecessary parentheses location verification"

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = (10)"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_f014_loc", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_diagnostic_at(diagnostics, "F014", 4, "Unnecessary parens on line 4")
    end subroutine test_f014_parens_location

    subroutine test_f015_continue_location()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        print *, ""
        print *, "F015: Redundant CONTINUE location verification"

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    do i = 1, 3"//new_line('a')// &
                    "        continue"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_f015_loc", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_diagnostic_at(diagnostics, "F015", 5, "Redundant CONTINUE on line 5")
    end subroutine test_f015_continue_location

end program test_f010_f015_location_strict
