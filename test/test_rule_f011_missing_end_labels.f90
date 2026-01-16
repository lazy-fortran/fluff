program test_rule_f011_missing_end_labels
    ! Test F011: Missing end statement labels rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked
    implicit none

    print *, "Testing F011: Missing end statement labels rule..."

    ! Test 1: Missing end labels (should trigger)
    call test_missing_end_labels()

    ! Test 2: Proper end labels (should not trigger)
    call test_proper_end_labels()

    ! Test 3: Mixed end labels
    call test_mixed_end_labels()

    ! Test 4: Function end labels
    call test_function_end_labels()

    print *, "[OK] All F011 tests passed!"

contains

    subroutine test_missing_end_labels()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f011

        test_code = "program test_prog"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    ""//new_line('a')// &
                    "contains"//new_line('a')// &
                    ""//new_line('a')// &
                    "    subroutine calc_values(x, y)"//new_line('a')// &
                    "        real, intent(in) :: x, y"//new_line('a')// &
                    "        print *, x + y"//new_line('a')// &
                    "    end subroutine"//new_line('a')// &  ! Missing label
                    ""//new_line('a')// &
                    "    function compute(a) result(b)"//new_line('a')// &
                    "        real, intent(in) :: a"//new_line('a')// &
                    "        real :: b"//new_line('a')// &
                    "        b = a * 2.0"//new_line('a')// &
                    "    end function"//new_line('a')// &  ! Missing label
                    ""//new_line('a')// &
                    "end program"  ! Missing label

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f011", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F011 violation
        found_f011 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F011") then
                    found_f011 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f011) then
            error stop "Failed: F011 should be triggered for missing end labels"
        end if

        print *, "  Missing end labels"

    end subroutine test_missing_end_labels

    subroutine test_proper_end_labels()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f011

        test_code = "program test_prog"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    ""//new_line('a')// &
                    "contains"//new_line('a')// &
                    ""//new_line('a')// &
                    "    subroutine calc_values(x, y)"//new_line('a')// &
                    "        real, intent(in) :: x, y"//new_line('a')// &
                    "        print *, x + y"//new_line('a')// &
                    "    end subroutine calc_values"//new_line('a')// &
                    ""//new_line('a')// &
                    "    function compute(a) result(b)"//new_line('a')// &
                    "        real, intent(in) :: a"//new_line('a')// &
                    "        real :: b"//new_line('a')// &
                    "        b = a * 2.0"//new_line('a')// &
                    "    end function compute"//new_line('a')// &
                    ""//new_line('a')// &
                    "end program test_prog"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f011_ok", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F011 violation
        found_f011 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F011") then
                    found_f011 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f011) then
            error stop "Failed: F011 should not be triggered for proper end labels"
        end if

        print *, "  Proper end labels"

    end subroutine test_proper_end_labels

    subroutine test_mixed_end_labels()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f011

        test_code = "module m"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "    subroutine ok()"//new_line('a')// &
                    "        implicit none"//new_line('a')// &
                    "    end subroutine ok"//new_line('a')// &
                    "    subroutine bad()"//new_line('a')// &
                    "        implicit none"//new_line('a')// &
                    "    end subroutine"//new_line('a')// &
                    "end module m"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f011_mixed", path)
        call write_text_file(path, test_code)

        call lint_file_checked(linter, path, diagnostics)

        found_f011 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F011") then
                    found_f011 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f011) then
            error stop "Failed: F011 should be triggered for mixed end labels"
        end if

        print *, "  Mixed end labels"
    end subroutine test_mixed_end_labels

    subroutine test_function_end_labels()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f011

        test_code = "module m"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "    function f(x) result(y)"//new_line('a')// &
                    "        integer, intent(in) :: x"//new_line('a')// &
                    "        integer :: y"//new_line('a')// &
                    "        y = x + 1"//new_line('a')// &
                    "    end function"//new_line('a')// &
                    "end module m"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f011_func", path)
        call write_text_file(path, test_code)

        call lint_file_checked(linter, path, diagnostics)

        found_f011 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F011") then
                    found_f011 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f011) then
            error stop "Failed: F011 should be triggered for missing function end label"
        end if

        print *, "  Function end labels"
    end subroutine test_function_end_labels

end program test_rule_f011_missing_end_labels
