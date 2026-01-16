program test_rule_f014_unnecessary_parentheses
    ! Test F014: Unnecessary parentheses rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked
    implicit none

    print *, "Testing F014: Unnecessary parentheses rule..."

    ! Test 1: Unnecessary parentheses (should trigger)
    call test_unnecessary_parentheses()

    ! Test 2: Necessary parentheses (should not trigger)
    call test_necessary_parentheses()

    ! Test 3: Expression clarity parentheses
    call test_expression_clarity()

    ! Test 4: Function call parentheses
    call test_function_call_parentheses()

    print *, "[OK] All F014 tests passed!"

contains

    subroutine test_unnecessary_parentheses()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f014

        ! Enable test - fortfront is now available

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y, z"//new_line('a')// &
                    "    x = (10)"//new_line('a')// &
                    ! Unnecessary parentheses
                    "    y = ((20))"//new_line('a')// &
                    ! Double unnecessary parentheses
                    "    z = (x + y) * 2"//new_line('a')// &   ! This one is needed
                    "    if ((x > 5)) then"//new_line('a')// &
                    ! Unnecessary in condition
                    "        print *, (x)"//new_line('a')// &  ! Unnecessary in print
                    "    end if"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f014", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F014 violation
        found_f014 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F014") then
                    found_f014 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f014) then
            error stop "Failed: F014 should be triggered for unnecessary parentheses"
        end if

        print *, "[OK] Unnecessary parentheses"

    end subroutine test_unnecessary_parentheses

    subroutine test_necessary_parentheses()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f014

        ! F014 rule now has proper precedence detection

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y, z"//new_line('a')// &
                    "    x = 10"//new_line('a')// &
                    "    y = 20"//new_line('a')// &
                    "    z = (x + y) * 2"//new_line('a')// &
                    ! Necessary for precedence
                    "    if (x > 5 .and. y < 30) then"//new_line('a')// &
                    ! Normal condition
                    "        print *, x, y"//new_line('a')// &
                    "    end if"//new_line('a')// &
                    "    z = x * (y + 10)"//new_line('a')// &
                    ! Necessary for precedence
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f014_ok", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F014 violation
        found_f014 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F014") then
                    found_f014 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f014) then
            error stop "Failed: F014 should not be triggered for necessary parentheses"
        end if

        print *, "[OK] Necessary parentheses"

    end subroutine test_necessary_parentheses

    subroutine test_expression_clarity()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f014

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y, z"//new_line('a')// &
                    "    x = 1"//new_line('a')// &
                    "    y = 2"//new_line('a')// &
                    "    z = (x + y)"//new_line('a')// &  ! Not flagged (has operator)
                    "    print *, z"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f014_clarity", path)
        call write_text_file(path, test_code)

        call lint_file_checked(linter, path, diagnostics)

        found_f014 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F014") then
                    found_f014 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f014) then
            error stop "Failed: F014 should not be triggered for expression "// &
                "clarity parentheses"
        end if

        print *, "[OK] Expression clarity parentheses"
    end subroutine test_expression_clarity

    subroutine test_function_call_parentheses()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f014

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y"//new_line('a')// &
                    "    x = 1"//new_line('a')// &
                    "    y = abs(x)"//new_line('a')// &
                    "    print *, y"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f014_call", path)
        call write_text_file(path, test_code)

        call lint_file_checked(linter, path, diagnostics)

        found_f014 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F014") then
                    found_f014 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f014) then
            error stop &
                "Failed: F014 should not be triggered for function call parentheses"
        end if

        print *, "[OK] Function call parentheses"
    end subroutine test_function_call_parentheses

end program test_rule_f014_unnecessary_parentheses
