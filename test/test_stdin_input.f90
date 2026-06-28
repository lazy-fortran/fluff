program test_stdin_input
    ! Test stdin support: lint_source entry point and "-" / --stdin-filename parsing
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use fluff_cli, only: cli_args_t
    implicit none

    print *, "Testing stdin input support..."

    call test_lint_source_reports_violation()
    call test_lint_source_clean()
    call test_dash_parses_as_stdin()
    call test_stdin_filename_flag()

    print *, "[OK] All stdin input tests passed!"

contains

    subroutine test_lint_source_reports_violation()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: source, error_msg
        integer :: i
        logical :: found_f001

        source = "program test"//new_line('a')// &
            "    integer :: x"//new_line('a')// &
            "    x = 42"//new_line('a')// &
            "end program test"

        linter = create_linter_engine()
        call linter%lint_source(source, "stdin", diagnostics, error_msg)

        if (error_msg /= "") then
            print *, "FAIL: lint_source error: ", error_msg
            stop 1
        end if

        found_f001 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F001") found_f001 = .true.
            end do
        end if
        if (.not. found_f001) then
            print *, "FAIL: expected F001 from stdin source"
            stop 1
        end if
    end subroutine test_lint_source_reports_violation

    subroutine test_lint_source_clean()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: source, error_msg
        integer :: i
        logical :: found_f001

        source = "program test"//new_line('a')// &
            "    implicit none"//new_line('a')// &
            "    integer :: x"//new_line('a')// &
            "    x = 42"//new_line('a')// &
            "end program test"

        linter = create_linter_engine()
        call linter%lint_source(source, "stdin", diagnostics, error_msg)

        if (error_msg /= "") then
            print *, "FAIL: lint_source error: ", error_msg
            stop 1
        end if

        found_f001 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F001") found_f001 = .true.
            end do
        end if
        if (found_f001) then
            print *, "FAIL: unexpected F001 on clean source"
            stop 1
        end if
    end subroutine test_lint_source_clean

    subroutine test_dash_parses_as_stdin()
        type(cli_args_t) :: args
        character(len=32) :: argv(2)

        argv(1) = "check"
        argv(2) = "-"
        call args%parse(2, argv)

        if (.not. allocated(args%files)) then
            print *, "FAIL: '-' did not produce a file entry"
            stop 1
        end if
        if (size(args%files) /= 1 .or. trim(args%files(1)) /= "-") then
            print *, "FAIL: '-' not recorded as stdin sentinel"
            stop 1
        end if
        if (args%stdin_filename /= "stdin") then
            print *, "FAIL: default stdin filename not 'stdin'"
            stop 1
        end if
    end subroutine test_dash_parses_as_stdin

    subroutine test_stdin_filename_flag()
        type(cli_args_t) :: args
        character(len=32) :: argv(4)

        argv(1) = "check"
        argv(2) = "--stdin-filename"
        argv(3) = "buffer.f90"
        argv(4) = "-"
        call args%parse(4, argv)

        if (args%stdin_filename /= "buffer.f90") then
            print *, "FAIL: --stdin-filename not honored"
            stop 1
        end if
    end subroutine test_stdin_filename_flag

end program test_stdin_input
