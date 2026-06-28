program test_format_check
    use fluff_cli
    use test_support, only: make_temp_fortran_path, read_text_file, &
        delete_file_if_exists
    implicit none

    print *, "Testing format --check mode..."

    call test_check_misformatted_exit_nonzero()
    call test_check_formatted_exit_zero()

    print *, "[OK] All format --check tests passed!"

contains

    subroutine write_test_file(path, content)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: content
        integer :: unit, ios

        open (newunit=unit, file=path, status="replace", action="write", &
            access="stream", form="unformatted", iostat=ios)
        if (ios /= 0) then
            error stop "Failed to open file for writing: "//trim(path)
        end if
        write (unit) content
        close (unit)
    end subroutine write_test_file

    subroutine test_check_misformatted_exit_nonzero()
        type(cli_app_t) :: app
        character(len=:), allocatable :: tmp, before, after, error_msg
        character(len=256) :: argv(3)
        integer :: exit_code

        app = create_cli_app()
        call make_temp_fortran_path("check_misfmt", tmp)

        call write_test_file(tmp, &
            "program t"//new_line('a')//&
            "  integer :: x,y"//new_line('a')//&
            "  x=1"//new_line('a')//&
            "end program t"//new_line('a'))
        call read_text_file(tmp, before, error_msg)
        if (error_msg /= "") error stop "Failed to read pre-check file"

        argv(1) = "format"
        argv(2) = "--check"
        argv(3) = trim(tmp)
        call app%args%parse(3, argv)
        call app%run(exit_code)

        if (exit_code /= 1) then
            write (*, '(A,I0)') "Failed: misformatted file should exit 1, got ", exit_code
            error stop 1
        end if

        call read_text_file(tmp, after, error_msg)
        if (error_msg /= "") error stop "Failed to read post-check file"
        if (after /= before) error stop "Failed: --check modified the file"

        call delete_file_if_exists(tmp)
        print *, "[OK] Misformatted file exits nonzero"

    end subroutine test_check_misformatted_exit_nonzero

    subroutine test_check_formatted_exit_zero()
        type(cli_app_t) :: app_fix, app_check
        character(len=:), allocatable :: tmp
        character(len=256) :: argv(3)
        integer :: exit_code

        call make_temp_fortran_path("check_fmt", tmp)

        call write_test_file(tmp, &
            "program t"//new_line('a')//&
            "  integer :: x,y"//new_line('a')//&
            "  x=1"//new_line('a')//&
            "end program t"//new_line('a'))

        app_fix = create_cli_app()
        argv(1) = "format"
        argv(2) = "--fix"
        argv(3) = trim(tmp)
        call app_fix%args%parse(3, argv)
        call app_fix%run(exit_code)
        if (exit_code /= 0) error stop "Failed: --fix should succeed"

        app_check = create_cli_app()
        argv(1) = "format"
        argv(2) = "--check"
        argv(3) = trim(tmp)
        call app_check%args%parse(3, argv)
        call app_check%run(exit_code)

        if (exit_code /= 0) then
            write (*, '(A,I0)') "Failed: formatted file should exit 0, got ", exit_code
            error stop 1
        end if

        call delete_file_if_exists(tmp)
        print *, "[OK] Formatted file exits zero"

    end subroutine test_check_formatted_exit_zero

end program test_format_check
