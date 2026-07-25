program test_cli_format_stdout
    !! End-to-end coverage of the `fluff format` stdout path.
    !!
    !! Review of #260 found that the fix had no oracle where the defect actually
    !! lived: reverting only the call site in run_format_command back to
    !! `print *, formatted_code` left the whole suite green while the binary
    !! reproduced the bug. The existing test calls write_formatted_output
    !! directly, which verifies the helper rather than the command that uses it.
    !!
    !! These tests drive the built binary, so they fail if the command stops
    !! calling the helper, whatever the helper itself does.
    !!
    !! The binary comes from resolve_fluff_binary in test_support, which derives
    !! it from the running test's own executable. Hardcoding a build layout is
    !! what #265 was filed for, and the first version of this file did exactly
    !! that in the other direction: it looked in fo's build tree and found
    !! nothing under fpm, which is what CI runs.
    use test_support, only: resolve_fluff_binary
    implicit none

    integer :: n_pass, n_fail
    character(len=:), allocatable :: bin

    n_pass = 0
    n_fail = 0

    call resolve_fluff_binary(bin)
    if (.not. allocated(bin)) then
        write (*, '(a)') 'FAIL: could not locate the fluff binary this build produced'
        error stop 1
    end if
    if (len_trim(bin) == 0) then
        write (*, '(a)') 'FAIL: could not locate the fluff binary this build produced'
        error stop 1
    end if

    call require_first_line_has_no_leading_space(bin)
    call require_output_is_idempotent(bin)

    write (*, '(a,i0,a,i0,a)') 'cli_format_stdout: ', n_pass, ' pass, ', &
        n_fail, ' fail'
    if (n_fail > 0) error stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (*, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert


    subroutine write_fixture(path)
        character(len=*), intent(in) :: path
        integer :: u

        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') 'program p'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') '    integer :: i'
        write (u, '(a)') '    i = 1'
        write (u, '(a)') '    print *, i'
        write (u, '(a)') 'end program p'
        close (u)
    end subroutine write_fixture

    subroutine run_format(bin, src, out)
        character(len=*), intent(in) :: bin, src, out

        call execute_command_line('"'//trim(bin)//'" format "'//trim(src)// &
            '" > "'//trim(out)//'" 2>/dev/null', wait=.true.)
    end subroutine run_format

    subroutine first_line(path, line)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: line
        integer :: u, ios

        line = ''
        open (newunit=u, file=trim(path), status='old', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) line
        close (u)
    end subroutine first_line

    subroutine require_first_line_has_no_leading_space(bin)
        !! The defect: list-directed output prepends a blank to the record, so
        !! `program p` came back as ` program p`.
        character(len=*), intent(in) :: bin
        character(len=512) :: src, out, line

        src = '/tmp/fluff_cli_fmt_src.f90'
        out = '/tmp/fluff_cli_fmt_out.txt'
        call write_fixture(src)
        call run_format(bin, src, out)
        call first_line(out, line)

        call assert(len_trim(line) > 0, &
            'the format command emitted something on stdout')
        if (len_trim(line) == 0) return
        call assert(line(1:1) /= ' ', &
            'the first emitted line starts in column 1, got "'//trim(line)//'"')

        call execute_command_line('rm -f "'//trim(src)//'" "'//trim(out)//'"')
    end subroutine require_first_line_has_no_leading_space

    subroutine require_output_is_idempotent(bin)
        !! Formatting the formatter's own output must be a no-op. This is the
        !! property #260 is about; the leading space broke it because the second
        !! pass saw a differently-indented first line.
        character(len=*), intent(in) :: bin
        character(len=512) :: src, out1, out2
        integer :: rc

        src = '/tmp/fluff_cli_idem_src.f90'
        out1 = '/tmp/fluff_cli_idem_1.f90'
        out2 = '/tmp/fluff_cli_idem_2.txt'
        call write_fixture(src)
        call run_format(bin, src, out1)
        call run_format(bin, out1, out2)

        call execute_command_line('diff -q "'//trim(out1)//'" "'//trim(out2)// &
            '" > /dev/null 2>&1', wait=.true., exitstat=rc)
        call assert(rc == 0, &
            'formatting the formatted output again changes nothing')

        call execute_command_line('rm -f "'//trim(src)//'" "'//trim(out1)// &
            '" "'//trim(out2)//'"')
    end subroutine require_output_is_idempotent

end program test_cli_format_stdout
