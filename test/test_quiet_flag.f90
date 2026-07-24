program test_quiet_flag
    use test_support, only: resolve_fluff_binary
    implicit none

    character(len=:), allocatable :: bin_path, tmpdir, tmpfile, tmpfile_clean, tmpout
    character(len=1024) :: line
    integer :: u, stat, ios, exitstat
    logical :: file_exists

    call resolve_fluff_binary(bin_path)

    tmpdir = "/tmp/fluff242_test_quiet"
    call execute_command_line("rm -rf " // tmpdir // " && mkdir -p " // tmpdir, wait=.true.)

    tmpfile = tmpdir // "/violation.f90"
    call execute_command_line(&
        'printf "program test_prog\ninteger :: x\nend program test_prog\n" > ' // tmpfile, &
        wait=.true.)

    tmpfile_clean = tmpdir // "/clean.f90"
    call execute_command_line(&
        'printf "program test_prog\nimplicit none\ninteger :: x\nx = 0\nend program test_prog\n" > ' // &
        tmpfile_clean, wait=.true.)

    tmpout = "/tmp/fluff242_output.txt"

    call execute_command_line(trim(bin_path) // " check -q " // tmpfile // " > " // tmpout // &
        " 2>/dev/null", wait=.true., exitstat=exitstat)
    inquire(file=tmpout, exist=file_exists)
    if (.not. file_exists) then
        print *, "FAIL: output file not created for -q check with violation"
        stop 1
    end if
    open(newunit=u, file=tmpout, status="old", action="read", iostat=stat)
    if (stat /= 0) then
        print *, "FAIL: cannot read -q output file"
        stop 1
    end if
    read(u, '(A)', iostat=ios) line
    close(u)
    if (ios == 0 .and. len_trim(line) > 0) then
        print *, "FAIL: -q output not empty with violation"
        stop 1
    end if
    if (exitstat == 0) then
        print *, "FAIL: -q exit code not non-zero with violation"
        stop 1
    end if

    call execute_command_line(trim(bin_path) // " check -q " // tmpfile_clean // " > " // &
        tmpout // " 2>/dev/null", wait=.true., exitstat=exitstat)
    open(newunit=u, file=tmpout, status="old", action="read", iostat=stat)
    if (stat /= 0) then
        print *, "FAIL: cannot read -q output for clean file"
        stop 1
    end if
    read(u, '(A)', iostat=ios) line
    close(u)
    if (ios == 0 .and. len_trim(line) > 0) then
        print *, "FAIL: -q output not empty with clean file"
        stop 1
    end if
    if (exitstat /= 0) then
        print *, "FAIL: -q exit code not zero with clean file"
        stop 1
    end if

    call execute_command_line(trim(bin_path) // " check " // tmpfile // " > " // tmpout // &
        " 2>/dev/null", wait=.true.)
    open(newunit=u, file=tmpout, status="old", action="read", iostat=stat)
    if (stat /= 0) then
        print *, "FAIL: cannot read output without -q"
        stop 1
    end if
    read(u, '(A)', iostat=ios) line
    close(u)
    if (ios /= 0 .or. len_trim(line) == 0) then
        print *, "FAIL: output empty without -q flag"
        stop 1
    end if

    print *, "PASS"
    stop 0

end program test_quiet_flag
