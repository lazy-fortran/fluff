program test_combined_json
    implicit none

    character(len=:), allocatable :: bin_path, tmpdir, tmpout, output
    character(len=1024) :: bin_buf, line
    integer :: u, stat, ios, i, j
    integer :: extra_arrays
    logical :: has_file1, has_file2, in_string

    call execute_command_line("ls build/gfortran_*/app/fluff > /tmp/fluff245_bin.txt 2>/dev/null", wait=.true.)
    open(newunit=u, file="/tmp/fluff245_bin.txt", status="old", action="read", iostat=stat)
    if (stat /= 0) then
        print *, "FAIL: binary not found"
        stop 1
    end if
    read(u, '(A)', iostat=stat) bin_buf
    close(u)
    if (stat /= 0 .or. len_trim(bin_buf) == 0) then
        print *, "FAIL: binary not found"
        stop 1
    end if
    bin_path = trim(bin_buf)

    tmpdir = "/tmp/fluff245_test_combined"
    call execute_command_line("rm -rf " // tmpdir // " && mkdir -p " // tmpdir, wait=.true.)
    call execute_command_line(&
        'printf "program f1\ninteger :: x\nend program f1\n" > ' // tmpdir // "/file1.f90", wait=.true.)
    call execute_command_line(&
        'printf "program f2\ninteger :: y\nend program f2\n" > ' // tmpdir // "/file2.f90", wait=.true.)

    tmpout = "/tmp/fluff245_output.txt"
    call execute_command_line(trim(bin_path) // " check --output-format json " // &
        tmpdir // " > " // tmpout // " 2>/dev/null", wait=.true.)

    open(newunit=u, file=tmpout, status="old", action="read", iostat=stat)
    if (stat /= 0) then
        print *, "FAIL: cannot read output"
        stop 1
    end if
    allocate(character(len=0) :: output)
    do
        read(u, '(A)', iostat=ios) line
        if (ios /= 0) exit
        output = output // trim(line) // new_line('a')
    end do
    close(u)

    if (len(output) == 0) then
        print *, "FAIL: empty output"
        stop 1
    end if

    i = 1
    do while (i <= len(output))
        if (output(i:i) /= ' ' .and. output(i:i) /= new_line('a')) exit
        i = i + 1
    end do
    if (i > len(output)) then
        print *, "FAIL: no non-empty content"
        stop 1
    end if
    if (output(i:i) /= '[') then
        print *, "FAIL: first char is not ["
        stop 1
    end if
    if (i /= 1) then
        print *, "FAIL: leading space before ["
        stop 1
    end if

    extra_arrays = 0
    in_string = .false.
    i = 1
    do while (i < len(output))
        if (output(i:i) == '"') in_string = .not. in_string
        if (.not. in_string) then
            if (output(i:i) == ']') then
                j = i + 1
                do while (j <= len(output))
                    if (output(j:j) /= ' ' .and. output(j:j) /= new_line('a')) exit
                    j = j + 1
                end do
                if (j <= len(output)) then
                    if (output(j:j) == '[') extra_arrays = extra_arrays + 1
                end if
            end if
        end if
        i = i + 1
    end do
    if (extra_arrays > 0) then
        print *, "FAIL: multiple top-level arrays"
        stop 1
    end if

    has_file1 = .false.
    has_file2 = .false.
    if (index(output, "Unused variable: x") > 0) has_file1 = .true.
    if (index(output, "Unused variable: y") > 0) has_file2 = .true.
    if (.not. has_file1) then
        print *, "FAIL: file1 diagnostic not in output"
        stop 1
    end if
    if (.not. has_file2) then
        print *, "FAIL: file2 diagnostic not in output"
        stop 1
    end if

    print *, "PASS"
    stop 0

end program test_combined_json
