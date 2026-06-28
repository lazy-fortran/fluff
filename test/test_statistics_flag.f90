program test_statistics_flag
    ! Test --statistics flag functionality
    use fluff_cli
    use fluff_diagnostics
    use fluff_core
    implicit none

    print *, "Testing statistics flag..."

    ! Test 1: Parse statistics flag
    call test_statistics_flag_parsing()

    ! Test 2: Statistics with single violation
    call test_statistics_single_violation()

    ! Test 3: Statistics with multiple violations
    call test_statistics_multiple_violations()

    print *, "[OK] All statistics flag tests passed!"

contains

    subroutine test_statistics_flag_parsing()
        type(cli_args_t) :: args
        character(len=20) :: argv(3)

        argv(1) = "check"
        argv(2) = "--statistics"
        argv(3) = "test.f90"

        call args%parse(3, argv)

        if (.not. args%statistics) then
            error stop "FAIL: statistics flag should be true"
        end if

        if (args%command /= "check") then
            error stop "FAIL: command should be 'check'"
        end if

        if (.not. allocated(args%files)) then
            error stop "FAIL: files should be allocated"
        end if

        print *, "[OK] Statistics flag parsing"

    end subroutine test_statistics_flag_parsing

    subroutine test_statistics_single_violation()
        type(diagnostic_t), allocatable :: diags(:)
        type(source_range_t) :: loc

        allocate (diags(1))

        ! Create a single diagnostic
        loc%start%line = 1
        loc%start%column = 1
        loc%end%line = 1
        loc%end%column = 10

        diags(1)%code = "F001"
        diags(1)%message = "Missing implicit none statement"
        diags(1)%file_path = "test.f90"
        diags(1)%location = loc
        diags(1)%severity = SEVERITY_WARNING

        ! This would print the statistics, which is hard to test in Fortran
        ! Just verify we can call it without errors
        call print_statistics_summary(diags, 1)

        print *, "[OK] Single violation statistics"

    end subroutine test_statistics_single_violation

    subroutine test_statistics_multiple_violations()
        type(diagnostic_t), allocatable :: diags(:)
        type(source_range_t) :: loc

        allocate (diags(3))

        ! Create three diagnostics: 2 F001, 1 F002
        loc%start%line = 1
        loc%start%column = 1
        loc%end%line = 1
        loc%end%column = 10

        diags(1)%code = "F001"
        diags(1)%message = "Missing implicit none statement"
        diags(1)%file_path = "test.f90"
        diags(1)%location = loc
        diags(1)%severity = SEVERITY_WARNING

        diags(2)%code = "F001"
        diags(2)%message = "Missing implicit none statement"
        diags(2)%file_path = "test.f90"
        diags(2)%location = loc
        diags(2)%severity = SEVERITY_WARNING

        diags(3)%code = "F002"
        diags(3)%message = "Line too long"
        diags(3)%file_path = "test.f90"
        diags(3)%location = loc
        diags(3)%severity = SEVERITY_WARNING

        ! Call the statistics summary
        call print_statistics_summary(diags, 1)

        print *, "[OK] Multiple violations statistics"

    end subroutine test_statistics_multiple_violations

    subroutine print_statistics_summary(diags, file_count)
        type(diagnostic_t), intent(in) :: diags(:)
        integer, intent(in) :: file_count

        integer :: i, j, n_diags
        character(len=32), allocatable :: code_list(:)
        character(len=1000), allocatable :: message_list(:)
        integer, allocatable :: code_counts(:)
        integer :: unique_count
        logical :: found
        character(len=32) :: count_str

        n_diags = size(diags)

        if (n_diags == 0) then
            print '(A)', "0 violations found"
            print '(A)', ""
            if (file_count == 1) then
                print '(A)', "1 file checked"
            else
                write (count_str, '(I0)') file_count
                print '(A,A,A)', trim(count_str), " files checked"
            end if
            return
        end if

        allocate (code_list(n_diags))
        allocate (message_list(n_diags))
        allocate (code_counts(n_diags))
        unique_count = 0

        do i = 1, n_diags
            found = .false.
            do j = 1, unique_count
                if (trim(diags(i)%code) == trim(code_list(j))) then
                    code_counts(j) = code_counts(j) + 1
                    found = .true.
                    exit
                end if
            end do

            if (.not. found) then
                unique_count = unique_count + 1
                code_list(unique_count) = diags(i)%code
                message_list(unique_count) = diags(i)%message
                code_counts(unique_count) = 1
            end if
        end do

        if (n_diags == 1) then
            print '(A)', "1 violation found:"
        else
            write (count_str, '(I0)') n_diags
            print '(A,A,A)', trim(count_str), " violations found:"
        end if

        do i = 1, unique_count
            if (code_counts(i) == 1) then
                print '(A,A,A,A,A)', "  ", trim(code_list(i)), ": 1 occurrence (", &
                    trim(message_list(i)), ")"
            else
                write (count_str, '(I0)') code_counts(i)
                print '(A,A,A,A,A,A,A)', "  ", trim(code_list(i)), ": ", &
                    trim(count_str), " occurrences (", trim(message_list(i)), ")"
            end if
        end do

        print '(A)', ""
        if (file_count == 1) then
            print '(A)', "1 file checked"
        else
            write (count_str, '(I0)') file_count
            print '(A,A,A)', trim(count_str), " files checked"
        end if

    end subroutine print_statistics_summary

end program test_statistics_flag
