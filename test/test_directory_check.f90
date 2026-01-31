program test_directory_check
    ! Test directory argument support for check command (issue #207)
    use fluff_cli, only: path_is_directory, expand_file_arguments
    implicit none

    logical :: all_passed

    print *, "Testing directory argument support (issue #207)..."

    all_passed = .true.

    call test_path_is_directory(all_passed)
    call test_expand_file_arguments(all_passed)

    if (all_passed) then
        print *, "[OK] All directory check tests passed!"
    else
        error stop "Some directory check tests failed"
    end if

contains

    subroutine test_path_is_directory(all_passed)
        logical, intent(inout) :: all_passed
        logical :: is_dir

        is_dir = path_is_directory("/tmp")
        if (.not. is_dir) then
            print *, "[FAIL] /tmp should be detected as directory"
            all_passed = .false.
        else
            print *, "[OK] /tmp detected as directory"
        end if

        is_dir = path_is_directory("/nonexistent_path_12345")
        if (is_dir) then
            print *, "[FAIL] Nonexistent path should not be a directory"
            all_passed = .false.
        else
            print *, "[OK] Nonexistent path not detected as directory"
        end if

    end subroutine test_path_is_directory

    subroutine test_expand_file_arguments(all_passed)
        logical, intent(inout) :: all_passed
        character(len=:), allocatable :: input_files(:), expanded(:)
        integer :: i

        call setup_test_directory()

        allocate (character(len=100) :: input_files(1))
        input_files(1) = "/tmp/fluff_test_dir_207"

        call expand_file_arguments(input_files, expanded)

        if (.not. allocated(expanded)) then
            print *, "[FAIL] expand_file_arguments should return files"
            all_passed = .false.
            call cleanup_test_directory()
            return
        end if

        if (size(expanded) < 2) then
            print *, "[FAIL] Expected at least 2 Fortran files, got ", size(expanded)
            all_passed = .false.
        else
            print *, "[OK] Found ", size(expanded), " Fortran files in test directory"
        end if

        call cleanup_test_directory()

    end subroutine test_expand_file_arguments

    subroutine setup_test_directory()
        call execute_command_line("mkdir -p /tmp/fluff_test_dir_207/subdir", wait=.true.)
        call execute_command_line("echo 'program test1' > /tmp/fluff_test_dir_207/test1.f90", &
                                  wait=.true.)
        call execute_command_line("echo 'program test2' > /tmp/fluff_test_dir_207/test2.F90", &
                                  wait=.true.)
        call execute_command_line("echo 'program test3' > /tmp/fluff_test_dir_207/subdir/test3.f90", &
                                  wait=.true.)
        call execute_command_line("echo 'not fortran' > /tmp/fluff_test_dir_207/readme.txt", &
                                  wait=.true.)
    end subroutine setup_test_directory

    subroutine cleanup_test_directory()
        call execute_command_line("rm -rf /tmp/fluff_test_dir_207", wait=.true.)
    end subroutine cleanup_test_directory

end program test_directory_check
