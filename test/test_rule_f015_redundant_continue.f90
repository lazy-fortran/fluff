program test_rule_f015_redundant_continue
    ! Test F015: Redundant continue statements rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked
    implicit none

    print *, "Testing F015: Redundant continue statements rule..."

    ! Test 1: Redundant continue statements (should trigger)
    call test_redundant_continue()

    ! Test 2: No continue statements (should not trigger)
    call test_no_continue()

    ! Test 3: Necessary continue statements
    call test_necessary_continue()

    ! Test 4: Loop labels and continue
    call test_loop_labels_continue()

    ! Test 5: I/O label targets and continue
    call test_io_label_targets_continue()

    ! Test 6: Positional FORMAT label and continue
    call test_positional_format_label_continue()

    print *, "All F015 tests passed!"

contains

    subroutine test_redundant_continue()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f015

        ! Enable test - fortfront is now available

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    do i = 1, 10"//new_line('a')// &
                    "        if (i == 5) then"//new_line('a')// &
                    "            continue"//new_line('a')// &  ! Redundant continue
                    "        end if"//new_line('a')// &
                    "        print *, i"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "    "//new_line('a')// &
                    "10  continue"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f015", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F015 violation
        found_f015 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F015") then
                    found_f015 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f015) then
            error stop "Failed: F015 should be triggered for redundant continue"
        end if

        print *, "  ✓ Redundant continue statements"

    end subroutine test_redundant_continue

    subroutine test_no_continue()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f015

        ! Enable test - fortfront is now available

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    do i = 1, 10"//new_line('a')// &
                    "        if (i == 5) then"//new_line('a')// &
                    "            exit"//new_line('a')// &
                    "        end if"//new_line('a')// &
                    "        print *, i"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f015_ok", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F015 violation
        found_f015 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F015") then
                    found_f015 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f015) then
            error stop "Failed: F015 should not be triggered when no continue "// &
                "statements"
        end if

        print *, "  ✓ No continue statements"

    end subroutine test_no_continue

    subroutine test_necessary_continue()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f015

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 1"//new_line('a')// &
                    "    if (x == 1) goto 20"//new_line('a')// &
                    "    x = 2"//new_line('a')// &
                    "20  continue"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f015_needed", path)
        call write_text_file(path, test_code)

        call lint_file_checked(linter, path, diagnostics)

        found_f015 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F015") then
                    found_f015 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f015) then
            error stop "Failed: F015 should not be triggered for labeled branch target"
        end if

        print *, "  ✓ Necessary continue statements"
    end subroutine test_necessary_continue

    subroutine test_loop_labels_continue()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f015

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i, s"//new_line('a')// &
                    "    s = 0"//new_line('a')// &
                    "    do 30 i = 1, 3"//new_line('a')// &
                    "        s = s + i"//new_line('a')// &
                    "30  continue"//new_line('a')// &
                    "    print *, s"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f015_do_label", path)
        call write_text_file(path, test_code)

        call lint_file_checked(linter, path, diagnostics)

        found_f015 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F015") then
                    found_f015 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f015) then
            error stop "Failed: F015 should not be triggered for DO end label"
        end if

        print *, "  ✓ Loop labels and continue"
    end subroutine test_loop_labels_continue

    subroutine test_io_label_targets_continue()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f015

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    read(10, *, end=20) x"//new_line('a')// &
                    "    x = x + 1"//new_line('a')// &
                    "20  continue"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f015_io_end", path)
        call write_text_file(path, test_code)

        call lint_file_checked(linter, path, diagnostics)

        found_f015 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F015") then
                    found_f015 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f015) then
            error stop "Failed: F015 should not flag I/O label target CONTINUE"
        end if

        print *, "  ✓ I/O label targets and continue"
    end subroutine test_io_label_targets_continue

    subroutine test_positional_format_label_continue()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f015

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 1"//new_line('a')// &
                    "    write(6, 20) x"//new_line('a')// &
                    "20  continue"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f015_positional_fmt", path)
        call write_text_file(path, test_code)

        call lint_file_checked(linter, path, diagnostics)

        found_f015 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F015") then
                    found_f015 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f015) then
            error stop "Failed: F015 should not flag positional FORMAT label CONTINUE"
        end if

        print *, "  ✓ Positional FORMAT label and continue"
    end subroutine test_positional_format_label_continue

end program test_rule_f015_redundant_continue
