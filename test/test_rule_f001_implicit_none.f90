program test_rule_f001_implicit_none
    ! Test F001: Missing implicit none rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked
    implicit none

    print *, "Testing F001: Missing implicit none rule..."

    ! Test 1: Program without implicit none (should trigger)
    call test_missing_implicit_none()

    ! Test 2: Program with implicit none (should not trigger)
    call test_has_implicit_none()

    ! Test 3: Module without implicit none (should trigger)
    call test_module_missing_implicit_none()

    ! Test 4: Subroutine without implicit none (should trigger)
    call test_subroutine_missing_implicit_none()

    ! Test 5: Interface blocks should not trigger
    call test_interface_block()

    print *, "All F001 tests passed!"

contains

    subroutine test_missing_implicit_none()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f001

        ! Enable test - fortfront is available

        test_code = "program test"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 42"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f001", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F001 violation
        found_f001 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F001") then
                    found_f001 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f001) then
            error stop "Failed: F001 should be triggered for missing implicit none"
        end if

        print *, "  ✓ Missing implicit none in program"

    end subroutine test_missing_implicit_none

    subroutine test_has_implicit_none()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f001

        ! Enable test - fortfront is available

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 42"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f001_ok", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F001 violation
        found_f001 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F001") then
                    found_f001 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f001) then
            error stop "Failed: F001 should not be triggered when implicit none "// &
                "is present"
        end if

        print *, "  ✓ Has implicit none"

    end subroutine test_has_implicit_none

    subroutine test_module_missing_implicit_none()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f001

        test_code = "module test_mod"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    contains"//new_line('a')// &
                    "    subroutine test_sub()"//new_line('a')// &
                    "        x = 42"//new_line('a')// &
                    "    end subroutine test_sub"//new_line('a')// &
                    "end module test_mod"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f001_mod", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F001 violation
        found_f001 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F001") then
                    found_f001 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f001) then
            error stop "Failed: F001 should be triggered for module missing "// &
                "implicit none"
        end if

        print *, "  ✓ Module missing implicit none"

    end subroutine test_module_missing_implicit_none

    subroutine test_subroutine_missing_implicit_none()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f001

        test_code = "subroutine test_sub(x)"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = x + 1"//new_line('a')// &
                    "end subroutine test_sub"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f001_sub", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F001 violation
        found_f001 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F001") then
                    found_f001 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f001) then
            error stop "Failed: F001 should be triggered for subroutine missing "// &
                "implicit none"
        end if

        print *, "  ✓ Subroutine missing implicit none"

    end subroutine test_subroutine_missing_implicit_none

    subroutine test_interface_block()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f001

        ! Interface blocks should NOT trigger F001
        test_code = "module test_mod"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    interface"//new_line('a')// &
                    "        subroutine external_sub(x)"//new_line('a')// &
                    "            integer :: x"//new_line('a')// &
                    "        end subroutine external_sub"//new_line('a')// &
                    "    end interface"//new_line('a')// &
                    "end module test_mod"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f001_interface", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check that F001 is NOT triggered for interface blocks
        found_f001 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F001") then
                    found_f001 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f001) then
            error stop "Failed: F001 should not be triggered for interface blocks"
        end if

        print *, "  ✓ Interface block handling"

    end subroutine test_interface_block

end program test_rule_f001_implicit_none
