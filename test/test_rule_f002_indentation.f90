program test_rule_f002_indentation
    ! Test F002: Inconsistent indentation rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked
    implicit none

    print *, "Testing F002: Inconsistent indentation rule..."

    ! Test 1: Inconsistent indentation (should trigger)
    call test_inconsistent_indentation()

    ! Test 2: Consistent 4-space indentation (should not trigger)
    call test_consistent_4_spaces()

    ! Test 3: Consistent 2-space indentation (should not trigger)
    call test_consistent_2_spaces()

    ! Test 4: Mixed indentation levels (should trigger)
    call test_mixed_levels()

    print *, "All F002 tests passed!"

contains

    subroutine test_inconsistent_indentation()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f002

        ! Note: using explicit spaces for clarity
        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &  ! 4 spaces
                    "  integer :: x"//new_line('a')// &      ! 2 spaces (inconsistent)
                    "    x = 42"//new_line('a')// &          ! 4 spaces
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f002", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F002 violation
        found_f002 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F002") then
                    found_f002 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f002) then
            error stop "Failed: F002 should be triggered for inconsistent indentation"
        end if

        print *, "  ✓ Inconsistent indentation"

    end subroutine test_inconsistent_indentation

    subroutine test_consistent_4_spaces()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f002

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 42"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f002_ok", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F002 violation
        found_f002 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F002") then
                    found_f002 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f002) then
            error stop "Failed: F002 should not be triggered for consistent indentation"
        end if

        print *, "  ✓ Consistent 4-space indentation"

    end subroutine test_consistent_4_spaces

    subroutine test_consistent_2_spaces()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f002

        test_code = "program test"//new_line('a')// &
                    "  implicit none"//new_line('a')// &
                    "  integer :: x"//new_line('a')// &
                    "  x = 42"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f002_2space", path)
        call write_text_file(path, test_code)

        call lint_file_checked(linter, path, diagnostics)

        found_f002 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F002") then
                    found_f002 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f002) then
            error stop "Failed: F002 should not be triggered for consistent "// &
                "2-space indentation"
        end if

        print *, "  ✓ Consistent 2-space indentation"
    end subroutine test_consistent_2_spaces

    subroutine test_mixed_levels()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f002

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "      x = 1"//new_line('a')// &  ! 6 spaces: inconsistent with 4
                    "end program test"

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_f002_mixed", path)
        call write_text_file(path, test_code)

        call lint_file_checked(linter, path, diagnostics)

        found_f002 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F002") then
                    found_f002 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f002) then
            error stop "Failed: F002 should be triggered for mixed indentation levels"
        end if

        print *, "  ✓ Mixed indentation levels"
    end subroutine test_mixed_levels

end program test_rule_f002_indentation
