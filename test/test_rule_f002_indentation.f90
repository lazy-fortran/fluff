program test_rule_f002_indentation
    ! Test F002: Inconsistent indentation rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
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
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f002

        ! Note: using explicit spaces for clarity
        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &  ! 4 spaces
                    "  integer :: x"//new_line('a')// &      ! 2 spaces (inconsistent)
                    "    x = 42"//new_line('a')// &          ! 4 spaces
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file="test_f002.f90", status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file("test_f002.f90", diagnostics, error_msg)

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

        ! Clean up
        open (unit=99, file="test_f002.f90", status="old")
        close (99, status="delete")

        if (.not. found_f002) then
            error stop "Failed: F002 should be triggered for inconsistent indentation"
        end if

        print *, "  ✓ Inconsistent indentation"

    end subroutine test_inconsistent_indentation

    subroutine test_consistent_4_spaces()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f002

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = 42"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        open (unit=99, file="test_f002_ok.f90", status="replace")
        write (99, '(A)') test_code
        close (99)

        ! Lint the file
        call linter%lint_file("test_f002_ok.f90", diagnostics, error_msg)

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

        ! Clean up
        open (unit=99, file="test_f002_ok.f90", status="old")
        close (99, status="delete")

        if (found_f002) then
            error stop "Failed: F002 should not be triggered for consistent indentation"
        end if

        print *, "  ✓ Consistent 4-space indentation"

    end subroutine test_consistent_4_spaces

    subroutine test_consistent_2_spaces()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f002

        test_code = "program test"//new_line('a')// &
                    "  implicit none"//new_line('a')// &
                    "  integer :: x"//new_line('a')// &
                    "  x = 42"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        open (unit=99, file="test_f002_2space.f90", status="replace")
        write (99, '(A)') test_code
        close (99)

        call linter%lint_file("test_f002_2space.f90", diagnostics, error_msg)

        found_f002 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F002") then
                    found_f002 = .true.
                    exit
                end if
            end do
        end if

        open (unit=99, file="test_f002_2space.f90", status="old")
        close (99, status="delete")

        if (found_f002) then
    error stop "Failed: F002 should not be triggered for consistent 2-space indentation"
        end if

        print *, "  ✓ Consistent 2-space indentation"
    end subroutine test_consistent_2_spaces

    subroutine test_mixed_levels()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f002

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "      x = 1"//new_line('a')// &  ! 6 spaces: inconsistent with 4
                    "end program test"

        linter = create_linter_engine()

        open (unit=99, file="test_f002_mixed.f90", status="replace")
        write (99, '(A)') test_code
        close (99)

        call linter%lint_file("test_f002_mixed.f90", diagnostics, error_msg)

        found_f002 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F002") then
                    found_f002 = .true.
                    exit
                end if
            end do
        end if

        open (unit=99, file="test_f002_mixed.f90", status="old")
        close (99, status="delete")

        if (.not. found_f002) then
            error stop "Failed: F002 should be triggered for mixed indentation levels"
        end if

        print *, "  ✓ Mixed indentation levels"
    end subroutine test_mixed_levels

end program test_rule_f002_indentation
