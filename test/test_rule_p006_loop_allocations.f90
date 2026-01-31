program test_rule_p006_loop_allocations
    ! Test P006: Unnecessary allocations in loops rule
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code, &
                            lint_file_checked
    implicit none

    print *, "Testing P006: Unnecessary allocations in loops rule..."

    call test_allocate_inside_loop_triggers()
    call test_allocate_outside_loop_is_ok()
    call test_deallocate_inside_loop_triggers()
    call test_nested_loop_allocate()
    call test_no_spurious_node_type_warnings()

    print *, "[OK] All P006 tests passed!"

contains

    subroutine test_allocate_inside_loop_triggers()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "integer :: i"//new_line('a')// &
                    "real, allocatable :: a(:)"//new_line('a')// &
                    "do i = 1, 10"//new_line('a')// &
                    "    allocate(a(10))"//new_line('a')// &
                    "    a = real(i)"//new_line('a')// &
                    "    deallocate(a)"//new_line('a')// &
                    "end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p006_bad", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P006", .true., &
                                        "allocate inside loop should be flagged")
        print *, "[OK] Allocate inside loop"
    end subroutine test_allocate_inside_loop_triggers

    subroutine test_allocate_outside_loop_is_ok()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "integer :: i"//new_line('a')// &
                    "real, allocatable :: a(:)"//new_line('a')// &
                    "allocate(a(10))"//new_line('a')// &
                    "do i = 1, 10"//new_line('a')// &
                    "    a = real(i)"//new_line('a')// &
                    "end do"//new_line('a')// &
                    "deallocate(a)"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p006_ok", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P006", .false., &
                                        "allocate outside loop should not be flagged")
        print *, "[OK] Allocate outside loop"
    end subroutine test_allocate_outside_loop_is_ok

    subroutine test_deallocate_inside_loop_triggers()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: count, j

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "integer :: i"//new_line('a')// &
                    "real, allocatable :: a(:)"//new_line('a')// &
                    "allocate(a(10))"//new_line('a')// &
                    "do i = 1, 10"//new_line('a')// &
                    "    a = real(i)"//new_line('a')// &
                    "    deallocate(a)"//new_line('a')// &
                    "    allocate(a(10))"//new_line('a')// &
                    "end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p006_dealloc", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        count = 0
        if (allocated(diagnostics)) then
            do j = 1, size(diagnostics)
                if (diagnostics(j)%code == "P006") count = count + 1
            end do
        end if

        if (count < 2) then
            print *, "[FAIL] Expected at least 2 P006 diagnostics for alloc+dealloc"
            error stop
        end if
        print *, "[OK] Deallocate inside loop triggers"
    end subroutine test_deallocate_inside_loop_triggers

    subroutine test_nested_loop_allocate()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path

        test_code = "program test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "integer :: i, j"//new_line('a')// &
                    "real, allocatable :: a(:)"//new_line('a')// &
                    "do i = 1, 10"//new_line('a')// &
                    "    do j = 1, 10"//new_line('a')// &
                    "        allocate(a(j))"//new_line('a')// &
                    "        deallocate(a)"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end do"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p006_nested", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        call assert_has_diagnostic_code(diagnostics, "P006", .true., &
                                        "nested loop allocate should be flagged")
        print *, "[OK] Nested loop allocate"
    end subroutine test_nested_loop_allocate

    subroutine test_no_spurious_node_type_warnings()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i, p006_count
        logical :: found_other

        test_code = "program alloc_test"//new_line('a')// &
                    "implicit none"//new_line('a')// &
                    "real, allocatable :: temp(:)"//new_line('a')// &
                    "integer :: i"//new_line('a')// &
                    "do i = 1, 100"//new_line('a')// &
                    "    allocate(temp(50))"//new_line('a')// &
                    "    temp = real(i)"//new_line('a')// &
                    "    deallocate(temp)"//new_line('a')// &
                    "end do"//new_line('a')// &
                    "end program alloc_test"

        linter = create_linter_engine()
        call make_temp_fortran_path("fluff_test_p006_issue209", path)
        call write_text_file(path, test_code)
        call lint_file_checked(linter, path, diagnostics)
        call delete_file_if_exists(path)

        p006_count = 0
        found_other = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "P006") then
                    p006_count = p006_count + 1
                else if (diagnostics(i)%code /= "F006") then
                    found_other = .true.
                end if
            end do
        end if

        if (p006_count /= 2) then
            print *, "[FAIL] Expected exactly 2 P006 diagnostics, got ", p006_count
            error stop
        end if
        if (found_other) then
            print *, "[FAIL] Unexpected non-P006/F006 diagnostics found"
            error stop
        end if
        print *, "[OK] No spurious warnings from issue #209 test case"
    end subroutine test_no_spurious_node_type_warnings

end program test_rule_p006_loop_allocations
