program test_cli_fix_application
    use test_support, only: write_text_file, read_text_file, delete_file_if_exists
    use fluff_fix_applicator, only: apply_fixes_to_file
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use fluff_diagnostics, only: diagnostic_t
    implicit none

    print *, "Testing CLI --fix flag file modification..."

    call test_fix_applies_to_file()
    call test_fix_skips_when_no_fixes()
    call test_fix_preserves_unrelated_content()

    print *, "[OK] All CLI --fix application tests passed!"

contains

    subroutine test_fix_applies_to_file()
        character(len=:), allocatable :: original_content, final_content
        character(len=:), allocatable :: error_msg
        character(len=256) :: test_file
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        integer :: fixes_applied

        print *, "  Testing fix applies to file..."

        test_file = "/tmp/test_fix_applies.f90"

        original_content = "program no_implicit_none" // new_line('a') // &
                          "integer :: x" // new_line('a') // &
                          "x = 42" // new_line('a') // &
                          "end program no_implicit_none"

        call write_text_file(test_file, original_content)

        linter = create_linter_engine()
        call linter%lint_file(trim(test_file), diagnostics, error_msg)

        if (error_msg /= "") then
            print *, "ERROR: Linting failed: ", error_msg
            error stop "Linting failed"
        end if

        if (.not. allocated(diagnostics)) then
            print *, "ERROR: No diagnostics returned"
            error stop "No diagnostics returned"
        end if

        if (size(diagnostics) == 0) then
            print *, "ERROR: Expected diagnostics for missing implicit none"
            error stop "Expected diagnostics"
        end if

        call apply_fixes_to_file(trim(test_file), diagnostics, fixes_applied, error_msg)

        if (error_msg /= "") then
            print *, "ERROR: Fix application failed: ", error_msg
            error stop "Fix application failed"
        end if

        if (fixes_applied == 0) then
            print *, "ERROR: Expected at least one fix to be applied"
            error stop "No fixes applied"
        end if

        call read_text_file(test_file, final_content, error_msg)

        if (error_msg /= "") then
            print *, "ERROR: Could not read fixed file: ", error_msg
            error stop "Could not read fixed file"
        end if

        if (index(final_content, "implicit none") == 0) then
            print *, "ERROR: Fixed file should contain 'implicit none'"
            print *, "Final content: ", final_content
            error stop "Fix not applied to file"
        end if

        call delete_file_if_exists(test_file)

        print *, "[OK] Fix applies to file"

    end subroutine test_fix_applies_to_file

    subroutine test_fix_skips_when_no_fixes()
        character(len=:), allocatable :: error_msg
        character(len=256) :: test_file
        type(diagnostic_t), allocatable :: diagnostics(:)
        integer :: fixes_applied

        print *, "  Testing fix skips when empty diagnostics..."

        test_file = "/tmp/test_fix_skips.f90"

        call write_text_file(test_file, "dummy content")

        allocate (diagnostics(0))

        call apply_fixes_to_file(trim(test_file), diagnostics, fixes_applied, error_msg)

        if (error_msg /= "") then
            print *, "ERROR: Unexpected error: ", error_msg
            error stop "Unexpected error"
        end if

        if (fixes_applied /= 0) then
            print *, "ERROR: No fixes should be applied for empty diagnostics"
            error stop "Unexpected fixes applied"
        end if

        call delete_file_if_exists(test_file)

        print *, "[OK] Fix skips when empty diagnostics"

    end subroutine test_fix_skips_when_no_fixes

    subroutine test_fix_preserves_unrelated_content()
        character(len=:), allocatable :: original_content, final_content
        character(len=:), allocatable :: error_msg
        character(len=256) :: test_file
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        integer :: fixes_applied

        print *, "  Testing fix preserves unrelated content..."

        test_file = "/tmp/test_fix_preserves.f90"

        original_content = "program test_preserve" // new_line('a') // &
                          "! Important comment to preserve" // new_line('a') // &
                          "integer :: important_var" // new_line('a') // &
                          "important_var = 999" // new_line('a') // &
                          "print *, important_var" // new_line('a') // &
                          "end program test_preserve"

        call write_text_file(test_file, original_content)

        linter = create_linter_engine()
        call linter%lint_file(trim(test_file), diagnostics, error_msg)

        if (error_msg /= "") then
            print *, "ERROR: Linting failed: ", error_msg
            error stop "Linting failed"
        end if

        if (.not. allocated(diagnostics)) then
            allocate (diagnostics(0))
        end if

        call apply_fixes_to_file(trim(test_file), diagnostics, fixes_applied, error_msg)

        if (error_msg /= "") then
            print *, "ERROR: Fix application failed: ", error_msg
            error stop "Fix application failed"
        end if

        call read_text_file(test_file, final_content, error_msg)

        if (error_msg /= "") then
            print *, "ERROR: Could not read fixed file: ", error_msg
            error stop "Could not read fixed file"
        end if

        if (index(final_content, "Important comment to preserve") == 0) then
            print *, "ERROR: Comment should be preserved"
            error stop "Comment not preserved"
        end if

        if (index(final_content, "important_var") == 0) then
            print *, "ERROR: Variable should be preserved"
            error stop "Variable not preserved"
        end if

        if (index(final_content, "999") == 0) then
            print *, "ERROR: Value 999 should be preserved"
            error stop "Value not preserved"
        end if

        call delete_file_if_exists(test_file)

        print *, "[OK] Fix preserves unrelated content"

    end subroutine test_fix_preserves_unrelated_content

end program test_cli_fix_application
