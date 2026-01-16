program test_lsp_code_actions
    use fluff_lsp_code_actions, only: apply_code_action, apply_fix_all, &
                                      format_code_action, generate_code_actions, &
                                          get_code_actions_at_position
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer :: total_tests, passed_tests
    real(dp) :: success_rate

    print *, "=== LSP Code Actions Test Suite ==="

    total_tests = 0
    passed_tests = 0

    ! Test code action functionality
    call test_quick_fix_generation()
    call test_code_action_formatting()
    call test_code_action_application()
    call test_multi_fix_scenarios()
    call test_fix_all_functionality()
    call test_code_action_context()

    print *, ""
    print *, "=== LSP Code Actions Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    if (total_tests > 0) then
        success_rate = real(passed_tests, dp)/real(total_tests, dp)*100.0_dp
    else
        success_rate = 0.0_dp
    end if
    print *, "Success rate: ", success_rate, "%"

    if (passed_tests == total_tests) then
        print *, "[OK] All LSP code action tests passed!"
    else
        print *, "[FAIL] Some tests failed"
    end if

contains

    subroutine test_quick_fix_generation()
        print *, ""
        print *, "Testing quick fix generation from diagnostics..."

        ! Test 1: Generate fix for missing implicit none
        call run_fix_test("Missing implicit none fix", &
                          "program test"//new_line('a')// &
                          "integer :: x"//new_line('a')// &
                          "end program", &
                          "F001", "Add implicit none", 1)

        ! Test 2: Generate fix for trailing whitespace
        call run_fix_test("Trailing whitespace fix", &
                          "program test    "//new_line('a')// &
                          "implicit none"//new_line('a')// &
                          "end program", &
                          "F004", "Remove trailing whitespace", 1)

        ! Test 3: Generate fix for inconsistent indentation
        call run_fix_test("Indentation fix", &
                          "program test"//new_line('a')// &
                          "implicit none"//new_line('a')// &
                          "   integer :: x"//new_line('a')// &
                          "end program", &
                          "F002", "Fix indentation", 1)

        ! Test 4: Generate fix for missing intent
        call run_fix_test("Missing intent fix", &
                          "subroutine test(x)"//new_line('a')// &
                          "real :: x"//new_line('a')// &
                          "end subroutine", &
                          "F008", "Add intent", 3)

        ! Test 5: No fix available scenario
        call run_fix_test("No fix available", &
                          "program test"//new_line('a')// &
                          "complex code issue"//new_line('a')// &
                          "end program", &
                          "F999", "", 0)

    end subroutine test_quick_fix_generation

    subroutine test_code_action_formatting()
        print *, ""
        print *, "Testing LSP code action message formatting..."

        ! Test 1: Format code action response
        call run_format_test("Code action format", &
                             "Add implicit none", "F001", 1, 0, &
                             "quickfix", .true.)

        ! Test 2: Format refactor action
        call run_format_test("Refactor action format", &
                             "Extract to function", "REF001", 5, 0, &
                             "refactor", .true.)

        ! Test 3: Format source organize action
        call run_format_test("Source action format", &
                             "Organize use statements", "ORG001", 1, 0, &
                             "source", .true.)

        ! Test 4: Invalid action format
        call run_format_test("Invalid action format", &
                             "", "", -1, -1, &
                             "invalid", .false.)

    end subroutine test_code_action_formatting

    subroutine test_code_action_application()
        print *, ""
        print *, "Testing code action application..."

        ! Test 1: Apply single edit
        call run_apply_test("Apply single edit", &
                            "program test"//new_line('a')// &
                            "integer :: x"//new_line('a')// &
                            "end program", &
                            "program test"//new_line('a')// &
                            "implicit none"//new_line('a')// &
                            "integer :: x"//new_line('a')// &
                            "end program", &
                            1, .true.)

        ! Test 2: Apply multiple edits
        call run_apply_test("Apply multiple edits", &
                            "program test    "//new_line('a')// &
                            "   integer :: x"//new_line('a')// &
                            "end program", &
                            "program test"//new_line('a')// &
                            "    implicit none"//new_line('a')// &
                            "    integer :: x"//new_line('a')// &
                            "end program", &
                            3, .true.)

        ! Test 3: Apply complex refactoring
        call run_apply_test("Apply refactoring", &
                            "x = 1 + 2 + 3", &
                            "temp = 1 + 2"//new_line('a')// &
                            "x = temp + 3", &
                            2, .true.)

        ! Test 4: Failed application
        call run_apply_test("Failed application", &
                            "invalid code", &
                            "invalid code", &
                            0, .false.)

    end subroutine test_code_action_application

    subroutine test_multi_fix_scenarios()
        print *, ""
        print *, "Testing multiple fix scenarios..."

        ! Test 1: Multiple fixes for same diagnostic
        call run_multifix_test("Multiple fix options", &
                               "subroutine test(x)"//new_line('a')// &
                               "real :: x"//new_line('a')// &
                               "end subroutine", &
                               "F008", ["Add intent(in)    ", "Add intent(out)   ", &
                                        "Add intent(inout) "], 3)

        ! Test 2: Fixes for multiple diagnostics
        call run_multifix_test("Multiple diagnostics", &
                               "program test    "//new_line('a')// &
                               "integer :: x"//new_line('a')// &
                               "end program", &
                               "ALL", ["Add implicit none ", "Remove whitespace "], 2)

        ! Test 3: Cascading fixes
        call run_multifix_test("Cascading fixes", &
                               "program test"//new_line('a')// &
                               "x = 1"//new_line('a')// &
                               "end program", &
                               "ALL", ["Add implicit none ", "Declare variable x"], 2)

    end subroutine test_multi_fix_scenarios

    subroutine test_fix_all_functionality()
        print *, ""
        print *, "Testing fix-all functionality..."

        ! Test 1: Fix all in file
        call run_fixall_test("Fix all in file", &
                             ["file:///test.f90"], "F004", 5, .true.)

        ! Test 2: Fix all in workspace
        call run_fixall_test("Fix all in workspace", &
                             ["file:///src/a.f90", "file:///src/b.f90"], &
                                 "F001", 3, .true.)

        ! Test 3: Fix all of type
        call run_fixall_test("Fix all of type", &
                             ["file:///test.f90"], "ALL", 10, .true.)

        ! Test 4: No fixes to apply
        call run_fixall_test("No fixes needed", &
                             ["file:///clean.f90"], "F001", 0, .true.)

    end subroutine test_fix_all_functionality

    subroutine test_code_action_context()
        print *, ""
        print *, "Testing code action context handling..."

        ! Test 1: Context at diagnostic location
        call run_context_test("Diagnostic context", &
                              "file:///test.f90", 2, 5, ["F001"], 1, .true.)

        ! Test 2: Context without diagnostics
        call run_context_test("No diagnostic context", &
                              "file:///test.f90", 10, 0, [""], 0, .true.)

        ! Test 3: Context with multiple diagnostics
        call run_context_test("Multiple diagnostics", &
                              "file:///test.f90", 5, 10, ["F001", "F002"], 2, .true.)

        ! Test 4: Invalid context
        call run_context_test("Invalid context", &
                              "", -1, -1, [""], 0, .false.)

    end subroutine test_code_action_context

    ! Helper subroutines for testing
    subroutine run_fix_test(test_name, code, diagnostic_code, expected_action, &
                            expected_count)
        character(len=*), intent(in) :: test_name, code, diagnostic_code, &
                                        expected_action
        integer, intent(in) :: expected_count

        character(len=:), allocatable :: actions(:)
        integer :: action_count
        logical :: success

        total_tests = total_tests + 1

        action_count = 0
        success = .false.

        ! Generate code actions from diagnostic
        call generate_code_actions(code, diagnostic_code, actions, &
                                   action_count, success)

        if (success .and. action_count == expected_count) then
            if (expected_count > 0) then
                if (.not. allocated(actions)) then
                    print *, "[FAIL] ", test_name, " - Actions not allocated"
                else if (size(actions) < 1) then
                    print *, "[FAIL] ", test_name, " - No actions returned"
                else if (index(actions(1), expected_action) > 0) then
                    print *, "[OK] ", test_name, " - Generated ", &
                        action_count, " actions"
                    passed_tests = passed_tests + 1
                else
                    print *, "[FAIL] ", test_name, " - Wrong action generated"
                end if
            else
                print *, "[OK] ", test_name, " - No actions as expected"
                passed_tests = passed_tests + 1
            end if
        else
            print *, "[FAIL] ", test_name, " - Expected ", expected_count, ", got ", &
                action_count
        end if

    end subroutine run_fix_test

    subroutine run_format_test(test_name, title, code, line, character, kind, &
                               should_succeed)
        character(len=*), intent(in) :: test_name, title, code, kind
        integer, intent(in) :: line, character
        logical, intent(in) :: should_succeed

        character(len=:), allocatable :: formatted_action
        logical :: success

        total_tests = total_tests + 1

        success = .false.

        ! Format code action for LSP
        call format_code_action(title, code, line, character, kind, &
                                formatted_action, success)

        if (success .eqv. should_succeed) then
            print *, "[OK] ", test_name
            passed_tests = passed_tests + 1
        else
            print *, "[FAIL] ", test_name, " - Formatting result unexpected"
        end if

    end subroutine run_format_test

    subroutine run_apply_test(test_name, original, expected, edit_count, should_succeed)
        character(len=*), intent(in) :: test_name, original, expected
        integer, intent(in) :: edit_count
        logical, intent(in) :: should_succeed

        character(len=:), allocatable :: result
        logical :: success

        total_tests = total_tests + 1

        success = .false.

        ! Apply code action edits
        call apply_code_action(original, result, edit_count, success)

        if (success .neqv. should_succeed) then
            print *, "[FAIL] ", test_name, " - Application result unexpected"
        else if (.not. should_succeed) then
            print *, "[OK] ", test_name, " - Application failed as expected"
            passed_tests = passed_tests + 1
        else if (.not. allocated(result)) then
            print *, "[FAIL] ", test_name, " - Result not allocated"
        else if (result == expected) then
            print *, "[OK] ", test_name, " - Applied ", edit_count, " edits"
            passed_tests = passed_tests + 1
        else
            print *, "[FAIL] ", test_name, " - Result doesn't match expected"
            print *, "        Expected:"
            print *, "        '", expected, "'"
            print *, "        Got:"
            print *, "        '", result, "'"
        end if

    end subroutine run_apply_test

    subroutine run_multifix_test(test_name, code, diagnostic_code, expected_actions, &
                                 expected_count)
        character(len=*), intent(in) :: test_name, code, diagnostic_code
        character(len=*), intent(in) :: expected_actions(:)
        integer, intent(in) :: expected_count

        character(len=:), allocatable :: actions(:)
        integer :: action_count
        logical :: success

        total_tests = total_tests + 1

        action_count = 0
        success = .false.

        ! Generate multiple code actions
        call generate_code_actions(code, diagnostic_code, actions, &
                                   action_count, success)

        if (success .and. action_count == expected_count) then
            print *, "[OK] ", test_name, " - Generated ", action_count, " actions"
            passed_tests = passed_tests + 1
        else
            print *, "[FAIL] ", test_name, " - Expected ", expected_count, ", got ", &
                action_count
        end if

    end subroutine run_multifix_test

    subroutine run_fixall_test(test_name, file_uris, diagnostic_code, expected_fixes, &
                               should_succeed)
        character(len=*), intent(in) :: test_name, file_uris(:), diagnostic_code
        integer, intent(in) :: expected_fixes
        logical, intent(in) :: should_succeed

        integer :: fixes_applied
        logical :: success

        total_tests = total_tests + 1

        fixes_applied = 0
        success = .false.

        ! Apply fix-all operation
        call apply_fix_all(file_uris, diagnostic_code, fixes_applied, success)

        if (success .eqv. should_succeed .and. fixes_applied == expected_fixes) then
            print *, "[OK] ", test_name, " - Applied ", fixes_applied, " fixes"
            passed_tests = passed_tests + 1
        else
            print *, "[FAIL] ", test_name, " - Fix-all result unexpected"
        end if

    end subroutine run_fixall_test

    subroutine run_context_test(test_name, uri, line, character, diagnostic_codes, &
                                expected_count, should_succeed)
        character(len=*), intent(in) :: test_name, uri
        integer, intent(in) :: line, character, expected_count
        character(len=*), intent(in) :: diagnostic_codes(:)
        logical, intent(in) :: should_succeed

        integer :: action_count
        logical :: success

        total_tests = total_tests + 1

        action_count = 0
        success = .false.

        ! Get code actions for context
        call get_code_actions_at_position(uri, line, character, diagnostic_codes, &
                                          action_count, success)

        if (success .eqv. should_succeed .and. action_count == expected_count) then
            print *, "[OK] ", test_name, " - Found ", action_count, " actions"
            passed_tests = passed_tests + 1
        else
            print *, "[FAIL] ", test_name, " - Context handling failed"
        end if

    end subroutine run_context_test

end program test_lsp_code_actions
