program test_lsp_diagnostics
    use fluff_ast, only: fluff_ast_context_t
    use fluff_json, only: json_array_length, json_get_member_json, &
                          json_get_string_member, &
                          json_parse
    use fluff_linter, only: linter_engine_t
    use fluff_lsp_protocol, only: lsp_clear_diagnostics_notification, &
                                  lsp_format_diagnostic, &
                                  lsp_publish_diagnostics_notification
    use fluff_lsp_server, only: fluff_lsp_server_t
    use fluff_diagnostics, only: diagnostic_t
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer :: total_tests, passed_tests

    print *, "=== LSP Diagnostic Publishing Test Suite ==="

    total_tests = 0
    passed_tests = 0

    call test_server_publish_matches_formatter()
    call test_clear_diagnostics_notification()
    call test_realtime_updates()
    call test_single_diagnostic_format_json()

    print *, ""
    print *, "=== LSP Diagnostics Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests, dp)/real(total_tests, dp)*100.0_dp, &
        "%"

    if (passed_tests /= total_tests) error stop 1
    stop 0

contains

    subroutine test_server_publish_matches_formatter()
        type(fluff_lsp_server_t) :: server
        character(len=:), allocatable :: expected, actual
        logical :: ok, found

        call server%initialize("/project/root")

        call lint_to_notification("file:///test.f90", sample_code_ok(), expected, ok)
        total_tests = total_tests + 1
        if (.not. ok) then
            print *, "[FAIL] Expected notification generation"
            return
        end if

        call server%handle_text_document_did_open("file:///test.f90", "fortran", 1, &
                                                  sample_code_ok(), ok)
        if (.not. ok) then
            print *, "[FAIL] Server didOpen"
            return
        end if

        call server%pop_notification(actual, found)
        if (.not. found) then
            print *, "[FAIL] Server did not produce diagnostics notification"
            return
        end if

        if (actual /= expected) then
            print *, "[FAIL] Server diagnostics notification mismatch"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] Server diagnostics matches formatter"
    end subroutine test_server_publish_matches_formatter

    subroutine test_clear_diagnostics_notification()
        character(len=:), allocatable :: notification
        character(len=:), allocatable :: err
        character(len=:), allocatable :: params_json, diagnostics_json
        logical :: ok, found
        integer :: n

        total_tests = total_tests + 1
        call lsp_clear_diagnostics_notification("file:///test.f90", notification, ok)
        if (.not. ok) then
            print *, "[FAIL] Clear notification generation"
            return
        end if

        call json_parse(notification, ok, err)
        if (.not. ok) then
            print *, "[FAIL] Clear notification JSON invalid"
            return
        end if

        call json_get_member_json(notification, "params", params_json, found, ok)
        if (.not. ok .or. .not. found) then
            print *, "[FAIL] Clear notification missing params"
            return
        end if

        call json_get_member_json(params_json, "diagnostics", &
                                  diagnostics_json, found, ok)
        if (.not. ok .or. .not. found) then
            print *, "[FAIL] Clear notification missing diagnostics"
            return
        end if

        call json_array_length(diagnostics_json, n, ok)
        if (.not. ok .or. n /= 0) then
            print *, "[FAIL] Clear notification diagnostics not empty"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] Clear diagnostics notification"
    end subroutine test_clear_diagnostics_notification

    subroutine test_realtime_updates()
        type(fluff_lsp_server_t) :: server
        character(len=:), allocatable :: expected1, expected2
        character(len=:), allocatable :: actual1, actual2
        logical :: ok, found

        call server%initialize("/project/root")

        call lint_to_notification("file:///rt.f90", sample_code_ok(), expected1, ok)
        total_tests = total_tests + 1
        if (.not. ok) then
            print *, "[FAIL] Expected realtime notification 1"
            return
        end if

        call lint_to_notification("file:///rt.f90", sample_code_changed(), &
                                  expected2, ok)
        if (.not. ok) then
            print *, "[FAIL] Expected realtime notification 2"
            return
        end if

        call server%handle_text_document_did_open("file:///rt.f90", "fortran", 1, &
                                                  sample_code_ok(), ok)
        if (.not. ok) then
            print *, "[FAIL] Server didOpen realtime"
            return
        end if
        call server%pop_notification(actual1, found)
        if (.not. found .or. actual1 /= expected1) then
            print *, "[FAIL] Realtime open notification mismatch"
            return
        end if

        call server%handle_text_document_did_change("file:///rt.f90", 2, &
                                                    sample_code_changed(), &
                                                    ok)
        if (.not. ok) then
            print *, "[FAIL] Server didChange realtime"
            return
        end if
        call server%pop_notification(actual2, found)
        if (.not. found .or. actual2 /= expected2) then
            print *, "[FAIL] Realtime change notification mismatch"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] Realtime diagnostics updates"
    end subroutine test_realtime_updates

    subroutine test_single_diagnostic_format_json()
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: formatted
        logical :: ok

        call lint_to_diagnostics(sample_code_ok(), diagnostics, ok)
        total_tests = total_tests + 1
        if (.not. ok) then
            print *, "[FAIL] Lint failed for formatting test"
            return
        end if

        if (size(diagnostics) == 0) then
            passed_tests = passed_tests + 1
            print *, "[OK] Diagnostic formatting skipped with no diagnostics"
            return
        end if

        call lsp_format_diagnostic(diagnostics(1), formatted, ok)
        if (.not. ok) then
            print *, "[FAIL] Diagnostic formatting failed"
            return
        end if

        call assert_valid_json(formatted, ok)
        if (.not. ok) then
            print *, "[FAIL] Diagnostic formatting JSON invalid"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] Diagnostic formatting JSON"
    end subroutine test_single_diagnostic_format_json

    subroutine lint_to_notification(uri, content, notification, success)
        character(len=*), intent(in) :: uri
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: notification
        logical, intent(out) :: success

        type(diagnostic_t), allocatable :: diagnostics(:)
        logical :: ok

        call lint_to_diagnostics(content, diagnostics, ok)
        if (.not. ok) then
            success = .false.
            notification = ""
            return
        end if

        call lsp_publish_diagnostics_notification(uri, diagnostics, &
                                                  notification, success)
    end subroutine lint_to_notification

    subroutine lint_to_diagnostics(content, diagnostics, success)
        character(len=*), intent(in) :: content
        type(diagnostic_t), allocatable, intent(out) :: diagnostics(:)
        logical, intent(out) :: success

        type(linter_engine_t) :: linter
        type(fluff_ast_context_t) :: ast_ctx
        character(len=:), allocatable :: error_msg

        call linter%initialize()
        call ast_ctx%from_source(content, error_msg)
        if (error_msg /= "") then
            success = .false.
            allocate (diagnostics(0))
            return
        end if

        call linter%lint_ast(ast_ctx, diagnostics)
        success = .true.
    end subroutine lint_to_diagnostics

    subroutine assert_valid_json(text, success)
        character(len=*), intent(in) :: text
        logical, intent(out) :: success

        character(len=:), allocatable :: err

        call json_parse(text, success, err)
    end subroutine assert_valid_json

    function sample_code_ok() result(code)
        character(len=:), allocatable :: code

        code = "program test"//new_line('a')// &
               "  implicit none"//new_line('a')// &
               "  integer :: x"//new_line('a')// &
               "  x = 1"//new_line('a')// &
               "end program"
    end function sample_code_ok

    function sample_code_changed() result(code)
        character(len=:), allocatable :: code

        code = "program test"//new_line('a')// &
               "  implicit none"//new_line('a')// &
               "  integer :: x"//new_line('a')// &
               "  x = 2"//new_line('a')// &
               "end program"
    end function sample_code_changed

end program test_lsp_diagnostics
