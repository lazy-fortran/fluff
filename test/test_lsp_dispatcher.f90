program test_lsp_dispatcher
    use fluff_json, only: json_get_member_json, json_get_string_member, &
                          json_has_member, json_parse
    use fluff_lsp_dispatcher, only: lsp_dispatcher_t
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer :: total_tests, passed_tests

    print *, "=== LSP Dispatcher Test Suite ==="

    total_tests = 0
    passed_tests = 0

    call test_initialize_request()
    call test_shutdown_request()
    call test_initialized_notification()
    call test_exit_notification()
    call test_document_lifecycle_via_dispatcher()
    call test_malformed_input()
    call test_unknown_method()

    print *, ""
    print *, "=== LSP Dispatcher Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests, dp)/real(total_tests, dp)* &
        100.0_dp, "%"

    if (passed_tests /= total_tests) error stop 1
    stop 0

contains

    subroutine test_initialize_request()
        type(lsp_dispatcher_t) :: dispatcher
        character(len=:), allocatable :: response
        character(len=:), allocatable :: init_request
        character(len=:), allocatable :: result_json, caps_json
        logical :: has_response, found, ok

        print *, ""
        print *, "Testing initialize request..."

        init_request = '{"jsonrpc":"2.0","id":1,"method":"initialize",'// &
                       '"params":{"processId":12345,'// &
                       '"rootPath":"/test/project","capabilities":{}}}'

        total_tests = total_tests + 1
        call dispatcher%dispatch(init_request, response, has_response)

        if (.not. has_response) then
            print *, "[FAIL] Initialize - no response"
            return
        end if

        call json_parse(response, ok, result_json)
        if (.not. ok) then
            print *, "[FAIL] Initialize - invalid JSON response"
            return
        end if

        call json_get_member_json(response, "result", result_json, found, ok)
        if (.not. ok .or. .not. found) then
            print *, "[FAIL] Initialize - missing result"
            return
        end if

        call json_has_member(result_json, "capabilities", found, ok)
        if (.not. ok .or. .not. found) then
            print *, "[FAIL] Initialize - missing capabilities"
            return
        end if

        call json_has_member(result_json, "serverInfo", found, ok)
        if (.not. ok .or. .not. found) then
            print *, "[FAIL] Initialize - missing serverInfo"
            return
        end if

        if (.not. dispatcher%server%is_initialized) then
            print *, "[FAIL] Initialize - server not marked initialized"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] Initialize request"
    end subroutine test_initialize_request

    subroutine test_shutdown_request()
        type(lsp_dispatcher_t) :: dispatcher
        character(len=:), allocatable :: response
        character(len=:), allocatable :: init_request, shutdown_request
        character(len=:), allocatable :: result_json
        logical :: has_response, found, ok

        print *, ""
        print *, "Testing shutdown request..."

        init_request = '{"jsonrpc":"2.0","id":1,"method":"initialize",'// &
                       '"params":{"processId":12345,"rootPath":"/test"}}'
        call dispatcher%dispatch(init_request, response, has_response)

        shutdown_request = '{"jsonrpc":"2.0","id":2,"method":"shutdown"}'

        total_tests = total_tests + 1
        call dispatcher%dispatch(shutdown_request, response, has_response)

        if (.not. has_response) then
            print *, "[FAIL] Shutdown - no response"
            return
        end if

        call json_parse(response, ok, result_json)
        if (.not. ok) then
            print *, "[FAIL] Shutdown - invalid JSON response"
            return
        end if

        call json_get_member_json(response, "result", result_json, found, ok)
        if (.not. ok .or. .not. found) then
            print *, "[FAIL] Shutdown - missing result"
            return
        end if

        if (result_json /= "null") then
            print *, "[FAIL] Shutdown - result should be null"
            return
        end if

        if (.not. dispatcher%server%is_shutdown) then
            print *, "[FAIL] Shutdown - server not marked shutdown"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] Shutdown request"
    end subroutine test_shutdown_request

    subroutine test_initialized_notification()
        type(lsp_dispatcher_t) :: dispatcher
        character(len=:), allocatable :: response
        character(len=:), allocatable :: init_request, initialized_notif
        logical :: has_response

        print *, ""
        print *, "Testing initialized notification..."

        init_request = '{"jsonrpc":"2.0","id":1,"method":"initialize",'// &
                       '"params":{"processId":12345,"rootPath":"/test"}}'
        call dispatcher%dispatch(init_request, response, has_response)

        initialized_notif = '{"jsonrpc":"2.0","method":"initialized","params":{}}'

        total_tests = total_tests + 1
        call dispatcher%dispatch(initialized_notif, response, has_response)

        if (has_response) then
            print *, "[FAIL] Initialized notification - should not have response"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] Initialized notification"
    end subroutine test_initialized_notification

    subroutine test_exit_notification()
        type(lsp_dispatcher_t) :: dispatcher
        character(len=:), allocatable :: response
        character(len=:), allocatable :: init_request, exit_notif
        logical :: has_response

        print *, ""
        print *, "Testing exit notification..."

        init_request = '{"jsonrpc":"2.0","id":1,"method":"initialize",'// &
                       '"params":{"processId":12345,"rootPath":"/test"}}'
        call dispatcher%dispatch(init_request, response, has_response)

        exit_notif = '{"jsonrpc":"2.0","method":"exit"}'

        total_tests = total_tests + 1
        call dispatcher%dispatch(exit_notif, response, has_response)

        if (has_response) then
            print *, "[FAIL] Exit notification - should not have response"
            return
        end if

        if (.not. dispatcher%should_exit) then
            print *, "[FAIL] Exit notification - should_exit not set"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] Exit notification"
    end subroutine test_exit_notification

    subroutine test_document_lifecycle_via_dispatcher()
        type(lsp_dispatcher_t) :: dispatcher
        character(len=:), allocatable :: response
        character(len=:), allocatable :: init_request
        character(len=:), allocatable :: did_open, did_change, did_save, did_close
        character(len=:), allocatable :: notification
        logical :: has_response, found

        print *, ""
        print *, "Testing document lifecycle via dispatcher..."

        init_request = '{"jsonrpc":"2.0","id":1,"method":"initialize",'// &
                       '"params":{"processId":12345,"rootPath":"/test"}}'
        call dispatcher%dispatch(init_request, response, has_response)

        did_open = '{"jsonrpc":"2.0","method":"textDocument/didOpen",'// &
                   '"params":{"textDocument":{'// &
                   '"uri":"file:///test.f90","languageId":"fortran",'// &
                   '"version":1,"text":"program test\n  implicit none\nend program"}}}'

        total_tests = total_tests + 1
        call dispatcher%dispatch(did_open, response, has_response)

        if (has_response) then
            print *, "[FAIL] didOpen - should not have response"
            return
        end if

        if (dispatcher%server%workspace%document_count /= 1) then
            print *, "[FAIL] didOpen - document not added"
            return
        end if

        call dispatcher%server%pop_notification(notification, found)
        if (.not. found) then
            print *, "[FAIL] didOpen - no diagnostics published"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] didOpen via dispatcher"

        did_change = '{"jsonrpc":"2.0","method":"textDocument/didChange",'// &
                     '"params":{"textDocument":{'// &
                     '"uri":"file:///test.f90","version":2},'// &
                     '"contentChanges":[{"text":"program changed\n  '// &
                     'implicit none\nend program"}]}}'

        total_tests = total_tests + 1
        call dispatcher%dispatch(did_change, response, has_response)

        if (has_response) then
            print *, "[FAIL] didChange - should not have response"
            return
        end if

        if (dispatcher%server%workspace%documents(1)%version /= 2) then
            print *, "[FAIL] didChange - version not updated"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] didChange via dispatcher"

        did_save = '{"jsonrpc":"2.0","method":"textDocument/didSave",'// &
                   '"params":{"textDocument":{"uri":"file:///test.f90"}}}'

        total_tests = total_tests + 1
        call dispatcher%dispatch(did_save, response, has_response)

        if (has_response) then
            print *, "[FAIL] didSave - should not have response"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] didSave via dispatcher"

        did_close = '{"jsonrpc":"2.0","method":"textDocument/didClose",'// &
                    '"params":{"textDocument":{"uri":"file:///test.f90"}}}'

        total_tests = total_tests + 1
        call dispatcher%dispatch(did_close, response, has_response)

        if (has_response) then
            print *, "[FAIL] didClose - should not have response"
            return
        end if

        if (dispatcher%server%workspace%document_count /= 0) then
            print *, "[FAIL] didClose - document not removed"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] didClose via dispatcher"
    end subroutine test_document_lifecycle_via_dispatcher

    subroutine test_malformed_input()
        type(lsp_dispatcher_t) :: dispatcher
        character(len=:), allocatable :: response
        character(len=:), allocatable :: error_json
        logical :: has_response, found, ok

        print *, ""
        print *, "Testing malformed input handling..."

        total_tests = total_tests + 1
        call dispatcher%dispatch("not valid json", response, has_response)

        if (.not. has_response) then
            print *, "[FAIL] Malformed input - should have error response"
            return
        end if

        call json_has_member(response, "error", found, ok)
        if (.not. ok .or. .not. found) then
            print *, "[FAIL] Malformed input - missing error in response"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] Malformed input handling"

        total_tests = total_tests + 1
        call dispatcher%dispatch("{}", response, has_response)

        if (.not. has_response) then
            print *, "[FAIL] Empty object - should have error response"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] Empty object handling"
    end subroutine test_malformed_input

    subroutine test_unknown_method()
        type(lsp_dispatcher_t) :: dispatcher
        character(len=:), allocatable :: response
        character(len=:), allocatable :: unknown_request
        character(len=:), allocatable :: err_json
        logical :: has_response, found, ok

        print *, ""
        print *, "Testing unknown method handling..."

        unknown_request = '{"jsonrpc":"2.0","id":1,'// &
                          '"method":"unknownMethod","params":{}}'

        total_tests = total_tests + 1
        call dispatcher%dispatch(unknown_request, response, has_response)

        if (.not. has_response) then
            print *, "[FAIL] Unknown method - should have error response"
            return
        end if

        call json_has_member(response, "error", found, ok)
        if (.not. ok .or. .not. found) then
            print *, "[FAIL] Unknown method - missing error in response"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "[OK] Unknown method handling"
    end subroutine test_unknown_method

end program test_lsp_dispatcher
