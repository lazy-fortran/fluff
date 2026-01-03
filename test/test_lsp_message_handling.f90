program test_lsp_message_handling
    use fluff_json_rpc, only: check_message_errors, parse_lsp_message, &
                              process_capabilities, process_diagnostic_message, &
                              process_document_sync, process_initialize_request
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer :: total_tests, passed_tests

    print *, "=== LSP Message Handling Test Suite ==="

    total_tests = 0
    passed_tests = 0

    ! Test LSP message parsing and handling
    call test_json_rpc_parsing()
    call test_initialize_request()
    call test_document_sync_messages()
    call test_diagnostic_messages()
    call test_capability_negotiation()
    call test_error_handling()

    print *, ""
    print *, "=== LSP Message Handling Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests, dp)/real(total_tests, dp)* &
        100.0_dp, "%"

    if (passed_tests /= total_tests) error stop 1
    stop 0

contains

    subroutine test_json_rpc_parsing()
        print *, ""
        print *, "Testing JSON-RPC message parsing..."

        ! Test 1: Parse basic request message
        call run_message_test("Basic request parsing", &
                              '{"jsonrpc":"2.0","id":1,'// &
                              '"method":"initialize","params":{}}', &
                              "request", 1, "initialize")

        ! Test 2: Parse notification message (no id)
        call run_message_test("Notification parsing", &
                              '{"jsonrpc":"2.0","method":"textDocument/didOpen",'// &
                              '"params":{}}', &
                              "notification", -1, "textDocument/didOpen")

        ! Test 3: Parse response message
        call run_message_test("Response parsing", &
                              '{"jsonrpc":"2.0","id":1,'// &
                              '"result":{"capabilities":{}}}', &
                              "response", 1, "")

        ! Test 4: Parse error response
        call run_message_test("Error response parsing", &
                              '{"jsonrpc":"2.0","id":1,"error":{"code":-32600,'// &
                              '"message":"Invalid Request"}}', &
                              "error", 1, "")

    end subroutine test_json_rpc_parsing

    subroutine test_initialize_request()
        print *, ""
        print *, "Testing LSP initialize request handling..."

        ! Test 1: Initialize with client capabilities
        call run_initialize_test("Initialize with capabilities", &
                                 '{"jsonrpc":"2.0","id":1,"method":"initialize",'// &
                                 '"params":{'// &
                                 '"processId":12345,"rootPath":"/project",'// &
                                 '"capabilities":{"textDocument":{'// &
                                 '"synchronization":{'// &
                                 '"dynamicRegistration":true'// &
                                 '}}}}}')

        ! Test 2: Initialize without optional fields
        call run_initialize_test("Initialize minimal", &
                                 '{"jsonrpc":"2.0","id":1,"method":"initialize",'// &
                                 '"params":{"processId":null}}')

        ! Test 3: Initialize with workspace folders
        call run_initialize_test("Initialize with workspaces", &
                                 '{"jsonrpc":"2.0","id":1,"method":"initialize",'// &
                                 '"params":{'// &
                                 '"processId":12345,'// &
                                 '"workspaceFolders":[{"uri":"file:///project",'// &
                                 '"name":"project"}]}}')

    end subroutine test_initialize_request

    subroutine test_document_sync_messages()
        print *, ""
        print *, "Testing document synchronization messages..."

        ! Test 1: textDocument/didOpen
        call run_document_test("Document did open", &
                               '{"jsonrpc":"2.0","method":"textDocument/didOpen",'// &
                               '"params":{'// &
                               '"textDocument":{"uri":"file:///test.f90",'// &
                               '"languageId":"fortran","version":1,'// &
                               '"text":"program test\\n'// &
                               '  implicit none\\nend program"}}}')

        ! Test 2: textDocument/didChange
        call run_document_test("Document did change", &
                               '{"jsonrpc":"2.0",'// &
                               '"method":"textDocument/didChange",'// &
                               '"params":{'// &
                               '"textDocument":{"uri":"file:///test.f90",'// &
                               '"version":2},'// &
                               '"contentChanges":[{"text":"program modified\\n'// &
                               '  implicit none\\n'// &
                               'end program"}]}}')

        ! Test 3: textDocument/didSave
        call run_document_test("Document did save", &
                               '{"jsonrpc":"2.0","method":"textDocument/didSave",'// &
                               '"params":{'// &
                               '"textDocument":{"uri":"file:///test.f90"},'// &
                               '"text":"saved content"}}')

        ! Test 4: textDocument/didClose
        call run_document_test("Document did close", &
                               '{"jsonrpc":"2.0","method":"textDocument/didClose",'// &
                               '"params":{"textDocument":{"uri":"file:///test.f90"}}}')

    end subroutine test_document_sync_messages

    subroutine test_diagnostic_messages()
        print *, ""
        print *, "Testing diagnostic publishing..."

        ! Test 1: Publish diagnostics
        call run_diagnostic_test("Publish diagnostics", &
                                 '{"jsonrpc":"2.0",'// &
                                 '"method":"textDocument/publishDiagnostics",'// &
                                 '"params":{'// &
                                 '"uri":"file:///test.f90","diagnostics":[{'// &
                                 '"range":{"start":{"line":0,"character":0},'// &
                                 '"end":{"line":0,"character":10}},'// &
                                 '"severity":1,"message":"Missing implicit '// &
                                 'none"}]}}')

        ! Test 2: Clear diagnostics
        call run_diagnostic_test("Clear diagnostics", &
                                 '{"jsonrpc":"2.0",'// &
                                 '"method":"textDocument/publishDiagnostics",'// &
                                 '"params":{"uri":"file:///test.f90",'// &
                                 '"diagnostics":[]}}')

        ! Test 3: Multiple diagnostics
        call run_diagnostic_test("Multiple diagnostics", &
                                 '{"jsonrpc":"2.0",'// &
                                 '"method":"textDocument/publishDiagnostics",'// &
                                 '"params":{'// &
                                 '"uri":"file:///test.f90","diagnostics":['// &
                                 '{"range":{"start":{"line":0,"character":0},'// &
                                 '"end":{"line":0,"character":10}},'// &
                                 '"severity":1,"message":"Error 1"},'// &
                                 '{"range":{"start":{"line":1,"character":0},'// &
                                 '"end":{"line":1,"character":5}},'// &
                                 '"severity":2,"message":"Warning '// &
                                 '1"}]}}')

    end subroutine test_diagnostic_messages

    subroutine test_capability_negotiation()
        print *, ""
        print *, "Testing LSP capability negotiation..."

        ! Test 1: Server capabilities response
        call run_capability_test("Server capabilities", &
                                 '{"textDocumentSync":2,"hoverProvider":true,'// &
                                 '"definitionProvider":true,'// &
                                 '"documentFormattingProvider":true,'// &
                                 '"diagnosticProvider":{'// &
                                 '"interFileDependencies":true}}')

        ! Test 2: Client capabilities processing
        call run_capability_test("Client capabilities", &
                                 '{"textDocument":{'// &
                                 '"synchronization":{"dynamicRegistration":true},'// &
                                 '"hover":{"dynamicRegistration":true},'// &
                                 '"formatting":{"dynamicRegistration":true}}}')

    end subroutine test_capability_negotiation

    subroutine test_error_handling()
        print *, ""
        print *, "Testing LSP error handling..."

        ! Test 1: Invalid JSON
        call run_error_test("Invalid JSON", "invalid json", "ParseError")

        ! Test 2: Missing required fields
        call run_error_test("Missing fields", '{"jsonrpc":"2.0"}', "InvalidRequest")

        ! Test 3: Invalid method
        call run_error_test("Invalid method", &
                            '{"jsonrpc":"2.0","id":1,'// &
                            '"method":"nonexistent","params":{}}', &
                            "MethodNotFound")

        ! Test 4: Invalid parameters
        call run_error_test("Invalid params", &
                            '{"jsonrpc":"2.0","id":1,'// &
                            '"method":"initialize","params":"invalid"}', &
                            "InvalidParams")

    end subroutine test_error_handling

    ! Helper subroutines for testing
    subroutine run_message_test(test_name, json_message, expected_type, expected_id, &
                                expected_method)
        character(len=*), intent(in) :: test_name, json_message, expected_type, &
                                        expected_method
        integer, intent(in) :: expected_id

        character(len=:), allocatable :: message_type, method
        integer :: message_id
        logical :: success

        total_tests = total_tests + 1

        call parse_lsp_message(json_message, message_type, message_id, method, success)

        if (.not. success) then
            print *, "  FAIL: ", test_name, " - Failed to parse message"
            print *, "        Message was: '", json_message, "'"
            return
        end if

        ! Validate parsed components
        if (message_type == expected_type .and. &
            (expected_id == -1 .or. message_id == expected_id) .and. &
            (len(expected_method) == 0 .or. method == expected_method)) then
            print *, "  PASS: ", test_name
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Expected type='", expected_type, &
                "', got='", message_type, "'"
            print *, "        Expected id=", expected_id, ", got=", message_id
            print *, "        Expected method='", expected_method, "', got='", &
                method, "'"
        end if

    end subroutine run_message_test

    subroutine run_initialize_test(test_name, json_message)
        character(len=*), intent(in) :: test_name, json_message

        logical :: success

        total_tests = total_tests + 1

        call process_initialize_request(json_message, success)

        if (success) then
            print *, "  PASS: ", test_name
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Initialize processing failed"
        end if

    end subroutine run_initialize_test

    subroutine run_document_test(test_name, json_message)
        character(len=*), intent(in) :: test_name, json_message

        logical :: success

        total_tests = total_tests + 1

        call process_document_sync(json_message, success)

        if (success) then
            print *, "  PASS: ", test_name
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Document sync processing failed"
        end if

    end subroutine run_document_test

    subroutine run_diagnostic_test(test_name, json_message)
        character(len=*), intent(in) :: test_name, json_message

        logical :: success

        total_tests = total_tests + 1

        call process_diagnostic_message(json_message, success)

        if (success) then
            print *, "  PASS: ", test_name
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Diagnostic processing failed"
        end if

    end subroutine run_diagnostic_test

    subroutine run_capability_test(test_name, json_capabilities)
        character(len=*), intent(in) :: test_name, json_capabilities

        logical :: success
        character(len=:), allocatable :: full_json

        total_tests = total_tests + 1

        full_json = '{"capabilities":'//json_capabilities//'}'

        call process_capabilities(full_json, success)

        if (success) then
            print *, "  PASS: ", test_name
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Capability processing failed"
        end if

    end subroutine run_capability_test

    subroutine run_error_test(test_name, json_message, expected_error_type)
        character(len=*), intent(in) :: test_name, json_message, expected_error_type

        character(len=:), allocatable :: error_type
        logical :: has_error

        total_tests = total_tests + 1

        call check_message_errors(json_message, error_type, has_error)

        if (has_error .and. error_type == expected_error_type) then
            print *, "  PASS: ", test_name, " - Correctly detected error: ", error_type
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Error handling failed"
        end if

    end subroutine run_error_test

end program test_lsp_message_handling
