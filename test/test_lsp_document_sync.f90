program test_lsp_document_sync
    use fluff_json, only: json_array_length, json_get_member_json, &
                          json_get_string_member, &
                          json_parse
    use fluff_lsp_server, only: fluff_lsp_server_t
    use fluff_lsp_uri, only: uri_to_path
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer :: total_tests, passed_tests

    print *, "=== LSP Document Synchronization Test Suite ==="

    total_tests = 0
    passed_tests = 0

    call test_document_lifecycle()
    call test_workspace_multiple_documents()
    call test_uri_parsing()

    print *, ""
    print *, "=== LSP Document Sync Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests, dp)/real(total_tests, dp)*100.0_dp, &
        "%"

    if (passed_tests /= total_tests) error stop 1
    stop 0

contains

    subroutine test_document_lifecycle()
        type(fluff_lsp_server_t) :: server
        character(len=:), allocatable :: notification
        logical :: success, found

        call server%initialize("/project/root")

        total_tests = total_tests + 1
        call server%handle_text_document_did_open("file:///test.f90", "fortran", 1, &
                                                  "program test"//new_line('a')// &
                                                  "  implicit none"//new_line('a')// &
                                                  "  integer :: x"//new_line('a')// &
                                                  "  x = 1"//new_line('a')// &
                                                  "end program", success)

        if (.not. success) then
            print *, "  FAIL: Document open"
            return
        end if

        if (server%workspace%document_count /= 1) then
            print *, "  FAIL: Document open - unexpected document_count"
            return
        end if

        if (server%workspace%documents(1)%uri /= "file:///test.f90") then
            print *, "  FAIL: Document open - unexpected uri"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "  PASS: Document open"

        call server%pop_notification(notification, found)
        if (found) then
            call assert_publish_diagnostics(notification, "file:///test.f90", success)
            total_tests = total_tests + 1
            if (success) then
                passed_tests = passed_tests + 1
                print *, "  PASS: Diagnostics notification after open"
            else
                print *, "  FAIL: Diagnostics notification after open"
                return
            end if
        end if

        total_tests = total_tests + 1
        call server%handle_text_document_did_change("file:///test.f90", 2, &
                                                    "program test"//new_line('a')// &
                                                    "  implicit none"//new_line('a')// &
                                                    "  integer :: x"//new_line('a')// &
                                                    "  x = 2"//new_line('a')// &
                                                    "end program", success)

        if (.not. success) then
            print *, "  FAIL: Document change"
            return
        end if

        if (server%workspace%documents(1)%version /= 2) then
            print *, "  FAIL: Document change - version not updated"
            return
        end if

        if (index(server%workspace%documents(1)%content, "x = 2") == 0) then
            print *, "  FAIL: Document change - content not updated"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "  PASS: Document change"

        total_tests = total_tests + 1
        call server%handle_text_document_did_save("file:///test.f90", success)
        if (success) then
            passed_tests = passed_tests + 1
            print *, "  PASS: Document save"
        else
            print *, "  FAIL: Document save"
            return
        end if

        total_tests = total_tests + 1
        call server%handle_text_document_did_close("file:///test.f90", success)
        if (.not. success) then
            print *, "  FAIL: Document close"
            return
        end if

        if (server%workspace%document_count /= 0) then
            print *, "  FAIL: Document close - document_count not decremented"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "  PASS: Document close"
    end subroutine test_document_lifecycle

    subroutine test_workspace_multiple_documents()
        type(fluff_lsp_server_t) :: server
        logical :: success

        call server%initialize("/project/root")

        total_tests = total_tests + 1
        call server%handle_text_document_did_open("file:///a.f90", "fortran", 1, &
                                                  "program a"//new_line('a')// &
                                                  "  implicit none"//new_line('a')// &
                                                  "end program", success)

        if (.not. success) then
            print *, "  FAIL: Open first document"
            return
        end if

        call server%handle_text_document_did_open("file:///b.f90", "fortran", 1, &
                                                  "program b"//new_line('a')// &
                                                  "  implicit none"//new_line('a')// &
                                                  "end program", success)

        if (.not. success) then
            print *, "  FAIL: Open second document"
            return
        end if

        if (server%workspace%document_count /= 2) then
            print *, "  FAIL: Multiple documents - unexpected document_count"
            return
        end if

        call server%handle_text_document_did_close("file:///a.f90", success)
        if (.not. success) then
            print *, "  FAIL: Close first document"
            return
        end if

        if (server%workspace%document_count /= 1) then
            print *, "  FAIL: Multiple documents - close did not remove"
            return
        end if

        passed_tests = passed_tests + 1
        print *, "  PASS: Multiple documents open/close"
    end subroutine test_workspace_multiple_documents

    subroutine test_uri_parsing()
        character(len=:), allocatable :: path
        logical :: success

        total_tests = total_tests + 1
        call uri_to_path("file:///home/user/project/main.f90", path, success)
        if (success .and. path == "/home/user/project/main.f90") then
            passed_tests = passed_tests + 1
            print *, "  PASS: URI parsing"
        else
            print *, "  FAIL: URI parsing"
            return
        end if

        total_tests = total_tests + 1
        call uri_to_path("file:///C:/Users/user/project/main.f90", path, success)
        if (success .and. path == "C:/Users/user/project/main.f90") then
            passed_tests = passed_tests + 1
            print *, "  PASS: Windows URI parsing"
        else
            print *, "  FAIL: Windows URI parsing"
            return
        end if

        total_tests = total_tests + 1
        call uri_to_path("file:///../relative/path.f90", path, success)
        if (success .and. path == "../relative/path.f90") then
            passed_tests = passed_tests + 1
            print *, "  PASS: Relative path URI parsing"
        else
            print *, "  FAIL: Relative path URI parsing"
            return
        end if

        total_tests = total_tests + 1
        call uri_to_path("file:///path%20with%20spaces/file.f90", path, success)
        if (success .and. path == "/path with spaces/file.f90") then
            passed_tests = passed_tests + 1
            print *, "  PASS: Percent decoding"
        else
            print *, "  FAIL: Percent decoding"
            return
        end if
    end subroutine test_uri_parsing

    subroutine assert_publish_diagnostics(notification, expected_uri, success)
        character(len=*), intent(in) :: notification
        character(len=*), intent(in) :: expected_uri
        logical, intent(out) :: success

        character(len=:), allocatable :: err
        character(len=:), allocatable :: params_json, method, uri, diagnostics_json
        logical :: ok, found
        integer :: n

        success = .false.
        call json_parse(notification, ok, err)
        if (.not. ok) return

        call json_get_string_member(notification, "method", method, found, ok)
        if (.not. ok .or. .not. found) return
        if (method /= "textDocument/publishDiagnostics") return

        call json_get_member_json(notification, "params", params_json, found, ok)
        if (.not. ok .or. .not. found) return

        call json_get_string_member(params_json, "uri", uri, found, ok)
        if (.not. ok .or. .not. found) return
        if (uri /= expected_uri) return

        call json_get_member_json(params_json, "diagnostics", &
                                  diagnostics_json, found, ok)
        if (.not. ok .or. .not. found) return

        call json_array_length(diagnostics_json, n, ok)
        if (.not. ok) return
        if (n < 0) return

        success = .true.
    end subroutine assert_publish_diagnostics

end program test_lsp_document_sync
