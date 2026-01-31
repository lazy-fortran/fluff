program test_lsp_framing
    use fluff_lsp_framing, only: lsp_parse_content_length
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer :: total_tests, passed_tests

    print *, "=== LSP Framing Test Suite ==="

    total_tests = 0
    passed_tests = 0

    call test_parse_content_length()
    call test_content_length_edge_cases()

    print *, ""
    print *, "=== LSP Framing Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests, dp)/real(total_tests, dp)* &
        100.0_dp, "%"

    if (passed_tests /= total_tests) error stop 1
    stop 0

contains

    subroutine test_parse_content_length()
        print *, ""
        print *, "Testing Content-Length header parsing..."

        ! Test 1: Valid Content-Length header
        call run_content_length_test("Valid Content-Length", &
                                     "Content-Length: 123", 123, .true.)

        ! Test 2: Content-Length with leading whitespace in value
        call run_content_length_test("Content-Length with whitespace", &
                                     "Content-Length:   456", 456, .true.)

        ! Test 3: Zero Content-Length
        call run_content_length_test("Zero Content-Length", &
                                     "Content-Length: 0", 0, .true.)

        ! Test 4: Large Content-Length
        call run_content_length_test("Large Content-Length", &
                                     "Content-Length: 9999999", 9999999, .true.)

    end subroutine test_parse_content_length

    subroutine test_content_length_edge_cases()
        print *, ""
        print *, "Testing Content-Length edge cases..."

        ! Test 1: Missing value
        call run_content_length_test("Missing value", &
                                     "Content-Length: ", -1, .false.)

        ! Test 2: Invalid value (non-numeric)
        call run_content_length_test("Non-numeric value", &
                                     "Content-Length: abc", -1, .false.)

        ! Test 3: Negative value
        call run_content_length_test("Negative value", &
                                     "Content-Length: -1", -1, .false.)

        ! Test 4: Empty header
        call run_content_length_test("Empty header", "", -1, .false.)

        ! Test 5: Just prefix
        call run_content_length_test("Just prefix", "Content-Length:", -1, .false.)

    end subroutine test_content_length_edge_cases

    subroutine run_content_length_test(test_name, header, expected_length, &
                                       expected_success)
        character(len=*), intent(in) :: test_name, header
        integer, intent(in) :: expected_length
        logical, intent(in) :: expected_success

        integer :: content_length
        logical :: success

        total_tests = total_tests + 1

        call lsp_parse_content_length(header, content_length, success)

        if (success .eqv. expected_success) then
            if (success) then
                if (content_length == expected_length) then
                    print *, "[OK] ", test_name
                    passed_tests = passed_tests + 1
                else
                    print *, "[FAIL] ", test_name, " - Expected length=", &
                        expected_length, ", got=", content_length
                end if
            else
                print *, "[OK] ", test_name
                passed_tests = passed_tests + 1
            end if
        else
            print *, "[FAIL] ", test_name, " - Expected success=", expected_success, &
                ", got=", success
        end if

    end subroutine run_content_length_test

end program test_lsp_framing
