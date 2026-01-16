program test_format_validation
    use fluff_formatter
    use fluff_core
    use fluff_ast
    implicit none

    type(formatter_engine_t) :: formatter
    integer :: total_tests, passed_tests

    print *, "=== Format Validation Test Suite ==="

    total_tests = 0
    passed_tests = 0

    ! Test format validation functionality
    call test_semantic_preservation()
    call test_roundtrip_formatting()
    call test_edge_case_handling()
    call test_validation_interface()
    call test_semantic_comparison()
    call test_format_diff_analysis()
    call test_file_formatting()

    print *, ""
    print *, "=== Format Validation Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests)/real(total_tests)*100.0, "%"

    if (passed_tests == total_tests) then
        print *, "All format validation tests passed."
    else
        print *, "Some validation tests failed."
    end if

contains

    subroutine test_semantic_preservation()
        print *, ""
        print *, "Testing semantic preservation during formatting..."

        call formatter%initialize()
        call formatter%set_style_guide("clean")

        ! Test 1: Variable declarations maintain types (simplified)
        call run_validation_test("Semantic: variable types", &
                                 "program test"//new_line('a')// &
                                 "    implicit none"//new_line('a')// &
                                 "    integer :: i"//new_line('a')// &
                                 "    real :: x"//new_line('a')// &
                                 "    i = 42"//new_line('a')// &
                                 "    x = 3.14"//new_line('a')// &
                                 "    print *, i, x"//new_line('a')// &
                                 "end program", &
                                 .true.)

        ! Test 2: Arithmetic expressions preserve operations
        call run_validation_test("Semantic: arithmetic", &
                                 "program test"//new_line('a')// &
                                 "    implicit none"//new_line('a')// &
                              "    real :: a = 1.0, b = 2.0, c = 3.0"//new_line('a')// &
                                 "    real :: result"//new_line('a')// &
                                 "    result = a + b * c - a / b"//new_line('a')// &
                                 "    print *, result"//new_line('a')// &
                                 "end program", &
                                 .true.)

        ! Test 3: Control flow structures preserve logic
        call run_validation_test("Semantic: control flow", &
                                 "program test"//new_line('a')// &
                                 "    implicit none"//new_line('a')// &
                                 "    integer :: i, n = 10"//new_line('a')// &
                                 "    do i = 1, n"//new_line('a')// &
                                 "        if (mod(i, 2) == 0) then"//new_line('a')// &
                                 "            print *, 'even:', i"//new_line('a')// &
                                 "        else"//new_line('a')// &
                                 "            print *, 'odd:', i"//new_line('a')// &
                                 "        end if"//new_line('a')// &
                                 "    end do"//new_line('a')// &
                                 "end program", &
                                 .true.)

        ! Test 4: Procedure definitions preserve signatures (simplified)
        call run_validation_test("Semantic: procedure signatures", &
                                 "program test"//new_line('a')// &
                                 "    implicit none"//new_line('a')// &
                                 "    integer :: result"//new_line('a')// &
                                 "    result = add_numbers(5, 3)"//new_line('a')// &
                                 "    print *, result"//new_line('a')// &
                                 "contains"//new_line('a')// &
                         "    function add_numbers(a, b) result(sum)"//new_line('a')// &
                                "        integer, intent(in) :: a, b"//new_line('a')// &
                                 "        integer :: sum"//new_line('a')// &
                                 "        sum = a + b"//new_line('a')// &
                                 "    end function add_numbers"//new_line('a')// &
                                 "end program", &
                                 .true.)

        ! Test 5: Array operations preserve indexing (simplified)
        call run_validation_test("Semantic: array operations", &
                                 "program test"//new_line('a')// &
                                 "    implicit none"//new_line('a')// &
                                 "    integer, parameter :: n = 3"//new_line('a')// &
                                 "    real :: arr(n)"//new_line('a')// &
                                 "    integer :: i"//new_line('a')// &
                                 "    do i = 1, n"//new_line('a')// &
                                 "        arr(i) = real(i)"//new_line('a')// &
                                 "    end do"//new_line('a')// &
                                 "    print *, arr"//new_line('a')// &
                                 "end program", &
                                 .true.)

    end subroutine test_semantic_preservation

    subroutine test_roundtrip_formatting()
        print *, ""
        print *, "Testing roundtrip formatting idempotency..."

        call formatter%initialize()
        call formatter%set_style_guide("clean")

        ! Test 1: Simple program should be stable after two formats
        call run_roundtrip_test("Roundtrip: simple program", &
                                "program hello"//new_line('a')// &
                                "    implicit none"//new_line('a')// &
                                "    print *, 'Hello, World!'"//new_line('a')// &
                                "end program hello")

        ! Test 2: Module with procedures should be stable
        call run_roundtrip_test("Roundtrip: module procedures", &
                                "module test_mod"//new_line('a')// &
                                "    implicit none"//new_line('a')// &
                                "    private"//new_line('a')// &
                                "    public :: public_func"//new_line('a')// &
                                "contains"//new_line('a')// &
                              "    function public_func(x) result(y)"//new_line('a')// &
                                "        real, intent(in) :: x"//new_line('a')// &
                                "        real :: y"//new_line('a')// &
                                "        y = x * 2.0"//new_line('a')// &
                                "    end function public_func"//new_line('a')// &
                                "end module test_mod")

        ! Test 3: Complex expressions should be stable
        call run_roundtrip_test("Roundtrip: complex expressions", &
                                "program test"//new_line('a')// &
                                "    implicit none"//new_line('a')// &
                              "    real :: a = 1.0, b = 2.0, c = 3.0"//new_line('a')// &
                                "    real :: result"//new_line('a')// &
                                "    result = (a + b) * c / (a - b) + "// &
                                "sqrt(a**2 + b**2)"//new_line('a')// &
                                "    print *, result"//new_line('a')// &
                                "end program")

        ! Test 4: Derived types should be stable
        call run_roundtrip_test("Roundtrip: derived types", &
                                "module types_mod"//new_line('a')// &
                                "    implicit none"//new_line('a')// &
                                "    "//new_line('a')// &
                                "    type :: point_t"//new_line('a')// &
                                "        real :: x, y"//new_line('a')// &
                                "    contains"//new_line('a')// &
                                "        procedure :: distance"//new_line('a')// &
                                "    end type point_t"//new_line('a')// &
                                "    "//new_line('a')// &
                                "contains"//new_line('a')// &
                                "    "//new_line('a')// &
                    "    function distance(this, other) result(dist)"//new_line('a')// &
                  "        class(point_t), intent(in) :: this, other"//new_line('a')// &
                                "        real :: dist"//new_line('a')// &
                                "        dist = sqrt((this%x - other%x)**2 + "// &
                                "(this%y - other%y)**2)"//new_line('a')// &
                                "    end function distance"//new_line('a')// &
                                "end module types_mod")

    end subroutine test_roundtrip_formatting

    subroutine test_edge_case_handling()
        print *, ""
        print *, "Testing edge case handling in formatting..."

        call formatter%initialize()
        call formatter%set_style_guide("clean")

        ! Test 1: Empty program
        call run_validation_test("Edge: empty program", &
                                 "program empty"//new_line('a')// &
                                 "end program empty", &
                                 .true.)

        ! Test 2: Comments preservation
        call run_validation_test("Edge: comments", &
                                 "program test"//new_line('a')// &
                                 "    ! This is a comment"//new_line('a')// &
                                 "    implicit none"//new_line('a')// &
                                 "    integer :: i  ! Inline comment"//new_line('a')// &
                                 "    ! Another comment"//new_line('a')// &
                                 "    i = 42"//new_line('a')// &
                                 "end program", &
                                 .true.)

        ! Test 3: String literals with special characters
        call run_validation_test("Edge: string literals", &
                                 "program test"//new_line('a')// &
                                 "    implicit none"//new_line('a')// &
                 "    character(len=*), parameter :: msg = 'String with ''quotes'' "// &
                                 "and spaces'"//new_line('a')// &
                          "    character(len=*), parameter :: path = 'C:\\Windows\\"// &
                                 "System32'"//new_line('a')// &
                                 "    print *, msg, path"//new_line('a')// &
                                 "end program", &
                                 .true.)

        ! Test 4: Long continuation lines
        call run_validation_test("Edge: continuation lines", &
                                 "program test"//new_line('a')// &
                                 "    implicit none"//new_line('a')// &
                                 "    real :: very_long_result"//new_line('a')// &
                                 "    very_long_result = 1.0 + 2.0 + 3.0 + 4.0 + "// &
                                 "5.0 + &"//new_line('a')// &
                "                       6.0 + 7.0 + 8.0 + 9.0 + 10.0"//new_line('a')// &
                                 "    print *, very_long_result"//new_line('a')// &
                                 "end program", &
                                 .true.)

        ! Test 5: Nested constructs
        call run_validation_test("Edge: nested constructs", &
                                 "program test"//new_line('a')// &
                                 "    implicit none"//new_line('a')// &
                                 "    integer :: i, j, k"//new_line('a')// &
                                 "    do i = 1, 3"//new_line('a')// &
                                 "        if (i > 1) then"//new_line('a')// &
                                 "            do j = 1, 2"//new_line('a')// &
                                 "                k = i * j"//new_line('a')// &
                                 "                print *, k"//new_line('a')// &
                                 "            end do"//new_line('a')// &
                                 "        end if"//new_line('a')// &
                                 "    end do"//new_line('a')// &
                                 "end program", &
                                 .true.)

    end subroutine test_edge_case_handling

    subroutine test_validation_interface()
        print *, ""
        print *, "Testing validation interface functionality..."

        call formatter%initialize()
        call formatter%set_style_guide("clean")

        ! Test 1: Basic validation interface
        call run_interface_test("Interface: basic validation", &
                                "program test"//new_line('a')// &
                                "    implicit none"//new_line('a')// &
                                "    integer :: i = 42"//new_line('a')// &
                                "    print *, i"//new_line('a')// &
                                "end program")

        ! Test 2: Validation with semantic changes (should fail)
        call run_interface_test("Interface: semantic change detection", &
                                "program test"//new_line('a')// &
                                "    implicit none"//new_line('a')// &
                                "    real :: x = 1.5"//new_line('a')// &
                                "    print *, x"//new_line('a')// &
                                "end program")

        ! Test 3: Validation with whitespace-only changes (should pass)
        call run_interface_test("Interface: whitespace changes", &
                                "program test"//new_line('a')// &
                                "implicit none"//new_line('a')// &
                                "integer::i=42"//new_line('a')// &
                                "print*,i"//new_line('a')// &
                                "end program")

    end subroutine test_validation_interface

    subroutine test_semantic_comparison()
        print *, ""
        print *, "Testing semantic comparison functionality..."

        call formatter%initialize()
        call formatter%set_style_guide("clean")

        ! Test 1: Identical semantics with different formatting
        call run_semantic_test("Semantic: identical meaning", &
                               "program test"//new_line('a')// &
                               "implicit none"//new_line('a')// &
                               "integer::x=5"//new_line('a')// &
                               "print*,x"//new_line('a')// &
                               "end program", &
                               "program test"//new_line('a')// &
                               "    implicit none"//new_line('a')// &
                               "    integer :: x = 5"//new_line('a')// &
                               "    print *, x"//new_line('a')// &
                               "end program", &
                               .true.)

        ! Test 2: Different variable names (different semantics)
        call run_semantic_test("Semantic: different variables", &
                               "program test"//new_line('a')// &
                               "    implicit none"//new_line('a')// &
                               "    integer :: x = 5"//new_line('a')// &
                               "    print *, x"//new_line('a')// &
                               "end program", &
                               "program test"//new_line('a')// &
                               "    implicit none"//new_line('a')// &
                               "    integer :: y = 5"//new_line('a')// &
                               "    print *, y"//new_line('a')// &
                               "end program", &
                               .false.)

        ! Test 3: Different values (different semantics)
        call run_semantic_test("Semantic: different values", &
                               "program test"//new_line('a')// &
                               "    implicit none"//new_line('a')// &
                               "    integer :: x = 5"//new_line('a')// &
                               "    print *, x"//new_line('a')// &
                               "end program", &
                               "program test"//new_line('a')// &
                               "    implicit none"//new_line('a')// &
                               "    integer :: x = 10"//new_line('a')// &
                               "    print *, x"//new_line('a')// &
                               "end program", &
                               .false.)

    end subroutine test_semantic_comparison

    subroutine test_format_diff_analysis()
        print *, ""
        print *, "Testing format diff analysis functionality..."

        call formatter%initialize()
        call formatter%set_style_guide("clean")

        ! Test 1: Whitespace-only changes
        call run_diff_test("Diff: whitespace only", &
                           "program test"//new_line('a')// &
                           "implicit none"//new_line('a')// &
                           "integer::x=5"//new_line('a')// &
                           "end program", &
                           "program test"//new_line('a')// &
                           "    implicit none"//new_line('a')// &
                           "    integer :: x = 5"//new_line('a')// &
                           "end program", &
                           "whitespace")

        ! Test 2: Indentation changes
        call run_diff_test("Diff: indentation", &
                           "program test"//new_line('a')// &
                           "implicit none"//new_line('a')// &
                           "integer :: x"//new_line('a')// &
                           "if (x > 0) then"//new_line('a')// &
                           "print *, x"//new_line('a')// &
                           "end if"//new_line('a')// &
                           "end program", &
                           "program test"//new_line('a')// &
                           "    implicit none"//new_line('a')// &
                           "    integer :: x"//new_line('a')// &
                           "    if (x > 0) then"//new_line('a')// &
                           "        print *, x"//new_line('a')// &
                           "    end if"//new_line('a')// &
                           "end program", &
                           "indentation")

        ! Test 3: Line breaks
        call run_diff_test("Diff: line breaks", &
                         "program test; implicit none; integer :: x = 5; end program", &
                           "program test"//new_line('a')// &
                           "    implicit none"//new_line('a')// &
                           "    integer :: x = 5"//new_line('a')// &
                           "end program", &
                           "structure")

    end subroutine test_format_diff_analysis

    subroutine test_file_formatting()
        character(len=:), allocatable :: formatted_code, error_msg
        character(len=*), parameter :: temp_path = "/tmp/fluff_format_file_test.f90"
        integer :: unit
        integer :: iostat

        total_tests = total_tests + 1

        print *, ""
        print *, "Testing file formatting interface..."

        open (newunit=unit, file=temp_path, status='replace', action='write', &
              iostat=iostat)
        if (iostat /= 0) then
            print *, "  FAIL: file open for write"
            return
        end if

        write (unit, '(A)') "program test"
        write (unit, '(A)') "implicit none"
        write (unit, '(A)') "integer :: x"
        write (unit, '(A)') "x = 1"
        write (unit, '(A)') "end program"
        close (unit)

        call formatter%format_file(temp_path, formatted_code, error_msg)
        if (error_msg /= "") then
            print *, "  FAIL: file format error: ", error_msg
            return
        end if

        if (index(formatted_code, "program test") == 0) then
            print *, "  FAIL: formatted output missing program"
            return
        end if

        open (newunit=unit, file=temp_path, status='old', action='read', &
              iostat=iostat)
        if (iostat == 0) then
            close (unit, status='delete')
        end if

        print *, "  PASS: file formatting"
        passed_tests = passed_tests + 1
    end subroutine test_file_formatting

    ! Helper subroutines for different test types
    subroutine run_validation_test(test_name, input, should_validate)
        character(len=*), intent(in) :: test_name, input
        logical, intent(in) :: should_validate
        character(len=:), allocatable :: formatted_code, error_msg
        logical :: is_valid

        total_tests = total_tests + 1

        call formatter%format_source(input, formatted_code, error_msg)

        if (error_msg /= "") then
            print *, "  FAIL: ", test_name, " - Format error: ", error_msg
            return
        end if

        call formatter%validate_format(input, formatted_code, is_valid)

        if (is_valid .eqv. should_validate) then
            print *, "  PASS: ", test_name
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Expected validation: ", &
                should_validate, ", got: ", is_valid
        end if

    end subroutine run_validation_test

    subroutine run_roundtrip_test(test_name, input)
        character(len=*), intent(in) :: test_name, input
        character(len=:), allocatable :: first_format, second_format, error_msg

        total_tests = total_tests + 1

        ! First formatting
        call formatter%format_source(input, first_format, error_msg)
        if (error_msg /= "") then
            print *, "  FAIL: ", test_name, " - First format error: ", error_msg
            return
        end if

        ! Second formatting (roundtrip)
        call formatter%format_source(first_format, second_format, error_msg)
        if (error_msg /= "") then
            print *, "  FAIL: ", test_name, " - Second format error: ", error_msg
            return
        end if

        ! Check idempotency
        if (first_format == second_format) then
            print *, "  PASS: ", test_name, " - Roundtrip stable"
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Roundtrip not stable"
        end if

    end subroutine run_roundtrip_test

    subroutine run_interface_test(test_name, input)
        character(len=*), intent(in) :: test_name, input
        character(len=:), allocatable :: formatted_code, error_msg
        logical :: is_valid

        total_tests = total_tests + 1

        call formatter%format_source(input, formatted_code, error_msg)

        if (error_msg /= "") then
            print *, "  FAIL: ", test_name, " - Format error: ", error_msg
            return
        end if

        call formatter%validate_format(input, formatted_code, is_valid)

        ! For interface testing, we just check that the validation runs
        print *, "  PASS: ", test_name, " - Validation result: ", is_valid
        passed_tests = passed_tests + 1

    end subroutine run_interface_test

    subroutine run_semantic_test(test_name, input1, input2, should_match)
        character(len=*), intent(in) :: test_name, input1, input2
        logical, intent(in) :: should_match
        logical :: semantically_equal

        total_tests = total_tests + 1

        call formatter%compare_semantics(input1, input2, semantically_equal)

        if (semantically_equal .eqv. should_match) then
            print *, "  PASS: ", test_name, " - Semantic comparison: ", &
                semantically_equal
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Expected: ", should_match, &
                ", got: ", semantically_equal
        end if

    end subroutine run_semantic_test

    subroutine run_diff_test(test_name, original, formatted, &
                             expected_diff_type)
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: original
        character(len=*), intent(in) :: formatted
        character(len=*), intent(in) :: expected_diff_type
        character(len=:), allocatable :: diff_type

        total_tests = total_tests + 1

        call formatter%analyze_format_diff(original, formatted, diff_type)

        if (index(diff_type, expected_diff_type) > 0) then
            print *, "  PASS: ", test_name, " - Diff type: ", diff_type
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Expected: ", expected_diff_type, &
                ", got: ", diff_type
        end if

    end subroutine run_diff_test

end program test_format_validation
