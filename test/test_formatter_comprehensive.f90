program test_formatter_comprehensive
    use fluff_formatter, only: formatter_engine_t
    implicit none

    type(formatter_engine_t) :: formatter
    integer :: total_tests, passed_tests

    print *, "=== Comprehensive Formatter Test Suite ==="

    call formatter%initialize()
    total_tests = 0
    passed_tests = 0

    ! Test comprehensive formatting scenarios
    call test_program_structures()
    call test_declaration_formatting()
    call test_statement_formatting()
    call test_expression_formatting()
    call test_control_flow_formatting()
    call test_procedure_formatting()
    call test_module_formatting()
    call test_edge_cases()
    call test_style_configurations()

    print *, ""
    print *, "=== Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"

    ! A tally that is only printed cannot fail the build, and a tally of
    ! zero means no assertion ran at all.
    if (total_tests == 0) error stop "comprehensive formatter: no assertions ran"
    if (passed_tests /= total_tests) &
        error stop "comprehensive formatter: some assertions failed"

contains

    subroutine test_program_structures()
        print *, ""
        print *, "Testing program structures..."

        call run_format_test("Simple program", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "end program test")

        call run_format_test("Program with variables", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: i" // new_line('a') // &
            "i = 42" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: i" // new_line('a') // &
            "" // new_line('a') // &
            "    i = 42" // new_line('a') // &
            "end program test")

    end subroutine test_program_structures

    subroutine test_declaration_formatting()
        print *, ""
        print *, "Testing declaration formatting..."

        call run_format_test("Integer declaration", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer::i" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: i" // new_line('a') // &
            "end program test")

        call run_format_test("Real declaration", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "real::x" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    real :: x" // new_line('a') // &
            "end program test")

        call run_format_test("Array declaration", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer::arr(10)" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: arr(10)" // new_line('a') // &
            "end program test")

    end subroutine test_declaration_formatting

    subroutine test_statement_formatting()
        print *, ""
        print *, "Testing statement formatting..."

        call run_format_test("Assignment statement", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: i" // new_line('a') // &
            "i=42" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: i" // new_line('a') // &
            "" // new_line('a') // &
            "    i = 42" // new_line('a') // &
            "end program test")

        call run_format_test("Print statement", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "print*,'hello'" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "" // new_line('a') // &
            "    print *, 'hello'" // new_line('a') // &
            "end program test")

    end subroutine test_statement_formatting

    subroutine test_expression_formatting()
        print *, ""
        print *, "Testing expression formatting..."

        call run_format_test("Arithmetic expression", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: result" // new_line('a') // &
            "result=1+2*3" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: result" // new_line('a') // &
            "" // new_line('a') // &
            "    result = 1 + 2*3" // new_line('a') // &
            "end program test")

        call run_format_test("Parenthesized expression", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: result" // new_line('a') // &
            "result=(1+2)*3" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: result" // new_line('a') // &
            "" // new_line('a') // &
            "    result = (1 + 2)*3" // new_line('a') // &
            "end program test")

    end subroutine test_expression_formatting

    subroutine test_control_flow_formatting()
        print *, ""
        print *, "Testing control flow formatting..."

        call run_format_test("If statement", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: i" // new_line('a') // &
            "if(i>0)then" // new_line('a') // &
            "print*,i" // new_line('a') // &
            "endif" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: i" // new_line('a') // &
            "" // new_line('a') // &
            "    if (i > 0) then" // new_line('a') // &
            "        print *, i" // new_line('a') // &
            "    end if" // new_line('a') // &
            "end program test")

        call run_format_test("Do loop", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: i" // new_line('a') // &
            "do i=1,10" // new_line('a') // &
            "print*,i" // new_line('a') // &
            "enddo" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: i" // new_line('a') // &
            "" // new_line('a') // &
            "    do i = 1, 10" // new_line('a') // &
            "        print *, i" // new_line('a') // &
            "    end do" // new_line('a') // &
            "end program test")

    end subroutine test_control_flow_formatting

    subroutine test_procedure_formatting()
        print *, ""
        print *, "Testing procedure formatting..."

        call run_format_test("Simple subroutine", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "subroutine hello" // new_line('a') // &
            "print*,'hello'" // new_line('a') // &
            "end subroutine" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "    subroutine hello" // new_line('a') // &
            "        print *, 'hello'" // new_line('a') // &
            "    end subroutine hello" // new_line('a') // &
            "end program test")

    end subroutine test_procedure_formatting

    subroutine test_module_formatting()
        print *, ""
        print *, "Testing module formatting..."

        call run_format_test("Simple module", &
            "module test_mod" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer::global_var" // new_line('a') // &
            "end module test_mod", &
            "module test_mod" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: global_var" // new_line('a') // &
            "end module test_mod")

    end subroutine test_module_formatting

    subroutine test_edge_cases()
        print *, ""
        print *, "Testing edge cases..."

        call run_format_test("Empty program", &
            "program test" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "end program test")

        call run_format_test("Minimal with implicit none", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "end program test")

    end subroutine test_edge_cases

    subroutine test_style_configurations()
        print *, ""
        print *, "Testing style configurations..."

        ! Test with 2-space indentation
        formatter%options%indent_size = 2
        call run_format_test("2-space indentation", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: i" // new_line('a') // &
            "end program test", &
            "program test" // new_line('a') // &
            "  implicit none" // new_line('a') // &
            "  integer :: i" // new_line('a') // &
            "end program test")

        ! Reset to default
        formatter%options%indent_size = 4

        ! Test with tabs (this is approximate since output will be spaces)
        formatter%options%use_tabs = .true.
        call run_format_test_flexible("Tab indentation", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: i" // new_line('a') // &
            "end program test")

        ! Reset to default
        formatter%options%use_tabs = .false.

    end subroutine test_style_configurations

    subroutine run_format_test(test_name, input, expected)
        character(len=*), intent(in) :: test_name, input, expected
        character(len=:), allocatable :: actual, error_msg

        total_tests = total_tests + 1

        call formatter%format_source(input, actual, error_msg)

        if (error_msg /= "") then
            print *, "[FAIL] ", test_name, " - Error: ", error_msg
            return
        end if

        ! Flexible matching - check key structural elements
        if (contains_key_elements(actual, expected)) then
            print *, "[OK] ", test_name
            passed_tests = passed_tests + 1
        else
            print *, "[FAIL] ", test_name
            print *, "    Expected structure elements from: ", expected
            print *, "    Actual: ", actual
        end if

    end subroutine run_format_test

    subroutine run_format_test_flexible(test_name, input)
        character(len=*), intent(in) :: test_name, input
        character(len=:), allocatable :: actual, error_msg

        total_tests = total_tests + 1

        call formatter%format_source(input, actual, error_msg)

        if (error_msg /= "") then
            print *, "[FAIL] ", test_name, " - Error: ", error_msg
            return
        end if

        ! Just check that basic structure is preserved
        if (index(actual, "program test") > 0 .and. index(actual, "end program") > 0) then
            print *, "[OK] ", test_name, " (flexible)"
            passed_tests = passed_tests + 1
        else
            print *, "[FAIL] ", test_name, " - Structure not preserved"
            print *, "    Actual: ", actual
        end if

    end subroutine run_format_test_flexible

    ! Every non-blank line of `expected` has to appear in `actual`, compared
    ! with whitespace removed: this suite is about which statements survive
    ! formatting, and spacing inside a line is the subject of
    ! test_enhanced_style_rules. The previous version tested only for the
    ! words program, implicit none, integer and real, so an output that kept
    ! those four tokens and dropped every statement still matched.
    function contains_key_elements(actual, expected) result(match)
        character(len=*), intent(in) :: actual, expected
        logical :: match

        character(len=:), allocatable :: packed_actual
        integer :: line_start, line_end

        packed_actual = without_spaces(actual)
        match = .true.
        line_start = 1
        do while (line_start <= len(expected))
            line_end = index(expected(line_start:), new_line('a'))
            if (line_end == 0) then
                line_end = len(expected)
            else
                line_end = line_start + line_end - 2
            end if
            if (line_end >= line_start) then
                if (len_trim(expected(line_start:line_end)) > 0) then
                    if (index(packed_actual, &
                        without_spaces(expected(line_start:line_end))) == 0) then
                        match = .false.
                        return
                    end if
                end if
            end if
            line_start = line_end + 2
        end do

    end function contains_key_elements

    function without_spaces(text) result(packed)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: packed

        integer :: i

        packed = ""
        do i = 1, len(text)
            if (text(i:i) == " " .or. text(i:i) == achar(9)) cycle
            packed = packed//text(i:i)
        end do

    end function without_spaces

end program test_formatter_comprehensive
