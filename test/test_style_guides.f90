program test_style_guides
    use fluff_formatter, only: formatter_engine_t
    implicit none

    type(formatter_engine_t) :: formatter
    integer :: total_tests, passed_tests

    print *, "=== Standard Style Guides Test Suite ==="

    total_tests = 0
    passed_tests = 0

    ! Test different style guide configurations
    call test_default_clean_style()
    call test_standard_fortran_style()
    call test_modern_fortran_style()
    call test_hpc_scientific_style()
    call test_custom_organization_style()
    call test_style_guide_detection()
    call test_style_inheritance()

    print *, ""
    print *, "=== Style Guide Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"

    ! A tally that is only printed cannot fail the build, and a tally of
    ! zero means no assertion ran at all.
    if (total_tests == 0) error stop "style guides: no assertions ran"
    if (passed_tests /= total_tests) &
        error stop "style guides: some assertions failed"

contains

    subroutine test_default_clean_style()
        print *, ""
        print *, "Testing default Clean Code style guide..."

        call formatter%initialize()
        call formatter%set_style_guide("clean")

        ! Test 1: 4-space indentation (no tabs)
        call run_style_test("Clean: 4-space indentation", &
            "module test_mod" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "subroutine hello" // new_line('a') // &
            "print *, 'hello'" // new_line('a') // &
            "end subroutine" // new_line('a') // &
            "end module", &
            "clean")

        ! Test 2: 88-character line limit
        call run_style_test("Clean: 88-char line limit", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "result = very_long_function_name_that_exceeds_limit(arg1, arg2, arg3, arg4, arg5)" // new_line('a') // &
            "end program", &
            "clean")

        ! Test 3: use module, only: style
        call run_style_test("Clean: explicit imports", &
            "module test_mod" // new_line('a') // &
            "use iso_fortran_env" // new_line('a') // &
            "use other_module" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "end module", &
            "clean")

        ! Test 4: real(dp) numeric style
        call run_style_test("Clean: real64 precision", &
            "module test_mod" // new_line('a') // &
            "use iso_fortran_env, only: dp => real64" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "real :: x" // new_line('a') // &
            "end module", &
            "clean")

        ! Test 5: pure procedures when possible
        call run_style_test("Clean: pure procedures", &
            "module test_mod" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "function calculate(x, y) result(z)" // new_line('a') // &
            "real, intent(in) :: x, y" // new_line('a') // &
            "real :: z" // new_line('a') // &
            "z = x + y" // new_line('a') // &
            "end function" // new_line('a') // &
            "end module", &
            "clean")

        ! Test 6: module naming convention
        call run_style_test("Clean: module naming", &
            "module TestModule" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "end module TestModule", &
            "clean")

        ! Test 7: procedure spacing (1 blank line between procedures)
        call run_style_test("Clean: procedure spacing", &
            "module test_mod" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "subroutine first" // new_line('a') // &
            "end subroutine" // new_line('a') // &
            "subroutine second" // new_line('a') // &
            "end subroutine" // new_line('a') // &
            "end module", &
            "clean")

    end subroutine test_default_clean_style

    subroutine test_standard_fortran_style()
        print *, ""
        print *, "Testing standard Fortran style guide..."

        call formatter%initialize()
        call formatter%set_style_guide("standard")

        ! Test 1: Conservative formatting
        call run_style_test("Standard: conservative style", &
            "PROGRAM TEST" // new_line('a') // &
            "IMPLICIT NONE" // new_line('a') // &
            "INTEGER I" // new_line('a') // &
            "I=1" // new_line('a') // &
            "END PROGRAM", &
            "standard")

        ! Test 2: Traditional spacing
        call run_style_test("Standard: traditional spacing", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "if(x.gt.0)then" // new_line('a') // &
            "print*,'positive'" // new_line('a') // &
            "endif" // new_line('a') // &
            "end program", &
            "standard")

    end subroutine test_standard_fortran_style

    subroutine test_modern_fortran_style()
        print *, ""
        print *, "Testing modern Fortran style guide..."

        call formatter%initialize()
        call formatter%set_style_guide("modern")

        ! Test 1: Modern operators and syntax
        call run_style_test("Modern: operators and syntax", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "if (x > 0) then" // new_line('a') // &
            "print *, 'positive'" // new_line('a') // &
            "end if" // new_line('a') // &
            "end program", &
            "modern")

        ! Test 2: Array syntax preferences
        call run_style_test("Modern: array syntax", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: arr(5)" // new_line('a') // &
            "arr = [1, 2, 3, 4, 5]" // new_line('a') // &
            "end program", &
            "modern")

        ! Test 3: Intent declarations
        call run_style_test("Modern: intent declarations", &
            "module test_mod" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "subroutine process(input, output)" // new_line('a') // &
            "real :: input, output" // new_line('a') // &
            "output = input * 2" // new_line('a') // &
            "end subroutine" // new_line('a') // &
            "end module", &
            "modern")

    end subroutine test_modern_fortran_style

    subroutine test_hpc_scientific_style()
        print *, ""
        print *, "Testing HPC/scientific computing style guide..."

        call formatter%initialize()
        call formatter%set_style_guide("hpc")

        ! Test 1: Performance-oriented formatting
        call run_style_test("HPC: performance formatting", &
            "program test" // new_line('a') // &
            "use omp_lib" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: i, n" // new_line('a') // &
            "real(8) :: a(1000), b(1000)" // new_line('a') // &
            "!$omp parallel do" // new_line('a') // &
            "do i = 1, n" // new_line('a') // &
            "a(i) = b(i) * 2.0" // new_line('a') // &
            "end do" // new_line('a') // &
            "end program", &
            "hpc")

        ! Test 2: Array indexing style (1-based)
        call run_style_test("HPC: array indexing", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: matrix(0:100, 0:100)" // new_line('a') // &
            "matrix(1, 1) = 42" // new_line('a') // &
            "end program", &
            "hpc")

        ! Test 3: Precision specifications
        call run_style_test("HPC: precision specs", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "real*8 :: x" // new_line('a') // &
            "double precision :: y" // new_line('a') // &
            "end program", &
            "hpc")

    end subroutine test_hpc_scientific_style

    subroutine test_custom_organization_style()
        print *, ""
        print *, "Testing custom organization style guide..."

        call formatter%initialize()
        call formatter%set_style_guide("custom")

        ! Test 1: Custom indentation (2 spaces)
        call formatter%configure_style("indent_size", "2")
        call run_style_test("Custom: 2-space indent", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: i" // new_line('a') // &
            "end program", &
            "custom")

        ! Test 2: Custom line length (100 chars)
        call formatter%configure_style("line_length", "100")
        call run_style_test("Custom: 100-char lines", &
            "program test" // new_line('a') // &
            "result = function_with_very_long_name(arg1, arg2, arg3, arg4, arg5, arg6)" // new_line('a') // &
            "end program", &
            "custom")

        ! Test 3: Custom spacing preferences
        call formatter%configure_style("operator_spacing", "minimal")
        call run_style_test("Custom: minimal spacing", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "x = a + b * c" // new_line('a') // &
            "end program", &
            "custom")

    end subroutine test_custom_organization_style

    subroutine test_style_guide_detection()
        print *, ""
        print *, "Testing automatic style guide detection..."

        call formatter%initialize()

        ! A "fortran77" detection case stood here. fluff has no fortran77
        ! style guide, so no output of detect_style_guide could ever have
        ! satisfied it; it only ever asserted that some string came back.
        ! Test 2: Detect from modern syntax
        call run_detection_test("Detection: modern Fortran", &
            "program test" // new_line('a') // &
            "use iso_fortran_env, only: real64" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "class(my_type), intent(in) :: obj" // new_line('a') // &
            "end program", &
            "modern")

        ! Test 3: Detect from HPC patterns
        call run_detection_test("Detection: HPC style", &
            "program test" // new_line('a') // &
            "use mpi" // new_line('a') // &
            "use omp_lib" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "real(8) :: array(1000000)" // new_line('a') // &
            "!$omp parallel do" // new_line('a') // &
            "end program", &
            "hpc")

    end subroutine test_style_guide_detection

    subroutine test_style_inheritance()
        print *, ""
        print *, "Testing style guide inheritance and customization..."

        call formatter%initialize()

        ! Test 1: Inherit from clean style and customize
        call formatter%set_style_guide("clean")
        call formatter%configure_style("indent_size", "2")
        call formatter%configure_style("line_length", "100")

        call run_style_test("Inheritance: clean + custom", &
            "module test_mod" // new_line('a') // &
            "use iso_fortran_env, only: dp => real64" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "pure function calc(x) result(y)" // new_line('a') // &
            "real(dp), intent(in) :: x" // new_line('a') // &
            "real(dp) :: y" // new_line('a') // &
            "y = x * 2.0_dp" // new_line('a') // &
            "end function" // new_line('a') // &
            "end module", &
            "clean_custom")

        ! Test 2: Override specific rules
        call formatter%set_style_guide("standard")
        call formatter%configure_style("use_modern_operators", "true")
        call formatter%configure_style("case_style", "lower")

        call run_style_test("Inheritance: standard + modern", &
            "PROGRAM TEST" // new_line('a') // &
            "IF (X.GT.0) THEN" // new_line('a') // &
            "PRINT *, 'HELLO'" // new_line('a') // &
            "ENDIF" // new_line('a') // &
            "END PROGRAM", &
            "standard_modern")

    end subroutine test_style_inheritance

    ! Helper subroutines for testing
    ! Format `input` under the style guide named `style_name` and require the
    ! result to carry that style guide's indentation. The former oracle was
    ! len(formatted_code) > 0, which holds for any output whatsoever, so the
    ! style guide could have been ignored entirely without a case failing.
    subroutine run_style_test(test_name, input, style_name)
        character(len=*), intent(in) :: test_name, input, style_name
        character(len=:), allocatable :: formatted_code, error_msg
        integer :: smallest_indent

        total_tests = total_tests + 1

        call formatter%format_source(input, formatted_code, error_msg)

        if (error_msg /= "") then
            print *, "[FAIL] ", test_name, " - Error: ", error_msg
            return
        end if

        if (len_trim(formatted_code) == 0) then
            print *, "[FAIL] ", test_name, " - Empty output"
            return
        end if

        smallest_indent = smallest_positive_indent(formatted_code)
        if (smallest_indent /= formatter%options%indent_size) then
            print *, "[FAIL] ", test_name, " - ", style_name, &
                " style indents by ", smallest_indent, &
                " but its indent_size is ", formatter%options%indent_size
            return
        end if

        print *, "[OK] ", test_name, " (", style_name, " style)"
        passed_tests = passed_tests + 1

    end subroutine run_style_test

    ! Width of the shallowest non-zero indent in `code`, or 0 when every line
    ! starts in column 1.
    function smallest_positive_indent(code) result(width)
        character(len=*), intent(in) :: code
        integer :: width

        integer :: line_start, line_end, lead

        width = 0
        line_start = 1
        do while (line_start <= len(code))
            line_end = index(code(line_start:), new_line('a'))
            if (line_end == 0) then
                line_end = len(code)
            else
                line_end = line_start + line_end - 2
            end if
            if (line_end >= line_start) then
                if (len_trim(code(line_start:line_end)) > 0) then
                    lead = verify(code(line_start:line_end), " ") - 1
                    if (lead > 0) then
                        if (width == 0 .or. lead < width) width = lead
                    end if
                end if
            end if
            line_start = line_end + 2
        end do

    end function smallest_positive_indent

    ! Style-guide detection has to name the style the source is written in.
    ! The former oracle only required a non-empty string, so expected_style
    ! was an unused dummy argument and any answer counted as the right one.
    subroutine run_detection_test(test_name, input, expected_style)
        character(len=*), intent(in) :: test_name, input, expected_style
        character(len=:), allocatable :: detected_style

        total_tests = total_tests + 1

        call formatter%detect_style_guide(input, detected_style)

        if (detected_style /= expected_style) then
            print *, "[FAIL] ", test_name, " - expected ", expected_style, &
                " but detected ", detected_style
            return
        end if

        print *, "[OK] ", test_name, " - Detected: ", detected_style
        passed_tests = passed_tests + 1

    end subroutine run_detection_test

end program test_style_guides
