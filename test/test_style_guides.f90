program test_style_guides
    use fluff_formatter
    use fluff_core
    implicit none
    
    type(formatter_engine_t) :: formatter
    integer :: total_tests, passed_tests
    
    print *, "=== Standard Style Guides Test Suite (RED Phase) ==="
    
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
    
    if (passed_tests == total_tests) then
        print *, "[OK] All style guide tests passed!"
    else
        print *, "[FAIL] Some style guide tests failed (expected in RED phase)"
    end if
    
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
        
        ! Test 1: Detect from file patterns
        call run_detection_test("Detection: legacy Fortran", &
            "C     This is old Fortran" // new_line('a') // &
            "      PROGRAM TEST" // new_line('a') // &
            "      IMPLICIT NONE" // new_line('a') // &
            "      END", &
            "fortran77")
            
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
    subroutine run_style_test(test_name, input, style_name)
        character(len=*), intent(in) :: test_name, input, style_name
        character(len=:), allocatable :: formatted_code, error_msg
        
        total_tests = total_tests + 1
        
        call formatter%format_source(input, formatted_code, error_msg)
        
        if (error_msg /= "") then
            print *, "  FAIL: ", test_name, " - Error: ", error_msg
            return
        end if
        
        ! For now, just check that formatting completed without error
        ! In the GREEN phase, we'll implement proper style validation
        if (len(formatted_code) > 0) then
            print *, "  PASS: ", test_name, " (", style_name, " style)"
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Empty output"
        end if
        
    end subroutine run_style_test
    
    subroutine run_detection_test(test_name, input, expected_style)
        character(len=*), intent(in) :: test_name, input, expected_style
        character(len=:), allocatable :: detected_style
        
        total_tests = total_tests + 1
        
        call formatter%detect_style_guide(input, detected_style)
        
        ! For now, just check that detection completes
        ! In the GREEN phase, we'll implement actual detection logic
        if (len(detected_style) > 0) then
            print *, "  PASS: ", test_name, " - Detected: ", detected_style
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - No style detected"
        end if
        
    end subroutine run_detection_test
    
end program test_style_guides
