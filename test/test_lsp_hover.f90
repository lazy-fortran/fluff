program test_lsp_hover
    use fluff_ast
    use fluff_lsp_hover
    implicit none
    
    integer :: total_tests, passed_tests
    
    print *, "=== LSP Hover Provider Test Suite (RED Phase) ==="
    
    total_tests = 0
    passed_tests = 0
    
    ! Test hover functionality
    call test_variable_hover()
    call test_procedure_hover()
    call test_type_hover()
    call test_module_hover()
    call test_intrinsic_hover()
    call test_hover_formatting()
    
    print *, ""
    print *, "=== LSP Hover Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"
    
    if (passed_tests == total_tests) then
        print *, "✅ All LSP hover tests passed!"
    else
        print *, "❌ Some tests failed (expected in RED phase)"
    end if
    
contains
    
    subroutine test_variable_hover()
        print *, ""
        print *, "Testing hover over variables..."
        
        ! Test 1: Hover over integer variable
        call run_hover_test("Integer variable hover", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: x = 42" // new_line('a') // &
            "end program", &
            3, 11, "integer :: x", .true.)
            
        ! Test 2: Hover over array variable
        call run_hover_test("Array variable hover", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "real :: matrix(10, 20)" // new_line('a') // &
            "end program", &
            3, 8, "real :: matrix(10, 20)", .true.)
            
        ! Test 3: Hover over derived type variable
        call run_hover_test("Derived type hover", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "type(my_type) :: obj" // new_line('a') // &
            "end program", &
            3, 17, "type(my_type) :: obj", .true.)
            
        ! Test 4: Hover over parameter
        call run_hover_test("Parameter hover", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "real, parameter :: pi = 3.14159" // new_line('a') // &
            "end program", &
            3, 19, "real, parameter :: pi = 3.14159", .true.)
            
        ! Test 5: No hover info available
        call run_hover_test("No hover info", &
            "program test" // new_line('a') // &
            "end program", &
            1, 5, "", .false.)
            
    end subroutine test_variable_hover
    
    subroutine test_procedure_hover()
        print *, ""
        print *, "Testing hover over procedures..."
        
        ! Test 1: Hover over subroutine
        call run_hover_test("Subroutine hover", &
            "subroutine calculate(x, y, result)" // new_line('a') // &
            "real, intent(in) :: x, y" // new_line('a') // &
            "real, intent(out) :: result" // new_line('a') // &
            "end subroutine", &
            1, 11, "subroutine calculate(x, y, result)", .true.)
            
        ! Test 2: Hover over function
        call run_hover_test("Function hover", &
            "function add(a, b) result(sum)" // new_line('a') // &
            "real :: a, b, sum" // new_line('a') // &
            "end function", &
            1, 9, "function add(a, b) result(sum)", .true.)
            
        ! Test 3: Hover over interface
        call run_hover_test("Interface hover", &
            "interface operator(+)" // new_line('a') // &
            "module procedure add_custom" // new_line('a') // &
            "end interface", &
            1, 10, "interface operator(+)", .true.)
            
        ! Test 4: Hover over generic procedure
        call run_hover_test("Generic procedure hover", &
            "interface swap" // new_line('a') // &
            "module procedure swap_int, swap_real" // new_line('a') // &
            "end interface", &
            1, 10, "generic interface swap", .true.)
            
    end subroutine test_procedure_hover
    
    subroutine test_type_hover()
        print *, ""
        print *, "Testing hover over type definitions..."
        
        ! Test 1: Hover over simple type
        call run_hover_test("Simple type hover", &
            "type :: point" // new_line('a') // &
            "real :: x, y" // new_line('a') // &
            "end type", &
            1, 8, "type :: point", .true.)
            
        ! Test 2: Hover over type with procedures
        call run_hover_test("Type with procedures hover", &
            "type :: vector" // new_line('a') // &
            "real :: x, y, z" // new_line('a') // &
            "contains" // new_line('a') // &
            "procedure :: length" // new_line('a') // &
            "end type", &
            1, 8, "type :: vector (with type-bound procedures)", .true.)
            
        ! Test 3: Hover over extended type
        call run_hover_test("Extended type hover", &
            "type, extends(shape) :: circle" // new_line('a') // &
            "real :: radius" // new_line('a') // &
            "end type", &
            1, 24, "type, extends(shape) :: circle", .true.)
            
    end subroutine test_type_hover
    
    subroutine test_module_hover()
        print *, ""
        print *, "Testing hover over module elements..."
        
        ! Test 1: Hover over module name
        call run_hover_test("Module name hover", &
            "module math_utils" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "end module", &
            1, 7, "module math_utils", .true.)
            
        ! Test 2: Hover over use statement
        call run_hover_test("Use statement hover", &
            "program test" // new_line('a') // &
            "use math_utils" // new_line('a') // &
            "end program", &
            2, 4, "use math_utils", .true.)
            
        ! Test 3: Hover over renamed import
        call run_hover_test("Renamed import hover", &
            "program test" // new_line('a') // &
            "use math_utils, only: pi_const => pi" // new_line('a') // &
            "end program", &
            2, 22, "pi_const => pi from module math_utils", .true.)
            
    end subroutine test_module_hover
    
    subroutine test_intrinsic_hover()
        print *, ""
        print *, "Testing hover over intrinsic functions..."

        ! Test 1: Hover over math intrinsic
        call run_hover_test("Math intrinsic hover", &
            "x = sin(angle)", &
            1, 4, "elemental real function sin(x)", .true.)

        ! Test 2: Hover over array intrinsic
        call run_hover_test("Array intrinsic hover", &
            "n = size(array)", &
            1, 4, "integer function size(array, dim, kind)", .true.)

        ! Test 3: Hover over type inquiry
        call run_hover_test("Type inquiry hover", &
            "k = kind(1.0)", &
            1, 4, "integer function kind(x)", .true.)

    end subroutine test_intrinsic_hover
    
    subroutine test_hover_formatting()
        print *, ""
        print *, "Testing hover message formatting..."
        
        ! Test 1: Markdown formatting
        call run_format_test("Markdown format", &
            "integer :: x", "Variable declaration", &
            "```fortran" // new_line('a') // &
            "integer :: x" // new_line('a') // &
            "```" // new_line('a') // &
            "Variable declaration", .true.)
            
        ! Test 2: Multi-line hover
        call run_format_test("Multi-line format", &
            "subroutine calc(x, y)", &
            "Calculates result from x and y", &
            "```fortran" // new_line('a') // &
            "subroutine calc(x, y)" // new_line('a') // &
            "```" // new_line('a') // &
            "Calculates result from x and y", .true.)
            
        ! Test 3: Documentation hover
        call run_format_test("Documentation format", &
            "real function area(radius)", &
            "!> Calculate circle area\n!> @param radius Circle radius\n!> @return Area", &
            "```fortran" // new_line('a') // &
            "real function area(radius)" // new_line('a') // &
            "```" // new_line('a') // &
            "Calculate circle area" // new_line('a') // &
            "**Parameters:**" // new_line('a') // &
            "- `radius`: Circle radius" // new_line('a') // &
            "**Returns:** Area", .true.)
            
    end subroutine test_hover_formatting
    
    ! Helper subroutines for testing
    subroutine run_hover_test(test_name, code, line, character, expected_hover, should_succeed)
        character(len=*), intent(in) :: test_name, code, expected_hover
        integer, intent(in) :: line, character
        logical, intent(in) :: should_succeed
        
        character(len=:), allocatable :: hover_content
        logical :: success
        
        total_tests = total_tests + 1
        
        ! Get hover information (placeholder)
        call get_hover_info(code, line, character, hover_content, success)
        
        if (success .eqv. should_succeed) then
            if (allocated(hover_content) .and. index(hover_content, expected_hover) > 0) then
                print *, "  PASS: ", test_name
                passed_tests = passed_tests + 1
            else if (.not. should_succeed) then
                print *, "  PASS: ", test_name, " - No hover as expected"
                passed_tests = passed_tests + 1
            else
                print *, "  FAIL: ", test_name, " - Wrong hover content"
                if (allocated(hover_content)) then
                    print *, "        Expected to contain: '", expected_hover, "'"
                    print *, "        Got: '", hover_content, "'"
                end if
            end if
        else
            print *, "  FAIL: ", test_name, " - Hover result unexpected"
            print *, "        Success: ", success, " Expected: ", should_succeed
        end if
        
    end subroutine run_hover_test
    
    subroutine run_format_test(test_name, signature, documentation, expected, should_succeed)
        character(len=*), intent(in) :: test_name, signature, documentation, expected
        logical, intent(in) :: should_succeed
        
        character(len=:), allocatable :: formatted
        logical :: success
        
        total_tests = total_tests + 1
        
        ! Format hover message (placeholder)
        call format_hover_message(signature, documentation, formatted, success)
        
        if (success .eqv. should_succeed) then
            if (allocated(formatted) .and. formatted == expected) then
                print *, "  PASS: ", test_name
                passed_tests = passed_tests + 1
            else
                print *, "  FAIL: ", test_name, " - Format doesn't match"
            end if
        else
            print *, "  FAIL: ", test_name, " - Formatting failed"
        end if
        
    end subroutine run_format_test
    
    
end program test_lsp_hover