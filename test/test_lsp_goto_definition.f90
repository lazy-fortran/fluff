program test_lsp_goto_definition
    use fluff_ast
    use fluff_lsp_goto_definition
    implicit none
    
    integer :: total_tests, passed_tests
    
    print *, "=== LSP Goto Definition Test Suite (RED Phase) ==="
    
    total_tests = 0
    passed_tests = 0
    
    ! Test goto definition functionality
    call test_variable_definition()
    call test_procedure_definition()
    call test_type_definition()
    call test_module_definition()
    call test_interface_definition()
    call test_cross_file_definition()
    
    print *, ""
    print *, "=== LSP Goto Definition Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"
    
    if (passed_tests == total_tests) then
        print *, "✅ All LSP goto definition tests passed!"
    else
        print *, "❌ Some tests failed (expected in RED phase)"
    end if
    
contains
    
    subroutine test_variable_definition()
        print *, ""
        print *, "Testing goto definition for variables..."
        
        ! Test 1: Local variable definition
        call run_definition_test("Local variable", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer :: x" // new_line('a') // &
            "x = 42" // new_line('a') // &
            "end program", &
            4, 0, "file:///test.f90", 3, 11, .true.)
            
        ! Test 2: Module variable definition
        call run_definition_test("Module variable", &
            "module mod" // new_line('a') // &
            "integer :: global_var" // new_line('a') // &
            "end module" // new_line('a') // &
            "program test" // new_line('a') // &
            "use mod" // new_line('a') // &
            "global_var = 10" // new_line('a') // &
            "end program", &
            6, 0, "file:///test.f90", 2, 11, .true.)
            
        ! Test 3: Procedure argument definition
        call run_definition_test("Procedure argument", &
            "subroutine calc(x, y)" // new_line('a') // &
            "real :: x, y" // new_line('a') // &
            "x = x + y" // new_line('a') // &
            "end subroutine", &
            3, 0, "file:///test.f90", 1, 16, .true.)
            
        ! Test 4: Array definition
        call run_definition_test("Array definition", &
            "program test" // new_line('a') // &
            "real :: matrix(10, 20)" // new_line('a') // &
            "matrix(1, 1) = 0.0" // new_line('a') // &
            "end program", &
            3, 0, "file:///test.f90", 2, 8, .true.)
            
        ! Test 5: Undefined variable
        call run_definition_test("Undefined variable", &
            "program test" // new_line('a') // &
            "undefined = 42" // new_line('a') // &
            "end program", &
            2, 0, "", -1, -1, .false.)
            
    end subroutine test_variable_definition
    
    subroutine test_procedure_definition()
        print *, ""
        print *, "Testing goto definition for procedures..."
        
        ! Test 1: Subroutine call definition
        call run_definition_test("Subroutine call", &
            "subroutine helper()" // new_line('a') // &
            "end subroutine" // new_line('a') // &
            "program test" // new_line('a') // &
            "call helper()" // new_line('a') // &
            "end program", &
            4, 5, "file:///test.f90", 1, 11, .true.)
            
        ! Test 2: Function call definition
        call run_definition_test("Function call", &
            "function square(x) result(y)" // new_line('a') // &
            "real :: x, y" // new_line('a') // &
            "end function" // new_line('a') // &
            "program test" // new_line('a') // &
            "y = square(2.0)" // new_line('a') // &
            "end program", &
            5, 4, "file:///test.f90", 1, 9, .true.)
            
        ! Test 3: Type-bound procedure
        call run_definition_test("Type-bound procedure", &
            "type :: vector" // new_line('a') // &
            "contains" // new_line('a') // &
            "procedure :: length" // new_line('a') // &
            "end type" // new_line('a') // &
            "type(vector) :: v" // new_line('a') // &
            "x = v%length()" // new_line('a') // &
            "", &
            6, 6, "file:///test.f90", 3, 13, .true.)
            
        ! Test 4: Generic interface
        call run_definition_test("Generic interface", &
            "interface swap" // new_line('a') // &
            "module procedure swap_int" // new_line('a') // &
            "end interface" // new_line('a') // &
            "call swap(a, b)" // new_line('a') // &
            "", &
            4, 5, "file:///test.f90", 1, 10, .true.)
            
    end subroutine test_procedure_definition
    
    subroutine test_type_definition()
        print *, ""
        print *, "Testing goto definition for types..."
        
        ! Test 1: Simple type usage
        call run_definition_test("Simple type usage", &
            "type :: point" // new_line('a') // &
            "real :: x, y" // new_line('a') // &
            "end type" // new_line('a') // &
            "type(point) :: p" // new_line('a') // &
            "", &
            4, 5, "file:///test.f90", 1, 8, .true.)
            
        ! Test 2: Extended type
        call run_definition_test("Extended type", &
            "type :: shape" // new_line('a') // &
            "end type" // new_line('a') // &
            "type, extends(shape) :: circle" // new_line('a') // &
            "end type" // new_line('a') // &
            "type(circle) :: c" // new_line('a') // &
            "", &
            5, 5, "file:///test.f90", 3, 24, .true.)
            
        ! Test 3: Type component access
        call run_definition_test("Type component", &
            "type :: coord" // new_line('a') // &
            "real :: lat, lon" // new_line('a') // &
            "end type" // new_line('a') // &
            "type(coord) :: location" // new_line('a') // &
            "location%lat = 0.0" // new_line('a') // &
            "", &
            5, 9, "file:///test.f90", 2, 8, .true.)
            
    end subroutine test_type_definition
    
    subroutine test_module_definition()
        print *, ""
        print *, "Testing goto definition for modules..."
        
        ! Test 1: Module use statement
        call run_definition_test("Module use", &
            "module utilities" // new_line('a') // &
            "end module" // new_line('a') // &
            "program test" // new_line('a') // &
            "use utilities" // new_line('a') // &
            "end program", &
            4, 4, "file:///test.f90", 1, 7, .true.)
            
        ! Test 2: Specific import
        call run_definition_test("Specific import", &
            "module math" // new_line('a') // &
            "real, parameter :: pi = 3.14159" // new_line('a') // &
            "end module" // new_line('a') // &
            "program test" // new_line('a') // &
            "use math, only: pi" // new_line('a') // &
            "end program", &
            5, 16, "file:///test.f90", 2, 19, .true.)
            
        ! Test 3: Renamed import
        call run_definition_test("Renamed import", &
            "module constants" // new_line('a') // &
            "real :: e = 2.71828" // new_line('a') // &
            "end module" // new_line('a') // &
            "program test" // new_line('a') // &
            "use constants, only: euler => e" // new_line('a') // &
            "x = euler" // new_line('a') // &
            "end program", &
            6, 4, "file:///test.f90", 2, 8, .true.)
            
    end subroutine test_module_definition
    
    subroutine test_interface_definition()
        print *, ""
        print *, "Testing goto definition for interfaces..."
        
        ! Test 1: Operator interface
        call run_definition_test("Operator interface", &
            "interface operator(+)" // new_line('a') // &
            "module procedure add_vectors" // new_line('a') // &
            "end interface" // new_line('a') // &
            "v3 = v1 + v2" // new_line('a') // &
            "", &
            4, 8, "file:///test.f90", 1, 10, .true.)
            
        ! Test 2: Assignment interface
        call run_definition_test("Assignment interface", &
            "interface assignment(=)" // new_line('a') // &
            "module procedure assign_custom" // new_line('a') // &
            "end interface" // new_line('a') // &
            "a = b" // new_line('a') // &
            "", &
            4, 2, "file:///test.f90", 1, 10, .true.)
            
        ! Test 3: Abstract interface
        call run_definition_test("Abstract interface", &
            "abstract interface" // new_line('a') // &
            "subroutine callback(x)" // new_line('a') // &
            "real :: x" // new_line('a') // &
            "end subroutine" // new_line('a') // &
            "end interface" // new_line('a') // &
            "procedure(callback) :: my_callback" // new_line('a') // &
            "", &
            6, 10, "file:///test.f90", 1, 9, .true.)
            
    end subroutine test_interface_definition
    
    subroutine test_cross_file_definition()
        print *, ""
        print *, "Testing cross-file goto definition..."
        
        ! Test 1: Module in different file
        call run_definition_test("Cross-file module", &
            "use external_module", &
            1, 4, "file:///src/external_module.f90", 1, 7, .true.)
            
        ! Test 2: Include file
        call run_definition_test("Include file", &
            "include 'common.inc'", &
            1, 9, "file:///include/common.inc", 1, 0, .true.)
            
        ! Test 3: Submodule definition
        call run_definition_test("Submodule", &
            "submodule (parent) child", &
            1, 11, "file:///src/parent.f90", 1, 7, .true.)
            
        ! Test 4: External procedure
        call run_definition_test("External procedure", &
            "external :: solver", &
            1, 12, "file:///lib/solver.f90", 1, 11, .true.)
            
    end subroutine test_cross_file_definition
    
    ! Helper subroutines for testing
    subroutine run_definition_test(test_name, code, line, character, expected_uri, expected_line, expected_char, should_succeed)
        character(len=*), intent(in) :: test_name, code, expected_uri
        integer, intent(in) :: line, character, expected_line, expected_char
        logical, intent(in) :: should_succeed
        
        character(len=:), allocatable :: result_uri
        integer :: result_line, result_char
        logical :: success
        
        total_tests = total_tests + 1
        
        ! Find definition location (placeholder)
        call find_definition(code, line, character, result_uri, result_line, result_char, success)
        
        if (success .eqv. should_succeed) then
            if (allocated(result_uri) .and. &
                result_uri == expected_uri .and. &
                result_line == expected_line .and. &
                result_char == expected_char) then
                print *, "  PASS: ", test_name
                passed_tests = passed_tests + 1
            else if (.not. should_succeed) then
                print *, "  PASS: ", test_name, " - No definition as expected"
                passed_tests = passed_tests + 1
            else
                print *, "  FAIL: ", test_name, " - Wrong location"
                if (allocated(result_uri)) then
                    print *, "        Expected: ", expected_uri, ":", expected_line, ":", expected_char
                    print *, "        Got:      ", result_uri, ":", result_line, ":", result_char
                end if
            end if
        else
            print *, "  FAIL: ", test_name, " - Definition lookup failed"
        end if
        
    end subroutine run_definition_test
    
    
end program test_lsp_goto_definition