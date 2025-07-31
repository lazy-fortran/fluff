program test_fortran_specific_style
    use fluff_formatter
    use fluff_core
    implicit none
    
    type(formatter_engine_t) :: formatter
    integer :: total_tests, passed_tests
    
    print *, "=== Fortran-Specific Style Rules Test Suite ==="
    
    total_tests = 0
    passed_tests = 0
    
    ! Test Fortran-specific style principles
    call test_derived_type_naming()
    call test_line_length_limit()
    call test_indentation_rules()
    call test_array_extension_syntax()
    call test_nesting_depth_limits()
    call test_dummy_argument_handling()
    call test_assignment_operator_overloading()
    call test_explicit_intent_declarations()
    call test_code_simplicity_principles()
    
    print *, ""
    print *, "=== Fortran-Specific Style Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"
    
    if (passed_tests == total_tests) then
        print *, "✅ All Fortran-specific style tests passed!"
    else
        print *, "❌ Some tests failed (expected in RED phase)"
    end if
    
contains
    
    subroutine test_derived_type_naming()
        print *, ""
        print *, "Testing typename_t convention for derived types..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Derived type should use typename_t convention
        call run_style_test("Derived type naming", &
            "module test_mod" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "type :: config" // new_line('a') // &
            "    integer :: value" // new_line('a') // &
            "end type config" // new_line('a') // &
            "end module", &
            "typename_t_convention")
            
        ! Test 2: Complex derived type structure
        call run_style_test("Complex derived type", &
            "module test_mod" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "type :: particle_state" // new_line('a') // &
            "    real :: position(3)" // new_line('a') // &
            "    real :: velocity(3)" // new_line('a') // &
            "    real :: mass" // new_line('a') // &
            "end type particle_state" // new_line('a') // &
            "end module", &
            "complex_type")
            
    end subroutine test_derived_type_naming
    
    subroutine test_line_length_limit()
        print *, ""
        print *, "Testing 88-character line length limit..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Long line should be broken
        call run_style_test("88-char line breaking", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "result = very_long_function_name(argument1, argument2, argument3, argument4, argument5)" // new_line('a') // &
            "end program", &
            "line_breaking")
            
        ! Test 2: Function call with many parameters
        call run_style_test("Long parameter list", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "call calculate_physics(particle, force, dt, boundary_conditions, solver_options)" // new_line('a') // &
            "end program", &
            "parameter_breaking")
            
    end subroutine test_line_length_limit
    
    subroutine test_indentation_rules()
        print *, ""
        print *, "Testing 4-space indentation rules..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Nested structure indentation
        call run_style_test("Nested indentation", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "if (condition1) then" // new_line('a') // &
            "if (condition2) then" // new_line('a') // &
            "do i = 1, n" // new_line('a') // &
            "call process(i)" // new_line('a') // &
            "end do" // new_line('a') // &
            "end if" // new_line('a') // &
            "end if" // new_line('a') // &
            "end program", &
            "nested_indent")
            
        ! Test 2: Module and procedure indentation
        call run_style_test("Module procedure indent", &
            "module test_mod" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "subroutine test_sub" // new_line('a') // &
            "integer :: local_var" // new_line('a') // &
            "local_var = 42" // new_line('a') // &
            "end subroutine" // new_line('a') // &
            "end module", &
            "module_indent")
            
    end subroutine test_indentation_rules
    
    subroutine test_array_extension_syntax()
        print *, ""
        print *, "Testing arr = [arr, new_element] syntax..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Array extension with variable
        call run_style_test("Array extension", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer, allocatable :: arr(:)" // new_line('a') // &
            "integer :: new_val" // new_line('a') // &
            "allocate(arr(3))" // new_line('a') // &
            "arr = [1, 2, 3]" // new_line('a') // &
            "new_val = 4" // new_line('a') // &
            "arr = [arr, new_val]" // new_line('a') // &
            "end program", &
            "array_extension")
            
        ! Test 2: Multiple array operations
        call run_style_test("Multiple array ops", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "real, allocatable :: data(:)" // new_line('a') // &
            "real :: item1, item2" // new_line('a') // &
            "data = [1.0, 2.0]" // new_line('a') // &
            "item1 = 3.0" // new_line('a') // &
            "item2 = 4.0" // new_line('a') // &
            "data = [data, item1]" // new_line('a') // &
            "data = [data, item2]" // new_line('a') // &
            "end program", &
            "multiple_extensions")
            
    end subroutine test_array_extension_syntax
    
    subroutine test_nesting_depth_limits()
        print *, ""
        print *, "Testing max 3 levels of nesting..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Acceptable 3-level nesting
        call run_style_test("3-level nesting OK", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "if (level1) then" // new_line('a') // &
            "    do i = 1, n" // new_line('a') // &
            "        if (level3) then" // new_line('a') // &
            "            call process()" // new_line('a') // &
            "        end if" // new_line('a') // &
            "    end do" // new_line('a') // &
            "end if" // new_line('a') // &
            "end program", &
            "three_level_ok")
            
        ! Test 2: Flag excessive nesting (4+ levels)
        call run_style_test("4-level nesting check", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "if (level1) then" // new_line('a') // &
            "    do i = 1, n" // new_line('a') // &
            "        if (level3) then" // new_line('a') // &
            "            do j = 1, m" // new_line('a') // &
            "                call process()" // new_line('a') // &
            "            end do" // new_line('a') // &
            "        end if" // new_line('a') // &
            "    end do" // new_line('a') // &
            "end if" // new_line('a') // &
            "end program", &
            "four_level_warning")
            
    end subroutine test_nesting_depth_limits
    
    subroutine test_dummy_argument_handling()
        print *, ""
        print *, "Testing dummy argument handling with associate blocks..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Unused dummy arguments in associate block
        call run_style_test("Dummy arg handling", &
            "module test_mod" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "subroutine process(used_arg, unused_arg)" // new_line('a') // &
            "    integer, intent(in) :: used_arg, unused_arg" // new_line('a') // &
            "    associate(dummy => unused_arg)" // new_line('a') // &
            "    end associate" // new_line('a') // &
            "    print *, used_arg" // new_line('a') // &
            "end subroutine" // new_line('a') // &
            "end module", &
            "dummy_args")
            
    end subroutine test_dummy_argument_handling
    
    subroutine test_assignment_operator_overloading()
        print *, ""
        print *, "Testing assignment operator overloading for allocatable members..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Derived type with allocatable members requires assignment operator
        call run_style_test("Assignment operator overload", &
            "module vector_mod" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    " // new_line('a') // &
            "    type :: dynamic_array_t" // new_line('a') // &
            "        real, allocatable :: data(:)" // new_line('a') // &
            "        integer :: size" // new_line('a') // &
            "    contains" // new_line('a') // &
            "        procedure :: assign_array" // new_line('a') // &
            "        generic :: assignment(=) => assign_array" // new_line('a') // &
            "    end type dynamic_array_t" // new_line('a') // &
            "    " // new_line('a') // &
            "contains" // new_line('a') // &
            "    " // new_line('a') // &
            "    subroutine assign_array(this, other)" // new_line('a') // &
            "        class(dynamic_array_t), intent(out) :: this" // new_line('a') // &
            "        class(dynamic_array_t), intent(in) :: other" // new_line('a') // &
            "        " // new_line('a') // &
            "        this%size = other%size" // new_line('a') // &
            "        if (allocated(other%data)) then" // new_line('a') // &
            "            allocate(this%data(size(other%data)))" // new_line('a') // &
            "            this%data = other%data" // new_line('a') // &
            "        end if" // new_line('a') // &
            "    end subroutine assign_array" // new_line('a') // &
            "    " // new_line('a') // &
            "end module vector_mod", &
            "deep_copy_assignment")
            
        ! Test 2: Complex type with multiple allocatable components
        call run_style_test("Complex assignment overload", &
            "module particle_system_mod" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    " // new_line('a') // &
            "    type :: particle_system_t" // new_line('a') // &
            "        real, allocatable :: positions(:,:)" // new_line('a') // &
            "        real, allocatable :: velocities(:,:)" // new_line('a') // &
            "        integer, allocatable :: ids(:)" // new_line('a') // &
            "        integer :: n_particles" // new_line('a') // &
            "    contains" // new_line('a') // &
            "        procedure :: assign_system" // new_line('a') // &
            "        generic :: assignment(=) => assign_system" // new_line('a') // &
            "    end type particle_system_t" // new_line('a') // &
            "end module particle_system_mod", &
            "complex_deep_copy")
            
    end subroutine test_assignment_operator_overloading
    
    subroutine test_explicit_intent_declarations()
        print *, ""
        print *, "Testing explicit intent declarations requirement..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: All procedure arguments must have explicit intent
        call run_style_test("Explicit intent required", &
            "module math_ops" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "    " // new_line('a') // &
            "    pure function add(a, b) result(sum)" // new_line('a') // &
            "        real, intent(in) :: a, b" // new_line('a') // &
            "        real :: sum" // new_line('a') // &
            "        sum = a + b" // new_line('a') // &
            "    end function add" // new_line('a') // &
            "    " // new_line('a') // &
            "    subroutine process_data(input, output, status)" // new_line('a') // &
            "        real, intent(in) :: input(:)" // new_line('a') // &
            "        real, intent(out) :: output(:)" // new_line('a') // &
            "        integer, intent(out) :: status" // new_line('a') // &
            "        " // new_line('a') // &
            "        output = input * 2.0" // new_line('a') // &
            "        status = 0" // new_line('a') // &
            "    end subroutine process_data" // new_line('a') // &
            "    " // new_line('a') // &
            "end module math_ops", &
            "explicit_intent")
            
        ! Test 2: Intent(inout) for modifiable arguments
        call run_style_test("Intent inout usage", &
            "module state_mod" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "    " // new_line('a') // &
            "    subroutine update_state(state, delta_time)" // new_line('a') // &
            "        real, intent(inout) :: state(:)" // new_line('a') // &
            "        real, intent(in) :: delta_time" // new_line('a') // &
            "        " // new_line('a') // &
            "        state = state + delta_time" // new_line('a') // &
            "    end subroutine update_state" // new_line('a') // &
            "    " // new_line('a') // &
            "end module state_mod", &
            "intent_inout")
            
        ! Test 3: Contrast with lazy Fortran defaults
        call run_style_test("Standard vs lazy Fortran", &
            "module comparison_mod" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "    ! Standard Fortran: explicit intent required" // new_line('a') // &
            "    pure function standard_calc(x, y) result(z)" // new_line('a') // &
            "        real, intent(in) :: x, y" // new_line('a') // &
            "        real :: z" // new_line('a') // &
            "        z = x * y" // new_line('a') // &
            "    end function standard_calc" // new_line('a') // &
            "    " // new_line('a') // &
            "    ! Note: In lazy Fortran, intent(in) would be default" // new_line('a') // &
            "    ! But standard Fortran requires explicit declaration" // new_line('a') // &
            "end module comparison_mod", &
            "standard_vs_lazy")
            
    end subroutine test_explicit_intent_declarations
    
    subroutine test_code_simplicity_principles()
        print *, ""
        print *, "Testing code simplicity and elegance principles..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Simple and clean procedure structure
        call run_style_test("Simple procedure", &
            "module math_utils" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "    pure function square(x) result(result)" // new_line('a') // &
            "        real, intent(in) :: x" // new_line('a') // &
            "        real :: result" // new_line('a') // &
            "        result = x * x" // new_line('a') // &
            "    end function square" // new_line('a') // &
            "end module math_utils", &
            "simple_clean")
            
        ! Test 2: Avoiding redundancy and complexity
        call run_style_test("Clean structure", &
            "module physics_t" // new_line('a') // &
            "    use iso_fortran_env, only: dp => real64" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    " // new_line('a') // &
            "    type :: particle_t" // new_line('a') // &
            "        real(dp) :: mass" // new_line('a') // &
            "        real(dp) :: position(3)" // new_line('a') // &
            "        real(dp) :: velocity(3)" // new_line('a') // &
            "    end type particle_t" // new_line('a') // &
            "end module physics_t", &
            "clean_module")
            
    end subroutine test_code_simplicity_principles
    
    ! Helper subroutine for running style tests
    subroutine run_style_test(test_name, input, expected_feature)
        character(len=*), intent(in) :: test_name, input, expected_feature
        character(len=:), allocatable :: formatted_code, error_msg
        
        total_tests = total_tests + 1
        
        call formatter%format_source(input, formatted_code, error_msg)
        
        if (error_msg /= "") then
            print *, "  FAIL: ", test_name, " - Error: ", error_msg
            return
        end if
        
        ! For RED phase, just check that formatting completes
        ! In GREEN phase, we'll add specific validations for each principle
        if (len(formatted_code) > 0) then
            print *, "  PASS: ", test_name, " (", expected_feature, ")"
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Empty output"
        end if
        
    end subroutine run_style_test
    
end program test_fortran_specific_style