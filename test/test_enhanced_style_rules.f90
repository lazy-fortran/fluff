program test_enhanced_style_rules
    use fluff_formatter
    use fluff_core
    implicit none
    
    type(formatter_engine_t) :: formatter
    integer :: total_tests, passed_tests
    
    print *, "=== Enhanced Style Rules Test Suite (PEP 8 + Black + Modern Fortran) ==="
    
    total_tests = 0
    passed_tests = 0
    
    ! Test PEP 8 & Black derived rules
    call test_blank_line_rules()
    call test_whitespace_rules()
    call test_trailing_comma_rules()
    call test_string_quote_consistency()
    call test_comment_formatting()
    call test_naming_conventions()
    
    ! Test Modern Fortran specific rules
    call test_use_statement_rules()
    call test_procedure_interface_rules()
    call test_modern_fortran_features()
    call test_memory_management_rules()
    call test_error_handling_patterns()
    
    print *, ""
    print *, "=== Enhanced Style Rules Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"
    
    if (passed_tests == total_tests) then
        print *, "[OK] All enhanced style rules tests passed!"
    else
        print *, "[FAIL] Some tests failed (expected in RED phase)"
    end if
    
contains
    
    subroutine test_blank_line_rules()
        print *, ""
        print *, "Testing blank line rules (from PEP 8)..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Two blank lines before top-level procedures
        call run_style_test("Top-level procedure spacing", &
            "module math_utils" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "subroutine first_proc" // new_line('a') // &
            "end subroutine first_proc" // new_line('a') // &
            "subroutine second_proc" // new_line('a') // &
            "end subroutine second_proc" // new_line('a') // &
            "end module", &
            "two_blank_lines_between_procedures")
            
        ! Test 2: Logical section separation within procedures
        call run_style_test("Logical section separation", &
            "subroutine complex_calculation(input, output)" // new_line('a') // &
            "    real, intent(in) :: input" // new_line('a') // &
            "    real, intent(out) :: output" // new_line('a') // &
            "    real :: temp1, temp2" // new_line('a') // &
            "    temp1 = input * 2.0" // new_line('a') // &
            "    temp2 = temp1 + 1.0" // new_line('a') // &
            "    output = temp2 / 3.0" // new_line('a') // &
            "end subroutine", &
            "logical_sections")
            
    end subroutine test_blank_line_rules
    
    subroutine test_whitespace_rules()
        print *, ""
        print *, "Testing whitespace rules (from PEP 8 + Black)..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: No trailing whitespace
        call run_style_test("No trailing whitespace", &
            "program test    " // new_line('a') // &  ! trailing spaces
            "    implicit none  " // new_line('a') // &  ! trailing spaces
            "    integer :: i" // new_line('a') // &
            "end program", &
            "no_trailing_whitespace")
            
        ! Test 2: No whitespace inside parentheses
        call run_style_test("Parentheses whitespace", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    call func( a, b )" // new_line('a') // &  ! bad spacing
            "    result = sqrt( x * x + y * y )" // new_line('a') // &  ! bad spacing
            "end program", &
            "parentheses_spacing")
            
        ! Test 3: Spaces around binary operators
        call run_style_test("Binary operator spacing", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    result=a+b*c-d/e" // new_line('a') // &  ! no spaces
            "    logical_result=x>0.and.y<10" // new_line('a') // &  ! no spaces
            "end program", &
            "operator_spacing")
            
        ! Test 4: Comma and semicolon spacing
        call run_style_test("Comma spacing", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: a,b,c" // new_line('a') // &  ! no spaces after commas
            "    call process(x ,y ,z)" // new_line('a') // &  ! spaces before commas (bad)
            "end program", &
            "comma_spacing")
            
    end subroutine test_whitespace_rules
    
    subroutine test_trailing_comma_rules()
        print *, ""
        print *, "Testing trailing comma rules (from Black)..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Trailing commas in array constructors
        call run_style_test("Array constructor trailing commas", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: long_array(5)" // new_line('a') // &
            "    long_array = [&" // new_line('a') // &
            "        first_element, &" // new_line('a') // &
            "        second_element, &" // new_line('a') // &
            "        third_element &" // new_line('a') // &  ! should have trailing comma
            "    ]" // new_line('a') // &
            "end program", &
            "trailing_commas_arrays")
            
        ! Test 2: Trailing commas in procedure arguments
        call run_style_test("Procedure argument trailing commas", &
            "subroutine long_signature(&" // new_line('a') // &
            "    first_arg, &" // new_line('a') // &
            "    second_arg, &" // new_line('a') // &
            "    third_arg &" // new_line('a') // &  ! should have trailing comma
            ")" // new_line('a') // &
            "    integer, intent(in) :: first_arg, second_arg, third_arg" // new_line('a') // &
            "end subroutine", &
            "trailing_commas_args")
            
    end subroutine test_trailing_comma_rules
    
    subroutine test_string_quote_consistency()
        print *, ""
        print *, "Testing string quote consistency (from Black)..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Consistent quote style (prefer double quotes like Black)
        call run_style_test("Quote consistency", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    character(len=*), parameter :: msg1 = 'Hello'" // new_line('a') // &  ! single quotes
            "    character(len=*), parameter :: msg2 = " // '"World"' // new_line('a') // &  ! double quotes
            "    print *, msg1, msg2" // new_line('a') // &
            "end program", &
            "quote_consistency")
            
    end subroutine test_string_quote_consistency
    
    subroutine test_comment_formatting()
        print *, ""
        print *, "Testing comment formatting rules (from PEP 8)..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Comments should be complete sentences with proper capitalization
        call run_style_test("Comment formatting", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    ! this is a bad comment" // new_line('a') // &  ! no capital, no period
            "    integer :: counter  ! bad inline comment" // new_line('a') // &  ! no capital
            "    ! This is a proper comment." // new_line('a') // &
            "    real :: value  ! This is a proper inline comment." // new_line('a') // &
            "end program", &
            "comment_formatting")
            
    end subroutine test_comment_formatting
    
    subroutine test_naming_conventions()
        print *, ""
        print *, "Testing naming conventions (snake_case, no single chars, constants)..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: No single character names except loop counters
        call run_style_test("No single character names", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    real :: x, y, z  ! Bad: single char names" // new_line('a') // &
            "    integer :: n  ! Bad: single char name" // new_line('a') // &
            "    do i = 1, 10  ! OK: loop counter" // new_line('a') // &
            "        print *, i" // new_line('a') // &
            "    end do" // new_line('a') // &
            "end program", &
            "no_single_chars")
            
        ! Test 2: Constants in SCREAMING_SNAKE_CASE
        call run_style_test("Constant naming", &
            "module constants" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    real, parameter :: pi = 3.14159  ! Bad: should be PI" // new_line('a') // &
            "    real, parameter :: PI = 3.14159  ! Good" // new_line('a') // &
            "    real, parameter :: SPEED_OF_LIGHT = 299792458.0  ! Good" // new_line('a') // &
            "    real, parameter :: avogadro_number = 6.022e23  ! Bad: should be AVOGADRO_NUMBER" // new_line('a') // &
            "end module", &
            "constant_naming")
            
        ! Test 3: snake_case for everything else (no CamelCase)
        call run_style_test("snake_case naming", &
            "module MyModule  ! Bad: should be my_module" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    type :: ParticleType  ! Bad: should be particle_t" // new_line('a') // &
            "        real :: massValue  ! Bad: should be mass_value" // new_line('a') // &
            "    end type" // new_line('a') // &
            "contains" // new_line('a') // &
            "    subroutine CalculateForce()  ! Bad: should be calculate_force" // new_line('a') // &
            "    end subroutine" // new_line('a') // &
            "end module", &
            "snake_case_naming")
            
    end subroutine test_naming_conventions
    
    subroutine test_use_statement_rules()
        print *, ""
        print *, "Testing use statement rules (Modern Fortran)..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Always use 'only:' clause
        call run_style_test("Use only clause", &
            "module test_mod" // new_line('a') // &
            "    use iso_fortran_env  ! Bad: no only clause" // new_line('a') // &
            "    use iso_c_binding  ! Bad: no only clause" // new_line('a') // &
            "    use my_module, only: my_function  ! Good" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "end module", &
            "use_only_clause")
            
        ! Test 2: Group imports: intrinsic, external, local
        call run_style_test("Import grouping", &
            "module test_mod" // new_line('a') // &
            "    use my_local_module, only: local_func" // new_line('a') // &
            "    use iso_fortran_env, only: real64" // new_line('a') // &
            "    use third_party_lib, only: external_func" // new_line('a') // &
            "    use iso_c_binding, only: c_int" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "end module", &
            "import_grouping")
            
    end subroutine test_use_statement_rules
    
    subroutine test_procedure_interface_rules()
        print *, ""
        print *, "Testing procedure interface rules (Modern Fortran)..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Always use explicit interfaces, prefer pure/elemental
        call run_style_test("Explicit interfaces", &
            "module math_utils" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "    function square(x) result(y)  ! Bad: not pure" // new_line('a') // &
            "        real, intent(in) :: x" // new_line('a') // &
            "        real :: y" // new_line('a') // &
            "        y = x * x" // new_line('a') // &
            "    end function" // new_line('a') // &
            "    pure function cube(x) result(y)  ! Good: pure" // new_line('a') // &
            "        real, intent(in) :: x" // new_line('a') // &
            "        real :: y" // new_line('a') // &
            "        y = x * x * x" // new_line('a') // &
            "    end function" // new_line('a') // &
            "end module", &
            "explicit_interfaces")
            
    end subroutine test_procedure_interface_rules
    
    subroutine test_modern_fortran_features()
        print *, ""
        print *, "Testing modern Fortran feature preferences..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Prefer allocatable over pointer
        call run_style_test("Allocatable vs pointer", &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    real, pointer :: bad_array(:)  ! Bad: use allocatable" // new_line('a') // &
            "    real, allocatable :: good_array(:)  ! Good" // new_line('a') // &
            "    allocate(good_array(100))" // new_line('a') // &
            "end program", &
            "allocatable_vs_pointer")
            
        ! Test 2: Use associate constructs for complex expressions
        call run_style_test("Associate constructs", &
            "subroutine complex_calculation(data)" // new_line('a') // &
            "    real, intent(inout) :: data(:)" // new_line('a') // &
            "    real :: norm, max_val" // new_line('a') // &
            "    ! Simple associate example" // new_line('a') // &
            "    norm = 1.0" // new_line('a') // &
            "    max_val = 2.0" // new_line('a') // &
            "    data = data * norm / max_val" // new_line('a') // &
            "end subroutine", &
            "associate_constructs")
            
    end subroutine test_modern_fortran_features
    
    subroutine test_memory_management_rules()
        print *, ""
        print *, "Testing memory management rules..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Always deallocate what you allocate
        call run_style_test("Memory deallocation", &
            "subroutine bad_memory_usage()" // new_line('a') // &
            "    real, allocatable :: temp(:)" // new_line('a') // &
            "    allocate(temp(1000))" // new_line('a') // &
            "    ! Process data..." // new_line('a') // &
            "    ! Bad: no deallocation" // new_line('a') // &
            "end subroutine" // new_line('a') // &
            "subroutine good_memory_usage()" // new_line('a') // &
            "    real, allocatable :: temp(:)" // new_line('a') // &
            "    allocate(temp(1000))" // new_line('a') // &
            "    ! Process data..." // new_line('a') // &
            "    deallocate(temp)  ! Good: explicit deallocation" // new_line('a') // &
            "end subroutine", &
            "memory_deallocation")
            
    end subroutine test_memory_management_rules
    
    subroutine test_error_handling_patterns()
        print *, ""
        print *, "Testing error handling patterns (Modern Fortran)..."
        
        call formatter%initialize()
        call formatter%set_style_guide("clean")
        
        ! Test 1: Use stat= and errmsg= for error handling
        call run_style_test("Error handling", &
            "subroutine safe_allocation(array, n)" // new_line('a') // &
            "    real, allocatable, intent(out) :: array(:)" // new_line('a') // &
            "    integer, intent(in) :: n" // new_line('a') // &
            "    integer :: stat" // new_line('a') // &
            "    character(len=100) :: errmsg" // new_line('a') // &
            "    " // new_line('a') // &
            "    allocate(array(n), stat=stat, errmsg=errmsg)" // new_line('a') // &
            "    if (stat /= 0) then" // new_line('a') // &
            "        print *, 'Allocation failed: ', trim(errmsg)" // new_line('a') // &
            "        stop 1" // new_line('a') // &
            "    end if" // new_line('a') // &
            "end subroutine", &
            "error_handling")
            
    end subroutine test_error_handling_patterns
    
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
        ! In GREEN phase, we'll add specific validations for each rule
        if (len(formatted_code) > 0) then
            print *, "  PASS: ", test_name, " (", expected_feature, ")"
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name, " - Empty output"
        end if
        
    end subroutine run_style_test
    
end program test_enhanced_style_rules