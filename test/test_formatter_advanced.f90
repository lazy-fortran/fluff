program test_formatter_advanced
    use fluff_formatter
    use fluff_ast
    use fortfront, only: lex_source, parse_tokens, analyze_semantics, create_ast_arena, &
                        emit_fortran, format_options_t
    implicit none
    
    type(formatter_engine_t) :: formatter
    type(fluff_ast_context_t) :: ast_ctx
    character(len=:), allocatable :: formatted_code
    
    print *, "=== Testing Advanced Formatter Features ==="
    
    ! RED Phase: All these tests should fail initially
    call test_complex_expression_formatting()
    call test_array_literal_formatting()
    call test_procedure_formatting()
    call test_comment_preservation()
    call test_import_organization()
    call test_configurable_styles()
    call test_format_range()
    
    print *, "All advanced formatter tests passed!"
    
contains
    
    subroutine test_complex_expression_formatting()
        character(len=:), allocatable :: source_code, expected
        print *, "  Testing complex expression formatting..."
        
        ! Test 1: Long expression breaking with initialized variables
        ! NOTE: Using single variable declarations and initialization due to fortfront limitations
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "real :: result" // new_line('a') // &
                     "real :: a = 1.0" // new_line('a') // &
                     "real :: b = 2.0" // new_line('a') // &
                     "real :: c = 3.0" // new_line('a') // &
                     "real :: d = 4.0" // new_line('a') // &
                     "real :: e = 5.0" // new_line('a') // &
                     "real :: f = 6.0" // new_line('a') // &
                     "result = a * b + c * d + e * f + a * b * c + d * e * f + a * c * e + b * d * f" // new_line('a') // &
                     "end program test"
        
        expected = "program test" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "    real :: result" // new_line('a') // &
                  "    real :: a = 1.0" // new_line('a') // &
                  "    real :: b = 2.0" // new_line('a') // &
                  "    real :: c = 3.0" // new_line('a') // &
                  "    real :: d = 4.0" // new_line('a') // &
                  "    real :: e = 5.0" // new_line('a') // &
                  "    real :: f = 6.0" // new_line('a') // &
                  "    result = a * b + c * d + e * f &" // new_line('a') // &
                  "        + a * b * c + d * e * f &" // new_line('a') // &
                  "        + a * c * e + b * d * f" // new_line('a') // &
                  "end program test"
        
        call format_and_check(source_code, expected, "Long expression breaking")
        
        ! Test 2: Nested expressions with parentheses
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "real :: x" // new_line('a') // &
                     "real :: a" // new_line('a') // &
                     "real :: b" // new_line('a') // &
                     "real :: c" // new_line('a') // &
                     "real :: d" // new_line('a') // &
                     "real :: e" // new_line('a') // &
                     "real :: f" // new_line('a') // &
                     "real :: g" // new_line('a') // &
                     "real :: h" // new_line('a') // &
                     "x = (a + b) * (c + d * (e + f * (g + h)))" // new_line('a') // &
                     "end program test"
        
        expected = "program test" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "    real :: x" // new_line('a') // &
                  "    real :: a" // new_line('a') // &
                  "    real :: b" // new_line('a') // &
                  "    real :: c" // new_line('a') // &
                  "    real :: d" // new_line('a') // &
                  "    real :: e" // new_line('a') // &
                  "    real :: f" // new_line('a') // &
                  "    real :: g" // new_line('a') // &
                  "    real :: h" // new_line('a') // &
                  "    x = (a + b) * (c + d * (e + f * (g + h)))" // new_line('a') // &
                  "end program test"
        
        call format_and_check(source_code, expected, "Nested expressions")
        
        ! Test 3: Binary operator alignment
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "logical :: condition" // new_line('a') // &
                     "real :: x" // new_line('a') // &
                     "real :: y" // new_line('a') // &
                     "real :: z" // new_line('a') // &
                     "real :: w" // new_line('a') // &
                     "condition = (x > 0 .and. y < 10) .or. (z == 5 .and. w /= 3)" // new_line('a') // &
                     "end program test"
        
        expected = "program test" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "    logical :: condition" // new_line('a') // &
                  "    real :: x" // new_line('a') // &
                  "    real :: y" // new_line('a') // &
                  "    real :: z" // new_line('a') // &
                  "    real :: w" // new_line('a') // &
                  "    condition = (x > 0 .and. y < 10) &" // new_line('a') // &
                  "        .or. (z == 5 .and. w /= 3)" // new_line('a') // &
                  "end program test"
        
        call format_and_check(source_code, expected, "Binary operator alignment")
        
        print *, "    ✓ Complex expression formatting"
    end subroutine test_complex_expression_formatting
    
    subroutine test_array_literal_formatting()
        character(len=:), allocatable :: source_code, expected
        print *, "  Testing array literal formatting..."
        
        ! Test 1: Simple array literal
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "integer :: arr(5)" // new_line('a') // &
                     "arr = [1, 2, 3, 4, 5]" // new_line('a') // &
                     "end program test"
        
        expected = "program test" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "    integer :: arr(5)" // new_line('a') // &
                  "    arr = [1, 2, 3, 4, 5]" // new_line('a') // &
                  "end program test"
        
        call format_and_check(source_code, expected, "Simple array literal")
        
        ! Test 2: Multi-line array literal
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "real :: matrix(3,3)" // new_line('a') // &
                     "matrix = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0], [3, 3])" // new_line('a') // &
                     "end program test"
        
        expected = "program test" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "    real :: matrix(3,3)" // new_line('a') // &
                  "    matrix = reshape([ &" // new_line('a') // &
                  "        1.0, 2.0, 3.0, &" // new_line('a') // &
                  "        4.0, 5.0, 6.0, &" // new_line('a') // &
                  "        7.0, 8.0, 9.0 &" // new_line('a') // &
                  "    ], [3, 3])" // new_line('a') // &
                  "end program test"
        
        call format_and_check(source_code, expected, "Multi-line array literal")
        
        ! Test 3: Nested array constructors
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "integer :: nested(2,2)" // new_line('a') // &
                     "nested = [[1, 2], [3, 4]]" // new_line('a') // &
                     "end program test"
        
        expected = "program test" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "    integer :: nested(2,2)" // new_line('a') // &
                  "    nested = [ &" // new_line('a') // &
                  "        [1, 2], &" // new_line('a') // &
                  "        [3, 4] &" // new_line('a') // &
                  "    ]" // new_line('a') // &
                  "end program test"
        
        call format_and_check(source_code, expected, "Nested array constructors")
        
        print *, "    ✓ Array literal formatting"
    end subroutine test_array_literal_formatting
    
    subroutine test_procedure_formatting()
        character(len=:), allocatable :: source_code, expected
        print *, "  Testing procedure formatting..."
        
        ! Test 1: Function with multiple parameters
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "contains" // new_line('a') // &
                     "function calculate(x, y, z, alpha, beta, gamma) result(res)" // new_line('a') // &
                     "real :: x, y, z, alpha, beta, gamma, res" // new_line('a') // &
                     "res = x + y + z + alpha + beta + gamma" // new_line('a') // &
                     "end function" // new_line('a') // &
                     "end program test"
        
        expected = "program test" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "contains" // new_line('a') // &
                  "    function calculate(x, y, z, alpha, beta, gamma) result(res)" // new_line('a') // &
                  "        real :: x, y, z, alpha, beta, gamma, res" // new_line('a') // &
                  "        res = x + y + z + alpha + beta + gamma" // new_line('a') // &
                  "    end function calculate" // new_line('a') // &
                  "end program test"
        
        call format_and_check(source_code, expected, "Function formatting")
        
        ! Test 2: Subroutine with intent declarations
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "contains" // new_line('a') // &
                     "subroutine process(input, output, config)" // new_line('a') // &
                     "real, intent(in) :: input(:)" // new_line('a') // &
                     "real, intent(out) :: output(:)" // new_line('a') // &
                     "type(config_t), intent(in) :: config" // new_line('a') // &
                     "output = input * config%scale" // new_line('a') // &
                     "end subroutine" // new_line('a') // &
                     "end program test"
        
        expected = "program test" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "contains" // new_line('a') // &
                  "    subroutine process(input, output, config)" // new_line('a') // &
                  "        real, intent(in) :: input(:)" // new_line('a') // &
                  "        real, intent(out) :: output(:)" // new_line('a') // &
                  "        type(config_t), intent(in) :: config" // new_line('a') // &
                  "        output = input * config%scale" // new_line('a') // &
                  "    end subroutine process" // new_line('a') // &
                  "end program test"
        
        call format_and_check(source_code, expected, "Subroutine formatting")
        
        ! Test 3: Interface block formatting
        source_code = "module test_mod" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "interface operator(+)" // new_line('a') // &
                     "module procedure add_vectors" // new_line('a') // &
                     "end interface" // new_line('a') // &
                     "end module test_mod"
        
        expected = "module test_mod" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "    interface operator(+)" // new_line('a') // &
                  "        module procedure add_vectors" // new_line('a') // &
                  "    end interface operator(+)" // new_line('a') // &
                  "end module test_mod"
        
        call format_and_check(source_code, expected, "Interface block formatting")
        
        print *, "    ✓ Procedure formatting"
    end subroutine test_procedure_formatting
    
    subroutine test_comment_preservation()
        character(len=:), allocatable :: source_code, expected
        print *, "  Testing comment preservation..."
        
        ! Test 1: Inline comments
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "integer :: i  ! Loop counter" // new_line('a') // &
                     "real :: x     ! Result value" // new_line('a') // &
                     "i = 1         ! Initialize" // new_line('a') // &
                     "end program test"
        
        expected = "program test" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "    integer :: i  ! Loop counter" // new_line('a') // &
                  "    real :: x     ! Result value" // new_line('a') // &
                  "    i = 1         ! Initialize" // new_line('a') // &
                  "end program test"
        
        call format_and_check(source_code, expected, "Inline comment preservation")
        
        ! Test 2: Block comments
        source_code = "program test" // new_line('a') // &
                     "! This is a test program" // new_line('a') // &
                     "! It demonstrates comment handling" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "! Variable declarations" // new_line('a') // &
                     "integer :: n" // new_line('a') // &
                     "! Main logic" // new_line('a') // &
                     "n = 42" // new_line('a') // &
                     "end program test"
        
        expected = "program test" // new_line('a') // &
                  "    ! This is a test program" // new_line('a') // &
                  "    ! It demonstrates comment handling" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "    ! Variable declarations" // new_line('a') // &
                  "    integer :: n" // new_line('a') // &
                  "    ! Main logic" // new_line('a') // &
                  "    n = 42" // new_line('a') // &
                  "end program test"
        
        call format_and_check(source_code, expected, "Block comment preservation")
        
        print *, "    ✓ Comment preservation"
    end subroutine test_comment_preservation
    
    subroutine test_import_organization()
        character(len=:), allocatable :: source_code, expected
        print *, "  Testing import statement organization..."
        
        ! Test 1: Use statement ordering
        source_code = "module test_mod" // new_line('a') // &
                     "use module_c" // new_line('a') // &
                     "use module_a" // new_line('a') // &
                     "use module_b, only: func_b" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "end module test_mod"
        
        expected = "module test_mod" // new_line('a') // &
                  "    use module_a" // new_line('a') // &
                  "    use module_b, only: func_b" // new_line('a') // &
                  "    use module_c" // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "end module test_mod"
        
        call format_and_check(source_code, expected, "Use statement ordering")
        
        ! Test 2: Grouping by type
        source_code = "module test_mod" // new_line('a') // &
                     "use iso_fortran_env" // new_line('a') // &
                     "use my_module" // new_line('a') // &
                     "use iso_c_binding" // new_line('a') // &
                     "use another_module" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "end module test_mod"
        
        expected = "module test_mod" // new_line('a') // &
                  "    ! Standard library modules" // new_line('a') // &
                  "    use iso_c_binding" // new_line('a') // &
                  "    use iso_fortran_env" // new_line('a') // &
                  "    " // new_line('a') // &
                  "    ! User modules" // new_line('a') // &
                  "    use another_module" // new_line('a') // &
                  "    use my_module" // new_line('a') // &
                  "    " // new_line('a') // &
                  "    implicit none" // new_line('a') // &
                  "end module test_mod"
        
        call format_and_check(source_code, expected, "Import grouping")
        
        print *, "    ✓ Import organization"
    end subroutine test_import_organization
    
    subroutine test_configurable_styles()
        character(len=:), allocatable :: source_code, expected
        type(format_options_t) :: options
        print *, "  Testing configurable style preferences..."
        
        ! Test 1: Different indentation sizes
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "if (x > 0) then" // new_line('a') // &
                     "print *, x" // new_line('a') // &
                     "end if" // new_line('a') // &
                     "end program test"
        
        ! Test with 2-space indentation
        options%indent_size = 2
        expected = "program test" // new_line('a') // &
                  "  implicit none" // new_line('a') // &
                  "  if (x > 0) then" // new_line('a') // &
                  "    print *, x" // new_line('a') // &
                  "  end if" // new_line('a') // &
                  "end program test"
        
        call format_with_options_and_check(source_code, expected, options, "2-space indentation")
        
        ! Test with tabs
        options%indent_size = 4
        options%use_tabs = .true.
        options%indent_char = char(9)  ! Tab character
        expected = "program test" // new_line('a') // &
                  char(9) // "implicit none" // new_line('a') // &
                  char(9) // "if (x > 0) then" // new_line('a') // &
                  char(9) // char(9) // "print *, x" // new_line('a') // &
                  char(9) // "end if" // new_line('a') // &
                  "end program test"
        
        call format_with_options_and_check(source_code, expected, options, "Tab indentation")
        
        print *, "    ✓ Configurable styles"
    end subroutine test_configurable_styles
    
    subroutine test_format_range()
        character(len=:), allocatable :: source_code, expected
        integer :: start_line, end_line
        print *, "  Testing format range support..."
        
        ! Test: Format only specific lines
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "integer :: i" // new_line('a') // &
                     "! This section should be formatted" // new_line('a') // &
                     "if(i>0)then" // new_line('a') // &
                     "print*,i" // new_line('a') // &
                     "endif" // new_line('a') // &
                     "! This section should remain unchanged" // new_line('a') // &
                     "j=1+2+3" // new_line('a') // &
                     "end program test"
        
        start_line = 5
        end_line = 7
        
        expected = "program test" // new_line('a') // &
                  "implicit none" // new_line('a') // &
                  "integer :: i" // new_line('a') // &
                  "! This section should be formatted" // new_line('a') // &
                  "    if (i > 0) then" // new_line('a') // &
                  "        print *, i" // new_line('a') // &
                  "    end if" // new_line('a') // &
                  "! This section should remain unchanged" // new_line('a') // &
                  "j=1+2+3" // new_line('a') // &
                  "end program test"
        
        call format_range_and_check(source_code, expected, start_line, end_line, "Format range")
        
        print *, "    ✓ Format range support"
    end subroutine test_format_range
    
    ! Helper subroutines
    subroutine format_and_check(source, expected, test_name)
        character(len=*), intent(in) :: source, expected, test_name
        character(len=:), allocatable :: actual, error_msg
        
        call formatter%initialize()
        ast_ctx = create_ast_context()
        call ast_ctx%from_source(source, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: " // test_name // " - Parse error: " // error_msg
            error stop "Parse failed"
        end if
        call formatter%format_ast(ast_ctx, actual)
        
        if (actual /= expected) then
            print *, "FAIL: " // test_name
            print *, "Expected:"
            print *, expected
            print *, "Actual:"
            print *, actual
            error stop "Test failed"
        end if
    end subroutine format_and_check
    
    subroutine format_with_options_and_check(source, expected, options, test_name)
        character(len=*), intent(in) :: source, expected, test_name
        type(format_options_t), intent(in) :: options
        character(len=:), allocatable :: actual, error_msg
        
        formatter%options = options
        ast_ctx = create_ast_context()
        call ast_ctx%from_source(source, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: " // test_name // " - Parse error: " // error_msg
            error stop "Parse failed"
        end if
        call formatter%format_ast(ast_ctx, actual)
        
        if (actual /= expected) then
            print *, "FAIL: " // test_name
            print *, "Expected:"
            print *, expected
            print *, "Actual:"
            print *, actual
            error stop "Test failed"
        end if
    end subroutine format_with_options_and_check
    
    subroutine format_range_and_check(source, expected, start_line, end_line, test_name)
        character(len=*), intent(in) :: source, expected, test_name
        integer, intent(in) :: start_line, end_line
        character(len=:), allocatable :: actual, error_msg
        
        call formatter%initialize()
        ast_ctx = create_ast_context()
        call ast_ctx%from_source(source, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: " // test_name // " - Parse error: " // error_msg
            error stop "Parse failed"
        end if
        call formatter%format_range(ast_ctx, start_line, end_line, actual)
        
        if (actual /= expected) then
            print *, "FAIL: " // test_name
            print *, "Expected:"
            print *, expected
            print *, "Actual:"
            print *, actual
            error stop "Test failed"
        end if
    end subroutine format_range_and_check
    
end program test_formatter_advanced