program test_rule_false_positive_analysis
    ! False positive analysis for all rules
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, assert_has_diagnostic_code
    implicit none
    
    print *, "Running comprehensive false positive analysis..."
    
    ! Test style rules for false positives
    call analyze_style_rule_false_positives()
    
    ! Test performance rules for false positives
    call analyze_performance_rule_false_positives()
    
    ! Test correctness rules for false positives
    call analyze_correctness_rule_false_positives()
    
    ! Analyze edge cases that might trigger false positives
    call analyze_edge_case_false_positives()
    
    print *, "False positive analysis completed!"
    
contains
    
    subroutine analyze_style_rule_false_positives()
        print *, "  🔍 Analyzing style rules for false positives..."
        
        ! F001: implicit none - test valid cases
        call test_f001_valid_cases()
        
        ! F002: indentation - test valid formatting
        call test_f002_valid_cases()
        
        ! F003: line length - test acceptable long lines
        call test_f003_valid_cases()
        
        ! F004: trailing whitespace - test intentional whitespace
        call test_f004_valid_cases()
        
        ! F005: mixed tabs/spaces - test consistent formatting
        call test_f005_valid_cases()
        
        print *, "    ✓ Style rule false positive analysis completed"
        
    end subroutine analyze_style_rule_false_positives
    
    subroutine analyze_performance_rule_false_positives()
        print *, "  🔍 Analyzing performance rules for false positives..."
        
        ! P001: array access - test legitimate non-contiguous access
        call test_p001_valid_cases()
        
        ! P002: loop ordering - test cases where order doesn't matter
        call test_p002_valid_cases()
        
        ! P003: array temporaries - test necessary temporaries
        call test_p003_valid_cases()
        
        ! P004: pure/elemental - test cases where not applicable
        call test_p004_valid_cases()
        
        print *, "    ✓ Performance rule false positive analysis completed"
        
    end subroutine analyze_performance_rule_false_positives
    
    subroutine analyze_correctness_rule_false_positives()
        print *, "  🔍 Analyzing correctness rules for false positives..."
        
        ! C001: undefined variable - test valid variable usage
        call test_c001_valid_cases()
        
        print *, "    ✓ Correctness rule false positive analysis completed"
        
    end subroutine analyze_correctness_rule_false_positives
    
    subroutine analyze_edge_case_false_positives()
        print *, "  🔍 Analyzing edge cases for false positives..."
        
        ! Test complex code patterns that might confuse rules
        call test_complex_patterns()
        
        ! Test legacy Fortran constructs
        call test_legacy_constructs()
        
        ! Test modern Fortran features
        call test_modern_features()
        
        print *, "    ✓ Edge case false positive analysis completed"
        
    end subroutine analyze_edge_case_false_positives
    
    ! F001 valid cases
    subroutine test_f001_valid_cases()
        character(len=:), allocatable :: test_code

        test_code = "program valid_implicit_none"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: valid_var"//new_line('a')// &
                    "    valid_var = 42"//new_line('a')// &
                    "    print *, valid_var"//new_line('a')// &
                    "end program valid_implicit_none"

        call run_valid_case("F001", test_code, "implicit none present")
    end subroutine test_f001_valid_cases
    
    ! F002 valid cases
    subroutine test_f002_valid_cases()
        character(len=:), allocatable :: test_code

        test_code = "program valid_indent"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    do i = 1, 2"//new_line('a')// &
                    "        print *, i"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program valid_indent"

        call run_valid_case("F002", test_code, "consistent indentation")
    end subroutine test_f002_valid_cases
    
    ! F003 valid cases
    subroutine test_f003_valid_cases()
        character(len=:), allocatable :: test_code

        test_code = "program valid_line_length"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    i = 1"//new_line('a')// &
                    "    print *, i"//new_line('a')// &
                    "end program valid_line_length"

        call run_valid_case("F003", test_code, "no long lines")
    end subroutine test_f003_valid_cases
    
    ! F004 valid cases
    subroutine test_f004_valid_cases()
        character(len=:), allocatable :: test_code

        test_code = "program valid_whitespace"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    i = 1"//new_line('a')// &
                    "end program valid_whitespace"

        call run_valid_case("F004", test_code, "no trailing whitespace")
    end subroutine test_f004_valid_cases
    
    ! F005 valid cases
    subroutine test_f005_valid_cases()
        character(len=:), allocatable :: test_code

        test_code = "program valid_tabs_spaces"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    i = 1"//new_line('a')// &
                    "end program valid_tabs_spaces"

        call run_valid_case("F005", test_code, "spaces-only indentation")
    end subroutine test_f005_valid_cases
    
    ! P001 valid cases
    subroutine test_p001_valid_cases()
        character(len=:), allocatable :: test_code

        test_code = "program sparse_matrix"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    real :: matrix(100, 100)"//new_line('a')// &
                    "    integer :: indices(50)"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    do i = 1, 50"//new_line('a')// &
                    "        matrix(indices(i), i) = real(i)"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program sparse_matrix"

        call run_valid_case("P001", test_code, "sparse access not a loop-order issue")
    end subroutine test_p001_valid_cases
    
    ! P002 valid cases
    subroutine test_p002_valid_cases()
        character(len=:), allocatable :: test_code

        test_code = "program nested_scalar"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i, j"//new_line('a')// &
                    "    integer :: s"//new_line('a')// &
                    "    s = 0"//new_line('a')// &
                    "    do i = 1, 5"//new_line('a')// &
                    "        do j = 1, 5"//new_line('a')// &
                    "            s = s + i + j"//new_line('a')// &
                    "        end do"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "    print *, s"//new_line('a')// &
                    "end program nested_scalar"

        call run_valid_case("P002", test_code, "no array access to reorder")
    end subroutine test_p002_valid_cases
    
    ! P003 valid cases
    subroutine test_p003_valid_cases()
        character(len=:), allocatable :: test_code

        test_code = "program elementwise_ok"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer, parameter :: n = 5"//new_line('a')// &
                    "    real :: a(n), b(n), c(n)"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    do i = 1, n"//new_line('a')// &
                    "        a(i) = b(i) + c(i)"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program elementwise_ok"

        call run_valid_case("P003", test_code, &
                            "element-wise assignment avoids temporaries")
    end subroutine test_p003_valid_cases
    
    ! P004 valid cases
    subroutine test_p004_valid_cases()
        character(len=:), allocatable :: test_code

        test_code = "subroutine has_side_effect(x)"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    real, intent(in) :: x"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end subroutine has_side_effect"

        call run_valid_case("P004", test_code, &
                            "procedure with IO is not pure candidate")
    end subroutine test_p004_valid_cases
    
    ! C001 valid cases
    subroutine test_c001_valid_cases()
        character(len=:), allocatable :: test_code

        test_code = "program defined_vars"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    i = 1"//new_line('a')// &
                    "    print *, i"//new_line('a')// &
                    "end program defined_vars"

        call run_valid_case("C001", test_code, "all variables defined")
    end subroutine test_c001_valid_cases
    
    ! Complex patterns that might trigger false positives
    subroutine test_complex_patterns()
        character(len=:), allocatable :: test_code

        test_code = "program complex_nested"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    type :: point_t"//new_line('a')// &
                    "        real :: x, y"//new_line('a')// &
                    "    end type point_t"//new_line('a')// &
                    "    type(point_t) :: p"//new_line('a')// &
                    "    p%x = 1.0"//new_line('a')// &
                    "    p%y = 2.0"//new_line('a')// &
                    "end program complex_nested"

        call run_valid_case("C001", test_code, "derived type member access defined")
    end subroutine test_complex_patterns
    
    ! Legacy Fortran constructs
    subroutine test_legacy_constructs()
        character(len=:), allocatable :: test_code

        test_code = "program legacy_constructs"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    do 10 i = 1, 3"//new_line('a')// &
                    "        print *, i"//new_line('a')// &
                    "10  continue"//new_line('a')// &
                    "end program legacy_constructs"

        call run_valid_case("F010", test_code, "legacy constructs without goto/common")
    end subroutine test_legacy_constructs
    
    ! Modern Fortran features
    subroutine test_modern_features()
        character(len=:), allocatable :: test_code

        test_code = "program modern_features"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: i"//new_line('a')// &
                    "    do concurrent (i = 1:3)"//new_line('a')// &
                    "        print *, i"//new_line('a')// &
                    "    end do"//new_line('a')// &
                    "end program modern_features"

        call run_valid_case("F002", test_code, "do concurrent formatting ok")
    end subroutine test_modern_features
    
    subroutine run_valid_case(rule_code, test_code, description)
        character(len=*), intent(in) :: rule_code, test_code, description
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: path

        linter = create_linter_engine()

        call make_temp_fortran_path("fluff_test_fp", path)
        call write_text_file(path, test_code)
        call linter%lint_file(path, diagnostics, error_msg)
        call delete_file_if_exists(path)

        if (len(error_msg) > 0) then
            error stop "Lint error in false positive test: "//trim(description)
        end if

        call assert_has_diagnostic_code(diagnostics, rule_code, .false., description)
    end subroutine run_valid_case
    
end program test_rule_false_positive_analysis
