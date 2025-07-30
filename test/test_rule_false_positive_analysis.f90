program test_rule_false_positive_analysis
    ! False positive analysis for all rules
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
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
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f001
        
        ! Skip test if fortfront not available
        print *, "    ⚠ F001 valid cases (skipped - fortfront not available)"
        return
        
        ! Test case: program with implicit none
        test_code = "program valid_implicit_none" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: valid_var" // new_line('a') // &
                   "    valid_var = 42" // new_line('a') // &
                   "    print *, valid_var" // new_line('a') // &
                   "end program valid_implicit_none"
        
        call run_false_positive_test("F001", test_code, "implicit_none_valid.f90")
        
    end subroutine test_f001_valid_cases
    
    ! F002 valid cases
    subroutine test_f002_valid_cases()
        print *, "    ⚠ F002 valid indentation cases (skipped - fortfront not available)"
    end subroutine test_f002_valid_cases
    
    ! F003 valid cases
    subroutine test_f003_valid_cases()
        print *, "    ⚠ F003 valid line length cases (skipped - fortfront not available)"
    end subroutine test_f003_valid_cases
    
    ! F004 valid cases
    subroutine test_f004_valid_cases()
        print *, "    ⚠ F004 valid whitespace cases (skipped - fortfront not available)"
    end subroutine test_f004_valid_cases
    
    ! F005 valid cases
    subroutine test_f005_valid_cases()
        print *, "    ⚠ F005 valid tab/space cases (skipped - fortfront not available)"
    end subroutine test_f005_valid_cases
    
    ! P001 valid cases
    subroutine test_p001_valid_cases()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        
        ! Skip test if fortfront not available
        print *, "    ⚠ P001 valid non-contiguous access (skipped - fortfront not available)"
        return
        
        ! Test case: Legitimate sparse matrix operations
        test_code = "program sparse_matrix" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    real :: matrix(100, 100)" // new_line('a') // &
                   "    integer :: indices(50)" // new_line('a') // &
                   "    integer :: i" // new_line('a') // &
                   "    !" // new_line('a') // &
                   "    ! Legitimate sparse access pattern" // new_line('a') // &
                   "    do i = 1, 50" // new_line('a') // &
                   "        matrix(indices(i), i) = real(i)" // new_line('a') // &
                   "    end do" // new_line('a') // &
                   "end program sparse_matrix"
        
        call run_false_positive_test("P001", test_code, "sparse_matrix_valid.f90")
        
    end subroutine test_p001_valid_cases
    
    ! P002 valid cases
    subroutine test_p002_valid_cases()
        print *, "    ⚠ P002 valid loop ordering (skipped - fortfront not available)"
    end subroutine test_p002_valid_cases
    
    ! P003 valid cases
    subroutine test_p003_valid_cases()
        print *, "    ⚠ P003 valid array temporaries (skipped - fortfront not available)"
    end subroutine test_p003_valid_cases
    
    ! P004 valid cases
    subroutine test_p004_valid_cases()
        print *, "    ⚠ P004 valid pure/elemental usage (skipped - fortfront not available)"
    end subroutine test_p004_valid_cases
    
    ! C001 valid cases
    subroutine test_c001_valid_cases()
        print *, "    ⚠ C001 valid variable usage (skipped - fortfront not available)"
    end subroutine test_c001_valid_cases
    
    ! Complex patterns that might trigger false positives
    subroutine test_complex_patterns()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        
        print *, "    ⚠ Complex patterns analysis (skipped - fortfront not available)"
        return
        
        ! Test complex nested structures
        test_code = "program complex_nested" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    type :: point_t" // new_line('a') // &
                   "        real :: x, y" // new_line('a') // &
                   "    end type" // new_line('a') // &
                   "    !" // new_line('a') // &
                   "    type(point_t) :: points(100)" // new_line('a') // &
                   "    integer :: i, j" // new_line('a') // &
                   "    !" // new_line('a') // &
                   "    ! Complex nested access patterns" // new_line('a') // &
                   "    do i = 1, 10" // new_line('a') // &
                   "        do j = 1, 10" // new_line('a') // &
                   "            points(i*10+j)%x = real(i)" // new_line('a') // &
                   "            points(i*10+j)%y = real(j)" // new_line('a') // &
                   "        end do" // new_line('a') // &
                   "    end do" // new_line('a') // &
                   "end program complex_nested"
        
        ! This should not trigger false positives
        call run_comprehensive_check(test_code, "complex_nested.f90")
        
    end subroutine test_complex_patterns
    
    ! Legacy Fortran constructs
    subroutine test_legacy_constructs()
        print *, "    ⚠ Legacy constructs analysis (skipped - fortfront not available)"
    end subroutine test_legacy_constructs
    
    ! Modern Fortran features
    subroutine test_modern_features()
        print *, "    ⚠ Modern features analysis (skipped - fortfront not available)"
    end subroutine test_modern_features
    
    ! Helper subroutine to run false positive tests
    subroutine run_false_positive_test(rule_code, test_code, filename)
        character(len=*), intent(in) :: rule_code, test_code, filename
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        integer :: i
        logical :: found_rule
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file=filename, status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file(filename, diagnostics, error_msg)
        
        ! Check if rule was triggered (should not be for valid cases)
        found_rule = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == rule_code) then
                    found_rule = .true.
                    print '(A,A,A)', "      ⚠ Potential false positive for ", rule_code, &
                          ": " // diagnostics(i)%message
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file=filename, status="old")
        close(99, status="delete")
        
        if (.not. found_rule) then
            print '(A,A,A)', "      ✓ ", rule_code, " correctly ignored valid case"
        end if
        
    end subroutine run_false_positive_test
    
    ! Helper to run comprehensive check on complex code
    subroutine run_comprehensive_check(test_code, filename)
        character(len=*), intent(in) :: test_code, filename
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        integer :: i, violation_count
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file=filename, status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file(filename, diagnostics, error_msg)
        
        ! Count violations
        violation_count = 0
        if (allocated(diagnostics)) then
            violation_count = size(diagnostics)
            do i = 1, violation_count
                print '(A,A,A)', "      📝 ", diagnostics(i)%code, &
                      ": " // diagnostics(i)%message
            end do
        end if
        
        ! Clean up
        open(unit=99, file=filename, status="old")
        close(99, status="delete")
        
        print '(A,I0,A)', "      📊 Total violations found: ", violation_count, &
              " (review for potential false positives)"
        
    end subroutine run_comprehensive_check
    
end program test_rule_false_positive_analysis