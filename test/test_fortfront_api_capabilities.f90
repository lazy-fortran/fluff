program test_fortfront_api_capabilities
    ! Comprehensive test of fortfront API capabilities through fluff integration
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none
    
    print *, "Testing fortfront API capabilities..."
    
    ! Test basic AST parsing
    call test_basic_ast_parsing()
    
    ! Test semantic analysis
    call test_semantic_analysis()
    
    ! Test type inference
    call test_type_inference()
    
    ! Test symbol table access
    call test_symbol_table_access()
    
    ! Test rule integration with real AST
    call test_rule_integration()
    
    print *, "Fortfront API capability testing completed!"
    
contains
    
    subroutine test_basic_ast_parsing()
        type(fluff_ast_context_t) :: ast_ctx
        character(len=:), allocatable :: test_code
        logical :: parsing_works
        
        print *, "  🌳 Testing basic AST parsing..."
        
        test_code = "program simple_test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: i" // new_line('a') // &
                   "    i = 42" // new_line('a') // &
                   "    print *, i" // new_line('a') // &
                   "end program simple_test"
        
        ! Create test file
        open(unit=99, file="fortfront_test_basic.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Try to parse with fortfront API
        parsing_works = .false.
        
        ! Test if we can create AST context from source
        ! This will tell us if fortfront API is working
        call test_ast_context_creation(ast_ctx, "fortfront_test_basic.f90", parsing_works)
        
        ! Clean up
        open(unit=99, file="fortfront_test_basic.f90", status="old")
        close(99, status="delete")
        
        if (parsing_works) then
            print *, "    ✅ Basic AST parsing: WORKING"
        else
            print *, "    ❌ Basic AST parsing: NOT WORKING (stub implementation)"
        end if
        
    end subroutine test_basic_ast_parsing
    
    subroutine test_semantic_analysis()
        type(fluff_ast_context_t) :: ast_ctx
        character(len=:), allocatable :: test_code
        logical :: semantic_works
        
        print *, "  🧠 Testing semantic analysis..."
        
        test_code = "program semantic_test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: x, y" // new_line('a') // &
                   "    real :: z" // new_line('a') // &
                   "    x = 10" // new_line('a') // &
                   "    y = x + 5" // new_line('a') // &
                   "    z = real(y)" // new_line('a') // &
                   "    print *, z" // new_line('a') // &
                   "end program semantic_test"
        
        ! Create test file
        open(unit=99, file="fortfront_test_semantic.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        semantic_works = .false.
        call test_semantic_analysis_features(ast_ctx, "fortfront_test_semantic.f90", semantic_works)
        
        ! Clean up
        open(unit=99, file="fortfront_test_semantic.f90", status="old")
        close(99, status="delete")
        
        if (semantic_works) then
            print *, "    ✅ Semantic analysis: WORKING"
        else
            print *, "    ❌ Semantic analysis: NOT WORKING (needs implementation)"
        end if
        
    end subroutine test_semantic_analysis
    
    subroutine test_type_inference()
        type(fluff_ast_context_t) :: ast_ctx
        character(len=:), allocatable :: test_code
        logical :: type_inference_works
        
        print *, "  🔍 Testing type inference..."
        
        test_code = "program type_test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    real :: a" // new_line('a') // &
                   "    double precision :: b" // new_line('a') // &
                   "    a = 3.14" // new_line('a') // &
                   "    b = 2.71828d0" // new_line('a') // &
                   "    ! Mixed precision operation" // new_line('a') // &
                   "    a = a + b" // new_line('a') // &
                   "end program type_test"
        
        ! Create test file
        open(unit=99, file="fortfront_test_types.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        type_inference_works = .false.
        call test_type_inference_features(ast_ctx, "fortfront_test_types.f90", type_inference_works)
        
        ! Clean up
        open(unit=99, file="fortfront_test_types.f90", status="old")
        close(99, status="delete")
        
        if (type_inference_works) then
            print *, "    ✅ Type inference: WORKING"
        else
            print *, "    ❌ Type inference: NOT WORKING (needs implementation)"
        end if
        
    end subroutine test_type_inference
    
    subroutine test_symbol_table_access()
        type(fluff_ast_context_t) :: ast_ctx
        character(len=:), allocatable :: test_code
        logical :: symbol_table_works
        
        print *, "  📚 Testing symbol table access..."
        
        test_code = "program symbol_test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: used_var, unused_var" // new_line('a') // &
                   "    used_var = 42" // new_line('a') // &
                   "    print *, used_var" // new_line('a') // &
                   "end program symbol_test"
        
        ! Create test file
        open(unit=99, file="fortfront_test_symbols.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        symbol_table_works = .false.
        call test_symbol_table_features(ast_ctx, "fortfront_test_symbols.f90", symbol_table_works)
        
        ! Clean up
        open(unit=99, file="fortfront_test_symbols.f90", status="old")
        close(99, status="delete")
        
        if (symbol_table_works) then
            print *, "    ✅ Symbol table access: WORKING"
        else
            print *, "    ❌ Symbol table access: NOT WORKING (needs implementation)"
        end if
        
    end subroutine test_symbol_table_access
    
    subroutine test_rule_integration()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: violations_found
        logical :: rules_working
        
        print *, "  ⚖️ Testing rule integration with fortfront..."
        
        ! Test code that should trigger multiple rules
        test_code = "program rule_test" // new_line('a') // &
                   "    integer :: i, unused_var" // new_line('a') // &  ! F001: no implicit none, F006: unused
                   "    i = 42" // new_line('a') // &
                   "    print *, undefined_var" // new_line('a') // &     ! F007/C001: undefined
                   "end program rule_test"
        
        ! Create test file
        open(unit=99, file="fortfront_test_rules.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        linter = create_linter_engine()
        call linter%lint_file("fortfront_test_rules.f90", diagnostics, error_msg)
        
        violations_found = 0
        if (allocated(diagnostics)) then
            violations_found = size(diagnostics)
        end if
        
        ! Clean up
        open(unit=99, file="fortfront_test_rules.f90", status="old")
        close(99, status="delete")
        
        rules_working = violations_found > 0
        
        if (rules_working) then
            print '(A,I0,A)', "    ✅ Rule integration: WORKING (", violations_found, " violations detected)"
        else
            print *, "    ❌ Rule integration: NOT WORKING (no violations detected, using stubs)"
        end if
        
    end subroutine test_rule_integration
    
    ! Helper subroutines for testing specific fortfront features
    
    subroutine test_ast_context_creation(ast_ctx, filename, success)
        type(fluff_ast_context_t), intent(out) :: ast_ctx
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        
        success = .false.
        
        ! Try to use fortfront API to parse file
        ! This would be something like: call ast_ctx%from_source(filename)
        ! For now, we check if the AST context type has the expected structure
        
        ! If fortfront API is working, we should be able to:
        ! 1. Parse the source file
        ! 2. Access the AST nodes
        ! 3. Get type information
        
        ! For now, this is a placeholder
        ! TODO: Implement actual fortfront API calls
        success = .false.  ! Will be true when fortfront API is integrated
        
    end subroutine test_ast_context_creation
    
    subroutine test_semantic_analysis_features(ast_ctx, filename, success)
        type(fluff_ast_context_t), intent(inout) :: ast_ctx
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        
        success = .false.
        
        ! Test semantic analysis features:
        ! 1. Variable scope resolution
        ! 2. Type checking
        ! 3. Symbol table construction
        ! 4. Usage tracking
        
        ! TODO: Implement actual semantic analysis tests
        success = .false.  ! Will be true when fortfront semantic analyzer is available
        
    end subroutine test_semantic_analysis_features
    
    subroutine test_type_inference_features(ast_ctx, filename, success)
        type(fluff_ast_context_t), intent(inout) :: ast_ctx
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        
        success = .false.
        
        ! Test type inference:
        ! 1. Expression type inference
        ! 2. Implicit conversion detection
        ! 3. Type compatibility checking
        
        ! TODO: Implement actual type inference tests
        success = .false.  ! Will be true when fortfront type system is available
        
    end subroutine test_type_inference_features
    
    subroutine test_symbol_table_features(ast_ctx, filename, success)
        type(fluff_ast_context_t), intent(inout) :: ast_ctx
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        
        success = .false.
        
        ! Test symbol table access:
        ! 1. Variable declaration lookup
        ! 2. Usage tracking
        ! 3. Scope resolution
        
        ! TODO: Implement actual symbol table tests
        success = .false.  ! Will be true when fortfront symbol tables are accessible
        
    end subroutine test_symbol_table_features
    
end program test_fortfront_api_capabilities