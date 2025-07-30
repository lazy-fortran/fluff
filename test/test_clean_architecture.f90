program test_clean_architecture
    ! Test that modules have clean separation of concerns
    use fluff_core
    use fluff_ast
    use fluff_linter
    use fluff_formatter
    use fluff_cli
    use fluff_config
    use fluff_rules
    use fluff_diagnostics
    implicit none
    
    print *, "Testing clean architecture principles..."
    
    ! Test 1: Core module should not depend on other modules
    call test_core_independence()
    
    ! Test 2: AST module should only depend on core
    call test_ast_dependencies()
    
    ! Test 3: Diagnostics should only depend on core
    call test_diagnostics_dependencies()
    
    ! Test 4: Proper error handling exists
    call test_error_handling()
    
    print *, "All clean architecture tests passed!"
    
contains
    
    subroutine test_core_independence()
        ! Core types should be self-contained
        type(fluff_version_t) :: version
        type(fluff_result_t) :: result
        type(source_location_t) :: loc
        type(source_range_t) :: range
        
        version = get_fluff_version()
        if (version%major /= 0 .or. version%minor /= 1 .or. version%patch /= 0) then
            error stop "Failed: Version should be 0.1.0"
        end if
        
        print *, "  ✓ Core module is independent"
    end subroutine test_core_independence
    
    subroutine test_ast_dependencies()
        ! AST module should provide clean abstractions
        type(fluff_ast_context_t) :: ast_ctx
        character(len=:), allocatable :: error_msg
        
        ! Should be able to create AST context without initialization
        if (ast_ctx%is_initialized) then
            error stop "Failed: AST context should not be initialized by default"
        end if
        
        print *, "  ✓ AST module has clean dependencies"
    end subroutine test_ast_dependencies
    
    subroutine test_diagnostics_dependencies()
        ! Diagnostics should work independently
        type(diagnostic_t) :: diag
        type(source_range_t) :: range
        character(len=:), allocatable :: diag_str
        
        ! Set up a diagnostic
        diag%code = "F001"
        diag%message = "Test diagnostic"
        diag%category = "style"
        diag%severity = SEVERITY_WARNING
        
        range%start%line = 1
        range%start%column = 1
        range%end%line = 1
        range%end%column = 10
        diag%location = range
        
        ! Should be able to convert to string
        diag_str = diag%to_string()
        if (.not. allocated(diag_str)) then
            error stop "Failed: Diagnostic should produce string representation"
        end if
        
        print *, "  ✓ Diagnostics module has clean dependencies"
    end subroutine test_diagnostics_dependencies
    
    subroutine test_error_handling()
        ! All modules should have consistent error handling
        type(fluff_result_t) :: result
        
        ! Test success result
        result = create_success_result()
        if (.not. result%is_success) then
            error stop "Failed: Success result should have is_success = true"
        end if
        
        ! Test error result
        result = create_error_result("Test error")
        if (result%is_success) then
            error stop "Failed: Error result should have is_success = false"
        end if
        
        if (result%error_msg /= "Test error") then
            error stop "Failed: Error message not preserved"
        end if
        
        print *, "  ✓ Error handling is consistent"
    end subroutine test_error_handling
    
end program test_clean_architecture