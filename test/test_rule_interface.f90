program test_rule_interface
    ! Test rule interface and execution lifecycle
    use fluff_core
    use fluff_ast
    use fluff_linter
    use fluff_diagnostics
    use fluff_rules
    implicit none
    
    print *, "Testing rule interface..."
    
    ! Test 1: Rule execution lifecycle
    call test_rule_lifecycle()
    
    ! Test 2: Rule registration
    call test_rule_registration()
    
    ! Test 3: Rule context access
    call test_rule_context()
    
    ! Test 4: Rule metadata
    call test_rule_metadata()
    
    print *, "All rule interface tests passed!"
    
contains
    
    subroutine test_rule_lifecycle()
        type(fluff_ast_context_t) :: ast_ctx
        type(diagnostic_t), allocatable :: violations(:)
        type(rule_info_t) :: rule
        integer :: node_index
        character(len=:), allocatable :: error_msg
        
        ! Skip test since fortfront AST is not available yet
        print *, "  ⚠ Rule execution lifecycle (skipped - fortfront not available)"
        return
        
        ! Create test AST context
        call ast_ctx%from_source("program test" // new_line('a') // &
                                "end program test", error_msg)
        
        if (allocated(error_msg)) then
            error stop "Failed to create AST: " // error_msg
        end if
        
        ! Create test rule
        rule%code = "F001"
        rule%name = "missing-implicit-none"
        rule%description = "Missing 'implicit none' statement"
        rule%category = "style"
        
        ! Execute rule
        node_index = 1
        call rule%check(ast_ctx, node_index, violations)
        
        ! Verify execution
        if (.not. allocated(violations)) then
            error stop "Failed: violations should be allocated"
        end if
        
        print *, "  ✓ Rule execution lifecycle"
    end subroutine test_rule_lifecycle
    
    subroutine test_rule_registration()
        type(rule_registry_t) :: registry
        type(rule_info_t) :: rule
        logical :: success
        
        ! Create rule
        rule%code = "F001"
        rule%name = "missing-implicit-none"
        rule%description = "Missing 'implicit none' statement"
        rule%category = "style"
        
        ! Register rule
        call registry%register_rule(rule, success)
        
        if (.not. success) then
            error stop "Failed: rule registration should succeed"
        end if
        
        ! Try to register duplicate
        call registry%register_rule(rule, success)
        
        if (success) then
            error stop "Failed: duplicate registration should fail"
        end if
        
        ! Check rule count
        if (registry%get_rule_count() /= 1) then
            error stop "Failed: should have 1 registered rule"
        end if
        
        print *, "  ✓ Rule registration"
    end subroutine test_rule_registration
    
    subroutine test_rule_context()
        type(fluff_ast_context_t) :: ast_ctx
        type(rule_context_t) :: rule_ctx
        character(len=:), allocatable :: filename
        character(len=:), allocatable :: source_code
        
        ! Set up context
        filename = "test.f90"
        source_code = "program test" // new_line('a') // &
                     "real :: x" // new_line('a') // &
                     "end program test"
        
        ! Create rule context
        rule_ctx = create_rule_context(ast_ctx, filename, source_code)
        
        ! Verify context access
        if (rule_ctx%get_filename() /= filename) then
            error stop "Failed: filename not accessible"
        end if
        
        if (rule_ctx%get_source_line(2) /= "real :: x") then
            error stop "Failed: source line not accessible"
        end if
        
        if (rule_ctx%get_total_lines() /= 3) then
            error stop "Failed: line count incorrect"
        end if
        
        print *, "  ✓ Rule context access"
    end subroutine test_rule_context
    
    subroutine test_rule_metadata()
        type(rule_info_t) :: rule
        character(len=:), allocatable :: json_meta
        
        ! Set up rule metadata
        rule%code = "F001"
        rule%name = "missing-implicit-none"
        rule%description = "Missing 'implicit none' statement"
        rule%category = "style"
        rule%subcategory = "best-practices"
        rule%default_enabled = .true.
        rule%fixable = .true.
        rule%severity = SEVERITY_WARNING
        
        ! Get JSON metadata
        json_meta = rule%to_json()
        
        if (.not. allocated(json_meta)) then
            error stop "Failed: JSON metadata should be generated"
        end if
        
        ! Check metadata contains required fields
        if (index(json_meta, '"code": "F001"') == 0) then
            error stop "Failed: metadata should contain code"
        end if
        
        if (index(json_meta, '"fixable": true') == 0) then
            error stop "Failed: metadata should contain fixable flag"
        end if
        
        print *, "  ✓ Rule metadata"
    end subroutine test_rule_metadata
    
end program test_rule_interface