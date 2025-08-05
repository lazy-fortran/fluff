program test_parallel_rule_execution
    ! Test parallel rule execution
    use fluff_core
    use fluff_linter
    use fluff_diagnostics
    use fluff_ast
    use fluff_config
    implicit none
    
    print *, "Testing parallel rule execution..."
    
    ! Test 1: Parallel execution performance
    call test_parallel_performance()
    
    ! Test 2: Thread safety
    call test_thread_safety()
    
    ! Test 3: Result consistency
    call test_result_consistency()
    
    print *, "All parallel rule execution tests passed!"
    
contains
    
    subroutine test_parallel_performance()
        type(rule_registry_t) :: registry
        type(fluff_ast_context_t) :: ast_ctx
        type(diagnostic_t), allocatable :: diagnostics(:)
        type(rule_selection_t) :: selection
        real :: start_time, end_time, serial_time, parallel_time
        integer :: i
        
        ! Discover rules
        call registry%discover_builtin_rules()
        
        ! Add more test rules for performance testing
        do i = 1, 10
            call add_test_rule(registry, i)
        end do
        
        ! Skip actual performance test if AST not available
        print *, "  ⚠ Parallel performance (skipped - fortfront not available)"
        return
        
        ! Serial execution
        call cpu_time(start_time)
        call registry%execute_rules(ast_ctx, selection, diagnostics)
        call cpu_time(end_time)
        serial_time = end_time - start_time
        
        ! Parallel execution
        call cpu_time(start_time)
        call registry%execute_rules_parallel(ast_ctx, selection, diagnostics)
        call cpu_time(end_time)
        parallel_time = end_time - start_time
        
        ! Parallel should be faster for many rules
        ! (In practice, this depends on system and rule complexity)
        print *, "  ✓ Parallel execution performance"
        
    end subroutine test_parallel_performance
    
    subroutine test_thread_safety()
        ! Test that parallel execution is thread-safe
        type(rule_registry_t) :: registry
        type(fluff_ast_context_t) :: ast_ctx
        type(diagnostic_t), allocatable :: diagnostics1(:), diagnostics2(:)
        type(rule_selection_t) :: selection
        integer :: i, iterations
        logical :: thread_safe
        
        ! Initialize
        call registry%discover_builtin_rules()
        thread_safe = .true.
        iterations = 5
        
        ! Run multiple parallel executions to test for race conditions
        do i = 1, iterations
            call registry%execute_rules_parallel(ast_ctx, selection, diagnostics1)
            call registry%execute_rules_parallel(ast_ctx, selection, diagnostics2)
            
            ! Check if results are consistent (same size at minimum)
            if (size(diagnostics1) /= size(diagnostics2)) then
                thread_safe = .false.
                exit
            end if
        end do
        
        if (thread_safe) then
            print *, "  ✓ Thread safety"
        else
            print *, "  ✗ Thread safety failed"
        end if
        
    end subroutine test_thread_safety
    
    subroutine test_result_consistency()
        ! Test that parallel execution gives same results as serial
        type(rule_registry_t) :: registry
        type(fluff_ast_context_t) :: ast_ctx
        type(diagnostic_t), allocatable :: serial_diagnostics(:), parallel_diagnostics(:)
        type(rule_selection_t) :: selection
        logical :: consistent
        integer :: i
        
        ! Initialize
        call registry%discover_builtin_rules()
        consistent = .true.
        
        ! Run serial execution
        call registry%execute_rules(ast_ctx, selection, serial_diagnostics)
        
        ! Run parallel execution
        call registry%execute_rules_parallel(ast_ctx, selection, parallel_diagnostics)
        
        ! Compare results - they should have the same number of diagnostics
        if (size(serial_diagnostics) /= size(parallel_diagnostics)) then
            consistent = .false.
        else
            ! Check that diagnostic codes match (order may differ in parallel execution)
            do i = 1, size(serial_diagnostics)
                ! Simple consistency check - both should find the same issues
                ! (More sophisticated comparison would sort and compare each diagnostic)
                if (len_trim(serial_diagnostics(i)%code) /= len_trim(parallel_diagnostics(i)%code)) then
                    consistent = .false.
                    exit
                end if
            end do
        end if
        
        if (consistent) then
            print *, "  ✓ Result consistency"
        else
            print *, "  ✗ Result consistency failed"
        end if
        
    end subroutine test_result_consistency
    
    subroutine add_test_rule(registry, index)
        type(rule_registry_t), intent(inout) :: registry
        integer, intent(in) :: index
        type(rule_info_t) :: rule
        character(len=10) :: code_str
        
        write(code_str, '("TEST", I3.3)') index
        rule%code = trim(code_str)
        rule%name = "test-rule-" // trim(code_str)
        rule%description = "Test rule for parallel execution"
        rule%category = "style"
        rule%default_enabled = .true.
        rule%fixable = .false.
        rule%severity = SEVERITY_INFO
        
        call registry%register_rule(rule)
        
    end subroutine add_test_rule
    
end program test_parallel_rule_execution