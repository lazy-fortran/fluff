program test_metrics_integration
    ! Test metrics integration with rule execution
    use fluff_core
    use fluff_linter
    use fluff_diagnostics
    use fluff_ast
    use fluff_config
    use fluff_metrics
    implicit none
    
    print *, "Testing metrics integration..."
    
    ! Test 1: Metrics collection during execution
    call test_execution_metrics()
    
    ! Test 2: Metrics report generation
    call test_metrics_report()
    
    ! Test 3: Metrics reset
    call test_metrics_reset()
    
    print *, "[OK] All metrics integration tests passed!"
    
contains
    
    subroutine test_execution_metrics()
        type(linter_engine_t) :: linter
        type(rule_registry_t) :: registry
        character(len=:), allocatable :: report
        
        ! Initialize linter
        linter = create_linter_engine()
        
        ! Get metrics report (should have no executions yet)
        report = linter%rule_registry%get_metrics_report()
        
        if (.not. allocated(report)) then
            error stop "Failed: metrics report should be available"
        end if
        
        if (index(report, "Rules executed: 0") == 0) then
            error stop "Failed: should show 0 rules executed initially"
        end if
        
        print *, "[OK] Metrics collection during execution"
        
    end subroutine test_execution_metrics
    
    subroutine test_metrics_report()
        type(rule_registry_t) :: registry
        character(len=:), allocatable :: report
        
        ! Initialize registry with metrics
        registry%metrics = create_metrics_collector()
        call registry%discover_builtin_rules()
        
        ! Get report
        report = registry%get_metrics_report()
        
        if (index(report, "Rule Execution Metrics") == 0) then
            error stop "Failed: report should contain header"
        end if
        
        print *, "[OK] Metrics report generation"
        
    end subroutine test_metrics_report
    
    subroutine test_metrics_reset()
        type(rule_registry_t) :: registry
        type(timer_t) :: timer
        character(len=:), allocatable :: report
        
        ! Initialize and add some metrics
        registry%metrics = create_metrics_collector()
        
        ! Simulate execution
        call registry%metrics%start_rule("TEST", timer)
        call registry%metrics%end_rule("TEST", timer, 5)
        
        ! Reset metrics
        call registry%reset_metrics()
        
        ! Get report after reset
        report = registry%get_metrics_report()
        
        if (index(report, "Total violations found: 0") == 0) then
            error stop "Failed: metrics should be reset"
        end if
        
        print *, "[OK] Metrics reset"
        
    end subroutine test_metrics_reset
    
end program test_metrics_integration
