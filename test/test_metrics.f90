program test_metrics
    ! Test performance metrics and statistics
    use fluff_core
    use fluff_metrics
    implicit none
    
    print *, "Testing performance metrics..."
    
    ! Test 1: Timer functionality
    call test_timer()
    
    ! Test 2: Rule statistics
    call test_rule_stats()
    
    ! Test 3: Metrics collector
    call test_metrics_collector()
    
    ! Test 4: Metrics reporting
    call test_metrics_report()
    
    print *, "All metrics tests passed!"
    
contains
    
    subroutine test_timer()
        type(timer_t) :: timer
        real :: elapsed
        
        ! Test basic timing
        call timer%start()
        call sleep_ms(1)  ! Sleep for 1ms (reduced from 10ms for speed)
        elapsed = timer%elapsed()
        
        if (elapsed <= 0.0) then
            error stop "Failed: timer should measure positive time"
        end if
        
        ! Test stop functionality
        call timer%stop()
        
        print *, "  OK Timer functionality"
        
    end subroutine test_timer
    
    subroutine test_rule_stats()
        type(rule_stats_t) :: stats
        
        stats%rule_code = "F001"
        
        ! Update statistics
        call stats%update(0.001, 2)
        call stats%update(0.002, 1)
        call stats%update(0.0015, 0)
        
        ! Verify statistics
        if (stats%execution_count /= 3) then
            error stop "Failed: execution count should be 3"
        end if
        
        if (stats%violation_count /= 3) then
            error stop "Failed: violation count should be 3"
        end if
        
        if (abs(stats%avg_time - 0.0015) > 0.0001) then
            error stop "Failed: average time incorrect"
        end if
        
        if (stats%min_time > 0.001 .or. stats%max_time < 0.002) then
            error stop "Failed: min/max times incorrect"
        end if
        
        ! Test reset
        call stats%reset()
        
        if (stats%execution_count /= 0) then
            error stop "Failed: stats should be reset"
        end if
        
        print *, "  OK Rule statistics"
        
    end subroutine test_rule_stats
    
    subroutine test_metrics_collector()
        type(metrics_collector_t) :: collector
        type(timer_t) :: timer
        type(rule_stats_t), pointer :: stats
        
        collector = create_metrics_collector()
        
        ! Simulate rule executions (reduced sleep times for speed)
        call collector%start_rule("F001", timer)
        call sleep_ms(1)
        call collector%end_rule("F001", timer, 2)

        call collector%start_rule("F002", timer)
        call sleep_ms(1)
        call collector%end_rule("F002", timer, 0)

        call collector%start_rule("F001", timer)
        call sleep_ms(1)
        call collector%end_rule("F001", timer, 1)
        
        ! Verify collector state
        if (collector%rule_count /= 2) then
            error stop "Failed: should have 2 rules"
        end if
        
        if (collector%total_violations /= 3) then
            error stop "Failed: should have 3 total violations"
        end if
        
        ! Check individual rule stats
        stats => collector%get_stats("F001")
        
        if (.not. associated(stats)) then
            error stop "Failed: should find F001 stats"
        end if
        
        if (stats%execution_count /= 2) then
            error stop "Failed: F001 should have 2 executions"
        end if
        
        ! Test enable/disable
        call collector%disable()
        call collector%start_rule("F003", timer)
        call collector%end_rule("F003", timer, 1)
        
        if (collector%rule_count /= 2) then
            error stop "Failed: disabled collector should not record"
        end if
        
        print *, "  OK Metrics collector"
        
    end subroutine test_metrics_collector
    
    subroutine test_metrics_report()
        type(metrics_collector_t) :: collector
        type(timer_t) :: timer
        character(len=:), allocatable :: report
        
        collector = create_metrics_collector()
        
        ! Add some data
        call collector%start_rule("F001", timer)
        call collector%end_rule("F001", timer, 5)
        
        ! Generate report
        report = collector%report()
        
        if (.not. allocated(report)) then
            error stop "Failed: report should be generated"
        end if
        
        if (index(report, "Rule Execution Metrics") == 0) then
            error stop "Failed: report should contain header"
        end if
        
        if (index(report, "F001") == 0) then
            error stop "Failed: report should contain rule code"
        end if
        
        print *, "  OK Metrics reporting"
        
    end subroutine test_metrics_report
    
    ! Helper to sleep for milliseconds
    subroutine sleep_ms(ms)
        integer, intent(in) :: ms
        real :: start_time, current_time
        
        call cpu_time(start_time)
        do
            call cpu_time(current_time)
            if ((current_time - start_time) * 1000.0 >= real(ms)) exit
        end do
        
    end subroutine sleep_ms
    
end program test_metrics
