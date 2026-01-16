program test_diagnostic_statistics
    ! REFACTOR: Test diagnostic statistics tracking
    use fluff_core
    use fluff_diagnostics
    implicit none
    
    print *, "Testing diagnostic statistics tracking..."
    
    ! Test 1: Basic statistics recording
    call test_basic_statistics_recording()
    
    ! Test 2: Statistics summary formatting
    call test_statistics_summary()
    
    ! Test 3: Collection statistics integration
    call test_collection_statistics()
    
    print *, "All diagnostic statistics tests passed!"
    
contains
    
    subroutine test_basic_statistics_recording()
        type(diagnostic_stats_t) :: stats
        
        print *, "  STATS Testing basic statistics recording..."
        
        ! Record some creation events
        call stats%record_creation(0.001)
        call stats%record_creation(0.002)
        call stats%record_creation(0.003)
        
        ! Record some formatting events  
        call stats%record_formatting(0.010)
        call stats%record_formatting(0.015)
        
        ! Record cache events
        call stats%record_cache_hit()
        call stats%record_cache_hit()
        call stats%record_cache_miss()
        
        ! Check recorded values
        if (stats%total_created /= 3) then
            error stop "Should have recorded 3 creations"
        end if
        
        if (stats%total_formatted /= 2) then
            error stop "Should have recorded 2 formatting operations"
        end if
        
        if (stats%cache_hits /= 2) then
            error stop "Should have recorded 2 cache hits"
        end if
        
        if (stats%cache_misses /= 1) then  
            error stop "Should have recorded 1 cache miss"
        end if
        
        if (abs(stats%total_creation_time - 0.006) > 0.0001) then
            error stop "Total creation time should be 0.006"
        end if
        
        if (abs(stats%total_formatting_time - 0.025) > 0.0001) then
            error stop "Total formatting time should be 0.025"
        end if
        
        print *, "    OK Basic statistics recording"
        
    end subroutine test_basic_statistics_recording
    
    subroutine test_statistics_summary()
        type(diagnostic_stats_t) :: stats
        character(len=:), allocatable :: summary
        
        print *, "  STATS Testing statistics summary formatting..."
        
        ! Record some events
        call stats%record_creation(0.002)
        call stats%record_creation(0.004)
        call stats%record_formatting(0.010)
        call stats%record_cache_hit()
        call stats%record_cache_miss()
        
        ! Get summary
        summary = stats%get_summary()
        
        ! Check that summary contains expected information
        if (index(summary, "Created: 2") == 0) then
            error stop "Summary should contain creation count"
        end if
        
        if (index(summary, "Formatted: 1") == 0) then
            error stop "Summary should contain formatting count"
        end if
        
        if (index(summary, "Cache hits: 1") == 0) then
            error stop "Summary should contain cache hit count"
        end if
        
        print *, "    OK Statistics summary formatting"
        
    end subroutine test_statistics_summary
    
    subroutine test_collection_statistics()
        type(diagnostic_collection_t) :: collection
        type(diagnostic_t) :: diagnostic
        type(source_range_t) :: location
        type(diagnostic_stats_t) :: stats
        character(len=:), allocatable :: summary
        
        print *, "  STATS Testing collection statistics integration..."
        
        ! Setup diagnostic
        location%start%line = 10
        location%start%column = 5
        location%end%line = 10
        location%end%column = 15
        
        diagnostic = create_diagnostic("F001", "Test diagnostic", "test.f90", &
                                     location, SEVERITY_WARNING)
        
        ! Add some diagnostics to trigger statistics
        call collection%add(diagnostic)
        call collection%add(diagnostic)
        
        ! Record some statistics manually for testing
        call collection%stats%record_creation(0.001)
        call collection%stats%record_formatting(0.005)
        call collection%stats%record_cache_hit()
        
        ! Get statistics from collection
        stats = collection%get_stats()
        
        if (stats%total_created /= 1) then
            error stop "Should have recorded creation statistics"
        end if
        
        if (stats%cache_hits /= 1) then
            error stop "Should have recorded cache hit"
        end if
        
        ! Get summary
        summary = stats%get_summary()
        
        if (len(summary) == 0) then
            error stop "Summary should not be empty"
        end if
        
        print *, "    OK Collection statistics integration"
        
    end subroutine test_collection_statistics
    
end program test_diagnostic_statistics
