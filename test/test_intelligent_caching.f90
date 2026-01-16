program test_intelligent_caching
    use fluff_core
    use fluff_analysis_cache
    use fluff_string_utils
    implicit none
    
    integer :: total_tests, passed_tests
    
    print *, "=== Intelligent Caching Test Suite ==="
    
    total_tests = 0
    passed_tests = 0
    
    ! Test cache functionality
    call test_cache_creation()
    call test_cache_invalidation()
    call test_cache_persistence()
    call test_cache_performance()
    call test_dependency_tracking()
    call test_cache_compression()
    call test_cache_management()
    call test_cache_statistics()
    
    print *, ""
    print *, "=== Intelligent Caching Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"
    
    if (passed_tests == total_tests) then
        print *, "[OK] All intelligent caching tests passed!"
    else
        print *, "[FAIL] Some tests failed (TDD stubs in progress)"
    end if
    
contains
    
    subroutine test_cache_creation()
        print *, ""
        print *, "Testing cache creation..."
        
        ! Test 1: Create basic cache
        call run_cache_test("Basic cache creation", &
            test_create_basic_cache, .true.)
        
        ! Test 2: Create cache with custom directory
        call run_cache_test("Custom cache directory", &
            test_create_cache_with_dir, .true.)
        
        ! Test 3: Create cache with configuration
        call run_cache_test("Cache with configuration", &
            test_create_configured_cache, .true.)
        
        ! Test 4: Invalid cache directory
        call run_cache_test("Invalid cache directory", &
            test_invalid_cache_dir, .true.)
        
    end subroutine test_cache_creation
    
    subroutine test_cache_invalidation()
        print *, ""
        print *, "Testing cache invalidation..."
        
        ! Test 1: Invalidate single file
        call run_cache_test("Invalidate single file", &
            test_invalidate_single_file, .true.)
        
        ! Test 2: Invalidate by pattern
        call run_cache_test("Invalidate by pattern", &
            test_invalidate_by_pattern, .true.)
        
        ! Test 3: Invalidate all cache
        call run_cache_test("Invalidate all cache", &
            test_invalidate_all_cache, .true.)
        
        ! Test 4: Invalidate on dependency change
        call run_cache_test("Invalidate on dependency change", &
            test_invalidate_on_dependency, .true.)
        
        ! Test 5: Selective invalidation
        call run_cache_test("Selective invalidation", &
            test_selective_invalidation, .true.)
        
        ! Test 6: Time-based invalidation
        call run_cache_test("Time-based invalidation", &
            test_time_based_invalidation, .true.)
        
    end subroutine test_cache_invalidation
    
    subroutine test_cache_persistence()
        print *, ""
        print *, "Testing cache persistence..."
        
        ! Test 1: Save cache to disk
        call run_cache_test("Save cache to disk", &
            test_save_cache_to_disk, .true.)
        
        ! Test 2: Load cache from disk
        call run_cache_test("Load cache from disk", &
            test_load_cache_from_disk, .true.)
        
        ! Test 3: Cache persistence across sessions
        call run_cache_test("Persistence across sessions", &
            test_cache_across_sessions, .true.)
        
        ! Test 4: Handle corrupted cache files
        call run_cache_test("Handle corrupted cache", &
            test_corrupted_cache_files, .true.)
        
        ! Test 5: Cache file versioning
        call run_cache_test("Cache file versioning", &
            test_cache_versioning, .true.)
        
        ! Test 6: Atomic cache updates
        call run_cache_test("Atomic cache updates", &
            test_atomic_cache_updates, .true.)
        
    end subroutine test_cache_persistence
    
    subroutine test_cache_performance()
        print *, ""
        print *, "Testing cache performance..."
        
        ! Test 1: Cache hit performance
        call run_cache_test("Cache hit performance", &
            test_cache_hit_performance, .true.)
        
        ! Test 2: Cache miss performance
        call run_cache_test("Cache miss performance", &
            test_cache_miss_performance, .true.)
        
        ! Test 3: Cache size vs performance
        call run_cache_test("Cache size vs performance", &
            test_cache_size_performance, .true.)
        
        ! Test 4: Memory usage optimization
        call run_cache_test("Memory usage optimization", &
            test_memory_optimization, .true.)
        
        ! Test 5: Cache eviction performance
        call run_cache_test("Cache eviction performance", &
            test_eviction_performance, .true.)
        
        ! Test 6: Concurrent cache access
        call run_cache_test("Concurrent cache access", &
            test_concurrent_access, .true.)
        
    end subroutine test_cache_performance
    
    subroutine test_dependency_tracking()
        print *, ""
        print *, "Testing file dependency tracking..."
        
        ! Test 1: Track simple dependencies
        call run_cache_test("Track simple dependencies", &
            test_track_simple_dependencies, .true.)
        
        ! Test 2: Track transitive dependencies
        call run_cache_test("Track transitive dependencies", &
            test_track_transitive_deps, .true.)
        
        ! Test 3: Update dependency graph
        call run_cache_test("Update dependency graph", &
            test_update_dependency_graph, .true.)
        
        ! Test 4: Detect circular dependencies
        call run_cache_test("Detect circular dependencies", &
            test_detect_circular_deps, .true.)
        
        ! Test 5: Dependency-based invalidation
        call run_cache_test("Dependency-based invalidation", &
            test_dependency_invalidation, .true.)
        
        ! Test 6: Cross-file dependency tracking
        call run_cache_test("Cross-file dependencies", &
            test_cross_file_dependencies, .true.)
        
    end subroutine test_dependency_tracking
    
    subroutine test_cache_compression()
        print *, ""
        print *, "Testing cache compression..."
        
        ! Test 1: Basic compression
        call run_cache_test("Basic compression", &
            test_basic_compression, .true.)
        
        ! Test 2: Compression ratio testing
        call run_cache_test("Compression ratio", &
            test_compression_ratio, .true.)
        
        ! Test 3: Compression performance
        call run_cache_test("Compression performance", &
            test_compression_performance, .true.)
        
        ! Test 4: Decompression accuracy
        call run_cache_test("Decompression accuracy", &
            test_decompression_accuracy, .true.)
        
        ! Test 5: Adaptive compression
        call run_cache_test("Adaptive compression", &
            test_adaptive_compression, .true.)
        
    end subroutine test_cache_compression
    
    subroutine test_cache_management()
        print *, ""
        print *, "Testing cache management..."
        
        ! Test 1: Cache size limits
        call run_cache_test("Cache size limits", &
            test_cache_size_limits, .true.)
        
        ! Test 2: LRU eviction policy
        call run_cache_test("LRU eviction policy", &
            test_lru_eviction, .true.)
        
        ! Test 3: Cache cleanup
        call run_cache_test("Cache cleanup", &
            test_cache_cleanup, .true.)
        
        ! Test 4: Cache defragmentation
        call run_cache_test("Cache defragmentation", &
            test_cache_defragmentation, .true.)
        
    end subroutine test_cache_management
    
    subroutine test_cache_statistics()
        print *, ""
        print *, "Testing cache statistics..."
        
        ! Test 1: Cache hit/miss ratios
        call run_cache_test("Hit/miss ratios", &
            test_hit_miss_ratios, .true.)
        
        ! Test 2: Cache usage statistics
        call run_cache_test("Usage statistics", &
            test_usage_statistics, .true.)
        
        ! Test 3: Performance metrics
        call run_cache_test("Performance metrics", &
            test_performance_metrics, .true.)
        
        ! Test 4: Cache efficiency analysis
        call run_cache_test("Efficiency analysis", &
            test_efficiency_analysis, .true.)
        
    end subroutine test_cache_statistics
    
    ! Helper subroutine for running tests
    subroutine run_cache_test(test_name, test_proc, should_succeed)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: should_succeed
        
        interface
            function test_proc() result(success)
                logical :: success
            end function test_proc
        end interface
        
        logical :: success
        
        total_tests = total_tests + 1
        success = test_proc()
        
        if (success .eqv. should_succeed) then
            print *, "  PASS: ", test_name
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name
        end if
        
    end subroutine run_cache_test
    
    ! Individual test functions (should fail in RED phase)
    function test_create_basic_cache() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        
        cache = create_analysis_cache()
        success = cache%is_initialized()
        
    end function test_create_basic_cache
    
    function test_create_cache_with_dir() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        
        cache = create_analysis_cache("/tmp/fluff_cache")
        success = cache%is_initialized() .and. cache%get_cache_dir() == "/tmp/fluff_cache"
        
    end function test_create_cache_with_dir
    
    function test_create_configured_cache() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(cache_config_t) :: config
        
        config%max_size_mb = 100
        config%max_entries = 1000
        config%compression_enabled = .true.
        
        cache = create_analysis_cache(config=config)
        success = cache%is_initialized()
        
    end function test_create_configured_cache
    
    function test_invalid_cache_dir() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        
        cache = create_analysis_cache("/invalid/readonly/path")
        success = .not. cache%is_initialized()
        
    end function test_invalid_cache_dir
    
    function test_invalidate_single_file() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(analysis_result_t) :: result
        
        cache = create_analysis_cache()
        result%file_path = "test.f90"
        
        call cache%store_analysis("test.f90", result)
        call cache%invalidate_cache("test.f90")
        
        success = .not. cache%has_cached_analysis("test.f90")
        
    end function test_invalidate_single_file
    
    function test_invalidate_by_pattern() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(analysis_result_t) :: result1, result2
        
        cache = create_analysis_cache()
        result1%file_path = "test1.f90"
        result2%file_path = "test2.f90"
        
        call cache%store_analysis("test1.f90", result1)
        call cache%store_analysis("test2.f90", result2)
        call cache%invalidate_by_pattern("*.f90")
        
        success = .not. cache%has_cached_analysis("test1.f90") .and. &
                 .not. cache%has_cached_analysis("test2.f90")
        
    end function test_invalidate_by_pattern
    
    function test_invalidate_all_cache() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(analysis_result_t) :: result
        integer :: initial_count, final_count
        
        cache = create_analysis_cache()
        result%file_path = "test.f90"
        
        call cache%store_analysis("test.f90", result)
        initial_count = cache%get_entry_count()
        
        call cache%invalidate_all()
        final_count = cache%get_entry_count()
        
        success = initial_count > 0 .and. final_count == 0
        
    end function test_invalidate_all_cache
    
    function test_invalidate_on_dependency() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(analysis_result_t) :: result
        
        cache = create_analysis_cache()
        result%file_path = "dependent.f90"
        
        call cache%store_analysis("dependent.f90", result)
        call cache%add_dependency("dependent.f90", "module.f90")
        call cache%invalidate_cache("module.f90")
        
        success = .not. cache%has_cached_analysis("dependent.f90")
        
    end function test_invalidate_on_dependency
    
    function test_selective_invalidation() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(analysis_result_t) :: result1, result2
        
        cache = create_analysis_cache()
        result1%file_path = "keep.f90"
        result2%file_path = "remove.f90"
        
        call cache%store_analysis("keep.f90", result1)
        call cache%store_analysis("remove.f90", result2)
        call cache%invalidate_cache("remove.f90")
        
        success = cache%has_cached_analysis("keep.f90") .and. &
                 .not. cache%has_cached_analysis("remove.f90")
        
    end function test_selective_invalidation
    
    function test_time_based_invalidation() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(analysis_result_t) :: result
        
        cache = create_analysis_cache()
        result%file_path = "old.f90"
        
        call cache%store_analysis("old.f90", result)
        ! Make the entry appear old by waiting or simulating old timestamps
        call cache%simulate_old_entry("old.f90", 7200)  ! 2 hours old
        call cache%invalidate_older_than(3600)  ! 1 hour
        
        success = .not. cache%has_cached_analysis("old.f90")
        
    end function test_time_based_invalidation
    
    function test_save_cache_to_disk() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(analysis_result_t) :: result
        
        cache = create_analysis_cache()
        result%file_path = "test.f90"
        
        call cache%store_analysis("test.f90", result)
        call cache%save_to_disk()
        
        success = cache%is_saved_to_disk()
        
    end function test_save_cache_to_disk
    
    function test_load_cache_from_disk() result(success)
        logical :: success
        type(analysis_cache_t) :: cache1, cache2
        type(analysis_result_t) :: result
        
        cache1 = create_analysis_cache()
        result%file_path = "test.f90"
        
        call cache1%store_analysis("test.f90", result)
        call cache1%save_to_disk()
        
        cache2 = create_analysis_cache()
        call cache2%load_from_disk()
        
        success = cache2%has_cached_analysis("test.f90")
        
    end function test_load_cache_from_disk
    
    function test_cache_across_sessions() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        logical :: exists_before, exists_after
        character(len=50) :: unique_dir
        integer :: time_val, unit, iostat
        
        ! Create cache with unique file in /tmp
        call system_clock(time_val)
        cache = create_analysis_cache("/tmp")
        exists_before = cache%cache_file_exists()
        
        call cache%create_persistent_cache()
        exists_after = cache%cache_file_exists()
        
        success = .not. exists_before .and. exists_after
        
        ! Clean up test cache file  
        if (exists_after .and. allocated(cache%cache_file_path)) then
            open(newunit=unit, file=cache%cache_file_path, iostat=iostat)
            if (iostat == 0) close(unit, status='delete')
        end if
        
    end function test_cache_across_sessions
    
    function test_corrupted_cache_files() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        
        cache = create_analysis_cache()
        call cache%simulate_corruption()
        call cache%load_from_disk()
        
        success = cache%is_initialized()  ! Should handle corruption gracefully
        
    end function test_corrupted_cache_files
    
    function test_cache_versioning() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        integer :: version
        
        cache = create_analysis_cache()
        version = cache%get_cache_version()
        
        success = version > 0
        
    end function test_cache_versioning
    
    function test_atomic_cache_updates() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        
        cache = create_analysis_cache()
        call cache%begin_atomic_update()
        call cache%commit_atomic_update()
        
        success = cache%is_consistent()
        
    end function test_atomic_cache_updates
    
    function test_cache_hit_performance() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(analysis_result_t) :: result
        real :: hit_time
        
        cache = create_analysis_cache()
        result%file_path = "test.f90"
        
        call cache%store_analysis("test.f90", result)
        hit_time = cache%measure_cache_hit_time("test.f90")
        
        success = hit_time > 0.0 .and. hit_time < 10.0  ! Should be fast
        
    end function test_cache_hit_performance
    
    function test_cache_miss_performance() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        real :: miss_time
        
        cache = create_analysis_cache()
        miss_time = cache%measure_cache_miss_time("nonexistent.f90")
        
        success = miss_time > 0.0
        
    end function test_cache_miss_performance
    
    function test_cache_size_performance() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(cache_performance_t) :: perf
        
        cache = create_analysis_cache()
        call cache%populate_test_data(1000)  ! 1000 entries
        
        perf = cache%benchmark_performance()
        success = perf%avg_lookup_time < 5.0  ! milliseconds
        
    end function test_cache_size_performance
    
    function test_memory_optimization() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        integer :: memory_before, memory_after
        
        cache = create_analysis_cache()
        memory_before = cache%get_memory_usage()
        
        call cache%optimize_memory()
        memory_after = cache%get_memory_usage()
        
        success = memory_after <= memory_before
        
    end function test_memory_optimization
    
    function test_eviction_performance() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        real :: eviction_time
        
        cache = create_analysis_cache()
        call cache%fill_to_capacity()
        
        eviction_time = cache%measure_eviction_time()
        success = eviction_time < 100.0  ! milliseconds
        
    end function test_eviction_performance
    
    function test_concurrent_access() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        
        cache = create_analysis_cache()
        call cache%enable_thread_safety(.true.)
        
        success = cache%test_concurrent_access()
        
    end function test_concurrent_access
    
    function test_track_simple_dependencies() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(string_array_t) :: deps
        
        cache = create_analysis_cache()
        call cache%add_dependency("main.f90", "module.f90")
        
        deps = cache%get_dependencies("main.f90")
        success = deps%count > 0
        
    end function test_track_simple_dependencies
    
    function test_track_transitive_deps() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(string_array_t) :: deps
        
        cache = create_analysis_cache()
        call cache%add_dependency("main.f90", "module1.f90")
        call cache%add_dependency("module1.f90", "module2.f90")
        
        deps = cache%get_transitive_dependencies("main.f90")
        success = deps%count >= 2  ! Should include both module1.f90 and module2.f90
        
    end function test_track_transitive_deps
    
    function test_update_dependency_graph() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        integer :: node_count_before, node_count_after
        
        cache = create_analysis_cache()
        node_count_before = cache%get_dependency_node_count()
        
        call cache%add_dependency("new.f90", "dep.f90")
        node_count_after = cache%get_dependency_node_count()
        
        success = node_count_after > node_count_before
        
    end function test_update_dependency_graph
    
    function test_detect_circular_deps() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        
        cache = create_analysis_cache()
        call cache%add_dependency("a.f90", "b.f90")
        call cache%add_dependency("b.f90", "a.f90")
        
        success = cache%has_circular_dependencies()
        
    end function test_detect_circular_deps
    
    function test_dependency_invalidation() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(analysis_result_t) :: result
        
        cache = create_analysis_cache()
        result%file_path = "dependent.f90"
        
        call cache%store_analysis("dependent.f90", result)
        call cache%add_dependency("dependent.f90", "dep.f90")
        call cache%file_changed("dep.f90")
        
        success = .not. cache%has_cached_analysis("dependent.f90")
        
    end function test_dependency_invalidation
    
    function test_cross_file_dependencies() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(string_array_t) :: affected_files
        
        cache = create_analysis_cache()
        call cache%add_dependency("file1.f90", "common.f90")
        call cache%add_dependency("file2.f90", "common.f90")
        
        affected_files = cache%get_files_depending_on("common.f90")
        success = affected_files%count >= 2  ! Should find both file1.f90 and file2.f90
        
    end function test_cross_file_dependencies
    
    function test_basic_compression() result(success)
        logical :: success
        type(analysis_cache_t) :: cache1, cache2
        type(analysis_result_t) :: result1, result2
        integer :: compressed_size, uncompressed_size
        
        ! Test compression by comparing compressed vs uncompressed storage
        cache1 = create_analysis_cache()
        cache2 = create_analysis_cache()
        result1%file_path = "large_file.f90"
        result2%file_path = "large_file.f90"
        
        call cache1%store_analysis_compressed("large_file.f90", result1)
        compressed_size = cache1%get_storage_size()
        
        call cache2%store_analysis("large_file.f90", result2)
        uncompressed_size = cache2%get_storage_size()
        
        success = compressed_size < uncompressed_size  ! Compression reduced size
        
    end function test_basic_compression
    
    function test_compression_ratio() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        real :: ratio
        
        cache = create_analysis_cache()
        call cache%populate_compressible_data()
        
        ratio = cache%get_compression_ratio()
        success = ratio > 1.1  ! At least 10% compression
        
    end function test_compression_ratio
    
    function test_compression_performance() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        real :: compress_time, decompress_time
        
        cache = create_analysis_cache()
        
        compress_time = cache%measure_compression_time()
        decompress_time = cache%measure_decompression_time()
        
        success = compress_time < 100.0 .and. decompress_time < 50.0  ! milliseconds
        
    end function test_compression_performance
    
    function test_decompression_accuracy() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(analysis_result_t) :: original, recovered
        
        cache = create_analysis_cache()
        original%file_path = "test.f90"
        
        call cache%store_analysis_compressed("test.f90", original)
        call cache%get_cached_analysis("test.f90", recovered)
        
        success = original%file_path == recovered%file_path
        
    end function test_decompression_accuracy
    
    function test_adaptive_compression() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        logical :: uses_compression
        
        cache = create_analysis_cache()
        call cache%enable_adaptive_compression(.true.)
        
        uses_compression = cache%should_compress_entry("large_data")
        success = uses_compression
        
    end function test_adaptive_compression
    
    function test_cache_size_limits() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        integer :: max_size, current_size
        
        cache = create_analysis_cache()
        call cache%set_max_size(1024)  ! 1MB
        
        max_size = cache%get_max_size()
        current_size = cache%get_current_size()
        
        success = current_size <= max_size
        
    end function test_cache_size_limits
    
    function test_lru_eviction() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(analysis_result_t) :: result
        
        cache = create_analysis_cache()
        call cache%set_eviction_policy("LRU")
        
        ! Fill cache beyond capacity
        call cache%fill_beyond_capacity()
        
        success = .not. cache%has_cached_analysis("oldest_entry.f90")
        
    end function test_lru_eviction
    
    function test_cache_cleanup() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        integer :: entries_before, entries_after
        
        cache = create_analysis_cache()
        call cache%populate_test_data(100)
        entries_before = cache%get_entry_count()
        
        call cache%cleanup()
        entries_after = cache%get_entry_count()
        
        success = entries_after <= entries_before
        
    end function test_cache_cleanup
    
    function test_cache_defragmentation() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        real :: fragmentation_before, fragmentation_after
        
        cache = create_analysis_cache()
        call cache%create_fragmentation()
        fragmentation_before = cache%get_fragmentation_ratio()
        
        call cache%defragment()
        fragmentation_after = cache%get_fragmentation_ratio()
        
        success = fragmentation_after < fragmentation_before
        
    end function test_cache_defragmentation
    
    function test_hit_miss_ratios() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(cache_statistics_t) :: stats
        
        cache = create_analysis_cache()
        call cache%simulate_cache_usage(100, 70)  ! 100 requests, 70 hits
        
        stats = cache%get_statistics()
        success = abs(stats%hit_ratio - 0.7) < 0.01  ! 70% hit rate
        
    end function test_hit_miss_ratios
    
    function test_usage_statistics() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(cache_statistics_t) :: stats
        
        cache = create_analysis_cache()
        call cache%simulate_cache_usage(50, 30)
        
        stats = cache%get_statistics()
        success = stats%total_requests == 50 .and. stats%cache_hits == 30
        
    end function test_usage_statistics
    
    function test_performance_metrics() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(cache_performance_t) :: perf
        
        cache = create_analysis_cache()
        call cache%run_performance_test()
        
        perf = cache%get_performance_metrics()
        success = perf%avg_lookup_time > 0.0
        
    end function test_performance_metrics
    
    function test_efficiency_analysis() result(success)
        logical :: success
        type(analysis_cache_t) :: cache
        type(cache_efficiency_t) :: efficiency
        
        cache = create_analysis_cache()
        call cache%analyze_efficiency()
        
        efficiency = cache%get_efficiency_analysis()
        success = efficiency%overall_efficiency >= 0.0 .and. efficiency%overall_efficiency <= 1.0
        
    end function test_efficiency_analysis
    
end program test_intelligent_caching
