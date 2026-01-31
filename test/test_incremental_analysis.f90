program test_incremental_analysis
    use fluff_core
    use fluff_incremental_analyzer
    implicit none
    
    integer :: total_tests, passed_tests
    
    print *, "=== Incremental Analysis Test Suite ==="
    
    total_tests = 0
    passed_tests = 0
    
    ! Test incremental analysis functionality
    call test_analyzer_creation()
    call test_dependency_graph()
    call test_change_propagation()
    call test_incremental_linting()
    call test_result_caching()
    call test_performance_optimization()
    
    print *, ""
    print *, "=== Incremental Analysis Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"
    
    if (passed_tests == total_tests) then
        print *, "[OK] All incremental analysis tests passed!"
    else
        print *, "[FAIL] Some tests failed"
    end if
    
contains
    
    subroutine test_analyzer_creation()
        print *, ""
        print *, "Testing incremental analyzer creation..."
        
        ! Test 1: Create basic analyzer
        call run_analysis_test("Basic analyzer creation", &
            test_create_analyzer, .true.)
        
        ! Test 2: Create analyzer with configuration
        call run_analysis_test("Configured analyzer", &
            test_create_configured_analyzer, .true.)
        
        ! Test 3: Initialize dependency tracking
        call run_analysis_test("Dependency tracking init", &
            test_init_dependency_tracking, .true.)
        
    end subroutine test_analyzer_creation
    
    subroutine test_dependency_graph()
        print *, ""
        print *, "Testing dependency graph management..."
        
        ! Test 1: Build dependency graph
        call run_analysis_test("Build dependency graph", &
            test_build_dependency_graph, .true.)
        
        ! Test 2: Update dependencies
        call run_analysis_test("Update dependencies", &
            test_update_dependencies, .true.)
        
        ! Test 3: Detect circular dependencies
        call run_analysis_test("Circular dependency detection", &
            test_detect_circular_deps, .true.)
        
        ! Test 4: Resolve transitive dependencies
        call run_analysis_test("Transitive dependencies", &
            test_transitive_dependencies, .true.)
        
    end subroutine test_dependency_graph
    
    subroutine test_change_propagation()
        print *, ""
        print *, "Testing change propagation..."
        
        ! Test 1: Propagate single file change
        call run_analysis_test("Single file change", &
            test_single_file_change, .true.)
        
        ! Test 2: Propagate module changes
        call run_analysis_test("Module change propagation", &
            test_module_change_propagation, .true.)
        
        ! Test 3: Interface changes
        call run_analysis_test("Interface change propagation", &
            test_interface_change_propagation, .true.)
        
        ! Test 4: Configuration changes
        call run_analysis_test("Config change propagation", &
            test_config_change_propagation, .true.)
        
    end subroutine test_change_propagation
    
    subroutine test_incremental_linting()
        print *, ""
        print *, "Testing incremental linting..."
        
        ! Test 1: Lint only changed files
        call run_analysis_test("Lint changed files only", &
            test_lint_changed_only, .true.)
        
        ! Test 2: Lint dependent files
        call run_analysis_test("Lint dependent files", &
            test_lint_dependent_files, .true.)
        
        ! Test 3: Skip unchanged files
        call run_analysis_test("Skip unchanged files", &
            test_skip_unchanged, .true.)
        
        ! Test 4: Merge incremental results
        call run_analysis_test("Merge incremental results", &
            test_merge_results, .true.)
        
    end subroutine test_incremental_linting
    
    subroutine test_result_caching()
        print *, ""
        print *, "Testing result caching..."
        
        ! Test 1: Cache analysis results
        call run_analysis_test("Cache analysis results", &
            test_cache_results, .true.)
        
        ! Test 2: Invalidate cache on change
        call run_analysis_test("Cache invalidation", &
            test_cache_invalidation, .true.)
        
        ! Test 3: Cache hit performance
        call run_analysis_test("Cache hit performance", &
            test_cache_performance, .true.)
        
        ! Test 4: Cache memory management
        call run_analysis_test("Cache memory management", &
            test_cache_memory, .true.)
        
    end subroutine test_result_caching
    
    subroutine test_performance_optimization()
        print *, ""
        print *, "Testing performance optimization..."
        
        ! Test 1: Parallel analysis
        call run_analysis_test("Parallel analysis", &
            test_parallel_analysis, .true.)
        
        ! Test 2: Work scheduling
        call run_analysis_test("Work scheduling", &
            test_work_scheduling, .true.)
        
        ! Test 3: Resource management
        call run_analysis_test("Resource management", &
            test_resource_management, .true.)
        
    end subroutine test_performance_optimization
    
    ! Helper subroutine for running tests
    subroutine run_analysis_test(test_name, test_proc, should_succeed)
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
            print *, "[OK] ", test_name
            passed_tests = passed_tests + 1
        else
            print *, "[FAIL] ", test_name
        end if
        
    end subroutine run_analysis_test
    
    ! Individual test functions (should fail in RED phase)
    function test_create_analyzer() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        
        analyzer = create_incremental_analyzer()
        success = analyzer%is_initialized()
        
    end function test_create_analyzer
    
    function test_create_configured_analyzer() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        type(incremental_config_t) :: config
        
        config%enable_caching = .true.
        config%max_cache_size = 1000
        config%enable_parallel = .true.
        
        analyzer = create_incremental_analyzer(config)
        success = analyzer%is_initialized()
        
    end function test_create_configured_analyzer
    
    function test_init_dependency_tracking() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        
        analyzer = create_incremental_analyzer()
        call analyzer%initialize_dependency_tracking()
        
        success = analyzer%is_dependency_tracking_enabled()
        
    end function test_init_dependency_tracking
    
    function test_build_dependency_graph() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        character(len=:), allocatable :: files(:)
        
        analyzer = create_incremental_analyzer()
        files = [character(len=20) :: "module1.f90", "module2.f90", "main.f90"]
        
        call analyzer%build_dependency_graph(files)
        success = analyzer%get_node_count() == 3
        
    end function test_build_dependency_graph
    
    function test_update_dependencies() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        
        analyzer = create_incremental_analyzer()
        call analyzer%build_dependency_graph([character(len=20) :: "test.f90"])
        
        call analyzer%update_dependencies("test.f90")
        success = analyzer%is_up_to_date("test.f90")
        
    end function test_update_dependencies
    
    function test_detect_circular_deps() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        logical :: has_cycles
        
        analyzer = create_incremental_analyzer()
        call analyzer%add_dependency("a.f90", "b.f90")
        call analyzer%add_dependency("b.f90", "a.f90")  ! Circular
        
        has_cycles = analyzer%has_circular_dependencies()
        success = has_cycles
        
    end function test_detect_circular_deps
    
    function test_transitive_dependencies() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        character(len=:), allocatable :: deps(:)
        
        analyzer = create_incremental_analyzer()
        call analyzer%add_dependency("a.f90", "b.f90")
        call analyzer%add_dependency("b.f90", "c.f90")
        
        deps = analyzer%get_transitive_dependencies("a.f90")
        success = size(deps) == 2  ! Should include b.f90 and c.f90
        
    end function test_transitive_dependencies
    
    function test_single_file_change() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        character(len=:), allocatable :: affected_files(:)
        
        analyzer = create_incremental_analyzer()
        call analyzer%file_changed("test.f90")
        
        affected_files = analyzer%get_affected_files()
        success = size(affected_files) >= 1
        
    end function test_single_file_change
    
    function test_module_change_propagation() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        character(len=:), allocatable :: affected_files(:)
        
        analyzer = create_incremental_analyzer()
        call analyzer%add_dependency("main.f90", "module.f90")
        call analyzer%file_changed("module.f90")
        
        affected_files = analyzer%get_affected_files()
        success = any(affected_files == "main.f90")
        
    end function test_module_change_propagation
    
    function test_interface_change_propagation() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        
        analyzer = create_incremental_analyzer()
        call analyzer%interface_changed("interface.f90")
        
        success = analyzer%requires_full_rebuild()
        
    end function test_interface_change_propagation
    
    function test_config_change_propagation() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        
        analyzer = create_incremental_analyzer()
        call analyzer%config_changed("fluff.toml")
        
        success = analyzer%requires_full_rebuild()
        
    end function test_config_change_propagation
    
    function test_lint_changed_only() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        character(len=:), allocatable :: files_to_lint(:)
        
        analyzer = create_incremental_analyzer()
        call analyzer%file_changed("test1.f90")
        
        files_to_lint = analyzer%get_files_to_analyze()
        success = size(files_to_lint) == 1 .and. files_to_lint(1) == "test1.f90"
        
    end function test_lint_changed_only
    
    function test_lint_dependent_files() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        character(len=:), allocatable :: files_to_lint(:)
        
        analyzer = create_incremental_analyzer()
        call analyzer%add_dependency("main.f90", "module.f90")
        call analyzer%file_changed("module.f90")
        
        files_to_lint = analyzer%get_files_to_analyze()
        success = size(files_to_lint) >= 2  ! module.f90 and main.f90
        
    end function test_lint_dependent_files
    
    function test_skip_unchanged() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        character(len=:), allocatable :: files_to_lint(:)
        
        analyzer = create_incremental_analyzer()
        call analyzer%mark_file_analyzed("unchanged.f90")
        
        files_to_lint = analyzer%get_files_to_analyze()
        success = .not. any(files_to_lint == "unchanged.f90")
        
    end function test_skip_unchanged
    
    function test_merge_results() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        type(analysis_results_t) :: results1, results2, merged
        
        analyzer = create_incremental_analyzer()
        
        ! Create mock results
        results1%file_count = 1
        results2%file_count = 1
        
        call analyzer%merge_results(results1, results2, merged)
        success = merged%file_count == 2
        
    end function test_merge_results
    
    function test_cache_results() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        type(analysis_results_t) :: results
        
        analyzer = create_incremental_analyzer()
        results%file_count = 1
        
        call analyzer%cache_results("test.f90", results)
        success = analyzer%has_cached_results("test.f90")
        
    end function test_cache_results
    
    function test_cache_invalidation() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        type(analysis_results_t) :: results
        
        analyzer = create_incremental_analyzer()
        results%file_count = 1
        
        call analyzer%cache_results("test.f90", results)
        call analyzer%invalidate_cache("test.f90")
        
        success = .not. analyzer%has_cached_results("test.f90")
        
    end function test_cache_invalidation
    
    function test_cache_performance() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        type(cache_stats_t) :: stats
        
        analyzer = create_incremental_analyzer()
        stats = analyzer%get_cache_stats()
        
        success = stats%hit_rate >= 0.0 .and. stats%hit_rate <= 1.0
        
    end function test_cache_performance
    
    function test_cache_memory() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        integer :: memory_usage
        
        analyzer = create_incremental_analyzer()
        memory_usage = analyzer%get_cache_memory_usage()
        
        success = memory_usage >= 0
        
    end function test_cache_memory
    
    function test_parallel_analysis() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        
        analyzer = create_incremental_analyzer()
        call analyzer%enable_parallel_analysis(.true.)
        
        success = analyzer%is_parallel_enabled()
        
    end function test_parallel_analysis
    
    function test_work_scheduling() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        type(work_schedule_t) :: schedule
        
        analyzer = create_incremental_analyzer()
        call analyzer%file_changed("test1.f90")
        call analyzer%file_changed("test2.f90")
        
        schedule = analyzer%create_work_schedule()
        success = schedule%task_count >= 2
        
    end function test_work_scheduling
    
    function test_resource_management() result(success)
        logical :: success
        type(incremental_analyzer_t) :: analyzer
        type(resource_stats_t) :: stats
        
        analyzer = create_incremental_analyzer()
        stats = analyzer%get_resource_stats()
        
        success = stats%memory_usage >= 0 .and. stats%cpu_usage >= 0.0
        
    end function test_resource_management
    
end program test_incremental_analysis
