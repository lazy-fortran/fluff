program test_file_watching
    use fluff_core
    use fluff_file_watcher
    implicit none
    
    integer :: total_tests, passed_tests
    
    print *, "=== File Watching Test Suite (RED Phase) ==="
    
    total_tests = 0
    passed_tests = 0
    
    ! Test file watcher creation and initialization
    call test_watcher_creation()
    call test_watcher_configuration()
    call test_single_file_watching()
    call test_directory_watching()
    call test_recursive_watching()
    call test_file_change_detection()
    call test_file_deletion_detection()
    call test_file_creation_detection()
    call test_configuration_reload()
    call test_incremental_analysis()
    call test_smart_rebuild_logic()
    call test_watch_filtering()
    call test_performance_monitoring()
    
    print *, ""
    print *, "=== File Watching Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"
    
    if (passed_tests == total_tests) then
        print *, "✅ All file watching tests passed!"
    else
        print *, "❌ Some tests failed (expected in RED phase)"
        error stop 1
    end if
    
contains
    
    subroutine test_watcher_creation()
        print *, ""
        print *, "Testing file watcher creation..."
        
        ! Test 1: Create basic file watcher
        call run_watcher_test("Basic watcher creation", &
            test_create_basic_watcher, .true.)
        
        ! Test 2: Create watcher with configuration
        call run_watcher_test("Watcher with config", &
            test_create_configured_watcher, .true.)
        
        ! Test 3: Create watcher with invalid config
        call run_watcher_test("Invalid configuration", &
            test_create_invalid_watcher, .true.)
        
    end subroutine test_watcher_creation
    
    subroutine test_watcher_configuration()
        print *, ""
        print *, "Testing watcher configuration..."
        
        ! Test 1: Set watch paths
        call run_watcher_test("Set watch paths", &
            test_set_watch_paths, .true.)
        
        ! Test 2: Set file patterns
        call run_watcher_test("Set file patterns", &
            test_set_file_patterns, .true.)
        
        ! Test 3: Set polling interval
        call run_watcher_test("Set polling interval", &
            test_set_polling_interval, .true.)
        
        ! Test 4: Enable/disable recursive watching
        call run_watcher_test("Recursive watching", &
            test_set_recursive_mode, .true.)
        
    end subroutine test_watcher_configuration
    
    subroutine test_single_file_watching()
        print *, ""
        print *, "Testing single file watching..."
        
        ! Test 1: Watch single Fortran file
        call run_watcher_test("Single file watch", &
            test_watch_single_file, .true.)
        
        ! Test 2: Stop watching single file
        call run_watcher_test("Stop single file watch", &
            test_stop_single_file, .true.)
        
        ! Test 3: Watch non-existent file
        call run_watcher_test("Non-existent file", &
            test_watch_nonexistent, .true.)
        
    end subroutine test_single_file_watching
    
    subroutine test_directory_watching()
        print *, ""
        print *, "Testing directory watching..."
        
        ! Test 1: Watch directory
        call run_watcher_test("Directory watching", &
            test_watch_directory, .true.)
        
        ! Test 2: Watch with file patterns
        call run_watcher_test("Pattern filtering", &
            test_watch_with_patterns, .true.)
        
        ! Test 3: Watch multiple directories
        call run_watcher_test("Multiple directories", &
            test_watch_multiple_dirs, .true.)
        
    end subroutine test_directory_watching
    
    subroutine test_recursive_watching()
        print *, ""
        print *, "Testing recursive directory watching..."
        
        ! Test 1: Recursive watching enabled
        call run_watcher_test("Recursive enabled", &
            test_recursive_enabled, .true.)
        
        ! Test 2: Recursive watching disabled
        call run_watcher_test("Recursive disabled", &
            test_recursive_disabled, .true.)
        
        ! Test 3: Deep directory structure
        call run_watcher_test("Deep directory structure", &
            test_deep_recursive, .true.)
        
    end subroutine test_recursive_watching
    
    subroutine test_file_change_detection()
        print *, ""
        print *, "Testing file change detection..."
        
        ! Test 1: Detect file modification
        call run_watcher_test("File modification", &
            test_detect_modification, .true.)
        
        ! Test 2: Detect multiple changes
        call run_watcher_test("Multiple changes", &
            test_detect_multiple_changes, .true.)
        
        ! Test 3: Ignore unchanged files
        call run_watcher_test("Ignore unchanged", &
            test_ignore_unchanged, .true.)
        
    end subroutine test_file_change_detection
    
    subroutine test_file_deletion_detection()
        print *, ""
        print *, "Testing file deletion detection..."
        
        ! Test 1: Detect file deletion
        call run_watcher_test("File deletion", &
            test_detect_deletion, .true.)
        
        ! Test 2: Handle deleted watched file
        call run_watcher_test("Deleted watched file", &
            test_handle_deleted_watched, .true.)
        
    end subroutine test_file_deletion_detection
    
    subroutine test_file_creation_detection()
        print *, ""
        print *, "Testing file creation detection..."
        
        ! Test 1: Detect new file
        call run_watcher_test("New file creation", &
            test_detect_creation, .true.)
        
        ! Test 2: Auto-watch new files
        call run_watcher_test("Auto-watch new files", &
            test_auto_watch_new, .true.)
        
    end subroutine test_file_creation_detection
    
    subroutine test_configuration_reload()
        print *, ""
        print *, "Testing configuration reload..."
        
        ! Test 1: Reload fluff.toml
        call run_watcher_test("Reload fluff.toml", &
            test_reload_config, .true.)
        
        ! Test 2: Apply new configuration
        call run_watcher_test("Apply new config", &
            test_apply_new_config, .true.)
        
        ! Test 3: Handle invalid config reload
        call run_watcher_test("Invalid config reload", &
            test_invalid_config_reload, .true.)
        
    end subroutine test_configuration_reload
    
    subroutine test_incremental_analysis()
        print *, ""
        print *, "Testing incremental analysis..."
        
        ! Test 1: Analyze only changed files
        call run_watcher_test("Analyze changed only", &
            test_analyze_changed_only, .true.)
        
        ! Test 2: Dependency tracking
        call run_watcher_test("Dependency tracking", &
            test_dependency_tracking, .true.)
        
        ! Test 3: Incremental results caching
        call run_watcher_test("Results caching", &
            test_results_caching, .true.)
        
    end subroutine test_incremental_analysis
    
    subroutine test_smart_rebuild_logic()
        print *, ""
        print *, "Testing smart rebuild logic..."
        
        ! Test 1: Minimal rebuild on change
        call run_watcher_test("Minimal rebuild", &
            test_minimal_rebuild, .true.)
        
        ! Test 2: Full rebuild when needed
        call run_watcher_test("Full rebuild trigger", &
            test_full_rebuild_trigger, .true.)
        
        ! Test 3: Rebuild optimization
        call run_watcher_test("Rebuild optimization", &
            test_rebuild_optimization, .true.)
        
    end subroutine test_smart_rebuild_logic
    
    subroutine test_watch_filtering()
        print *, ""
        print *, "Testing watch filtering..."
        
        ! Test 1: Include/exclude patterns
        call run_watcher_test("Include/exclude patterns", &
            test_include_exclude_patterns, .true.)
        
        ! Test 2: Ignore hidden files
        call run_watcher_test("Ignore hidden files", &
            test_ignore_hidden, .true.)
        
        ! Test 3: Filter by file extension
        call run_watcher_test("Extension filtering", &
            test_extension_filtering, .true.)
        
    end subroutine test_watch_filtering
    
    subroutine test_performance_monitoring()
        print *, ""
        print *, "Testing performance monitoring..."
        
        ! Test 1: Watch performance metrics
        call run_watcher_test("Performance metrics", &
            test_watch_performance, .true.)
        
        ! Test 2: Memory usage tracking
        call run_watcher_test("Memory usage", &
            test_memory_tracking, .true.)
        
        ! Test 3: Event processing time
        call run_watcher_test("Event processing time", &
            test_event_timing, .true.)
        
    end subroutine test_performance_monitoring
    
    ! Helper subroutine for running tests
    subroutine run_watcher_test(test_name, test_proc, should_succeed)
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
        
    end subroutine run_watcher_test
    
    ! Individual test functions (these should fail in RED phase)
    function test_create_basic_watcher() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        ! This should fail because file_watcher_t doesn't exist yet
        watcher = create_file_watcher()
        success = watcher%is_initialized()
        
    end function test_create_basic_watcher
    
    function test_create_configured_watcher() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        type(watch_config_t) :: config
        
        config%polling_interval_ms = 100
        config%recursive = .true.
        config%patterns = [character(len=10) :: "*.f90", "*.F90"]
        
        watcher = create_file_watcher(config)
        success = watcher%is_initialized()
        
    end function test_create_configured_watcher
    
    function test_create_invalid_watcher() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        type(watch_config_t) :: config
        
        config%polling_interval_ms = -1  ! Invalid
        
        watcher = create_file_watcher(config)
        success = .not. watcher%is_initialized()
        
    end function test_create_invalid_watcher
    
    function test_set_watch_paths() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        character(len=:), allocatable :: paths(:)
        
        watcher = create_file_watcher()
        paths = [character(len=20) :: "./src", "./test"]
        
        call watcher%set_watch_paths(paths)
        success = size(watcher%get_watch_paths()) == 2
        
    end function test_set_watch_paths
    
    function test_set_file_patterns() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        character(len=:), allocatable :: patterns(:)
        
        watcher = create_file_watcher()
        patterns = [character(len=10) :: "*.f90", "*.F90", "*.toml"]
        
        call watcher%set_file_patterns(patterns)
        success = size(watcher%get_file_patterns()) == 3
        
    end function test_set_file_patterns
    
    function test_set_polling_interval() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%set_polling_interval(250)
        
        success = watcher%get_polling_interval() == 250
        
    end function test_set_polling_interval
    
    function test_set_recursive_mode() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%set_recursive(.true.)
        
        success = watcher%is_recursive()
        
    end function test_set_recursive_mode
    
    function test_watch_single_file() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        success = watcher%is_watching()
        
    end function test_watch_single_file
    
    function test_stop_single_file() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        call watcher%stop_watching()
        
        success = .not. watcher%is_watching()
        
    end function test_stop_single_file
    
    function test_watch_nonexistent() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        character(len=:), allocatable :: paths(:)
        
        watcher = create_file_watcher()
        paths = [character(len=30) :: "/nonexistent/path"]
        call watcher%set_watch_paths(paths)
        
        call watcher%start_watching()
        success = .not. watcher%is_watching()
        
    end function test_watch_nonexistent
    
    function test_watch_directory() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        character(len=:), allocatable :: paths(:)
        
        watcher = create_file_watcher()
        paths = [character(len=10) :: "./src"]
        call watcher%set_watch_paths(paths)
        call watcher%start_watching()
        
        success = watcher%is_watching()
        
    end function test_watch_directory
    
    function test_watch_with_patterns() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        character(len=:), allocatable :: paths(:), patterns(:)
        
        watcher = create_file_watcher()
        paths = [character(len=10) :: "./src"]
        patterns = [character(len=10) :: "*.f90"]
        
        call watcher%set_watch_paths(paths)
        call watcher%set_file_patterns(patterns)
        call watcher%start_watching()
        
        success = watcher%is_watching()
        
    end function test_watch_with_patterns
    
    function test_watch_multiple_dirs() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        character(len=:), allocatable :: paths(:)
        
        watcher = create_file_watcher()
        paths = [character(len=10) :: "./src", "./test"]
        call watcher%set_watch_paths(paths)
        call watcher%start_watching()
        
        success = watcher%is_watching()
        
    end function test_watch_multiple_dirs
    
    function test_recursive_enabled() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%set_recursive(.true.)
        call watcher%start_watching()
        
        success = watcher%is_recursive() .and. watcher%is_watching()
        
    end function test_recursive_enabled
    
    function test_recursive_disabled() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%set_recursive(.false.)
        call watcher%start_watching()
        
        success = .not. watcher%is_recursive() .and. watcher%is_watching()
        
    end function test_recursive_disabled
    
    function test_deep_recursive() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        character(len=:), allocatable :: paths(:)
        
        watcher = create_file_watcher()
        paths = [character(len=10) :: "./"]
        call watcher%set_watch_paths(paths)
        call watcher%set_recursive(.true.)
        call watcher%start_watching()
        
        success = watcher%is_watching()
        
    end function test_deep_recursive
    
    function test_detect_modification() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        type(file_change_event_t) :: event
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        ! Simulate file change
        call watcher%handle_file_change("test.f90", FILE_MODIFIED)
        success = watcher%get_last_event(event)
        
    end function test_detect_modification
    
    function test_detect_multiple_changes() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        integer :: event_count
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        call watcher%handle_file_change("test1.f90", FILE_MODIFIED)
        call watcher%handle_file_change("test2.f90", FILE_MODIFIED)
        
        event_count = watcher%get_event_count()
        success = event_count == 2
        
    end function test_detect_multiple_changes
    
    function test_ignore_unchanged() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        integer :: initial_count, final_count
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        initial_count = watcher%get_event_count()
        ! Simulate checking unchanged file
        call watcher%check_file_changes()
        final_count = watcher%get_event_count()
        
        success = initial_count == final_count
        
    end function test_ignore_unchanged
    
    function test_detect_deletion() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        type(file_change_event_t) :: event
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        call watcher%handle_file_change("test.f90", FILE_DELETED)
        success = watcher%get_last_event(event) .and. event%change_type == FILE_DELETED
        
    end function test_detect_deletion
    
    function test_handle_deleted_watched() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        call watcher%handle_file_change("watched.f90", FILE_DELETED)
        success = watcher%is_watching()  ! Should continue watching other files
        
    end function test_handle_deleted_watched
    
    function test_detect_creation() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        type(file_change_event_t) :: event
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        call watcher%handle_file_change("new.f90", FILE_CREATED)
        success = watcher%get_last_event(event) .and. event%change_type == FILE_CREATED
        
    end function test_detect_creation
    
    function test_auto_watch_new() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        call watcher%handle_file_change("new.f90", FILE_CREATED)
        success = watcher%is_file_watched("new.f90")
        
    end function test_auto_watch_new
    
    function test_reload_config() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        call watcher%reload_configuration()
        success = watcher%is_watching()
        
    end function test_reload_config
    
    function test_apply_new_config() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        integer :: old_interval, new_interval
        
        watcher = create_file_watcher()
        old_interval = watcher%get_polling_interval()
        
        call watcher%reload_configuration()
        new_interval = watcher%get_polling_interval()
        
        success = new_interval /= old_interval  ! Assuming config changed
        
    end function test_apply_new_config
    
    function test_invalid_config_reload() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        ! Simulate invalid config file
        call watcher%reload_configuration()
        success = watcher%is_watching()  ! Should continue with old config
        
    end function test_invalid_config_reload
    
    function test_analyze_changed_only() result(success)
        use fluff_string_utils
        logical :: success
        type(file_watcher_t) :: watcher
        type(string_array_t) :: changed_files
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        call watcher%handle_file_change("test.f90", FILE_MODIFIED)
        changed_files = watcher%get_changed_files()
        
        success = changed_files%count == 1 .and. changed_files%get_item(1) == "test.f90"
        call changed_files%cleanup()
        
    end function test_analyze_changed_only
    
    function test_dependency_tracking() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        character(len=:), allocatable :: dependent_files(:)
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        call watcher%handle_file_change("module.f90", FILE_MODIFIED)
        dependent_files = watcher%get_dependent_files("module.f90")
        
        success = size(dependent_files) > 0
        
    end function test_dependency_tracking
    
    function test_results_caching() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        logical :: cache_enabled
        
        watcher = create_file_watcher()
        call watcher%enable_results_caching(.true.)
        
        cache_enabled = watcher%is_caching_enabled()
        success = cache_enabled
        
    end function test_results_caching
    
    function test_minimal_rebuild() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        type(rebuild_info_t) :: info
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        call watcher%handle_file_change("test.f90", FILE_MODIFIED)
        info = watcher%get_rebuild_info()
        
        success = info%rebuild_type == REBUILD_MINIMAL
        
    end function test_minimal_rebuild
    
    function test_full_rebuild_trigger() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        type(rebuild_info_t) :: info
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        call watcher%handle_file_change("fluff.toml", FILE_MODIFIED)
        info = watcher%get_rebuild_info()
        
        success = info%rebuild_type == REBUILD_FULL
        
    end function test_full_rebuild_trigger
    
    function test_rebuild_optimization() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%enable_rebuild_optimization(.true.)
        
        success = watcher%is_optimization_enabled()
        
    end function test_rebuild_optimization
    
    function test_include_exclude_patterns() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        character(len=:), allocatable :: include(:), exclude(:)
        
        watcher = create_file_watcher()
        include = [character(len=10) :: "*.f90"]
        exclude = [character(len=10) :: "*.tmp"]
        
        call watcher%set_include_patterns(include)
        call watcher%set_exclude_patterns(exclude)
        
        success = watcher%should_watch_file("test.f90") .and. &
                 .not. watcher%should_watch_file("temp.tmp")
        
    end function test_include_exclude_patterns
    
    function test_ignore_hidden() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        
        watcher = create_file_watcher()
        call watcher%set_ignore_hidden(.true.)
        
        success = .not. watcher%should_watch_file(".hidden_file")
        
    end function test_ignore_hidden
    
    function test_extension_filtering() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        character(len=:), allocatable :: extensions(:)
        
        watcher = create_file_watcher()
        extensions = [character(len=10) :: "f90", "F90", "toml"]
        call watcher%set_watched_extensions(extensions)
        
        success = watcher%should_watch_file("test.f90") .and. &
                  .not. watcher%should_watch_file("test.txt")
        
    end function test_extension_filtering
    
    function test_watch_performance() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        type(watch_performance_t) :: perf
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        perf = watcher%get_performance_stats()
        success = perf%events_processed >= 0
        
    end function test_watch_performance
    
    function test_memory_tracking() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        integer :: memory_usage
        
        watcher = create_file_watcher()
        memory_usage = watcher%get_memory_usage()
        
        success = memory_usage > 0
        
    end function test_memory_tracking
    
    function test_event_timing() result(success)
        logical :: success
        type(file_watcher_t) :: watcher
        real :: avg_time
        
        watcher = create_file_watcher()
        call watcher%start_watching()
        
        call watcher%handle_file_change("test.f90", FILE_MODIFIED)
        avg_time = watcher%get_average_event_time()
        
        success = avg_time >= 0.0
        
    end function test_event_timing
    
end program test_file_watching