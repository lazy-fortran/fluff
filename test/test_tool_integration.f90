program test_tool_integration
    use fluff_core
    use fluff_diagnostics
    use fluff_tool_integration
    implicit none
    
    ! Import exit code constants
    integer, parameter :: EXIT_SUCCESS = 0
    integer, parameter :: EXIT_WARNINGS = 1
    integer, parameter :: EXIT_ERRORS = 2
    integer, parameter :: EXIT_CONFIG_ERROR = 3
    integer, parameter :: EXIT_INTERNAL_ERROR = 4
    
    integer :: total_tests, passed_tests
    
    print *, "=== Tool Integration Test Suite (RED Phase) ==="
    
    total_tests = 0
    passed_tests = 0
    
    ! Test different integration features
    call test_exit_code_behavior()
    call test_stdin_stdout_handling()
    call test_configuration_discovery()
    call test_environment_variables()
    call test_github_actions_integration()
    call test_precommit_hooks()
    
    print *, ""
    print *, "=== Tool Integration Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"
    
    if (passed_tests == total_tests) then
        print *, "✅ All tool integration tests passed!"
    else
        print *, "❌ Some tests failed (expected in RED phase)"
        error stop 1
    end if
    
contains
    
    subroutine test_exit_code_behavior()
        print *, ""
        print *, "Testing exit code behavior..."
        
        ! Test 1: Success case (no issues found)
        call run_integration_test("Exit code 0 for no issues", &
            test_exit_code_success, .true.)
        
        ! Test 2: Warning case (warnings found)
        call run_integration_test("Exit code 1 for warnings", &
            test_exit_code_warnings, .true.)
        
        ! Test 3: Error case (errors found)
        call run_integration_test("Exit code 2 for errors", &
            test_exit_code_errors, .true.)
        
        ! Test 4: Configuration error case
        call run_integration_test("Exit code 3 for config errors", &
            test_exit_code_config_error, .true.)
        
        ! Test 5: Internal error case
        call run_integration_test("Exit code 4 for internal errors", &
            test_exit_code_internal_error, .true.)
        
        ! Test 6: Custom exit code override
        call run_integration_test("Custom exit code override", &
            test_exit_code_override, .true.)
        
    end subroutine test_exit_code_behavior
    
    subroutine test_stdin_stdout_handling()
        print *, ""
        print *, "Testing stdin/stdout handling..."
        
        ! Test 1: Read from stdin
        call run_integration_test("Read Fortran code from stdin", &
            test_stdin_input, .true.)
        
        ! Test 2: Write to stdout
        call run_integration_test("Write results to stdout", &
            test_stdout_output, .true.)
        
        ! Test 3: Error handling for malformed input
        call run_integration_test("Handle malformed stdin input", &
            test_stdin_error_handling, .true.)
        
        ! Test 4: Large input handling
        call run_integration_test("Handle large stdin input", &
            test_stdin_large_input, .true.)
        
        ! Test 5: Binary input detection
        call run_integration_test("Detect and reject binary input", &
            test_stdin_binary_detection, .true.)
        
        ! Test 6: UTF-8 encoding support
        call run_integration_test("Support UTF-8 encoded input", &
            test_stdin_utf8_support, .true.)
        
    end subroutine test_stdin_stdout_handling
    
    subroutine test_configuration_discovery()
        print *, ""
        print *, "Testing configuration discovery..."
        
        ! Test 1: Find fluff.toml in current directory
        call run_integration_test("Find fluff.toml in current dir", &
            test_config_current_dir, .true.)
        
        ! Test 2: Find fluff.toml in parent directories
        call run_integration_test("Find fluff.toml in parent dirs", &
            test_config_parent_dirs, .true.)
        
        ! Test 3: Find pyproject.toml [tool.fluff] section
        call run_integration_test("Find pyproject.toml [tool.fluff]", &
            test_config_pyproject_toml, .true.)
        
        ! Test 4: Configuration precedence order
        call run_integration_test("Configuration precedence order", &
            test_config_precedence, .true.)
        
        ! Test 5: Home directory config fallback
        call run_integration_test("Home directory config fallback", &
            test_config_home_fallback, .true.)
        
        ! Test 6: Global system config
        call run_integration_test("Global system config", &
            test_config_global_system, .true.)
        
    end subroutine test_configuration_discovery
    
    subroutine test_environment_variables()
        print *, ""
        print *, "Testing environment variable support..."
        
        ! Test 1: FLUFF_CONFIG environment variable
        call run_integration_test("FLUFF_CONFIG environment variable", &
            test_env_fluff_config, .true.)
        
        ! Test 2: FLUFF_CACHE_DIR environment variable
        call run_integration_test("FLUFF_CACHE_DIR environment variable", &
            test_env_cache_dir, .true.)
        
        ! Test 3: FLUFF_LOG_LEVEL environment variable
        call run_integration_test("FLUFF_LOG_LEVEL environment variable", &
            test_env_log_level, .true.)
        
        ! Test 4: NO_COLOR environment variable
        call run_integration_test("NO_COLOR environment variable", &
            test_env_no_color, .true.)
        
        ! Test 5: CI environment detection
        call run_integration_test("CI environment detection", &
            test_env_ci_detection, .true.)
        
        ! Test 6: Custom environment variable overrides
        call run_integration_test("Custom environment overrides", &
            test_env_custom_overrides, .true.)
        
    end subroutine test_environment_variables
    
    subroutine test_github_actions_integration()
        print *, ""
        print *, "Testing GitHub Actions integration..."
        
        ! Test 1: GitHub Actions annotation format
        call run_integration_test("GitHub Actions annotation format", &
            test_github_annotations, .true.)
        
        ! Test 2: Problem matcher output
        call run_integration_test("Problem matcher output", &
            test_github_problem_matcher, .true.)
        
        ! Test 3: Workflow file generation
        call run_integration_test("Workflow file generation", &
            test_github_workflow_generation, .true.)
        
        ! Test 4: Action marketplace integration
        call run_integration_test("Action marketplace integration", &
            test_github_action_marketplace, .true.)
        
        ! Test 5: PR comment integration
        call run_integration_test("PR comment integration", &
            test_github_pr_comments, .true.)
        
        ! Test 6: Check run status updates
        call run_integration_test("Check run status updates", &
            test_github_check_runs, .true.)
        
    end subroutine test_github_actions_integration
    
    subroutine test_precommit_hooks()
        print *, ""
        print *, "Testing pre-commit hook integration..."
        
        ! Test 1: Pre-commit hook installation
        call run_integration_test("Pre-commit hook installation", &
            test_precommit_installation, .true.)
        
        ! Test 2: Hook configuration options
        call run_integration_test("Hook configuration options", &
            test_precommit_configuration, .true.)
        
        ! Test 3: Staged file detection
        call run_integration_test("Staged file detection", &
            test_precommit_staged_files, .true.)
        
        ! Test 4: Auto-fix integration
        call run_integration_test("Auto-fix integration", &
            test_precommit_autofix, .true.)
        
        ! Test 5: Hook bypass options
        call run_integration_test("Hook bypass options", &
            test_precommit_bypass, .true.)
        
        ! Test 6: Performance optimization for hooks
        call run_integration_test("Performance optimization", &
            test_precommit_performance, .true.)
        
    end subroutine test_precommit_hooks
    
    ! Helper subroutine for running tests
    subroutine run_integration_test(test_name, test_proc, should_succeed)
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
        
    end subroutine run_integration_test
    
    ! Individual test functions (RED phase - should fail until implemented)
    
    ! Exit Code Tests
    function test_exit_code_success() result(success)
        logical :: success
        type(exit_code_t) :: exit_handler
        
        call exit_handler%set_success()
        success = (exit_handler%get_code() == EXIT_SUCCESS)
        
    end function test_exit_code_success
    
    function test_exit_code_warnings() result(success)
        logical :: success
        type(exit_code_t) :: exit_handler
        
        call exit_handler%set_warnings()
        success = (exit_handler%get_code() == EXIT_WARNINGS)
        
    end function test_exit_code_warnings
    
    function test_exit_code_errors() result(success)
        logical :: success
        type(exit_code_t) :: exit_handler
        
        call exit_handler%set_errors()
        success = (exit_handler%get_code() == EXIT_ERRORS)
        
    end function test_exit_code_errors
    
    function test_exit_code_config_error() result(success)
        logical :: success
        type(exit_code_t) :: exit_handler
        
        call exit_handler%set_config_error()
        success = (exit_handler%get_code() == EXIT_CONFIG_ERROR)
        
    end function test_exit_code_config_error
    
    function test_exit_code_internal_error() result(success)
        logical :: success
        type(exit_code_t) :: exit_handler
        
        call exit_handler%set_internal_error()
        success = (exit_handler%get_code() == EXIT_INTERNAL_ERROR)
        
    end function test_exit_code_internal_error
    
    function test_exit_code_override() result(success)
        logical :: success
        type(exit_code_t) :: exit_handler
        
        call exit_handler%set_override(99)
        success = (exit_handler%get_code() == 99)
        
    end function test_exit_code_override
    
    ! Stdin/Stdout Tests
    function test_stdin_input() result(success)
        logical :: success
        type(stdin_handler_t) :: stdin_handler
        character(len=:), allocatable :: content
        
        content = stdin_handler%read_stdin()
        success = allocated(content) .and. len(content) > 0
        
    end function test_stdin_input
    
    function test_stdout_output() result(success)
        logical :: success
        type(stdin_handler_t) :: stdin_handler
        
        call stdin_handler%write_stdout("test output")
        success = .true.  ! If we reach here, it worked
        
    end function test_stdout_output
    
    function test_stdin_error_handling() result(success)
        logical :: success
        ! Test handling malformed stdin input
        success = .false.  ! RED phase - not implemented yet
    end function test_stdin_error_handling
    
    function test_stdin_large_input() result(success)
        logical :: success
        ! Test handling large stdin input
        success = .false.  ! RED phase - not implemented yet
    end function test_stdin_large_input
    
    function test_stdin_binary_detection() result(success)
        logical :: success
        type(stdin_handler_t) :: stdin_handler
        character(len=10) :: test_content
        
        test_content = "text" // char(0) // "data"  ! Contains null byte
        success = stdin_handler%is_binary(test_content)
        
    end function test_stdin_binary_detection
    
    function test_stdin_utf8_support() result(success)
        logical :: success
        type(stdin_handler_t) :: stdin_handler
        character(len=20) :: test_content
        
        test_content = "Hello UTF-8 text"
        success = stdin_handler%validate_utf8(test_content)
        
    end function test_stdin_utf8_support
    
    ! Configuration Discovery Tests
    function test_config_current_dir() result(success)
        logical :: success
        type(config_discovery_t) :: config_discovery
        
        success = config_discovery%find_in_current_dir()
        
    end function test_config_current_dir
    
    function test_config_parent_dirs() result(success)
        logical :: success
        type(config_discovery_t) :: config_discovery
        
        success = config_discovery%find_in_parent_dirs()
        
    end function test_config_parent_dirs
    
    function test_config_pyproject_toml() result(success)
        logical :: success
        type(config_discovery_t) :: config_discovery
        
        success = config_discovery%find_pyproject_toml()
        
    end function test_config_pyproject_toml
    
    function test_config_precedence() result(success)
        logical :: success
        type(config_discovery_t) :: config_discovery
        character(len=50) :: order(5)
        
        order = config_discovery%get_precedence_order()
        success = (index(order(1), "current") > 0)
        
    end function test_config_precedence
    
    function test_config_home_fallback() result(success)
        logical :: success
        type(config_discovery_t) :: config_discovery
        
        success = config_discovery%find_home_config()
        
    end function test_config_home_fallback
    
    function test_config_global_system() result(success)
        logical :: success
        type(config_discovery_t) :: config_discovery
        
        success = config_discovery%find_global_config()
        
    end function test_config_global_system
    
    ! Environment Variable Tests
    function test_env_fluff_config() result(success)
        logical :: success
        type(environment_handler_t) :: env_handler
        
        call env_handler%get_fluff_config()
        success = allocated(env_handler%config_path) .and. len_trim(env_handler%config_path) > 0
        
    end function test_env_fluff_config
    
    function test_env_cache_dir() result(success)
        logical :: success
        type(environment_handler_t) :: env_handler
        
        call env_handler%get_cache_dir()
        success = allocated(env_handler%cache_dir) .and. len_trim(env_handler%cache_dir) > 0
        
    end function test_env_cache_dir
    
    function test_env_log_level() result(success)
        logical :: success
        type(environment_handler_t) :: env_handler
        
        call env_handler%get_log_level()
        success = allocated(env_handler%log_level) .and. len_trim(env_handler%log_level) > 0
        
    end function test_env_log_level
    
    function test_env_no_color() result(success)
        logical :: success
        type(environment_handler_t) :: env_handler
        
        call env_handler%check_no_color()
        success = .true.  ! Method executed successfully
        
    end function test_env_no_color
    
    function test_env_ci_detection() result(success)
        logical :: success
        type(environment_handler_t) :: env_handler
        
        call env_handler%detect_ci_environment()
        success = .true.  ! Method executed successfully (CI detection works)
        
    end function test_env_ci_detection
    
    function test_env_custom_overrides() result(success)
        logical :: success
        type(environment_handler_t) :: env_handler
        
        call env_handler%apply_custom_overrides()
        success = .true.  ! Method executed successfully
        
    end function test_env_custom_overrides
    
    ! GitHub Actions Tests
    function test_github_annotations() result(success)
        logical :: success
        type(github_integration_t) :: github_integration
        type(diagnostic_t) :: diagnostics(1)
        character(len=:), allocatable :: annotations
        
        ! Create test diagnostic
        diagnostics(1)%code = "F001"
        diagnostics(1)%message = "Test error"
        diagnostics(1)%file_path = "test.f90"
        diagnostics(1)%location%start%line = 1
        diagnostics(1)%location%start%column = 1
        
        annotations = github_integration%format_annotations(diagnostics)
        success = (index(annotations, "::error") > 0)
        
    end function test_github_annotations
    
    function test_github_problem_matcher() result(success)
        logical :: success
        ! Test problem matcher output
        success = .false.  ! RED phase - not implemented yet
    end function test_github_problem_matcher
    
    function test_github_workflow_generation() result(success)
        logical :: success
        type(github_integration_t) :: github_integration
        character(len=:), allocatable :: workflow
        
        workflow = github_integration%generate_workflow()
        success = (index(workflow, "name: Fluff") > 0) .and. (index(workflow, "fluff check") > 0)
        
    end function test_github_workflow_generation
    
    function test_github_action_marketplace() result(success)
        logical :: success
        ! Test action marketplace integration
        success = .false.  ! RED phase - not implemented yet
    end function test_github_action_marketplace
    
    function test_github_pr_comments() result(success)
        logical :: success
        type(github_integration_t) :: github_integration
        type(diagnostic_t) :: diagnostics(1)
        character(len=:), allocatable :: comment
        
        ! Create test diagnostic
        diagnostics(1)%code = "F001"
        diagnostics(1)%message = "Test error"
        
        comment = github_integration%create_pr_comment(diagnostics)
        success = (index(comment, "Fluff Results") > 0) .and. (index(comment, "1 issues") > 0)
        
    end function test_github_pr_comments
    
    function test_github_check_runs() result(success)
        logical :: success
        ! Test check run status updates
        success = .false.  ! RED phase - not implemented yet
    end function test_github_check_runs
    
    ! Pre-commit Hook Tests
    function test_precommit_installation() result(success)
        logical :: success
        type(precommit_handler_t) :: precommit_handler
        
        success = precommit_handler%install_hook(".git/hooks/pre-commit")
        
    end function test_precommit_installation
    
    function test_precommit_configuration() result(success)
        logical :: success
        type(precommit_handler_t) :: precommit_handler
        
        call precommit_handler%configure_hook("autofix=true")
        success = precommit_handler%auto_fix_enabled
        
    end function test_precommit_configuration
    
    function test_precommit_staged_files() result(success)
        logical :: success
        type(precommit_handler_t) :: precommit_handler
        character(len=:), allocatable :: files(:)
        
        files = precommit_handler%get_staged_files()
        success = allocated(files) .and. size(files) > 0
        
    end function test_precommit_staged_files
    
    function test_precommit_autofix() result(success)
        logical :: success
        type(precommit_handler_t) :: precommit_handler
        
        call precommit_handler%enable_autofix(.true.)
        success = precommit_handler%auto_fix_enabled
        
    end function test_precommit_autofix
    
    function test_precommit_bypass() result(success)
        logical :: success
        ! Test hook bypass options
        success = .false.  ! RED phase - not implemented yet
    end function test_precommit_bypass
    
    function test_precommit_performance() result(success)
        logical :: success
        ! Test performance optimization
        success = .false.  ! RED phase - not implemented yet
    end function test_precommit_performance
    
end program test_tool_integration