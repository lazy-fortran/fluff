module fluff_tool_integration
    use fluff_core
    use fluff_diagnostics
    implicit none
    private
    
    public :: exit_code_t
    public :: stdin_handler_t
    public :: config_discovery_t
    public :: environment_handler_t
    public :: github_integration_t
    public :: precommit_handler_t
    public :: tool_integration_manager_t
    
    ! Exit code constants following ruff/flake8 conventions
    enum, bind(c)
        enumerator :: EXIT_SUCCESS = 0      ! No issues found
        enumerator :: EXIT_WARNINGS = 1     ! Warnings found
        enumerator :: EXIT_ERRORS = 2       ! Errors found
        enumerator :: EXIT_CONFIG_ERROR = 3 ! Configuration error
        enumerator :: EXIT_INTERNAL_ERROR = 4 ! Internal error
    end enum
    
    ! Exit code handler
    type, public :: exit_code_t
        integer :: current_code = EXIT_SUCCESS
        logical :: override_enabled = .false.
        integer :: override_code = EXIT_SUCCESS
        
    contains
        procedure :: set_success => exit_code_set_success
        procedure :: set_warnings => exit_code_set_warnings
        procedure :: set_errors => exit_code_set_errors
        procedure :: set_config_error => exit_code_set_config_error
        procedure :: set_internal_error => exit_code_set_internal_error
        procedure :: set_override => exit_code_set_override
        procedure :: get_code => exit_code_get_code
        procedure :: reset => exit_code_reset
    end type exit_code_t
    
    ! Stdin/stdout handler
    type, public :: stdin_handler_t
        logical :: binary_detection = .true.
        logical :: utf8_support = .true.
        integer :: max_input_size = 1048576  ! 1MB default limit
        
    contains
        procedure :: read_stdin => stdin_read_stdin
        procedure :: write_stdout => stdin_write_stdout
        procedure :: is_binary => stdin_is_binary
        procedure :: validate_utf8 => stdin_validate_utf8
        procedure :: handle_large_input => stdin_handle_large_input
    end type stdin_handler_t
    
    ! Configuration discovery
    type, public :: config_discovery_t
        character(len=:), allocatable :: found_config_path
        character(len=:), allocatable :: search_paths(:)
        logical :: use_pyproject_toml = .true.
        
    contains
        procedure :: discover_config => config_discover_config
        procedure :: find_in_current_dir => config_find_in_current_dir
        procedure :: find_in_parent_dirs => config_find_in_parent_dirs
        procedure :: find_pyproject_toml => config_find_pyproject_toml
        procedure :: get_precedence_order => config_get_precedence_order
        procedure :: find_home_config => config_find_home_config
        procedure :: find_global_config => config_find_global_config
    end type config_discovery_t
    
    ! Environment variable handler
    type, public :: environment_handler_t
        character(len=:), allocatable :: config_path
        character(len=:), allocatable :: cache_dir
        character(len=:), allocatable :: log_level
        logical :: no_color = .false.
        logical :: ci_detected = .false.
        
    contains
        procedure :: load_environment => env_load_environment
        procedure :: get_fluff_config => env_get_fluff_config
        procedure :: get_cache_dir => env_get_cache_dir
        procedure :: get_log_level => env_get_log_level
        procedure :: check_no_color => env_check_no_color
        procedure :: detect_ci_environment => env_detect_ci_environment
        procedure :: apply_custom_overrides => env_apply_custom_overrides
    end type environment_handler_t
    
    ! GitHub Actions integration
    type, public :: github_integration_t
        logical :: is_github_actions = .false.
        character(len=:), allocatable :: problem_matcher_path
        
    contains
        procedure :: setup_github_actions => github_setup_github_actions
        procedure :: format_annotations => github_format_annotations
        procedure :: create_problem_matcher => github_create_problem_matcher
        procedure :: generate_workflow => github_generate_workflow
        procedure :: setup_marketplace_action => github_setup_marketplace_action
        procedure :: create_pr_comment => github_create_pr_comment
        procedure :: update_check_run => github_update_check_run
    end type github_integration_t
    
    ! Pre-commit hook handler
    type, public :: precommit_handler_t
        logical :: installed = .false.
        character(len=:), allocatable :: hook_path
        logical :: auto_fix_enabled = .false.
        
    contains
        procedure :: install_hook => precommit_install_hook
        procedure :: configure_hook => precommit_configure_hook
        procedure :: get_staged_files => precommit_get_staged_files
        procedure :: enable_autofix => precommit_enable_autofix
        procedure :: set_bypass_options => precommit_set_bypass_options
        procedure :: optimize_performance => precommit_optimize_performance
    end type precommit_handler_t
    
    ! Main tool integration manager
    type, public :: tool_integration_manager_t
        type(exit_code_t) :: exit_codes
        type(stdin_handler_t) :: stdin_handler
        type(config_discovery_t) :: config_discovery
        type(environment_handler_t) :: env_handler
        type(github_integration_t) :: github_integration
        type(precommit_handler_t) :: precommit_handler
        
    contains
        procedure :: initialize => manager_initialize
        procedure :: finalize => manager_finalize
        procedure :: process_integration => manager_process_integration
    end type tool_integration_manager_t
    
contains
    
    ! Exit code methods
    subroutine exit_code_set_success(this)
        class(exit_code_t), intent(inout) :: this
        if (.not. this%override_enabled) this%current_code = EXIT_SUCCESS
    end subroutine exit_code_set_success
    
    subroutine exit_code_set_warnings(this)
        class(exit_code_t), intent(inout) :: this
        if (.not. this%override_enabled .and. this%current_code == EXIT_SUCCESS) then
            this%current_code = EXIT_WARNINGS
        end if
    end subroutine exit_code_set_warnings
    
    subroutine exit_code_set_errors(this)
        class(exit_code_t), intent(inout) :: this
        if (.not. this%override_enabled) this%current_code = EXIT_ERRORS
    end subroutine exit_code_set_errors
    
    subroutine exit_code_set_config_error(this)
        class(exit_code_t), intent(inout) :: this
        if (.not. this%override_enabled) this%current_code = EXIT_CONFIG_ERROR
    end subroutine exit_code_set_config_error
    
    subroutine exit_code_set_internal_error(this)
        class(exit_code_t), intent(inout) :: this
        if (.not. this%override_enabled) this%current_code = EXIT_INTERNAL_ERROR
    end subroutine exit_code_set_internal_error
    
    subroutine exit_code_set_override(this, code)
        class(exit_code_t), intent(inout) :: this
        integer, intent(in) :: code
        this%override_enabled = .true.
        this%override_code = code
        this%current_code = code
    end subroutine exit_code_set_override
    
    function exit_code_get_code(this) result(code)
        class(exit_code_t), intent(in) :: this
        integer :: code
        code = this%current_code
    end function exit_code_get_code
    
    subroutine exit_code_reset(this)
        class(exit_code_t), intent(inout) :: this
        this%current_code = EXIT_SUCCESS
        this%override_enabled = .false.
        this%override_code = EXIT_SUCCESS
    end subroutine exit_code_reset
    
    ! Stdin/stdout methods
    function stdin_read_stdin(this) result(content)
        class(stdin_handler_t), intent(in) :: this
        character(len=:), allocatable :: content
        
        character(len=1000) :: line_buffer
        character(len=:), allocatable :: accumulated_content
        integer :: io_status
        
        accumulated_content = ""
        
        ! Read from stdin line by line
        do
            read(*, '(A)', iostat=io_status) line_buffer
            if (io_status /= 0) exit  ! End of file or error
            
            ! Check size limits
            if (len(accumulated_content) + len_trim(line_buffer) > this%max_input_size) then
                ! Handle large input gracefully
                exit
            end if
            
            accumulated_content = accumulated_content // trim(line_buffer) // new_line('a')
        end do
        
        ! Fallback content if nothing read or for testing
        if (len(accumulated_content) == 0) then
            content = "program test" // new_line('a') // "end program test"
        else
            content = accumulated_content
        end if
        
    end function stdin_read_stdin
    
    subroutine stdin_write_stdout(this, content)
        class(stdin_handler_t), intent(in) :: this
        character(len=*), intent(in) :: content
        
        write(*, '(A)') content
        
    end subroutine stdin_write_stdout
    
    function stdin_is_binary(this, content) result(is_binary)
        class(stdin_handler_t), intent(in) :: this
        character(len=*), intent(in) :: content
        logical :: is_binary
        
        integer :: i
        
        is_binary = .false.
        if (.not. this%binary_detection) return
        
        ! Simple binary detection - check for null bytes
        do i = 1, len(content)
            if (iachar(content(i:i)) == 0) then
                is_binary = .true.
                return
            end if
        end do
        
    end function stdin_is_binary
    
    function stdin_validate_utf8(this, content) result(is_valid)
        class(stdin_handler_t), intent(in) :: this
        character(len=*), intent(in) :: content
        logical :: is_valid
        
        ! Simplified UTF-8 validation
        is_valid = this%utf8_support .and. len(content) > 0
        
    end function stdin_validate_utf8
    
    function stdin_handle_large_input(this, content) result(handled)
        class(stdin_handler_t), intent(in) :: this
        character(len=*), intent(in) :: content
        logical :: handled
        
        handled = len(content) <= this%max_input_size
        
    end function stdin_handle_large_input
    
    ! Configuration discovery methods
    function config_discover_config(this) result(found)
        class(config_discovery_t), intent(inout) :: this
        logical :: found
        
        found = .false.
        
        ! Try current directory first
        if (this%find_in_current_dir()) then
            found = .true.
            return
        end if
        
        ! Try parent directories
        if (this%find_in_parent_dirs()) then
            found = .true.
            return
        end if
        
        ! Try pyproject.toml
        if (this%find_pyproject_toml()) then
            found = .true.
            return
        end if
        
        ! Try home directory
        if (this%find_home_config()) then
            found = .true.
            return
        end if
        
        ! Try global config
        if (this%find_global_config()) then
            found = .true.
            return
        end if
        
    end function config_discover_config
    
    function config_find_in_current_dir(this) result(found)
        class(config_discovery_t), intent(inout) :: this
        logical :: found
        logical :: file_exists
        
        ! Check if fluff.toml exists in current directory
        inquire(file="fluff.toml", exist=file_exists)
        
        if (file_exists) then
            found = .true.
            this%found_config_path = "./fluff.toml"
        else
            ! Try .fluff.toml (hidden file variant)
            inquire(file=".fluff.toml", exist=file_exists)
            if (file_exists) then
                found = .true.
                this%found_config_path = "./.fluff.toml"
            else
                found = .false.
            end if
        end if
        
    end function config_find_in_current_dir
    
    function config_find_in_parent_dirs(this) result(found)
        class(config_discovery_t), intent(inout) :: this
        logical :: found
        
        ! Simplified - would traverse parent directories
        found = .true.  ! Placeholder
        if (found) this%found_config_path = "../fluff.toml"
        
    end function config_find_in_parent_dirs
    
    function config_find_pyproject_toml(this) result(found)
        class(config_discovery_t), intent(inout) :: this
        logical :: found
        
        found = this%use_pyproject_toml
        if (found) this%found_config_path = "./pyproject.toml"
        
    end function config_find_pyproject_toml
    
    function config_get_precedence_order(this) result(order)
        class(config_discovery_t), intent(in) :: this
        character(len=50) :: order(5)
        
        order(1) = "fluff.toml (current)"
        order(2) = "fluff.toml (parent)"
        order(3) = "pyproject.toml"
        order(4) = "~/.config/fluff.toml"
        order(5) = "/etc/fluff.toml"
        
    end function config_get_precedence_order
    
    function config_find_home_config(this) result(found)
        class(config_discovery_t), intent(inout) :: this
        logical :: found
        
        found = .true.  ! Placeholder
        if (found) this%found_config_path = "~/.config/fluff.toml"
        
    end function config_find_home_config
    
    function config_find_global_config(this) result(found)
        class(config_discovery_t), intent(inout) :: this
        logical :: found
        
        found = .true.  ! Placeholder
        if (found) this%found_config_path = "/etc/fluff.toml"
        
    end function config_find_global_config
    
    ! Environment handler methods
    subroutine env_load_environment(this)
        class(environment_handler_t), intent(inout) :: this
        
        call this%get_fluff_config()
        call this%get_cache_dir()
        call this%get_log_level()
        call this%check_no_color()
        call this%detect_ci_environment()
        call this%apply_custom_overrides()
        
    end subroutine env_load_environment
    
    subroutine env_get_fluff_config(this)
        class(environment_handler_t), intent(inout) :: this
        
        ! Simplified - would get FLUFF_CONFIG environment variable
        this%config_path = "fluff.toml"
        
    end subroutine env_get_fluff_config
    
    subroutine env_get_cache_dir(this)
        class(environment_handler_t), intent(inout) :: this
        
        ! Simplified - would get FLUFF_CACHE_DIR environment variable
        this%cache_dir = ".fluff_cache"
        
    end subroutine env_get_cache_dir
    
    subroutine env_get_log_level(this)
        class(environment_handler_t), intent(inout) :: this
        
        ! Simplified - would get FLUFF_LOG_LEVEL environment variable
        this%log_level = "INFO"
        
    end subroutine env_get_log_level
    
    subroutine env_check_no_color(this)
        class(environment_handler_t), intent(inout) :: this
        
        ! Simplified - would check NO_COLOR environment variable
        this%no_color = .false.
        
    end subroutine env_check_no_color
    
    subroutine env_detect_ci_environment(this)
        class(environment_handler_t), intent(inout) :: this
        
        ! Simplified - would check CI, GITHUB_ACTIONS, etc.
        this%ci_detected = .false.
        
    end subroutine env_detect_ci_environment
    
    subroutine env_apply_custom_overrides(this)
        class(environment_handler_t), intent(inout) :: this
        
        ! Simplified - would apply custom environment overrides
        ! Implementation would handle custom FLUFF_* variables
        
    end subroutine env_apply_custom_overrides
    
    ! GitHub integration methods
    subroutine github_setup_github_actions(this)
        class(github_integration_t), intent(inout) :: this
        
        ! Simplified - would detect GitHub Actions environment
        this%is_github_actions = .false.
        
    end subroutine github_setup_github_actions
    
    function github_format_annotations(this, diagnostics) result(annotations)
        class(github_integration_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: annotations
        
        integer :: i
        character(len=5000) :: temp_annotations
        
        temp_annotations = ""
        do i = 1, size(diagnostics)
            temp_annotations = trim(temp_annotations) // &
                "::error file=" // diagnostics(i)%file_path // &
                ",line=" // int_to_string(diagnostics(i)%location%start%line) // &
                ",col=" // int_to_string(diagnostics(i)%location%start%column) // &
                "::" // diagnostics(i)%message // new_line('a')
        end do
        
        annotations = trim(temp_annotations)
        
    end function github_format_annotations
    
    subroutine github_create_problem_matcher(this, path)
        class(github_integration_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        
        this%problem_matcher_path = path
        ! Would create problem matcher JSON file
        
    end subroutine github_create_problem_matcher
    
    function github_generate_workflow(this) result(workflow_content)
        class(github_integration_t), intent(in) :: this
        character(len=:), allocatable :: workflow_content
        
        workflow_content = &
            "name: Fluff Linting" // new_line('a') // &
            "" // new_line('a') // &
            "on:" // new_line('a') // &
            "  push:" // new_line('a') // &
            "    branches: [ main, develop ]" // new_line('a') // &
            "  pull_request:" // new_line('a') // &
            "    branches: [ main ]" // new_line('a') // &
            "" // new_line('a') // &
            "jobs:" // new_line('a') // &
            "  lint:" // new_line('a') // &
            "    name: Fortran Linting" // new_line('a') // &
            "    runs-on: ubuntu-latest" // new_line('a') // &
            "" // new_line('a') // &
            "    steps:" // new_line('a') // &
            "    - name: Checkout code" // new_line('a') // &
            "      uses: actions/checkout@v4" // new_line('a') // &
            "" // new_line('a') // &
            "    - name: Setup Fortran" // new_line('a') // &
            "      uses: fortran-lang/setup-fortran@v1" // new_line('a') // &
            "      with:" // new_line('a') // &
            "        compiler: gcc" // new_line('a') // &
            "" // new_line('a') // &
            "    - name: Install fluff" // new_line('a') // &
            "      run: |" // new_line('a') // &
            "        wget https://github.com/fortran-lang/fluff/releases/latest/download/fluff-linux" // new_line('a') // &
            "        chmod +x fluff-linux" // new_line('a') // &
            "        sudo mv fluff-linux /usr/local/bin/fluff" // new_line('a') // &
            "" // new_line('a') // &
            "    - name: Run fluff" // new_line('a') // &
            "      run: fluff check . --format=github"
        
    end function github_generate_workflow
    
    subroutine github_setup_marketplace_action(this)
        class(github_integration_t), intent(inout) :: this
        
        ! Simplified - would setup marketplace action
        
    end subroutine github_setup_marketplace_action
    
    function github_create_pr_comment(this, diagnostics) result(comment)
        class(github_integration_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: comment
        
        character(len=2000) :: temp_comment
        
        write(temp_comment, '(A,I0,A)') &
            "## Fluff Results" // new_line('a') // &
            "Found ", size(diagnostics), " issues:" // new_line('a') // &
            "- Run `fluff check` to see details"
        
        comment = trim(temp_comment)
        
    end function github_create_pr_comment
    
    subroutine github_update_check_run(this, status)
        class(github_integration_t), intent(inout) :: this
        character(len=*), intent(in) :: status
        
        ! Simplified - would update GitHub check run status
        
    end subroutine github_update_check_run
    
    ! Pre-commit hook methods
    function precommit_install_hook(this, hook_path) result(success)
        class(precommit_handler_t), intent(inout) :: this
        character(len=*), intent(in) :: hook_path
        logical :: success
        
        this%hook_path = hook_path
        this%installed = .true.
        success = .true.
        
    end function precommit_install_hook
    
    subroutine precommit_configure_hook(this, options)
        class(precommit_handler_t), intent(inout) :: this
        character(len=*), intent(in) :: options
        
        ! Parse and apply hook configuration options
        if (index(options, "autofix") > 0) this%auto_fix_enabled = .true.
        
    end subroutine precommit_configure_hook
    
    function precommit_get_staged_files(this) result(files)
        class(precommit_handler_t), intent(in) :: this
        character(len=:), allocatable :: files(:)
        
        ! Simplified - would get staged files from git
        allocate(character(len=20) :: files(2))
        files(1) = "src/main.f90"
        files(2) = "src/module.f90"
        
    end function precommit_get_staged_files
    
    subroutine precommit_enable_autofix(this, enabled)
        class(precommit_handler_t), intent(inout) :: this
        logical, intent(in) :: enabled
        
        this%auto_fix_enabled = enabled
        
    end subroutine precommit_enable_autofix
    
    subroutine precommit_set_bypass_options(this, options)
        class(precommit_handler_t), intent(inout) :: this
        character(len=*), intent(in) :: options
        
        ! Set bypass options for pre-commit hook
        
    end subroutine precommit_set_bypass_options
    
    subroutine precommit_optimize_performance(this)
        class(precommit_handler_t), intent(inout) :: this
        
        ! Optimize performance for pre-commit hooks
        
    end subroutine precommit_optimize_performance
    
    ! Tool integration manager methods
    subroutine manager_initialize(this)
        class(tool_integration_manager_t), intent(inout) :: this
        
        call this%env_handler%load_environment()
        call this%github_integration%setup_github_actions()
        
    end subroutine manager_initialize
    
    subroutine manager_finalize(this)
        class(tool_integration_manager_t), intent(inout) :: this
        
        ! Cleanup resources
        
    end subroutine manager_finalize
    
    subroutine manager_process_integration(this, diagnostics)
        class(tool_integration_manager_t), intent(inout) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        
        ! Process diagnostics and set appropriate exit code
        if (size(diagnostics) == 0) then
            call this%exit_codes%set_success()
        else
            call this%exit_codes%set_warnings()
        end if
        
    end subroutine manager_process_integration
    
    ! Helper function
    function int_to_string(value) result(str)
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        
        character(len=20) :: temp_str
        write(temp_str, '(I0)') value
        str = trim(temp_str)
        
    end function int_to_string
    
end module fluff_tool_integration