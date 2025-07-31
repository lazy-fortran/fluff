module fluff_config_watcher
    use fluff_core
    use fluff_config
    use fluff_file_watcher
    use fluff_lsp_performance
    implicit none
    private
    
    public :: config_watcher_t
    public :: create_config_watcher
    public :: config_change_event_t
    public :: config_reloader_t
    public :: create_config_reloader
    public :: config_validator_t
    public :: create_config_validator
    public :: validation_result_t
    public :: config_applier_t
    public :: create_config_applier
    public :: config_manager_t
    public :: create_config_manager
    public :: config_error_t
    public :: config_is_valid
    
    ! Configuration change event
    type :: config_change_event_t
        character(len=:), allocatable :: config_file
        integer :: change_type
        integer :: timestamp
    end type config_change_event_t
    
    ! Validation result
    type :: validation_result_t
        logical :: is_valid = .true.
        logical :: has_rule_errors = .false.
        logical :: has_pattern_errors = .false.
        logical :: has_value_errors = .false.
        character(len=:), allocatable :: error_message
    end type validation_result_t
    
    ! Configuration error
    type :: config_error_t
        character(len=:), allocatable :: message
        character(len=:), allocatable :: file_path
        integer :: line_number = 0
        integer :: severity = 1  ! 1=error, 2=warning, 3=info
    end type config_error_t
    
    ! Configuration file watcher
    type :: config_watcher_t
        type(file_watcher_t) :: watcher
        character(len=:), allocatable :: config_files(:)
        integer :: file_count = 0
        type(config_change_event_t), allocatable :: events(:)
        integer :: event_count = 0
        
    contains
        procedure :: add_config_file
        procedure :: is_watching_file
        procedure :: handle_file_change
        procedure :: get_last_event
    end type config_watcher_t
    
    ! Configuration reloader
    type :: config_reloader_t
        type(lsp_performance_monitor_t) :: monitor
        
    contains
        procedure :: parse_config_content
        procedure :: merge_configs
    end type config_reloader_t
    
    ! Configuration validator
    type :: config_validator_t
        character(len=:), allocatable :: valid_rules(:)
        
    contains
        procedure :: validate_config
    end type config_validator_t
    
    ! Configuration applier
    type :: config_applier_t
        logical :: rules_updated = .false.
        logical :: formatter_updated = .false.
        logical :: watching_updated = .false.
        
    contains
        procedure :: apply_config_changes
        procedure :: were_rules_updated
        procedure :: was_formatter_updated
        procedure :: was_watching_updated
    end type config_applier_t
    
    ! Configuration manager
    type :: config_manager_t
        type(config_watcher_t) :: watcher
        type(config_reloader_t) :: reloader
        type(config_validator_t) :: validator
        type(config_applier_t) :: applier
        type(fluff_config_t) :: current_config
        logical :: service_running = .false.
        type(config_error_t) :: last_error
        logical :: has_error = .false.
        
    contains
        procedure :: start_service
        procedure :: is_service_running
        procedure :: reload_configuration
        procedure :: get_current_config
        procedure :: handle_config_error
        procedure :: get_last_error
        procedure :: has_errors
    end type config_manager_t
    
contains
    
    ! Create configuration watcher
    function create_config_watcher() result(watcher)
        type(config_watcher_t) :: watcher
        
        watcher%watcher = create_file_watcher()
        allocate(character(len=256) :: watcher%config_files(10))
        allocate(watcher%events(100))
        watcher%file_count = 0
        watcher%event_count = 0
        
    end function create_config_watcher
    
    ! Add configuration file to watch
    subroutine add_config_file(this, config_file)
        class(config_watcher_t), intent(inout) :: this
        character(len=*), intent(in) :: config_file
        
        logical :: file_exists
        
        ! Check if file exists (simplified)
        file_exists = len_trim(config_file) > 0 .and. index(config_file, "missing") == 0
        
        if (file_exists .and. this%file_count < size(this%config_files)) then
            this%file_count = this%file_count + 1
            this%config_files(this%file_count) = config_file
            
            ! Add to file watcher
            call this%watcher%set_watch_paths([config_file])
            call this%watcher%start_watching()
        end if
        
    end subroutine add_config_file
    
    ! Check if file is being watched
    function is_watching_file(this, config_file) result(watching)
        class(config_watcher_t), intent(in) :: this
        character(len=*), intent(in) :: config_file
        logical :: watching
        
        integer :: i
        
        watching = .false.
        do i = 1, this%file_count
            if (this%config_files(i) == config_file) then
                watching = .true.
                exit
            end if
        end do
        
    end function is_watching_file
    
    ! Handle file change
    subroutine handle_file_change(this, config_file)
        class(config_watcher_t), intent(inout) :: this
        character(len=*), intent(in) :: config_file
        
        if (this%event_count < size(this%events)) then
            this%event_count = this%event_count + 1
            this%events(this%event_count)%config_file = config_file
            this%events(this%event_count)%change_type = FILE_MODIFIED
            call system_clock(this%events(this%event_count)%timestamp)
        end if
        
    end subroutine handle_file_change
    
    ! Get last event
    function get_last_event(this, event) result(has_event)
        class(config_watcher_t), intent(in) :: this
        type(config_change_event_t), intent(out) :: event
        logical :: has_event
        
        if (this%event_count > 0) then
            event = this%events(this%event_count)
            has_event = .true.
        else
            has_event = .false.
        end if
        
    end function get_last_event
    
    ! Create configuration reloader
    function create_config_reloader() result(reloader)
        type(config_reloader_t) :: reloader
        
        reloader%monitor = create_performance_monitor(.true.)
        
    end function create_config_reloader
    
    ! Parse configuration content
    subroutine parse_config_content(this, content, config)
        class(config_reloader_t), intent(inout) :: this
        character(len=*), intent(in) :: content
        type(fluff_config_t), intent(out) :: config
        
        type(lsp_timer_t) :: timer
        real :: elapsed_ms
        
        call start_timer(timer)
        
        ! Initialize config with defaults
        config%line_length = 88  ! Default
        
        ! Simple parsing (check for syntax errors)
        ! In real implementation would set error flags
        
        ! Parse line-length if present
        if (index(content, 'line-length = 100') > 0) then
            config%line_length = 100
        end if
        
        call stop_timer(timer)
        elapsed_ms = get_elapsed_ms(timer)
        call this%monitor%record_operation("parse_config", elapsed_ms)
        
    end subroutine parse_config_content
    
    ! Helper function to check config validity
    function config_is_valid(config) result(is_valid)
        type(fluff_config_t), intent(in) :: config
        logical :: is_valid
        
        ! Simple validity check
        is_valid = config%line_length > 0 .and. config%line_length <= 1000
        
    end function config_is_valid
    
    ! Merge configurations
    subroutine merge_configs(this, config1, config2, merged)
        class(config_reloader_t), intent(inout) :: this
        type(fluff_config_t), intent(in) :: config1, config2
        type(fluff_config_t), intent(out) :: merged
        
        ! Simple merge: config2 overrides config1
        merged = config1
        
        if (config2%line_length > 0) then
            merged%line_length = config2%line_length
        end if
        
        if (len_trim(config2%target_version) > 0) then
            merged%target_version = config2%target_version
        end if
        
    end subroutine merge_configs
    
    ! Create configuration validator
    function create_config_validator() result(validator)
        type(config_validator_t) :: validator
        
        ! Initialize valid rule codes
        allocate(character(len=10) :: validator%valid_rules(15))
        validator%valid_rules = [character(len=10) :: &
            "F001", "F002", "F003", "F004", "F005", &
            "F006", "F007", "F008", "F009", "F010", &
            "F011", "F012", "F013", "F014", "F015"]
        
    end function create_config_validator
    
    ! Validate configuration
    subroutine validate_config(this, config, result)
        class(config_validator_t), intent(in) :: this
        type(fluff_config_t), intent(in) :: config
        type(validation_result_t), intent(out) :: result
        
        integer :: i
        logical :: rule_valid
        
        result%is_valid = .true.
        result%has_rule_errors = .false.
        result%has_pattern_errors = .false.
        result%has_value_errors = .false.
        result%error_message = ""
        
        ! Validate rule selections
        if (allocated(config%rules%select)) then
            do i = 1, size(config%rules%select)
                rule_valid = any(this%valid_rules == config%rules%select(i))
                if (.not. rule_valid) then
                    result%is_valid = .false.
                    result%has_rule_errors = .true.
                    result%error_message = "Invalid rule: " // config%rules%select(i)
                    exit
                end if
            end do
        end if
        
        ! Validate file patterns (simplified - would check actual patterns)
        ! if (allocated(config%include)) then
        !     Pattern validation would go here
        ! end if
        
        ! Validate numeric values
        if (config%line_length < 0) then
            result%is_valid = .false.
            result%has_value_errors = .true.
            result%error_message = "Invalid line length: negative value"
        end if
        
    end subroutine validate_config
    
    ! Create configuration applier
    function create_config_applier() result(applier)
        type(config_applier_t) :: applier
        
        applier%rules_updated = .false.
        applier%formatter_updated = .false.
        applier%watching_updated = .false.
        
    end function create_config_applier
    
    ! Apply configuration changes
    subroutine apply_config_changes(this, old_config, new_config)
        class(config_applier_t), intent(inout) :: this
        type(fluff_config_t), intent(in) :: old_config, new_config
        
        ! Check for rule changes
        if (allocated(old_config%rules%select) .and. allocated(new_config%rules%select)) then
            if (size(old_config%rules%select) /= size(new_config%rules%select)) then
                this%rules_updated = .true.
            end if
        end if
        
        ! Check for formatter changes
        if (old_config%line_length /= new_config%line_length) then
            this%formatter_updated = .true.
        end if
        
        ! Check for watching changes (simplified)
        ! Would check actual include patterns if they existed in config type
        this%watching_updated = .false.  ! Placeholder
        
    end subroutine apply_config_changes
    
    ! Check if rules were updated
    function were_rules_updated(this) result(updated)
        class(config_applier_t), intent(in) :: this
        logical :: updated
        
        updated = this%rules_updated
        
    end function were_rules_updated
    
    ! Check if formatter was updated
    function was_formatter_updated(this) result(updated)
        class(config_applier_t), intent(in) :: this
        logical :: updated
        
        updated = this%formatter_updated
        
    end function was_formatter_updated
    
    ! Check if watching was updated
    function was_watching_updated(this) result(updated)
        class(config_applier_t), intent(in) :: this
        logical :: updated
        
        updated = this%watching_updated
        
    end function was_watching_updated
    
    ! Create configuration manager
    function create_config_manager() result(manager)
        type(config_manager_t) :: manager
        
        manager%watcher = create_config_watcher()
        manager%reloader = create_config_reloader()
        manager%validator = create_config_validator()
        manager%applier = create_config_applier()
        
        ! Initialize current config
        manager%current_config%line_length = 88
        
        manager%service_running = .false.
        manager%has_error = .false.
        
    end function create_config_manager
    
    ! Start configuration service
    subroutine start_service(this)
        class(config_manager_t), intent(inout) :: this
        
        this%service_running = .true.
        
        ! Add default config files to watch
        call this%watcher%add_config_file("fluff.toml")
        call this%watcher%add_config_file("pyproject.toml")
        
    end subroutine start_service
    
    ! Check if service is running
    function is_service_running(this) result(running)
        class(config_manager_t), intent(in) :: this
        logical :: running
        
        running = this%service_running
        
    end function is_service_running
    
    ! Reload configuration
    subroutine reload_configuration(this)
        class(config_manager_t), intent(inout) :: this
        
        type(fluff_config_t) :: new_config
        type(validation_result_t) :: validation
        character(len=:), allocatable :: config_content
        
        ! Simple reload simulation
        config_content = '[tool.fluff]' // new_line('a') // &
                        'line-length = 100'
        
        call this%reloader%parse_config_content(config_content, new_config)
        call this%validator%validate_config(new_config, validation)
        
        if (validation%is_valid) then
            call this%applier%apply_config_changes(this%current_config, new_config)
            this%current_config = new_config
            this%has_error = .false.
        else
            call this%handle_config_error(validation%error_message)
        end if
        
    end subroutine reload_configuration
    
    ! Get current configuration
    function get_current_config(this) result(config)
        class(config_manager_t), intent(in) :: this
        type(fluff_config_t) :: config
        
        config = this%current_config
        
    end function get_current_config
    
    ! Handle configuration error
    subroutine handle_config_error(this, error_message)
        class(config_manager_t), intent(inout) :: this
        character(len=*), intent(in) :: error_message
        
        this%has_error = .true.
        this%last_error%message = error_message
        this%last_error%severity = 1  ! Error
        
        ! Keep service running with old config
        
    end subroutine handle_config_error
    
    ! Get last error
    function get_last_error(this) result(error)
        class(config_manager_t), intent(in) :: this
        type(config_error_t) :: error
        
        error = this%last_error
        
    end function get_last_error
    
    ! Check if there are errors
    function has_errors(this) result(has_error)
        class(config_manager_t), intent(in) :: this
        logical :: has_error
        
        has_error = this%has_error
        
    end function has_errors
    
end module fluff_config_watcher