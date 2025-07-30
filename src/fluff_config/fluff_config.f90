module fluff_config
    ! Configuration management
    use fluff_core
    implicit none
    private
    
    ! Per-file ignore patterns
    type, public :: per_file_ignore_t
        character(len=:), allocatable :: pattern    ! File glob pattern
        character(len=:), allocatable :: rules(:)   ! Rules to ignore
    end type per_file_ignore_t
    
    ! Rule selection configuration
    type, public :: rule_selection_t
        character(len=:), allocatable :: select(:)        ! Rules to enable
        character(len=:), allocatable :: ignore(:)        ! Rules to disable
        character(len=:), allocatable :: extend_select(:) ! Additional rules to enable
        type(per_file_ignore_t), allocatable :: per_file_ignores(:)
    contains
        procedure :: is_rule_enabled => selection_is_rule_enabled
    end type rule_selection_t
    
    ! Configuration type
    type, public :: fluff_config_t
        logical :: fix = .false.
        logical :: show_fixes = .false.
        integer :: line_length = 88
        character(len=:), allocatable :: target_version   ! "2008", "2018", "2023"
        character(len=:), allocatable :: output_format    ! "text", "json", "sarif"
        type(rule_selection_t) :: rules
    contains
        procedure :: from_file => config_from_file
        procedure :: from_cli_args => config_from_cli_args
        procedure :: validate => config_validate
        procedure :: merge => config_merge
    end type fluff_config_t
    
    ! Public procedures
    public :: create_default_config
    public :: load_config
    
contains
    
    ! Create default configuration
    function create_default_config() result(config)
        type(fluff_config_t) :: config
        
        config%fix = .false.
        config%show_fixes = .false.
        config%line_length = 88
        config%target_version = "2018"
        config%output_format = "text"
        
        ! Default rule selection - enable all style rules
        allocate(character(len=4) :: config%rules%select(1))
        config%rules%select(1) = "F"  ! All F-prefixed rules
        
    end function create_default_config
    
    ! Load configuration from file and CLI
    function load_config(config_file, cli_args) result(config)
        character(len=*), intent(in), optional :: config_file
        type(*), intent(in), optional :: cli_args
        type(fluff_config_t) :: config
        
        ! Start with defaults
        config = create_default_config()
        
        ! Load from file if provided
        if (present(config_file)) then
            call config%from_file(config_file)
        end if
        
        ! Apply CLI overrides if provided
        if (present(cli_args)) then
            call config%from_cli_args(cli_args)
        end if
        
        ! Validate final configuration
        if (.not. config%validate()) then
            ! Reset to defaults if invalid
            config = create_default_config()
        end if
        
    end function load_config
    
    ! Load configuration from TOML file
    subroutine config_from_file(this, filename)
        class(fluff_config_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        ! TODO: Implement TOML parsing
        ! For now, just keep defaults
        
    end subroutine config_from_file
    
    ! Apply CLI argument overrides
    subroutine config_from_cli_args(this, cli_args)
        class(fluff_config_t), intent(inout) :: this
        type(*), intent(in) :: cli_args
        
        ! TODO: Apply CLI overrides
        ! For now, just keep current values
        
    end subroutine config_from_cli_args
    
    ! Validate configuration
    function config_validate(this) result(valid)
        class(fluff_config_t), intent(in) :: this
        logical :: valid
        
        valid = .true.
        
        ! Validate line length
        if (this%line_length < 40 .or. this%line_length > 200) then
            valid = .false.
        end if
        
        ! Validate target version
        select case (this%target_version)
        case ("2008", "2018", "2023")
            ! Valid versions
        case default
            valid = .false.
        end select
        
        ! Validate output format
        select case (this%output_format)
        case ("text", "json", "sarif")
            ! Valid formats
        case default
            valid = .false.
        end select
        
    end function config_validate
    
    ! Merge two configurations (other overrides this)
    subroutine config_merge(this, other)
        class(fluff_config_t), intent(inout) :: this
        type(fluff_config_t), intent(in) :: other
        
        ! Merge simple fields
        this%fix = other%fix
        this%show_fixes = other%show_fixes
        this%line_length = other%line_length
        
        if (allocated(other%target_version)) then
            this%target_version = other%target_version
        end if
        
        if (allocated(other%output_format)) then
            this%output_format = other%output_format
        end if
        
        ! TODO: Merge rule selections
        
    end subroutine config_merge
    
    ! Check if a rule is enabled
    function selection_is_rule_enabled(this, rule_code) result(enabled)
        class(rule_selection_t), intent(in) :: this
        character(len=*), intent(in) :: rule_code
        logical :: enabled
        
        integer :: i
        
        enabled = .false.
        
        ! Check if rule matches any select pattern
        if (allocated(this%select)) then
            do i = 1, size(this%select)
                if (rule_matches_pattern(rule_code, this%select(i))) then
                    enabled = .true.
                    exit
                end if
            end do
        end if
        
        ! Check if rule is explicitly ignored
        if (enabled .and. allocated(this%ignore)) then
            do i = 1, size(this%ignore)
                if (rule_matches_pattern(rule_code, this%ignore(i))) then
                    enabled = .false.
                    exit
                end if
            end do
        end if
        
    end function selection_is_rule_enabled
    
    ! Check if rule code matches pattern
    function rule_matches_pattern(rule_code, pattern) result(matches)
        character(len=*), intent(in) :: rule_code
        character(len=*), intent(in) :: pattern
        logical :: matches
        
        ! Simple prefix matching for now
        if (len(pattern) <= len(rule_code)) then
            matches = rule_code(1:len(pattern)) == pattern
        else
            matches = .false.
        end if
        
    end function rule_matches_pattern
    
end module fluff_config