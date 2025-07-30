module fluff_cli
    ! Command-line interface
    use fluff_core
    use fluff_linter
    use fluff_formatter
    use fluff_config
    implicit none
    private
    
    ! CLI argument types
    type, public :: cli_args_t
        character(len=:), allocatable :: command     ! "check", "format", "server"
        character(len=:), allocatable :: files(:)    ! Files to process
        logical :: fix = .false.
        logical :: diff = .false.
        logical :: watch = .false.
        logical :: version = .false.
        logical :: help = .false.
        character(len=:), allocatable :: config_file
        character(len=:), allocatable :: output_format  ! "text", "json", "sarif"
    contains
        procedure :: parse => args_parse
        procedure :: validate => args_validate
    end type cli_args_t
    
    ! CLI application
    type, public :: cli_app_t
        type(cli_args_t) :: args
        type(linter_engine_t) :: linter
        type(formatter_engine_t) :: formatter
    contains
        procedure :: run => app_run
        procedure :: print_help => app_print_help
        procedure :: print_version => app_print_version
    end type cli_app_t
    
    ! Public procedures
    public :: create_cli_app
    
contains
    
    ! Create CLI application
    function create_cli_app() result(app)
        type(cli_app_t) :: app
        
        app%linter = create_linter_engine()
        app%formatter = create_formatter_engine()
        
    end function create_cli_app
    
    ! Parse command line arguments
    subroutine args_parse(this, argc, argv)
        class(cli_args_t), intent(inout) :: this
        integer, intent(in) :: argc
        character(len=*), intent(in) :: argv(argc)
        
        integer :: i
        logical :: expecting_value
        character(len=:), allocatable :: current_flag
        
        ! Default values
        this%command = "check"
        this%output_format = "text"
        
        if (argc == 0) return
        
        ! Parse arguments
        i = 1
        expecting_value = .false.
        
        do while (i <= argc)
            if (expecting_value) then
                ! Handle flag values
                select case (current_flag)
                case ("--config")
                    this%config_file = trim(argv(i))
                case ("--output-format")
                    this%output_format = trim(argv(i))
                end select
                expecting_value = .false.
                
            else if (argv(i)(1:2) == "--") then
                ! Long flags
                select case (trim(argv(i)))
                case ("--fix")
                    this%fix = .true.
                case ("--diff")
                    this%diff = .true.
                case ("--watch")
                    this%watch = .true.
                case ("--version")
                    this%version = .true.
                case ("--help")
                    this%help = .true.
                case ("--config")
                    current_flag = "--config"
                    expecting_value = .true.
                case ("--output-format")
                    current_flag = "--output-format"
                    expecting_value = .true.
                end select
                
            else if (argv(i)(1:1) == "-") then
                ! Short flags
                select case (trim(argv(i)))
                case ("-h")
                    this%help = .true.
                case ("-v")
                    this%version = .true.
                end select
                
            else if (i == 1) then
                ! First non-flag argument is the command
                select case (trim(argv(i)))
                case ("check", "format", "server")
                    this%command = trim(argv(i))
                case default
                    ! If not a valid command, treat as file
                    if (.not. allocated(this%files)) allocate(character(len=0) :: this%files(0))
                    this%files = [this%files, trim(argv(i))]
                end select
                
            else
                ! File arguments
                if (.not. allocated(this%files)) allocate(character(len=0) :: this%files(0))
                this%files = [this%files, trim(argv(i))]
            end if
            
            i = i + 1
        end do
        
    end subroutine args_parse
    
    ! Validate arguments
    function args_validate(this) result(valid)
        class(cli_args_t), intent(in) :: this
        logical :: valid
        
        valid = .true.
        
        ! Validate command
        select case (this%command)
        case ("check", "format", "server")
            ! Valid commands
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
        
    end function args_validate
    
    ! Run the CLI application
    subroutine app_run(this, exit_code)
        class(cli_app_t), intent(inout) :: this
        integer, intent(out) :: exit_code
        
        exit_code = 0
        
        ! Handle version flag
        if (this%args%version) then
            call this%print_version()
            return
        end if
        
        ! Handle help flag
        if (this%args%help) then
            call this%print_help()
            return
        end if
        
        ! Execute command
        select case (this%args%command)
        case ("check")
            call execute_check(this, exit_code)
        case ("format")
            call execute_format(this, exit_code)
        case ("server")
            call execute_server(this, exit_code)
        end select
        
    end subroutine app_run
    
    ! Print help message
    subroutine app_print_help(this)
        class(cli_app_t), intent(in) :: this
        
        print *, "fluff - A linting and formatting tool for Fortran"
        print *, ""
        print *, "Usage: fluff [COMMAND] [OPTIONS] [FILES...]"
        print *, ""
        print *, "Commands:"
        print *, "  check     Run linting checks (default)"
        print *, "  format    Format source files"
        print *, "  server    Run LSP server"
        print *, ""
        print *, "Options:"
        print *, "  --fix             Apply fixes automatically"
        print *, "  --diff            Show diff instead of applying changes"
        print *, "  --watch           Watch files for changes"
        print *, "  --config FILE     Use configuration file"
        print *, "  --output-format   Output format: text, json, sarif"
        print *, "  -h, --help        Show this help message"
        print *, "  -v, --version     Show version information"
        
    end subroutine app_print_help
    
    ! Print version information
    subroutine app_print_version(this)
        class(cli_app_t), intent(in) :: this
        type(fluff_version_t) :: version
        
        version = get_fluff_version()
        print *, "fluff ", version%to_string()
        
    end subroutine app_print_version
    
    ! Execute check command
    subroutine execute_check(app, exit_code)
        type(cli_app_t), intent(inout) :: app
        integer, intent(out) :: exit_code
        
        ! TODO: Implement linting
        exit_code = 0
        
    end subroutine execute_check
    
    ! Execute format command
    subroutine execute_format(app, exit_code)
        type(cli_app_t), intent(inout) :: app
        integer, intent(out) :: exit_code
        
        ! TODO: Implement formatting
        exit_code = 0
        
    end subroutine execute_format
    
    ! Execute server command
    subroutine execute_server(app, exit_code)
        type(cli_app_t), intent(inout) :: app
        integer, intent(out) :: exit_code
        
        print *, "LSP server not yet implemented"
        exit_code = 1
        
    end subroutine execute_server
    
end module fluff_cli