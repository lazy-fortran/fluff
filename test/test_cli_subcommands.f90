program test_cli_subcommands
    ! Test CLI subcommand execution
    use fluff_cli
    use fluff_linter
    use fluff_formatter
    implicit none
    
    print *, "Testing CLI subcommands..."
    
    ! Test 1: Check command initialization
    call test_check_command_init()
    
    ! Test 2: Format command initialization
    call test_format_command_init()
    
    ! Test 3: Server command handling
    call test_server_command()
    
    ! Test 4: Help flag handling
    call test_help_flag()
    
    ! Test 5: Version flag handling
    call test_version_flag()
    
    print *, "All CLI subcommand tests passed!"
    
contains
    
    subroutine test_check_command_init()
        type(cli_app_t) :: app
        character(len=10) :: argv(2)
        integer :: exit_code
        
        app = create_cli_app()
        
        argv(1) = "check"
        argv(2) = "test.f90"
        
        call app%args%parse(2, argv)
        
        ! Verify linter is initialized
        if (.not. app%linter%is_initialized) then
            error stop "Failed: linter should be initialized"
        end if
        
        print *, "  OK Check command initialization"
        
    end subroutine test_check_command_init
    
    subroutine test_format_command_init()
        type(cli_app_t) :: app
        character(len=10) :: argv(2)
        
        app = create_cli_app()
        
        argv(1) = "format"
        argv(2) = "test.f90"
        
        call app%args%parse(2, argv)
        
        ! Verify formatter is initialized
        if (.not. app%formatter%is_initialized) then
            error stop "Failed: formatter should be initialized"
        end if
        
        print *, "  OK Format command initialization"
        
    end subroutine test_format_command_init
    
    subroutine test_server_command()
        type(cli_app_t) :: app
        character(len=10) :: argv(1)
        integer :: exit_code
        
        app = create_cli_app()
        
        argv(1) = "server"
        
        call app%args%parse(1, argv)
        
        if (app%args%command /= "server") then
            error stop "Failed: command should be 'server'"
        end if
        
        ! Server command should fail for now (not implemented)
        ! We just verify it's recognized
        
        print *, "  OK Server command recognized"
        
    end subroutine test_server_command
    
    subroutine test_help_flag()
        type(cli_app_t) :: app
        character(len=10) :: argv(1)
        
        app = create_cli_app()
        
        argv(1) = "--help"
        
        call app%args%parse(1, argv)
        
        if (.not. app%args%help) then
            error stop "Failed: help flag should be true"
        end if
        
        print *, "  OK Help flag handling"
        
    end subroutine test_help_flag
    
    subroutine test_version_flag()
        type(cli_app_t) :: app
        character(len=10) :: argv(1)
        
        app = create_cli_app()
        
        argv(1) = "--version"
        
        call app%args%parse(1, argv)
        
        if (.not. app%args%version) then
            error stop "Failed: version flag should be true"
        end if
        
        print *, "  OK Version flag handling"
        
    end subroutine test_version_flag
    
end program test_cli_subcommands
