program test_cli_override
    ! Test CLI override behavior for configuration
    use fluff_config
    use fluff_cli
    implicit none
    
    print *, "Testing CLI configuration override..."
    
    ! Test 1: CLI overrides file config
    call test_cli_overrides_file()
    
    ! Test 2: Merge configuration sources
    call test_config_merge()
    
    ! Test 3: Priority order
    call test_priority_order()
    
    print *, "All CLI override tests passed!"
    
contains
    
    subroutine test_cli_overrides_file()
        type(fluff_config_t) :: file_config, cli_config, final_config
        type(cli_args_t) :: args
        
        ! Set up file config
        file_config%fix = .false.
        file_config%line_length = 80
        file_config%output_format = "text"
        
        ! Set up CLI args
        args%fix = .true.
        args%output_format = "json"
        
        ! Create CLI config from args
        call cli_config%from_cli_args(args)
        
        ! Merge configs (CLI should override)
        call final_config%merge(file_config, cli_config)
        
        if (.not. final_config%fix) then
            print *, "ERROR: CLI fix=true should override file fix=false"
            return
        end if
        
        if (final_config%output_format /= "json") then
            print *, "ERROR: CLI output format should override file format"
            return
        end if
        
        if (final_config%line_length /= 80) then
            print *, "ERROR: line_length should remain from file when not in CLI"
            return
        end if
        
        print *, "  ✓ CLI overrides file configuration"
    end subroutine test_cli_overrides_file
    
    subroutine test_config_merge()
        type(fluff_config_t) :: base, override, merged
        
        ! Set up base config
        base%fix = .false.
        base%show_fixes = .true.
        base%line_length = 88
        base%target_version = "2008"
        allocate(character(len=10) :: base%rules%select(2))
        base%rules%select(1) = "F"
        base%rules%select(2) = "W"
        
        ! Set up override config
        override%fix = .true.
        override%line_length = 100
        allocate(character(len=10) :: override%rules%ignore(1))
        override%rules%ignore(1) = "F001"
        
        ! Merge configs
        call merged%merge(base, override)
        
        ! Check merged values
        if (.not. merged%fix) then
            print *, "ERROR: fix should be overridden to true"
            return
        end if
        
        if (.not. merged%show_fixes) then
            print *, "ERROR: show_fixes should be preserved from base"
            return
        end if
        
        if (merged%line_length /= 100) then
            print *, "ERROR: line_length should be overridden to 100"
            return
        end if
        
        if (merged%target_version /= "2008") then
            print *, "ERROR: target_version should be preserved from base"
            return
        end if
        
        if (.not. allocated(merged%rules%select)) then
            print *, "ERROR: select rules should be preserved"
            return
        end if
        
        if (.not. allocated(merged%rules%ignore)) then
            print *, "ERROR: ignore rules should be added"
            return
        end if
        
        print *, "  ✓ Configuration merge"
    end subroutine test_config_merge
    
    subroutine test_priority_order()
        type(fluff_config_t) :: default, file, cli, final
        
        ! Default config
        default = create_default_config()
        
        ! File config (partial override)
        file%line_length = 100
        file%target_version = "2018"
        
        ! CLI config (highest priority)
        cli%line_length = 120
        
        ! Apply in order: default -> file -> cli
        call final%merge(default, file)
        call final%merge(final, cli)
        
        if (final%line_length /= 120) then
            print *, "ERROR: CLI should have highest priority"
            return
        end if
        
        if (final%target_version /= "2018") then
            print *, "ERROR: File config should override defaults"
            return
        end if
        
        print *, "  ✓ Configuration priority order"
    end subroutine test_priority_order
    
end program test_cli_override