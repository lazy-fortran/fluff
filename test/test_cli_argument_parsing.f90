program test_cli_argument_parsing
    ! Test CLI argument parsing functionality
    use fluff_cli
    implicit none
    
    type(cli_args_t) :: args
    logical :: test_passed
    
    print *, "Testing CLI argument parsing..."
    
    ! Test 1: Parse basic check command
    call test_basic_check_command()
    
    ! Test 2: Parse format command with options
    call test_format_command_with_options()
    
    ! Test 3: Parse multiple files
    call test_multiple_files()
    
    ! Test 4: Parse config file option
    call test_config_file_option()
    
    ! Test 5: Parse output format
    call test_output_format()
    
    print *, "All CLI argument parsing tests passed!"
    
contains
    
    subroutine test_basic_check_command()
        type(cli_args_t) :: args
        character(len=10) :: argv(2)
        
        argv(1) = "check"
        argv(2) = "test.f90"
        
        call args%parse(2, argv)
        
        if (args%command /= "check") then
            error stop "Failed: command should be 'check'"
        end if
        
        if (.not. allocated(args%files)) then
            error stop "Failed: files should be allocated"
        end if
        
        if (size(args%files) /= 1) then
            error stop "Failed: should have 1 file"
        end if
        
        if (args%files(1) /= "test.f90") then
            error stop "Failed: file should be 'test.f90'"
        end if
        
        print *, "  ✓ Basic check command parsing"
        
    end subroutine test_basic_check_command
    
    subroutine test_format_command_with_options()
        type(cli_args_t) :: args
        character(len=10) :: argv(4)
        
        argv(1) = "format"
        argv(2) = "--diff"
        argv(3) = "--fix"
        argv(4) = "test.f90"
        
        call args%parse(4, argv)
        
        if (args%command /= "format") then
            error stop "Failed: command should be 'format'"
        end if
        
        if (.not. args%diff) then
            error stop "Failed: diff should be true"
        end if
        
        if (.not. args%fix) then
            error stop "Failed: fix should be true"
        end if
        
        print *, "  ✓ Format command with options"
        
    end subroutine test_format_command_with_options
    
    subroutine test_multiple_files()
        type(cli_args_t) :: args
        character(len=10) :: argv(4)
        
        argv(1) = "check"
        argv(2) = "file1.f90"
        argv(3) = "file2.f90"
        argv(4) = "file3.f90"
        
        call args%parse(4, argv)
        
        if (size(args%files) /= 3) then
            error stop "Failed: should have 3 files"
        end if
        
        if (args%files(1) /= "file1.f90" .or. &
            args%files(2) /= "file2.f90" .or. &
            args%files(3) /= "file3.f90") then
            error stop "Failed: incorrect files parsed"
        end if
        
        print *, "  ✓ Multiple files parsing"
        
    end subroutine test_multiple_files
    
    subroutine test_config_file_option()
        type(cli_args_t) :: args
        character(len=20) :: argv(4)
        
        argv(1) = "check"
        argv(2) = "--config"
        argv(3) = "fluff.toml"
        argv(4) = "test.f90"
        
        call args%parse(4, argv)
        
        if (args%config_file /= "fluff.toml") then
            error stop "Failed: config file should be 'fluff.toml'"
        end if
        
        print *, "  ✓ Config file option parsing"
        
    end subroutine test_config_file_option
    
    subroutine test_output_format()
        type(cli_args_t) :: args
        character(len=20) :: argv(4)
        
        argv(1) = "check"
        argv(2) = "--output-format"
        argv(3) = "json"
        argv(4) = "test.f90"
        
        call args%parse(4, argv)
        
        if (args%output_format /= "json") then
            error stop "Failed: output format should be 'json'"
        end if
        
        print *, "  ✓ Output format parsing"
        
    end subroutine test_output_format
    
end program test_cli_argument_parsing