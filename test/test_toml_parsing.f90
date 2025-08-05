program test_toml_parsing
    ! Test namelist configuration file parsing
    use fluff_config
    implicit none
    
    print *, "Testing namelist configuration parsing..."
    
    ! Test 1: Parse basic configuration
    call test_basic_config()
    
    ! Test 2: Invalid configuration handling  
    call test_invalid_config()
    
    print *, "All namelist parsing tests passed!"
    
contains
    
    subroutine test_basic_config()
        type(fluff_config_t) :: config
        character(len=:), allocatable :: toml_content
        character(len=:), allocatable :: error_msg
        
        ! Sample namelist content
        toml_content = '&fluff_config' // new_line('a') // &
                      '  fix = .true.' // new_line('a') // &
                      '  show_fixes = .true.' // new_line('a') // &
                      '  line_length = 100' // new_line('a') // &
                      '  target_version = "2018"' // new_line('a') // &
                      '  output_format = "json"' // new_line('a') // &
                      '/'
        
        call config%from_toml_string(toml_content, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Config content was:"
            print *, toml_content
            print *, "Error message:"
            print *, error_msg
            error stop "Failed to parse basic config: " // error_msg
        end if
        
        print *, "Config after parsing:"
        print *, "fix =", config%fix
        print *, "show_fixes =", config%show_fixes
        print *, "line_length =", config%line_length
        print *, "target_version =", config%target_version
        print *, "output_format =", config%output_format
        
        if (.not. config%fix) then
            error stop "Failed: fix should be true"
        end if
        
        if (.not. config%show_fixes) then
            error stop "Failed: show_fixes should be true"
        end if
        
        if (config%line_length /= 100) then
            error stop "Failed: line_length should be 100"
        end if
        
        if (config%target_version /= "2018") then
            error stop "Failed: target_version should be 2018"
        end if
        
        if (config%output_format /= "json") then
            error stop "Failed: output_format should be json"
        end if
        
        print *, "  ✓ Basic configuration parsing"
    end subroutine test_basic_config
    
    
    subroutine test_invalid_config()
        type(fluff_config_t) :: config
        character(len=:), allocatable :: toml_content
        character(len=:), allocatable :: error_msg
        
        ! Invalid namelist content
        toml_content = '&fluff_config' // new_line('a') // &
                      '  line_length = "not a number"' // new_line('a') // &
                      '/'
        
        call config%from_toml_string(toml_content, error_msg)
        
        if (.not. allocated(error_msg) .or. len_trim(error_msg) == 0) then
            error stop "Failed: should produce error for invalid config"
        end if
        
        if (index(error_msg, "configuration") == 0) then
            error stop "Failed: error message should mention configuration"
        end if
        
        print *, "  ✓ Invalid configuration handling"
    end subroutine test_invalid_config
    
end program test_toml_parsing