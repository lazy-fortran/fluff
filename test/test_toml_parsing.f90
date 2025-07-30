program test_toml_parsing
    ! Test TOML configuration file parsing
    use fluff_config
    implicit none
    
    print *, "Testing TOML configuration parsing..."
    
    ! Test 1: Parse basic configuration
    call test_basic_config()
    
    ! Test 2: Parse rule selection
    call test_rule_selection()
    
    ! Test 3: Parse per-file ignores
    call test_per_file_ignores()
    
    ! Test 4: Invalid configuration handling
    call test_invalid_config()
    
    print *, "All TOML parsing tests passed!"
    
contains
    
    subroutine test_basic_config()
        type(fluff_config_t) :: config
        character(len=:), allocatable :: toml_content
        character(len=:), allocatable :: error_msg
        
        ! Sample TOML content
        toml_content = '[tool.fluff]' // new_line('a') // &
                      'fix = true' // new_line('a') // &
                      'show-fixes = true' // new_line('a') // &
                      'line-length = 100' // new_line('a') // &
                      'target-version = "2018"' // new_line('a') // &
                      'output-format = "json"'
        
        call config%from_toml_string(toml_content, error_msg)
        
        if (allocated(error_msg)) then
            error stop "Failed to parse basic config: " // error_msg
        end if
        
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
    
    subroutine test_rule_selection()
        type(fluff_config_t) :: config
        character(len=:), allocatable :: toml_content
        character(len=:), allocatable :: error_msg
        
        ! Sample TOML with rule selection
        toml_content = '[tool.fluff]' // new_line('a') // &
                      'select = ["F", "W"]' // new_line('a') // &
                      'ignore = ["F001", "W002"]' // new_line('a') // &
                      'extend-select = ["C"]'
        
        call config%from_toml_string(toml_content, error_msg)
        
        if (allocated(error_msg)) then
            error stop "Failed to parse rule selection: " // error_msg
        end if
        
        if (.not. allocated(config%rules%select)) then
            error stop "Failed: select rules should be allocated"
        end if
        
        if (size(config%rules%select) /= 2) then
            error stop "Failed: should have 2 selected rule categories"
        end if
        
        if (config%rules%select(1) /= "F" .or. config%rules%select(2) /= "W") then
            error stop "Failed: select rules not parsed correctly"
        end if
        
        print *, "  ✓ Rule selection parsing"
    end subroutine test_rule_selection
    
    subroutine test_per_file_ignores()
        type(fluff_config_t) :: config
        character(len=:), allocatable :: toml_content
        character(len=:), allocatable :: error_msg
        
        ! Sample TOML with per-file ignores
        toml_content = '[tool.fluff]' // new_line('a') // &
                      '' // new_line('a') // &
                      '[tool.fluff.per-file-ignores]' // new_line('a') // &
                      '"test/*.f90" = ["F001"]' // new_line('a') // &
                      '"legacy/*.f90" = ["F", "W"]'
        
        call config%from_toml_string(toml_content, error_msg)
        
        if (allocated(error_msg)) then
            error stop "Failed to parse per-file ignores: " // error_msg
        end if
        
        if (.not. allocated(config%rules%per_file_ignores)) then
            error stop "Failed: per-file ignores should be allocated"
        end if
        
        if (size(config%rules%per_file_ignores) /= 2) then
            error stop "Failed: should have 2 per-file ignore patterns"
        end if
        
        print *, "  ✓ Per-file ignores parsing"
    end subroutine test_per_file_ignores
    
    subroutine test_invalid_config()
        type(fluff_config_t) :: config
        character(len=:), allocatable :: toml_content
        character(len=:), allocatable :: error_msg
        
        ! Invalid TOML content
        toml_content = '[tool.fluff]' // new_line('a') // &
                      'line-length = "not a number"'
        
        call config%from_toml_string(toml_content, error_msg)
        
        if (.not. allocated(error_msg)) then
            error stop "Failed: should produce error for invalid config"
        end if
        
        if (index(error_msg, "line-length") == 0) then
            error stop "Failed: error message should mention line-length"
        end if
        
        print *, "  ✓ Invalid configuration handling"
    end subroutine test_invalid_config
    
end program test_toml_parsing