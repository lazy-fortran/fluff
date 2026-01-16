program test_config_schema
    ! Test configuration schema validation and documentation
    use fluff_config
    implicit none
    
    print *, "Testing configuration schema validation..."
    
    ! Test 1: Comprehensive validation messages
    call test_validation_messages()
    
    ! Test 2: Configuration defaults
    call test_config_defaults()
    
    ! Test 3: Configuration profiles
    call test_config_profiles()
    
    ! Test 4: Schema documentation
    call test_schema_documentation()
    
    print *, "[OK] All configuration schema tests passed!"
    
contains
    
    subroutine test_validation_messages()
        type(fluff_config_t) :: config
        character(len=:), allocatable :: error_msg
        logical :: is_valid
        
        ! Test detailed line length error
        config%line_length = 1000
        is_valid = config%validate(error_msg)
        
        if (is_valid) then
            error stop "Failed: should fail validation"
        end if
        
        if (index(error_msg, "line-length") == 0) then
            error stop "Failed: error should mention line-length"
        end if
        
        if (index(error_msg, "40") == 0 .or. index(error_msg, "200") == 0) then
            error stop "Failed: error should include valid range"
        end if
        
        ! Test invalid rule code error
        allocate(character(len=10) :: config%rules%select(1))
        config%rules%select(1) = "Z999"
        config%line_length = 88  ! Fix line length
        
        is_valid = config%validate(error_msg)
        
        if (is_valid) then
            error stop "Failed: should fail validation for invalid rule"
        end if
        
        if (index(error_msg, "Z999") == 0) then
            error stop "Failed: error should mention the invalid rule code"
        end if
        
        print *, "[OK] Comprehensive validation messages"
    end subroutine test_validation_messages
    
    subroutine test_config_defaults()
        type(fluff_config_t) :: config
        
        config = create_default_config()
        
        ! Check all defaults are sensible
        if (config%fix) then
            error stop "Failed: fix should default to false"
        end if
        
        if (config%line_length /= 88) then
            error stop "Failed: line_length should default to 88"
        end if
        
        if (.not. allocated(config%target_version)) then
            error stop "Failed: target_version should have default"
        end if
        
        if (config%target_version /= "2018") then
            error stop "Failed: target_version should default to 2018"
        end if
        
        if (.not. allocated(config%output_format)) then
            error stop "Failed: output_format should have default"
        end if
        
        if (config%output_format /= "text") then
            error stop "Failed: output_format should default to text"
        end if
        
        print *, "[OK] Configuration defaults"
    end subroutine test_config_defaults
    
    subroutine test_config_profiles()
        type(fluff_config_t) :: config
        character(len=:), allocatable :: profile_name
        
        ! Test loading pre-defined profiles
        profile_name = "strict"
        config = load_config_profile(profile_name)
        
        if (config%line_length /= 80) then
            error stop "Failed: strict profile should use 80 char lines"
        end if
        
        ! Test performance profile
        profile_name = "performance"
        config = load_config_profile(profile_name)
        
        if (.not. allocated(config%rules%select)) then
            error stop "Failed: performance profile should select rules"
        end if
        
        print *, "[OK] Configuration profiles"
    end subroutine test_config_profiles
    
    subroutine test_schema_documentation()
        character(len=:), allocatable :: schema_doc
        
        ! Get schema documentation
        schema_doc = get_config_schema_doc()
        
        if (.not. allocated(schema_doc)) then
            error stop "Failed: schema documentation should be available"
        end if
        
        ! Check it contains key information
        if (index(schema_doc, "line-length") == 0) then
            error stop "Failed: schema should document line-length"
        end if
        
        if (index(schema_doc, "target-version") == 0) then
            error stop "Failed: schema should document target-version"
        end if
        
        if (index(schema_doc, "[tool.fluff]") == 0) then
            error stop "Failed: schema should show TOML structure"
        end if
        
        print *, "[OK] Schema documentation"
    end subroutine test_schema_documentation
    
end program test_config_schema
