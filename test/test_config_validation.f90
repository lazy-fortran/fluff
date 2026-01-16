program test_config_validation
    ! Test configuration validation
    use fluff_config
    implicit none
    
    print *, "Testing configuration validation..."
    
    ! Test 1: Validate line length
    call test_line_length_validation()
    
    ! Test 2: Validate target version
    call test_target_version_validation()
    
    ! Test 3: Validate output format
    call test_output_format_validation()
    
    ! Test 4: Validate rule codes
    call test_rule_code_validation()
    
    print *, "[OK] All configuration validation tests passed!"
    
contains
    
    subroutine test_line_length_validation()
        type(fluff_config_t) :: config
        logical :: is_valid
        character(len=:), allocatable :: error_msg
        
        ! Test valid line length
        config%line_length = 88
        is_valid = config%validate(error_msg)
        
        if (.not. is_valid) then
            error stop "Failed: line length 88 should be valid"
        end if
        
        ! Test invalid line length (too small)
        config%line_length = 0
        is_valid = config%validate(error_msg)
        
        if (is_valid) then
            error stop "Failed: line length 0 should be invalid"
        end if
        
        ! Test invalid line length (too large)
        config%line_length = 1000
        is_valid = config%validate(error_msg)
        
        if (is_valid) then
            error stop "Failed: line length 1000 should be invalid"
        end if
        
        print *, "[OK] Line length validation"
    end subroutine test_line_length_validation
    
    subroutine test_target_version_validation()
        type(fluff_config_t) :: config
        logical :: is_valid
        character(len=:), allocatable :: error_msg
        
        ! Test valid versions
        config%target_version = "2008"
        is_valid = config%validate(error_msg)
        if (.not. is_valid) then
            error stop "Failed: version 2008 should be valid"
        end if
        
        config%target_version = "2018"
        is_valid = config%validate(error_msg)
        if (.not. is_valid) then
            error stop "Failed: version 2018 should be valid"
        end if
        
        config%target_version = "2023"
        is_valid = config%validate(error_msg)
        if (.not. is_valid) then
            error stop "Failed: version 2023 should be valid"
        end if
        
        ! Test invalid version
        config%target_version = "1995"
        is_valid = config%validate(error_msg)
        if (is_valid) then
            error stop "Failed: version 1995 should be invalid"
        end if
        
        print *, "[OK] Target version validation"
    end subroutine test_target_version_validation
    
    subroutine test_output_format_validation()
        type(fluff_config_t) :: config
        logical :: is_valid
        character(len=:), allocatable :: error_msg
        
        ! Test valid formats
        config%output_format = "text"
        is_valid = config%validate(error_msg)
        if (.not. is_valid) then
            error stop "Failed: format 'text' should be valid"
        end if
        
        config%output_format = "json"
        is_valid = config%validate(error_msg)
        if (.not. is_valid) then
            error stop "Failed: format 'json' should be valid"
        end if
        
        config%output_format = "sarif"
        is_valid = config%validate(error_msg)
        if (.not. is_valid) then
            error stop "Failed: format 'sarif' should be valid"
        end if
        
        ! Test invalid format
        config%output_format = "xml"
        is_valid = config%validate(error_msg)
        if (is_valid) then
            error stop "Failed: format 'xml' should be invalid"
        end if
        
        print *, "[OK] Output format validation"
    end subroutine test_output_format_validation
    
    subroutine test_rule_code_validation()
        type(fluff_config_t) :: config
        logical :: is_valid
        character(len=:), allocatable :: error_msg
        
        ! Allocate rule arrays
        allocate(character(len=10) :: config%rules%select(3))
        
        ! Test valid rule codes
        config%rules%select(1) = "F"
        config%rules%select(2) = "F001"
        config%rules%select(3) = "W002"
        
        is_valid = config%validate(error_msg)
        if (.not. is_valid) then
            error stop "Failed: rule codes should be valid"
        end if
        
        ! Test invalid rule code
        config%rules%select(3) = "X999"
        is_valid = config%validate(error_msg)
        
        if (is_valid) then
            error stop "Failed: rule code X999 should be invalid"
        end if
        
        print *, "[OK] Rule code validation"
    end subroutine test_rule_code_validation
    
end program test_config_validation
