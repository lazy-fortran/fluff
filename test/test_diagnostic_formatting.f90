program test_diagnostic_formatting
    ! RED: Test diagnostic formatting functionality
    use fluff_core
    use fluff_diagnostics
    implicit none
    
    print *, "Testing diagnostic formatting (RED phase)..."
    
    ! Test 1: Basic diagnostic formatting
    call test_basic_diagnostic_formatting()
    
    ! Test 2: Source code snippets in diagnostics
    call test_source_code_snippets()
    
    ! Test 3: Multiple output formats
    call test_multiple_output_formats()
    
    ! Test 4: Severity level formatting
    call test_severity_level_formatting()
    
    ! Test 5: Diagnostic with fix suggestions
    call test_diagnostic_with_fixes()
    
    print *, "All diagnostic formatting tests passed!"
    
contains
    
    subroutine test_basic_diagnostic_formatting()
        type(diagnostic_t) :: diagnostic
        character(len=:), allocatable :: formatted_output
        
        print *, "  🔧 Testing basic diagnostic formatting..."
        
        ! Create a basic diagnostic
        diagnostic%code = "F001"
        diagnostic%message = "Missing 'implicit none' statement"
        diagnostic%category = "style"
        diagnostic%severity = SEVERITY_WARNING
        diagnostic%location%start%line = 1
        diagnostic%location%start%column = 1
        diagnostic%location%end%line = 1
        diagnostic%location%end%column = 15
        
        ! Test formatting
        formatted_output = format_diagnostic(diagnostic, OUTPUT_FORMAT_TEXT)
        
        ! Check that formatted output contains expected elements
        if (index(formatted_output, "F001") == 0) then
            error stop "Formatted output should contain rule code F001"
        end if
        
        if (index(formatted_output, "Missing 'implicit none' statement") == 0) then
            error stop "Formatted output should contain diagnostic message"
        end if
        
        if (index(formatted_output, "1:1") == 0) then
            error stop "Formatted output should contain location information"
        end if
        
        print *, "    ✓ Basic diagnostic formatting"
        
    end subroutine test_basic_diagnostic_formatting
    
    subroutine test_source_code_snippets()
        type(diagnostic_t) :: diagnostic
        character(len=:), allocatable :: formatted_output
        character(len=:), allocatable :: source_lines
        
        print *, "  🔧 Testing source code snippets in diagnostics..."
        
        ! Create diagnostic with source context
        diagnostic%code = "F003"
        diagnostic%message = "Line exceeds maximum length (88 characters)"
        diagnostic%category = "style"
        diagnostic%severity = SEVERITY_INFO
        diagnostic%location%start%line = 5
        diagnostic%location%start%column = 89
        diagnostic%location%end%line = 5
        diagnostic%location%end%column = 120
        
        ! Mock source lines
        source_lines = "program long_line_example" // new_line('a') // &
                      "    implicit none" // new_line('a') // &
                      "    integer :: i" // new_line('a') // &
                      "    real :: result" // new_line('a') // &
                      "    result = some_very_long_function_name_that_exceeds_the_maximum_line_length_limit(i, 42)" // new_line('a') // &
                      "end program long_line_example"
        
        ! Test formatting with source context
        formatted_output = format_diagnostic_with_source(diagnostic, source_lines, OUTPUT_FORMAT_TEXT)
        
        ! Check that formatted output contains source snippet
        if (index(formatted_output, "some_very_long_function_name") == 0) then
            error stop "Formatted output should contain source code snippet"
        end if
        
        ! Check that it contains line numbers
        if (index(formatted_output, "5 |") == 0) then
            error stop "Formatted output should contain line numbers"
        end if
        
        print *, "    ✓ Source code snippets in diagnostics"
        
    end subroutine test_source_code_snippets
    
    subroutine test_multiple_output_formats()
        type(diagnostic_t) :: diagnostic
        character(len=:), allocatable :: text_output, json_output, sarif_output
        
        print *, "  🔧 Testing multiple output formats..."
        
        ! Create diagnostic
        diagnostic%code = "P001"
        diagnostic%message = "Non-contiguous array access pattern detected"
        diagnostic%category = "performance"
        diagnostic%severity = SEVERITY_WARNING
        diagnostic%location%start%line = 10
        diagnostic%location%start%column = 12
        diagnostic%location%end%line = 10
        diagnostic%location%end%column = 25
        
        ! Test different output formats
        text_output = format_diagnostic(diagnostic, OUTPUT_FORMAT_TEXT)
        json_output = format_diagnostic(diagnostic, OUTPUT_FORMAT_JSON)
        sarif_output = format_diagnostic(diagnostic, OUTPUT_FORMAT_SARIF)
        
        ! Verify text format
        if (index(text_output, "P001") == 0) then
            error stop "Text output should contain rule code"
        end if
        
        ! Verify JSON format
        if (index(json_output, '"code"') == 0 .or. index(json_output, '"P001"') == 0) then
            error stop "JSON output should be valid JSON with code field"
        end if
        
        ! Verify SARIF format
        if (index(sarif_output, '"ruleId"') == 0 .or. index(sarif_output, '"P001"') == 0) then
            error stop "SARIF output should be valid SARIF with ruleId field"
        end if
        
        print *, "    ✓ Multiple output formats (text, JSON, SARIF)"
        
    end subroutine test_multiple_output_formats
    
    subroutine test_severity_level_formatting()
        type(diagnostic_t) :: diagnostic_error, diagnostic_warning, diagnostic_info
        character(len=:), allocatable :: error_output, warning_output, info_output
        
        print *, "  🔧 Testing severity level formatting..."
        
        ! Create diagnostics with different severity levels
        diagnostic_error%code = "C001"
        diagnostic_error%message = "Undefined variable usage"
        diagnostic_error%category = "correctness"
        diagnostic_error%severity = SEVERITY_ERROR
        
        diagnostic_warning%code = "F006"
        diagnostic_warning%message = "Unused variable declaration"
        diagnostic_warning%category = "style"
        diagnostic_warning%severity = SEVERITY_WARNING
        
        diagnostic_info%code = "P007"
        diagnostic_info%message = "Mixed precision arithmetic"
        diagnostic_info%category = "performance"
        diagnostic_info%severity = SEVERITY_INFO
        
        ! Test severity formatting
        error_output = format_diagnostic(diagnostic_error, OUTPUT_FORMAT_TEXT)
        warning_output = format_diagnostic(diagnostic_warning, OUTPUT_FORMAT_TEXT)
        info_output = format_diagnostic(diagnostic_info, OUTPUT_FORMAT_TEXT)
        
        ! Check severity indicators
        if (index(error_output, "error") == 0 .and. index(error_output, "ERROR") == 0) then
            error stop "Error diagnostic should contain error indicator"
        end if
        
        if (index(warning_output, "warning") == 0 .and. index(warning_output, "WARNING") == 0) then
            error stop "Warning diagnostic should contain warning indicator"
        end if
        
        if (index(info_output, "info") == 0 .and. index(info_output, "INFO") == 0) then
            error stop "Info diagnostic should contain info indicator"
        end if
        
        print *, "    ✓ Severity level formatting (error, warning, info)"
        
    end subroutine test_severity_level_formatting
    
    subroutine test_diagnostic_with_fixes()
        type(diagnostic_t) :: diagnostic
        type(fix_suggestion_t) :: fix
        type(text_edit_t) :: edit
        character(len=:), allocatable :: formatted_output
        
        print *, "  🔧 Testing diagnostic with fix suggestions..."
        
        ! Create diagnostic with fix
        diagnostic%code = "F001"
        diagnostic%message = "Missing 'implicit none' statement"
        diagnostic%category = "style"
        diagnostic%severity = SEVERITY_WARNING
        diagnostic%location%start%line = 1
        diagnostic%location%start%column = 1
        diagnostic%location%end%line = 1
        diagnostic%location%end%column = 15
        
        ! Create fix suggestion  
        fix%description = "Add 'implicit none' statement"
        fix%is_safe = .true.
        
        ! Create text edit
        edit%range%start%line = 2
        edit%range%start%column = 1
        edit%range%end%line = 2
        edit%range%end%column = 1
        edit%new_text = "    implicit none" // new_line('a')
        
        allocate(fix%edits(1))
        fix%edits(1) = edit
        
        allocate(diagnostic%fixes(1))
        diagnostic%fixes(1) = fix
        
        ! Test formatting with fixes
        formatted_output = format_diagnostic(diagnostic, OUTPUT_FORMAT_TEXT)
        
        ! Check that fix information is included
        if (index(formatted_output, "Add 'implicit none' statement") == 0) then
            error stop "Formatted output should contain fix description"
        end if
        
        print *, "    ✓ Diagnostic with fix suggestions"
        
    end subroutine test_diagnostic_with_fixes
    
end program test_diagnostic_formatting