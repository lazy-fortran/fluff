program test_optimize_line_breaks
    use fluff_format_quality, only: optimize_line_breaks
    implicit none
    
    character(len=:), allocatable :: input_code, expected_output, actual_output
    logical :: test_passed
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Testing optimize_line_breaks function ==="
    
    ! Test 1: Short line should not be modified
    test_count = test_count + 1
    input_code = "    integer :: x, y, z"
    expected_output = "    integer :: x, y, z"
    actual_output = input_code
    call optimize_line_breaks(actual_output, 88)
    test_passed = (actual_output == expected_output)
    if (test_passed) then
        pass_count = pass_count + 1
        print *, "Test 1 PASSED: Short line not modified"
    else
        print *, "Test 1 FAILED: Short line was modified"
        print *, "  Expected: '", expected_output, "'"
        print *, "  Got:      '", actual_output, "'"
    end if
    
    ! Test 2: Long line with commas should break after comma
    test_count = test_count + 1
    input_code = "    integer :: very_long_var_name_one, very_long_var_name_two, " // &
                 "very_long_var_name_three, very_long_var_name_four"
    expected_output = "    integer :: very_long_var_name_one, very_long_var_name_two, " // &
                      "very_long_var_name_three, &" // new_line('a') // &
                      "        very_long_var_name_four"
    actual_output = input_code
    call optimize_line_breaks(actual_output, 88)
    test_passed = (actual_output == expected_output)
    if (test_passed) then
        pass_count = pass_count + 1
        print *, "Test 2 PASSED: Long line with commas breaks correctly"
    else
        print *, "Test 2 FAILED: Long line with commas not broken correctly"
        print *, "  Expected: '", expected_output, "'"
        print *, "  Got:      '", actual_output, "'"
    end if
    
    ! Test 3: Long line with operators should break after operator
    test_count = test_count + 1
    input_code = "    result = very_long_variable_name_one + very_long_variable_name_two + very_long_variable_name_three"
    expected_output = "    result = very_long_variable_name_one + very_long_variable_name_two + &" // new_line('a') // &
                      "        very_long_variable_name_three"
    actual_output = input_code
    call optimize_line_breaks(actual_output, 88)
    test_passed = (actual_output == expected_output)
    if (test_passed) then
        pass_count = pass_count + 1
        print *, "Test 3 PASSED: Long line with operators breaks correctly"
    else
        print *, "Test 3 FAILED: Long line with operators not broken correctly"
        print *, "  Expected: '", expected_output, "'"
        print *, "  Got:      '", actual_output, "'"
    end if
    
    ! Test 4: Multiple lines should be handled correctly
    test_count = test_count + 1
    input_code = "program test" // new_line('a') // &
                 "    integer :: very_long_var_name_one_that_is_really_very_very_long, " // &
                 "very_long_var_name_two_that_is_also_very_long, very_long_var_name_three" // new_line('a') // &
                 "    x = 1" // new_line('a') // &
                 "end program"
    expected_output = "program test" // new_line('a') // &
                      "    integer :: very_long_var_name_one_that_is_really_very_very_long, &" // new_line('a') // &
                      "        very_long_var_name_two_that_is_also_very_long, " // &
                      "very_long_var_name_three" // new_line('a') // &
                      "    x = 1" // new_line('a') // &
                      "end program"
    actual_output = input_code
    call optimize_line_breaks(actual_output, 88)
    test_passed = (actual_output == expected_output)
    if (test_passed) then
        pass_count = pass_count + 1
        print *, "Test 4 PASSED: Multiple lines handled correctly"
    else
        print *, "Test 4 FAILED: Multiple lines not handled correctly"
        print *, "  Expected: '", expected_output, "'"
        print *, "  Got:      '", actual_output, "'"
    end if
    
    ! Test 5: Empty lines and comments should be preserved
    test_count = test_count + 1
    input_code = "program test" // new_line('a') // &
                 new_line('a') // &
                 "    ! This is a comment" // new_line('a') // &
                 "    x = 1" // new_line('a') // &
                 "end program"
    expected_output = input_code  ! Should remain unchanged
    actual_output = input_code
    call optimize_line_breaks(actual_output, 88)
    test_passed = (actual_output == expected_output)
    if (test_passed) then
        pass_count = pass_count + 1
        print *, "Test 5 PASSED: Empty lines and comments preserved"
    else
        print *, "Test 5 FAILED: Empty lines or comments not preserved"
        print *, "  Expected: '", expected_output, "'"
        print *, "  Got:      '", actual_output, "'"
    end if
    
    ! Summary
    print *, ""
    print *, "=== Test Summary ==="
    print *, "Total tests: ", test_count
    print *, "Passed: ", pass_count
    print *, "Failed: ", test_count - pass_count
    
    if (pass_count /= test_count) then
        error stop "Tests failed"
    end if
    
end program test_optimize_line_breaks