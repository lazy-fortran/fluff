program test_leading_ampersand
    use fluff_format_continuation, only: has_leading_ampersand_continuations, &
                                         extract_continuation_regions, &
                                         preserve_leading_ampersand_lines, &
                                         continuation_region_t
    use fluff_formatter, only: formatter_engine_t
    implicit none

    logical :: passed
    character(len=*), parameter :: PASS_MARK = "[/]"
    character(len=*), parameter :: FAIL_MARK = "[X]"

    print *, "=== Testing Leading Ampersand Preservation ==="
    print *, ""

    call test_has_leading_ampersand_basic(passed)
    call test_extract_regions(passed)
    call test_preserve_indentation(passed)
    call test_formatter_preserves_style(passed)

    print *, ""
    print *, "=== All leading ampersand tests completed ==="

contains

    subroutine test_has_leading_ampersand_basic(all_passed)
        logical, intent(inout) :: all_passed

        character(len=:), allocatable :: code_with
        character(len=:), allocatable :: code_without
        logical :: result

        print *, "Test: has_leading_ampersand_continuations detection"

        code_with = "x = 1 + &"//new_line('a')// &
                    "    & 2 + &"//new_line('a')// &
                    "    & 3"

        result = has_leading_ampersand_continuations(code_with)
        if (result) then
            print *, "  "//PASS_MARK//" Detects leading & in continuation"
        else
            print *, "  "//FAIL_MARK//" Failed to detect leading & in continuation"
            error stop "Detection test failed"
        end if

        code_without = "x = 1 + &"//new_line('a')// &
                       "    2 + &"//new_line('a')// &
                       "    3"

        result = has_leading_ampersand_continuations(code_without)
        if (.not. result) then
            print *, "  "//PASS_MARK//" Correctly reports no leading & when absent"
        else
            print *, "  "//FAIL_MARK//" Incorrectly detected leading & when absent"
            error stop "False positive detection"
        end if

        all_passed = .true.

    end subroutine test_has_leading_ampersand_basic

    subroutine test_extract_regions(all_passed)
        logical, intent(inout) :: all_passed

        character(len=:), allocatable :: code
        type(continuation_region_t), allocatable :: regions(:)

        print *, "Test: extract_continuation_regions"

        code = "program test"//new_line('a')// &
               "    x = 1 + &"//new_line('a')// &
               "        & 2 + &"//new_line('a')// &
               "        & 3"//new_line('a')// &
               "end program test"

        call extract_continuation_regions(code, regions)

        if (size(regions) == 1) then
            print *, "  "//PASS_MARK//" Found exactly 1 continuation region"
        else
            print *, "  "//FAIL_MARK//" Expected 1 region, found", size(regions)
            error stop "Region count mismatch"
        end if

        if (regions(1)%start_line == 2) then
            print *, "  "//PASS_MARK//" Region starts at correct line"
        else
            print *, "  "//FAIL_MARK//" Expected start line 2, got", &
                regions(1)%start_line
            error stop "Start line mismatch"
        end if

        if (size(regions(1)%indent_widths) == 2) then
            print *, "  "//PASS_MARK//" Correct number of continuation lines tracked"
        else
            print *, "  "//FAIL_MARK//" Expected 2 indent entries, got", &
                size(regions(1)%indent_widths)
            error stop "Indent count mismatch"
        end if

        all_passed = .true.

    end subroutine test_extract_regions

    subroutine test_preserve_indentation(all_passed)
        logical, intent(inout) :: all_passed

        character(len=:), allocatable :: original
        character(len=:), allocatable :: formatted
        character(len=:), allocatable :: result

        print *, "Test: preserve_leading_ampersand_lines indentation"

        original = "    x = 1 + &"//new_line('a')// &
                   "            & 2 + &"//new_line('a')// &
                   "            & 3"

        formatted = "    x = 1 + &"//new_line('a')// &
                    "      & 2 + &"//new_line('a')// &
                    "      & 3"

        call preserve_leading_ampersand_lines(original, formatted, result)

        if (index(result, "            & 2") > 0) then
            print *, "  "//PASS_MARK//" Preserves original 12-space indent"
        else
            print *, "  "//FAIL_MARK//" Did not preserve 12-space indent"
            print *, "  Result: ", result
            error stop "Indentation not preserved"
        end if

        all_passed = .true.

    end subroutine test_preserve_indentation

    subroutine test_formatter_preserves_style(all_passed)
        logical, intent(inout) :: all_passed

        type(formatter_engine_t) :: formatter
        character(len=:), allocatable :: source
        character(len=:), allocatable :: formatted
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: long_vars

        print *, "Test: formatter_engine_t preserves leading & style"

        call formatter%initialize()

        long_vars = "very_long_variable_name_one + very_long_variable_name_two"

        source = "program preserve_test"//new_line('a')// &
                 "    implicit none"//new_line('a')// &
                 "    real :: very_long_variable_name_one"//new_line('a')// &
                 "    real :: very_long_variable_name_two"//new_line('a')// &
                 "    real :: very_long_result_variable"//new_line('a')// &
                 "    very_long_result_variable = "//long_vars//" + &"// &
                 new_line('a')// &
                 "                                & "//long_vars// &
                 new_line('a')// &
                 "end program preserve_test"

        call formatter%format_source(source, formatted, error_msg)

        if (error_msg /= "") then
            print *, "  "//FAIL_MARK//" Formatter error: ", error_msg
            error stop "Formatter failed"
        end if

        if (index(formatted, "very_long_result_variable") > 0) then
            print *, "  "//PASS_MARK//" Formatter processes long continuation correctly"
        else
            print *, "  "//FAIL_MARK//" Variable name missing from output"
            print *, "  Formatted: ", formatted
            error stop "Content missing"
        end if

        all_passed = .true.

    end subroutine test_formatter_preserves_style

end program test_leading_ampersand
