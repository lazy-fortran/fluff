program test_diagnostic_serialization
    use fluff_core, only: source_range_t, source_location_t
    use fluff_diagnostics, only: diagnostic_t, fix_suggestion_t, text_edit_t, &
        create_diagnostic, SEVERITY_WARNING, &
        format_diagnostic_sarif
    implicit none

    call test_json_fix_output()

contains

    subroutine test_json_fix_output()
        type(diagnostic_t) :: diag
        type(fix_suggestion_t) :: fixes(1)
        type(text_edit_t) :: edits(1)
        type(source_location_t) :: start_pos, end_pos
        type(source_range_t) :: loc_range, edit_range
        character(len=:), allocatable :: json_output, sarif_output

        ! Build diagnostic
        start_pos%line = 1
        start_pos%column = 1
        end_pos%line = 1
        end_pos%column = 1
        loc_range%start = start_pos
        loc_range%end = end_pos

        diag = create_diagnostic("F001", "Missing implicit none", "test.f90", loc_range)

        ! Build fix
        edit_range%start = start_pos
        edit_range%end = end_pos
        edits(1)%range = edit_range
        edits(1)%new_text = "implicit none"

        fixes(1)%description = "add implicit none"
        fixes(1)%is_safe = .true.
        allocate(fixes(1)%edits(1))
        fixes(1)%edits(1) = edits(1)

        allocate(diag%fixes(1))
        diag%fixes(1) = fixes(1)

        ! Test JSON output
        json_output = diag%to_json()

        if (index(json_output, '"fixes"') == 0) then
            error stop "[FAIL] JSON output missing fixes field"
        end if

        if (index(json_output, '"new_text"') == 0) then
            error stop "[FAIL] JSON output missing new_text field"
        end if

        if (index(json_output, '"is_safe"') == 0) then
            error stop "[FAIL] JSON output missing is_safe field"
        end if

        print *, "[OK] JSON fix output test passed"

        ! Test SARIF output
        sarif_output = format_diagnostic_sarif(diag)

        if (index(sarif_output, '"fixes"') == 0) then
            error stop "[FAIL] SARIF output missing fixes field"
        end if

        if (index(sarif_output, '"replacements"') == 0) then
            error stop "[FAIL] SARIF output missing replacements field"
        end if

        if (index(sarif_output, '"deletedRegion"') == 0) then
            error stop "[FAIL] SARIF output missing deletedRegion field"
        end if

        print *, "[OK] SARIF fix output test passed"
        print *, "[OK] All JSON/SARIF fix output tests passed!"

    end subroutine test_json_fix_output

end program test_diagnostic_serialization
