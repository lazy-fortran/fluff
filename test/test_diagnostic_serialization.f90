program test_diagnostic_serialization
    use fluff_core, only: source_range_t, source_location_t
    use fluff_diagnostics, only: diagnostic_t, fix_suggestion_t, text_edit_t, &
        create_diagnostic, SEVERITY_WARNING, &
        format_diagnostic_sarif, diagnostic_collection_t
    use fluff_json, only: json_parse, json_get_string_member, &
        json_get_member_json, json_array_get_element_json, json_array_length
    implicit none

    call test_json_fix_output()
    call test_json_escaped_message()
    call test_collection_and_sarif_roundtrip()

contains

    subroutine test_collection_and_sarif_roundtrip()
        type(diagnostic_t) :: diag, late
        type(diagnostic_collection_t) :: collection
        type(source_range_t) :: loc
        character(len=:), allocatable :: message, path, json, value, decoded, nested
        logical :: valid, found
        integer :: count

        message = 'quoted "value" café'//new_line('a')// &
            repeat('x', 24000)//' END-COLLECTION'
        path = 'C:\quoted "directory"\café.f90'
        diag = create_diagnostic('F"001', message, path, loc)
        allocate (diag%fixes(1))
        allocate (diag%fixes(1)%edits(1))
        diag%fixes(1)%description = 'replace "value"'
        diag%fixes(1)%edits(1)%range = loc
        diag%fixes(1)%edits(1)%new_text = 'replacement "café"'//new_line('a')
        late = create_diagnostic('LATE', 'last diagnostic', 'late.f90', loc)
        call collection%add(diag)
        call collection%add(late)
        json = collection%to_json()
        call require_valid_json(json)
        call json_array_length(json, count, valid)
        if (.not. valid .or. count /= 2) error stop 'collection count changed'
        value = element(json, 1)
        call json_get_string_member(value, 'message', decoded, found, valid)
        if (.not. found .or. .not. valid) error stop 'collection message missing'
        if (decoded /= message) error stop 'collection message lost bytes'
        value = element(json, 2)
        call json_get_string_member(value, 'code', decoded, found, valid)
        if (.not. found .or. .not. valid) error stop 'late diagnostic missing'
        if (decoded /= 'LATE') error stop 'late diagnostic changed'

        json = format_diagnostic_sarif(diag)
        call require_valid_json(json)
        call check_sarif_result(json, message, path)
        value = element(member(json, 'fixes'), 1)
        nested = member(value, 'description')
        call json_get_string_member(nested, 'text', decoded, found, valid)
        if (.not. found .or. .not. valid) error stop 'fix description missing'
        if (decoded /= 'replace "value"') error stop 'fix description changed'
        value = element(member(value, 'artifactChanges'), 1)
        nested = element(member(value, 'replacements'), 1)
        nested = member(nested, 'insertedContent')
        call json_get_string_member(nested, 'text', decoded, found, valid)
        if (.not. found .or. .not. valid) error stop 'replacement missing'
        if (decoded /= 'replacement "café"'//new_line('a')) &
            error stop 'replacement changed'
        value = member(value, 'artifactLocation')
        call json_get_string_member(value, 'uri', decoded, found, valid)
        if (.not. found .or. .not. valid) error stop 'fix URI missing'
        if (decoded /= path) error stop 'fix URI changed'

        json = collection%to_sarif()
        call require_valid_json(json)
        value = element(member(json, 'runs'), 1)
        value = member(value, 'results')
        call json_array_length(value, count, valid)
        if (.not. valid .or. count /= 2) error stop 'SARIF count changed'
        value = element(value, 1)
        call check_sarif_result(value, message, path)
    end subroutine test_collection_and_sarif_roundtrip

    subroutine check_sarif_result(json, message, path)
        character(len=*), intent(in) :: json, message, path
        character(len=:), allocatable :: value, decoded
        logical :: found, valid

        call json_get_string_member(json, 'ruleId', decoded, found, valid)
        if (.not. found .or. .not. valid) error stop 'SARIF rule missing'
        if (decoded /= 'F"001') error stop 'SARIF rule changed'
        value = member(json, 'message')
        call json_get_string_member(value, 'text', decoded, found, valid)
        if (.not. found .or. .not. valid) error stop 'SARIF message missing'
        if (decoded /= message) error stop 'SARIF message lost bytes'
        value = element(member(json, 'locations'), 1)
        value = member(value, 'physicalLocation')
        value = member(value, 'artifactLocation')
        call json_get_string_member(value, 'uri', decoded, found, valid)
        if (.not. found .or. .not. valid) error stop 'SARIF URI missing'
        if (decoded /= path) error stop 'SARIF URI changed'
    end subroutine check_sarif_result

    subroutine require_valid_json(json)
        character(len=*), intent(in) :: json
        character(len=:), allocatable :: error
        logical :: valid

        call json_parse(json, valid, error)
        if (.not. valid) then
            print *, error
            error stop 'serialized diagnostic JSON is malformed'
        end if
    end subroutine require_valid_json

    function member(json, name) result(value)
        character(len=*), intent(in) :: json, name
        character(len=:), allocatable :: value
        logical :: found, valid

        call json_get_member_json(json, name, value, found, valid)
        if (.not. found .or. .not. valid) error stop 'diagnostic member missing'
    end function member

    function element(json, index) result(value)
        character(len=*), intent(in) :: json
        integer, intent(in) :: index
        character(len=:), allocatable :: value
        logical :: found, valid

        call json_array_get_element_json(json, index, value, found, valid)
        if (.not. found .or. .not. valid) error stop 'diagnostic element missing'
    end function element

    subroutine test_json_escaped_message()
        type(diagnostic_t) :: diag
        type(source_range_t) :: loc
        character(len=:), allocatable :: message, json, decoded, error
        logical :: valid, found

        message = 'quoted "value" at C:\path café'//new_line('a')// &
            repeat('x', 24000)//' END-MESSAGE'
        diag = create_diagnostic('F001', message, 'quoted "file".f90', loc)
        json = diag%to_json()
        call json_parse(json, valid, error)
        if (.not. valid) error stop 'diagnostic with quotes must be valid JSON'
        call json_get_string_member(json, 'message', decoded, found, valid)
        if (.not. found .or. .not. valid) error stop 'diagnostic message missing'
        if (decoded /= message) error stop 'diagnostic message lost bytes'
        call json_get_string_member(json, 'file', decoded, found, valid)
        if (.not. found .or. .not. valid) error stop 'diagnostic file missing'
        if (decoded /= 'quoted "file".f90') error stop 'diagnostic file lost bytes'
    end subroutine test_json_escaped_message

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
