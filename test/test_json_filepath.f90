program test_json_filepath
    use fluff_core
    use fluff_diagnostics
    implicit none

    type(diagnostic_t) :: diag
    type(source_range_t) :: loc
    character(len=:), allocatable :: json
    integer :: file_idx, path_idx

    ! Setup location
    loc%start%line = 42
    loc%start%column = 10
    loc%end%line = 42
    loc%end%column = 20

    ! Create diagnostic with file_path
    diag = create_diagnostic( &
        code = "F001", &
        message = "Test message", &
        severity = SEVERITY_WARNING, &
        location = loc, &
        file_path = "src/test/file.f90" &
        )

    ! Convert to JSON
    json = diag%to_json()

    ! Check that "file" key is present
    file_idx = index(json, '"file"')
    if (file_idx == 0) then
        print '(a)', 'FAIL: "file" key not found in JSON output'
        stop 1
    end if

    ! Check that the file path is in the output
    path_idx = index(json, 'src/test/file.f90')
    if (path_idx == 0) then
        print '(a)', 'FAIL: file path not found in JSON output'
        stop 1
    end if

    ! Check that "file" appears before the path value in the correct format
    if (file_idx > path_idx) then
        print '(a)', 'FAIL: "file" key not properly positioned before path value'
        stop 1
    end if

    ! Verify file key is after category and before location
    if (index(json, '"category"') >= file_idx) then
        print '(a)', 'FAIL: "file" key not after "category"'
        stop 1
    end if

    if (index(json, '"location"') <= file_idx) then
        print '(a)', 'FAIL: "file" key not before "location"'
        stop 1
    end if

    ! Test with empty file_path (unallocated)
    diag%file_path = ""
    json = diag%to_json()

    file_idx = index(json, '"file": ""')
    if (file_idx == 0) then
        print '(a)', 'FAIL: empty file path not handled correctly'
        stop 1
    end if

    ! Test with special characters that need escaping
    diag%file_path = 'path\with"quotes\and\backslash.f90'
    json = diag%to_json()

    if (index(json, '"file"') == 0) then
        print '(a)', 'FAIL: file key missing with special chars'
        stop 1
    end if

    ! All tests passed
    print '(a)', 'PASS'
    stop 0

end program test_json_filepath
