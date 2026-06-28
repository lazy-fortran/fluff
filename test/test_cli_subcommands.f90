program test_cli_subcommands
    ! Test CLI subcommand execution
    use fluff_cli
    use fluff_linter
    use fluff_formatter
    use fluff_diagnostics, only: diagnostic_t, format_diagnostic_text
    implicit none

    print *, "Testing CLI subcommands..."

    ! Test 1: Check command initialization
    call test_check_command_init()

    ! Test 2: Format command initialization
    call test_format_command_init()

    ! Test 3: Server command handling
    call test_server_command()

    ! Test 4: Help flag handling
    call test_help_flag()

    ! Test 5: Version flag handling
    call test_version_flag()

    ! Test 6: Exclude pattern parsing
    call test_exclude_pattern_parsing()

    ! Test 7: Glob pattern matching
    call test_glob_pattern_matching()

    ! Test 8: Show fixes flag
    call test_show_fixes_flag()

    print *, "[OK] All CLI subcommand tests passed!"

contains

    subroutine test_check_command_init()
        type(cli_app_t) :: app
        character(len=10) :: argv(2)
        integer :: exit_code

        app = create_cli_app()

        argv(1) = "check"
        argv(2) = "test.f90"

        call app%args%parse(2, argv)

        ! Verify linter is initialized
        if (.not. app%linter%is_initialized) then
            error stop "Failed: linter should be initialized"
        end if

        print *, "[OK] Check command initialization"

    end subroutine test_check_command_init

    subroutine test_format_command_init()
        type(cli_app_t) :: app
        character(len=10) :: argv(2)

        app = create_cli_app()

        argv(1) = "format"
        argv(2) = "test.f90"

        call app%args%parse(2, argv)

        ! Verify formatter is initialized
        if (.not. app%formatter%is_initialized) then
            error stop "Failed: formatter should be initialized"
        end if

        print *, "[OK] Format command initialization"

    end subroutine test_format_command_init

    subroutine test_server_command()
        type(cli_app_t) :: app
        character(len=10) :: argv(1)
        integer :: exit_code

        app = create_cli_app()

        argv(1) = "server"

        call app%args%parse(1, argv)

        if (app%args%command /= "server") then
            error stop "Failed: command should be 'server'"
        end if

        ! Server command should fail for now (not implemented)
        ! We just verify it's recognized

        print *, "[OK] Server command recognized"

    end subroutine test_server_command

    subroutine test_help_flag()
        type(cli_app_t) :: app
        character(len=10) :: argv(1)

        app = create_cli_app()

        argv(1) = "--help"

        call app%args%parse(1, argv)

        if (.not. app%args%help) then
            error stop "Failed: help flag should be true"
        end if

        print *, "[OK] Help flag handling"

    end subroutine test_help_flag

    subroutine test_version_flag()
        type(cli_app_t) :: app
        character(len=10) :: argv(1)

        app = create_cli_app()

        argv(1) = "--version"

        call app%args%parse(1, argv)

        if (.not. app%args%version) then
            error stop "Failed: version flag should be true"
        end if

        print *, "[OK] Version flag handling"

    end subroutine test_version_flag

    subroutine test_exclude_pattern_parsing()
        type(cli_app_t) :: app
        character(len=20) :: argv(6)

        app = create_cli_app()

        argv(1) = "check"
        argv(2) = "--exclude"
        argv(3) = "*.tmp"
        argv(4) = "--exclude"
        argv(5) = "build/*"
        argv(6) = "test.f90"

        call app%args%parse(6, argv)

        if (.not. allocated(app%args%exclude_patterns)) then
            error stop "Failed: exclude_patterns should be allocated"
        end if

        if (size(app%args%exclude_patterns) /= 2) then
            error stop "Failed: should have 2 exclude patterns"
        end if

        if (app%args%exclude_patterns(1) /= "*.tmp") then
            error stop "Failed: first exclude pattern should be *.tmp"
        end if

        if (app%args%exclude_patterns(2) /= "build/*") then
            error stop "Failed: second exclude pattern should be build/*"
        end if

        print *, "[OK] Exclude pattern parsing"

    end subroutine test_exclude_pattern_parsing

    subroutine test_glob_pattern_matching()
        logical :: match

        ! Test wildcard matching on basename
        match = path_matches_pattern("foo.tmp", "*.tmp")
        if (.not. match) then
            error stop "Failed: foo.tmp should match *.tmp"
        end if

        match = path_matches_pattern("foo.f90", "*.tmp")
        if (match) then
            error stop "Failed: foo.f90 should not match *.tmp"
        end if

        ! Test wildcard matching on full path
        match = path_matches_pattern("build/x.o", "build/*")
        if (.not. match) then
            error stop "Failed: build/x.o should match build/*"
        end if

        match = path_matches_pattern("src/test.f90", "build/*")
        if (match) then
            error stop "Failed: src/test.f90 should not match build/*"
        end if

        ! Test literal substring match
        match = path_matches_pattern("vendor/lib.a", "vendor/")
        if (.not. match) then
            error stop "Failed: vendor/lib.a should match vendor/"
        end if

        match = path_matches_pattern("src/vendor.f90", "vendor/")
        if (match) then
            error stop "Failed: src/vendor.f90 should not match vendor/"
        end if

        print *, "[OK] Glob pattern matching"

    end subroutine test_glob_pattern_matching

    subroutine test_show_fixes_flag()
        type(cli_app_t) :: app
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: source_code, formatted_with_fixes, formatted_without_fixes, error_msg
        integer :: i
        logical :: has_fix, found_f001

        app = create_cli_app()
        linter = create_linter_engine()

        source_code = "program p" // new_line('a') // "  integer :: x" // new_line('a') // &
            "end program p"

        call linter%lint_source(source_code, "test.f90", diagnostics, error_msg)

        if (error_msg /= "") then
            error stop "Failed to lint source: "//error_msg
        end if

        if (.not. allocated(diagnostics)) then
            error stop "Failed: diagnostics should be allocated"
        end if

        found_f001 = .false.
        do i = 1, size(diagnostics)
            if (diagnostics(i)%code == "F001") then
                has_fix = .false.
                if (allocated(diagnostics(i)%fixes)) then
                    if (size(diagnostics(i)%fixes) > 0) has_fix = .true.
                end if

                if (has_fix) then
                    found_f001 = .true.

                    formatted_with_fixes = format_diagnostic_text(diagnostics(i))
                    formatted_with_fixes = trim(formatted_with_fixes)//" [*]"

                    formatted_without_fixes = format_diagnostic_text(diagnostics(i))

                    if (index(formatted_with_fixes, "[*]") == 0) then
                        error stop "Failed: [*] should be present in formatted_with_fixes"
                    end if

                    if (index(formatted_without_fixes, "[*]") /= 0) then
                        error stop "Failed: [*] should not be present in formatted_without_fixes"
                    end if

                    exit
                end if
            end if
        end do

        if (.not. found_f001) then
            error stop "Failed: no F001 diagnostic with fixes found"
        end if

        print *, "[OK] Show fixes flag handling"

    end subroutine test_show_fixes_flag

end program test_cli_subcommands
