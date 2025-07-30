program test_module_compilation
    ! Test that all fluff modules compile and link correctly
    use fluff_core
    use fluff_ast
    use fluff_linter
    use fluff_formatter
    use fluff_cli
    use fluff_config
    use fluff_rules
    use fluff_diagnostics
    implicit none
    
    ! Test that basic types are accessible
    type(fluff_version_t) :: version
    type(fluff_context_t) :: context
    
    print *, "Testing module compilation..."
    
    ! Test version access
    version = get_fluff_version()
    if (version%major /= 0 .or. version%minor /= 1 .or. version%patch /= 0) then
        error stop "Version mismatch"
    end if
    
    ! Test context creation
    context = create_fluff_context()
    if (.not. allocated(context%name)) then
        error stop "Context creation failed"
    end if
    
    print *, "All modules compiled successfully!"
    
end program test_module_compilation