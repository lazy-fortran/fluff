program test_module_dependencies
    ! Test that module dependencies are correctly resolved
    use fluff_linter
    use fluff_formatter
    implicit none
    
    ! Linter should be able to use AST module internally
    type(linter_engine_t) :: linter
    
    ! Formatter should be able to use AST module internally  
    type(formatter_engine_t) :: formatter
    
    print *, "Testing module dependency resolution..."
    
    ! Create linter (which internally uses fluff_ast)
    linter = create_linter_engine()
    if (.not. linter%is_initialized) then
        error stop "Linter initialization failed"
    end if
    
    ! Create formatter (which internally uses fluff_ast)
    formatter = create_formatter_engine()
    if (.not. formatter%is_initialized) then
        error stop "Formatter initialization failed"
    end if
    
    print *, "Module dependencies resolved correctly!"
    
end program test_module_dependencies