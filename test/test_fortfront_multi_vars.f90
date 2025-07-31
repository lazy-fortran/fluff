program test_fortfront_multi_vars
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source_code, formatted_code, error_msg
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: root_index
    
    print *, "=== Testing fortfront multi-variable declaration ==="
    
    ! Multi-variable declaration test
    source_code = "program test" // new_line('a') // &
                 "implicit none" // new_line('a') // &
                 "real :: result, a, b, c, d, e, f" // new_line('a') // &
                 "result = a * b + c * d" // new_line('a') // &
                 "end program test"
    
    print *, "Input code:"
    print *, source_code
    print *, ""
    
    ! Lex and parse
    call lex_source(source_code, tokens, error_msg)
    if (error_msg /= "") then
        error stop "Lexing failed: " // error_msg
    end if
    
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, root_index, error_msg)
    if (error_msg /= "") then
        error stop "Parsing failed: " // error_msg
    end if
    
    ! Emit code
    call emit_fortran(arena, root_index, formatted_code)
    
    print *, "Output code:"
    print *, formatted_code
    print *, ""
    
    ! Check if all variables are preserved
    if (index(formatted_code, "a") == 0 .or. index(formatted_code, "b") == 0) then
        print *, "ISSUE: Variables not preserved in declaration"
    end if
    
    if (index(formatted_code, "result = a") == 0) then
        print *, "ISSUE: Assignment not preserved"
    end if
    
end program test_fortfront_multi_vars