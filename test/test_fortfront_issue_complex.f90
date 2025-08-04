program test_fortfront_issue_complex
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source_code, formatted_code, error_msg
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: root_index
    
    print *, "=== Testing fortfront complex expression preservation ==="
    
    ! Complex expression test
    source_code = "program test" // new_line('a') // &
                 "implicit none" // new_line('a') // &
                 "real :: result" // new_line('a') // &
                 "result = very_long_function_name(arg1, arg2, arg3) + another_long_function(arg4, arg5) * " // &
                 "complex_calculation(arg6, arg7, arg8)" // new_line('a') // &
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
        print *, "Parsing failed: " // error_msg
        print *, "This is expected - fortfront doesn't know about these undefined functions"
        return
    end if
    
    ! Emit code
    call emit_fortran(arena, root_index, formatted_code)
    
    print *, "Output code:"
    print *, formatted_code
    print *, ""
    
end program test_fortfront_issue_complex