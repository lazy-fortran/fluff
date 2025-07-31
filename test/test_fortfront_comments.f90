program test_fortfront_comments
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source_code, formatted_code, error_msg
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: root_index
    
    print *, "=== Testing fortfront comment preservation ==="
    
    ! Test with comments
    source_code = "program test" // new_line('a') // &
                 "! This is a comment" // new_line('a') // &
                 "implicit none" // new_line('a') // &
                 "integer :: i  ! Inline comment" // new_line('a') // &
                 "! Another comment" // new_line('a') // &
                 "i = 42" // new_line('a') // &
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
    
    ! Check if comments are preserved
    if (index(formatted_code, "This is a comment") == 0) then
        print *, "ISSUE: Comments are not preserved in emit_fortran output"
    else
        print *, "Comments are preserved"
    end if
    
    if (index(formatted_code, "Inline comment") == 0) then
        print *, "ISSUE: Inline comments are not preserved"
    end if
    
end program test_fortfront_comments