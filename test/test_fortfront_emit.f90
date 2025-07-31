program test_fortfront_emit
    use fortfront, only: lex_source, parse_tokens, create_ast_arena, emit_fortran, &
                        token_t, ast_arena_t, analyze_semantics
    implicit none
    
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: source_code, formatted_code, error_msg
    integer :: root_index
    
    ! Test code with inconsistent spacing
    source_code = "program test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    integer :: i,j,k" // new_line('a') // &
                 "    i=1" // new_line('a') // &
                 "    j=i+2" // new_line('a') // &
                 "end program test"
    
    print *, "Original code:"
    print *, source_code
    print *, ""
    
    ! Lex
    call lex_source(source_code, tokens, error_msg)
    if (error_msg /= "") then
        print *, "Lex error:", error_msg
        stop 1
    end if
    
    ! Parse  
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, root_index, error_msg)
    if (error_msg /= "") then
        print *, "Parse error:", error_msg
        stop 1
    end if
    
    ! Analyze
    call analyze_semantics(arena, root_index)
    
    ! Debug: print AST info
    print *, "Root index:", root_index
    print *, "Arena size:", arena%size
    
    ! Emit
    call emit_fortran(arena, root_index, formatted_code)
    
    print *, "Fortfront output:"
    print *, formatted_code
    
end program test_fortfront_emit