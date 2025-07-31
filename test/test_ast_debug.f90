program test_ast_debug
    use fortfront
    implicit none
    
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: source_code, error_msg
    integer :: root_index, i
    integer, allocatable :: children(:)
    
    ! Test code
    source_code = "program test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    integer :: i,j,k" // new_line('a') // &
                 "    i=1" // new_line('a') // &
                 "    j=i+2" // new_line('a') // &
                 "end program test"
    
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
    
    ! Debug AST structure
    print *, "Root index:", root_index
    print *, "Node type at root:", get_node_type(arena, root_index)
    print *, "NODE_PROGRAM constant:", NODE_PROGRAM
    
    ! Check program node details
    block
        type(program_node), pointer :: prog_node
        prog_node => get_node_as_program(arena, root_index)
        if (associated(prog_node)) then
            print *, "Program name:", prog_node%name
            if (allocated(prog_node%body_indices)) then
                print *, "Body indices:", prog_node%body_indices
            else
                print *, "No body indices allocated"
            end if
        end if
        
        ! Check inner program (index 1)
        print *, ""
        print *, "Inner program (index 1):"
        prog_node => get_node_as_program(arena, 1)
        if (associated(prog_node)) then
            print *, "  Name:", prog_node%name
            if (allocated(prog_node%body_indices)) then
                print *, "  Body indices:", prog_node%body_indices
            else
                print *, "  No body indices"
            end if
        end if
    end block
    
    ! Map node types
    print *, ""
    print *, "Node type constants:"
    print *, "  NODE_PROGRAM:", NODE_PROGRAM
    print *, "  NODE_ASSIGNMENT:", NODE_ASSIGNMENT
    print *, "  NODE_BINARY_OP:", NODE_BINARY_OP
    print *, "  NODE_FUNCTION_DEF:", NODE_FUNCTION_DEF
    print *, "  NODE_IDENTIFIER:", NODE_IDENTIFIER
    print *, "  NODE_LITERAL:", NODE_LITERAL
    print *, "  NODE_DECLARATION:", NODE_DECLARATION
    
    ! Explore all nodes in arena
    print *, ""
    print *, "All nodes in arena:"
    do i = 1, arena%size
        print *, "  Index", i, "type:", get_node_type(arena, i), "parent:", get_parent(arena, i)
        
        ! Show declaration details
        if (get_node_type(arena, i) == NODE_DECLARATION) then
            print *, "    Declaration node found"
        end if
    end do
    
end program test_ast_debug