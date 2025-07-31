module fluff_formatter_visitor
    ! AST visitor for custom formatting
    use fluff_core
    use fortfront
    implicit none
    private
    
    ! Formatter visitor type
    type, public :: formatter_visitor_t
        type(format_options_t) :: options
        character(len=:), allocatable :: output
        integer :: indent_level = 0
        logical :: need_newline = .false.
        integer :: column = 0
    contains
        procedure :: visit_node => visitor_visit_node
        procedure :: append => visitor_append
        procedure :: append_line => visitor_append_line
        procedure :: indent => visitor_indent
        procedure :: dedent => visitor_dedent
        procedure :: newline => visitor_newline
        procedure :: space => visitor_space
        procedure :: get_indent_string => visitor_get_indent_string
        procedure :: format_declaration_group => visitor_format_declaration_group
    end type formatter_visitor_t
    
    ! Public procedures
    public :: create_formatter_visitor, format_with_visitor
    public :: get_node_as_declaration  ! For debugging
    
contains
    
    ! Create a new formatter visitor
    function create_formatter_visitor(options) result(visitor)
        type(format_options_t), intent(in) :: options
        type(formatter_visitor_t) :: visitor
        
        visitor%options = options
        visitor%output = ""
        visitor%indent_level = 0
        visitor%need_newline = .false.
        visitor%column = 0
        
    end function create_formatter_visitor
    
    ! Format AST using visitor pattern
    function format_with_visitor(arena, root_index, options) result(formatted_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(format_options_t), intent(in) :: options
        character(len=:), allocatable :: formatted_code
        
        type(formatter_visitor_t) :: visitor
        
        visitor = create_formatter_visitor(options)
        call visitor%visit_node(arena, root_index)
        formatted_code = visitor%output
        
    end function format_with_visitor
    
    ! Visit a node and format it
    recursive subroutine visitor_visit_node(this, arena, node_index)
        class(formatter_visitor_t), intent(inout) :: this
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        integer :: node_type
        integer, allocatable :: children(:)
        class(ast_node), pointer :: node_ptr
        type(program_node), pointer :: prog_node
        type(assignment_node), pointer :: assign_node
        type(binary_op_node), pointer :: binop_node
        type(identifier_node), pointer :: id_node
        type(literal_node), pointer :: lit_node
        type(declaration_node), pointer :: decl_node
        character(len=20) :: temp_str
        integer :: i, j
        
        if (node_index <= 0 .or. node_index > arena%size) return
        
        node_type = get_node_type(arena, node_index)
        
        select case (node_type)
        case (NODE_PROGRAM)
            prog_node => get_node_as_program(arena, node_index)
            if (associated(prog_node)) then
                ! Special handling for fortfront's wrapper "main" program
                if (prog_node%name == "main" .and. allocated(prog_node%body_indices)) then
                    ! Find the actual program node and process remaining statements
                    do i = 1, size(prog_node%body_indices)
                        if (get_node_type(arena, prog_node%body_indices(i)) == NODE_PROGRAM) then
                            ! Found nested program - output its header
                            block
                                type(program_node), pointer :: inner_prog
                                inner_prog => get_node_as_program(arena, prog_node%body_indices(i))
                                if (associated(inner_prog)) then
                                    call this%append("program ")
                                    call this%append(inner_prog%name)
                                    call this%newline()
                                    call this%indent()
                                    
                                    ! Process remaining statements after the program node
                                    do j = i + 1, size(prog_node%body_indices)
                                        ! Skip error literal nodes
                                        if (get_node_type(arena, prog_node%body_indices(j)) == NODE_LITERAL) then
                                            lit_node => get_node_as_literal(arena, prog_node%body_indices(j))
                                            if (associated(lit_node)) then
                                                if (index(lit_node%value, "ERROR") == 0) then
                                                    call this%visit_node(arena, prog_node%body_indices(j))
                                                end if
                                            end if
                                        else if (get_node_type(arena, prog_node%body_indices(j)) == NODE_DECLARATION) then
                                            ! Handle declaration - check if next nodes are also declarations
                                            call this%format_declaration_group(arena, prog_node%body_indices(j))
                                        else
                                            call this%visit_node(arena, prog_node%body_indices(j))
                                        end if
                                    end do
                                    
                                    call this%dedent()
                                    call this%append_line("end program " // inner_prog%name)
                                end if
                            end block
                            return
                        end if
                    end do
                end if
                
                ! Normal program formatting
                call this%append("program ")
                call this%append(prog_node%name)
                call this%newline()
                call this%indent()
                
                ! Visit body statements
                if (allocated(prog_node%body_indices)) then
                    do i = 1, size(prog_node%body_indices)
                        call this%visit_node(arena, prog_node%body_indices(i))
                    end do
                end if
                
                call this%dedent()
                call this%append_line("end program " // prog_node%name)
            end if
            
        case (NODE_ASSIGNMENT)
            assign_node => get_node_as_assignment(arena, node_index)
            if (associated(assign_node)) then
                call this%append(this%get_indent_string())
                
                ! Visit target
                if (assign_node%target_index > 0) then
                    call this%visit_node(arena, assign_node%target_index)
                end if
                
                call this%append(" = ")
                
                ! Visit value
                if (assign_node%value_index > 0) then
                    call this%visit_node(arena, assign_node%value_index)
                end if
                
                call this%newline()
            end if
            
        case (NODE_BINARY_OP)
            binop_node => get_node_as_binary_op(arena, node_index)
            if (associated(binop_node)) then
                ! Visit left operand
                if (binop_node%left_index > 0) then
                    call this%visit_node(arena, binop_node%left_index)
                end if
                
                ! Add operator with spaces
                call this%space()
                call this%append(binop_node%operator)
                call this%space()
                
                ! Visit right operand
                if (binop_node%right_index > 0) then
                    call this%visit_node(arena, binop_node%right_index)
                end if
            end if
            
        case (NODE_IDENTIFIER)
            id_node => get_node_as_identifier(arena, node_index)
            if (associated(id_node)) then
                call this%append(id_node%name)
            end if
            
        case (NODE_LITERAL)
            lit_node => get_node_as_literal(arena, node_index)
            if (associated(lit_node)) then
                ! Check if this is an "implicit none" statement
                if (index(lit_node%value, "implicit none") > 0) then
                    call this%append(this%get_indent_string())
                    call this%append("implicit none")
                    call this%newline()
                else
                    call this%append(lit_node%value)
                end if
            end if
            
        case (NODE_DECLARATION)
            decl_node => get_node_as_declaration(arena, node_index)
            if (associated(decl_node)) then
                call this%append(this%get_indent_string())
                call this%append(decl_node%type_name)
                if (decl_node%has_kind) then
                    write(temp_str, '("(",I0,")")') decl_node%kind_value
                    call this%append(temp_str)
                end if
                call this%append(" :: ")
                call this%append(decl_node%var_name)
                call this%newline()
            end if
            
        case default
            ! Handle other node types generically
            children = get_children(arena, node_index)
            do i = 1, size(children)
                call this%visit_node(arena, children(i))
            end do
        end select
        
    end subroutine visitor_visit_node
    
    ! Append text to output
    subroutine visitor_append(this, text)
        class(formatter_visitor_t), intent(inout) :: this
        character(len=*), intent(in) :: text
        
        this%output = this%output // text
        this%column = this%column + len(text)
        this%need_newline = .false.
        
    end subroutine visitor_append
    
    ! Append a line with proper indentation
    subroutine visitor_append_line(this, text)
        class(formatter_visitor_t), intent(inout) :: this
        character(len=*), intent(in) :: text
        
        if (this%column > 0) call this%newline()
        call this%append(this%get_indent_string())
        call this%append(text)
        call this%newline()
        
    end subroutine visitor_append_line
    
    ! Increase indent level
    subroutine visitor_indent(this)
        class(formatter_visitor_t), intent(inout) :: this
        this%indent_level = this%indent_level + 1
    end subroutine visitor_indent
    
    ! Decrease indent level
    subroutine visitor_dedent(this)
        class(formatter_visitor_t), intent(inout) :: this
        if (this%indent_level > 0) then
            this%indent_level = this%indent_level - 1
        end if
    end subroutine visitor_dedent
    
    ! Add newline
    subroutine visitor_newline(this)
        class(formatter_visitor_t), intent(inout) :: this
        this%output = this%output // new_line('a')
        this%column = 0
        this%need_newline = .false.
    end subroutine visitor_newline
    
    ! Add space
    subroutine visitor_space(this)
        class(formatter_visitor_t), intent(inout) :: this
        call this%append(" ")
    end subroutine visitor_space
    
    ! Get current indentation string
    function visitor_get_indent_string(this) result(indent)
        class(formatter_visitor_t), intent(in) :: this
        character(len=:), allocatable :: indent
        integer :: total_spaces
        
        total_spaces = this%indent_level * this%options%indent_size
        if (this%options%use_spaces) then
            allocate(character(len=total_spaces) :: indent)
            indent = repeat(' ', total_spaces)
        else
            ! Use tabs
            allocate(character(len=this%indent_level) :: indent)
            indent = repeat(char(9), this%indent_level)
        end if
        
    end function visitor_get_indent_string
    
    ! Format a group of declarations with the same type
    subroutine visitor_format_declaration_group(this, arena, first_decl_index)
        class(formatter_visitor_t), intent(inout) :: this
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: first_decl_index
        
        type(declaration_node), pointer :: decl
        character(len=:), allocatable :: type_name
        character(len=20) :: temp_str
        integer :: i, next_index
        logical :: first
        
        ! Get first declaration
        decl => get_node_as_declaration(arena, first_decl_index)
        if (.not. associated(decl)) return
        
        ! Start declaration line
        call this%append(this%get_indent_string())
        call this%append(decl%type_name)
        if (decl%has_kind) then
            write(temp_str, '("(",I0,")")') decl%kind_value
            call this%append(temp_str)
        end if
        call this%append(" :: ")
        
        type_name = decl%type_name
        
        ! Output first variable
        call this%append(decl%var_name)
        first = .false.
        
        ! Look for consecutive declarations of the same type
        next_index = first_decl_index + 1
        do while (next_index <= arena%size)
            if (get_node_type(arena, next_index) == NODE_DECLARATION) then
                decl => get_node_as_declaration(arena, next_index)
                if (associated(decl)) then
                    if (decl%type_name == type_name) then
                        call this%append(", ")
                        call this%append(decl%var_name)
                        next_index = next_index + 1
                    else
                        exit
                    end if
                else
                    exit
                end if
            else
                exit
            end if
        end do
        
        call this%newline()
        
    end subroutine visitor_format_declaration_group
    
    ! Helper to get typed node pointers
    function get_node_as_binary_op(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(binary_op_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (binary_op_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_binary_op
    
    function get_node_as_identifier(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(identifier_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (identifier_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_identifier
    
    function get_node_as_literal(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(literal_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (literal_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_literal
    
    function get_node_as_declaration(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(declaration_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (declaration_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_declaration
    
end module fluff_formatter_visitor