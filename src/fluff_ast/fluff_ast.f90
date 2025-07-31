module fluff_ast
    ! AST manipulation and traversal (fortfront wrapper)
    use fluff_core
    use fortfront, only: ast_arena_t, semantic_context_t, token_t
    implicit none
    private
    
    ! AST context wrapper for fluff
    type, public :: fluff_ast_context_t
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: semantic_ctx
        integer :: root_index = 0
        logical :: is_initialized = .false.
    contains
        procedure :: from_source => ast_from_source
        procedure :: traverse => ast_traverse
        procedure :: get_node_type => ast_get_node_type
        procedure :: get_children => ast_get_children
        procedure :: get_node_location => ast_get_node_location
    end type fluff_ast_context_t
    
    ! Node type enumeration
    enum, bind(c)
        enumerator :: NODE_UNKNOWN = 0
        enumerator :: NODE_PROGRAM = 1
        enumerator :: NODE_ASSIGNMENT = 2
        enumerator :: NODE_BINARY_OP = 3
        enumerator :: NODE_FUNCTION_DEF = 4
        enumerator :: NODE_IDENTIFIER = 5
        enumerator :: NODE_LITERAL = 6
        enumerator :: NODE_ARRAY_LITERAL = 7
        enumerator :: NODE_IF_STATEMENT = 8
        enumerator :: NODE_DO_LOOP = 9
        enumerator :: NODE_VARIABLE_DECL = 10
    end enum
    
    ! Public procedures
    public :: create_ast_context
    
contains
    
    ! Create a new AST context
    function create_ast_context() result(ctx)
        type(fluff_ast_context_t) :: ctx
        ctx%is_initialized = .false.
    end function create_ast_context
    
    ! Parse source code into AST
    subroutine ast_from_source(this, source_code, error_msg)
        use fortfront, only: lex_source, parse_tokens, analyze_semantics, &
                            token_t, ast_arena_t, semantic_context_t, &
                            create_ast_arena, create_semantic_context
        class(fluff_ast_context_t), intent(inout) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(token_t), allocatable :: tokens(:)
        
        ! Initialize
        error_msg = ""
        this%is_initialized = .false.
        
        ! Lexical analysis
        call lex_source(source_code, tokens, error_msg)
        if (error_msg /= "") return
        
        ! Create AST arena
        this%arena = create_ast_arena()
        
        ! Parsing
        call parse_tokens(tokens, this%arena, this%root_index, error_msg)
        if (error_msg /= "") return
        
        ! Semantic analysis
        this%semantic_ctx = create_semantic_context()
        call analyze_semantics(this%arena, this%root_index)
        
        ! Mark as initialized
        this%is_initialized = .true.
        
    end subroutine ast_from_source
    
    ! Traverse AST with visitor pattern
    subroutine ast_traverse(this, visitor, pre_order)
        class(fluff_ast_context_t), intent(in) :: this
        class(*), intent(inout) :: visitor
        logical, intent(in), optional :: pre_order
        
        ! TODO: Implement traversal using fortfront's capabilities
        
    end subroutine ast_traverse
    
    ! Get node type
    function ast_get_node_type(this, node_index) result(node_type)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer :: node_type
        
        node_type = NODE_UNKNOWN
        ! TODO: Implement node type detection
        
    end function ast_get_node_type
    
    ! Get children of a node
    function ast_get_children(this, node_index) result(children)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer, allocatable :: children(:)
        
        allocate(children(0))
        ! TODO: Implement child retrieval
        
    end function ast_get_children
    
    ! Get node location
    function ast_get_node_location(this, node_index) result(location)
        use fluff_core, only: source_range_t, source_location_t
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: node_index
        type(source_range_t) :: location
        
        ! Initialize with invalid location
        location%start%line = 0
        location%start%column = 0
        location%end%line = 0
        location%end%column = 0
        ! TODO: Implement location retrieval from AST
        
    end function ast_get_node_location
    
end module fluff_ast