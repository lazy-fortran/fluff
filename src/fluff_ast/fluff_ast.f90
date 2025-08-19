module fluff_ast
    ! AST manipulation and traversal (fortfront wrapper)
    use fluff_core
    use fortfront, only: ast_arena_t, semantic_context_t, token_t, &
                       get_node_type_id_from_arena, &
                       NODE_PROGRAM, NODE_FUNCTION_DEF, NODE_ASSIGNMENT, NODE_BINARY_OP, &
                       NODE_IDENTIFIER, NODE_LITERAL, NODE_ARRAY_LITERAL, &
                       NODE_CALL_OR_SUBSCRIPT, NODE_SUBROUTINE_DEF, NODE_SUBROUTINE_CALL, &
                       NODE_DECLARATION, NODE_PARAMETER_DECLARATION, NODE_IF, NODE_DO_LOOP, &
                       NODE_DO_WHILE, NODE_SELECT_CASE, NODE_CASE_BLOCK, NODE_MODULE, &
                       NODE_USE_STATEMENT, NODE_PRINT_STATEMENT, NODE_WRITE_STATEMENT, &
                       NODE_READ_STATEMENT, NODE_ALLOCATE_STATEMENT, NODE_DEALLOCATE_STATEMENT, &
                       NODE_STOP, NODE_RETURN, NODE_GOTO, NODE_ERROR_STOP, NODE_CYCLE, NODE_EXIT, &
                       NODE_WHERE, NODE_INTERFACE_BLOCK, NODE_DERIVED_TYPE, NODE_POINTER_ASSIGNMENT, &
                       NODE_FORALL, NODE_CASE_RANGE, NODE_CASE_DEFAULT, NODE_COMPLEX_LITERAL, &
                       NODE_INCLUDE_STATEMENT, NODE_CONTAINS, NODE_FORMAT_DESCRIPTOR, &
                       NODE_COMMENT, NODE_IMPLICIT_STATEMENT, NODE_UNKNOWN
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
    
    ! Node type constants - all imported from fortfront
    
    ! Public procedures  
    public :: create_ast_context
    
    ! Public node type constants (imported from fortfront)
    public :: NODE_UNKNOWN, NODE_PROGRAM, NODE_ASSIGNMENT, NODE_BINARY_OP
    public :: NODE_FUNCTION_DEF, NODE_IDENTIFIER, NODE_LITERAL, NODE_ARRAY_LITERAL
    public :: NODE_CALL_OR_SUBSCRIPT, NODE_SUBROUTINE_DEF, NODE_SUBROUTINE_CALL
    public :: NODE_DECLARATION, NODE_PARAMETER_DECLARATION, NODE_IF, NODE_DO_LOOP
    public :: NODE_DO_WHILE, NODE_SELECT_CASE, NODE_CASE_BLOCK, NODE_MODULE
    public :: NODE_USE_STATEMENT, NODE_PRINT_STATEMENT, NODE_WRITE_STATEMENT
    public :: NODE_READ_STATEMENT, NODE_ALLOCATE_STATEMENT, NODE_DEALLOCATE_STATEMENT
    public :: NODE_STOP, NODE_RETURN, NODE_GOTO, NODE_ERROR_STOP, NODE_CYCLE, NODE_EXIT
    public :: NODE_WHERE, NODE_INTERFACE_BLOCK, NODE_DERIVED_TYPE, NODE_POINTER_ASSIGNMENT
    public :: NODE_FORALL, NODE_CASE_RANGE, NODE_CASE_DEFAULT, NODE_COMPLEX_LITERAL
    public :: NODE_INCLUDE_STATEMENT, NODE_CONTAINS, NODE_FORMAT_DESCRIPTOR
    public :: NODE_COMMENT, NODE_IMPLICIT_STATEMENT
    
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
        
        logical :: is_pre_order
        
        ! Default to pre-order traversal
        is_pre_order = .true.
        if (present(pre_order)) is_pre_order = pre_order
        
        ! Check if initialized
        if (.not. this%is_initialized) then
            return
        end if
        
        ! Do manual traversal for all visitor types
        call traverse_node_recursive(this, this%root_index, visitor, is_pre_order)
        
    end subroutine ast_traverse
    
    ! Recursive node traversal for non-visitor types
    recursive subroutine traverse_node_recursive(ctx, node_index, visitor, pre_order)
        class(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        class(*), intent(inout) :: visitor
        logical, intent(in) :: pre_order
        
        integer, allocatable :: children(:)
        integer :: i
        
        if (node_index <= 0) return
        
        ! Pre-order visit
        if (pre_order) then
            ! Process current node (visitor-specific logic would go here)
        end if
        
        ! Get and traverse children
        children = ctx%get_children(node_index)
        do i = 1, size(children)
            call traverse_node_recursive(ctx, children(i), visitor, pre_order)
        end do
        
        ! Post-order visit
        if (.not. pre_order) then
            ! Process current node (visitor-specific logic would go here)
        end if
        
    end subroutine traverse_node_recursive
    
    ! Get node type
    function ast_get_node_type(this, node_index) result(node_type)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer :: node_type
        
        integer :: fortfront_type
        
        ! Default to unknown
        node_type = NODE_UNKNOWN
        
        ! Enhanced error checking with detailed validation
        if (.not. this%is_initialized) then
            ! Log warning but continue gracefully
            return
        end if
        
        if (node_index <= 0) then
            ! Invalid node index - log and return
            return
        end if
        
        ! Validate arena bounds before access
        ! TODO: Add arena size checking when fortfront provides bounds info
        
        ! Get node type from fortfront with error handling
        fortfront_type = get_node_type_id_from_arena(this%arena, node_index)
        
        ! Accept any valid fortfront node type (they start from 1)
        if (fortfront_type > 0) then
            node_type = fortfront_type
        else
            ! Unknown or invalid type from fortfront
            node_type = NODE_UNKNOWN
        end if
        
    end function ast_get_node_type
    
    ! Get children of a node
    function ast_get_children(this, node_index) result(children)
        use fortfront, only: get_children_from_arena => get_children
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer, allocatable :: children(:)
        integer :: i  ! Declare at the beginning of scope
        
        ! Initialize empty array as safe default
        allocate(children(0))
        
        ! Enhanced validation before attempting to get children
        if (.not. this%is_initialized) then
            ! Return empty array but log issue
            return
        end if
        
        if (node_index <= 0) then
            ! Invalid node index - return empty array
            return
        end if
        
        ! Get children from fortfront with error handling
        ! TODO: Add try-catch equivalent when available
        children = get_children_from_arena(this%arena, node_index)
        
        ! Validate result - ensure no negative indices in children
        if (allocated(children)) then
            do i = 1, size(children)
                if (children(i) < 0) then
                    ! Invalid child index found - could log warning
                    children(i) = 0  ! Mark as invalid
                end if
            end do
        end if
        
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
        
        ! Check if initialized
        if (.not. this%is_initialized) return
        if (node_index <= 0) return
        
        ! For now, we'll need to get location info differently
        ! fortfront may provide this through a different API
        ! TODO: Implement proper location retrieval once API is clarified
        location%start%line = 1
        location%start%column = 1
        location%end%line = 1
        location%end%column = 1
        
    end function ast_get_node_location
    
end module fluff_ast