module fluff_ast
    ! AST manipulation and traversal (fortfront wrapper)
    use fluff_core, only: source_range_t
    use fortfront, only: &
        ast_arena_t, semantic_context_t, &
        tooling_load_ast_from_string, tooling_parse_options_t, &
        create_semantic_context, analyze_program, &
        get_node_type_id_from_arena, get_node_location, get_children, &
        set_source_text, has_source_text, get_source_text, get_source_line, &
        get_source_range, &
        get_trivia_for_ast_node, trivia_t, &
        symbol_info_t, get_symbols_in_scope, get_all_symbols, is_symbol_defined, &
        lookup_symbol, &
        variable_usage_info_t, create_variable_usage_info, &
        get_variables_in_expression, &
        get_identifiers_in_subtree, &
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
        NODE_INCLUDE_STATEMENT, NODE_CONTAINS, NODE_FORMAT_DESCRIPTOR, NODE_COMMENT, &
        NODE_IMPLICIT_STATEMENT, NODE_UNKNOWN
    implicit none
    private

    ! AST context wrapper for fluff
    type, public :: fluff_trivia_t
        integer :: kind = 0
        character(len=:), allocatable :: text
        integer :: start_pos = 0
        integer :: end_pos = 0
    end type fluff_trivia_t

    type, public :: fluff_ast_context_t
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: semantic_ctx
        integer :: root_index = 0
        logical :: is_initialized = .false.
    contains
        procedure :: from_source => ast_from_source
        procedure :: get_node_type => ast_get_node_type
        procedure :: get_children => ast_get_children
        procedure :: get_node_location => ast_get_node_location
        procedure :: has_source => ast_has_source
        procedure :: get_source_text => ast_get_source_text
        procedure :: get_source_line => ast_get_source_line
        procedure :: get_source_range => ast_get_source_range
        procedure :: get_trivia_for_node => ast_get_trivia_for_node
        procedure :: get_symbols_in_scope => ast_get_symbols_in_scope
        procedure :: get_all_symbols => ast_get_all_symbols
        procedure :: is_symbol_defined => ast_is_symbol_defined
        procedure :: lookup_symbol => ast_lookup_symbol
        procedure :: get_variables_in_expression => ast_get_variables_in_expression
        procedure :: get_identifiers_in_subtree => ast_get_identifiers_in_subtree
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
    public :: NODE_WHERE, NODE_INTERFACE_BLOCK, NODE_DERIVED_TYPE, &
              NODE_POINTER_ASSIGNMENT
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
        class(fluff_ast_context_t), intent(inout) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: error_msg
        type(ast_arena_t) :: arena
        integer :: root_index

        ! Initialize
        error_msg = ""
        this%is_initialized = .false.

        call tooling_load_ast_from_string(source_code, arena, root_index, error_msg, &
                                          tooling_parse_options_t())
        if (len(error_msg) > 0) return

        call set_source_text(arena, source_code)

        this%arena = arena
        this%root_index = root_index

        ! Semantic analysis
        call create_semantic_context(this%semantic_ctx)
        call analyze_program(this%semantic_ctx, this%arena, this%root_index)

        ! Mark as initialized
        this%is_initialized = .true.

    end subroutine ast_from_source

    ! Get node type
    function ast_get_node_type(this, node_index) result(node_type)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer :: node_type

        ! Default to unknown
        node_type = NODE_UNKNOWN

        if (.not. this%is_initialized) return
        if (node_index <= 0) then
            return
        end if

        node_type = get_node_type_id_from_arena(this%arena, node_index)
        if (node_type <= 0) node_type = NODE_UNKNOWN

    end function ast_get_node_type

    ! Get children of a node
    function ast_get_children(this, node_index) result(children)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer, allocatable :: children(:)
        integer :: i

        allocate (children(0))

        if (.not. this%is_initialized) return
        if (node_index <= 0) return

        children = get_children(this%arena, node_index)
        do i = 1, size(children)
            if (children(i) < 0) children(i) = 0
        end do

    end function ast_get_children

    ! Get node location
    function ast_get_node_location(this, node_index) result(location)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: node_index
        type(source_range_t) :: location
        integer :: line, column

        ! Initialize with invalid location
        location%start%line = 0
        location%start%column = 0
        location%end%line = 0
        location%end%column = 0

        ! Check if initialized
        if (.not. this%is_initialized) return
        if (node_index <= 0) return

        ! Use fortfront API to get actual location
        call get_node_location(this%arena, node_index, line, column)
        location%start%line = line
        location%start%column = column
        location%end%line = line  ! For now, end = start (single point)
        location%end%column = column

    end function ast_get_node_location

    function ast_has_source(this) result(found)
        class(fluff_ast_context_t), intent(in) :: this
        logical :: found

        found = .false.
        if (.not. this%is_initialized) return
        found = has_source_text(this%arena)
    end function ast_has_source

    subroutine ast_get_source_text(this, text, found)
        class(fluff_ast_context_t), intent(in) :: this
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found

        text = ""
        found = .false.
        if (.not. this%is_initialized) return

        call get_source_text(this%arena, text, found)
    end subroutine ast_get_source_text

    subroutine ast_get_source_line(this, line_number, line_text, found)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: line_number
        character(len=:), allocatable, intent(out) :: line_text
        logical, intent(out) :: found

        line_text = ""
        found = .false.
        if (.not. this%is_initialized) return

        call get_source_line(this%arena, line_number, line_text, found)
    end subroutine ast_get_source_line

    subroutine ast_get_source_range(this, start_line, start_col, end_line, end_col, &
                                    text, found)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: start_line
        integer, intent(in) :: start_col
        integer, intent(in) :: end_line
        integer, intent(in) :: end_col
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found

        text = ""
        found = .false.
        if (.not. this%is_initialized) return

        call get_source_range(this%arena, start_line, start_col, end_line, end_col, &
                              text, found)
    end subroutine ast_get_source_range

    subroutine ast_get_trivia_for_node(this, node_index, leading, trailing, found)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: node_index
        type(fluff_trivia_t), allocatable, intent(out) :: leading(:)
        type(fluff_trivia_t), allocatable, intent(out) :: trailing(:)
        logical, intent(out) :: found

        character(len=:), allocatable :: source
        type(trivia_t), allocatable :: leading_ff(:)
        type(trivia_t), allocatable :: trailing_ff(:)

        allocate (leading(0))
        allocate (trailing(0))
        found = .false.

        if (.not. this%is_initialized) return
        if (node_index <= 0) return

        call get_source_text(this%arena, source, found)
        if (.not. found) return

        call get_trivia_for_ast_node(source, this%arena, node_index, leading_ff, &
                                     trailing_ff, found)
        if (.not. found) return

        call convert_trivia_array(leading_ff, leading)
        call convert_trivia_array(trailing_ff, trailing)
    end subroutine ast_get_trivia_for_node

    function ast_get_symbols_in_scope(this, scope_level) result(symbols)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in), optional :: scope_level
        type(symbol_info_t), allocatable :: symbols(:)

        if (.not. this%is_initialized) then
            allocate (symbols(0))
            return
        end if

        if (present(scope_level)) then
            symbols = get_symbols_in_scope(this%semantic_ctx%scopes, scope_level)
        else
            symbols = get_symbols_in_scope(this%semantic_ctx%scopes)
        end if
    end function ast_get_symbols_in_scope

    function ast_get_all_symbols(this) result(symbols)
        class(fluff_ast_context_t), intent(in) :: this
        type(symbol_info_t), allocatable :: symbols(:)

        if (.not. this%is_initialized) then
            allocate (symbols(0))
            return
        end if

        symbols = get_all_symbols(this%semantic_ctx%scopes)
    end function ast_get_all_symbols

    function ast_is_symbol_defined(this, name) result(defined)
        class(fluff_ast_context_t), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: defined

        defined = .false.
        if (.not. this%is_initialized) return

        defined = is_symbol_defined(this%semantic_ctx%scopes, name)
    end function ast_is_symbol_defined

    function ast_lookup_symbol(this, name) result(info)
        class(fluff_ast_context_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(symbol_info_t) :: info

        info = symbol_info_t()
        info%name = name
        if (.not. this%is_initialized) return

        info = lookup_symbol(this%semantic_ctx%scopes, name)
    end function ast_lookup_symbol

    function ast_get_variables_in_expression(this, expr_index) result(info)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: expr_index
        type(variable_usage_info_t) :: info

        if (.not. this%is_initialized) then
            info = create_variable_usage_info()
            return
        end if

        info = get_variables_in_expression(this%arena, expr_index)
    end function ast_get_variables_in_expression

    function ast_get_identifiers_in_subtree(this, root_index) result(identifiers)
        class(fluff_ast_context_t), intent(in) :: this
        integer, intent(in) :: root_index
        character(len=:), allocatable :: identifiers(:)

        if (.not. this%is_initialized) then
            allocate (character(len=0) :: identifiers(0))
            return
        end if

        identifiers = get_identifiers_in_subtree(this%arena, root_index)
    end function ast_get_identifiers_in_subtree

    subroutine convert_trivia_array(input, output)
        type(trivia_t), allocatable, intent(in) :: input(:)
        type(fluff_trivia_t), allocatable, intent(out) :: output(:)
        integer :: i

        if (.not. allocated(input)) then
            allocate (output(0))
            return
        end if

        allocate (output(size(input)))
        do i = 1, size(input)
            output(i)%kind = input(i)%kind
            output(i)%start_pos = input(i)%start_pos
            output(i)%end_pos = input(i)%end_pos
            if (allocated(input(i)%text)) then
                output(i)%text = input(i)%text
            else
                output(i)%text = ""
            end if
        end do
    end subroutine convert_trivia_array

end module fluff_ast
