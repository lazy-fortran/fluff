module fortfront_compat
    ! Compatibility layer for missing fortfront APIs
    ! These are stub implementations that allow fluff to build
    ! while proper fortfront integration is developed.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortfront, only: ast_arena_t, semantic_context_t, identifier_node
    implicit none
    private

    ! Symbol information type for compatibility
    type, public :: compat_symbol_info_t
        character(len=:), allocatable :: name
        integer :: scope_type = 0
        logical :: is_used = .false.
    end type compat_symbol_info_t

    public :: get_identifier_name
    public :: get_symbols_in_scope
    public :: is_identifier_defined_direct
    public :: get_unused_variables_direct
    public :: get_declaration_info

    ! Control flow graph types and functions
    type, public :: control_flow_graph_t
        integer :: node_count = 0
        integer, allocatable :: edges(:, :)
    end type control_flow_graph_t

    public :: build_control_flow_graph
    public :: find_unreachable_code

    ! AST node accessor functions
    public :: get_assignment_indices
    public :: get_binary_op_info
    public :: get_call_info
    public :: get_program_info
    public :: get_unused_procedures

contains

    ! Get the name of an identifier node
    function get_identifier_name(arena, node_index, name) result(success)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        logical :: success

        success = .false.
        name = ""

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            if (allocated(node%name)) then
                name = node%name
                success = .true.
            end if
        end select
    end function get_identifier_name

    ! Get symbols defined in a given scope
    subroutine get_symbols_in_scope(ctx, scope_type, symbols, count)
        type(semantic_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_type
        type(compat_symbol_info_t), allocatable, intent(out) :: symbols(:)
        integer, intent(out) :: count

        ! Stub implementation - returns empty list
        allocate(symbols(0))
        count = 0
    end subroutine get_symbols_in_scope

    ! Check if an identifier is defined in the current context
    function is_identifier_defined_direct(arena, ctx, name) result(is_defined)
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name
        logical :: is_defined

        ! Stub implementation - assume all identifiers are defined
        is_defined = .true.
    end function is_identifier_defined_direct

    ! Get unused variables in a given scope
    function get_unused_variables_direct(arena, ctx, scope_type, unused_vars) &
            result(success)
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_type
        character(len=:), allocatable, intent(out) :: unused_vars(:)
        logical :: success

        ! Stub implementation - returns empty list
        allocate(character(len=0) :: unused_vars(0))
        success = .true.
    end function get_unused_variables_direct

    ! Get declaration information for a node
    function get_declaration_info(arena, node_index, var_names, type_spec, &
                                 attributes) result(success)
        use fortfront, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: var_names(:)
        character(len=:), allocatable, intent(out) :: type_spec
        character(len=:), allocatable, intent(out) :: attributes(:)
        logical :: success
        integer :: attr_count

        success = .false.
        allocate(character(len=0) :: var_names(0))
        allocate(character(len=0) :: attributes(0))
        type_spec = ""

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (declaration_node)
            ! Extract variable names from declaration
            if (node%is_multi_declaration .and. allocated(node%var_names)) then
                deallocate(var_names)
                allocate(var_names(size(node%var_names)), source=node%var_names)
            else if (allocated(node%var_name)) then
                deallocate(var_names)
                allocate(character(len=len(node%var_name)) :: var_names(1))
                var_names(1) = node%var_name
            end if
            ! Extract type specifier
            if (allocated(node%type_name)) then
                type_spec = node%type_name
            end if
            ! Build attributes list from declaration flags
            attr_count = 0
            if (node%is_allocatable) attr_count = attr_count + 1
            if (node%is_pointer) attr_count = attr_count + 1
            if (node%is_target) attr_count = attr_count + 1
            if (node%is_optional) attr_count = attr_count + 1
            if (node%is_parameter) attr_count = attr_count + 1
            if (attr_count > 0) then
                deallocate(attributes)
                allocate(character(len=16) :: attributes(attr_count))
                attr_count = 0
                if (node%is_allocatable) then
                    attr_count = attr_count + 1
                    attributes(attr_count) = "allocatable"
                end if
                if (node%is_pointer) then
                    attr_count = attr_count + 1
                    attributes(attr_count) = "pointer"
                end if
                if (node%is_target) then
                    attr_count = attr_count + 1
                    attributes(attr_count) = "target"
                end if
                if (node%is_optional) then
                    attr_count = attr_count + 1
                    attributes(attr_count) = "optional"
                end if
                if (node%is_parameter) then
                    attr_count = attr_count + 1
                    attributes(attr_count) = "parameter"
                end if
            end if
            success = .true.
        end select
    end function get_declaration_info

    ! Build control flow graph from AST
    function build_control_flow_graph(arena, root_index) result(cfg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(control_flow_graph_t) :: cfg

        ! Stub implementation
        cfg%node_count = 0
        allocate(cfg%edges(0, 2))
    end function build_control_flow_graph

    ! Find unreachable code in control flow graph
    function find_unreachable_code(cfg) result(unreachable)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: unreachable(:)

        ! Stub implementation - returns empty array
        allocate(unreachable(0))
    end function find_unreachable_code

    ! Get assignment indices from an assignment node
    function get_assignment_indices(arena, node_index, target_index, value_index, &
                                   operator_str) result(success)
        use fortfront, only: assignment_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: target_index, value_index
        character(len=:), allocatable, intent(out) :: operator_str
        logical :: success

        success = .false.
        target_index = 0
        value_index = 0
        operator_str = "="

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            target_index = node%target_index
            value_index = node%value_index
            if (allocated(node%operator)) then
                operator_str = node%operator
            end if
            success = .true.
        end select
    end function get_assignment_indices

    ! Get binary operation info
    function get_binary_op_info(arena, node_index, left_index, right_index, &
                               operator_str) result(success)
        use fortfront, only: binary_op_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: left_index, right_index
        character(len=:), allocatable, intent(out) :: operator_str
        logical :: success

        success = .false.
        left_index = 0
        right_index = 0
        operator_str = ""

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            left_index = node%left_index
            right_index = node%right_index
            if (allocated(node%operator)) then
                operator_str = node%operator
            else
                operator_str = ""
            end if
            success = .true.
        end select
    end function get_binary_op_info

    ! Get call information
    function get_call_info(arena, node_index, name, arg_indices) result(success)
        use fortfront, only: call_or_subscript_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: arg_indices(:)
        logical :: success

        success = .false.
        name = ""
        allocate(arg_indices(0))

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (call_or_subscript_node)
            if (allocated(node%name)) then
                name = node%name
            end if
            if (allocated(node%arg_indices)) then
                deallocate(arg_indices)
                allocate(arg_indices(size(node%arg_indices)))
                arg_indices = node%arg_indices
            end if
            success = .true.
        end select
    end function get_call_info

    ! Get program information
    function get_program_info(arena, node_index, name, body_indices) result(success)
        use fortfront, only: program_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: body_indices(:)
        logical :: success

        success = .false.
        name = ""
        allocate(body_indices(0))

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            if (allocated(node%name)) then
                name = node%name
            end if
            if (allocated(node%body_indices)) then
                deallocate(body_indices)
                allocate(body_indices(size(node%body_indices)))
                body_indices = node%body_indices
            end if
            success = .true.
        end select
    end function get_program_info

    ! Get unused procedures from call graph
    function get_unused_procedures(graph) result(unused)
        use fortfront, only: call_graph_t
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: unused(:)

        ! Stub implementation - returns empty list
        allocate(character(len=0) :: unused(0))
    end function get_unused_procedures

end module fortfront_compat
