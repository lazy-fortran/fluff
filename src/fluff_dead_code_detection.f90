module fluff_dead_code_detection
    use fluff_core
    use fluff_diagnostics
    use fluff_ast
    use ast_nodes_misc, only: module_procedure_node, visibility_statement_node
    use ast_nodes_transfer, only: goto_node
    use fortfront, only: ast_arena_t, semantic_context_t, &
                         lex_source, parse_tokens, analyze_semantics, &
                         create_ast_arena, create_semantic_context, &
                         declaration_node, identifier_node, assignment_node, &
                         function_def_node, subroutine_def_node, &
                         return_node, stop_node, if_node, &
                         ast_node, program_node, binary_op_node, &
                         call_or_subscript_node, subroutine_call_node, &
                         literal_node, print_statement_node, do_loop_node, &
                         do_while_node, select_case_node, derived_type_node, &
                         interface_block_node, module_node, use_statement_node, &
                         include_statement_node, parameter_declaration_node, &
                         LITERAL_LOGICAL, get_node_type_id_from_arena, &
                         symbol_reference_t, &
                         NODE_RETURN, NODE_STOP, NODE_CYCLE, NODE_EXIT, &
                         visit_node_at, get_node_type_id, &
                         call_graph_t, build_call_graph_from_arena, is_procedure_used

    implicit none
    private

    public :: dead_code_detector_t
    public :: unused_variable_t
    public :: unreachable_code_t

    ! Unused variable information
    type, public :: unused_variable_t
        character(len=:), allocatable :: variable_name
        character(len=:), allocatable :: scope_name
        integer :: declaration_line = 0
        integer :: declaration_column = 0
        logical :: is_parameter = .false.
        logical :: is_dummy_argument = .false.
        logical :: has_initialization = .false.
        character(len=:), allocatable :: variable_type
    contains
        procedure :: to_diagnostic => unused_variable_to_diagnostic
    end type unused_variable_t

    ! Unreachable code information
    type, public :: unreachable_code_t
        integer :: start_line = 0
        integer :: end_line = 0
        integer :: start_column = 0
        integer :: end_column = 0
        character(len=:), allocatable :: reason  ! e.g., "after_return"
        character(len=:), allocatable :: code_snippet
    contains
        procedure :: to_diagnostic => unreachable_code_to_diagnostic
    end type unreachable_code_t

    ! Dead code analyzer for AST-based analysis
    type, public :: dead_code_visitor_t
        type(unused_variable_t), allocatable :: unused_variables(:)
        type(unreachable_code_t), allocatable :: unreachable_code_blocks(:)
        integer :: unused_count = 0
        integer :: unreachable_count = 0
        character(len=:), allocatable :: declared_variables(:)
        character(len=:), allocatable :: used_variables(:)
        character(len=:), allocatable :: self_assigned_only(:)
        integer :: declared_count = 0
        integer :: used_count = 0
        integer :: self_assigned_count = 0
        logical :: after_terminating_statement = .false.
    contains
        ! Additional procedures for analysis
        procedure :: add_declared_variable => dc_add_declared_variable
        procedure :: add_used_variable => dc_add_used_variable
        procedure :: add_self_assigned_only => dc_add_self_assigned_only
        procedure :: is_variable_used => dc_is_variable_used
        procedure :: is_only_self_assigned => dc_is_only_self_assigned
        procedure :: add_unreachable_code => dc_add_unreachable_code
        procedure :: finalize_analysis => dc_finalize_analysis
        procedure :: clear => dc_clear
    end type dead_code_visitor_t

    ! Main dead code detector using AST analysis
    type, public :: dead_code_detector_t
        type(dead_code_visitor_t) :: visitor
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        type(call_graph_t) :: call_graph
        logical :: analyze_cross_module = .false.
    contains
        procedure :: analyze_source_code => detector_analyze_source_ast
        procedure :: process_node => detector_process_node
        procedure :: process_indices => detector_process_indices
        procedure :: process_parameter_declarations => &
            detector_process_parameter_declarations
        procedure :: process_node_enhanced => detector_process_node_enhanced
        procedure :: detect_unreachable_code => detector_detect_unreachable_code
        procedure :: mark_subsequent_unreachable => detector_mark_subsequent_unreachable
        procedure :: check_impossible_condition => detector_check_impossible_condition
        procedure :: mark_if_block_unreachable => detector_mark_if_block_unreachable
        procedure :: get_diagnostics => detector_get_diagnostics
        procedure :: clear => detector_clear
        procedure :: process_internal_procedures => detector_process_internal_procedures
        procedure :: is_procedure_called => detector_is_procedure_called
        procedure :: analyze_unused_procedures => detector_analyze_unused_procedures
        procedure :: is_unconditional_terminator => detector_is_unconditional_terminator
        procedure :: detect_goto_unreachable_code_ast => &
            detector_detect_goto_unreachable_code_ast
    end type dead_code_detector_t

contains

    ! AST-based dead code detection using fortfront APIs
    function detector_analyze_source_ast(this, source_code, file_path) &
        result(found_dead_code)
        use fortfront, only: token_t
        class(dead_code_detector_t), intent(inout) :: this
        character(len=*), intent(in) :: source_code
        character(len=*), intent(in) :: file_path
        logical :: found_dead_code

        ! Local variables
        character(len=:), allocatable :: source_copy
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        integer :: i, prog_index

        found_dead_code = .false.

        ! Clear previous results
        call this%clear()

        ! Parse source code using fortfront AST API
        call lex_source(source_code, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            ! Skip analysis if parsing fails
            return
        end if

        this%arena = create_ast_arena()
        call parse_tokens(tokens, this%arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            ! Skip analysis if parsing fails
            return
        end if

        call create_semantic_context(this%sem_ctx)
        call analyze_semantics(this%arena, prog_index)

        ! Clear visitor state
        call this%visitor%clear()

        ! 1. Detect unreachable code after return/stop statements
        call this%detect_unreachable_code()

        call this%detect_goto_unreachable_code_ast()

        ! 2. Build call graph for unused procedure detection
        this%call_graph = build_call_graph_from_arena(this%arena, prog_index)
        call this%analyze_unused_procedures()

        ! 3. Analyze variable usage for unused variables
        ! Process all nodes to collect declarations and usages
        do i = 1, this%arena%size
            if (i > 0 .and. i <= this%arena%size .and. &
                allocated(this%arena%entries(i)%node)) then
                call this%process_node_enhanced(i)
            end if
        end do

        ! Note: Some constructs may not be fully analyzed due to fortfront limitations

        ! Finalize analysis to identify unused variables
        call this%visitor%finalize_analysis()

        ! Check if we found any dead code
        found_dead_code = this%visitor%unused_count > 0 .or. &
                          this%visitor%unreachable_count > 0

    end function detector_analyze_source_ast

    function detector_get_diagnostics(this) result(diagnostics)
        class(dead_code_detector_t), intent(in) :: this
        type(diagnostic_t), allocatable :: diagnostics(:)

        integer :: total_count, i, idx

        total_count = this%visitor%unused_count + this%visitor%unreachable_count
        allocate (diagnostics(total_count))

        idx = 1

        ! Add unused variable diagnostics
        if (allocated(this%visitor%unused_variables)) then
            do i = 1, size(this%visitor%unused_variables)
                diagnostics(idx) = this%visitor%unused_variables(i)%to_diagnostic()
                idx = idx + 1
            end do
        end if

        ! Add unreachable code diagnostics
        if (allocated(this%visitor%unreachable_code_blocks)) then
            do i = 1, size(this%visitor%unreachable_code_blocks)
                diagnostics(idx) = &
                    this%visitor%unreachable_code_blocks(i)%to_diagnostic()
                idx = idx + 1
            end do
        end if

    end function detector_get_diagnostics

    subroutine detector_clear(this)
        class(dead_code_detector_t), intent(inout) :: this

        call this%visitor%clear()

    end subroutine detector_clear

    ! Detect unreachable code by analyzing control flow in AST
    subroutine detector_detect_unreachable_code(this)
        class(dead_code_detector_t), intent(inout) :: this
        integer :: i, node_type

        ! Detect unreachable code using AST traversal

        ! For each node, check if it's a terminating statement
        do i = 1, this%arena%size
            if (.not. allocated(this%arena%entries(i)%node)) cycle

            ! Get the node type using proper fortfront API
            node_type = get_node_type_id_from_arena(this%arena, i)

            ! Check if this node is a terminating statement
            select case (node_type)
            case (NODE_RETURN)
                ! Only mark subsequent code as unreachable if this is an unconditional return
                ! (i.e., not inside a conditional block like if/then)
                if (this%is_unconditional_terminator(i)) then
                    call this%mark_subsequent_unreachable(i)
                else
                    ! Return is conditional - don't mark subsequent code as unreachable
                end if
            case (NODE_STOP)
                ! Only mark subsequent code as unreachable if this is an unconditional stop
                if (this%is_unconditional_terminator(i)) then
                    call this%mark_subsequent_unreachable(i)
                end if
            case (NODE_CYCLE, NODE_EXIT)
                ! These also make subsequent code unreachable in some cases
                ! But only if unconditional
                if (this%is_unconditional_terminator(i)) then
                    call this%mark_subsequent_unreachable(i)
                end if
            end select

            ! Also check for impossible conditions (if_node with literal false)
            select type (node => this%arena%entries(i)%node)
            type is (if_node)
                call this%check_impossible_condition(i)
            end select
        end do

    end subroutine detector_detect_unreachable_code

    ! Mark subsequent statements in the same block as unreachable
    subroutine detector_mark_subsequent_unreachable(this, terminator_idx)
        class(dead_code_detector_t), intent(inout) :: this
        integer, intent(in) :: terminator_idx
        integer :: i, terminator_depth, parent_idx
        logical :: in_same_block

        if (terminator_idx <= 0 .or. terminator_idx > this%arena%size) return

        ! Mark subsequent unreachable statements

        terminator_depth = this%arena%entries(terminator_idx)%depth
        parent_idx = this%arena%entries(terminator_idx)%parent_index
        in_same_block = .true.

        ! Look for sibling nodes after the terminator
        do i = terminator_idx + 1, this%arena%size
            if (.not. allocated(this%arena%entries(i)%node)) cycle

            ! Stop marking as unreachable if we've left the terminator's immediate block
            ! If depth is less than terminator, we've exited to a parent scope
            if (this%arena%entries(i)%depth < terminator_depth) then
                exit  ! Exited the conditional block - stop marking as unreachable
            end if

            ! Check if we're still in the same sequential block
            ! We need to stop if we encounter an end statement or exit the block
            select type (node => this%arena%entries(i)%node)
            class is (ast_node)
                ! Check for block-ending nodes
                if (this%arena%entries(i)%node_type == "end" .or. &
                    this%arena%entries(i)%node_type == "else" .or. &
                    this%arena%entries(i)%node_type == "elseif" .or. &
                    this%arena%entries(i)%node_type == "case") then
                    in_same_block = .false.
                    exit
                end if
            end select

            ! Only mark code at the SAME depth and with same parent as unreachable
            ! This ensures we stay within the conditional block
            if (this%arena%entries(i)%parent_index == parent_idx .and. &
                this%arena%entries(i)%depth == terminator_depth .and. &
                in_same_block) then
                ! This is unreachable code within the same conditional block
                select type (node => this%arena%entries(i)%node)
                type is (print_statement_node)
                    call this%visitor%add_unreachable_code( &
                        node%line, node%line, node%column, node%column + 10, &
                        "after_termination", "Code after terminating statement")
                type is (assignment_node)
                    call this%visitor%add_unreachable_code( &
                        node%line, node%line, node%column, node%column + 10, &
                        "after_termination", "Code after terminating statement")
                type is (subroutine_call_node)
                    call this%visitor%add_unreachable_code( &
                        node%line, node%line, node%column, node%column + 10, &
                        "after_termination", "Code after terminating statement")
                end select
            end if
        end do

    end subroutine detector_mark_subsequent_unreachable

    ! Check for impossible conditions like if (.false.)
    subroutine detector_check_impossible_condition(this, if_idx)
        class(dead_code_detector_t), intent(inout) :: this
        integer, intent(in) :: if_idx
        integer :: cond_idx

        select type (node => this%arena%entries(if_idx)%node)
        type is (if_node)
            cond_idx = node%condition_index
            if (cond_idx > 0 .and. cond_idx <= this%arena%size) then
                if (allocated(this%arena%entries(cond_idx)%node)) then
                    select type (cond => this%arena%entries(cond_idx)%node)
                    type is (literal_node)
                        ! Check if it's a literal false
                        if (cond%literal_kind == LITERAL_LOGICAL .and. &
                            (cond%value == ".false." .or. cond%value == ".FALSE.")) then
                            ! Mark the then-block as unreachable
                            call this%mark_if_block_unreachable(if_idx, .true.)
                        end if
                    end select
                end if
            end if
        end select

    end subroutine detector_check_impossible_condition

    ! Mark if-block contents as unreachable
    subroutine detector_mark_if_block_unreachable(this, if_idx, is_then_block)
        class(dead_code_detector_t), intent(inout) :: this
        integer, intent(in) :: if_idx
        logical, intent(in) :: is_then_block
        integer :: i, if_depth
        logical :: in_target_block

        if_depth = this%arena%entries(if_idx)%depth
        in_target_block = .false.

        ! Find the then-block contents
        do i = if_idx + 1, this%arena%size
            if (.not. allocated(this%arena%entries(i)%node)) cycle

            ! Exit if we've left the if statement
            if (this%arena%entries(i)%depth <= if_depth) exit

            ! We're in the then block (first child block)
            if (this%arena%entries(i)%depth == if_depth + 1) then
                in_target_block = is_then_block
            end if

            if (in_target_block) then
                select type (node => this%arena%entries(i)%node)
                class is (ast_node)
                    call this%visitor%add_unreachable_code( &
                        node%line, node%line, node%column, node%column + 10, &
                        "impossible_condition", "code in always-false condition")
                end select
            end if
        end do

    end subroutine detector_mark_if_block_unreachable

    ! Process AST node for dead code analysis
    subroutine detector_process_node(this, node_index)
        class(dead_code_detector_t), intent(inout) :: this
        integer, intent(in) :: node_index

        ! Check if node exists and get basic node information
        integer :: node_type_id
        character(len=50) :: node_type
        logical :: exists

        ! Check if node exists using arena size
        if (node_index <= 0 .or. node_index > this%arena%size) return
        if (.not. allocated(this%arena%entries(node_index)%node)) return

        ! Get node type for analysis
        node_type_id = get_node_type_id_from_arena(this%arena, node_index)
        node_type = "unknown"

    end subroutine detector_process_node

    ! Process an array of node indices
    subroutine detector_process_indices(this, indices)
        class(dead_code_detector_t), intent(inout) :: this
        integer, intent(in) :: indices(:)
        integer :: i

        do i = 1, size(indices)
            if (indices(i) > 0 .and. indices(i) <= this%arena%size) then
                if (allocated(this%arena%entries(indices(i))%node)) then
                    call this%process_node(indices(i))
                end if
            end if
        end do
    end subroutine detector_process_indices

    ! Process parameter declarations
    subroutine detector_process_parameter_declarations(this, param_indices)
        class(dead_code_detector_t), intent(inout) :: this
        integer, intent(in) :: param_indices(:)
        integer :: i

        do i = 1, size(param_indices)
            if (param_indices(i) > 0 .and. param_indices(i) <= this%arena%size) then
                if (allocated(this%arena%entries(param_indices(i))%node)) then
                    select type (param_node => &
                                 this%arena%entries(param_indices(i))%node)
                    type is (parameter_declaration_node)
                        ! Declare parameter as a variable
                        call this%visitor%add_declared_variable(param_node%name)
                    type is (declaration_node)
                        ! Sometimes parameters are declaration nodes
                        call this%visitor%add_declared_variable(param_node%var_name)
                    end select
                end if
            end if
        end do
    end subroutine detector_process_parameter_declarations

    ! Enhanced node processing using new fortfront APIs
    recursive subroutine detector_process_node_enhanced(this, node_index)
        class(dead_code_detector_t), intent(inout) :: this
        integer, intent(in) :: node_index

        character(len=:), allocatable :: var_name, operator_str, type_spec
        character(len=:), allocatable :: var_names(:), attributes(:)
        character(len=:), allocatable :: target_var_name, value_var_name
        integer :: left_index, right_index, target_index, value_index, i
        integer, allocatable :: indices(:)
        logical :: found, is_self_assignment

        if (node_index <= 0 .or. node_index > this%arena%size) return
        if (.not. allocated(this%arena%entries(node_index)%node)) return

        ! Process based on node type

        select type (node => this%arena%entries(node_index)%node)
        type is (declaration_node)
            ! Get declaration info using fortfront API
            found = get_declaration_info(this%arena, node_index, var_names, type_spec, &
                                         attributes)
            if (found .and. allocated(var_names)) then
                do i = 1, size(var_names)
                    call this%visitor%add_declared_variable(var_names(i))
                end do
            end if

        type is (identifier_node)
            ! Get identifier name using fortfront API
            found = get_identifier_name(this%arena, node_index, var_name)
            if (found .and. allocated(var_name)) then
                call this%visitor%add_used_variable(var_name)
            end if

        type is (assignment_node)
            ! Get assignment info using fortfront API
            found = get_assignment_indices(this%arena, node_index, target_index, &
                                           value_index, operator_str)

            if (found) then
                is_self_assignment = .false.

                ! Process target (left side) - this is a definition, not a use
                if (target_index > 0) then
                    found = get_identifier_name(this%arena, target_index, &
                                                target_var_name)
                    ! Don't count assignment target as usage
                end if

                ! Check for self-assignment (x = x)
                if (value_index > 0 .and. allocated(target_var_name)) then
                    found = get_identifier_name(this%arena, value_index, value_var_name)
                    if (found .and. allocated(value_var_name)) then
                        if (target_var_name == value_var_name) then
                            is_self_assignment = .true.
                            ! Track this as a self-assigned variable
                            call this%visitor%add_self_assigned_only(target_var_name)
                        end if
                    end if
                end if

                ! Process value (right side). This counts as usage unless self-assignment.
                if (value_index > 0 .and. .not. is_self_assignment) then
                    call this%process_node_enhanced(value_index)
                end if
            end if

        type is (binary_op_node)
            ! Get binary operation info using fortfront API
            found = get_binary_op_info(this%arena, node_index, left_index, &
                                       right_index, operator_str)

            if (found) then
                ! Process both operands
                if (left_index > 0) call this%process_node_enhanced(left_index)
                if (right_index > 0) call this%process_node_enhanced(right_index)
            end if

        type is (call_or_subscript_node)
            ! Process function/array reference using fortfront API
            found = get_call_info(this%arena, node_index, var_name, indices)
            if (found) then
                if (allocated(var_name)) then
                    call this%visitor%add_used_variable(var_name)
                end if
                if (allocated(indices)) then
                    do i = 1, size(indices)
                        if (indices(i) > 0) call this%process_node_enhanced(indices(i))
                    end do
                end if
            end if

        type is (subroutine_call_node)
            ! Track subroutine calls - simplified due to field access limitations

        type is (print_statement_node)
            ! Process print arguments - simplified due to field access limitations

        type is (if_node)
            ! Process condition and body - limited due to field access limitations
            ! We can still check for impossible conditions
            if (node%condition_index > 0) then
                call this%process_node_enhanced(node%condition_index)
            end if

        type is (do_loop_node)
            ! Process loop variable and body - simplified due to field access limitations

        type is (literal_node)
            ! Literals don't contain identifiers to process

        type is (program_node)
            ! Process program body using fortfront API
            found = get_program_info(this%arena, node_index, var_name, indices)
            if (found) then
                if (allocated(indices)) then
                    do i = 1, size(indices)
                        if (indices(i) > 0) call this%process_node_enhanced(indices(i))
                    end do
                end if
            end if
            ! Note: Internal procedures will be analyzed by call graph

        type is (subroutine_def_node)
            ! Process subroutine parameters and body - simplified due to field access limitations
            ! Mark the subroutine name if available
            if (allocated(node%name)) then
                ! Track that this is a defined subroutine
            end if

        type is (function_def_node)
            ! Process function parameters and body - simplified due to field access limitations
            ! Mark the function name if available
            if (allocated(node%name)) then
                ! Track that this is a defined function
            end if

        type is (module_node)
            ! Process module body - simplified due to field access limitations

        type is (return_node)
            ! Return statements don't have identifiers but mark control flow

        type is (stop_node)
            ! Stop statements may have error codes - simplified due to field access limitations

        type is (parameter_declaration_node)
            ! Parameter declarations need special handling
            ! They declare parameters, not regular variables

        class default
            ! For other node types, process any children we can find
        end select

    end subroutine detector_process_node_enhanced

    ! Helper to process internal procedures
    subroutine detector_process_internal_procedures(this, proc_indices)
        class(dead_code_detector_t), intent(inout) :: this
        integer, intent(in) :: proc_indices(:)
        integer :: i
        logical :: is_called
        character(len=:), allocatable :: proc_name

        ! Check each internal procedure for usage
        do i = 1, size(proc_indices)
            if (proc_indices(i) > 0 .and. proc_indices(i) <= this%arena%size) then
                if (allocated(this%arena%entries(proc_indices(i))%node)) then
                    is_called = .false.
                    select type (proc_node => this%arena%entries(proc_indices(i))%node)
                    type is (subroutine_def_node)
                        proc_name = proc_node%name
                        ! Check if this procedure is called anywhere
                        is_called = this%is_procedure_called(proc_name)
                        if (.not. is_called) then
                            ! Add as unused procedure
                            call this%visitor%add_unreachable_code( &
                                proc_node%line, proc_node%line, proc_node%column, &
                                proc_node%column + 10, &
                                "unused_procedure", &
                                "Unused internal procedure '"//proc_name//"'")
                        end if
                    type is (function_def_node)
                        proc_name = proc_node%name
                        ! Check if this function is called anywhere
                        is_called = this%is_procedure_called(proc_name)
                        if (.not. is_called) then
                            ! Add as unused procedure
                            call this%visitor%add_unreachable_code( &
                                proc_node%line, proc_node%line, proc_node%column, &
                                proc_node%column + 10, &
                                "unused_procedure", &
                                "Unused internal function '"//proc_name//"'")
                        end if
                    end select
                    ! Process the procedure body regardless
                    call this%process_node_enhanced(proc_indices(i))
                end if
            end if
        end do
    end subroutine detector_process_internal_procedures

    ! Check if a procedure is called anywhere in the AST
    function detector_is_procedure_called(this, proc_name) result(is_called)
        class(dead_code_detector_t), intent(in) :: this
        character(len=*), intent(in) :: proc_name
        logical :: is_called
        is_called = is_procedure_used(this%call_graph, proc_name)
    end function detector_is_procedure_called

    ! Analyze unused procedures using call graph
    subroutine detector_analyze_unused_procedures(this)
        class(dead_code_detector_t), intent(inout) :: this
        character(len=:), allocatable :: public_names(:)
        character(len=:), allocatable :: interface_names(:)
        integer :: i
        character(len=:), allocatable :: proc_name
        integer :: proc_line, proc_column

        call collect_public_procedure_names(this, public_names)
        call collect_interface_procedure_names(this, interface_names)

        do i = 1, this%arena%size
            if (.not. allocated(this%arena%entries(i)%node)) cycle

            proc_name = ""
            proc_line = 0
            proc_column = 0
            select type (node => this%arena%entries(i)%node)
            type is (subroutine_def_node)
                proc_name = node%name
                proc_line = node%line
                proc_column = node%column
            type is (function_def_node)
                proc_name = node%name
                proc_line = node%line
                proc_column = node%column
            class default
                cycle
            end select

            if (len_trim(proc_name) == 0) cycle
            if (is_procedure_used(this%call_graph, trim(proc_name))) cycle
            if (name_in_list(public_names, proc_name)) cycle
            if (name_in_list(interface_names, proc_name)) cycle

            call this%visitor%add_unreachable_code( &
                proc_line, proc_line, proc_column, proc_column + 20, &
                "unused_procedure", "Unused procedure '"//trim(proc_name)//"'")
        end do

    end subroutine detector_analyze_unused_procedures

    pure logical function name_in_list(names, name) result(found)
        character(len=*), intent(in) :: names(:)
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        do i = 1, size(names)
            if (trim(names(i)) == trim(name)) then
                found = .true.
                return
            end if
        end do
    end function name_in_list

    subroutine collect_public_procedure_names(this, names)
        class(dead_code_detector_t), intent(in) :: this
        character(len=:), allocatable, intent(out) :: names(:)

        character(len=256), allocatable :: tmp(:)
        integer :: i, j, count

        allocate (tmp(64))
        tmp = ""
        count = 0

        do i = 1, this%arena%size
            if (.not. allocated(this%arena%entries(i)%node)) cycle
            select type (node => this%arena%entries(i)%node)
            type is (visibility_statement_node)
                if (node%is_private) cycle
                if (.not. node%has_list) cycle
                if (.not. allocated(node%names)) cycle
                do j = 1, size(node%names)
                    if (.not. allocated(node%names(j)%s)) cycle
                    if (len_trim(node%names(j)%s) == 0) cycle
                    if (count >= size(tmp)) then
                        call grow_name_list(tmp, size(tmp)*2)
                    end if
                    count = count + 1
                    tmp(count) = node%names(j)%s
                end do
            end select
        end do

        call finalize_name_list(tmp, count, names)
    end subroutine collect_public_procedure_names

    subroutine collect_interface_procedure_names(this, names)
        class(dead_code_detector_t), intent(in) :: this
        character(len=:), allocatable, intent(out) :: names(:)

        character(len=256), allocatable :: tmp(:)
        integer :: i, j, k, count, idx

        allocate (tmp(64))
        tmp = ""
        count = 0

        do i = 1, this%arena%size
            if (.not. allocated(this%arena%entries(i)%node)) cycle
            select type (iface => this%arena%entries(i)%node)
            type is (interface_block_node)
                if (.not. allocated(iface%procedure_indices)) cycle
                do j = 1, size(iface%procedure_indices)
                    idx = iface%procedure_indices(j)
                    if (idx <= 0 .or. idx > this%arena%size) cycle
                    if (.not. allocated(this%arena%entries(idx)%node)) cycle
                    select type (proc_decl => this%arena%entries(idx)%node)
                    type is (module_procedure_node)
                        if (.not. allocated(proc_decl%procedure_names)) cycle
                        do k = 1, size(proc_decl%procedure_names)
                            if (.not. allocated(proc_decl%procedure_names(k)%s)) cycle
                            if (len_trim(proc_decl%procedure_names(k)%s) == 0) cycle
                            if (count >= size(tmp)) then
                                call grow_name_list(tmp, size(tmp)*2)
                            end if
                            count = count + 1
                            tmp(count) = proc_decl%procedure_names(k)%s
                        end do
                    end select
                end do
            end select
        end do

        call finalize_name_list(tmp, count, names)
    end subroutine collect_interface_procedure_names

    subroutine grow_name_list(list, new_size)
        character(len=256), allocatable, intent(inout) :: list(:)
        integer, intent(in) :: new_size

        character(len=256), allocatable :: tmp(:)
        integer :: old_size

        old_size = size(list)
        if (new_size <= old_size) return

        allocate (tmp(new_size))
        tmp = ""
        tmp(1:old_size) = list
        call move_alloc(tmp, list)
    end subroutine grow_name_list

    subroutine finalize_name_list(tmp, count, names)
        character(len=256), allocatable, intent(inout) :: tmp(:)
        integer, intent(in) :: count
        character(len=:), allocatable, intent(out) :: names(:)
        integer :: max_len, i

        if (count == 0) then
            allocate (character(len=0) :: names(0))
            return
        end if

        max_len = maxval([(len_trim(tmp(i)), i=1, count)])
        allocate (character(len=max_len) :: names(count))
        names = tmp(1:count)
    end subroutine finalize_name_list

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

    function get_declaration_info(arena, node_index, var_names, type_spec, attributes) &
        result(success)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: var_names(:)
        character(len=:), allocatable, intent(out) :: type_spec
        character(len=:), allocatable, intent(out) :: attributes(:)
        logical :: success
        integer :: attr_count

        success = .false.
        allocate (character(len=0) :: var_names(0))
        allocate (character(len=0) :: attributes(0))
        type_spec = ""

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (declaration_node)
            if (node%is_multi_declaration .and. allocated(node%var_names)) then
                deallocate (var_names)
                allocate (var_names(size(node%var_names)), source=node%var_names)
            else if (allocated(node%var_name)) then
                deallocate (var_names)
                allocate (character(len=len(node%var_name)) :: var_names(1))
                var_names(1) = node%var_name
            end if

            if (allocated(node%type_name)) then
                type_spec = node%type_name
            end if

            attr_count = 0
            if (node%is_allocatable) attr_count = attr_count + 1
            if (node%is_pointer) attr_count = attr_count + 1
            if (node%is_target) attr_count = attr_count + 1
            if (node%is_optional) attr_count = attr_count + 1
            if (node%is_parameter) attr_count = attr_count + 1

            if (attr_count > 0) then
                deallocate (attributes)
                allocate (character(len=16) :: attributes(attr_count))
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

    function get_assignment_indices(arena, node_index, target_index, value_index, &
                                    operator_str) result(success)
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
            if (allocated(node%operator)) operator_str = node%operator
            success = .true.
        end select
    end function get_assignment_indices

    function get_binary_op_info(arena, node_index, left_index, right_index, &
                                operator_str) result(success)
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
            if (allocated(node%operator)) operator_str = node%operator
            success = .true.
        end select
    end function get_binary_op_info

    function get_call_info(arena, node_index, name, arg_indices) result(success)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: arg_indices(:)
        logical :: success

        success = .false.
        name = ""
        allocate (arg_indices(0))

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (call_or_subscript_node)
            if (allocated(node%name)) name = node%name
            if (allocated(node%arg_indices)) then
                deallocate (arg_indices)
                allocate (arg_indices(size(node%arg_indices)))
                arg_indices = node%arg_indices
            end if
            success = .true.
        end select
    end function get_call_info

    function get_program_info(arena, node_index, name, body_indices) result(success)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: body_indices(:)
        logical :: success

        success = .false.
        name = ""
        allocate (body_indices(0))

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            if (allocated(node%name)) name = node%name
            if (allocated(node%body_indices)) then
                deallocate (body_indices)
                allocate (body_indices(size(node%body_indices)))
                body_indices = node%body_indices
            end if
            success = .true.
        end select
    end function get_program_info

    ! Check if a terminating statement (return/stop) is unconditional
    function detector_is_unconditional_terminator(this, terminator_idx) &
        result(is_unconditional)
        class(dead_code_detector_t), intent(in) :: this
        integer, intent(in) :: terminator_idx
        logical :: is_unconditional
        integer :: current_idx

        is_unconditional = .true.
        ! Assume unconditional unless we find a conditional parent

        if (terminator_idx <= 0 .or. terminator_idx > this%arena%size) return

        ! Traverse up the parent chain looking for conditional constructs
        current_idx = this%arena%entries(terminator_idx)%parent_index

        do while (current_idx > 0 .and. current_idx <= this%arena%size)
            if (allocated(this%arena%entries(current_idx)%node)) then
                select type (ancestor => this%arena%entries(current_idx)%node)
                type is (if_node)
                    ! Found an if statement ancestor - this return is conditional
                    ! For validation functions with early returns, the return is conditional
                    is_unconditional = .false.
                    return
                type is (do_loop_node)
                    ! Found a do loop ancestor - this return is conditional
                    is_unconditional = .false.
                    return
                type is (select_case_node)
                    ! Found a select case ancestor - this return is conditional
                    is_unconditional = .false.
                    return
                type is (program_node)
                    ! Reached the program level - stop searching
                    exit
                type is (function_def_node)
                    ! Reached the function level - stop searching
                    exit
                type is (subroutine_def_node)
                    ! Reached the subroutine level - stop searching
                    exit
                end select
            end if
            ! Move to the next parent
            current_idx = this%arena%entries(current_idx)%parent_index
        end do

    end function detector_is_unconditional_terminator

    subroutine detector_detect_goto_unreachable_code_ast(this)
        class(dead_code_detector_t), intent(inout) :: this
        integer :: i, j, stmt_index
        logical :: after_goto
        character(len=:), allocatable :: target_label

        after_goto = .false.
        target_label = ""

        do i = 1, this%arena%size
            if (.not. allocated(this%arena%entries(i)%node)) cycle
            select type (prog => this%arena%entries(i)%node)
            type is (program_node)
                if (.not. allocated(prog%body_indices)) cycle
                after_goto = .false.
                target_label = ""
                do j = 1, size(prog%body_indices)
                    stmt_index = prog%body_indices(j)
                    if (stmt_index <= 0 .or. stmt_index > this%arena%size) cycle
                    if (.not. allocated(this%arena%entries(stmt_index)%node)) cycle

                    if (after_goto) then
                        select type (stmt => this%arena%entries(stmt_index)%node)
                        class is (ast_node)
                            if (allocated(stmt%stmt_label)) then
                                if (trim(stmt%stmt_label) == trim(target_label)) then
                                    after_goto = .false.
                                end if
                            end if
                        end select

                        if (after_goto) then
                            select type (stmt => this%arena%entries(stmt_index)%node)
                            class is (ast_node)
                                call this%visitor%add_unreachable_code( &
                                    stmt%line, stmt%line, stmt%column, &
                                    stmt%column + 20, &
                                    "after_goto", "Code after goto statement")
                            end select
                        end if
                    end if

                    select type (stmt => this%arena%entries(stmt_index)%node)
                    type is (goto_node)
                        if (allocated(stmt%label)) then
                            if (len_trim(stmt%label) > 0) then
                                after_goto = .true.
                                target_label = stmt%label
                            end if
                        end if
                    end select
                end do
            end select
        end do
    end subroutine detector_detect_goto_unreachable_code_ast

    ! Helper procedures for visitor integration
    subroutine add_unused_variable_to_visitor(visitor, var_name, scope, line, col, &
                                              is_param, is_dummy)
        type(dead_code_visitor_t), intent(inout) :: visitor
        character(len=*), intent(in) :: var_name, scope
        integer, intent(in) :: line, col
        logical, intent(in) :: is_param, is_dummy

        type(unused_variable_t), allocatable :: temp(:)
        integer :: n

        if (.not. allocated(visitor%unused_variables)) then
            allocate (visitor%unused_variables(1))
            n = 1
        else
            n = size(visitor%unused_variables)
            allocate (temp(n + 1))
            temp(1:n) = visitor%unused_variables
            call move_alloc(temp, visitor%unused_variables)
            n = n + 1
        end if

        visitor%unused_variables(n)%variable_name = var_name
        visitor%unused_variables(n)%scope_name = scope
        visitor%unused_variables(n)%declaration_line = line
        visitor%unused_variables(n)%declaration_column = col
        visitor%unused_variables(n)%is_parameter = is_param
        visitor%unused_variables(n)%is_dummy_argument = is_dummy
        visitor%unused_count = visitor%unused_count + 1

    end subroutine add_unused_variable_to_visitor

    subroutine add_unreachable_code_to_visitor(visitor, start_line, end_line, &
                                               start_col, end_col, reason, snippet)
        type(dead_code_visitor_t), intent(inout) :: visitor
        integer, intent(in) :: start_line, end_line, start_col, end_col
        character(len=*), intent(in) :: reason, snippet

        type(unreachable_code_t), allocatable :: temp(:)
        integer :: n

        if (.not. allocated(visitor%unreachable_code_blocks)) then
            allocate (visitor%unreachable_code_blocks(1))
            n = 1
        else
            n = size(visitor%unreachable_code_blocks)
            allocate (temp(n + 1))
            temp(1:n) = visitor%unreachable_code_blocks
            call move_alloc(temp, visitor%unreachable_code_blocks)
            n = n + 1
        end if

        visitor%unreachable_code_blocks(n)%start_line = start_line
        visitor%unreachable_code_blocks(n)%end_line = end_line
        visitor%unreachable_code_blocks(n)%start_column = start_col
        visitor%unreachable_code_blocks(n)%end_column = end_col
        visitor%unreachable_code_blocks(n)%reason = reason
        visitor%unreachable_code_blocks(n)%code_snippet = snippet
        visitor%unreachable_count = visitor%unreachable_count + 1

    end subroutine add_unreachable_code_to_visitor

    ! Unused variable diagnostic conversion
    function unused_variable_to_diagnostic(this) result(diag)
        class(unused_variable_t), intent(in) :: this
        type(diagnostic_t) :: diag

        type(source_range_t) :: location

        diag%code = "D001"
        diag%message = "Unused variable '"//this%variable_name//"'"
        diag%category = "dead_code"
        diag%severity = SEVERITY_WARNING

        location%start%line = this%declaration_line
        location%start%column = this%declaration_column
        location%end%line = this%declaration_line
        location%end%column = this%declaration_column + len(this%variable_name) - 1
        diag%location = location

        ! No fix suggestions for RED phase
        if (allocated(diag%fixes)) deallocate (diag%fixes)

    end function unused_variable_to_diagnostic

    ! Unreachable code diagnostic conversion
    function unreachable_code_to_diagnostic(this) result(diag)
        class(unreachable_code_t), intent(in) :: this
        type(diagnostic_t) :: diag

        type(source_range_t) :: location

        diag%code = "D002"
        diag%message = "Unreachable code detected ("//this%reason//")"
        diag%category = "dead_code"
        diag%severity = SEVERITY_WARNING

        location%start%line = this%start_line
        location%start%column = this%start_column
        location%end%line = this%end_line
        location%end%column = this%end_column
        diag%location = location

        ! No fix suggestions for RED phase
        if (allocated(diag%fixes)) deallocate (diag%fixes)

    end function unreachable_code_to_diagnostic

    ! Helper methods for visitor
    subroutine dc_add_declared_variable(this, var_name)
        class(dead_code_visitor_t), intent(inout) :: this
        character(len=*), intent(in) :: var_name

        character(len=:), allocatable :: temp(:)
        integer :: n, max_len, i

        if (.not. allocated(this%declared_variables)) then
            allocate (character(len=len(var_name)) :: this%declared_variables(1))
            this%declared_variables(1) = var_name
            this%declared_count = 1
        else
            n = this%declared_count
            max_len = max(len(this%declared_variables), len(var_name))
            allocate (character(len=max_len) :: temp(n + 1))

            do i = 1, n
                temp(i) = this%declared_variables(i)
            end do
            temp(n + 1) = var_name

            call move_alloc(temp, this%declared_variables)
            this%declared_count = n + 1
        end if

    end subroutine dc_add_declared_variable

    subroutine dc_add_used_variable(this, var_name)
        class(dead_code_visitor_t), intent(inout) :: this
        character(len=*), intent(in) :: var_name

        character(len=:), allocatable :: temp(:)
        integer :: n, max_len, i

        if (.not. allocated(this%used_variables)) then
            allocate (character(len=len(var_name)) :: this%used_variables(1))
            this%used_variables(1) = var_name
            this%used_count = 1
        else
            n = this%used_count
            max_len = max(len(this%used_variables), len(var_name))
            allocate (character(len=max_len) :: temp(n + 1))

            do i = 1, n
                temp(i) = this%used_variables(i)
            end do
            temp(n + 1) = var_name

            call move_alloc(temp, this%used_variables)
            this%used_count = n + 1
        end if

    end subroutine dc_add_used_variable

    function dc_is_variable_used(this, var_name) result(used)
        class(dead_code_visitor_t), intent(in) :: this
        character(len=*), intent(in) :: var_name
        logical :: used
        integer :: i

        used = .false.
        if (allocated(this%used_variables)) then
            do i = 1, this%used_count
                if (this%used_variables(i) == var_name) then
                    used = .true.
                    return
                end if
            end do
        end if

    end function dc_is_variable_used

    subroutine dc_add_self_assigned_only(this, var_name)
        class(dead_code_visitor_t), intent(inout) :: this
        character(len=*), intent(in) :: var_name

        character(len=:), allocatable :: temp(:)
        integer :: n, max_len, i

        if (.not. allocated(this%self_assigned_only)) then
            allocate (character(len=len(var_name)) :: this%self_assigned_only(1))
            this%self_assigned_only(1) = var_name
            this%self_assigned_count = 1
        else
            n = this%self_assigned_count
            max_len = max(len(this%self_assigned_only), len(var_name))
            allocate (character(len=max_len) :: temp(n + 1))

            do i = 1, n
                temp(i) = this%self_assigned_only(i)
            end do
            temp(n + 1) = var_name

            call move_alloc(temp, this%self_assigned_only)
            this%self_assigned_count = n + 1
        end if

    end subroutine dc_add_self_assigned_only

    function dc_is_only_self_assigned(this, var_name) result(is_self_only)
        class(dead_code_visitor_t), intent(in) :: this
        character(len=*), intent(in) :: var_name
        logical :: is_self_only
        integer :: i

        is_self_only = .false.
        if (allocated(this%self_assigned_only)) then
            do i = 1, this%self_assigned_count
                if (this%self_assigned_only(i) == var_name) then
                    is_self_only = .true.
                    return
                end if
            end do
        end if

    end function dc_is_only_self_assigned

    subroutine dc_add_unreachable_code(this, start_line, end_line, start_col, end_col, &
                                       reason, snippet)
        class(dead_code_visitor_t), intent(inout) :: this
        integer, intent(in) :: start_line, end_line, start_col, end_col
        character(len=*), intent(in) :: reason, snippet

        type(unreachable_code_t), allocatable :: temp(:)
        integer :: n

        if (.not. allocated(this%unreachable_code_blocks)) then
            allocate (this%unreachable_code_blocks(1))
            n = 0
        else
            n = size(this%unreachable_code_blocks)
            allocate (temp(n + 1))
            temp(1:n) = this%unreachable_code_blocks
            call move_alloc(temp, this%unreachable_code_blocks)
        end if

        this%unreachable_code_blocks(n + 1)%start_line = start_line
        this%unreachable_code_blocks(n + 1)%end_line = end_line
        this%unreachable_code_blocks(n + 1)%start_column = start_col
        this%unreachable_code_blocks(n + 1)%end_column = end_col
        this%unreachable_code_blocks(n + 1)%reason = reason
        this%unreachable_code_blocks(n + 1)%code_snippet = snippet

        this%unreachable_count = n + 1

    end subroutine dc_add_unreachable_code

    subroutine dc_finalize_analysis(this)
        class(dead_code_visitor_t), intent(inout) :: this
        integer :: i

        ! Check all declared variables for usage
        if (allocated(this%declared_variables)) then
            do i = 1, this%declared_count
                ! Unused means not used at all or only used in self-assignments.
                if (.not. this%is_variable_used(this%declared_variables(i)) .or. &
                    this%is_only_self_assigned(this%declared_variables(i))) then
                    call add_unused_variable_to_visitor(this, &
                                                        this%declared_variables(i), &
                                                            "program", 1, 1, &
                                                            .false., .false.)
                end if
            end do
        end if

    end subroutine dc_finalize_analysis

    subroutine dc_clear(this)
        class(dead_code_visitor_t), intent(inout) :: this

        if (allocated(this%unused_variables)) deallocate (this%unused_variables)
        if (allocated(this%unreachable_code_blocks)) deallocate &
            (this%unreachable_code_blocks)
        if (allocated(this%declared_variables)) deallocate (this%declared_variables)
        if (allocated(this%used_variables)) deallocate (this%used_variables)
        if (allocated(this%self_assigned_only)) deallocate (this%self_assigned_only)

        this%unused_count = 0
        this%unreachable_count = 0
        this%declared_count = 0
        this%used_count = 0
        this%self_assigned_count = 0
        this%after_terminating_statement = .false.

    end subroutine dc_clear
end module fluff_dead_code_detection
