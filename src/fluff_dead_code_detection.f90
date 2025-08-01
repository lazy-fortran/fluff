module fluff_dead_code_detection
    use fluff_core
    use fluff_diagnostics
    use fluff_ast
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
                         LITERAL_LOGICAL, get_symbol_info, get_symbol_references, &
                         get_assignment_indices, get_binary_op_info, &
                         get_identifier_name, get_call_info, get_declaration_info, &
                         traverse_ast, node_exists, symbol_info_t, &
                         symbol_reference_t, get_children
    use ast_arena, only: ast_entry_t
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
        character(len=:), allocatable :: reason  ! "after_return", "impossible_condition", etc.
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
        integer :: declared_count = 0
        integer :: used_count = 0
        logical :: after_terminating_statement = .false.
    contains
        ! Additional procedures for analysis
        procedure :: add_declared_variable => dc_add_declared_variable
        procedure :: add_used_variable => dc_add_used_variable
        procedure :: is_variable_used => dc_is_variable_used
        procedure :: finalize_analysis => dc_finalize_analysis
        procedure :: clear => dc_clear
    end type dead_code_visitor_t
    
    ! Main dead code detector using AST analysis
    type, public :: dead_code_detector_t
        type(dead_code_visitor_t) :: visitor
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        logical :: analyze_cross_module = .false.
    contains
        procedure :: analyze_source_code => detector_analyze_source_ast
        procedure :: process_node => detector_process_node
        procedure :: process_indices => detector_process_indices
        procedure :: process_parameter_declarations => detector_process_parameter_declarations
        procedure :: process_node_enhanced => detector_process_node_enhanced
        procedure :: detect_unreachable_code => detector_detect_unreachable_code
        procedure :: mark_subsequent_unreachable => detector_mark_subsequent_unreachable
        procedure :: check_impossible_condition => detector_check_impossible_condition
        procedure :: mark_if_block_unreachable => detector_mark_if_block_unreachable
        procedure :: get_diagnostics => detector_get_diagnostics
        procedure :: clear => detector_clear
    end type dead_code_detector_t
    
contains
    
    ! AST-based dead code detection
    function detector_analyze_source_ast(this, source_code, file_path) result(found_dead_code)
        use fortfront, only: token_t
        class(dead_code_detector_t), intent(inout) :: this
        character(len=*), intent(in) :: source_code
        character(len=*), intent(in) :: file_path
        logical :: found_dead_code
        
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        logical :: success
        integer :: i, prog_index
        
        found_dead_code = .false.
        
        ! Clear previous results
        call this%clear()
        
        ! Parse source code using fortfront AST API
        call lex_source(source_code, tokens, error_msg)
        if (error_msg /= "") then
            ! AST parsing failed - this is a fortfront bug
            print *, "ERROR: fortfront lex_source failed in dead code detection!"
            print *, "Error: ", error_msg
            print *, "Source file: ", file_path
            print *, "File a GitHub issue at https://github.com/fortfront/fortfront"
            error stop "AST parsing required - no fallbacks!"
        end if
        
        this%arena = create_ast_arena()
        call parse_tokens(tokens, this%arena, prog_index, error_msg)
        if (error_msg /= "") then
            ! AST parsing failed - this is a fortfront bug
            print *, "ERROR: fortfront parse_tokens failed in dead code detection!"
            print *, "Error: ", error_msg
            print *, "Source file: ", file_path
            print *, "File a GitHub issue at https://github.com/fortfront/fortfront"
            error stop "AST parsing required - no fallbacks!"
        end if
        
        this%sem_ctx = create_semantic_context()
        call analyze_semantics(this%arena, prog_index)
        
        ! Clear visitor state
        call this%visitor%clear()
        
        ! First pass: collect all declarations and usages using enhanced APIs
        do i = 1, this%arena%size
            if (node_exists(this%arena, i)) then
                call this%process_node_enhanced(i)
            end if
        end do
        
        ! Second pass: check for unreachable code
        call this%detect_unreachable_code()
        
        ! Finalize analysis to identify unused variables
        call this%visitor%finalize_analysis()
        
        ! Check if we found any dead code
        found_dead_code = this%visitor%unused_count > 0 .or. this%visitor%unreachable_count > 0
        
    end function detector_analyze_source_ast
    
    function detector_get_diagnostics(this) result(diagnostics)
        class(dead_code_detector_t), intent(in) :: this
        type(diagnostic_t), allocatable :: diagnostics(:)
        
        integer :: total_count, i, idx
        
        total_count = this%visitor%unused_count + this%visitor%unreachable_count
        allocate(diagnostics(total_count))
        
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
                diagnostics(idx) = this%visitor%unreachable_code_blocks(i)%to_diagnostic()
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
        integer :: i, j, parent_idx, block_end
        logical :: found_terminator
        
        ! For each node, check if it's in a block after a terminating statement
        do i = 1, this%arena%size
            if (.not. allocated(this%arena%entries(i)%node)) cycle
            
            ! Check if this node is a terminating statement
            select type (node => this%arena%entries(i)%node)
            type is (return_node)
                ! Find all subsequent nodes in the same block
                call this%mark_subsequent_unreachable(i)
            type is (stop_node)
                ! Find all subsequent nodes in the same block
                call this%mark_subsequent_unreachable(i)
            type is (if_node)
                ! Check for if (.false.) pattern
                call this%check_impossible_condition(i)
            end select
        end do
        
    end subroutine detector_detect_unreachable_code
    
    ! Mark subsequent statements in the same block as unreachable
    subroutine detector_mark_subsequent_unreachable(this, terminator_idx)
        class(dead_code_detector_t), intent(inout) :: this
        integer, intent(in) :: terminator_idx
        integer :: i, terminator_depth, parent_idx
        
        if (terminator_idx <= 0 .or. terminator_idx > this%arena%size) return
        
        terminator_depth = this%arena%entries(terminator_idx)%depth
        parent_idx = this%arena%entries(terminator_idx)%parent_index
        
        ! Look for sibling nodes after the terminator
        do i = terminator_idx + 1, this%arena%size
            if (.not. allocated(this%arena%entries(i)%node)) cycle
            
            ! If we've exited the parent block, stop
            if (this%arena%entries(i)%depth < terminator_depth) exit
            
            ! If this is a sibling node (same parent and depth)
            if (this%arena%entries(i)%parent_index == parent_idx .and. &
                this%arena%entries(i)%depth == terminator_depth) then
                ! This is unreachable code
                select type (node => this%arena%entries(i)%node)
                class is (ast_node)
                    call add_unreachable_code_to_visitor(this%visitor, &
                        node%line, node%line, node%column, node%column + 10, &
                        "after_termination", "code after " // this%arena%entries(terminator_idx)%node_type)
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
                    call add_unreachable_code_to_visitor(this%visitor, &
                        node%line, node%line, node%column, node%column + 10, &
                        "impossible_condition", "code in always-false condition")
                end select
            end if
        end do
        
    end subroutine detector_mark_if_block_unreachable
    
    ! Process AST node for dead code analysis
    subroutine detector_process_node(this, entry, node_index)
        class(dead_code_detector_t), intent(inout) :: this
        type(ast_entry_t), intent(in) :: entry
        integer, intent(in) :: node_index
        
        ! Check if we're after a terminating statement
        if (this%visitor%after_terminating_statement .and. &
            entry%parent_index > 0 .and. &
            entry%node_type /= "return_node" .and. &
            entry%node_type /= "stop_node") then
            ! This code is unreachable
            select type (node => entry%node)
            class is (ast_node)
                call add_unreachable_code_to_visitor(this%visitor, &
                    node%line, node%line, node%column, node%column + 10, &
                    "after_termination", "code after terminating statement")
            end select
        end if
        
        select type (node => entry%node)
        type is (declaration_node)
            ! Variable declaration
            call this%visitor%add_declared_variable(node%var_name)
        type is (identifier_node)
            ! Variable usage
            call this%visitor%add_used_variable(node%name)
        type is (assignment_node)
            ! Assignment uses variables on RHS (node%value_index)
            ! The target (node%target_index) is being assigned, not used
            if (node%value_index > 0 .and. node%value_index <= this%arena%size) then
                call this%process_node(this%arena%entries(node%value_index), node%value_index)
            end if
        type is (binary_op_node)
            ! Process both operands to find identifiers
            if (node%left_index > 0 .and. node%left_index <= this%arena%size) then
                call this%process_node(this%arena%entries(node%left_index), node%left_index)
            end if
            if (node%right_index > 0 .and. node%right_index <= this%arena%size) then
                call this%process_node(this%arena%entries(node%right_index), node%right_index)
            end if
        type is (do_loop_node)
            ! Do loops declare and use loop variables
            call this%visitor%add_declared_variable(node%var_name)
            call this%visitor%add_used_variable(node%var_name)
        type is (call_or_subscript_node)
            ! Function calls use the function name
            call this%visitor%add_used_variable(node%name)
            ! Process arguments
            if (allocated(node%arg_indices)) then
                call this%process_indices(node%arg_indices)
            end if
        type is (subroutine_call_node)
            ! Subroutine calls use the subroutine name
            call this%visitor%add_used_variable(node%name)
            ! Process arguments
            if (allocated(node%arg_indices)) then
                call this%process_indices(node%arg_indices)
            end if
        type is (function_def_node)
            ! Function definitions declare parameters
            if (allocated(node%param_indices)) then
                call this%process_parameter_declarations(node%param_indices)
            end if
        type is (subroutine_def_node)
            ! Subroutine definitions declare parameters
            if (allocated(node%param_indices)) then
                call this%process_parameter_declarations(node%param_indices)
            end if
        type is (print_statement_node)
            ! Print statements use variables in expression_indices
            if (allocated(node%expression_indices)) then
                call this%process_indices(node%expression_indices)
            end if
        type is (if_node)
            ! Process condition to find variable usage
            if (node%condition_index > 0 .and. node%condition_index <= this%arena%size) then
                call this%process_node(this%arena%entries(node%condition_index), node%condition_index)
            end if
        type is (return_node)
            ! Mark subsequent statements as potentially unreachable
            this%visitor%after_terminating_statement = .true.
        type is (stop_node)
            ! Mark subsequent statements as potentially unreachable
            this%visitor%after_terminating_statement = .true.
        class default
            ! Other node types - could still be relevant
        end select
        
    end subroutine detector_process_node
    
    ! Process an array of node indices
    subroutine detector_process_indices(this, indices)
        class(dead_code_detector_t), intent(inout) :: this
        integer, intent(in) :: indices(:)
        integer :: i
        
        do i = 1, size(indices)
            if (indices(i) > 0 .and. indices(i) <= this%arena%size) then
                if (allocated(this%arena%entries(indices(i))%node)) then
                    call this%process_node(this%arena%entries(indices(i)), indices(i))
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
                    select type (param_node => this%arena%entries(param_indices(i))%node)
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
        
        character(len=:), allocatable :: name, var_names(:), type_spec, attributes(:)
        integer :: target_index, value_index, left_index, right_index
        integer, allocatable :: arg_indices(:), child_indices(:)
        character(len=:), allocatable :: operator
        logical :: found
        integer :: i
        
        ! Use new accessor functions for type-safe access
        if (get_identifier_name(this%arena, node_index, name)) then
            ! Variable usage
            call this%visitor%add_used_variable(name)
            
        else if (get_assignment_indices(this%arena, node_index, target_index, value_index, operator)) then
            ! Process assignment - value side for usage
            if (value_index > 0) then
                call this%process_node_enhanced(value_index)
            end if
            
        else if (get_binary_op_info(this%arena, node_index, left_index, right_index, operator)) then
            ! Process both operands
            if (left_index > 0) then
                call this%process_node_enhanced(left_index)
            end if
            if (right_index > 0) then
                call this%process_node_enhanced(right_index)
            end if
            
        else if (get_call_info(this%arena, node_index, name, arg_indices)) then
            ! Function/procedure call uses the name
            if (allocated(name) .and. name /= "") then
                call this%visitor%add_used_variable(name)
            end if
            ! Process arguments
            do i = 1, size(arg_indices)
                if (arg_indices(i) > 0) then
                    call this%process_node_enhanced(arg_indices(i))
                end if
            end do
            
        else if (get_declaration_info(this%arena, node_index, var_names, type_spec, attributes)) then
            ! Variable declarations
            do i = 1, size(var_names)
                if (len_trim(var_names(i)) > 0) then
                    call this%visitor%add_declared_variable(trim(var_names(i)))
                end if
            end do
        end if
        
        ! Process child nodes using new API
        child_indices = get_children(this%arena, node_index)
        do i = 1, size(child_indices)
            call this%process_node_enhanced(child_indices(i))
        end do
        
    end subroutine detector_process_node_enhanced
    
    ! Helper procedures for visitor integration
    subroutine add_unused_variable_to_visitor(visitor, var_name, scope, line, col, is_param, is_dummy)
        type(dead_code_visitor_t), intent(inout) :: visitor
        character(len=*), intent(in) :: var_name, scope
        integer, intent(in) :: line, col
        logical, intent(in) :: is_param, is_dummy
        
        type(unused_variable_t), allocatable :: temp(:)
        integer :: n
        
        if (.not. allocated(visitor%unused_variables)) then
            allocate(visitor%unused_variables(1))
            n = 1
        else
            n = size(visitor%unused_variables)
            allocate(temp(n + 1))
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
    
    subroutine add_unreachable_code_to_visitor(visitor, start_line, end_line, start_col, end_col, reason, snippet)
        type(dead_code_visitor_t), intent(inout) :: visitor
        integer, intent(in) :: start_line, end_line, start_col, end_col
        character(len=*), intent(in) :: reason, snippet
        
        type(unreachable_code_t), allocatable :: temp(:)
        integer :: n
        
        if (.not. allocated(visitor%unreachable_code_blocks)) then
            allocate(visitor%unreachable_code_blocks(1))
            n = 1
        else
            n = size(visitor%unreachable_code_blocks)
            allocate(temp(n + 1))
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
        diag%message = "Unused variable '" // this%variable_name // "'"
        diag%category = "dead_code"
        diag%severity = SEVERITY_WARNING
        
        location%start%line = this%declaration_line
        location%start%column = this%declaration_column
        location%end%line = this%declaration_line
        location%end%column = this%declaration_column + len(this%variable_name) - 1
        diag%location = location
        
        ! No fix suggestions for RED phase
        if (allocated(diag%fixes)) deallocate(diag%fixes)
        
    end function unused_variable_to_diagnostic
    
    ! Unreachable code diagnostic conversion
    function unreachable_code_to_diagnostic(this) result(diag)
        class(unreachable_code_t), intent(in) :: this
        type(diagnostic_t) :: diag
        
        type(source_range_t) :: location
        
        diag%code = "D002"
        diag%message = "Unreachable code detected (" // this%reason // ")"
        diag%category = "dead_code"
        diag%severity = SEVERITY_WARNING
        
        location%start%line = this%start_line
        location%start%column = this%start_column
        location%end%line = this%end_line
        location%end%column = this%end_column
        diag%location = location
        
        ! No fix suggestions for RED phase
        if (allocated(diag%fixes)) deallocate(diag%fixes)
        
    end function unreachable_code_to_diagnostic
    
    
    
    ! Helper methods for visitor
    subroutine dc_add_declared_variable(this, var_name)
        class(dead_code_visitor_t), intent(inout) :: this
        character(len=*), intent(in) :: var_name
        
        character(len=:), allocatable :: temp(:)
        integer :: n, max_len, i
        
        if (.not. allocated(this%declared_variables)) then
            allocate(character(len=len(var_name)) :: this%declared_variables(1))
            this%declared_variables(1) = var_name
            this%declared_count = 1
        else
            n = this%declared_count
            max_len = max(len(this%declared_variables), len(var_name))
            allocate(character(len=max_len) :: temp(n + 1))
            
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
            allocate(character(len=len(var_name)) :: this%used_variables(1))
            this%used_variables(1) = var_name
            this%used_count = 1
        else
            n = this%used_count
            max_len = max(len(this%used_variables), len(var_name))
            allocate(character(len=max_len) :: temp(n + 1))
            
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
    
    subroutine dc_finalize_analysis(this)
        class(dead_code_visitor_t), intent(inout) :: this
        integer :: i
        
        ! Check all declared variables for usage
        if (allocated(this%declared_variables)) then
            do i = 1, this%declared_count
                if (.not. this%is_variable_used(this%declared_variables(i))) then
                    call add_unused_variable_to_visitor(this, this%declared_variables(i), "program", 1, 1, .false., .false.)
                end if
            end do
        end if
        
    end subroutine dc_finalize_analysis
    
    subroutine dc_clear(this)
        class(dead_code_visitor_t), intent(inout) :: this
        
        if (allocated(this%unused_variables)) deallocate(this%unused_variables)
        if (allocated(this%unreachable_code_blocks)) deallocate(this%unreachable_code_blocks)
        if (allocated(this%declared_variables)) deallocate(this%declared_variables)
        if (allocated(this%used_variables)) deallocate(this%used_variables)
        
        this%unused_count = 0
        this%unreachable_count = 0
        this%declared_count = 0
        this%used_count = 0
        this%after_terminating_statement = .false.
        
    end subroutine dc_clear
    
end module fluff_dead_code_detection