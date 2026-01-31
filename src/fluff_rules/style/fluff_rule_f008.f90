module fluff_rule_f008
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: declaration_node, function_def_node, identifier_node, &
                         interface_block_node, parameter_declaration_node, &
                         subroutine_def_node
    use ast_nodes_data, only: mixed_construct_container_node
    implicit none
    private

    public :: check_f008_missing_intent

contains

    subroutine check_f008_missing_intent(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count
        integer, allocatable :: interface_procs(:)
        integer :: n_interface_procs
        integer, allocatable :: start_indices(:)

        allocate (tmp(0))
        violation_count = 0

        allocate (interface_procs(0))
        n_interface_procs = 0

        call get_traversal_roots(ctx, ctx%root_index, start_indices)

        call collect_interface_procedures_multi(ctx, start_indices, interface_procs, &
                                                n_interface_procs)

        call walk_procedures_multi(ctx, start_indices, interface_procs, &
                                   n_interface_procs, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f008_missing_intent

    subroutine get_traversal_roots(ctx, root_index, start_indices)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: root_index
        integer, allocatable, intent(out) :: start_indices(:)

        if (root_index <= 0) then
            allocate (start_indices(0))
            return
        end if
        if (.not. allocated(ctx%arena%entries(root_index)%node)) then
            allocate (start_indices(0))
            return
        end if

        select type (n => ctx%arena%entries(root_index)%node)
        type is (mixed_construct_container_node)
            if (allocated(n%explicit_program_indices)) then
                start_indices = n%explicit_program_indices
            else
                allocate (start_indices(0))
            end if
        class default
            allocate (start_indices(1))
            start_indices(1) = root_index
        end select
    end subroutine get_traversal_roots

    subroutine collect_interface_procedures_multi(ctx, start_indices, interface_procs, &
                                                  n_procs)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: start_indices(:)
        integer, allocatable, intent(inout) :: interface_procs(:)
        integer, intent(inout) :: n_procs

        integer :: i

        do i = 1, size(start_indices)
            if (start_indices(i) <= 0) cycle
            call collect_interface_procedures(ctx, start_indices(i), interface_procs, &
                                              n_procs)
        end do
    end subroutine collect_interface_procedures_multi

    subroutine walk_procedures_multi(ctx, start_indices, interface_procs, n_procs, &
                                     tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: start_indices(:)
        integer, intent(in) :: interface_procs(:)
        integer, intent(in) :: n_procs
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: i

        do i = 1, size(start_indices)
            if (start_indices(i) <= 0) cycle
            call walk_procedures(ctx, start_indices(i), interface_procs, n_procs, &
                                 tmp, violation_count)
        end do
    end subroutine walk_procedures_multi

    recursive subroutine collect_interface_procedures(ctx, node_index, &
                                                      interface_procs, n_procs)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        integer, allocatable, intent(inout) :: interface_procs(:)
        integer, intent(inout) :: n_procs

        integer, allocatable :: children(:)
        integer, allocatable :: new_procs(:)
        integer :: i, j, proc_idx

        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (n => ctx%arena%entries(node_index)%node)
        type is (interface_block_node)
            if (allocated(n%procedure_indices)) then
                do j = 1, size(n%procedure_indices)
                    proc_idx = n%procedure_indices(j)
                    if (proc_idx <= 0) cycle
                    if (n_procs >= size(interface_procs)) then
                        allocate (new_procs(max(16, 2*size(interface_procs))))
                      if (n_procs > 0) new_procs(1:n_procs) = interface_procs(1:n_procs)
                        call move_alloc(new_procs, interface_procs)
                    end if
                    n_procs = n_procs + 1
                    interface_procs(n_procs) = proc_idx
                end do
            end if
        end select

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) <= 0) cycle
           call collect_interface_procedures(ctx, children(i), interface_procs, n_procs)
        end do
    end subroutine collect_interface_procedures

    recursive subroutine walk_procedures(ctx, node_index, interface_procs, n_procs, &
                                         tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        integer, intent(in) :: interface_procs(:)
        integer, intent(in) :: n_procs
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer, allocatable :: children(:)
        integer :: i
        logical :: is_interface

        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        is_interface = .false.
        do i = 1, n_procs
            if (interface_procs(i) == node_index) then
                is_interface = .true.
                exit
            end if
        end do

        if (.not. is_interface) then
            select type (n => ctx%arena%entries(node_index)%node)
            type is (subroutine_def_node)
                call check_procedure(ctx, node_index, tmp, violation_count)
            type is (function_def_node)
                call check_procedure(ctx, node_index, tmp, violation_count)
            end select
        end if

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) <= 0) cycle
            call walk_procedures(ctx, children(i), interface_procs, n_procs, tmp, &
                                 violation_count)
        end do
    end subroutine walk_procedures

    subroutine check_procedure(ctx, proc_index, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: proc_index
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        character(len=64), allocatable :: param_names(:)
        logical, allocatable :: has_intent(:)
        integer :: i, n_params
        logical :: missing_any

        call get_procedure_param_names(ctx, proc_index, param_names)
        if (.not. allocated(param_names)) return
        n_params = size(param_names)
        if (n_params <= 0) return

        allocate (has_intent(n_params))
        has_intent = .false.

        call collect_param_intents(ctx, proc_index, proc_index, param_names, &
                                   has_intent)

        missing_any = .false.
        do i = 1, n_params
            if (len_trim(param_names(i)) <= 0) cycle
            if (.not. has_intent(i)) then
                missing_any = .true.
                exit
            end if
        end do
        if (.not. missing_any) return

        call push_diagnostic(tmp, violation_count, create_diagnostic( &
                             code="F008", &
                         message="Missing intent declaration for procedure arguments", &
                             file_path=current_filename, &
                             location=ctx%get_node_location(proc_index), &
                             severity=SEVERITY_WARNING))
    end subroutine check_procedure

    subroutine get_procedure_param_names(ctx, proc_index, names)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: proc_index
        character(len=64), allocatable, intent(out) :: names(:)

        integer, allocatable :: param_indices(:)
        integer :: i
        character(len=:), allocatable :: name

        if (allocated(names)) deallocate (names)
        if (.not. allocated(ctx%arena%entries(proc_index)%node)) then
            allocate (names(0))
            return
        end if

        select type (p => ctx%arena%entries(proc_index)%node)
        type is (subroutine_def_node)
            if (.not. allocated(p%param_indices)) then
                allocate (names(0))
                return
            end if
            param_indices = p%param_indices
        type is (function_def_node)
            if (.not. allocated(p%param_indices)) then
                allocate (names(0))
                return
            end if
            param_indices = p%param_indices
        class default
            allocate (names(0))
            return
        end select

        if (.not. allocated(param_indices)) then
            allocate (names(0))
            return
        end if
        if (size(param_indices) <= 0) then
            allocate (names(0))
            return
        end if

        allocate (names(size(param_indices)))
        names = ""
        do i = 1, size(param_indices)
            name = ""
            if (param_indices(i) > 0) then
                if (allocated(ctx%arena%entries(param_indices(i))%node)) then
                    select type (n => ctx%arena%entries(param_indices(i))%node)
                    type is (identifier_node)
                        if (allocated(n%name)) name = to_lower_ascii(trim(n%name))
                    type is (parameter_declaration_node)
                        if (allocated(n%name)) name = to_lower_ascii(trim(n%name))
                    end select
                end if
            end if
            names(i) = name
        end do
        if (allocated(param_indices)) deallocate (param_indices)
    end subroutine get_procedure_param_names

    recursive subroutine collect_param_intents(ctx, proc_index, node_index, &
                                               param_names, has_intent)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: proc_index
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: param_names(:)
        logical, intent(inout) :: has_intent(:)

        integer, allocatable :: children(:)
        integer :: i

        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        if (node_index /= proc_index) then
            select type (n => ctx%arena%entries(node_index)%node)
            type is (subroutine_def_node)
                return
            type is (function_def_node)
                return
            end select
        end if

        select type (n => ctx%arena%entries(node_index)%node)
        type is (declaration_node)
            call mark_intents_from_declaration(n, param_names, has_intent)
        end select

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) <= 0) cycle
            call collect_param_intents(ctx, proc_index, children(i), param_names, &
                                       has_intent)
        end do
        if (allocated(children)) deallocate (children)
    end subroutine collect_param_intents

    subroutine mark_intents_from_declaration(decl, param_names, has_intent)
        type(declaration_node), intent(in) :: decl
        character(len=*), intent(in) :: param_names(:)
        logical, intent(inout) :: has_intent(:)

        integer :: i, j
        character(len=:), allocatable :: declared

        if (.not. decl%has_intent) return

        if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
            do i = 1, size(decl%var_names)
                declared = to_lower_ascii(trim(decl%var_names(i)))
                do j = 1, min(size(param_names), size(has_intent))
                    if (trim(declared) == trim(param_names(j))) then
                        has_intent(j) = .true.
                    end if
                end do
            end do
        else if (allocated(decl%var_name)) then
            declared = to_lower_ascii(trim(decl%var_name))
            do j = 1, min(size(param_names), size(has_intent))
                if (trim(declared) == trim(param_names(j))) then
                    has_intent(j) = .true.
                end if
            end do
        end if
    end subroutine mark_intents_from_declaration

end module fluff_rule_f008
