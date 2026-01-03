module fluff_rule_f008
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: declaration_node, function_def_node, identifier_node, &
                         interface_block_node, parameter_declaration_node, &
                         subroutine_def_node
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
        logical, allocatable :: is_interface_proc(:)
        integer :: i

        allocate (tmp(0))
        violation_count = 0

        call mark_interface_procedures(ctx, is_interface_proc)
        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            if (allocated(is_interface_proc)) then
                if (i <= size(is_interface_proc)) then
                    if (is_interface_proc(i)) cycle
                end if
            end if
            select type (n => ctx%arena%entries(i)%node)
            type is (subroutine_def_node)
                call check_procedure(ctx, i, tmp, violation_count)
            type is (function_def_node)
                call check_procedure(ctx, i, tmp, violation_count)
            end select
        end do

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f008_missing_intent

    subroutine mark_interface_procedures(ctx, is_interface_proc)
        type(fluff_ast_context_t), intent(in) :: ctx
        logical, allocatable, intent(out) :: is_interface_proc(:)

        integer :: i, j
        integer :: proc_idx

        allocate (is_interface_proc(ctx%arena%size))
        is_interface_proc = .false.

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (n => ctx%arena%entries(i)%node)
            type is (interface_block_node)
                if (.not. allocated(n%procedure_indices)) cycle
                do j = 1, size(n%procedure_indices)
                    proc_idx = n%procedure_indices(j)
                    if (proc_idx <= 0) cycle
                    if (proc_idx > size(is_interface_proc)) cycle
                    is_interface_proc(proc_idx) = .true.
                end do
            end select
        end do
    end subroutine mark_interface_procedures

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
