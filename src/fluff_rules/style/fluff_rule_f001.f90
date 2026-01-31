module fluff_rule_f001
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, fix_suggestion_t, text_edit_t, &
                                 create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename
    use fluff_rule_diagnostic_utils, only: push_diagnostic
    use fortfront, only: comment_node, declaration_node, directive_node, &
                         function_def_node, implicit_statement_node, &
                         interface_block_node, module_node, program_node, &
                         subroutine_def_node, use_statement_node
    implicit none
    private

    public :: check_f001_implicit_none

contains

    subroutine check_f001_implicit_none(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        allocate (tmp(0))
        violation_count = 0

        call walk_tree(ctx, ctx%root_index, .false., tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f001_implicit_none

    recursive subroutine walk_tree(ctx, node_index, in_interface, tmp, &
                                   violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        logical, intent(in) :: in_interface
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer, allocatable :: children(:)
        integer :: i
        logical :: child_in_interface

        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        child_in_interface = in_interface
        select type (node => ctx%arena%entries(node_index)%node)
        type is (interface_block_node)
            child_in_interface = .true.
        type is (program_node)
            if (.not. in_interface) then
                call check_scope(ctx, node_index, tmp, violation_count)
            end if
        type is (module_node)
            if (.not. in_interface) then
                call check_scope(ctx, node_index, tmp, violation_count)
            end if
        type is (subroutine_def_node)
            if (.not. in_interface) then
                call check_scope(ctx, node_index, tmp, violation_count)
            end if
        type is (function_def_node)
            if (.not. in_interface) then
                call check_scope(ctx, node_index, tmp, violation_count)
            end if
        end select

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) <= 0) cycle
            call walk_tree(ctx, children(i), child_in_interface, tmp, violation_count)
        end do
        if (allocated(children)) deallocate (children)
    end subroutine walk_tree

    subroutine check_scope(ctx, scope_index, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_index
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        type(diagnostic_t) :: diag
        type(fix_suggestion_t) :: fix
        type(text_edit_t) :: edit
        type(source_range_t) :: location
        type(source_range_t) :: child_location
        logical :: has_implicit_none
        integer, allocatable :: children(:)
        integer :: i
        integer :: insert_line

        has_implicit_none = scope_has_implicit_none(ctx, scope_index, scope_index)
        if (has_implicit_none) return

        if (.not. scope_has_declarations(ctx, scope_index, scope_index)) return

        location = ctx%get_node_location(scope_index)
        diag = create_diagnostic( &
               code="F001", &
               message="Missing implicit none statement", &
               file_path=current_filename, &
               location=location, &
               severity=SEVERITY_WARNING)

        fix%description = "Add implicit none statement"
        fix%is_safe = .true.

        insert_line = location%start%line + 1
        children = ctx%get_children(scope_index)
        do i = 1, size(children)
            if (children(i) <= 0) cycle
            if (.not. allocated(ctx%arena%entries(children(i))%node)) cycle

            select type (n => ctx%arena%entries(children(i))%node)
            type is (use_statement_node)
                child_location = ctx%get_node_location(children(i))
                insert_line = max(insert_line, child_location%end%line + 1)
            type is (comment_node)
                cycle
            type is (directive_node)
                cycle
            class default
                exit
            end select
        end do
        if (allocated(children)) deallocate (children)

        edit%range%start%line = insert_line
        edit%range%start%column = 1
        edit%range%end%line = insert_line
        edit%range%end%column = 1
        edit%new_text = "    implicit none"//new_line('a')

        allocate (fix%edits(1))
        fix%edits(1) = edit
        allocate (diag%fixes(1))
        diag%fixes(1) = fix

        call push_diagnostic(tmp, violation_count, diag)
    end subroutine check_scope

    recursive logical function scope_has_implicit_none(ctx, scope_index, &
                                                       node_index) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_index
        integer, intent(in) :: node_index

        integer, allocatable :: children(:)
        integer :: i

        found = .false.
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        if (node_index /= scope_index) then
            select type (n => ctx%arena%entries(node_index)%node)
            type is (subroutine_def_node)
                return
            type is (function_def_node)
                return
            end select
        end if

        select type (n => ctx%arena%entries(node_index)%node)
        type is (implicit_statement_node)
            found = n%is_none
            return
        end select

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) <= 0) cycle
            if (scope_has_implicit_none(ctx, scope_index, children(i))) then
                found = .true.
                exit
            end if
        end do
        if (allocated(children)) deallocate (children)
    end function scope_has_implicit_none

    recursive logical function scope_has_declarations(ctx, scope_index, &
                                                      node_index) result(found)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_index
        integer, intent(in) :: node_index

        integer, allocatable :: children(:)
        integer :: i

        found = .false.
        if (node_index <= 0) return
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        if (node_index /= scope_index) then
            select type (n => ctx%arena%entries(node_index)%node)
            type is (subroutine_def_node)
                return
            type is (function_def_node)
                return
            end select
        end if

        select type (n => ctx%arena%entries(node_index)%node)
        type is (declaration_node)
            found = .true.
            return
        end select

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) <= 0) cycle
            if (scope_has_declarations(ctx, scope_index, children(i))) then
                found = .true.
                exit
            end if
        end do
        if (allocated(children)) deallocate (children)
    end function scope_has_declarations

end module fluff_rule_f001
