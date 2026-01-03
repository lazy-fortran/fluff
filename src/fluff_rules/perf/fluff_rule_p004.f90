module fluff_rule_p004
    use fluff_ast, only: fluff_ast_context_t, NODE_FUNCTION_DEF, NODE_SUBROUTINE_DEF
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fortfront, only: allocate_statement_node, error_stop_node, function_def_node, &
                         print_statement_node, read_statement_node, stop_node, &
                         subroutine_call_node, subroutine_def_node, write_statement_node
    implicit none
    private

    public :: check_p004_pure_elemental

contains

    subroutine check_p004_pure_elemental(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count
        integer :: i

        allocate (tmp(16))
        violation_count = 0

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            if (ctx%get_node_type(i) == NODE_FUNCTION_DEF .or. &
                ctx%get_node_type(i) == NODE_SUBROUTINE_DEF) then
                if (procedure_is_pure_candidate(ctx, i)) then
                    call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                        code="P004", &
                            message="Consider adding pure attribute for optimization", &
                                        file_path="", &
                                        location=ctx%get_node_location(i), &
                                        severity=SEVERITY_INFO))
                end if
            end if
        end do

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_p004_pure_elemental

    logical function procedure_is_pure_candidate(ctx, node_index) result(ok)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index

        ok = .false.
        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (p => ctx%arena%entries(node_index)%node)
        type is (function_def_node)
            if (has_prefix(p%prefix_keywords, "pure")) return
            if (procedure_has_side_effects(ctx, node_index)) return
            ok = .true.
        type is (subroutine_def_node)
            if (has_prefix(p%prefix_keywords, "pure")) return
            if (procedure_has_side_effects(ctx, node_index)) return
            ok = .true.
        end select
    end function procedure_is_pure_candidate

    recursive logical function procedure_has_side_effects(ctx, node_index) result(has)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index

        integer, allocatable :: children(:)
        integer :: i

        has = .false.
        if (node_index <= 0) return

        if (allocated(ctx%arena%entries(node_index)%node)) then
            select type (n => ctx%arena%entries(node_index)%node)
            type is (print_statement_node)
                has = .true.
                return
            type is (write_statement_node)
                has = .true.
                return
            type is (read_statement_node)
                has = .true.
                return
            type is (allocate_statement_node)
                has = .true.
                return
            type is (subroutine_call_node)
                has = .true.
                return
            type is (stop_node)
                has = .true.
                return
            type is (error_stop_node)
                has = .true.
                return
            end select
        end if

        children = ctx%get_children(node_index)
        do i = 1, size(children)
            if (children(i) > 0) then
                if (procedure_has_side_effects(ctx, children(i))) then
                    has = .true.
                    exit
                end if
            end if
        end do
        if (allocated(children)) deallocate (children)
    end function procedure_has_side_effects

    logical function has_prefix(prefix_keywords, key) result(found)
        character(len=16), allocatable, intent(in) :: prefix_keywords(:)
        character(len=*), intent(in) :: key

        integer :: i
        character(len=:), allocatable :: kw

        found = .false.
        if (.not. allocated(prefix_keywords)) return
        do i = 1, size(prefix_keywords)
            kw = to_lower_ascii(trim(prefix_keywords(i)))
            if (kw == to_lower_ascii(trim(key))) then
                found = .true.
                exit
            end if
        end do
    end function has_prefix

end module fluff_rule_p004
