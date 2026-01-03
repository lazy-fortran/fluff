module fluff_rule_f001
    use fluff_ast, only: fluff_ast_context_t, NODE_DECLARATION, NODE_FUNCTION_DEF, &
                         NODE_MODULE, NODE_PROGRAM, NODE_SUBROUTINE_DEF, NODE_UNKNOWN
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, fix_suggestion_t, text_edit_t, &
                                 create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_source_text
    implicit none
    private

    public :: check_f001_implicit_none

contains

    subroutine check_f001_implicit_none(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        call check_f001_implicit_none_ast_based(ctx, node_index, violations)
    end subroutine check_f001_implicit_none

    subroutine check_f001_implicit_none_ast_based(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count
        logical :: found_implicit_none
        integer :: node_type
        type(fix_suggestion_t) :: fix
        type(text_edit_t) :: edit
        type(source_range_t) :: location

        allocate (temp_violations(10))
        violation_count = 0

        node_type = ctx%get_node_type(node_index)

        if (node_type == NODE_PROGRAM .or. node_type == NODE_MODULE .or. &
            node_type == NODE_FUNCTION_DEF .or. node_type == NODE_SUBROUTINE_DEF .or. &
            node_type == NODE_DECLARATION .or. node_type == NODE_UNKNOWN .or. &
            node_index == 1) then

            if (allocated(current_source_text)) then
                found_implicit_none = index(current_source_text, "implicit") > 0 .and. &
                                      index(current_source_text, "none") > 0
                if (.not. found_implicit_none) then
                    location = ctx%get_node_location(node_index)
                    violation_count = 1

                    temp_violations(violation_count) = create_diagnostic( &
                                                       code="F001", &
                                                       message= &
                                                       "Missing implicit none "// &
                                                       "statement", &
                                                       file_path="", &
                                                       location=location, &
                                                       severity=SEVERITY_WARNING &
                                                       )

                    fix%description = "Add implicit none statement"
                    fix%is_safe = .true.

                    edit%range%start%line = location%start%line + 1
                    edit%range%start%column = 1
                    edit%range%end%line = location%start%line + 1
                    edit%range%end%column = 1
                    edit%new_text = "    implicit none"//new_line('a')

                    allocate (fix%edits(1))
                    fix%edits(1) = edit

                    allocate (temp_violations(violation_count)%fixes(1))
                    temp_violations(violation_count)%fixes(1) = fix
                end if
            end if
        end if

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = temp_violations(1:violation_count)
        end if
    end subroutine check_f001_implicit_none_ast_based

end module fluff_rule_f001
