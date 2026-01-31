module fluff_rule_f009
    use fluff_ast, only: fluff_ast_context_t, NODE_ASSIGNMENT, NODE_DECLARATION
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_core, only: source_range_t
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: assignment_node, call_or_subscript_node, declaration_node, &
                         identifier_node
    implicit none
    private

    public :: check_f009_inconsistent_intent_impl

    type :: intent_var_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: intent
        type(source_range_t) :: decl_location
        logical :: was_assigned = .false.
    end type intent_var_t

contains

    subroutine check_f009_inconsistent_intent_impl(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(intent_var_t), allocatable :: intents(:)
        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count
        integer :: i

        allocate (intents(0))
        allocate (tmp(32))
        violation_count = 0

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (n => ctx%arena%entries(i)%node)
            type is (declaration_node)
                call add_declaration_intents(ctx, i, intents)
            end select
        end do

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (n => ctx%arena%entries(i)%node)
            type is (assignment_node)
                call handle_assignment_node(ctx, i, intents, tmp, violation_count)
            end select
        end do

        call add_unassigned_out(intents, tmp, violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = tmp(1:violation_count)
        end if
    end subroutine check_f009_inconsistent_intent_impl

    subroutine add_declaration_intents(ctx, node_index, intents)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(intent_var_t), allocatable, intent(inout) :: intents(:)

        integer :: i
        character(len=:), allocatable :: name
        character(len=:), allocatable :: intent_lc

        if (.not. allocated(ctx%arena%entries(node_index)%node)) return

        select type (node => ctx%arena%entries(node_index)%node)
        type is (declaration_node)
            if (.not. node%has_intent) return
            intent_lc = to_lower_ascii(trim(node%intent))

            if (node%is_multi_declaration .and. allocated(node%var_names)) then
                do i = 1, size(node%var_names)
                    name = to_lower_ascii(trim(node%var_names(i)))
                    call upsert_intent(intents, name, intent_lc, &
                                       ctx%get_node_location(node_index))
                end do
            else if (allocated(node%var_name)) then
                name = to_lower_ascii(trim(node%var_name))
                call upsert_intent(intents, name, intent_lc, &
                                   ctx%get_node_location(node_index))
            end if
        end select
    end subroutine add_declaration_intents

    subroutine handle_assignment_node(ctx, node_index, intents, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(intent_var_t), allocatable, intent(inout) :: intents(:)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: target_index
        character(len=:), allocatable :: target

        select type (a => ctx%arena%entries(node_index)%node)
        type is (assignment_node)
            target_index = a%target_index
        class default
            target_index = 0
        end select

        if (target_index <= 0) then
            return
        end if

        call get_lhs_base_name(ctx, target_index, target)
        if (.not. allocated(target)) return

        target = to_lower_ascii(trim(target))
        call handle_assignment_to_target(target, &
                                         ctx%get_node_location(node_index), intents, &
                                         tmp, violation_count)
    end subroutine handle_assignment_node

    subroutine get_lhs_base_name(ctx, lhs_index, name)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: lhs_index
        character(len=:), allocatable, intent(out) :: name

        name = ""
        if (lhs_index <= 0) return
        if (.not. allocated(ctx%arena%entries(lhs_index)%node)) return

        select type (lhs => ctx%arena%entries(lhs_index)%node)
        type is (identifier_node)
            if (allocated(lhs%name)) name = lhs%name
        type is (call_or_subscript_node)
            if (allocated(lhs%name)) name = lhs%name
        end select
    end subroutine get_lhs_base_name

    subroutine handle_assignment_to_target(target, location, intents, tmp, &
                                           violation_count)
        character(len=*), intent(in) :: target
        type(source_range_t), intent(in) :: location
        type(intent_var_t), allocatable, intent(inout) :: intents(:)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: i

        do i = 1, size(intents)
            if (intents(i)%name == target) then
                intents(i)%was_assigned = .true.
                if (intents(i)%intent == "in") then
                    call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                        code="F009", &
                                        message= &
                                        "Do not assign to intent(in) dummy argument", &
                                        file_path=current_filename, &
                                        location=location, &
                                        severity=SEVERITY_WARNING &
                                        ))
                end if
                exit
            end if
        end do
    end subroutine handle_assignment_to_target

    subroutine add_unassigned_out(intents, tmp, violation_count)
        type(intent_var_t), allocatable, intent(in) :: intents(:)
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        integer :: i

        do i = 1, size(intents)
            if (intents(i)%intent == "out" .and. .not. intents(i)%was_assigned) then
                call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                    code="F009", &
                                    message= &
                                    "intent(out) dummy argument is never assigned", &
                                    file_path=current_filename, &
                                    location=intents(i)%decl_location, &
                                    severity=SEVERITY_WARNING &
                                    ))
            end if
        end do
    end subroutine add_unassigned_out

    subroutine upsert_intent(intents, name, intent, decl_location)
        type(intent_var_t), allocatable, intent(inout) :: intents(:)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: intent
        type(source_range_t), intent(in) :: decl_location

        integer :: i

        do i = 1, size(intents)
            if (intents(i)%name == name) then
                intents(i)%intent = intent
                intents(i)%decl_location = decl_location
                return
            end if
        end do

        intents = [intents, intent_var_t(name=name, intent=intent, &
                                         decl_location=decl_location)]
    end subroutine upsert_intent

end module fluff_rule_f009
