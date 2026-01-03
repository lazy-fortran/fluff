module fluff_rule_f010
    use fluff_ast, only: fluff_ast_context_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_diagnostic_utils, only: push_diagnostic, to_lower_ascii
    use fluff_rule_file_context, only: current_filename
    use fortfront, only: comment_node, goto_node
    implicit none
    private

    public :: check_f010_obsolete_features

contains

    subroutine check_f010_obsolete_features(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count
        integer :: i

        allocate (tmp(0))
        violation_count = 0

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (n => ctx%arena%entries(i)%node)
            type is (goto_node)
                call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                     code="F010", &
                                     message="Obsolete feature: GOTO", &
                                     file_path=current_filename, &
                                     location=ctx%get_node_location(i), &
                                     severity=SEVERITY_WARNING))
            type is (comment_node)
                call check_legacy_comment(ctx, i, n%text, tmp, violation_count)
            end select
        end do

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f010_obsolete_features

    subroutine check_legacy_comment(ctx, node_index, text, tmp, violation_count)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(in) :: text
        type(diagnostic_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: violation_count

        character(len=:), allocatable :: trimmed
        character(len=:), allocatable :: lowered

        if (.not. allocated(text)) return
        trimmed = adjustl(text)
        if (len_trim(trimmed) <= 0) return
        if (trimmed(1:1) == "!") return

        lowered = to_lower_ascii(trimmed)
        if (starts_with(lowered, "common")) then
            call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                 code="F010", &
                                 message="Obsolete feature: COMMON", &
                                 file_path=current_filename, &
                                 location=ctx%get_node_location(node_index), &
                                 severity=SEVERITY_WARNING))
        else if (starts_with(lowered, "equivalence")) then
            call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                 code="F010", &
                                 message="Obsolete feature: EQUIVALENCE", &
                                 file_path=current_filename, &
                                 location=ctx%get_node_location(node_index), &
                                 severity=SEVERITY_WARNING))
        end if
    end subroutine check_legacy_comment

    pure logical function starts_with(s, prefix) result(ok)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: prefix

        integer :: n

        n = len(prefix)
        ok = .false.
        if (len(s) < n) return
        ok = s(1:n) == prefix
    end function starts_with

end module fluff_rule_f010
