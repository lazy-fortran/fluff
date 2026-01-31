module fluff_rule_f005
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename
    use fluff_rule_trivia_utils, only: location_buffer_t, location_buffer_init, &
                                       location_buffer_push, location_buffer_finish, &
                                       text_has_space_and_tab
    use fortfront, only: token_t, tokenize_core_with_trivia, trivia_token_t
    use lexer_token_types, only: TK_WHITESPACE
    implicit none
    private

    public :: check_f005_mixed_tabs_spaces

    integer, parameter :: INDENT_NONE = 0
    integer, parameter :: INDENT_SPACES = 1
    integer, parameter :: INDENT_TABS = 2

contains

    subroutine check_f005_mixed_tabs_spaces(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        character(len=:), allocatable :: source_text
        logical :: found
        type(token_t), allocatable :: tokens(:)
        type(source_range_t), allocatable :: locations(:)
        integer :: i

        call ctx%get_source_text(source_text, found)
        if (.not. found) then
            allocate (violations(0))
            return
        end if

        call tokenize_core_with_trivia(source_text, tokens)

        call collect_mixed_indentation(tokens, locations)
        if (size(locations) <= 0) then
            allocate (violations(0))
            return
        end if

        allocate (violations(size(locations)))
        do i = 1, size(locations)
            violations(i) = create_diagnostic( &
                            code="F005", &
                            message="Mixed tabs and spaces", &
                            file_path=current_filename, &
                            location=locations(i), &
                            severity=SEVERITY_WARNING)
        end do
    end subroutine check_f005_mixed_tabs_spaces

    subroutine collect_mixed_indentation(tokens, locations)
        type(token_t), allocatable, intent(in) :: tokens(:)
        type(source_range_t), allocatable, intent(out) :: locations(:)

        type(location_buffer_t) :: buffer
        integer :: i
        integer :: file_indent_style

        call location_buffer_init(buffer)
        if (.not. allocated(tokens)) then
            call location_buffer_finish(buffer, locations)
            return
        end if
        if (size(tokens) <= 0) then
            call location_buffer_finish(buffer, locations)
            return
        end if

        file_indent_style = INDENT_NONE
        do i = 1, size(tokens)
            call collect_from_trivia(tokens(i)%leading_trivia, buffer, &
                                     file_indent_style)
        end do
        call location_buffer_finish(buffer, locations)
    end subroutine collect_mixed_indentation

    subroutine collect_from_trivia(trivia, buffer, file_indent_style)
        type(trivia_token_t), allocatable, intent(in) :: trivia(:)
        type(location_buffer_t), intent(inout) :: buffer
        integer, intent(inout) :: file_indent_style

        integer :: i
        integer :: end_col
        integer :: this_style
        type(source_range_t) :: location
        logical :: has_tab, has_space

        if (.not. allocated(trivia)) return
        if (size(trivia) <= 0) return

        do i = 1, size(trivia)
            if (trivia(i)%kind /= TK_WHITESPACE) cycle
            if (trivia(i)%column /= 1) cycle
            if (.not. allocated(trivia(i)%text)) cycle
            if (len(trivia(i)%text) == 0) cycle

            has_tab = index(trivia(i)%text, achar(9)) > 0
            has_space = index(trivia(i)%text, " ") > 0

            if (has_tab .and. has_space) then
                end_col = trivia(i)%column + len(trivia(i)%text) - 1
                location%start%line = trivia(i)%line
                location%start%column = trivia(i)%column
                location%end%line = trivia(i)%line
                location%end%column = end_col
                call location_buffer_push(buffer, location)
                cycle
            end if

            if (has_tab) then
                this_style = INDENT_TABS
            else if (has_space) then
                this_style = INDENT_SPACES
            else
                cycle
            end if

            if (file_indent_style == INDENT_NONE) then
                file_indent_style = this_style
            else if (file_indent_style /= this_style) then
                end_col = trivia(i)%column + len(trivia(i)%text) - 1
                location%start%line = trivia(i)%line
                location%start%column = trivia(i)%column
                location%end%line = trivia(i)%line
                location%end%column = end_col
                call location_buffer_push(buffer, location)
            end if
        end do
    end subroutine collect_from_trivia

end module fluff_rule_f005
