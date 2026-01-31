module fluff_formatter_validation
    use fortfront, only: lex_source, parse_tokens, create_ast_arena, emit_fortran, &
                         token_t, ast_arena_t
    use lexer_token_types, only: TK_NEWLINE, TK_COMMENT
    implicit none
    private

    public :: validate_format_with_emit
    public :: compare_semantics_with_emit
    public :: analyze_format_diff_with_tokens

contains

    subroutine validate_format_with_emit(original_code, formatted_code, is_valid)
        character(len=*), intent(in) :: original_code
        character(len=*), intent(in) :: formatted_code
        logical, intent(out) :: is_valid
        character(len=:), allocatable :: original_normalized
        character(len=:), allocatable :: formatted_normalized

        call normalize_source_with_emit(original_code, original_normalized)
        call normalize_source_with_emit(formatted_code, formatted_normalized)
        is_valid = (original_normalized == formatted_normalized)
    end subroutine validate_format_with_emit

    subroutine compare_semantics_with_emit(code1, code2, are_equivalent)
        character(len=*), intent(in) :: code1
        character(len=*), intent(in) :: code2
        logical, intent(out) :: are_equivalent
        character(len=:), allocatable :: normalized1
        character(len=:), allocatable :: normalized2

        call normalize_source_with_emit(code1, normalized1)
        call normalize_source_with_emit(code2, normalized2)
        are_equivalent = (normalized1 == normalized2)
    end subroutine compare_semantics_with_emit

    subroutine analyze_format_diff_with_tokens(original, formatted, diff_type)
        character(len=*), intent(in) :: original
        character(len=*), intent(in) :: formatted
        character(len=:), allocatable, intent(out) :: diff_type
        type(token_t), allocatable :: tokens_a(:)
        type(token_t), allocatable :: tokens_b(:)
        character(len=:), allocatable :: error_msg
        logical :: indentation_diff
        logical :: spacing_diff
        logical :: structure_diff

        call lex_source(original, tokens_a, error_msg)
        if (error_msg /= "") then
            diff_type = "error"
            return
        end if

        call lex_source(formatted, tokens_b, error_msg)
        if (error_msg /= "") then
            diff_type = "error"
            return
        end if

        if (.not. tokens_match(tokens_a, tokens_b)) then
            diff_type = "structure"
            return
        end if

        call compare_token_positions(tokens_a, tokens_b, indentation_diff, &
                                     spacing_diff, structure_diff)
        if (structure_diff) then
            diff_type = "structure"
        else if (spacing_diff) then
            diff_type = "whitespace"
        else if (indentation_diff) then
            diff_type = "indentation"
        else
            diff_type = "none"
        end if
    end subroutine analyze_format_diff_with_tokens

    subroutine normalize_source_with_emit(source_code, normalized_code)
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: normalized_code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg
        integer :: prog_index

        call lex_source(source_code, tokens, error_msg)
        if (error_msg /= "") then
            normalized_code = source_code
            return
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            normalized_code = source_code
            return
        end if

        call emit_fortran(arena, prog_index, normalized_code)
    end subroutine normalize_source_with_emit

    function tokens_match(tokens_a, tokens_b) result(match)
        type(token_t), intent(in) :: tokens_a(:)
        type(token_t), intent(in) :: tokens_b(:)
        logical :: match
        integer :: i

        match = .false.
        if (size(tokens_a) /= size(tokens_b)) return

        do i = 1, size(tokens_a)
            if (tokens_a(i)%kind /= tokens_b(i)%kind) return
            if (tokens_a(i)%text /= tokens_b(i)%text) return
        end do

        match = .true.
    end function tokens_match

    subroutine compare_token_positions(tokens_a, tokens_b, indentation_diff, &
                                       spacing_diff, structure_diff)
        type(token_t), intent(in) :: tokens_a(:)
        type(token_t), intent(in) :: tokens_b(:)
        logical, intent(out) :: indentation_diff
        logical, intent(out) :: spacing_diff
        logical, intent(out) :: structure_diff
        integer :: i
        integer :: prev_line
        integer :: line_offset
        logical :: has_offset

        indentation_diff = .false.
        spacing_diff = .false.
        structure_diff = .false.
        prev_line = -1
        line_offset = 0
        has_offset = .false.

        do i = 1, size(tokens_a)
            if (tokens_a(i)%kind == TK_NEWLINE) cycle
            if (tokens_a(i)%kind == TK_COMMENT) cycle

            if (tokens_a(i)%line /= tokens_b(i)%line) then
                structure_diff = .true.
                return
            end if

            if (tokens_a(i)%line /= prev_line) then
                line_offset = tokens_b(i)%column - tokens_a(i)%column
                has_offset = .true.
                if (line_offset /= 0) indentation_diff = .true.
            else
                if (has_offset) then
                    if (tokens_b(i)%column - tokens_a(i)%column /= line_offset) then
                        spacing_diff = .true.
                    end if
                else if (tokens_a(i)%column /= tokens_b(i)%column) then
                    spacing_diff = .true.
                end if
            end if

            prev_line = tokens_a(i)%line
        end do
    end subroutine compare_token_positions

end module fluff_formatter_validation
