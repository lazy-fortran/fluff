program test_fortfront_trivia_smoke
    use fluff_ast, only: create_ast_context, fluff_ast_context_t
    use fortfront, only: token_t, trivia_token_t, tokenize_core_with_trivia
    use lexer_token_types, only: TK_NEWLINE, TK_WHITESPACE
    implicit none

    type(token_t), allocatable :: tokens(:)
    type(fluff_ast_context_t) :: ctx
    character(len=:), allocatable :: source
    character(len=:), allocatable :: stored
    character(len=:), allocatable :: error_msg
    logical :: saw_newline
    logical :: saw_whitespace
    integer :: trailing_ws_count
    logical :: found

    source = "program test"//new_line('a')// &
             "    implicit none   "//new_line('a')// &
             "end program test"//new_line('a')

    ctx = create_ast_context()
    call ctx%from_source(source, error_msg)
    if (len(error_msg) > 0) error stop error_msg

    call ctx%get_source_text(stored, found)
    if (.not. found) error stop "Expected ctx to store source text"

    call tokenize_core_with_trivia(stored, tokens)

    call assert_has_trivia(tokens, saw_newline, saw_whitespace)
    if (.not. saw_newline) error stop "Expected lexer trivia to include newline"
    if (.not. saw_whitespace) error stop "Expected lexer trivia to include whitespace"

    trailing_ws_count = count_trailing_whitespace(tokens)
    if (trailing_ws_count <= 0) then
        call dump_trivia(tokens)
        error stop "Expected trailing whitespace trivia before newline"
    end if

contains

    subroutine assert_has_trivia(tokens, saw_newline, saw_whitespace)
        type(token_t), allocatable, intent(in) :: tokens(:)
        logical, intent(out) :: saw_newline
        logical, intent(out) :: saw_whitespace

        integer :: i

        saw_newline = .false.
        saw_whitespace = .false.

        if (.not. allocated(tokens)) return

        do i = 1, size(tokens)
            call scan_trivia(tokens(i)%leading_trivia, saw_newline, saw_whitespace)
            call scan_trivia(tokens(i)%trailing_trivia, saw_newline, saw_whitespace)
        end do
    end subroutine assert_has_trivia

    subroutine scan_trivia(trivia, saw_newline, saw_whitespace)
        type(trivia_token_t), allocatable, intent(in) :: trivia(:)
        logical, intent(inout) :: saw_newline
        logical, intent(inout) :: saw_whitespace

        integer :: i

        if (.not. allocated(trivia)) return
        if (size(trivia) <= 0) return

        do i = 1, size(trivia)
            if (trivia(i)%kind == TK_NEWLINE) saw_newline = .true.
            if (trivia(i)%kind == TK_WHITESPACE) saw_whitespace = .true.
        end do
    end subroutine scan_trivia

    integer function count_trailing_whitespace(tokens) result(count)
        type(token_t), allocatable, intent(in) :: tokens(:)
        integer :: i

        count = 0
        if (.not. allocated(tokens)) return

        do i = 1, size(tokens)
            call count_in_trivia(tokens(i)%leading_trivia, count)
        end do
    end function count_trailing_whitespace

    subroutine count_in_trivia(trivia, count)
        type(trivia_token_t), allocatable, intent(in) :: trivia(:)
        integer, intent(inout) :: count
        integer :: i

        if (.not. allocated(trivia)) return
        if (size(trivia) <= 0) return

        do i = 2, size(trivia)
            if (trivia(i)%kind /= TK_NEWLINE) cycle
            if (trivia(i - 1)%kind /= TK_WHITESPACE) cycle
            if (.not. allocated(trivia(i - 1)%text)) cycle
            if (len(trivia(i - 1)%text) <= 0) cycle
            count = count + 1
        end do
    end subroutine count_in_trivia

    subroutine dump_trivia(tokens)
        type(token_t), allocatable, intent(in) :: tokens(:)
        integer :: i

        if (.not. allocated(tokens)) return

        do i = 1, size(tokens)
            call dump_trivia_list(tokens(i)%leading_trivia)
        end do
    end subroutine dump_trivia

    subroutine dump_trivia_list(trivia)
        type(trivia_token_t), allocatable, intent(in) :: trivia(:)
        integer :: i

        if (.not. allocated(trivia)) return
        if (size(trivia) <= 0) return

        do i = 1, size(trivia)
            if (allocated(trivia(i)%text)) then
                print *, "trivia:", trivia(i)%kind, trivia(i)%line, trivia(i)%column, &
                    len(trivia(i)%text)
            else
                print *, "trivia:", trivia(i)%kind, trivia(i)%line, trivia(i)%column, 0
            end if
        end do
    end subroutine dump_trivia_list

end program test_fortfront_trivia_smoke
