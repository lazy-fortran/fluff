module fluff_token_helpers
    use fluff_core, only: source_range_t
    use fortfront, only: token_t
    use lexer_token_types, only: TK_NEWLINE, TK_WHITESPACE
    implicit none
    private

    public :: token_location
    public :: token_location_point
    public :: first_nontrivia_in_line
    public :: next_nontrivia_same_line
    public :: next_nontrivia

contains

    pure function token_location(tok) result(location)
        type(token_t), intent(in) :: tok
        type(source_range_t) :: location

        integer :: end_col

        location%start%line = tok%line
        location%start%column = tok%column
        location%end%line = tok%line
        end_col = tok%column
        if (allocated(tok%text)) end_col = end_col + len(tok%text) - 1
        location%end%column = end_col
    end function token_location

    pure function token_location_point(tok) result(location)
        type(token_t), intent(in) :: tok
        type(source_range_t) :: location

        location%start%line = tok%line
        location%start%column = tok%column
        location%end%line = tok%line
        location%end%column = tok%column
    end function token_location_point

    integer function first_nontrivia_in_line(tokens, start_idx) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx

        integer :: line
        integer :: i

        idx = 0
        line = tokens(start_idx)%line
        do i = start_idx, size(tokens)
            if (tokens(i)%line /= line) exit
            if (tokens(i)%kind == TK_NEWLINE) cycle
            if (tokens(i)%kind == TK_WHITESPACE) cycle
            idx = i
            return
        end do
    end function first_nontrivia_in_line

    integer function next_nontrivia_same_line(tokens, start_idx) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx

        integer :: line
        integer :: i

        idx = 0
        if (start_idx <= 0) return
        if (start_idx > size(tokens)) return
        line = tokens(start_idx)%line

        do i = start_idx, size(tokens)
            if (tokens(i)%line /= line) exit
            if (tokens(i)%kind == TK_NEWLINE) cycle
            if (tokens(i)%kind == TK_WHITESPACE) cycle
            idx = i
            return
        end do
    end function next_nontrivia_same_line

    integer function next_nontrivia(tokens, start_idx) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx

        integer :: i

        idx = 0
        if (start_idx <= 0) return
        if (start_idx > size(tokens)) return

        do i = start_idx, size(tokens)
            if (tokens(i)%kind == TK_NEWLINE) cycle
            if (tokens(i)%kind == TK_WHITESPACE) cycle
            idx = i
            return
        end do
    end function next_nontrivia

end module fluff_token_helpers
