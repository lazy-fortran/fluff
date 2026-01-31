module fluff_text_helpers
    use fluff_core, only: source_range_t
    use fortfront, only: token_t
    use lexer_token_types, only: TK_NEWLINE, TK_WHITESPACE
    implicit none
    private

    public :: starts_with
    public :: is_comment_only_line
    public :: int_to_str
    public :: token_location
    public :: first_nontrivia_in_line
    public :: next_nontrivia_same_line
    public :: is_lowercase_letter
    public :: is_uppercase_letter
    public :: has_uppercase
    public :: has_lowercase

contains

    pure logical function starts_with(s, prefix) result(ok)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: prefix

        integer :: n

        n = len(prefix)
        ok = .false.
        if (len(s) < n) return
        ok = s(1:n) == prefix
    end function starts_with

    logical function is_comment_only_line(line_text) result(is_comment)
        character(len=*), intent(in) :: line_text

        integer :: i
        character(len=1) :: ch

        is_comment = .false.
        do i = 1, len(line_text)
            ch = line_text(i:i)
            if (ch == " " .or. ch == achar(9) .or. ch == achar(13)) cycle
            is_comment = (ch == "!")
            return
        end do
    end function is_comment_only_line

    pure function int_to_str(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str

        write (str, "(I0)") i
    end function int_to_str

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

    pure logical function is_lowercase_letter(ch) result(ok)
        character(len=1), intent(in) :: ch

        ok = (ch >= "a" .and. ch <= "z")
    end function is_lowercase_letter

    pure logical function is_uppercase_letter(ch) result(ok)
        character(len=1), intent(in) :: ch

        ok = (ch >= "A" .and. ch <= "Z")
    end function is_uppercase_letter

    pure logical function has_uppercase(name) result(has_upper)
        character(len=*), intent(in) :: name

        integer :: i

        has_upper = .false.
        do i = 1, len(name)
            if (is_uppercase_letter(name(i:i))) then
                has_upper = .true.
                return
            end if
        end do
    end function has_uppercase

    pure logical function has_lowercase(name) result(has_lower)
        character(len=*), intent(in) :: name

        integer :: i

        has_lower = .false.
        do i = 1, len(name)
            if (is_lowercase_letter(name(i:i))) then
                has_lower = .true.
                return
            end if
        end do
    end function has_lowercase

end module fluff_text_helpers
