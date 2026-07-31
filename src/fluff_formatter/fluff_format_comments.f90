module fluff_format_comments
    !! Re-attachment of inline comments to the statements they document.
    !!
    !! The code generator emits statements from the AST, and a trailing comment
    !! that the parser could not hang off a node is either dropped or emitted on
    !! a line of its own. Either way the comment stops documenting its
    !! statement. This pass compares the original source with the emitted text
    !! and puts every trailing comment back on the emitted line that carries the
    !! same statement, leaving standalone comments standalone.
    implicit none
    private

    public :: preserve_inline_comments

    type :: inline_comment_t
        character(len=:), allocatable :: key
        character(len=:), allocatable :: comment
        logical :: attached = .false.
    end type inline_comment_t

    type :: text_line_t
        character(len=:), allocatable :: text
    end type text_line_t

contains

    subroutine preserve_inline_comments(source_code, formatted_code, result_code)
        !! Copy trailing comments from source_code onto the matching statement
        !! lines of formatted_code.
        character(len=*), intent(in) :: source_code
        character(len=*), intent(in) :: formatted_code
        character(len=:), allocatable, intent(out) :: result_code

        type(inline_comment_t), allocatable :: pending(:)
        type(text_line_t), allocatable :: source_lines(:)
        type(text_line_t), allocatable :: lines(:)
        character(len=:), allocatable :: code, comment
        integer :: i, next_pending, match

        call split_lines(formatted_code, lines)
        if (.not. allocated(lines)) then
            result_code = formatted_code
            return
        end if

        call split_lines(source_code, source_lines)
        call collect_inline_comments(source_lines, pending)
        if (size(pending) == 0) then
            result_code = formatted_code
            return
        end if

        next_pending = 1
        do i = 1, size(lines)
            call split_code_comment(lines(i)%text, code, comment)
            if (len(comment) > 0) cycle
            if (len_trim(code) == 0) cycle
            if (ends_with_ampersand(code)) cycle
            match = find_match(pending, next_pending, normalize_code(code))
            if (match == 0) cycle
            lines(i)%text = trim(lines(i)%text)//' '//pending(match)%comment
            pending(match)%attached = .true.
            next_pending = match + 1
        end do

        call drop_orphan_comments(source_lines, pending, lines)
        call join_lines(lines, ends_with_newline(formatted_code), result_code)

    end subroutine preserve_inline_comments

    subroutine collect_inline_comments(source_lines, pending)
        !! Gather every source line that carries both code and a comment.
        type(text_line_t), intent(in) :: source_lines(:)
        type(inline_comment_t), allocatable, intent(out) :: pending(:)

        type(inline_comment_t), allocatable :: buffer(:)
        character(len=:), allocatable :: code, comment
        integer :: i, count

        allocate (buffer(size(source_lines)))
        count = 0

        do i = 1, size(source_lines)
            call split_code_comment(source_lines(i)%text, code, comment)
            if (len(comment) == 0) cycle
            if (len_trim(code) == 0) cycle
            ! A comment after a continuation marker belongs to a physical line
            ! the emitter folds away, so there is no statement to put it on.
            if (ends_with_ampersand(code)) cycle
            count = count + 1
            buffer(count)%key = normalize_code(code)
            buffer(count)%comment = comment
            buffer(count)%attached = .false.
        end do

        allocate (pending(count))
        do i = 1, count
            pending(i) = buffer(i)
        end do

    end subroutine collect_inline_comments

    integer function find_match(pending, from_index, key) result(match)
        !! First unattached pending comment at or after from_index whose
        !! statement matches key. Searching forward only keeps the emitted order
        !! of repeated statements intact.
        type(inline_comment_t), intent(in) :: pending(:)
        integer, intent(in) :: from_index
        character(len=*), intent(in) :: key

        integer :: i

        match = 0
        if (len(key) == 0) return

        do i = max(1, from_index), size(pending)
            if (pending(i)%attached) cycle
            if (pending(i)%key /= key) cycle
            match = i
            return
        end do

    end function find_match

    subroutine drop_orphan_comments(source_lines, pending, lines)
        !! Remove emitted standalone comment lines that only exist because the
        !! emitter detached a comment we have now put back on its statement.
        !! Comments that were standalone in the source are kept.
        type(text_line_t), intent(in) :: source_lines(:)
        type(inline_comment_t), intent(in) :: pending(:)
        type(text_line_t), allocatable, intent(inout) :: lines(:)

        type(text_line_t), allocatable :: kept(:)
        character(len=:), allocatable :: code, comment
        integer :: i, n_kept, allowed

        allocate (kept(size(lines)))
        n_kept = 0

        do i = 1, size(lines)
            call split_code_comment(lines(i)%text, code, comment)
            if (len(comment) > 0 .and. len_trim(code) == 0) then
                if (was_attached(pending, comment)) then
                    allowed = count_standalone(source_lines, comment) - &
                        count_standalone_kept(kept, n_kept, comment)
                    if (allowed <= 0) cycle
                end if
            end if
            n_kept = n_kept + 1
            kept(n_kept)%text = lines(i)%text
        end do

        deallocate (lines)
        allocate (lines(n_kept))
        do i = 1, n_kept
            lines(i) = kept(i)
        end do

    end subroutine drop_orphan_comments

    logical function was_attached(pending, comment) result(found)
        type(inline_comment_t), intent(in) :: pending(:)
        character(len=*), intent(in) :: comment

        integer :: i

        found = .false.
        do i = 1, size(pending)
            if (.not. pending(i)%attached) cycle
            if (pending(i)%comment /= comment) cycle
            found = .true.
            return
        end do

    end function was_attached

    integer function count_standalone(source_lines, comment) result(count)
        type(text_line_t), intent(in) :: source_lines(:)
        character(len=*), intent(in) :: comment

        character(len=:), allocatable :: code, line_comment
        integer :: i

        count = 0
        do i = 1, size(source_lines)
            call split_code_comment(source_lines(i)%text, code, line_comment)
            if (len(line_comment) == 0) cycle
            if (len_trim(code) /= 0) cycle
            if (line_comment /= comment) cycle
            count = count + 1
        end do

    end function count_standalone

    integer function count_standalone_kept(kept, n_kept, comment) result(count)
        type(text_line_t), intent(in) :: kept(:)
        integer, intent(in) :: n_kept
        character(len=*), intent(in) :: comment

        character(len=:), allocatable :: code, line_comment
        integer :: i

        count = 0
        do i = 1, n_kept
            call split_code_comment(kept(i)%text, code, line_comment)
            if (len(line_comment) == 0) cycle
            if (len_trim(code) /= 0) cycle
            if (line_comment /= comment) cycle
            count = count + 1
        end do

    end function count_standalone_kept

    subroutine split_code_comment(line, code, comment)
        !! Split a line at its comment marker, ignoring bangs inside character
        !! literals. comment keeps the bang and is trimmed of trailing blanks.
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: code
        character(len=:), allocatable, intent(out) :: comment

        character(len=1) :: ch, quote
        integer :: i
        logical :: in_string

        in_string = .false.
        quote = ' '

        do i = 1, len(line)
            ch = line(i:i)
            if (in_string) then
                if (ch == quote) in_string = .false.
                cycle
            end if
            if (ch == '"' .or. ch == "'") then
                in_string = .true.
                quote = ch
                cycle
            end if
            if (ch /= '!') cycle
            code = line(1:i - 1)
            comment = trim(line(i:))
            return
        end do

        code = line
        comment = ""

    end subroutine split_code_comment

    function normalize_code(code) result(key)
        !! Case-folded statement text with runs of blanks collapsed, so that
        !! spacing and case changes made by the emitter still match.
        character(len=*), intent(in) :: code
        character(len=:), allocatable :: key

        character(len=1) :: ch
        integer :: i
        logical :: pending_space

        key = ""
        pending_space = .false.

        do i = 1, len_trim(code)
            ch = code(i:i)
            if (ch == ' ' .or. ch == achar(9)) then
                if (len(key) > 0) pending_space = .true.
                cycle
            end if
            if (pending_space) then
                key = key//' '
                pending_space = .false.
            end if
            if (ch >= 'A' .and. ch <= 'Z') ch = achar(iachar(ch) + 32)
            key = key//ch
        end do

    end function normalize_code

    logical function ends_with_ampersand(code) result(has_amp)
        character(len=*), intent(in) :: code

        has_amp = .false.
        if (len_trim(code) == 0) return
        has_amp = code(len_trim(code):len_trim(code)) == '&'

    end function ends_with_ampersand

    logical function ends_with_newline(text) result(has_newline)
        character(len=*), intent(in) :: text

        has_newline = .false.
        if (len(text) == 0) return
        has_newline = text(len(text):len(text)) == new_line('a')

    end function ends_with_newline

    subroutine split_lines(text, lines)
        character(len=*), intent(in) :: text
        type(text_line_t), allocatable, intent(out) :: lines(:)

        integer :: i, count, start_pos, limit

        limit = len(text)
        if (limit > 0) then
            if (text(limit:limit) == new_line('a')) limit = limit - 1
        end if

        count = 0
        start_pos = 1
        do i = 1, limit
            if (text(i:i) /= new_line('a')) cycle
            count = count + 1
        end do
        count = count + 1

        allocate (lines(count))

        count = 0
        start_pos = 1
        do i = 1, limit
            if (text(i:i) /= new_line('a')) cycle
            count = count + 1
            lines(count)%text = text(start_pos:i - 1)
            start_pos = i + 1
        end do
        count = count + 1
        if (start_pos <= limit) then
            lines(count)%text = text(start_pos:limit)
        else
            lines(count)%text = ""
        end if

    end subroutine split_lines

    subroutine join_lines(lines, trailing_newline, text)
        type(text_line_t), intent(in) :: lines(:)
        logical, intent(in) :: trailing_newline
        character(len=:), allocatable, intent(out) :: text

        integer :: i

        text = ""
        do i = 1, size(lines)
            if (i > 1) text = text//new_line('a')
            text = text//lines(i)%text
        end do
        if (trailing_newline) text = text//new_line('a')

    end subroutine join_lines

end module fluff_format_comments
