module fluff_format_continuation
    implicit none
    private

    public :: has_leading_ampersand_continuations
    public :: preserve_leading_ampersand_lines
    public :: extract_continuation_regions

    type, public :: continuation_region_t
        integer :: start_line = 0
        integer :: end_line = 0
        integer, allocatable :: indent_widths(:)
    end type continuation_region_t

contains

    function has_leading_ampersand_continuations(source_code) result(has_any)
        character(len=*), intent(in) :: source_code
        logical :: has_any

        integer :: pos, line_start, line_end, len_src
        character(len=:), allocatable :: line
        logical :: prev_has_trailing_amp

        has_any = .false.
        prev_has_trailing_amp = .false.
        len_src = len(source_code)
        if (len_src == 0) return

        pos = 1
        do while (pos <= len_src)
            line_start = pos
            line_end = pos
            do
                if (line_end > len_src) exit
                if (source_code(line_end:line_end) == new_line('a')) exit
                line_end = line_end + 1
            end do

            if (line_end > line_start) then
                line = source_code(line_start:line_end - 1)
            else
                line = ""
            end if

            if (prev_has_trailing_amp .and. starts_with_ampersand(line)) then
                has_any = .true.
                return
            end if

            prev_has_trailing_amp = ends_with_ampersand(line)

            if (line_end <= len_src) then
                pos = line_end + 1
            else
                exit
            end if
        end do

    end function has_leading_ampersand_continuations

    subroutine extract_continuation_regions(source_code, regions)
        character(len=*), intent(in) :: source_code
        type(continuation_region_t), allocatable, intent(out) :: regions(:)

        type(continuation_region_t) :: temp_regions(1000)
        integer :: num_regions, region_line_count
        integer :: pos, line_num, line_start, line_end, len_src
        character(len=:), allocatable :: line
        logical :: prev_has_trailing_amp, in_region
        integer :: temp_indents(100)

        allocate (regions(0))
        num_regions = 0
        len_src = len(source_code)
        if (len_src == 0) return

        pos = 1
        line_num = 0
        prev_has_trailing_amp = .false.
        in_region = .false.
        region_line_count = 0

        do while (pos <= len_src)
            line_num = line_num + 1
            line_start = pos
            line_end = pos

            do
                if (line_end > len_src) exit
                if (source_code(line_end:line_end) == new_line('a')) exit
                line_end = line_end + 1
            end do

            if (line_end > line_start) then
                line = source_code(line_start:line_end - 1)
            else
                line = ""
            end if

            if (prev_has_trailing_amp .and. starts_with_ampersand(line)) then
                if (.not. in_region) then
                    in_region = .true.
                    num_regions = num_regions + 1
                    temp_regions(num_regions)%start_line = line_num - 1
                    region_line_count = 1
                    temp_indents(region_line_count) = count_indent(line)
                else
                    region_line_count = region_line_count + 1
                    temp_indents(region_line_count) = count_indent(line)
                end if
            else if (in_region) then
                temp_regions(num_regions)%end_line = line_num - 1
                allocate (temp_regions(num_regions)%indent_widths(region_line_count))
                temp_regions(num_regions)%indent_widths = &
                    temp_indents(1:region_line_count)
                in_region = .false.
                region_line_count = 0
            end if

            prev_has_trailing_amp = ends_with_ampersand(line)

            if (line_end <= len_src) then
                pos = line_end + 1
            else
                exit
            end if
        end do

        if (in_region) then
            temp_regions(num_regions)%end_line = line_num
            allocate (temp_regions(num_regions)%indent_widths(region_line_count))
            temp_regions(num_regions)%indent_widths = temp_indents(1:region_line_count)
        end if

        if (num_regions > 0) then
            regions = temp_regions(1:num_regions)
        end if

    end subroutine extract_continuation_regions

    subroutine preserve_leading_ampersand_lines(original_code, formatted_code, &
                                                result_code)
        character(len=*), intent(in) :: original_code
        character(len=*), intent(in) :: formatted_code
        character(len=:), allocatable, intent(out) :: result_code

        type(continuation_region_t), allocatable :: orig_regions(:)
        type(continuation_region_t), allocatable :: fmt_regions(:)
        character(len=:), allocatable :: fmt_lines(:)
        character(len=:), allocatable :: orig_lines(:)
        integer :: num_fmt_lines, num_orig_lines
        integer :: r, line_idx, region_line_idx
        integer :: orig_indent, fmt_indent
        character(len=:), allocatable :: trimmed_content

        call extract_continuation_regions(original_code, orig_regions)

        if (size(orig_regions) == 0) then
            result_code = formatted_code
            return
        end if

        call extract_continuation_regions(formatted_code, fmt_regions)
        call split_to_lines(formatted_code, fmt_lines, num_fmt_lines)
        call split_to_lines(original_code, orig_lines, num_orig_lines)

        do r = 1, min(size(orig_regions), size(fmt_regions))
            region_line_idx = 0
            do line_idx = fmt_regions(r)%start_line + 1, fmt_regions(r)%end_line
                if (line_idx > num_fmt_lines) exit
                region_line_idx = region_line_idx + 1

                if (region_line_idx > size(orig_regions(r)%indent_widths)) cycle
                if (region_line_idx > size(fmt_regions(r)%indent_widths)) cycle

                orig_indent = orig_regions(r)%indent_widths(region_line_idx)
                fmt_indent = fmt_regions(r)%indent_widths(region_line_idx)

                if (orig_indent /= fmt_indent .and. &
                    starts_with_ampersand(fmt_lines(line_idx))) then
                    trimmed_content = adjustl(fmt_lines(line_idx))
                    fmt_lines(line_idx) = repeat(' ', orig_indent)//trimmed_content
                end if
            end do
        end do

        result_code = ""
        do line_idx = 1, num_fmt_lines
            if (line_idx > 1) result_code = result_code//new_line('a')
            result_code = result_code//trim(fmt_lines(line_idx))
        end do

    end subroutine preserve_leading_ampersand_lines

    function starts_with_ampersand(line) result(starts)
        character(len=*), intent(in) :: line
        logical :: starts

        character(len=:), allocatable :: trimmed

        starts = .false.
        if (len(line) == 0) return

        trimmed = adjustl(line)
        if (len(trimmed) == 0) return
        starts = trimmed(1:1) == '&'

    end function starts_with_ampersand

    function ends_with_ampersand(line) result(ends)
        character(len=*), intent(in) :: line
        logical :: ends

        integer :: trim_len, pos, comment_start
        logical :: in_string
        character :: quote_char, c

        ends = .false.
        trim_len = len_trim(line)
        if (trim_len == 0) return

        in_string = .false.
        quote_char = ' '
        comment_start = 0

        pos = 1
        do while (pos <= trim_len)
            c = line(pos:pos)
            if (in_string) then
                if (c == quote_char) then
                    if (pos < trim_len .and. line(pos + 1:pos + 1) == quote_char) then
                        pos = pos + 1
                    else
                        in_string = .false.
                    end if
                end if
            else
                if (c == '"' .or. c == "'") then
                    in_string = .true.
                    quote_char = c
                else if (c == '!') then
                    comment_start = pos
                    exit
                end if
            end if
            pos = pos + 1
        end do

        if (comment_start > 0) then
            pos = comment_start - 1
            do while (pos > 0 .and. line(pos:pos) == ' ')
                pos = pos - 1
            end do
        else
            pos = trim_len
        end if

        if (pos > 0) ends = line(pos:pos) == '&'

    end function ends_with_ampersand

    function count_indent(line) result(indent)
        character(len=*), intent(in) :: line
        integer :: indent

        integer :: i

        indent = 0
        do i = 1, len(line)
            if (line(i:i) == ' ') then
                indent = indent + 1
            else if (line(i:i) == achar(9)) then
                indent = indent + 4
            else
                exit
            end if
        end do

    end function count_indent

    subroutine split_to_lines(text, lines, num_lines)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: num_lines

        character(len=2000) :: temp_lines(5000)
        integer :: pos, line_start, line_end, len_text, max_line_len

        num_lines = 0
        max_line_len = 0
        len_text = len(text)
        if (len_text == 0) then
            allocate (character(len=1) :: lines(0))
            return
        end if

        pos = 1
        do while (pos <= len_text .and. num_lines < 5000)
            line_start = pos
            line_end = pos
            do
                if (line_end > len_text) exit
                if (text(line_end:line_end) == new_line('a')) exit
                line_end = line_end + 1
            end do

            num_lines = num_lines + 1
            if (line_end > line_start) then
                temp_lines(num_lines) = text(line_start:min(line_end - 1, &
                                                            line_start + 1999))
                max_line_len = max(max_line_len, line_end - line_start)
            else
                temp_lines(num_lines) = ""
            end if

            if (line_end <= len_text) then
                pos = line_end + 1
            else
                exit
            end if
        end do

        max_line_len = max(max_line_len, 1)
        allocate (character(len=max_line_len) :: lines(num_lines))
        lines = temp_lines(1:num_lines)

    end subroutine split_to_lines

end module fluff_format_continuation
