module fluff_format_indent
    ! Rescaling of the indentation width of already generated code.
    !
    ! fortfront's code generator honours set_indent_config only for the
    ! statement nesting it indents through get_indent(). The emitters for a
    ! program or module body build their prefix from a literal four spaces, so
    ! asking fortfront for a width other than four yields a mixture: top level
    ! declarations at four columns and nested statements at the requested
    ! width. The formatter therefore emits at the one width fortfront is
    ! self-consistent about and converts the result here.
    implicit none
    private

    ! The indentation width fortfront emits one nesting level as.
    integer, parameter, public :: emitted_indent_width = 4

    public :: rescale_indentation

contains

    ! Reinterpret every leading run of blanks in `code` as a number of nesting
    ! levels of `emitted_indent_width` columns and re-emit it at `indent_size`
    ! columns of `indent_char` per level. Columns beyond a whole number of
    ! levels are alignment rather than nesting and are carried over unchanged.
    function rescale_indentation(code, indent_size, indent_char) result(rescaled)
        character(len=*), intent(in) :: code
        integer, intent(in) :: indent_size
        character(len=1), intent(in) :: indent_char
        character(len=:), allocatable :: rescaled

        integer :: line_start, line_end, newline_offset

        if (indent_size < 1) then
            rescaled = code
            return
        end if

        if (indent_size == emitted_indent_width .and. indent_char == " ") then
            rescaled = code
            return
        end if

        rescaled = ""
        line_start = 1
        do while (line_start <= len(code))
            newline_offset = index(code(line_start:), new_line("a"))
            if (newline_offset == 0) then
                line_end = len(code)
            else
                line_end = line_start + newline_offset - 2
            end if

            rescaled = rescaled// &
                rescale_line(code(line_start:line_end), indent_size, indent_char)
            if (line_end < len(code)) rescaled = rescaled//new_line("a")

            line_start = line_end + 2
        end do

    end function rescale_indentation

    function rescale_line(line, indent_size, indent_char) result(rescaled)
        character(len=*), intent(in) :: line
        integer, intent(in) :: indent_size
        character(len=1), intent(in) :: indent_char
        character(len=:), allocatable :: rescaled

        integer :: lead, level, alignment

        if (len_trim(line) == 0) then
            rescaled = ""
            return
        end if

        lead = verify(line, " ") - 1
        if (lead <= 0) then
            rescaled = line
            return
        end if

        level = lead/emitted_indent_width
        alignment = lead - level*emitted_indent_width
        rescaled = repeat(indent_char, level*indent_size)// &
            repeat(" ", alignment)//line(lead + 1:)

    end function rescale_line

end module fluff_format_indent
