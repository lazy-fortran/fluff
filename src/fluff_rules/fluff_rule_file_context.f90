module fluff_rule_file_context
    implicit none
    private

    public :: current_filename
    public :: current_line_length
    public :: current_tab_width
    public :: set_current_file_context

    character(len=:), allocatable :: current_filename
    integer :: current_line_length = 88
    integer :: current_tab_width = 4

contains

    subroutine set_current_file_context(filename, line_length, tab_width)
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: line_length
        integer, intent(in), optional :: tab_width

        current_filename = filename
        if (present(line_length)) then
            if (line_length > 0) then
                current_line_length = line_length
            else
                current_line_length = 88
            end if
        else
            current_line_length = 88
        end if
        if (present(tab_width)) then
            if (tab_width > 0) then
                current_tab_width = tab_width
            else
                current_tab_width = 4
            end if
        else
            current_tab_width = 4
        end if
    end subroutine set_current_file_context

end module fluff_rule_file_context
