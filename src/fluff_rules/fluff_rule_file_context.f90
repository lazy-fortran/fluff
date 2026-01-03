module fluff_rule_file_context
    implicit none
    private

    public :: current_filename
    public :: current_source_text
    public :: current_line_length
    public :: set_current_file_context

    character(len=:), allocatable :: current_filename
    character(len=:), allocatable :: current_source_text
    integer :: current_line_length = 88

contains

    subroutine set_current_file_context(filename, source_text, line_length)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: source_text
        integer, intent(in), optional :: line_length

        current_filename = filename
        current_source_text = source_text
        if (present(line_length)) then
            if (line_length > 0) then
                current_line_length = line_length
            else
                current_line_length = 88
            end if
        else
            current_line_length = 88
        end if
    end subroutine set_current_file_context

end module fluff_rule_file_context
