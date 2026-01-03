module fluff_rule_file_context
    implicit none
    private

    public :: current_filename
    public :: current_source_text
    public :: set_current_file_context

    character(len=:), allocatable :: current_filename
    character(len=:), allocatable :: current_source_text

contains

    subroutine set_current_file_context(filename, source_text)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: source_text

        current_filename = filename
        current_source_text = source_text
    end subroutine set_current_file_context

end module fluff_rule_file_context
