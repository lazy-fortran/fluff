module fluff_visual_columns
    implicit none
    private

    public :: visual_columns

contains

    integer function visual_columns(line_text, tab_width) result(cols)
        character(len=*), intent(in) :: line_text
        integer, intent(in), optional :: tab_width

        integer :: width
        integer :: i
        integer :: col
        integer :: next_stop
        character(len=1) :: ch

        width = 4
        if (present(tab_width)) width = tab_width

        col = 0
        do i = 1, len(line_text)
            ch = line_text(i:i)
            if (i == len(line_text) .and. ch == achar(13)) exit

            select case (ch)
            case (achar(9))
                next_stop = ((col/width) + 1)*width
                col = next_stop
            case default
                col = col + 1
            end select
        end do

        cols = col
    end function visual_columns

end module fluff_visual_columns
