module fluff_json_array
    use fluff_json_lexer, only: skip_ws
    use fluff_json_value, only: skip_value
    implicit none
    private

    public :: json_array_get_element_json
    public :: json_array_length
    public :: json_is_array

contains

    subroutine json_array_get_element_json(array_json, idx, element_json, &
                                           found, success)
        character(len=*), intent(in) :: array_json
        integer, intent(in) :: idx
        character(len=:), allocatable, intent(out) :: element_json
        logical, intent(out) :: found
        logical, intent(out) :: success

        integer :: pos
        integer :: i
        integer :: start_pos, end_pos
        character(len=:), allocatable :: err

        element_json = ""
        found = .false.
        success = .false.
        err = ""
        pos = 1

        if (idx < 1) return

        call skip_ws(array_json, pos)
        if (pos > len(array_json) .or. array_json(pos:pos) /= '[') return
        pos = pos + 1
        call skip_ws(array_json, pos)

        if (pos <= len(array_json) .and. array_json(pos:pos) == ']') then
            success = .true.
            return
        end if

        do i = 1, idx
            call skip_ws(array_json, pos)
            start_pos = pos
            call skip_value(array_json, pos, success, err)
            if (.not. success) return
            end_pos = pos - 1

            if (i == idx) then
                element_json = array_json(start_pos:end_pos)
                found = .true.
            end if

            call skip_ws(array_json, pos)
            if (pos > len(array_json)) then
                success = .false.
                return
            end if

            if (array_json(pos:pos) == ',') then
                pos = pos + 1
                cycle
            end if
            if (array_json(pos:pos) == ']') then
                pos = pos + 1
                exit
            end if

            success = .false.
            return
        end do

        success = .true.
    end subroutine json_array_get_element_json

    subroutine json_array_length(array_json, length, success)
        character(len=*), intent(in) :: array_json
        integer, intent(out) :: length
        logical, intent(out) :: success

        integer :: pos
        character(len=:), allocatable :: err

        length = 0
        success = .false.
        err = ""
        pos = 1

        call skip_ws(array_json, pos)
        if (pos > len(array_json) .or. array_json(pos:pos) /= '[') return
        pos = pos + 1
        call skip_ws(array_json, pos)

        if (pos <= len(array_json) .and. array_json(pos:pos) == ']') then
            pos = pos + 1
            call skip_ws(array_json, pos)
            success = pos > len(array_json)
            return
        end if

        do
            call skip_value(array_json, pos, success, err)
            if (.not. success) return
            length = length + 1

            call skip_ws(array_json, pos)
            if (pos > len(array_json)) then
                success = .false.
                return
            end if

            if (array_json(pos:pos) == ',') then
                pos = pos + 1
                cycle
            end if
            if (array_json(pos:pos) == ']') then
                pos = pos + 1
                exit
            end if

            success = .false.
            return
        end do

        call skip_ws(array_json, pos)
        success = pos > len(array_json)
    end subroutine json_array_length

    subroutine json_is_array(value_json, is_array, success)
        character(len=*), intent(in) :: value_json
        logical, intent(out) :: is_array
        logical, intent(out) :: success

        integer :: pos

        pos = 1
        call skip_ws(value_json, pos)
        if (pos > len(value_json)) then
            is_array = .false.
            success = .false.
            return
        end if

        is_array = value_json(pos:pos) == '['
        success = .true.
    end subroutine json_is_array

end module fluff_json_array
