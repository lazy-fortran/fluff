module fluff_json_object
    use fluff_json_lexer, only: skip_ws, json_starts_with_literal
    use fluff_json_string, only: parse_string_value, json_get_string_value
    use fluff_json_value, only: skip_value
    implicit none
    private

    public :: json_has_member
    public :: json_get_member_json
    public :: json_get_string_member
    public :: json_get_int_member
    public :: json_is_object
    public :: json_is_null

contains

    subroutine json_has_member(obj_json, key, found, success)
        character(len=*), intent(in) :: obj_json
        character(len=*), intent(in) :: key
        logical, intent(out) :: found
        logical, intent(out) :: success

        character(len=:), allocatable :: tmp

        call json_get_member_json(obj_json, key, tmp, found, success)
    end subroutine json_has_member

    subroutine json_get_string_member(obj_json, key, out, found, success)
        character(len=*), intent(in) :: obj_json
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: found
        logical, intent(out) :: success

        character(len=:), allocatable :: value_json

        out = ""
        call json_get_member_json(obj_json, key, value_json, found, success)
        if (.not. success .or. .not. found) return
        call json_get_string_value(value_json, out, success)
        if (.not. success) then
            found = .false.
            out = ""
        end if
    end subroutine json_get_string_member

    subroutine json_get_int_member(obj_json, key, out, found, success)
        character(len=*), intent(in) :: obj_json
        character(len=*), intent(in) :: key
        integer, intent(out) :: out
        logical, intent(out) :: found
        logical, intent(out) :: success

        character(len=:), allocatable :: value_json
        integer :: pos
        integer :: start_pos, end_pos
        integer :: iostat_val

        out = 0
        call json_get_member_json(obj_json, key, value_json, found, success)
        if (.not. success .or. .not. found) return

        pos = 1
        call skip_ws(value_json, pos)

        if (json_starts_with_literal(value_json, pos, "null")) then
            out = 0
            found = .false.
            success = .true.
            return
        end if

        start_pos = pos
        if (pos <= len(value_json) .and. value_json(pos:pos) == '-') pos = pos + 1
        if (pos > len(value_json)) then
            found = .false.
            success = .false.
            return
        end if

        if (value_json(pos:pos) < '0' .or. value_json(pos:pos) > '9') then
            found = .false.
            success = .false.
            return
        end if

        do while (pos <= len(value_json))
            if (value_json(pos:pos) < '0' .or. value_json(pos:pos) > '9') exit
            pos = pos + 1
        end do
        end_pos = pos - 1

        call skip_ws(value_json, pos)
        if (pos <= len(value_json)) then
            found = .false.
            success = .false.
            return
        end if

        read (value_json(start_pos:end_pos), *, iostat=iostat_val) out
        success = iostat_val == 0
        if (.not. success) found = .false.
    end subroutine json_get_int_member

    subroutine json_get_member_json(obj_json, key, value_json, found, success)
        character(len=*), intent(in) :: obj_json
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: value_json
        logical, intent(out) :: found
        logical, intent(out) :: success

        integer :: pos
        character(len=:), allocatable :: err
        character(len=:), allocatable :: member_key
        integer :: start_pos, end_pos

        value_json = ""
        found = .false.
        success = .false.
        err = ""
        pos = 1

        call skip_ws(obj_json, pos)
        if (pos > len(obj_json) .or. obj_json(pos:pos) /= '{') then
            return
        end if
        pos = pos + 1

        call skip_ws(obj_json, pos)
        if (pos <= len(obj_json) .and. obj_json(pos:pos) == '}') then
            success = .true.
            return
        end if

        do
            call skip_ws(obj_json, pos)
            call parse_string_value(obj_json, pos, member_key, success, err)
            if (.not. success) return

            call skip_ws(obj_json, pos)
            if (pos > len(obj_json) .or. obj_json(pos:pos) /= ':') then
                success = .false.
                return
            end if
            pos = pos + 1

            call skip_ws(obj_json, pos)
            start_pos = pos
            call skip_value(obj_json, pos, success, err)
            if (.not. success) return
            end_pos = pos - 1

            if (member_key == key) then
                value_json = obj_json(start_pos:end_pos)
                found = .true.
            end if

            call skip_ws(obj_json, pos)
            if (pos > len(obj_json)) then
                success = .false.
                return
            end if

            if (obj_json(pos:pos) == ',') then
                pos = pos + 1
                cycle
            end if
            if (obj_json(pos:pos) == '}') then
                pos = pos + 1
                exit
            end if

            success = .false.
            return
        end do

        success = .true.
    end subroutine json_get_member_json

    subroutine json_is_object(value_json, is_object, success)
        character(len=*), intent(in) :: value_json
        logical, intent(out) :: is_object
        logical, intent(out) :: success

        integer :: pos

        pos = 1
        call skip_ws(value_json, pos)
        if (pos > len(value_json)) then
            is_object = .false.
            success = .false.
            return
        end if

        is_object = value_json(pos:pos) == '{'
        success = .true.
    end subroutine json_is_object

    subroutine json_is_null(value_json, is_null, success)
        character(len=*), intent(in) :: value_json
        logical, intent(out) :: is_null
        logical, intent(out) :: success

        integer :: pos

        pos = 1
        call skip_ws(value_json, pos)
        is_null = json_starts_with_literal(value_json, pos, "null")
        success = .true.
    end subroutine json_is_null

end module fluff_json_object
