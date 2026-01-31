module fluff_json
    use fluff_json_lexer, only: skip_ws
    use fluff_json_string, only: json_escape_string, json_get_string_value
    use fluff_json_value, only: skip_value
    use fluff_json_object, only: json_has_member, json_get_member_json, &
                                 json_get_string_member, json_get_int_member, &
                                 json_is_object, json_is_null
    use fluff_json_array, only: json_array_get_element_json, json_array_length, &
                                json_is_array
    implicit none
    private

    public :: json_escape_string
    public :: json_has_member
    public :: json_parse
    public :: json_get_int_member
    public :: json_get_member_json
    public :: json_get_string_member
    public :: json_get_string_value
    public :: json_array_get_element_json
    public :: json_array_length
    public :: json_is_array
    public :: json_is_null
    public :: json_is_object

contains

    subroutine json_parse(text, success, error_message)
        character(len=*), intent(in) :: text
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message

        integer :: pos

        success = .false.
        error_message = ""
        pos = 1

        call skip_value(text, pos, success, error_message)
        if (.not. success) return

        call skip_ws(text, pos)
        if (pos <= len(text)) then
            success = .false.
            error_message = "Trailing data after JSON value"
        end if
    end subroutine json_parse

end module fluff_json
