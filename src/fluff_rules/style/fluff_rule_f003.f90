module fluff_rule_f003
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename, current_line_length
    implicit none
    private

    public :: check_f003_line_length
    public :: f003_visual_columns

contains

    subroutine check_f003_line_length(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        integer :: violation_count
        integer :: line_num
        integer :: max_length
        integer :: line_length
        logical :: found
        character(len=:), allocatable :: line_text

        if (.not. ctx%has_source()) then
            allocate (violations(0))
            return
        end if

        max_length = current_line_length
        if (max_length <= 0) max_length = 88

        violation_count = 0
        line_num = 1
        do
            call ctx%get_source_line(line_num, line_text, found)
            if (.not. found) exit

            line_length = f003_visual_columns(line_text)
            if (line_length > max_length) then
                if (.not. is_comment_only_line(line_text)) then
                    violation_count = violation_count + 1
                end if
            end if

            line_num = line_num + 1
        end do

        allocate (violations(violation_count))
        if (violation_count == 0) then
            return
        end if

        violation_count = 0
        line_num = 1
        do
            call ctx%get_source_line(line_num, line_text, found)
            if (.not. found) exit

            line_length = f003_visual_columns(line_text)
            if (line_length > max_length) then
                if (.not. is_comment_only_line(line_text)) then
                    violation_count = violation_count + 1
                    violations(violation_count) = create_f003_diagnostic(line_num, &
                                                                         line_length, &
                                                                         max_length)
                end if
            end if

            line_num = line_num + 1
        end do
    end subroutine check_f003_line_length

    function create_f003_diagnostic(line_num, line_length, max_length) result(diag)
        integer, intent(in) :: line_num
        integer, intent(in) :: line_length
        integer, intent(in) :: max_length
        type(diagnostic_t) :: diag

        type(source_range_t) :: location

        location%start%line = line_num
        location%start%column = max_length + 1
        location%end%line = line_num
        location%end%column = line_length

        diag = create_diagnostic( &
               code="F003", &
               message="Line too long ("//trim(int_to_str(line_length))//" > "// &
               trim(int_to_str(max_length))//" characters)", &
               file_path=current_filename, &
               location=location, &
               severity=SEVERITY_WARNING)
    end function create_f003_diagnostic

    integer function f003_visual_columns(line_text) result(cols)
        character(len=*), intent(in) :: line_text
        integer, parameter :: tab_width = 4
        integer :: i, col, next_stop
        character(len=1) :: ch

        col = 0
        do i = 1, len(line_text)
            ch = line_text(i:i)
            if (i == len(line_text) .and. ch == achar(13)) exit

            select case (ch)
            case (achar(9))
                next_stop = ((col/tab_width) + 1)*tab_width
                col = next_stop
            case default
                col = col + 1
            end select
        end do

        cols = col
    end function f003_visual_columns

    logical function is_comment_only_line(line_text) result(is_comment)
        character(len=*), intent(in) :: line_text

        integer :: i
        character(len=1) :: ch

        is_comment = .false.
        do i = 1, len(line_text)
            ch = line_text(i:i)
            if (ch == " " .or. ch == achar(9) .or. ch == achar(13)) cycle
            is_comment = (ch == "!")
            return
        end do
    end function is_comment_only_line

    function int_to_str(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str

        write (str, "(I0)") i
    end function int_to_str

end module fluff_rule_f003
