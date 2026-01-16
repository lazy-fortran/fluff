module fluff_format_quality_improve
    use fluff_format_quality_types, only: aesthetic_settings_t, &
                                          create_aesthetic_settings
    implicit none
    private

    public :: apply_aesthetic_improvements
    public :: optimize_line_breaks
    public :: combine_short_lines

contains

    subroutine apply_aesthetic_improvements(input_code, output_code, settings)
        character(len=*), intent(in) :: input_code
        character(len=:), allocatable, intent(out) :: output_code
        type(aesthetic_settings_t), intent(in) :: settings

        character(len=:), allocatable :: temp_code

        temp_code = input_code

        if (settings%add_blank_lines) then
            call add_logical_blank_lines(temp_code, settings)
        end if

        if (settings%align_declarations) then
            call align_variable_declarations(temp_code)
        end if

        if (settings%align_assignments) then
            call align_assignment_operators(temp_code)
        end if

        if (settings%group_related_statements) then
            call group_related_code(temp_code)
        end if

        if (settings%improve_operator_spacing) then
            call improve_operator_spacing(temp_code)
        end if

        if (settings%optimize_line_breaks) then
            call optimize_line_breaks(temp_code, settings%max_line_length)
        end if

        output_code = temp_code

    end subroutine apply_aesthetic_improvements

    subroutine add_logical_blank_lines(code, settings)
        character(len=:), allocatable, intent(inout) :: code
        type(aesthetic_settings_t), intent(in) :: settings

        character(len=:), allocatable :: result
        integer :: code_index, line_start, line_end
        character(len=:), allocatable :: line, prev_line
        logical :: needs_blank_line

        result = ""
        prev_line = ""
        line_start = 1

        do code_index = 1, len(code)
            if (code(code_index:code_index) == new_line("a") .or. &
                code_index == len(code)) then
                line_end = code_index - 1
                if (code_index == len(code)) line_end = code_index

                line = trim(code(line_start:line_end))

                needs_blank_line = should_add_blank_line_before(line, prev_line)
                if (needs_blank_line .and. len(prev_line) > 0) then
                    result = result // new_line("a")
                end if

                result = result // line
                if (code_index < len(code)) result = result // new_line("a")

                prev_line = line
                line_start = code_index + 1
            end if
        end do

        code = result

    end subroutine add_logical_blank_lines

    subroutine align_variable_declarations(code)
        character(len=:), allocatable, intent(inout) :: code

        character(len=:), allocatable :: result
        integer :: decl_pos, search_pos

        result = code
        search_pos = 1

        do while (search_pos <= len(result))
            decl_pos = index(result(search_pos:), "::")
            if (decl_pos == 0) exit

            decl_pos = decl_pos + search_pos - 1

            if (decl_pos > 1 .and. result(decl_pos-1:decl_pos-1) /= " ") then
                result = result(1:decl_pos-1) // " " // result(decl_pos:)
                decl_pos = decl_pos + 1
            end if

            if (decl_pos < len(result) - 1 .and. &
                result(decl_pos+2:decl_pos+2) /= " ") then
                result = result(1:decl_pos+1) // " " // result(decl_pos+2:)
            end if

            search_pos = decl_pos + 2
        end do

        code = result

    end subroutine align_variable_declarations

    subroutine align_assignment_operators(code)
        character(len=:), allocatable, intent(inout) :: code

        character(len=:), allocatable :: result
        integer :: eq_pos, search_pos

        result = code
        search_pos = 1

        do while (search_pos <= len(result))
            eq_pos = index(result(search_pos:), "=")
            if (eq_pos == 0) exit

            eq_pos = eq_pos + search_pos - 1

            if (eq_pos > 1 .and. (result(eq_pos-1:eq_pos-1) == "=" .or. &
                                  result(eq_pos-1:eq_pos-1) == "/" .or. &
                                  result(eq_pos-1:eq_pos-1) == "<" .or. &
                                  result(eq_pos-1:eq_pos-1) == ">")) then
                search_pos = eq_pos + 1
                cycle
            end if

            if (eq_pos < len(result) .and. result(eq_pos+1:eq_pos+1) == "=") then
                search_pos = eq_pos + 2
                cycle
            end if

            if (eq_pos > 1 .and. result(eq_pos-1:eq_pos-1) /= " ") then
                result = result(1:eq_pos-1) // " " // result(eq_pos:)
                eq_pos = eq_pos + 1
            end if

            if (eq_pos < len(result) .and. result(eq_pos+1:eq_pos+1) /= " ") then
                result = result(1:eq_pos) // " " // result(eq_pos+1:)
            end if

            search_pos = eq_pos + 2
        end do

        code = result

    end subroutine align_assignment_operators

    subroutine group_related_code(code)
        character(len=:), allocatable, intent(inout) :: code

        call add_logical_blank_lines(code, create_aesthetic_settings())

    end subroutine group_related_code

    subroutine improve_operator_spacing(code)
        character(len=:), allocatable, intent(inout) :: code

        character(len=:), allocatable :: result
        integer :: pos
        character :: op

        result = code

        pos = 1
        do while (pos <= len(result) - 1)
            op = result(pos:pos)

            select case (op)
            case ("+", "-", "*", "/")
                if (op == "*" .and. pos < len(result) .and. &
                    result(pos+1:pos+1) == "*") then
                    pos = pos + 1
                    cycle
                end if

                if (pos > 1 .and. result(pos-1:pos-1) /= " ") then
                    result = result(1:pos-1) // " " // result(pos:)
                    pos = pos + 1
                end if

                if (pos < len(result) .and. result(pos+1:pos+1) /= " ") then
                    result = result(1:pos) // " " // result(pos+1:)
                end if
            end select

            pos = pos + 1
        end do

        code = result

    end subroutine improve_operator_spacing

    subroutine optimize_line_breaks(code, max_length)
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: max_length

        type(aesthetic_settings_t) :: settings

        settings = create_aesthetic_settings()

        if (settings%combine_short_lines) then
            call combine_short_lines(code, max_length)
        end if

        call break_long_lines(code, max_length)

    end subroutine optimize_line_breaks

    subroutine break_long_lines(code, max_length)
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: max_length

        character(len=:), allocatable :: result
        character(len=:), allocatable :: line
        integer :: start_pos, end_pos, newline_pos
        logical :: first_line, skip_line_breaking

        result = ""
        first_line = .true.
        skip_line_breaking = .false.

        start_pos = 1
        do while (start_pos <= len(code))
            newline_pos = index(code(start_pos:), new_line("a"))
            if (newline_pos == 0) then
                end_pos = len(code)
            else
                end_pos = start_pos + newline_pos - 2
            end if

            if (end_pos >= start_pos) then
                line = code(start_pos:end_pos)
            else
                line = ""
            end if

            if (index(adjustl(line), "! fmt: skip") > 0 .or. &
                index(adjustl(line), "! fluff: noqa") > 0) then
                skip_line_breaking = .true.
            else if (index(adjustl(line), "! fmt: on") > 0 .or. &
                     index(adjustl(line), "! fluff: qa") > 0) then
                skip_line_breaking = .false.
            end if

            if (len_trim(line) > max_length .and. .not. skip_line_breaking) then
                if (.not. first_line) result = result // new_line("a")
                call break_fortran_line(line(1:len_trim(line)), result, max_length)
                first_line = .false.
            else
                if (.not. first_line) result = result // new_line("a")
                if (len_trim(line) > 0) then
                    result = result // line(1:len_trim(line))
                end if
                first_line = .false.
            end if

            if (newline_pos == 0) exit
            start_pos = start_pos + newline_pos
        end do

        code = result

    end subroutine break_long_lines

    subroutine combine_short_lines(code, max_length)
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: max_length

        character(len=:), allocatable :: result
        character(len=:), allocatable :: combined_line
        character(len=:), allocatable :: current_line, next_line
        integer :: pos, next_pos
        integer :: current_end, next_end
        integer :: current_newline, next_newline
        logical :: can_combine, first_line, skip_combining

        result = ""
        pos = 1
        first_line = .true.
        skip_combining = .false.

        do while (pos <= len(code))
            current_newline = index(code(pos:), new_line("a"))
            if (current_newline == 0) then
                current_end = len(code)
            else
                current_end = pos + current_newline - 2
            end if

            if (current_end >= pos) then
                current_line = code(pos:current_end)
            else
                current_line = ""
            end if

            if (index(adjustl(current_line), "! fmt: skip") > 0 .or. &
                index(adjustl(current_line), "! fluff: noqa") > 0) then
                skip_combining = .true.
            else if (index(adjustl(current_line), "! fmt: on") > 0 .or. &
                     index(adjustl(current_line), "! fluff: qa") > 0) then
                skip_combining = .false.
            end if

            can_combine = .false.
            if (current_newline /= 0 .and. .not. skip_combining) then
                next_pos = pos + current_newline
                if (next_pos <= len(code)) then
                    next_newline = index(code(next_pos:), new_line("a"))
                    if (next_newline == 0) then
                        next_end = len(code)
                    else
                        next_end = next_pos + next_newline - 2
                    end if

                    if (next_end >= next_pos) then
                        next_line = code(next_pos:next_end)
                    else
                        next_line = ""
                    end if

                    if (can_combine_lines(trim(current_line), adjustl(next_line), &
                                          max_length)) then
                        call combine_two_lines(trim(current_line), adjustl(next_line), &
                                               combined_line)
                        if (len_trim(combined_line) <= max_length) then
                            if (.not. first_line) result = result // new_line("a")
                            result = result // combined_line
                            first_line = .false.
                            can_combine = .true.

                            if (next_newline == 0) exit
                            pos = next_pos + next_newline
                        end if
                    end if
                end if
            end if

            if (.not. can_combine) then
                if (.not. first_line) result = result // new_line("a")
                if (len_trim(current_line) > 0) then
                    result = result // current_line(1:len_trim(current_line))
                end if
                first_line = .false.

                if (current_newline == 0) exit
                pos = pos + current_newline
            end if
        end do

        code = result

    end subroutine combine_short_lines

    function can_combine_lines(line1, line2, max_length) result(can_combine)
        character(len=*), intent(in) :: line1, line2
        integer, intent(in) :: max_length
        logical :: can_combine

        character(len=:), allocatable :: trim1, trim2

        can_combine = .false.
        trim1 = adjustl(line1)
        trim2 = adjustl(line2)

        if (len_trim(trim1) == 0 .or. len_trim(trim2) == 0) return
        if (trim1(1:1) == "!" .or. trim2(1:1) == "!") return

        if (is_block_start(trim1) .or. is_block_start(trim2)) return
        if (is_block_end(trim1) .or. is_block_end(trim2)) return

        if (len_trim(line1) > 0) then
            if (line1(len_trim(line1):len_trim(line1)) == "&") return
        end if

        if (len_trim(line1) + len_trim(line2) + 1 > max_length) return

        if (is_declaration(trim1) .and. is_declaration(trim2)) then
            if (len_trim(line1) < max_length / 4 .and. &
                len_trim(line2) < max_length / 4) then
                can_combine = .true.
                return
            end if
        end if

        if (is_simple_statement(trim1) .and. is_simple_statement(trim2)) then
            if (len_trim(line1) < max_length / 3 .and. &
                len_trim(line2) < max_length / 3) then
                can_combine = .true.
                return
            end if
        end if

    end function can_combine_lines

    subroutine combine_two_lines(line1, line2, combined)
        character(len=*), intent(in) :: line1, line2
        character(len=:), allocatable, intent(out) :: combined

        character(len=:), allocatable :: trim1, trim2

        trim1 = trim(line1)
        trim2 = adjustl(line2)
        combined = trim1 // "; " // trim2

    end subroutine combine_two_lines

    function is_block_start(line) result(is_start)
        character(len=*), intent(in) :: line
        logical :: is_start

        is_start = index(line, "program ") == 1 .or. &
                   index(line, "module ") == 1 .or. &
                   index(line, "subroutine ") == 1 .or. &
                   index(line, "function ") == 1 .or. &
                   index(line, "type ") == 1 .or. &
                   index(line, "interface") == 1 .or. &
                   index(line, "contains") == 1

    end function is_block_start

    function is_block_end(line) result(is_end)
        character(len=*), intent(in) :: line
        logical :: is_end

        is_end = index(line, "end ") == 1 .or. &
                 index(line, "contains") == 1

    end function is_block_end

    function is_declaration(line) result(is_decl)
        character(len=*), intent(in) :: line
        logical :: is_decl

        is_decl = index(line, "integer") == 1 .or. &
                  index(line, "real") == 1 .or. &
                  index(line, "character") == 1 .or. &
                  index(line, "logical") == 1 .or. &
                  index(line, "complex") == 1 .or. &
                  index(line, "type") == 1

    end function is_declaration

    function is_simple_statement(line) result(is_simple)
        character(len=*), intent(in) :: line
        logical :: is_simple
        integer :: eq_pos

        eq_pos = index(line, "=")
        is_simple = eq_pos > 0 .and. index(line, "==") == 0 .and. &
                    index(line, ">=") == 0 .and. index(line, "<=") == 0

    end function is_simple_statement

    subroutine break_fortran_line(line, result, max_length)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(inout) :: result
        integer, intent(in) :: max_length

        character(len=:), allocatable :: remaining, current_segment
        integer :: break_pos, break_next_start, leading_spaces, trimmed_len
        logical :: found_break

        leading_spaces = count_leading_spaces(line)
        remaining = line

        do while (len_trim(remaining) > 0)
            trimmed_len = len_trim(remaining)
            if (trimmed_len <= max_length) then
                result = result // remaining(1:trimmed_len)
                exit
            end if

            call find_break_location(remaining, max_length, break_pos, &
                                     break_next_start, found_break)

            if (.not. found_break .or. break_pos <= 0) then
                result = result // remaining(1:trimmed_len)
                exit
            end if

            current_segment = remaining(1:break_pos) // " &"
            result = result // trim(current_segment)

            call prepare_continuation_line(remaining, break_next_start, leading_spaces)
            if (len_trim(remaining) > 0) then
                result = result // new_line("a")
            end if
        end do

    end subroutine break_fortran_line

    integer function count_leading_spaces(line) result(num_spaces)
        character(len=*), intent(in) :: line
        integer :: index

        num_spaces = 0
        do index = 1, len(line)
            if (line(index:index) == " ") then
                num_spaces = num_spaces + 1
            else
                exit
            end if
        end do

    end function count_leading_spaces

    subroutine find_break_location(remaining, max_length, break_pos, break_next_start, &
                                   found_break)
        character(len=*), intent(in) :: remaining
        integer, intent(in) :: max_length
        integer, intent(out) :: break_pos, break_next_start
        logical, intent(out) :: found_break

        integer :: char_index, limit, trimmed_len
        logical :: in_string, seen_nonspace
        character :: quote_char

        found_break = .false.
        break_pos = 0
        break_next_start = 0

        in_string = .false.
        seen_nonspace = .false.
        quote_char = " "

        trimmed_len = len_trim(remaining)
        limit = min(max_length, trimmed_len)

        char_index = 1
        do while (char_index <= limit)
            if (in_string) then
                if (remaining(char_index:char_index) == quote_char) then
                    if (char_index < trimmed_len .and. &
                        remaining(char_index+1:char_index+1) == quote_char) then
                        char_index = char_index + 1
                    else
                        in_string = .false.
                        quote_char = " "
                    end if
                end if
            else
                if (remaining(char_index:char_index) == '"' .or. &
                    remaining(char_index:char_index) == "'") then
                    in_string = .true.
                    quote_char = remaining(char_index:char_index)
                    seen_nonspace = .true.
                else
                    if (remaining(char_index:char_index) /= " ") seen_nonspace = .true.
                    if (.not. seen_nonspace) then
                        char_index = char_index + 1
                        cycle
                    end if

                    if (remaining(char_index:char_index) == " " .and. &
                        char_index > 1 .and. char_index < trimmed_len) then
                        break_pos = char_index - 1
                        break_next_start = char_index + 1
                        found_break = .true.
                    else if (char_index < trimmed_len) then
                        select case (remaining(char_index:char_index))
                        case (",", "+", "-", "/", ")")
                            break_pos = char_index
                            break_next_start = char_index + 1
                            found_break = .true.
                        case ("*")
                            if (char_index < trimmed_len) then
                                if (remaining(char_index+1:char_index+1) /= "*") then
                                    break_pos = char_index
                                    break_next_start = char_index + 1
                                    found_break = .true.
                                end if
                            else
                                break_pos = char_index
                                break_next_start = char_index + 1
                                found_break = .true.
                            end if
                        end select
                    end if
                end if
            end if
            char_index = char_index + 1
        end do

    end subroutine find_break_location

    subroutine prepare_continuation_line(remaining, break_next_start, leading_spaces)
        character(len=:), allocatable, intent(inout) :: remaining
        integer, intent(in) :: break_next_start
        integer, intent(in) :: leading_spaces

        character(len=:), allocatable :: tail
        integer :: trimmed_len

        trimmed_len = len_trim(remaining)
        if (break_next_start <= trimmed_len) then
            tail = remaining(break_next_start:trimmed_len)
            remaining = repeat(" ", leading_spaces + 4) // trim(adjustl(tail))
        else
            remaining = ""
        end if

    end subroutine prepare_continuation_line

    function should_add_blank_line_before(current_line, previous_line) &
        result(needs_blank)
        character(len=*), intent(in) :: current_line, previous_line
        logical :: needs_blank

        character(len=:), allocatable :: curr_trim, prev_trim

        needs_blank = .false.
        curr_trim = trim(current_line)
        prev_trim = trim(previous_line)

        if (len(prev_trim) == 0) return

        if (index(curr_trim, "subroutine ") == 1 .or. &
            index(curr_trim, "function ") == 1 .or. &
            index(curr_trim, "pure function ") == 1 .or. &
            index(curr_trim, "elemental function ") == 1) then
            if (index(prev_trim, "end ") /= 1 .and. prev_trim /= "contains") then
                needs_blank = .true.
            end if
        end if

        if (index(curr_trim, "type ") == 1 .or. index(curr_trim, "type,") == 1) then
            needs_blank = .true.
        end if

        if (index(curr_trim, "do ") == 1 .or. index(curr_trim, "if ") == 1) then
            if (index(prev_trim, "integer") /= 1 .and. &
                index(prev_trim, "real") /= 1 .and. &
                index(prev_trim, "logical") /= 1 .and. &
                index(prev_trim, "character") /= 1) then
                needs_blank = .true.
            end if
        end if

    end function should_add_blank_line_before

end module fluff_format_quality_improve
