module fluff_rule_f010
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_WARNING
    use fluff_rule_file_context, only: current_filename, current_source_text
    implicit none
    private

    public :: check_f010_obsolete_features

contains

    subroutine check_f010_obsolete_features(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: temp_violations(:)
        integer :: violation_count

        if (.not. allocated(current_source_text)) then
            allocate (violations(0))
            return
        end if

        allocate (temp_violations(100))
        violation_count = 0

        call analyze_obsolete_features_from_text(current_source_text, temp_violations, &
                                                 violation_count)

        allocate (violations(violation_count))
        if (violation_count > 0) then
            violations = temp_violations(1:violation_count)
        end if
    end subroutine check_f010_obsolete_features

    subroutine analyze_obsolete_features_from_text(source_text, violations, &
                                                   violation_count)
        character(len=*), intent(in) :: source_text
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        integer :: pos, line_num
        logical :: done
        character(len=:), allocatable :: line_content
        character(len=:), allocatable :: line_lower

        pos = 1
        line_num = 0

        do
            call next_source_line(source_text, pos, line_num, line_content, done)
            if (done) exit
            if (is_comment_line(line_content)) cycle

            line_lower = to_lower(line_content)
            call check_obsolete_feature(line_lower, line_num, "goto", &
                                        "Obsolete feature: GOTO", violations, &
                                        violation_count)
            call check_obsolete_feature(line_lower, line_num, "common", &
                                        "Obsolete feature: COMMON", violations, &
                                        violation_count)
            call check_obsolete_feature(line_lower, line_num, "equivalence", &
                                        "Obsolete feature: EQUIVALENCE", violations, &
                                        violation_count)
        end do
    end subroutine analyze_obsolete_features_from_text

    subroutine next_source_line(source_text, pos, line_num, line_content, done)
        character(len=*), intent(in) :: source_text
        integer, intent(inout) :: pos
        integer, intent(inout) :: line_num
        character(len=:), allocatable, intent(out) :: line_content
        logical, intent(out) :: done

        integer :: next_pos
        integer :: line_start, line_end

        if (pos > len(source_text)) then
            done = .true.
            line_content = ""
            return
        end if

        line_num = line_num + 1
        next_pos = index(source_text(pos:), new_line("a"))
        if (next_pos == 0) then
            line_start = pos
            line_end = len(source_text)
            pos = len(source_text) + 1
        else
            line_start = pos
            line_end = pos + next_pos - 2
            pos = pos + next_pos
        end if

        if (line_end < line_start) then
            line_content = ""
        else
            line_content = source_text(line_start:line_end)
        end if

        done = .false.
    end subroutine next_source_line

    subroutine check_obsolete_feature(line_lower, line_num, keyword, message, &
                                      violations, violation_count)
        character(len=*), intent(in) :: line_lower
        integer, intent(in) :: line_num
        character(len=*), intent(in) :: keyword
        character(len=*), intent(in) :: message
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count

        integer :: pos

        pos = index(line_lower, keyword)
        if (pos <= 0) return
        if (.not. is_keyword_at_position(line_lower, keyword, pos)) return

        call push_obsolete_violation(violations, violation_count, line_num, &
                                     pos, len(keyword), message)
    end subroutine check_obsolete_feature

    subroutine push_obsolete_violation(violations, violation_count, line_num, &
                                       pos, keyword_len, message)
        type(diagnostic_t), intent(inout) :: violations(:)
        integer, intent(inout) :: violation_count
        integer, intent(in) :: line_num
        integer, intent(in) :: pos
        integer, intent(in) :: keyword_len
        character(len=*), intent(in) :: message

        type(source_range_t) :: location

        violation_count = violation_count + 1
        if (violation_count <= size(violations)) then
            location%start%line = line_num
            location%start%column = pos
            location%end%line = line_num
            location%end%column = pos + keyword_len - 1
            violations(violation_count) = create_diagnostic( &
                                          code="F010", &
                                          message=message, &
                                          file_path=current_filename, &
                                          location=location, &
                                          severity=SEVERITY_WARNING)
        end if
    end subroutine push_obsolete_violation

    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i
        integer :: ascii_val

        lower_str = str
        do i = 1, len(str)
            ascii_val = iachar(str(i:i))
            if (ascii_val >= iachar("A") .and. ascii_val <= iachar("Z")) then
                lower_str(i:i) = achar(ascii_val + 32)
            end if
        end do
    end function to_lower

    logical function is_keyword_at_position(line, keyword, pos) result(is_keyword)
        character(len=*), intent(in) :: line
        character(len=*), intent(in) :: keyword
        integer, intent(in) :: pos

        logical :: start_ok, end_ok
        integer :: end_pos

        end_pos = pos + len(keyword) - 1

        if (pos == 1) then
            start_ok = .true.
        else if (pos > 1 .and. pos <= len(line)) then
            start_ok = .not. is_alphanumeric(line(pos - 1:pos - 1))
        else
            start_ok = .false.
        end if

        if (end_pos == len(line)) then
            end_ok = .true.
        else if (end_pos > 0 .and. end_pos < len(line)) then
            end_ok = .not. is_alphanumeric(line(end_pos + 1:end_pos + 1))
        else
            end_ok = .false.
        end if

        is_keyword = start_ok .and. end_ok
    end function is_keyword_at_position

    logical function is_alphanumeric(ch) result(is_alnum)
        character, intent(in) :: ch

        is_alnum = (ch >= "a" .and. ch <= "z") .or. &
                   (ch >= "A" .and. ch <= "Z") .or. &
                   (ch >= "0" .and. ch <= "9") .or. &
                   (ch == "_")
    end function is_alphanumeric

    logical function is_comment_line(line) result(is_comment)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: trimmed

        trimmed = adjustl(line)
        if (len_trim(trimmed) == 0) then
            is_comment = .false.
        else
            is_comment = trimmed(1:1) == "!"
        end if
    end function is_comment_line

end module fluff_rule_f010
