module fluff_format_quality
    ! Format quality assessment and metrics
    use iso_fortran_env, only: dp => real64
    implicit none
    private
    
    ! Quality metrics type
    type, public :: format_quality_t
        real(dp) :: indentation_score = 0.0_dp
        real(dp) :: spacing_score = 0.0_dp
        real(dp) :: readability_score = 0.0_dp
        real(dp) :: structure_score = 0.0_dp
        real(dp) :: consistency_score = 0.0_dp
        real(dp) :: line_length_score = 0.0_dp
        real(dp) :: overall_score = 0.0_dp
        integer :: total_lines = 0
        integer :: blank_lines = 0
        integer :: long_lines = 0
        character(len=:), allocatable :: recommendations(:)
    contains
        procedure :: calculate_overall_score
        procedure :: generate_recommendations
        procedure :: print_report
    end type format_quality_t
    
    ! Aesthetic improvement settings
    type, public :: aesthetic_settings_t
        logical :: add_blank_lines = .true.
        logical :: align_declarations = .true.
        logical :: align_assignments = .true.
        logical :: group_related_statements = .true.
        logical :: improve_operator_spacing = .true.
        logical :: optimize_line_breaks = .true.
        integer :: max_line_length = 88
        integer :: indent_size = 4
        real(dp) :: blank_line_ratio = 0.15_dp  ! Target 15% blank lines
    end type aesthetic_settings_t
    
    ! Public interface
    public :: assess_format_quality, apply_aesthetic_improvements
    public :: create_quality_metrics, create_aesthetic_settings
    public :: optimize_line_breaks
    
contains
    
    ! Create default quality metrics
    function create_quality_metrics() result(quality)
        type(format_quality_t) :: quality
        ! Initialized with default values
    end function create_quality_metrics
    
    ! Create default aesthetic settings
    function create_aesthetic_settings() result(settings)
        type(aesthetic_settings_t) :: settings
        ! Initialized with default values
    end function create_aesthetic_settings
    
    ! Assess the quality of formatted code
    subroutine assess_format_quality(code, quality)
        character(len=*), intent(in) :: code
        type(format_quality_t), intent(out) :: quality
        
        ! Analyze different quality aspects
        call assess_indentation(code, quality%indentation_score)
        call assess_spacing(code, quality%spacing_score)
        call assess_readability(code, quality%readability_score)
        call assess_structure(code, quality%structure_score)
        call assess_consistency(code, quality%consistency_score)
        call assess_line_length(code, quality%line_length_score)
        
        ! Count metrics
        quality%total_lines = count_lines(code)
        quality%blank_lines = count_blank_lines(code)
        quality%long_lines = count_long_lines(code, 88)
        
        ! Calculate overall score
        call quality%calculate_overall_score()
        
        ! Generate recommendations
        call quality%generate_recommendations()
        
    end subroutine assess_format_quality
    
    ! Apply aesthetic improvements to code
    subroutine apply_aesthetic_improvements(input_code, output_code, settings)
        character(len=*), intent(in) :: input_code
        character(len=:), allocatable, intent(out) :: output_code
        type(aesthetic_settings_t), intent(in) :: settings
        
        character(len=:), allocatable :: temp_code
        
        ! Start with input code
        temp_code = input_code
        
        ! Apply improvements in order
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
    
    ! Quality assessment procedures
    subroutine assess_indentation(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score
        
        integer :: consistent_lines, total_lines
        integer :: expected_indent, actual_indent
        integer :: i, line_start, line_end
        logical :: in_procedure, in_if, in_do
        
        total_lines = count_lines(code)
        consistent_lines = 0
        expected_indent = 0
        
        in_procedure = .false.
        in_if = .false.
        in_do = .false.
        
        line_start = 1
        do i = 1, len(code)
            if (code(i:i) == new_line('a') .or. i == len(code)) then
                line_end = i - 1
                if (i == len(code)) line_end = i
                
                call analyze_line_indentation(code(line_start:line_end), &
                    expected_indent, actual_indent, in_procedure, in_if, in_do)
                
                if (abs(actual_indent - expected_indent) <= 1) then
                    consistent_lines = consistent_lines + 1
                end if
                
                line_start = i + 1
            end if
        end do
        
        if (total_lines > 0) then
            score = real(consistent_lines, dp) / real(total_lines, dp) * 10.0_dp
        else
            score = 10.0_dp
        end if
        
    end subroutine assess_indentation
    
    subroutine assess_spacing(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score
        
        integer :: good_spacing, total_operators
        integer :: i
        
        good_spacing = 0
        total_operators = 0
        
        ! Check spacing around common operators
        do i = 1, len(code) - 2
            select case (code(i:i))
            case ('=')
                total_operators = total_operators + 1
                if (i > 1 .and. i < len(code)) then
                    if (code(i-1:i-1) == ' ' .and. code(i+1:i+1) == ' ') then
                        good_spacing = good_spacing + 1
                    end if
                end if
            case ('+', '-', '*', '/')
                if (code(i-1:i+1) /= '**') then  ! Skip ** operator
                    total_operators = total_operators + 1
                    if (i > 1 .and. i < len(code)) then
                        if (code(i-1:i-1) == ' ' .and. code(i+1:i+1) == ' ') then
                            good_spacing = good_spacing + 1
                        end if
                    end if
                end if
            end select
        end do
        
        if (total_operators > 0) then
            score = real(good_spacing, dp) / real(total_operators, dp) * 10.0_dp
        else
            score = 9.0_dp  ! Default good score if no operators
        end if
        
    end subroutine assess_spacing
    
    subroutine assess_readability(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score
        
        integer :: total_lines, blank_lines
        real(dp) :: blank_line_ratio, ideal_ratio
        
        total_lines = count_lines(code)
        blank_lines = count_blank_lines(code)
        ideal_ratio = 0.15_dp  ! 15% blank lines is ideal
        
        if (total_lines > 0) then
            blank_line_ratio = real(blank_lines, dp) / real(total_lines, dp)
            
            ! Score based on how close to ideal ratio
            if (blank_line_ratio >= ideal_ratio) then
                score = 10.0_dp - min(5.0_dp, (blank_line_ratio - ideal_ratio) * 20.0_dp)
            else
                score = 5.0_dp + (blank_line_ratio / ideal_ratio) * 5.0_dp
            end if
        else
            score = 8.0_dp
        end if
        
    end subroutine assess_readability
    
    subroutine assess_structure(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score
        
        logical :: has_implicit_none, has_proper_end, has_contains
        
        has_implicit_none = index(code, 'implicit none') > 0
        has_proper_end = index(code, 'end program') > 0 .or. &
                        index(code, 'end module') > 0 .or. &
                        index(code, 'end subroutine') > 0 .or. &
                        index(code, 'end function') > 0
        has_contains = index(code, 'contains') > 0
        
        score = 6.0_dp
        if (has_implicit_none) score = score + 2.0_dp
        if (has_proper_end) score = score + 1.5_dp
        if (has_contains) score = score + 0.5_dp
        
    end subroutine assess_structure
    
    subroutine assess_consistency(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score
        
        ! Simplified consistency check
        logical :: consistent_case, consistent_spacing
        
        consistent_case = check_case_consistency(code)
        consistent_spacing = check_spacing_consistency(code)
        
        score = 6.0_dp
        if (consistent_case) score = score + 2.0_dp
        if (consistent_spacing) score = score + 2.0_dp
        
    end subroutine assess_consistency
    
    subroutine assess_line_length(code, score)
        character(len=*), intent(in) :: code
        real(dp), intent(out) :: score
        
        integer :: total_lines, long_lines
        
        total_lines = count_lines(code)
        long_lines = count_long_lines(code, 88)
        
        if (total_lines > 0) then
            score = real(total_lines - long_lines, dp) / real(total_lines, dp) * 10.0_dp
        else
            score = 10.0_dp
        end if
        
    end subroutine assess_line_length
    
    ! Aesthetic improvement procedures
    subroutine add_logical_blank_lines(code, settings)
        character(len=:), allocatable, intent(inout) :: code
        type(aesthetic_settings_t), intent(in) :: settings
        
        character(len=:), allocatable :: result
        integer :: i, line_start, line_end, pos
        character(len=:), allocatable :: line, prev_line
        logical :: needs_blank_line
        
        result = ""
        prev_line = ""
        line_start = 1
        
        do i = 1, len(code)
            if (code(i:i) == new_line('a') .or. i == len(code)) then
                line_end = i - 1
                if (i == len(code)) line_end = i
                
                line = trim(code(line_start:line_end))
                
                ! Check if we need a blank line before this line
                needs_blank_line = should_add_blank_line_before(line, prev_line)
                
                if (needs_blank_line .and. len(prev_line) > 0) then
                    result = result // new_line('a')
                end if
                
                result = result // line
                if (i < len(code)) result = result // new_line('a')
                
                prev_line = line
                line_start = i + 1
            end if
        end do
        
        code = result
        
    end subroutine add_logical_blank_lines
    
    subroutine align_variable_declarations(code)
        character(len=:), allocatable, intent(inout) :: code
        
        ! Simplified alignment - align :: in declarations
        character(len=:), allocatable :: result
        integer :: max_name_length, i, j, pos
        character(len=:), allocatable :: line
        
        ! For now, just ensure consistent spacing around ::
        result = code
        
        ! Replace inconsistent :: spacing
        pos = 1
        do while (pos <= len(result))
            i = index(result(pos:), '::')
            if (i == 0) exit
            
            i = i + pos - 1
            
            ! Ensure space before and after ::
            if (i > 1 .and. result(i-1:i-1) /= ' ') then
                result = result(1:i-1) // ' ' // result(i:)
                i = i + 1
            end if
            
            if (i < len(result) - 1 .and. result(i+2:i+2) /= ' ') then
                result = result(1:i+1) // ' ' // result(i+2:)
            end if
            
            pos = i + 2
        end do
        
        code = result
        
    end subroutine align_variable_declarations
    
    subroutine align_assignment_operators(code)
        character(len=:), allocatable, intent(inout) :: code
        
        ! Simplified alignment - ensure consistent spacing around =
        character(len=:), allocatable :: result
        integer :: i, pos
        
        result = code
        pos = 1
        
        do while (pos <= len(result))
            i = index(result(pos:), '=')
            if (i == 0) exit
            
            i = i + pos - 1
            
            ! Skip == and /= and <= and >= operators
            if (i > 1 .and. (result(i-1:i-1) == '=' .or. result(i-1:i-1) == '/' .or. &
                            result(i-1:i-1) == '<' .or. result(i-1:i-1) == '>')) then
                pos = i + 1
                cycle
            end if
            
            if (i < len(result) .and. result(i+1:i+1) == '=') then
                pos = i + 2
                cycle
            end if
            
            ! Ensure space before and after =
            if (i > 1 .and. result(i-1:i-1) /= ' ') then
                result = result(1:i-1) // ' ' // result(i:)
                i = i + 1
            end if
            
            if (i < len(result) .and. result(i+1:i+1) /= ' ') then
                result = result(1:i) // ' ' // result(i+1:)
            end if
            
            pos = i + 2
        end do
        
        code = result
        
    end subroutine align_assignment_operators
    
    subroutine group_related_code(code)
        character(len=:), allocatable, intent(inout) :: code
        
        ! Add blank lines to separate logical groups
        ! This is a simplified implementation
        call add_logical_blank_lines(code, create_aesthetic_settings())
        
    end subroutine group_related_code
    
    subroutine improve_operator_spacing(code)
        character(len=:), allocatable, intent(inout) :: code
        
        ! Improve spacing around operators
        character(len=:), allocatable :: result
        integer :: i, pos
        character :: op
        
        result = code
        
        ! Common operators to improve spacing for
        pos = 1
        do while (pos <= len(result) - 1)
            op = result(pos:pos)
            
            select case (op)
            case ('+', '-', '*', '/')
                ! Skip ** operator
                if (op == '*' .and. pos < len(result) .and. result(pos+1:pos+1) == '*') then
                    pos = pos + 1
                    cycle
                end if
                
                ! Add spaces around operator if missing
                if (pos > 1 .and. result(pos-1:pos-1) /= ' ') then
                    result = result(1:pos-1) // ' ' // result(pos:)
                    pos = pos + 1
                end if
                
                if (pos < len(result) .and. result(pos+1:pos+1) /= ' ') then
                    result = result(1:pos) // ' ' // result(pos+1:)
                end if
            end select
            
            pos = pos + 1
        end do
        
        code = result
        
    end subroutine improve_operator_spacing
    
    subroutine optimize_line_breaks(code, max_length)
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: max_length
        
        character(len=:), allocatable :: result
        character(len=1000), allocatable :: lines(:)
        integer :: num_lines, i
        logical :: first_line
        
        allocate(lines(10000))
        call split_code_into_lines(code, lines, num_lines)
        
        result = ""
        first_line = .true.
        
        do i = 1, num_lines
            ! Safe trimming
            if (i <= size(lines)) then
                if (len_trim(lines(i)) > max_length) then
                    if (.not. first_line) then
                        result = result // new_line('a')
                    end if
                    call break_fortran_line(lines(i)(1:len_trim(lines(i))), result, max_length)
                    first_line = .false.
                else
                    if (.not. first_line) then
                        result = result // new_line('a')
                    end if
                    if (len_trim(lines(i)) > 0) then
                        result = result // lines(i)(1:len_trim(lines(i)))
                    end if
                    first_line = .false.
                end if
            end if
        end do
        
        code = result
        
    end subroutine optimize_line_breaks
    
    subroutine split_code_into_lines(code, lines, num_lines)
        character(len=*), intent(in) :: code
        character(len=*), intent(out) :: lines(:)
        integer, intent(out) :: num_lines
        
        integer :: i, line_start, current_line
        logical :: found_newline
        
        num_lines = 0
        current_line = 1
        line_start = 1
        found_newline = .false.
        
        ! Initialize all lines to empty
        do i = 1, size(lines)
            lines(i) = ""
        end do
        
        do i = 1, len(code)
            if (code(i:i) == new_line('a')) then
                if (i > line_start) then
                    lines(current_line) = code(line_start:i-1)
                else
                    lines(current_line) = ""
                end if
                num_lines = current_line
                current_line = current_line + 1
                line_start = i + 1
                found_newline = .true.
                if (current_line > size(lines)) exit
            end if
        end do
        
        ! Handle last line or single line without newline
        if (line_start <= len(code) .and. current_line <= size(lines)) then
            lines(current_line) = code(line_start:)
            num_lines = current_line
        else if (.not. found_newline .and. len(code) > 0) then
            ! Single line with no newline
            lines(1) = code
            num_lines = 1
        end if
        
    end subroutine split_code_into_lines
    
    subroutine break_fortran_line(line, result, max_length)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(inout) :: result
        integer, intent(in) :: max_length
        
        character(len=:), allocatable :: remaining, current_segment
        integer :: break_pos, i, leading_spaces
        logical :: found_break, in_string
        character :: quote_char
        
        ! Count leading spaces
        leading_spaces = 0
        do i = 1, len(line)
            if (line(i:i) == ' ') then
                leading_spaces = leading_spaces + 1
            else
                exit
            end if
        end do
        
        remaining = line
        
        do while (len_trim(remaining) > 0)
            if (len_trim(remaining) <= max_length) then
                result = result // remaining
                exit
            end if
            
            ! Find a good break point
            found_break = .false.
            in_string = .false.
            quote_char = ' '
            
            ! Scan backwards from max_length to find a break point
            do i = min(max_length, len_trim(remaining)), 1, -1
                ! Track string literals
                if (remaining(i:i) == '"' .or. remaining(i:i) == "'") then
                    if (.not. in_string) then
                        in_string = .true.
                        quote_char = remaining(i:i)
                    else if (remaining(i:i) == quote_char) then
                        in_string = .false.
                    end if
                end if
                
                ! Don't break inside strings
                if (in_string) cycle
                
                ! Good break points (after these characters)
                if (i < len_trim(remaining) .and. &
                    (remaining(i:i) == ',' .or. &
                     remaining(i:i) == '+' .or. &
                     remaining(i:i) == '-' .or. &
                     remaining(i:i) == '*' .or. &
                     remaining(i:i) == '/' .or. &
                     remaining(i:i) == ')')) then
                    
                    ! Don't break ** operator
                    if (remaining(i:i) == '*' .and. i < len(remaining) .and. remaining(i+1:i+1) == '*') then
                        cycle
                    end if
                    
                    current_segment = remaining(1:i) // ' &'
                    remaining = repeat(' ', leading_spaces + 4) // adjustl(remaining(i+1:))
                    found_break = .true.
                    exit
                end if
                
                ! Break at spaces (before the space)
                if (remaining(i:i) == ' ' .and. i > 1 .and. i < len_trim(remaining)) then
                    current_segment = remaining(1:i-1) // ' &'
                    remaining = repeat(' ', leading_spaces + 4) // adjustl(remaining(i+1:))
                    found_break = .true.
                    exit
                end if
            end do
            
            ! If no good break found, force break
            if (.not. found_break) then
                if (max_length-2 > 0) then
                    current_segment = remaining(1:max_length-2) // ' &'
                    remaining = repeat(' ', leading_spaces + 4) // adjustl(remaining(max_length-1:))
                else
                    current_segment = remaining
                    remaining = ""
                end if
            end if
            
            result = result // trim(current_segment)
            if (len_trim(remaining) > 0) then
                result = result // new_line('a')
            end if
        end do
        
    end subroutine break_fortran_line
    
    ! Helper functions
    function should_add_blank_line_before(current_line, previous_line) result(needs_blank)
        character(len=*), intent(in) :: current_line, previous_line
        logical :: needs_blank
        
        character(len=:), allocatable :: curr_trim, prev_trim
        
        needs_blank = .false.
        curr_trim = trim(current_line)
        prev_trim = trim(previous_line)
        
        if (len(prev_trim) == 0) return
        
        ! Add blank line before procedure definitions
        if (index(curr_trim, 'subroutine ') == 1 .or. &
            index(curr_trim, 'function ') == 1 .or. &
            index(curr_trim, 'pure function ') == 1 .or. &
            index(curr_trim, 'elemental function ') == 1) then
            if (index(prev_trim, 'end ') /= 1 .and. prev_trim /= 'contains') then
                needs_blank = .true.
            end if
        end if
        
        ! Add blank line before type definitions
        if (index(curr_trim, 'type ') == 1 .or. index(curr_trim, 'type,') == 1) then
            needs_blank = .true.
        end if
        
        ! Add blank line before major control structures
        if (index(curr_trim, 'do ') == 1 .or. index(curr_trim, 'if ') == 1) then
            if (index(prev_trim, 'integer') /= 1 .and. index(prev_trim, 'real') /= 1 .and. &
                index(prev_trim, 'logical') /= 1 .and. index(prev_trim, 'character') /= 1) then
                needs_blank = .true.
            end if
        end if
        
    end function should_add_blank_line_before
    
    function check_case_consistency(code) result(consistent)
        character(len=*), intent(in) :: code
        logical :: consistent
        
        ! Simplified check - assume consistent if lowercase keywords found
        consistent = index(code, 'program') > 0 .or. index(code, 'module') > 0 .or. &
                    index(code, 'subroutine') > 0 .or. index(code, 'function') > 0
        
    end function check_case_consistency
    
    function check_spacing_consistency(code) result(consistent)
        character(len=*), intent(in) :: code
        logical :: consistent
        
        ! Simplified check - assume consistent if some good spacing found
        consistent = index(code, ' = ') > 0 .or. index(code, ' :: ') > 0
        
    end function check_spacing_consistency
    
    subroutine analyze_line_indentation(line, expected_indent, actual_indent, &
                                       in_procedure, in_if, in_do)
        character(len=*), intent(in) :: line
        integer, intent(inout) :: expected_indent
        integer, intent(out) :: actual_indent
        logical, intent(inout) :: in_procedure, in_if, in_do
        
        character(len=:), allocatable :: trimmed_line
        integer :: i
        
        trimmed_line = trim(line)
        
        ! Count leading spaces
        actual_indent = 0
        do i = 1, len(line)
            if (line(i:i) == ' ') then
                actual_indent = actual_indent + 1
            else
                exit
            end if
        end do
        
        ! Update context and expected indentation
        if (len(trimmed_line) == 0) return
        
        ! Check for procedure start/end
        if (index(trimmed_line, 'subroutine ') == 1 .or. &
            index(trimmed_line, 'function ') == 1) then
            in_procedure = .true.
            expected_indent = 0
        else if (index(trimmed_line, 'end subroutine') == 1 .or. &
                index(trimmed_line, 'end function') == 1) then
            in_procedure = .false.
            expected_indent = 0
        else if (in_procedure) then
            expected_indent = 4
        end if
        
        ! Adjust for nested constructs
        if (index(trimmed_line, 'if ') == 1) then
            in_if = .true.
        else if (index(trimmed_line, 'end if') == 1) then
            in_if = .false.
        end if
        
        if (index(trimmed_line, 'do ') == 1) then
            in_do = .true.
        else if (index(trimmed_line, 'end do') == 1) then
            in_do = .false.
        end if
        
        if (in_if) expected_indent = expected_indent + 4
        if (in_do) expected_indent = expected_indent + 4
        
    end subroutine analyze_line_indentation
    
    function count_lines(code) result(count)
        character(len=*), intent(in) :: code
        integer :: count, i
        
        count = 1
        do i = 1, len(code)
            if (code(i:i) == new_line('a')) count = count + 1
        end do
    end function count_lines
    
    function count_blank_lines(code) result(count)
        character(len=*), intent(in) :: code
        integer :: count, i
        logical :: line_is_blank, prev_was_newline
        
        count = 0
        line_is_blank = .true.
        prev_was_newline = .true.
        
        do i = 1, len(code)
            if (code(i:i) == new_line('a')) then
                if (line_is_blank .and. .not. prev_was_newline) then
                    count = count + 1
                end if
                line_is_blank = .true.
                prev_was_newline = .true.
            else if (code(i:i) /= ' ' .and. code(i:i) /= char(9)) then
                line_is_blank = .false.
                prev_was_newline = .false.
            else
                prev_was_newline = .false.
            end if
        end do
    end function count_blank_lines
    
    function count_long_lines(code, max_length) result(count)
        character(len=*), intent(in) :: code
        integer, intent(in) :: max_length
        integer :: count, line_length, i
        
        count = 0
        line_length = 0
        
        do i = 1, len(code)
            if (code(i:i) == new_line('a')) then
                if (line_length > max_length) count = count + 1
                line_length = 0
            else
                line_length = line_length + 1
            end if
        end do
        
        ! Check last line
        if (line_length > max_length) count = count + 1
    end function count_long_lines
    
    ! Quality metrics methods
    subroutine calculate_overall_score(this)
        class(format_quality_t), intent(inout) :: this
        
        ! Weighted average of quality aspects
        this%overall_score = (this%indentation_score * 0.25_dp + &
                             this%spacing_score * 0.20_dp + &
                             this%readability_score * 0.25_dp + &
                             this%structure_score * 0.15_dp + &
                             this%consistency_score * 0.10_dp + &
                             this%line_length_score * 0.05_dp)
                             
    end subroutine calculate_overall_score
    
    subroutine generate_recommendations(this)
        class(format_quality_t), intent(inout) :: this
        
        character(len=50), allocatable :: temp_recommendations(:)
        integer :: count
        
        count = 0
        allocate(temp_recommendations(10))
        
        if (this%indentation_score < 8.0_dp) then
            count = count + 1
            temp_recommendations(count) = "Improve indentation consistency (use 4 spaces)"
        end if
        
        if (this%spacing_score < 8.0_dp) then
            count = count + 1
            temp_recommendations(count) = "Add spaces around operators (=, +, -, *, /)"
        end if
        
        if (this%readability_score < 7.0_dp) then
            count = count + 1
            temp_recommendations(count) = "Add blank lines to separate logical sections"
        end if
        
        if (this%line_length_score < 8.0_dp) then
            count = count + 1
            temp_recommendations(count) = "Break long lines (keep under 88 characters)"
        end if
        
        if (this%structure_score < 8.0_dp) then
            count = count + 1
            temp_recommendations(count) = "Add 'implicit none' and proper end statements"
        end if
        
        if (count == 0) then
            count = 1
            temp_recommendations(count) = "Code quality is excellent!"
        end if
        
        allocate(character(len=50) :: this%recommendations(count))
        this%recommendations = temp_recommendations(1:count)
        
    end subroutine generate_recommendations
    
    subroutine print_report(this)
        class(format_quality_t), intent(in) :: this
        integer :: i
        
        print *, "=== Format Quality Report ==="
        print *, "Overall Score:    ", this%overall_score, "/10"
        print *, ""
        print *, "Detailed Scores:"
        print *, "  Indentation:   ", this%indentation_score, "/10"
        print *, "  Spacing:       ", this%spacing_score, "/10"
        print *, "  Readability:   ", this%readability_score, "/10"
        print *, "  Structure:     ", this%structure_score, "/10"
        print *, "  Consistency:   ", this%consistency_score, "/10"
        print *, "  Line Length:   ", this%line_length_score, "/10"
        print *, ""
        print *, "Metrics:"
        print *, "  Total Lines:   ", this%total_lines
        print *, "  Blank Lines:   ", this%blank_lines
        print *, "  Long Lines:    ", this%long_lines
        print *, ""
        print *, "Recommendations:"
        do i = 1, size(this%recommendations)
            print *, "  ", i, ". ", trim(this%recommendations(i))
        end do
        
    end subroutine print_report
    
end module fluff_format_quality