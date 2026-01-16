module fluff_formatter
    ! Code formatting engine
    use fluff_ast
    use fluff_format_quality
    use fluff_user_feedback
    use fluff_formatter_style, only: detect_style_guide_from_source, &
                                     configure_clean_style, configure_standard_style, &
                                     configure_modern_style, configure_hpc_style, &
                                     configure_custom_style
    use fluff_formatter_validation, only: validate_format_with_emit, &
                                          compare_semantics_with_emit, &
                                          analyze_format_diff_with_tokens
    use fortfront, only: format_options_t
    implicit none
    private

    ! Formatter engine type
    type, public :: formatter_engine_t
        logical :: is_initialized = .false.
        type(format_options_t) :: options
        character(len=:), allocatable :: current_style_guide
        type(aesthetic_settings_t) :: aesthetic_settings
        logical :: enable_quality_improvements = .true.
    contains
        procedure :: initialize => formatter_initialize
        procedure :: format_file => formatter_format_file
        procedure :: format_ast => formatter_format_ast
        procedure :: format_source => formatter_format_source
        procedure :: format_range => formatter_format_range
        procedure :: set_style_guide => formatter_set_style_guide
        procedure :: configure_style => formatter_configure_style
        procedure :: detect_style_guide => formatter_detect_style_guide
        procedure :: validate_format => formatter_validate_format
        procedure :: compare_semantics => formatter_compare_semantics
        procedure :: analyze_format_diff => formatter_analyze_format_diff
        procedure :: assess_quality => formatter_assess_quality
        procedure :: format_with_quality => formatter_format_with_quality
        procedure :: set_aesthetic_settings => formatter_set_aesthetic_settings
        procedure :: collect_user_feedback => formatter_collect_user_feedback
        procedure :: format_with_feedback => formatter_format_with_feedback
    end type formatter_engine_t

    ! Public procedures
    public :: create_formatter_engine

contains

    ! Create a new formatter engine
    function create_formatter_engine() result(formatter)
        type(formatter_engine_t) :: formatter
        call formatter%initialize()
    end function create_formatter_engine

    ! Initialize formatter engine
    subroutine formatter_initialize(this)
        class(formatter_engine_t), intent(inout) :: this

        this%is_initialized = .true.

        ! Set default style guide and options
        this%current_style_guide = "clean"
        call configure_clean_style(this%options)

        ! Initialize aesthetic settings
        this%aesthetic_settings = create_aesthetic_settings()
        this%enable_quality_improvements = .true.

    end subroutine formatter_initialize

    ! Format a file
    subroutine formatter_format_file(this, filename, formatted_code, error_msg)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: formatted_code
        character(len=:), allocatable, intent(out) :: error_msg

        ! Read file and format using fortfront
        integer :: unit, iostat
        integer :: file_size
        character(len=:), allocatable :: source_code

        error_msg = ""

        open (newunit=unit, file=filename, status="old", action="read", &
              access="stream", form="unformatted", iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Could not open file: "//filename
            return
        end if

        inquire (unit=unit, size=file_size, iostat=iostat)
        if (iostat /= 0) then
            close (unit)
            error_msg = "Could not stat file: "//filename
            return
        end if

        allocate (character(len=file_size) :: source_code)
        if (file_size > 0) then
            read (unit, iostat=iostat) source_code
            if (iostat /= 0) then
                close (unit)
                error_msg = "Could not read file: "//filename
                return
            end if
        end if

        close (unit)

        ! Format the source code
        call this%format_source(source_code, formatted_code, error_msg)

    end subroutine formatter_format_file

    ! Format an AST
    subroutine formatter_format_ast(this, ast_ctx, formatted_code)
        use codegen_api, only: get_indent_config, get_line_length_config, &
                               get_type_standardization, set_indent_config, &
                               set_line_length_config, set_type_standardization
        use fortfront, only: emit_fortran
        class(formatter_engine_t), intent(in) :: this
        type(fluff_ast_context_t), intent(inout) :: ast_ctx
        character(len=:), allocatable, intent(out) :: formatted_code

        character(len=:), allocatable :: temp_code
        character(len=:), allocatable :: improved_code
        character(len=:), allocatable :: next_code
        integer :: saved_indent_size
        integer :: saved_line_length
        character(len=1) :: saved_indent_char
        logical :: saved_standardize_types
        integer :: iter
        logical :: changed

        ! Use fortfront's formatting
        if (ast_ctx%is_initialized) then
            call get_indent_config(saved_indent_size, saved_indent_char)
            call get_line_length_config(saved_line_length)
            call get_type_standardization(saved_standardize_types)

            call set_indent_config(this%options%indent_size, this%options%indent_char)
            call set_line_length_config(this%options%line_length)
            call set_type_standardization(this%options%standardize_types)

            call emit_fortran(ast_ctx%arena, ast_ctx%root_index, temp_code)

            call set_indent_config(saved_indent_size, saved_indent_char)
            call set_line_length_config(saved_line_length)
            call set_type_standardization(saved_standardize_types)

            if (this%enable_quality_improvements) then
                improved_code = temp_code
                do iter = 1, 2
                    call apply_aesthetic_improvements(improved_code, next_code, &
                                                      this%aesthetic_settings)
                    changed = next_code /= improved_code
                    improved_code = next_code
                    if (.not. changed) exit
                end do
                formatted_code = improved_code
            else
                formatted_code = temp_code
            end if
        else
            formatted_code = ""
        end if

    end subroutine formatter_format_ast

    ! Format source code
    subroutine formatter_format_source(this, source_code, formatted_code, error_msg)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: formatted_code
        character(len=:), allocatable, intent(out) :: error_msg

        type(fluff_ast_context_t) :: ast_ctx
        character(len=:), allocatable :: current_code
        character(len=:), allocatable :: next_code
        character(len=:), allocatable :: prev_code
        integer :: iter

        error_msg = ""
        current_code = source_code
        prev_code = ""

        do iter = 1, 4
            call ast_ctx%from_source(current_code, error_msg)
            if (error_msg /= "") then
                call report_fortfront_failure("tooling_load_ast_from_string", error_msg)
            end if

            call this%format_ast(ast_ctx, next_code)
            if (next_code == current_code) exit
            if (next_code == prev_code) then
                current_code = next_code
                exit
            end if
            prev_code = current_code
            current_code = next_code
        end do

        formatted_code = current_code

    end subroutine formatter_format_source

    ! Format a specific range of lines
    subroutine formatter_format_range(this, ast_ctx, start_line, end_line, &
                                      formatted_code)
        use codegen_api, only: get_indent_config, get_line_length_config, &
                               get_type_standardization, set_indent_config, &
                               set_line_length_config, set_type_standardization
        use fortfront, only: emit_fortran
        class(formatter_engine_t), intent(in) :: this
        type(fluff_ast_context_t), intent(inout) :: ast_ctx
        integer, intent(in) :: start_line, end_line
        character(len=:), allocatable, intent(out) :: formatted_code

        ! Implement range-specific formatting by filtering lines
        character(len=:), allocatable :: full_formatted
        character(len=1000) :: lines(10000)
        integer :: num_lines, i, line_start, line_end
        integer :: saved_indent_size
        integer :: saved_line_length
        character(len=1) :: saved_indent_char
        logical :: saved_standardize_types

        if (ast_ctx%is_initialized) then
            ! First format the entire file
            call get_indent_config(saved_indent_size, saved_indent_char)
            call get_line_length_config(saved_line_length)
            call get_type_standardization(saved_standardize_types)

            call set_indent_config(this%options%indent_size, this%options%indent_char)
            call set_line_length_config(this%options%line_length)
            call set_type_standardization(this%options%standardize_types)

            call emit_fortran(ast_ctx%arena, ast_ctx%root_index, full_formatted)

            call set_indent_config(saved_indent_size, saved_indent_char)
            call set_line_length_config(saved_line_length)
            call set_type_standardization(saved_standardize_types)

            ! Split into lines and extract the range
            call split_lines_simple(full_formatted, lines, num_lines)

            formatted_code = ""
            line_start = max(1, start_line)
            line_end = min(num_lines, end_line)

            do i = line_start, line_end
                if (i > 1) formatted_code = formatted_code//new_line('a')
                formatted_code = formatted_code//trim(lines(i))
            end do
        else
            formatted_code = ""
        end if

    end subroutine formatter_format_range

    ! Set the active style guide
    subroutine formatter_set_style_guide(this, style_name)
        class(formatter_engine_t), intent(inout) :: this
        character(len=*), intent(in) :: style_name

        this%current_style_guide = style_name

        ! Configure options based on style guide
        select case (trim(style_name))
        case ("clean")
            call configure_clean_style(this%options)
        case ("standard")
            call configure_standard_style(this%options)
        case ("modern")
            call configure_modern_style(this%options)
        case ("hpc")
            call configure_hpc_style(this%options)
        case ("custom")
            call configure_custom_style(this%options)
        case default
            call configure_clean_style(this%options)
        end select

    end subroutine formatter_set_style_guide

    ! Configure specific style options
    subroutine formatter_configure_style(this, option_name, option_value)
        class(formatter_engine_t), intent(inout) :: this
        character(len=*), intent(in) :: option_name, option_value

        integer :: iostat
        integer :: value

        select case (trim(option_name))
        case ("indent_size")
            read (option_value, *, iostat=iostat) value
            if (iostat == 0 .and. value > 0) then
                this%options%indent_size = value
                this%aesthetic_settings%indent_size = value
            end if
        case ("line_length")
            read (option_value, *, iostat=iostat) value
            if (iostat == 0 .and. value > 0) then
                this%options%line_length = value
                this%aesthetic_settings%max_line_length = value
            end if
        case ("use_tabs")
            if (trim(option_value) == "true") then
                this%options%use_tabs = .true.
                this%options%indent_char = achar(9)
            else
                this%options%use_tabs = .false.
                this%options%indent_char = ' '
            end if
        case ("standardize_types")
            if (trim(option_value) == "true") then
                this%options%standardize_types = .true.
            else
                this%options%standardize_types = .false.
            end if
        case default
            ! Ignore unknown options for now
        end select

    end subroutine formatter_configure_style

    ! Detect style guide from AST analysis
    subroutine formatter_detect_style_guide(this, source_code, detected_style)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: detected_style

        call detect_style_guide_from_source(source_code, detected_style)

    end subroutine formatter_detect_style_guide

    ! Style configuration helpers
    ! Format validation procedures
    subroutine formatter_validate_format(this, original_code, formatted_code, is_valid)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: original_code, formatted_code
        logical, intent(out) :: is_valid

        call validate_format_with_emit(original_code, formatted_code, is_valid)

    end subroutine formatter_validate_format

    subroutine formatter_compare_semantics(this, code1, code2, are_equivalent)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: code1, code2
        logical, intent(out) :: are_equivalent

        call compare_semantics_with_emit(code1, code2, are_equivalent)

    end subroutine formatter_compare_semantics

    subroutine formatter_analyze_format_diff(this, original, formatted, diff_type)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: original, formatted
        character(len=:), allocatable, intent(out) :: diff_type

        call analyze_format_diff_with_tokens(original, formatted, diff_type)

    end subroutine formatter_analyze_format_diff

    ! Quality assessment procedures
    subroutine formatter_assess_quality(this, source_code, quality)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        type(format_quality_t), intent(out) :: quality

        call assess_format_quality(source_code, quality)

    end subroutine formatter_assess_quality

    subroutine formatter_format_with_quality(this, source_code, formatted_code, &
                                             error_msg, quality)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: formatted_code, error_msg
        type(format_quality_t), intent(out) :: quality

        ! Format the code
        call this%format_source(source_code, formatted_code, error_msg)

        if (error_msg /= "") then
            quality = create_quality_metrics()
            return
        end if

        ! Assess quality of formatted result
        call assess_format_quality(formatted_code, quality)

    end subroutine formatter_format_with_quality

    subroutine formatter_set_aesthetic_settings(this, settings)
        class(formatter_engine_t), intent(inout) :: this
        type(aesthetic_settings_t), intent(in) :: settings

        this%aesthetic_settings = settings

    end subroutine formatter_set_aesthetic_settings

    ! User feedback procedures
    subroutine formatter_collect_user_feedback(this, original_code, formatted_code, &
                                               quality, feedback)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: original_code, formatted_code
        type(format_quality_t), intent(in) :: quality
        type(user_feedback_t), intent(out) :: feedback

        call collect_interactive_feedback(original_code, formatted_code, quality, &
                                          feedback)

    end subroutine formatter_collect_user_feedback

    subroutine formatter_format_with_feedback(this, source_code, formatted_code, &
                                              error_msg, quality, feedback, &
                                              collect_feedback)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: formatted_code, error_msg
        type(format_quality_t), intent(out) :: quality
        type(user_feedback_t), intent(out) :: feedback
        logical, intent(in), optional :: collect_feedback

        logical :: should_collect

        should_collect = .false.
        if (present(collect_feedback)) should_collect = collect_feedback

        ! Format with quality assessment
        call this%format_with_quality(source_code, formatted_code, error_msg, quality)

        if (error_msg /= "") then
            feedback = create_user_feedback()
            return
        end if

        ! Collect user feedback if requested
        if (should_collect) then
            call this%collect_user_feedback(source_code, formatted_code, &
                                            quality, feedback)
        else
            feedback = create_user_feedback()
        end if

    end subroutine formatter_format_with_feedback

    ! Helper subroutine to split text into lines
    subroutine split_lines_simple(text, lines, num_lines)
        character(len=*), intent(in) :: text
        character(len=1000), intent(out) :: lines(:)
        integer, intent(out) :: num_lines

        integer :: i, start_pos, end_pos, newline_pos

        num_lines = 0
        start_pos = 1

        do while (start_pos <= len(text) .and. num_lines < size(lines))
            ! Find next newline
            newline_pos = index(text(start_pos:), new_line('a'))
            if (newline_pos == 0) then
                ! No more newlines, take rest of string
                end_pos = len(text)
            else
                end_pos = start_pos + newline_pos - 2
            end if

            if (end_pos >= start_pos) then
                num_lines = num_lines + 1
                lines(num_lines) = text(start_pos:end_pos)
            end if

            if (newline_pos == 0) exit
            start_pos = start_pos + newline_pos
        end do

    end subroutine split_lines_simple

    subroutine report_fortfront_failure(stage, error_msg)
        character(len=*), intent(in) :: stage
        character(len=*), intent(in) :: error_msg

        print *, "ERROR: fortfront "//trim(stage)//" failed in formatter"
        print *, "Error: ", error_msg
        print *, "File a GitHub issue at https://github.com/fortfront/fortfront"
        error stop "AST parsing required - no fallbacks"
    end subroutine report_fortfront_failure

end module fluff_formatter
