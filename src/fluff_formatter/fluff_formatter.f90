module fluff_formatter
    ! Code formatting engine
    use fluff_ast
    use fluff_format_quality
    use fluff_user_feedback
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
        call configure_clean_style(this)
        
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
        
        ! TODO: Read file and format
        formatted_code = ""
        error_msg = ""
        
    end subroutine formatter_format_file
    
    ! Format an AST
    subroutine formatter_format_ast(this, ast_ctx, formatted_code)
        use fortfront, only: emit_fortran
        class(formatter_engine_t), intent(in) :: this
        type(fluff_ast_context_t), intent(inout) :: ast_ctx
        character(len=:), allocatable, intent(out) :: formatted_code
        
        ! Use fortfront's formatting
        if (ast_ctx%is_initialized) then
            call emit_fortran(ast_ctx%arena, ast_ctx%root_index, formatted_code)
        else
            formatted_code = ""
        end if
        
    end subroutine formatter_format_ast
    
    ! Format source code
    subroutine formatter_format_source(this, source_code, formatted_code, error_msg)
        use fortfront, only: transform_lazy_fortran_string_with_format
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: formatted_code
        character(len=:), allocatable, intent(out) :: error_msg
        
        character(len=:), allocatable :: temp_code
        
        ! Use fortfront's new formatting API with options
        call transform_lazy_fortran_string_with_format(source_code, temp_code, error_msg, this%options)
        
        if (error_msg /= "") then
            print *, "ERROR: fortfront transform_lazy_fortran failed in formatter!"
            print *, "Error: ", error_msg
            print *, "File a GitHub issue at https://github.com/fortfront/fortfront"
            error stop "AST parsing required - no fallbacks!"
        end if
        
        ! Apply aesthetic improvements if enabled
        if (this%enable_quality_improvements) then
            call apply_aesthetic_improvements(temp_code, formatted_code, this%aesthetic_settings)
        else
            formatted_code = temp_code
        end if
        
    end subroutine formatter_format_source
    
    ! Format a specific range of lines
    subroutine formatter_format_range(this, ast_ctx, start_line, end_line, formatted_code)
        use fortfront, only: emit_fortran
        class(formatter_engine_t), intent(in) :: this
        type(fluff_ast_context_t), intent(inout) :: ast_ctx
        integer, intent(in) :: start_line, end_line
        character(len=:), allocatable, intent(out) :: formatted_code
        
        ! For now, just format the whole file
        ! TODO: Implement range-specific formatting
        if (ast_ctx%is_initialized) then
            call emit_fortran(ast_ctx%arena, ast_ctx%root_index, formatted_code)
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
            call configure_clean_style(this)
        case ("standard")
            call configure_standard_style(this)
        case ("modern")
            call configure_modern_style(this)
        case ("hpc")
            call configure_hpc_style(this)
        case ("custom")
            call configure_custom_style(this)
        case default
            call configure_clean_style(this)  ! Default to clean style
        end select
        
    end subroutine formatter_set_style_guide
    
    ! Configure specific style options
    subroutine formatter_configure_style(this, option_name, option_value)
        class(formatter_engine_t), intent(inout) :: this
        character(len=*), intent(in) :: option_name, option_value
        
        select case (trim(option_name))
        case ("indent_size")
            read(option_value, *) this%options%indent_size
        case ("line_length")
            ! Note: format_options_t may not have line_length field
            ! This is a placeholder for future enhancement
        case ("use_tabs")
            if (trim(option_value) == "true") then
                this%options%use_tabs = .true.
            else
                this%options%use_tabs = .false.
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
        use fortfront, only: lex_source, parse_tokens, create_ast_arena, &
                             token_t, ast_arena_t, use_statement_node, &
                             derived_type_node, interface_block_node
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: detected_style
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg
        integer :: prog_index, i
        logical :: has_class_types, has_modules, has_interfaces
        logical :: has_mpi, has_openmp, has_iso_env
        
        ! Parse source code
        call lex_source(source_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "ERROR: fortfront lex_source failed in formatter style detection!"
            print *, "Error: ", error_msg
            print *, "File a GitHub issue at https://github.com/fortfront/fortfront"
            error stop "AST parsing required - no fallbacks!"
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "ERROR: fortfront parse_tokens failed in formatter style detection!"
            print *, "Error: ", error_msg
            print *, "File a GitHub issue at https://github.com/fortfront/fortfront"
            error stop "AST parsing required - no fallbacks!"
        end if
        
        ! Analyze AST for style indicators
        has_class_types = .false.
        has_modules = .false.
        has_interfaces = .false.
        has_mpi = .false.
        has_openmp = .false.
        has_iso_env = .false.
        
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                class is (derived_type_node)
                    has_class_types = .true.
                class is (interface_block_node)
                    has_interfaces = .true.
                class is (use_statement_node)
                    if (node%module_name == "mpi" .or. node%module_name == "mpi_f08") then
                        has_mpi = .true.
                    else if (node%module_name == "omp_lib") then
                        has_openmp = .true.
                    else if (node%module_name == "iso_fortran_env") then
                        has_iso_env = .true.
                    end if
                end select
                
                ! Check for module definitions
                if (arena%entries(i)%node_type == "module_node") then
                    has_modules = .true.
                end if
            end if
        end do
        
        ! Determine style based on AST analysis
        if (has_mpi .or. has_openmp) then
            detected_style = "hpc"
        else if (has_class_types .or. has_interfaces) then
            detected_style = "modern"
        else if (has_iso_env .and. has_modules) then
            detected_style = "clean"
        else if (.not. has_modules .and. .not. has_interfaces) then
            detected_style = "fortran77"
        else
            detected_style = "standard"
        end if
        
    end subroutine formatter_detect_style_guide
    
    ! Style configuration helpers
    subroutine configure_clean_style(formatter)
        type(formatter_engine_t), intent(inout) :: formatter
        
        ! Core Clean Code + PEP 8 + Black + Fortran-specific principles:
        
        ! === LAYOUT & FORMATTING (from PEP 8 + Black) ===
        ! - 4 spaces indentation (no tabs)
        ! - 88 character line limit (Black standard)
        ! - 2 blank lines before top-level procedures
        ! - 1 blank line between methods within modules
        ! - Blank lines to separate logical sections (sparingly)
        
        ! === WHITESPACE (from PEP 8) ===
        ! - No trailing whitespace anywhere
        ! - No whitespace inside parentheses: func(a, b) not func( a, b )
        ! - No whitespace before commas: a, b, c not a , b , c
        ! - Single space after commas: a, b, c not a,b,c
        ! - Spaces around binary operators: a + b not a+b
        ! - Trailing commas in multiline constructs (from Black)
        
        ! === NAMING (snake_case only, no CamelCase) ===
        ! - typename_t convention for derived types
        ! - snake_case for variables, procedures, modules
        ! - SCREAMING_SNAKE_CASE for constants
        ! - No single character names except loop counters (i, j, k)
        
        ! === STRINGS & COMMENTS ===
        ! - Consistent quote style (prefer double quotes like Black)
        ! - Comments are complete sentences with proper capitalization
        ! - Use inline comments sparingly
        
        ! === FORTRAN-SPECIFIC CRITICAL RULES ===
        ! - Assignment operator (=) MUST be overloaded for derived types 
        !   with allocatable members (prevents gfortran double-free errors)
        ! - Intent MUST always be declared in standard Fortran
        !   (lazy Fortran defaults to intent(in) but standard requires explicit)
        ! - No polymorphic arrays (use wrapper types)
        ! - Max 3 levels of nesting for readability
        ! - Extend arrays with arr = [arr, new_element] syntax
        
        ! === MODERN FORTRAN BEST PRACTICES ===
        ! - Always use 'only:' clause in use statements
        ! - Group imports: intrinsic modules, external libraries, local modules
        ! - Prefer 'pure' and 'elemental' procedures when possible
        ! - Prefer 'allocatable' over 'pointer'
        ! - Use 'associate' constructs for complex expressions
        ! - Always use 'stat=' and 'errmsg=' for error handling
        ! - Always deallocate what you allocate
        ! - Use dummy argument associate blocks to avoid warnings
        
        formatter%options%indent_size = 4
        formatter%options%use_tabs = .false.
        formatter%options%indent_char = ' '
        formatter%options%standardize_types = .false.
        
    end subroutine configure_clean_style
    
    subroutine configure_standard_style(formatter)
        type(formatter_engine_t), intent(inout) :: formatter
        
        formatter%options%indent_size = 4
        formatter%options%use_tabs = .false.
        formatter%options%indent_char = ' '
        formatter%options%standardize_types = .true.
        
    end subroutine configure_standard_style
    
    subroutine configure_modern_style(formatter)
        type(formatter_engine_t), intent(inout) :: formatter
        
        formatter%options%indent_size = 4
        formatter%options%use_tabs = .false.
        formatter%options%indent_char = ' '
        formatter%options%standardize_types = .false.
        
    end subroutine configure_modern_style
    
    subroutine configure_hpc_style(formatter)
        type(formatter_engine_t), intent(inout) :: formatter
        
        formatter%options%indent_size = 2  ! Compact for HPC
        formatter%options%use_tabs = .false.
        formatter%options%indent_char = ' '
        formatter%options%standardize_types = .true.  ! Explicit precision
        
    end subroutine configure_hpc_style
    
    subroutine configure_custom_style(formatter)
        type(formatter_engine_t), intent(inout) :: formatter
        
        ! Start with clean style defaults, allow customization
        formatter%options%indent_size = 4
        formatter%options%use_tabs = .false.
        formatter%options%indent_char = ' '
        formatter%options%standardize_types = .false.
        
    end subroutine configure_custom_style
    
    ! Format validation procedures
    subroutine formatter_validate_format(this, original_code, formatted_code, is_valid)
        use fortfront, only: transform_lazy_fortran_string_with_format
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: original_code, formatted_code
        logical, intent(out) :: is_valid
        
        character(len=:), allocatable :: original_normalized, formatted_normalized
        character(len=:), allocatable :: error_msg1, error_msg2
        
        ! Normalize both codes by formatting them identically
        call transform_lazy_fortran_string_with_format(original_code, original_normalized, error_msg1, this%options)
        call transform_lazy_fortran_string_with_format(formatted_code, formatted_normalized, error_msg2, this%options)
        
        ! If either normalization failed, validation fails
        if (error_msg1 /= "" .or. error_msg2 /= "") then
            print *, "ERROR: fortfront transform_lazy_fortran failed in formatter validation!"
            print *, "Error1: ", error_msg1
            print *, "Error2: ", error_msg2
            print *, "File a GitHub issue at https://github.com/fortfront/fortfront"
            error stop "AST parsing required - no fallbacks!"
        end if
        
        ! Compare normalized versions (should be identical for semantic preservation)
        is_valid = (original_normalized == formatted_normalized)
        
    end subroutine formatter_validate_format
    
    subroutine formatter_compare_semantics(this, code1, code2, are_equivalent)
        use fortfront, only: transform_lazy_fortran_string_with_format
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: code1, code2
        logical, intent(out) :: are_equivalent
        
        character(len=:), allocatable :: normalized1, normalized2
        character(len=:), allocatable :: error_msg1, error_msg2
        
        ! Normalize both codes by formatting them identically
        call transform_lazy_fortran_string_with_format(code1, normalized1, error_msg1, this%options)
        call transform_lazy_fortran_string_with_format(code2, normalized2, error_msg2, this%options)
        
        ! If either normalization failed, they're not equivalent
        if (error_msg1 /= "" .or. error_msg2 /= "") then
            print *, "ERROR: fortfront transform_lazy_fortran failed in formatter equivalence check!"
            print *, "Error1: ", error_msg1
            print *, "Error2: ", error_msg2
            print *, "File a GitHub issue at https://github.com/fortfront/fortfront"
            error stop "AST parsing required - no fallbacks!"
        end if
        
        ! Remove all whitespace and compare structure
        are_equivalent = (remove_whitespace(normalized1) == remove_whitespace(normalized2))
        
    end subroutine formatter_compare_semantics
    
    subroutine formatter_analyze_format_diff(this, original, formatted, diff_type)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: original, formatted
        character(len=:), allocatable, intent(out) :: diff_type
        
        character(len=:), allocatable :: orig_no_ws, form_no_ws
        logical :: same_structure, same_indentation, same_spacing
        
        ! Remove all whitespace to check structural changes
        orig_no_ws = remove_whitespace(original)
        form_no_ws = remove_whitespace(formatted)
        same_structure = (orig_no_ws == form_no_ws)
        
        ! Check indentation patterns
        same_indentation = check_indentation_similarity(original, formatted)
        
        ! Check spacing patterns
        same_spacing = check_spacing_similarity(original, formatted)
        
        ! Classify the type of differences
        if (same_structure) then
            if (.not. same_indentation) then
                diff_type = "indentation"
            else if (.not. same_spacing) then
                diff_type = "whitespace"
            else
                diff_type = "none"
            end if
        else
            diff_type = "structure"
        end if
        
    end subroutine formatter_analyze_format_diff
    
    ! Helper function to remove all whitespace from code
    function remove_whitespace(code) result(cleaned)
        character(len=*), intent(in) :: code
        character(len=:), allocatable :: cleaned
        integer :: i, j
        
        allocate(character(len=len(code)) :: cleaned)
        j = 0
        
        do i = 1, len(code)
            if (code(i:i) /= ' ' .and. code(i:i) /= char(9) .and. &
                code(i:i) /= char(10) .and. code(i:i) /= char(13)) then
                j = j + 1
                cleaned(j:j) = code(i:i)
            end if
        end do
        
        cleaned = cleaned(1:j)
        
    end function remove_whitespace
    
    ! Helper function to check indentation similarity
    function check_indentation_similarity(code1, code2) result(similar)
        character(len=*), intent(in) :: code1, code2
        logical :: similar
        
        ! Simplified check: count leading spaces in both codes
        integer :: lines1, lines2, indent1, indent2
        integer :: pos1, pos2, line_start1, line_start2
        
        lines1 = count_lines(code1)
        lines2 = count_lines(code2)
        
        if (lines1 /= lines2) then
            similar = .false.
            return
        end if
        
        ! For now, assume similar if same number of lines
        similar = .true.
        
    end function check_indentation_similarity
    
    ! Helper function to check spacing similarity
    function check_spacing_similarity(code1, code2) result(similar)
        character(len=*), intent(in) :: code1, code2
        logical :: similar
        
        ! Simplified: check if operators have consistent spacing
        integer :: equals1, equals2, plus1, plus2
        
        equals1 = count_char_occurrences(code1, '=')
        equals2 = count_char_occurrences(code2, '=')
        plus1 = count_char_occurrences(code1, '+')
        plus2 = count_char_occurrences(code2, '+')
        
        similar = (equals1 == equals2) .and. (plus1 == plus2)
        
    end function check_spacing_similarity
    
    ! Helper function to count lines
    function count_lines(code) result(count)
        character(len=*), intent(in) :: code
        integer :: count, i
        
        count = 1
        do i = 1, len(code)
            if (code(i:i) == new_line('a')) then
                count = count + 1
            end if
        end do
        
    end function count_lines
    
    ! Helper function to count character occurrences
    function count_char_occurrences(code, char) result(count)
        character(len=*), intent(in) :: code
        character, intent(in) :: char
        integer :: count, i
        
        count = 0
        do i = 1, len(code)
            if (code(i:i) == char) then
                count = count + 1
            end if
        end do
        
    end function count_char_occurrences
    
    ! Quality assessment procedures
    subroutine formatter_assess_quality(this, source_code, quality)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        type(format_quality_t), intent(out) :: quality
        
        call assess_format_quality(source_code, quality)
        
    end subroutine formatter_assess_quality
    
    subroutine formatter_format_with_quality(this, source_code, formatted_code, error_msg, quality)
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
    subroutine formatter_collect_user_feedback(this, original_code, formatted_code, quality, feedback)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: original_code, formatted_code
        type(format_quality_t), intent(in) :: quality
        type(user_feedback_t), intent(out) :: feedback
        
        call collect_interactive_feedback(original_code, formatted_code, quality, feedback)
        
    end subroutine formatter_collect_user_feedback
    
    subroutine formatter_format_with_feedback(this, source_code, formatted_code, error_msg, &
                                             quality, feedback, collect_feedback)
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
            call this%collect_user_feedback(source_code, formatted_code, quality, feedback)
        else
            feedback = create_user_feedback()
        end if
        
    end subroutine formatter_format_with_feedback
    
end module fluff_formatter