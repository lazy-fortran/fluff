module fluff_formatter
    ! Code formatting engine
    use fluff_ast
    use fortfront, only: format_options_t
    implicit none
    private
    
    ! Formatter engine type
    type, public :: formatter_engine_t
        logical :: is_initialized = .false.
        type(format_options_t) :: options
        character(len=:), allocatable :: current_style_guide
    contains
        procedure :: initialize => formatter_initialize
        procedure :: format_file => formatter_format_file
        procedure :: format_ast => formatter_format_ast
        procedure :: format_source => formatter_format_source
        procedure :: format_range => formatter_format_range
        procedure :: set_style_guide => formatter_set_style_guide
        procedure :: configure_style => formatter_configure_style
        procedure :: detect_style_guide => formatter_detect_style_guide
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
        
        ! Use fortfront's new formatting API with options
        call transform_lazy_fortran_string_with_format(source_code, formatted_code, error_msg, this%options)
        
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
    
    ! Detect style guide from source code patterns
    subroutine formatter_detect_style_guide(this, source_code, detected_style)
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: detected_style
        
        ! Simple detection based on patterns
        if (index(source_code, "PROGRAM") > 0 .and. index(source_code, "program") == 0) then
            detected_style = "fortran77"
        else if (index(source_code, "class(") > 0 .or. index(source_code, "only:") > 0) then
            detected_style = "modern"
        else if (index(source_code, "use mpi") > 0 .or. index(source_code, "use omp_lib") > 0 .or. &
                index(source_code, "!$omp") > 0) then
            detected_style = "hpc"
        else if (index(source_code, "use iso_fortran_env") > 0) then
            detected_style = "clean"
        else
            detected_style = "standard"
        end if
        
    end subroutine formatter_detect_style_guide
    
    ! Style configuration helpers
    subroutine configure_clean_style(formatter)
        type(formatter_engine_t), intent(inout) :: formatter
        
        ! Core Clean Code + Fortran-specific principles:
        ! - 4 spaces indentation (no tabs)
        ! - 88 character line limit 
        ! - typename_t convention for derived types
        ! - No polymorphic arrays (use wrapper types)
        ! - Max 3 levels of nesting
        ! - Extend arrays with arr = [arr, new_element] syntax
        ! - Assignment operator (=) MUST be overloaded for derived types 
        !   with allocatable members (prevents gfortran double-free errors)
        ! - Intent MUST always be declared in standard Fortran
        !   (lazy Fortran defaults to intent(in) but standard requires explicit)
        ! - Prefer simple and elegant solutions
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
    
end module fluff_formatter