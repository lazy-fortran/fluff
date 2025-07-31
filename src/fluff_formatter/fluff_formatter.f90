module fluff_formatter
    ! Code formatting engine
    use fluff_core
    use fluff_ast
    implicit none
    private
    
    ! Formatter engine type
    type, public :: formatter_engine_t
        logical :: is_initialized = .false.
        type(format_options_t) :: options
    contains
        procedure :: initialize => formatter_initialize
        procedure :: format_file => formatter_format_file
        procedure :: format_ast => formatter_format_ast
        procedure :: format_source => formatter_format_source
        procedure :: format_range => formatter_format_range
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
        
        ! Set default options
        this%options%indent_size = 4
        this%options%line_length = 88
        this%options%use_spaces = .true.
        this%options%style_guide = "standard"
        
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
        class(formatter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: formatted_code
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(fluff_ast_context_t) :: ast_ctx
        
        ! Parse source to AST using fortfront
        ast_ctx = create_ast_context()
        call ast_ctx%from_source(source_code, error_msg)
        if (error_msg /= "") return
        
        ! Format the AST using fortfront's code generation
        call this%format_ast(ast_ctx, formatted_code)
        
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
    
end module fluff_formatter