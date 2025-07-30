module fluff_errors
    ! Comprehensive error handling for fluff
    use fluff_core
    implicit none
    private
    
    ! Error codes
    enum, bind(c)
        enumerator :: ERROR_NONE = 0
        enumerator :: ERROR_FILE_NOT_FOUND = 1
        enumerator :: ERROR_PARSE_FAILED = 2
        enumerator :: ERROR_SEMANTIC_FAILED = 3
        enumerator :: ERROR_INVALID_CONFIG = 4
        enumerator :: ERROR_IO_FAILED = 5
        enumerator :: ERROR_INTERNAL = 99
    end enum
    
    ! Export error codes
    public :: ERROR_NONE, ERROR_FILE_NOT_FOUND, ERROR_PARSE_FAILED
    public :: ERROR_SEMANTIC_FAILED, ERROR_INVALID_CONFIG
    public :: ERROR_IO_FAILED, ERROR_INTERNAL
    
    ! Error context
    type, public :: error_context_t
        integer :: code = ERROR_NONE
        character(len=:), allocatable :: message
        character(len=:), allocatable :: file
        type(source_location_t) :: location
    contains
        procedure :: set => error_set
        procedure :: clear => error_clear
        procedure :: is_error => error_is_error
        procedure :: to_string => error_to_string
    end type error_context_t
    
    ! Error handler
    type, public :: error_handler_t
        type(error_context_t) :: current_error
        logical :: fail_fast = .false.
    contains
        procedure :: handle => handler_handle
        procedure :: reset => handler_reset
    end type error_handler_t
    
    ! Public procedures
    public :: create_error_handler
    public :: error_code_to_string
    
contains
    
    ! Create error handler
    function create_error_handler(fail_fast) result(handler)
        logical, intent(in), optional :: fail_fast
        type(error_handler_t) :: handler
        
        if (present(fail_fast)) then
            handler%fail_fast = fail_fast
        end if
        
    end function create_error_handler
    
    ! Set error
    subroutine error_set(this, code, message, file, location)
        class(error_context_t), intent(inout) :: this
        integer, intent(in) :: code
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: file
        type(source_location_t), intent(in), optional :: location
        
        this%code = code
        this%message = message
        
        if (present(file)) then
            this%file = file
        end if
        
        if (present(location)) then
            this%location = location
        end if
        
    end subroutine error_set
    
    ! Clear error
    subroutine error_clear(this)
        class(error_context_t), intent(inout) :: this
        
        this%code = ERROR_NONE
        if (allocated(this%message)) deallocate(this%message)
        if (allocated(this%file)) deallocate(this%file)
        this%location%line = 0
        this%location%column = 0
        
    end subroutine error_clear
    
    ! Check if error
    function error_is_error(this) result(is_error)
        class(error_context_t), intent(in) :: this
        logical :: is_error
        
        is_error = this%code /= ERROR_NONE
        
    end function error_is_error
    
    ! Convert error to string
    function error_to_string(this) result(str)
        class(error_context_t), intent(in) :: this
        character(len=:), allocatable :: str
        
        character(len=200) :: buffer
        
        if (this%code == ERROR_NONE) then
            str = "No error"
            return
        end if
        
        write(buffer, '(A,": ",A)') error_code_to_string(this%code), this%message
        str = trim(buffer)
        
        if (allocated(this%file)) then
            write(buffer, '(A," in file ",A)') trim(str), this%file
            str = trim(buffer)
            
            if (this%location%line > 0) then
                write(buffer, '(A," at line ",I0,":",I0)') trim(str), &
                    this%location%line, this%location%column
                str = trim(buffer)
            end if
        end if
        
    end function error_to_string
    
    ! Handle error
    subroutine handler_handle(this, error_ctx)
        class(error_handler_t), intent(inout) :: this
        type(error_context_t), intent(in) :: error_ctx
        
        this%current_error = error_ctx
        
        if (this%fail_fast .and. error_ctx%is_error()) then
            print *, "Error: ", error_ctx%to_string()
            error stop
        end if
        
    end subroutine handler_handle
    
    ! Reset handler
    subroutine handler_reset(this)
        class(error_handler_t), intent(inout) :: this
        
        call this%current_error%clear()
        
    end subroutine handler_reset
    
    ! Convert error code to string
    function error_code_to_string(code) result(str)
        integer, intent(in) :: code
        character(len=:), allocatable :: str
        
        select case (code)
        case (ERROR_NONE)
            str = "No error"
        case (ERROR_FILE_NOT_FOUND)
            str = "File not found"
        case (ERROR_PARSE_FAILED)
            str = "Parse failed"
        case (ERROR_SEMANTIC_FAILED)
            str = "Semantic analysis failed"
        case (ERROR_INVALID_CONFIG)
            str = "Invalid configuration"
        case (ERROR_IO_FAILED)
            str = "I/O operation failed"
        case (ERROR_INTERNAL)
            str = "Internal error"
        case default
            str = "Unknown error"
        end select
        
    end function error_code_to_string
    
end module fluff_errors