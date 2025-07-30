module fluff_core
    ! Core types and utilities for fluff
    implicit none
    private
    
    ! Version information
    type, public :: fluff_version_t
        integer :: major = 0
        integer :: minor = 1  
        integer :: patch = 0
        character(len=:), allocatable :: pre_release
    contains
        procedure :: to_string => version_to_string
    end type fluff_version_t
    
    ! Core context for fluff operations
    type, public :: fluff_context_t
        character(len=:), allocatable :: name
        type(fluff_version_t) :: version
        logical :: initialized = .false.
    contains
        procedure :: initialize => context_initialize
        procedure :: finalize => context_finalize
    end type fluff_context_t
    
    ! Source location tracking
    type, public :: source_location_t
        integer :: line = 0
        integer :: column = 0
        integer :: byte_offset = 0
    end type source_location_t
    
    ! Source range for diagnostics
    type, public :: source_range_t
        type(source_location_t) :: start
        type(source_location_t) :: end
    contains
        procedure :: is_valid => range_is_valid
    end type source_range_t
    
    ! Public procedures
    public :: get_fluff_version
    public :: create_fluff_context
    
contains
    
    ! Get current fluff version
    function get_fluff_version() result(version)
        type(fluff_version_t) :: version
        ! Version is initialized with default values
    end function get_fluff_version
    
    ! Create a new fluff context
    function create_fluff_context() result(context)
        type(fluff_context_t) :: context
        
        context%name = "fluff"
        context%version = get_fluff_version()
        call context%initialize()
        
    end function create_fluff_context
    
    ! Convert version to string
    function version_to_string(this) result(str)
        class(fluff_version_t), intent(in) :: this
        character(len=:), allocatable :: str
        character(len=20) :: buffer
        
        write(buffer, '(I0,".",I0,".",I0)') this%major, this%minor, this%patch
        str = trim(buffer)
        
        if (allocated(this%pre_release)) then
            str = str // "-" // this%pre_release
        end if
        
    end function version_to_string
    
    ! Initialize context
    subroutine context_initialize(this)
        class(fluff_context_t), intent(inout) :: this
        this%initialized = .true.
    end subroutine context_initialize
    
    ! Finalize context
    subroutine context_finalize(this)
        class(fluff_context_t), intent(inout) :: this
        this%initialized = .false.
    end subroutine context_finalize
    
    ! Check if range is valid
    function range_is_valid(this) result(valid)
        class(source_range_t), intent(in) :: this
        logical :: valid
        
        valid = this%start%line > 0 .and. this%start%column > 0 .and. &
                this%end%line >= this%start%line
                
        if (valid .and. this%end%line == this%start%line) then
            valid = this%end%column >= this%start%column
        end if
        
    end function range_is_valid
    
end module fluff_core