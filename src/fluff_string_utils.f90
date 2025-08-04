module fluff_string_utils
    ! String utilities module to work around gfortran bugs with allocatable character arrays
    ! This implements the derived type wrapper pattern recommended by the Fortran community
    implicit none
    private
    
    ! String type wrapper - this avoids gfortran segfaults with deferred-length allocatable arrays
    type, public :: string_t
        character(len=:), allocatable :: str
    contains
        procedure :: set => string_set
        procedure :: get => string_get
        procedure :: length => string_length
        procedure :: is_allocated => string_is_allocated
    end type string_t
    
    ! String array type for returning multiple strings
    type, public :: string_array_t
        type(string_t), allocatable :: items(:)
        integer :: count = 0
    contains
        procedure :: init => string_array_init
        procedure :: resize => string_array_resize
        procedure :: append => string_array_append
        procedure :: get_item => string_array_get_item
        procedure :: set_item => string_array_set_item
        procedure :: to_fixed_array => string_array_to_fixed_array
        procedure :: from_fixed_array => string_array_from_fixed
        procedure :: cleanup => string_array_cleanup
    end type string_array_t
    
    public :: create_string_array
    
contains
    
    ! String methods
    subroutine string_set(this, text)
        class(string_t), intent(inout) :: this
        character(len=*), intent(in) :: text
        
        if (allocated(this%str)) deallocate(this%str)
        this%str = trim(text)
    end subroutine string_set
    
    function string_get(this) result(text)
        class(string_t), intent(in) :: this
        character(len=:), allocatable :: text
        
        if (allocated(this%str)) then
            text = this%str
        else
            text = ""
        end if
    end function string_get
    
    function string_length(this) result(length)
        class(string_t), intent(in) :: this
        integer :: length
        
        if (allocated(this%str)) then
            length = len(this%str)
        else
            length = 0
        end if
    end function string_length
    
    function string_is_allocated(this) result(is_alloc)
        class(string_t), intent(in) :: this
        logical :: is_alloc
        
        is_alloc = allocated(this%str)
    end function string_is_allocated
    
    ! String array methods
    subroutine string_array_init(this, size)
        class(string_array_t), intent(inout) :: this
        integer, intent(in) :: size
        integer :: actual_size
        
        ! Ensure minimum size of 1
        actual_size = max(1, size)
        
        ! Clean up existing items if allocated
        if (allocated(this%items)) then
            call this%cleanup()
        end if
        
        allocate(this%items(actual_size))
        this%count = 0
    end subroutine string_array_init
    
    subroutine string_array_resize(this, new_size)
        class(string_array_t), intent(inout) :: this
        integer, intent(in) :: new_size
        type(string_t), allocatable :: temp(:)
        integer :: i, old_size
        
        ! Validate input
        if (new_size <= 0) return
        
        if (.not. allocated(this%items)) then
            call this%init(new_size)
            return
        end if
        
        old_size = size(this%items)
        if (new_size == old_size) return
        
        ! Save existing data
        allocate(temp(min(old_size, new_size)))
        do i = 1, min(old_size, new_size)
            if (allocated(this%items(i)%str)) then
                allocate(character(len=len(this%items(i)%str)) :: temp(i)%str)
                temp(i)%str = this%items(i)%str
            end if
        end do
        
        ! Clean up old items properly to avoid memory leaks
        do i = 1, old_size
            if (allocated(this%items(i)%str)) then
                deallocate(this%items(i)%str)
            end if
        end do
        deallocate(this%items)
        
        ! Allocate new array
        allocate(this%items(new_size))
        
        ! Restore data
        do i = 1, min(old_size, new_size)
            if (allocated(temp(i)%str)) then
                allocate(character(len=len(temp(i)%str)) :: this%items(i)%str)
                this%items(i)%str = temp(i)%str
            end if
        end do
        
        ! Clean up temp array
        do i = 1, size(temp)
            if (allocated(temp(i)%str)) then
                deallocate(temp(i)%str)
            end if
        end do
        deallocate(temp)
    end subroutine string_array_resize
    
    subroutine string_array_append(this, text)
        class(string_array_t), intent(inout) :: this
        character(len=*), intent(in) :: text
        integer :: new_size
        
        if (.not. allocated(this%items)) then
            call this%init(10)
        end if
        
        if (this%count >= size(this%items)) then
            ! Use growth factor of 1.5 instead of 2 for better memory usage
            new_size = max(size(this%items) + size(this%items)/2, size(this%items) + 1)
            call this%resize(new_size)
        end if
        
        this%count = this%count + 1
        call this%items(this%count)%set(text)
    end subroutine string_array_append
    
    function string_array_get_item(this, index) result(text)
        class(string_array_t), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: text
        
        if (allocated(this%items) .and. index > 0 .and. index <= this%count) then
            text = this%items(index)%get()
        else
            text = ""
        end if
    end function string_array_get_item
    
    subroutine string_array_set_item(this, index, text)
        class(string_array_t), intent(inout) :: this
        integer, intent(in) :: index
        character(len=*), intent(in) :: text
        
        ! Initialize if not allocated
        if (.not. allocated(this%items)) then
            call this%init(max(10, index))
        end if
        
        ! Resize if index is out of bounds
        if (index > size(this%items)) then
            call this%resize(max(index, size(this%items) * 2))
        end if
        
        if (index > 0) then
            call this%items(index)%set(text)
            if (index > this%count) this%count = index
        end if
    end subroutine string_array_set_item
    
    ! Convert to fixed-length character array (for compatibility)
    function string_array_to_fixed_array(this, fixed_len) result(fixed_array)
        class(string_array_t), intent(in) :: this
        integer, intent(in), optional :: fixed_len
        character(len=256), allocatable :: fixed_array(:)
        integer :: i
        
        ! Note: fixed_len parameter is provided for future use but not yet implemented
        ! to avoid changing the fixed array length which might break existing code
        
        if (this%count > 0) then
            allocate(fixed_array(this%count))
            do i = 1, this%count
                if (allocated(this%items(i)%str)) then
                    fixed_array(i) = this%items(i)%get()
                else
                    fixed_array(i) = ""
                end if
            end do
        else
            allocate(fixed_array(0))  ! Return empty array instead of single empty string
        end if
    end function string_array_to_fixed_array
    
    ! Create from fixed-length character array
    subroutine string_array_from_fixed(this, fixed_array)
        class(string_array_t), intent(inout) :: this
        character(len=*), intent(in) :: fixed_array(:)
        integer :: i
        
        call this%init(size(fixed_array))
        do i = 1, size(fixed_array)
            call this%append(trim(fixed_array(i)))
        end do
    end subroutine string_array_from_fixed
    
    subroutine string_array_cleanup(this)
        class(string_array_t), intent(inout) :: this
        integer :: i
        
        if (allocated(this%items)) then
            ! Properly deallocate all string components
            do i = 1, size(this%items)
                if (allocated(this%items(i)%str)) then
                    deallocate(this%items(i)%str)
                end if
            end do
            deallocate(this%items)
        end if
        this%count = 0
    end subroutine string_array_cleanup
    
    ! Factory function for string arrays
    function create_string_array(initial_size) result(arr)
        integer, intent(in), optional :: initial_size
        type(string_array_t) :: arr
        integer :: size_to_use
        
        size_to_use = 10
        if (present(initial_size)) then
            size_to_use = max(1, initial_size)  ! Ensure at least size 1
        end if
        
        call arr%init(size_to_use)
    end function create_string_array
    
end module fluff_string_utils