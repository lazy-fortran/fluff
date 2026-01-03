module fluff_rule_trivia_utils
    use fluff_core, only: source_range_t
    implicit none
    private

    public :: location_buffer_t
    public :: location_buffer_init
    public :: location_buffer_push
    public :: location_buffer_finish
    public :: text_has_space_and_tab

    type :: location_buffer_t
        type(source_range_t), allocatable :: data(:)
        integer :: n = 0
    end type location_buffer_t

contains

    subroutine location_buffer_init(buf)
        type(location_buffer_t), intent(inout) :: buf

        buf%n = 0
        if (allocated(buf%data)) deallocate (buf%data)
        allocate (buf%data(8))
    end subroutine location_buffer_init

    subroutine location_buffer_push(buf, location)
        type(location_buffer_t), intent(inout) :: buf
        type(source_range_t), intent(in) :: location

        type(source_range_t), allocatable :: tmp(:)
        integer :: capacity, new_capacity

        if (.not. allocated(buf%data)) then
            allocate (buf%data(8))
            buf%n = 0
        end if

        capacity = size(buf%data)
        if (buf%n >= capacity) then
            new_capacity = max(1, 2*capacity)
            allocate (tmp(new_capacity))
            if (buf%n > 0) tmp(1:buf%n) = buf%data(1:buf%n)
            call move_alloc(tmp, buf%data)
        end if

        buf%n = buf%n + 1
        buf%data(buf%n) = location
    end subroutine location_buffer_push

    subroutine location_buffer_finish(buf, locations)
        type(location_buffer_t), intent(inout) :: buf
        type(source_range_t), allocatable, intent(out) :: locations(:)

        if (buf%n <= 0) then
            allocate (locations(0))
        else
            allocate (locations(buf%n))
            locations = buf%data(1:buf%n)
        end if

        if (allocated(buf%data)) deallocate (buf%data)
        buf%n = 0
    end subroutine location_buffer_finish

    pure logical function text_has_space_and_tab(text) result(has_both)
        character(len=*), intent(in) :: text

        has_both = (index(text, " ") > 0 .and. index(text, achar(9)) > 0)
    end function text_has_space_and_tab

end module fluff_rule_trivia_utils
