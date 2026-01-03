module fluff_rule_diagnostic_utils
    use fluff_diagnostics, only: diagnostic_t
    implicit none
    private

    public :: push_diagnostic
    public :: to_lower_ascii

contains

    subroutine push_diagnostic(buffer, count, diagnostic)
        type(diagnostic_t), allocatable, intent(inout) :: buffer(:)
        integer, intent(inout) :: count
        type(diagnostic_t), intent(in) :: diagnostic

        type(diagnostic_t), allocatable :: grown(:)
        integer :: new_size

        if (.not. allocated(buffer)) then
            new_size = max(8, count + 1)
            allocate (buffer(new_size))
        end if

        if (count >= size(buffer)) then
            new_size = max(2*size(buffer), count + 1)
            allocate (grown(new_size))
            if (count > 0) grown(1:count) = buffer(1:count)
            call move_alloc(grown, buffer)
        end if

        count = count + 1
        buffer(count) = diagnostic
    end subroutine push_diagnostic

    pure function to_lower_ascii(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out

        integer :: i, c

        out = s
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) then
                out(i:i) = achar(c + 32)
            end if
        end do
    end function to_lower_ascii

end module fluff_rule_diagnostic_utils
