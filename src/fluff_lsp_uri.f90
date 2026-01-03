module fluff_lsp_uri
    implicit none
    private

    public :: uri_to_path

contains

    subroutine uri_to_path(uri, path, success)
        character(len=*), intent(in) :: uri
        character(len=:), allocatable, intent(out) :: path
        logical, intent(out) :: success

        character(len=:), allocatable :: raw
        character(len=:), allocatable :: decoded
        integer :: pos

        path = ""
        success = .false.

        if (index(uri, "file://") /= 1) return

        raw = uri(8:)
        call percent_decode(raw, decoded, success)
        if (.not. success) return

        if (len(decoded) >= 3) then
            if (decoded(1:1) == "/" .and. is_alpha(decoded(2:2)) .and. &
                decoded(3:3) == ":") then
                decoded = decoded(2:)
            end if
        end if

        if (len(decoded) >= 2) then
            if (decoded(1:2) == "/." .and. (len(decoded) == 2 .or. &
                                            decoded(3:3) == "/" .or. &
                                            decoded(3:3) == ".")) then
                decoded = decoded(2:)
            end if
        end if

        pos = 1
        do while (pos <= len(decoded))
            if (decoded(pos:pos) == char(0)) then
                success = .false.
                return
            end if
            pos = pos + 1
        end do

        path = decoded
        success = .true.
    end subroutine uri_to_path

    subroutine percent_decode(input, output, success)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: output
        logical, intent(out) :: success

        integer :: i
        integer :: hi, lo
        integer :: code

        output = ""
        success = .false.
        i = 1
        do while (i <= len(input))
            if (input(i:i) /= "%") then
                output = output//input(i:i)
                i = i + 1
                cycle
            end if

            if (i + 2 > len(input)) return
            hi = hex_digit(input(i + 1:i + 1))
            lo = hex_digit(input(i + 2:i + 2))
            if (hi < 0 .or. lo < 0) return

            code = hi*16 + lo
            output = output//achar(code)
            i = i + 3
        end do

        success = .true.
    end subroutine percent_decode

    pure integer function hex_digit(c) result(val)
        character(len=1), intent(in) :: c

        select case (c)
        case ("0":"9")
            val = iachar(c) - iachar("0")
        case ("a":"f")
            val = 10 + iachar(c) - iachar("a")
        case ("A":"F")
            val = 10 + iachar(c) - iachar("A")
        case default
            val = -1
        end select
    end function hex_digit

    pure logical function is_alpha(c) result(res)
        character(len=1), intent(in) :: c

        res = (c >= "A" .and. c <= "Z") .or. (c >= "a" .and. c <= "z")
    end function is_alpha

end module fluff_lsp_uri
