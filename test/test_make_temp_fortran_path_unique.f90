program test_make_temp_fortran_path_unique
    use test_support, only: make_temp_fortran_path
    implicit none

    integer, parameter :: n = 500
    integer :: i, j
    character(len=:), allocatable :: path
    character(len=512) :: paths(n)

    paths = ""

    do i = 1, n
        call make_temp_fortran_path("fluff_test_temp_path", path)
        if (.not. allocated(path)) then
            error stop "make_temp_fortran_path did not allocate path"
        end if
        if (len_trim(path) == 0) then
            error stop "make_temp_fortran_path returned empty path"
        end if
        paths(i) = path
    end do

    do i = 1, n - 1
        do j = i + 1, n
            if (trim(paths(i)) == trim(paths(j))) then
                error stop "Duplicate temp path generated"
            end if
        end do
    end do
end program test_make_temp_fortran_path_unique
