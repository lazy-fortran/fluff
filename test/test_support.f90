module test_support
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: linter_engine_t
    implicit none
    private

    public :: make_temp_fortran_path
    public :: write_text_file
    public :: delete_file_if_exists
    public :: lint_file_checked
    public :: assert_has_diagnostic_code
    public :: assert_diagnostic_location
    public :: assert_equal_int

    integer(int64), parameter :: temp_path_clock_mod = 1000000000_int64
    integer(int64), save :: temp_path_last_clock = -1_int64
    integer(int64), save :: temp_path_seq = 0_int64
    character(len=*), parameter :: temp_path_name_fmt = &
                                   '(A,"_",I4.4,I2.2,I2.2,"_",I2.2,I2.2,I2.2'// &
                                   ',"_",I3.3,"_",I9.9'// &
                                   ',"_",I0)'

contains

    subroutine make_temp_fortran_path(stem, path)
        character(len=*), intent(in) :: stem
        character(len=:), allocatable, intent(out) :: path

        integer :: values(8)
        integer(int64) :: clock_count
        integer(int64) :: clock_suffix
        character(len=256) :: name

        call date_and_time(values=values)
        call system_clock(count=clock_count)

        if (clock_count == temp_path_last_clock) then
            temp_path_seq = temp_path_seq + 1_int64
        else
            temp_path_seq = 0_int64
            temp_path_last_clock = clock_count
        end if

        clock_suffix = modulo(clock_count, temp_path_clock_mod)

        write (name, temp_path_name_fmt) &
            trim(stem), values(1), values(2), values(3), values(5), values(6), &
            values(7), values(8), clock_suffix, temp_path_seq
        path = "/tmp/"//trim(name)//".f90"
    end subroutine make_temp_fortran_path

    subroutine write_text_file(path, text)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: text

        integer :: unit

        open (newunit=unit, file=path, status="replace", action="write")
        write (unit, '(A)') text
        close (unit)
    end subroutine write_text_file

    subroutine delete_file_if_exists(path)
        character(len=*), intent(in) :: path

        integer :: unit, ios

        open (newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios == 0) then
            close (unit, status="delete")
        end if
    end subroutine delete_file_if_exists

    subroutine lint_file_checked(linter, path, diags)
        type(linter_engine_t), intent(inout) :: linter
        character(len=*), intent(in) :: path
        type(diagnostic_t), allocatable, intent(out) :: diags(:)

        character(len=:), allocatable :: error_msg

        call linter%lint_file(path, diags, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') "lint_file error: "//trim(error_msg)
                flush (error_unit)
                error stop trim(error_msg)
            end if
        end if
    end subroutine lint_file_checked

    subroutine assert_has_diagnostic_code(diags, code, expected, message)
        type(diagnostic_t), allocatable, intent(in) :: diags(:)
        character(len=*), intent(in) :: code
        logical, intent(in) :: expected
        character(len=*), intent(in) :: message

        integer :: i
        logical :: found

        found = .false.
        if (allocated(diags)) then
            do i = 1, size(diags)
                if (diags(i)%code == code) then
                    found = .true.
                    exit
                end if
            end do
        end if

        if (found .neqv. expected) then
            if (allocated(diags)) then
                do i = 1, size(diags)
                    write (error_unit, '(A,1X,A,1X,A,1X,A)') &
                        "diagnostic:", diags(i)%code, "-", trim(diags(i)%message)
                end do
            else
                write (error_unit, '(A)') "diagnostic: <none>"
            end if
            flush (error_unit)
            if (expected) then
                error stop "Failed: expected diagnostic "//trim(code)// &
                    " not found: "//trim(message)
            else
                error stop "Failed: unexpected diagnostic "//trim(code)// &
                    " found: "//trim(message)
            end if
        end if
    end subroutine assert_has_diagnostic_code

    subroutine assert_diagnostic_location(diags, code, line, column, message)
        type(diagnostic_t), allocatable, intent(in) :: diags(:)
        character(len=*), intent(in) :: code
        integer, intent(in) :: line
        integer, intent(in) :: column
        character(len=*), intent(in) :: message

        integer :: i
        logical :: found

        found = .false.
        if (allocated(diags)) then
            do i = 1, size(diags)
                if (diags(i)%code /= code) cycle
                found = .true.
                if (diags(i)%location%start%line /= line .or. &
                    diags(i)%location%start%column /= column) then
                    write (error_unit, '(A,1X,A,1X,I0,":",I0)') &
                        "diagnostic location:", diags(i)%code, &
                        diags(i)%location%start%line, diags(i)%location%start%column
                    flush (error_unit)
                    error stop "Failed: wrong diagnostic location for "//trim(code)// &
                        ": "//trim(message)
                end if
                return
            end do
        end if

        if (.not. found) then
            call assert_has_diagnostic_code(diags, code, .true., message)
        end if
    end subroutine assert_diagnostic_location

    subroutine assert_equal_int(actual, expected, context)
        integer, intent(in) :: actual
        integer, intent(in) :: expected
        character(len=*), intent(in) :: context

        if (actual /= expected) then
            write (error_unit, '(A)') "Failed: "//trim(context)
            write (error_unit, '(A,I0)') "  expected: ", expected
            write (error_unit, '(A,I0)') "  actual:   ", actual
            flush (error_unit)
            error stop 1
        end if
    end subroutine assert_equal_int

end module test_support
