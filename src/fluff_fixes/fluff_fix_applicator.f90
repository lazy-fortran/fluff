module fluff_fix_applicator
    use fluff_diagnostics, only: diagnostic_t, fix_suggestion_t
    implicit none
    private

    public :: apply_fixes_to_file
    public :: read_text_file
    public :: write_text_file

contains

    subroutine apply_fixes_to_file(file_path, diagnostics, fixes_applied, error_msg)
        character(len=*), intent(in) :: file_path
        type(diagnostic_t), intent(in) :: diagnostics(:)
        integer, intent(out) :: fixes_applied
        character(len=:), allocatable, intent(out) :: error_msg

        character(len=:), allocatable :: source_code, current_code, fixed_code
        integer :: i, j
        type(fix_suggestion_t) :: fix
        integer, allocatable :: applied_lines(:)
        integer :: applied_count
        logical :: can_apply

        fixes_applied = 0
        error_msg = ""

        if (size(diagnostics) == 0) return

        call read_text_file(file_path, source_code, error_msg)
        if (error_msg /= "") return

        current_code = source_code
        allocate (applied_lines(0))
        applied_count = 0

        do i = 1, size(diagnostics)
            if (.not. allocated(diagnostics(i)%fixes)) cycle
            if (size(diagnostics(i)%fixes) == 0) cycle

            fix = diagnostics(i)%fixes(1)

            if (.not. fix%is_safe) cycle
            if (.not. allocated(fix%edits)) cycle
            if (size(fix%edits) == 0) cycle

            can_apply = .true.
            do j = 1, size(fix%edits)
                if (line_already_modified(fix%edits(j)%range%start%line, &
                                          applied_lines, applied_count)) then
                    can_apply = .false.
                    exit
                end if
            end do

            if (.not. can_apply) cycle

            call fix%apply(current_code, fixed_code)

            do j = 1, size(fix%edits)
                call mark_line_modified(fix%edits(j)%range%start%line, &
                                        applied_lines, applied_count)
            end do

            current_code = fixed_code
            fixes_applied = fixes_applied + 1
        end do

        if (fixes_applied > 0) then
            call write_text_file(file_path, current_code, error_msg)
        end if

    end subroutine apply_fixes_to_file

    function line_already_modified(line, applied_lines, count) result(modified)
        integer, intent(in) :: line
        integer, intent(in) :: applied_lines(:)
        integer, intent(in) :: count
        logical :: modified
        integer :: i

        modified = .false.
        do i = 1, count
            if (applied_lines(i) == line) then
                modified = .true.
                return
            end if
        end do
    end function line_already_modified

    subroutine mark_line_modified(line, applied_lines, count)
        integer, intent(in) :: line
        integer, allocatable, intent(inout) :: applied_lines(:)
        integer, intent(inout) :: count

        integer, allocatable :: temp(:)

        if (count >= size(applied_lines)) then
            allocate (temp(size(applied_lines) + 10))
            if (count > 0) temp(1:count) = applied_lines(1:count)
            call move_alloc(temp, applied_lines)
        end if

        count = count + 1
        applied_lines(count) = line
    end subroutine mark_line_modified

    subroutine read_text_file(file_path, content, error_msg)
        character(len=*), intent(in) :: file_path
        character(len=:), allocatable, intent(out) :: content
        character(len=:), allocatable, intent(out) :: error_msg

        integer :: unit, iostat_val
        character(len=4096) :: line
        logical :: first

        content = ""
        error_msg = ""
        first = .true.

        open (newunit=unit, file=file_path, status="old", action="read", &
              iostat=iostat_val)
        if (iostat_val /= 0) then
            error_msg = "Could not open file"
            return
        end if

        do
            read (unit, '(A)', iostat=iostat_val) line
            if (iostat_val /= 0) exit
            if (first) then
                content = trim(line)
                first = .false.
            else
                content = content//new_line('a')//trim(line)
            end if
        end do

        close (unit)
    end subroutine read_text_file

    subroutine write_text_file(file_path, content, error_msg)
        character(len=*), intent(in) :: file_path
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: error_msg

        integer :: unit, iostat_val

        error_msg = ""
        open (newunit=unit, file=file_path, status="replace", action="write", &
              iostat=iostat_val)
        if (iostat_val /= 0) then
            error_msg = "Could not open file for writing"
            return
        end if

        write (unit, '(A)') content
        close (unit)
    end subroutine write_text_file

end module fluff_fix_applicator
