module fluff_common
    ! Common types and interfaces shared across modules
    use fluff_core
    implicit none
    private
    
    ! File patterns
    type, public :: file_pattern_t
        character(len=:), allocatable :: pattern
        logical :: is_exclude = .false.
    end type file_pattern_t
    
    ! File list
    type, public :: file_list_t
        character(len=:), allocatable :: files(:)
        integer :: count = 0
    contains
        procedure :: add => file_list_add
        procedure :: clear => file_list_clear
        procedure :: contains => file_list_contains
    end type file_list_t
    
    ! String utilities
    public :: string_starts_with
    public :: string_ends_with
    public :: string_contains
    public :: string_trim_quotes
    
    ! File utilities  
    public :: is_fortran_file
    public :: get_file_extension
    public :: normalize_path
    
contains
    
    ! Add file to list
    subroutine file_list_add(this, filename)
        class(file_list_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        character(len=:), allocatable :: temp(:)
        integer :: n
        
        if (.not. allocated(this%files)) then
            allocate(character(len=len(filename)) :: this%files(10))
            this%count = 0
        else if (this%count >= size(this%files)) then
            ! Grow array
            n = size(this%files)
            allocate(character(len=max(len(this%files), len(filename))) :: temp(n * 2))
            temp(1:n) = this%files
            call move_alloc(temp, this%files)
        end if
        
        this%count = this%count + 1
        this%files(this%count) = filename
        
    end subroutine file_list_add
    
    ! Clear file list
    subroutine file_list_clear(this)
        class(file_list_t), intent(inout) :: this
        this%count = 0
    end subroutine file_list_clear
    
    ! Check if file is in list
    function file_list_contains(this, filename) result(found)
        class(file_list_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        logical :: found
        
        integer :: i
        
        found = .false.
        do i = 1, this%count
            if (this%files(i) == filename) then
                found = .true.
                exit
            end if
        end do
        
    end function file_list_contains
    
    ! String starts with
    function string_starts_with(str, prefix) result(starts)
        character(len=*), intent(in) :: str
        character(len=*), intent(in) :: prefix
        logical :: starts
        
        starts = .false.
        if (len(str) >= len(prefix)) then
            starts = str(1:len(prefix)) == prefix
        end if
        
    end function string_starts_with
    
    ! String ends with
    function string_ends_with(str, suffix) result(ends)
        character(len=*), intent(in) :: str
        character(len=*), intent(in) :: suffix
        logical :: ends
        
        integer :: str_len, suffix_len
        
        ends = .false.
        str_len = len(str)
        suffix_len = len(suffix)
        
        if (str_len >= suffix_len) then
            ends = str(str_len - suffix_len + 1:str_len) == suffix
        end if
        
    end function string_ends_with
    
    ! String contains
    function string_contains(str, substr) result(contains)
        character(len=*), intent(in) :: str
        character(len=*), intent(in) :: substr
        logical :: contains
        
        contains = index(str, substr) > 0
        
    end function string_contains
    
    ! Trim quotes from string
    function string_trim_quotes(str) result(trimmed)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: trimmed
        
        integer :: str_len
        
        str_len = len_trim(str)
        
        if (str_len >= 2) then
            if ((str(1:1) == '"' .and. str(str_len:str_len) == '"') .or. &
                (str(1:1) == "'" .and. str(str_len:str_len) == "'")) then
                trimmed = str(2:str_len-1)
            else
                trimmed = trim(str)
            end if
        else
            trimmed = trim(str)
        end if
        
    end function string_trim_quotes
    
    ! Check if file is a Fortran file
    function is_fortran_file(filename) result(is_fortran)
        character(len=*), intent(in) :: filename
        logical :: is_fortran
        
        character(len=:), allocatable :: ext
        
        ext = get_file_extension(filename)
        is_fortran = ext == "f90" .or. ext == "f95" .or. ext == "f03" .or. &
                     ext == "f08" .or. ext == "f18" .or. ext == "f" .or. &
                     ext == "for" .or. ext == "fpp" .or. ext == "F90" .or. &
                     ext == "F95" .or. ext == "F03" .or. ext == "F08" .or. &
                     ext == "F" .or. ext == "FOR" .or. ext == "FPP"
        
    end function is_fortran_file
    
    ! Get file extension
    function get_file_extension(filename) result(ext)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: ext
        
        integer :: dot_pos
        
        dot_pos = index(filename, ".", back=.true.)
        
        if (dot_pos > 0 .and. dot_pos < len(filename)) then
            ext = filename(dot_pos+1:)
        else
            ext = ""
        end if
        
    end function get_file_extension
    
    ! Normalize file path
    function normalize_path(path) result(normalized)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: normalized
        
        ! TODO: Implement proper path normalization
        ! For now, just trim
        normalized = trim(path)
        
    end function normalize_path
    
end module fluff_common