module fluff_cache
    ! AST caching for performance optimization
    use fluff_core
    use fluff_ast
    implicit none
    private
    
    ! Cache entry for AST
    type :: cache_entry_t
        character(len=:), allocatable :: filename
        character(len=:), allocatable :: content_hash
        type(fluff_ast_context_t), allocatable :: ast_ctx
        real :: timestamp
        logical :: is_valid = .false.
    end type cache_entry_t
    
    ! AST cache
    type, public :: ast_cache_t
        type(cache_entry_t), allocatable :: entries(:)
        integer :: size = 0
        integer :: capacity = 10
        integer :: max_capacity = 100
        real :: ttl = 300.0  ! Time to live in seconds
    contains
        procedure :: get => cache_get
        procedure :: get_ast => cache_get_ast
        procedure :: put => cache_put
        procedure :: invalidate => cache_invalidate
        procedure :: clear => cache_clear
        procedure :: evict_oldest => cache_evict_oldest
        procedure :: compute_hash => cache_compute_hash
    end type ast_cache_t
    
    ! Public procedures
    public :: create_ast_cache
    
contains
    
    ! Create a new AST cache
    function create_ast_cache(max_size, ttl) result(cache)
        integer, optional, intent(in) :: max_size
        real, optional, intent(in) :: ttl
        type(ast_cache_t) :: cache
        
        if (present(max_size)) cache%max_capacity = max_size
        if (present(ttl)) cache%ttl = ttl
        
        allocate(cache%entries(cache%capacity))
        cache%size = 0
        
    end function create_ast_cache
    
    ! Get AST from cache (returns index if found, 0 otherwise)
    function cache_get(this, filename, content) result(index)
        class(ast_cache_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: content
        integer :: index
        
        character(len=:), allocatable :: hash
        integer :: i
        real :: current_time
        
        index = 0
        hash = this%compute_hash(content)
        
        call cpu_time(current_time)
        
        ! Search for cached entry
        do i = 1, this%size
            if (this%entries(i)%is_valid .and. &
                this%entries(i)%filename == filename .and. &
                this%entries(i)%content_hash == hash) then
                
                ! Check if entry is still fresh
                if (current_time - this%entries(i)%timestamp < this%ttl) then
                    index = i
                    return
                else
                    ! Entry expired, invalidate it
                    this%entries(i)%is_valid = .false.
                end if
            end if
        end do
        
    end function cache_get
    
    ! Get AST by index
    function cache_get_ast(this, index) result(ast_ctx)
        class(ast_cache_t), intent(in) :: this
        integer, intent(in) :: index
        type(fluff_ast_context_t) :: ast_ctx
        
        if (index > 0 .and. index <= this%size .and. &
            this%entries(index)%is_valid .and. &
            allocated(this%entries(index)%ast_ctx)) then
            ast_ctx = this%entries(index)%ast_ctx
        end if
        
    end function cache_get_ast
    
    ! Put AST in cache
    subroutine cache_put(this, filename, content, ast_ctx)
        class(ast_cache_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: content
        type(fluff_ast_context_t), intent(in) :: ast_ctx
        
        character(len=:), allocatable :: hash
        integer :: i, slot
        real :: current_time
        type(cache_entry_t), allocatable :: temp(:)
        
        hash = this%compute_hash(content)
        call cpu_time(current_time)
        
        ! Find available slot or reuse existing
        slot = 0
        do i = 1, this%size
            if (.not. this%entries(i)%is_valid .or. &
                (this%entries(i)%filename == filename .and. &
                 this%entries(i)%content_hash == hash)) then
                slot = i
                exit
            end if
        end do
        
        ! If no slot found and cache not full
        if (slot == 0 .and. this%size < this%capacity .and. this%size < this%max_capacity) then
            this%size = this%size + 1
            slot = this%size
        end if
        
        ! If still no slot and at capacity
        if (slot == 0) then
            ! Grow cache if below max capacity
            if (this%size < this%max_capacity) then
                if (this%capacity < this%max_capacity) then
                    ! Grow array
                    allocate(temp(min(this%capacity * 2, this%max_capacity)))
                    temp(1:this%size) = this%entries(1:this%size)
                    call move_alloc(temp, this%entries)
                    this%capacity = size(this%entries)
                end if
                
                this%size = this%size + 1
                slot = this%size
            else
                ! Evict oldest entry and reuse slot
                call this%evict_oldest()
                ! Find first invalid slot
                do i = 1, this%size
                    if (.not. this%entries(i)%is_valid) then
                        slot = i
                        exit
                    end if
                end do
                if (slot == 0) slot = 1  ! Fallback
            end if
        end if
        
        ! Store entry
        this%entries(slot)%filename = filename
        this%entries(slot)%content_hash = hash
        if (.not. allocated(this%entries(slot)%ast_ctx)) then
            allocate(this%entries(slot)%ast_ctx)
        end if
        this%entries(slot)%ast_ctx = ast_ctx
        this%entries(slot)%timestamp = current_time
        this%entries(slot)%is_valid = .true.
        
    end subroutine cache_put
    
    ! Invalidate cache entry
    subroutine cache_invalidate(this, filename)
        class(ast_cache_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        integer :: i
        
        do i = 1, this%size
            if (this%entries(i)%is_valid .and. &
                this%entries(i)%filename == filename) then
                this%entries(i)%is_valid = .false.
            end if
        end do
        
    end subroutine cache_invalidate
    
    ! Clear entire cache
    subroutine cache_clear(this)
        class(ast_cache_t), intent(inout) :: this
        
        integer :: i
        
        do i = 1, this%size
            this%entries(i)%is_valid = .false.
        end do
        
        this%size = 0
        
    end subroutine cache_clear
    
    ! Evict oldest entry
    subroutine cache_evict_oldest(this)
        class(ast_cache_t), intent(inout) :: this
        
        integer :: i, oldest_idx
        real :: oldest_time
        
        if (this%size == 0) return
        
        oldest_idx = 1
        oldest_time = this%entries(1)%timestamp
        
        do i = 2, this%size
            if (this%entries(i)%is_valid .and. &
                this%entries(i)%timestamp < oldest_time) then
                oldest_idx = i
                oldest_time = this%entries(i)%timestamp
            end if
        end do
        
        this%entries(oldest_idx)%is_valid = .false.
        
        ! Compact array if evicting last entry
        if (oldest_idx == this%size) then
            this%size = this%size - 1
        end if
        
    end subroutine cache_evict_oldest
    
    ! Compute content hash (simple checksum)
    function cache_compute_hash(this, content) result(hash)
        class(ast_cache_t), intent(in) :: this
        character(len=*), intent(in) :: content
        character(len=:), allocatable :: hash
        
        integer :: i
        integer(kind=8) :: checksum
        character(len=16) :: hash_str
        
        checksum = 0
        do i = 1, len(content)
            checksum = checksum * 31 + iachar(content(i:i))
            checksum = modulo(checksum, huge(checksum))
        end do
        
        write(hash_str, '(Z16.16)') checksum
        hash = trim(adjustl(hash_str))
        
    end function cache_compute_hash
    
end module fluff_cache