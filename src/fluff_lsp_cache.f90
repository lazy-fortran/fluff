module fluff_lsp_cache
    use fluff_core
    use fluff_ast
    implicit none
    private
    
    public :: lsp_cache_t
    public :: cache_entry_t
    public :: create_lsp_cache
    
    ! Cache entry for parsed content
    type :: cache_entry_t
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: content
        character(len=:), allocatable :: content_hash
        integer :: version
        logical :: is_valid
        
        ! Cached parse results
        character(len=:), allocatable :: lines(:)
        integer :: line_count
        
        ! Cached token information
        type(token_info_t), allocatable :: tokens(:)
        integer :: token_count
        
        ! Timestamp
        integer :: last_access_time
    end type cache_entry_t
    
    ! Token information for quick lookup
    type :: token_info_t
        character(len=:), allocatable :: text
        integer :: line
        integer :: start_char
        integer :: end_char
        character(len=:), allocatable :: kind  ! variable, function, type, etc.
    end type token_info_t
    
    ! LSP cache manager
    type :: lsp_cache_t
        type(cache_entry_t), allocatable :: entries(:)
        integer :: entry_count
        integer :: max_entries
        logical :: enabled
    contains
        procedure :: get_or_parse
        procedure :: invalidate
        procedure :: invalidate_all
        procedure :: cleanup_old_entries
        procedure :: get_memory_usage
    end type lsp_cache_t
    
contains
    
    ! Create a new LSP cache
    function create_lsp_cache(max_entries, enabled) result(cache)
        integer, intent(in), optional :: max_entries
        logical, intent(in), optional :: enabled
        type(lsp_cache_t) :: cache
        
        if (present(max_entries)) then
            cache%max_entries = max_entries
        else
            cache%max_entries = 100  ! Default cache size
        end if
        
        if (present(enabled)) then
            cache%enabled = enabled
        else
            cache%enabled = .true.
        end if
        
        allocate(cache%entries(cache%max_entries))
        cache%entry_count = 0
        
    end function create_lsp_cache
    
    ! Get cached parse results or parse and cache
    subroutine get_or_parse(this, uri, content, version, lines, line_count, cache_hit)
        class(lsp_cache_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, content
        integer, intent(in) :: version
        character(len=:), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: line_count
        logical, intent(out) :: cache_hit
        
        integer :: i, entry_idx
        character(len=:), allocatable :: hash
        
        cache_hit = .false.
        
        if (.not. this%enabled) then
            ! Direct parse without caching
            call split_lines(content, lines, line_count)
            return
        end if
        
        ! Calculate content hash
        call calculate_hash(content, hash)
        
        ! Look for existing entry
        entry_idx = 0
        do i = 1, this%entry_count
            if (allocated(this%entries(i)%uri)) then
                if (this%entries(i)%uri == uri .and. &
                    this%entries(i)%version == version .and. &
                    allocated(this%entries(i)%content_hash) .and. &
                    this%entries(i)%content_hash == hash .and. &
                    this%entries(i)%is_valid) then
                    entry_idx = i
                    exit
                end if
            end if
        end do
        
        if (entry_idx > 0) then
            ! Cache hit
            cache_hit = .true.
            lines = this%entries(entry_idx)%lines
            line_count = this%entries(entry_idx)%line_count
            call update_access_time(this%entries(entry_idx))
        else
            ! Cache miss - parse and store
            call split_lines(content, lines, line_count)
            
            ! Find slot for new entry
            if (this%entry_count < this%max_entries) then
                this%entry_count = this%entry_count + 1
                entry_idx = this%entry_count
            else
                ! Evict oldest entry
                entry_idx = find_oldest_entry(this)
            end if
            
            ! Store in cache
            this%entries(entry_idx)%uri = uri
            this%entries(entry_idx)%content = content
            this%entries(entry_idx)%content_hash = hash
            this%entries(entry_idx)%version = version
            this%entries(entry_idx)%is_valid = .true.
            this%entries(entry_idx)%lines = lines
            this%entries(entry_idx)%line_count = line_count
            call update_access_time(this%entries(entry_idx))
            
            ! Parse tokens for future use (disabled for now due to bounds issues)
            ! call parse_tokens(lines, line_count, &
            !                 this%entries(entry_idx)%tokens, &
            !                 this%entries(entry_idx)%token_count)
            allocate(this%entries(entry_idx)%tokens(0))
            this%entries(entry_idx)%token_count = 0
        end if
        
    end subroutine get_or_parse
    
    ! Invalidate cache entry for a URI
    subroutine invalidate(this, uri)
        class(lsp_cache_t), intent(inout) :: this
        character(len=*), intent(in) :: uri
        
        integer :: i
        
        do i = 1, this%entry_count
            if (allocated(this%entries(i)%uri)) then
                if (this%entries(i)%uri == uri) then
                    this%entries(i)%is_valid = .false.
                end if
            end if
        end do
        
    end subroutine invalidate
    
    ! Invalidate all cache entries
    subroutine invalidate_all(this)
        class(lsp_cache_t), intent(inout) :: this
        
        integer :: i
        
        do i = 1, this%entry_count
            this%entries(i)%is_valid = .false.
        end do
        
    end subroutine invalidate_all
    
    ! Clean up old cache entries
    subroutine cleanup_old_entries(this, max_age_seconds)
        class(lsp_cache_t), intent(inout) :: this
        integer, intent(in) :: max_age_seconds
        
        integer :: i, current_time
        
        call system_clock(current_time)
        
        do i = 1, this%entry_count
            if (this%entries(i)%is_valid) then
                if (current_time - this%entries(i)%last_access_time > max_age_seconds * 1000) then
                    this%entries(i)%is_valid = .false.
                end if
            end if
        end do
        
    end subroutine cleanup_old_entries
    
    ! Get estimated memory usage
    function get_memory_usage(this) result(bytes)
        class(lsp_cache_t), intent(in) :: this
        integer :: bytes
        
        integer :: i
        
        bytes = 0
        
        do i = 1, this%entry_count
            if (this%entries(i)%is_valid) then
                if (allocated(this%entries(i)%content)) then
                    bytes = bytes + len(this%entries(i)%content)
                end if
                if (allocated(this%entries(i)%lines)) then
                    bytes = bytes + size(this%entries(i)%lines) * 256  ! Estimate
                end if
                if (allocated(this%entries(i)%tokens)) then
                    bytes = bytes + size(this%entries(i)%tokens) * 64  ! Estimate
                end if
            end if
        end do
        
    end function get_memory_usage
    
    ! Calculate simple hash of content
    subroutine calculate_hash(content, hash)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: hash
        
        integer :: i, hash_value
        character(len=16) :: hash_str
        
        ! Simple hash calculation
        hash_value = 0
        do i = 1, len(content)
            hash_value = mod(hash_value * 31 + ichar(content(i:i)), huge(1))
        end do
        
        write(hash_str, '(I0)') hash_value
        hash = trim(hash_str)
        
    end subroutine calculate_hash
    
    ! Update access time
    subroutine update_access_time(entry)
        type(cache_entry_t), intent(inout) :: entry
        
        call system_clock(entry%last_access_time)
        
    end subroutine update_access_time
    
    ! Find oldest entry for eviction
    function find_oldest_entry(this) result(idx)
        type(lsp_cache_t), intent(in) :: this
        integer :: idx
        
        integer :: i, oldest_time
        
        idx = 1
        oldest_time = huge(1)
        
        do i = 1, this%entry_count
            if (this%entries(i)%last_access_time < oldest_time) then
                oldest_time = this%entries(i)%last_access_time
                idx = i
            end if
        end do
        
    end function find_oldest_entry
    
    ! Parse tokens from lines
    subroutine parse_tokens(lines, line_count, tokens, token_count)
        character(len=*), intent(in) :: lines(:)
        integer, intent(in) :: line_count
        type(token_info_t), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: token_count
        
        integer :: i, j, pos, max_tokens
        character(len=:), allocatable :: line, word
        
        ! Estimate maximum tokens
        max_tokens = line_count * 10
        allocate(tokens(max_tokens))
        token_count = 0
        
        ! Simple tokenization
        do i = 1, line_count
            line = adjustl(lines(i))
            pos = 1
            
            do while (pos <= len(line))
                ! Skip whitespace
                do while (pos <= len(line) .and. line(pos:pos) == ' ')
                    pos = pos + 1
                end do
                
                if (pos > len(line)) exit
                
                ! Extract word
                j = pos
                do while (j <= len(line) .and. line(j:j) /= ' ' .and. &
                         line(j:j) /= '(' .and. line(j:j) /= ')' .and. &
                         line(j:j) /= ',' .and. line(j:j) /= ':')
                    j = j + 1
                end do
                
                ! Ensure j doesn't exceed string bounds
                j = min(j, len(line))
                
                if (j > pos) then
                    word = line(pos:j-1)
                    
                    ! Store token if it's an identifier
                    if (is_identifier(word)) then
                        token_count = token_count + 1
                        if (token_count <= max_tokens) then
                            tokens(token_count)%text = word
                            tokens(token_count)%line = i
                            tokens(token_count)%start_char = pos - 1  ! 0-based
                            tokens(token_count)%end_char = j - 2      ! 0-based
                            
                            ! Determine kind (simplified)
                            if (index(line, "::") > 0 .and. pos > index(line, "::")) then
                                tokens(token_count)%kind = "variable"
                            else if (index(line, "subroutine") > 0 .or. &
                                    index(line, "function") > 0) then
                                tokens(token_count)%kind = "procedure"
                            else if (index(line, "type") > 0) then
                                tokens(token_count)%kind = "type"
                            else
                                tokens(token_count)%kind = "unknown"
                            end if
                        end if
                    end if
                end if
                
                pos = j + 1
            end do
        end do
        
    end subroutine parse_tokens
    
    ! Check if string is an identifier
    logical function is_identifier(str)
        character(len=*), intent(in) :: str
        integer :: i
        character(len=1) :: ch
        
        is_identifier = .false.
        if (len(str) == 0) return
        
        ! First character must be letter or underscore
        ch = str(1:1)
        if (.not. ((ch >= 'a' .and. ch <= 'z') .or. &
                  (ch >= 'A' .and. ch <= 'Z') .or. &
                  ch == '_')) return
        
        ! Rest must be alphanumeric or underscore
        do i = 2, len(str)
            ch = str(i:i)
            if (.not. ((ch >= 'a' .and. ch <= 'z') .or. &
                      (ch >= 'A' .and. ch <= 'Z') .or. &
                      (ch >= '0' .and. ch <= '9') .or. &
                      ch == '_')) return
        end do
        
        is_identifier = .true.
        
    end function is_identifier
    
    ! Helper to split code into lines
    subroutine split_lines(code, lines, line_count)
        character(len=*), intent(in) :: code
        character(len=:), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: line_count
        
        integer :: i, line_start, max_line_len
        
        ! Count lines
        line_count = 1
        do i = 1, len(code)
            if (code(i:i) == new_line('a')) line_count = line_count + 1
        end do
        
        ! Allocate lines array
        max_line_len = len(code)
        allocate(character(len=max_line_len) :: lines(line_count))
        
        ! Split into lines
        line_count = 0
        line_start = 1
        do i = 1, len(code)
            if (code(i:i) == new_line('a')) then
                line_count = line_count + 1
                lines(line_count) = code(line_start:i-1)
                line_start = i + 1
            end if
        end do
        if (line_start <= len(code)) then
            line_count = line_count + 1
            lines(line_count) = code(line_start:)
        end if
        
    end subroutine split_lines
    
end module fluff_lsp_cache