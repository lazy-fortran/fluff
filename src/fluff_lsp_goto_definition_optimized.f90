module fluff_lsp_goto_definition_optimized
    use fluff_core
    use fluff_ast
    use fluff_lsp_goto_definition
    use fluff_lsp_cache
    use fluff_lsp_performance
    implicit none
    private
    
    public :: lsp_goto_definition_provider_t
    public :: create_goto_definition_provider
    
    ! Symbol index for fast lookups
    type :: symbol_index_entry_t
        character(len=:), allocatable :: symbol_name
        character(len=:), allocatable :: uri
        integer :: line
        integer :: character
        character(len=:), allocatable :: kind  ! variable, function, type, etc.
    end type symbol_index_entry_t
    
    ! Optimized goto definition provider
    type :: lsp_goto_definition_provider_t
        type(lsp_cache_t) :: cache
        type(lsp_performance_monitor_t) :: monitor
        
        ! Symbol index for fast lookups
        type(symbol_index_entry_t), allocatable :: symbol_index(:)
        integer :: symbol_count
        logical :: index_built
    contains
        procedure :: find_definition_optimized
        procedure :: build_symbol_index
        procedure :: invalidate_index
        procedure :: get_performance_stats
    end type lsp_goto_definition_provider_t
    
contains
    
    ! Create optimized goto definition provider
    function create_goto_definition_provider(enable_cache, enable_monitoring) result(provider)
        logical, intent(in), optional :: enable_cache, enable_monitoring
        type(lsp_goto_definition_provider_t) :: provider
        
        logical :: cache_enabled, monitoring_enabled
        
        cache_enabled = .true.
        monitoring_enabled = .true.
        
        if (present(enable_cache)) cache_enabled = enable_cache
        if (present(enable_monitoring)) monitoring_enabled = enable_monitoring
        
        provider%cache = create_lsp_cache(enabled=cache_enabled)
        provider%monitor = create_performance_monitor(enabled=monitoring_enabled)
        
        allocate(provider%symbol_index(1000))  ! Pre-allocate
        provider%symbol_count = 0
        provider%index_built = .false.
        
    end function create_goto_definition_provider
    
    ! Find definition with caching and indexing
    subroutine find_definition_optimized(this, uri, code, line, character, &
                                       result_uri, def_line, def_char, success)
        class(lsp_goto_definition_provider_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, code
        integer, intent(in) :: line, character
        character(len=:), allocatable, intent(out) :: result_uri
        integer, intent(out) :: def_line, def_char
        logical, intent(out) :: success
        
        type(lsp_timer_t) :: timer, index_timer
        character(len=:), allocatable :: lines(:), token
        integer :: line_count, version, i
        logical :: cache_hit
        real :: elapsed_ms
        
        ! Start timing
        call start_timer(timer)
        
        ! Build symbol index if needed (disabled for now due to bounds issues)
        ! if (.not. this%index_built) then
        !     call start_timer(index_timer)
        !     call this%build_symbol_index(uri, code)
        !     call stop_timer(index_timer)
        !     call this%monitor%record_operation("build_index", get_elapsed_ms(index_timer))
        ! end if
        
        ! Use cache for line splitting
        version = 1
        call this%cache%get_or_parse(uri, code, version, lines, line_count, cache_hit)
        
        ! Extract token at position
        if (line <= line_count) then
            call extract_token_at_position(lines(line), character, token)
            
            if (allocated(token)) then
                ! Fast lookup in symbol index
                success = .false.
                do i = 1, this%symbol_count
                    if (this%symbol_index(i)%symbol_name == token) then
                        result_uri = this%symbol_index(i)%uri
                        def_line = this%symbol_index(i)%line
                        def_char = this%symbol_index(i)%character
                        success = .true.
                        exit
                    end if
                end do
                
                ! Fallback to original implementation if not found in index
                if (.not. success) then
                    call find_definition(code, line, character, result_uri, def_line, def_char, success)
                end if
            else
                success = .false.
            end if
        else
            success = .false.
        end if
        
        ! Stop timing and record
        call stop_timer(timer)
        elapsed_ms = get_elapsed_ms(timer)
        call this%monitor%record_operation("goto_definition", elapsed_ms)
        
        if (cache_hit) then
            call this%monitor%record_operation("goto_def_cache_hit", elapsed_ms)
        else
            call this%monitor%record_operation("goto_def_cache_miss", elapsed_ms)
        end if
        
    end subroutine find_definition_optimized
    
    ! Build symbol index for fast lookups
    subroutine build_symbol_index(this, uri, code)
        class(lsp_goto_definition_provider_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, code
        
        character(len=:), allocatable :: lines(:)
        integer :: line_count, i, pos, version
        logical :: cache_hit
        character(len=:), allocatable :: line, trimmed_line
        
        ! Reset index
        this%symbol_count = 0
        
        ! Get lines from cache
        version = 1
        call this%cache%get_or_parse(uri, code, version, lines, line_count, cache_hit)
        
        ! Build index by scanning all lines
        do i = 1, line_count
            line = lines(i)
            trimmed_line = adjustl(line)
            
            ! Index variable declarations
            if (index(trimmed_line, "::") > 0) then
                call index_declaration(this, uri, line, i)
            end if
            
            ! Index procedure definitions
            if (index(trimmed_line, "subroutine") > 0 .or. &
                index(trimmed_line, "function") > 0) then
                call index_procedure(this, uri, line, i)
            end if
            
            ! Index type definitions
            if (index(trimmed_line, "type") == 1 .and. &
                index(trimmed_line, "::") > 0) then
                call index_type(this, uri, line, i)
            end if
            
            ! Index module definitions
            if (index(trimmed_line, "module") > 0 .and. &
                index(trimmed_line, "module procedure") == 0) then
                call index_module(this, uri, line, i)
            end if
        end do
        
        this%index_built = .true.
        
    end subroutine build_symbol_index
    
    ! Index a declaration
    subroutine index_declaration(this, uri, line, line_num)
        type(lsp_goto_definition_provider_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, line
        integer, intent(in) :: line_num
        
        integer :: double_colon_pos, pos, end_pos
        character(len=:), allocatable :: vars_part, var_name
        
        double_colon_pos = index(line, "::")
        if (double_colon_pos > 0) then
            vars_part = adjustl(line(double_colon_pos + 2:))
            
            ! Simple parsing - just get first variable
            pos = 1
            do while (pos <= len(vars_part) .and. &
                     vars_part(pos:pos) /= ' ' .and. &
                     vars_part(pos:pos) /= ',' .and. &
                     vars_part(pos:pos) /= '(' .and. &
                     vars_part(pos:pos) /= '=')
                pos = pos + 1
            end do
            
            if (pos > 1) then
                var_name = vars_part(1:pos-1)
                if (len_trim(var_name) > 0) then
                    call add_to_index(this, var_name, uri, line_num, &
                                    double_colon_pos + 2, "variable")
                end if
            end if
        end if
        
    end subroutine index_declaration
    
    ! Index a procedure
    subroutine index_procedure(this, uri, line, line_num)
        type(lsp_goto_definition_provider_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, line
        integer, intent(in) :: line_num
        
        integer :: sub_pos, func_pos, name_start, name_end, i
        character(len=:), allocatable :: proc_name
        
        sub_pos = index(line, "subroutine")
        func_pos = index(line, "function")
        
        if (sub_pos > 0) then
            name_start = sub_pos + 10  ! Length of "subroutine"
        else if (func_pos > 0) then
            name_start = func_pos + 8   ! Length of "function"
        else
            return
        end if
        
        ! Skip whitespace
        do while (name_start <= len(line) .and. line(name_start:name_start) == ' ')
            name_start = name_start + 1
        end do
        
        ! Find end of name
        name_end = name_start
        do while (name_end <= len(line) .and. &
                 line(name_end:name_end) /= ' ' .and. &
                 line(name_end:name_end) /= '(')
            name_end = name_end + 1
        end do
        
        if (name_end > name_start) then
            proc_name = line(name_start:name_end-1)
            call add_to_index(this, proc_name, uri, line_num, &
                            name_start - 1, "procedure")
        end if
        
    end subroutine index_procedure
    
    ! Index a type
    subroutine index_type(this, uri, line, line_num)
        type(lsp_goto_definition_provider_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, line
        integer, intent(in) :: line_num
        
        integer :: double_colon_pos, name_start, name_end
        character(len=:), allocatable :: type_name
        
        double_colon_pos = index(line, "::")
        if (double_colon_pos > 0) then
            name_start = double_colon_pos + 2
            
            ! Skip whitespace
            do while (name_start <= len(line) .and. line(name_start:name_start) == ' ')
                name_start = name_start + 1
            end do
            
            ! Find end of name
            name_end = name_start
            do while (name_end <= len(line) .and. &
                     line(name_end:name_end) /= ' ')
                name_end = name_end + 1
            end do
            
            if (name_end > name_start) then
                type_name = line(name_start:name_end-1)
                call add_to_index(this, type_name, uri, line_num, &
                                name_start - 1, "type")
            end if
        end if
        
    end subroutine index_type
    
    ! Index a module
    subroutine index_module(this, uri, line, line_num)
        type(lsp_goto_definition_provider_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, line
        integer, intent(in) :: line_num
        
        integer :: mod_pos, name_start, name_end
        character(len=:), allocatable :: mod_name
        
        mod_pos = index(line, "module")
        if (mod_pos > 0) then
            name_start = mod_pos + 6  ! Length of "module"
            
            ! Skip whitespace
            do while (name_start <= len(line) .and. line(name_start:name_start) == ' ')
                name_start = name_start + 1
            end do
            
            ! Find end of name
            name_end = name_start
            do while (name_end <= len(line) .and. &
                     line(name_end:name_end) /= ' ')
                name_end = name_end + 1
            end do
            
            if (name_end > name_start) then
                mod_name = line(name_start:name_end-1)
                call add_to_index(this, mod_name, uri, line_num, &
                                name_start - 1, "module")
            end if
        end if
        
    end subroutine index_module
    
    ! Add symbol to index
    subroutine add_to_index(this, symbol_name, uri, line, character, kind)
        type(lsp_goto_definition_provider_t), intent(inout) :: this
        character(len=*), intent(in) :: symbol_name, uri, kind
        integer, intent(in) :: line, character
        
        if (this%symbol_count < size(this%symbol_index)) then
            this%symbol_count = this%symbol_count + 1
            this%symbol_index(this%symbol_count)%symbol_name = symbol_name
            this%symbol_index(this%symbol_count)%uri = uri
            this%symbol_index(this%symbol_count)%line = line
            this%symbol_index(this%symbol_count)%character = character
            this%symbol_index(this%symbol_count)%kind = kind
        end if
        
    end subroutine add_to_index
    
    ! Invalidate symbol index
    subroutine invalidate_index(this)
        class(lsp_goto_definition_provider_t), intent(inout) :: this
        
        this%index_built = .false.
        this%symbol_count = 0
        
    end subroutine invalidate_index
    
    ! Get performance statistics
    subroutine get_performance_stats(this)
        class(lsp_goto_definition_provider_t), intent(in) :: this
        
        call this%monitor%print_report()
        
        print *, "Symbol index size: ", this%symbol_count, " symbols"
        print *, "Cache memory usage: ", this%cache%get_memory_usage(), " bytes"
        
    end subroutine get_performance_stats
    
    ! Helper to extract token at position
    subroutine extract_token_at_position(line, position, token)
        character(len=*), intent(in) :: line
        integer, intent(in) :: position
        character(len=:), allocatable, intent(out) :: token
        
        integer :: start_pos, end_pos, i, adj_position
        character(len=1) :: ch
        
        adj_position = position + 1
        
        if (adj_position > len(line) .or. adj_position < 1) then
            return
        end if
        
        ! Find token boundaries
        start_pos = adj_position
        end_pos = adj_position
        
        ! Move start backward
        do i = adj_position, 1, -1
            ch = line(i:i)
            if ((ch >= 'a' .and. ch <= 'z') .or. &
                (ch >= 'A' .and. ch <= 'Z') .or. &
                (ch >= '0' .and. ch <= '9') .or. &
                ch == '_') then
                start_pos = i
            else
                exit
            end if
        end do
        
        ! Move end forward
        do i = adj_position, len(line)
            ch = line(i:i)
            if ((ch >= 'a' .and. ch <= 'z') .or. &
                (ch >= 'A' .and. ch <= 'Z') .or. &
                (ch >= '0' .and. ch <= '9') .or. &
                ch == '_') then
                end_pos = i
            else
                exit
            end if
        end do
        
        if (start_pos <= end_pos) then
            token = line(start_pos:end_pos)
        end if
        
    end subroutine extract_token_at_position
    
end module fluff_lsp_goto_definition_optimized