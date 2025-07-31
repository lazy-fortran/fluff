module fluff_lsp_server_optimized
    use fluff_core
    use fluff_lsp_hover_optimized
    use fluff_lsp_goto_definition_optimized
    use fluff_lsp_performance
    use fluff_lsp_memory
    use fluff_lsp_cache
    implicit none
    private
    
    public :: optimized_lsp_server_t
    public :: create_optimized_lsp_server
    
    ! Document state for incremental updates
    type :: document_state_t
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: content
        integer :: version
        logical :: is_dirty
        integer :: last_modified
    end type document_state_t
    
    ! Optimized LSP server with all performance enhancements
    type :: optimized_lsp_server_t
        type(lsp_hover_provider_t) :: hover_provider
        type(lsp_goto_definition_provider_t) :: goto_provider
        type(lsp_performance_monitor_t) :: global_monitor
        type(memory_pool_t) :: memory_pool
        
        ! Document management
        type(document_state_t), allocatable :: documents(:)
        integer :: document_count
        
        ! Configuration
        logical :: cache_enabled
        logical :: monitoring_enabled
        integer :: max_cache_size
        integer :: cleanup_interval
    contains
        procedure :: handle_initialize
        procedure :: handle_text_document_did_open
        procedure :: handle_text_document_did_change
        procedure :: handle_hover_request
        procedure :: handle_goto_definition_request
        procedure :: cleanup_resources
        procedure :: get_server_stats
    end type optimized_lsp_server_t
    
contains
    
    ! Create optimized LSP server
    function create_optimized_lsp_server(config) result(server)
        type(config_t), intent(in), optional :: config
        type(optimized_lsp_server_t) :: server
        
        ! Default configuration
        server%cache_enabled = .true.
        server%monitoring_enabled = .true.
        server%max_cache_size = 100
        server%cleanup_interval = 300  ! 5 minutes
        
        ! Initialize components
        server%hover_provider = create_hover_provider(server%cache_enabled, server%monitoring_enabled)
        server%goto_provider = create_goto_definition_provider(server%cache_enabled, server%monitoring_enabled)
        server%global_monitor = create_performance_monitor(server%monitoring_enabled)
        server%memory_pool = create_memory_pool(enabled=.true.)
        
        ! Initialize document storage
        allocate(server%documents(server%max_cache_size))
        server%document_count = 0
        
    end function create_optimized_lsp_server
    
    ! Handle LSP initialize request
    subroutine handle_initialize(this, capabilities)
        class(optimized_lsp_server_t), intent(inout) :: this
        character(len=:), allocatable, intent(out) :: capabilities
        
        type(lsp_timer_t) :: timer
        real :: elapsed_ms
        
        call start_timer(timer)
        
        capabilities = &
            "{" // &
            '"textDocumentSync": 1,' // &
            '"hoverProvider": true,' // &
            '"definitionProvider": true,' // &
            '"codeActionProvider": true' // &
            "}"
        
        call stop_timer(timer)
        elapsed_ms = get_elapsed_ms(timer)
        call this%global_monitor%record_operation("initialize", elapsed_ms)
        
    end subroutine handle_initialize
    
    ! Handle text document open
    subroutine handle_text_document_did_open(this, uri, content, version)
        class(optimized_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, content
        integer, intent(in) :: version
        
        type(lsp_timer_t) :: timer
        real :: elapsed_ms
        integer :: doc_idx
        
        call start_timer(timer)
        
        ! Find or create document entry
        doc_idx = find_document(this, uri)
        if (doc_idx == 0) then
            if (this%document_count < size(this%documents)) then
                this%document_count = this%document_count + 1
                doc_idx = this%document_count
            else
                ! Evict oldest document
                doc_idx = find_oldest_document(this)
            end if
        end if
        
        ! Store document state
        this%documents(doc_idx)%uri = uri
        this%documents(doc_idx)%content = content
        this%documents(doc_idx)%version = version
        this%documents(doc_idx)%is_dirty = .false.
        call system_clock(this%documents(doc_idx)%last_modified)
        
        ! Preload into caches
        call this%hover_provider%preload_file(uri, content)
        call this%goto_provider%build_symbol_index(uri, content)
        
        call stop_timer(timer)
        elapsed_ms = get_elapsed_ms(timer)
        call this%global_monitor%record_operation("did_open", elapsed_ms)
        
    end subroutine handle_text_document_did_open
    
    ! Handle text document change (incremental update)
    subroutine handle_text_document_did_change(this, uri, content, version)
        class(optimized_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, content
        integer, intent(in) :: version
        
        type(lsp_timer_t) :: timer
        real :: elapsed_ms
        integer :: doc_idx
        
        call start_timer(timer)
        
        ! Find document
        doc_idx = find_document(this, uri)
        if (doc_idx > 0) then
            ! Update document state
            this%documents(doc_idx)%content = content
            this%documents(doc_idx)%version = version
            this%documents(doc_idx)%is_dirty = .true.
            call system_clock(this%documents(doc_idx)%last_modified)
            
            ! Invalidate caches for this document
            call this%hover_provider%cache%invalidate(uri)
            call this%goto_provider%invalidate_index()
        end if
        
        call stop_timer(timer)
        elapsed_ms = get_elapsed_ms(timer)
        call this%global_monitor%record_operation("did_change", elapsed_ms)
        
    end subroutine handle_text_document_did_change
    
    ! Handle hover request
    subroutine handle_hover_request(this, uri, line, character, hover_result, success)
        class(optimized_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: uri
        integer, intent(in) :: line, character
        character(len=:), allocatable, intent(out) :: hover_result
        logical, intent(out) :: success
        
        type(lsp_timer_t) :: timer
        real :: elapsed_ms
        integer :: doc_idx
        
        call start_timer(timer)
        
        success = .false.
        
        ! Find document
        doc_idx = find_document(this, uri)
        if (doc_idx > 0) then
            call this%hover_provider%get_hover_info_optimized( &
                uri, this%documents(doc_idx)%content, line, character, hover_result, success)
        end if
        
        call stop_timer(timer)
        elapsed_ms = get_elapsed_ms(timer)
        call this%global_monitor%record_operation("hover_request", elapsed_ms)
        
    end subroutine handle_hover_request
    
    ! Handle goto definition request
    subroutine handle_goto_definition_request(this, uri, line, character, &
                                            result_uri, def_line, def_char, success)
        class(optimized_lsp_server_t), intent(inout) :: this
        character(len=*), intent(in) :: uri
        integer, intent(in) :: line, character
        character(len=:), allocatable, intent(out) :: result_uri
        integer, intent(out) :: def_line, def_char
        logical, intent(out) :: success
        
        type(lsp_timer_t) :: timer
        real :: elapsed_ms
        integer :: doc_idx
        
        call start_timer(timer)
        
        success = .false.
        
        ! Find document
        doc_idx = find_document(this, uri)
        if (doc_idx > 0) then
            call this%goto_provider%find_definition_optimized( &
                uri, this%documents(doc_idx)%content, line, character, &
                result_uri, def_line, def_char, success)
        end if
        
        call stop_timer(timer)
        elapsed_ms = get_elapsed_ms(timer)
        call this%global_monitor%record_operation("goto_definition_request", elapsed_ms)
        
    end subroutine handle_goto_definition_request
    
    ! Cleanup resources
    subroutine cleanup_resources(this)
        class(optimized_lsp_server_t), intent(inout) :: this
        
        type(lsp_timer_t) :: timer
        real :: elapsed_ms
        
        call start_timer(timer)
        
        ! Cleanup old cache entries
        call this%hover_provider%cache%cleanup_old_entries(this%cleanup_interval)
        call this%goto_provider%cache%cleanup_old_entries(this%cleanup_interval)
        
        ! Reset memory pool if needed
        if (this%memory_pool%get_stats()%current_usage > 10 * 1024 * 1024) then  ! 10MB
            call this%memory_pool%reset_pool()
        end if
        
        call stop_timer(timer)
        elapsed_ms = get_elapsed_ms(timer)
        call this%global_monitor%record_operation("cleanup", elapsed_ms)
        
    end subroutine cleanup_resources
    
    ! Get server performance statistics
    subroutine get_server_stats(this)
        class(optimized_lsp_server_t), intent(in) :: this
        
        type(memory_stats_t) :: mem_stats
        
        print *, ""
        print *, "=== Optimized LSP Server Statistics ==="
        
        print *, ""
        print *, "Global Performance:"
        call this%global_monitor%print_report()
        
        print *, ""
        print *, "Hover Provider Performance:"
        call this%hover_provider%get_performance_stats()
        
        print *, ""
        print *, "Goto Definition Provider Performance:"
        call this%goto_provider%get_performance_stats()
        
        print *, ""
        print *, "Memory Usage:"
        mem_stats = this%memory_pool%get_stats()
        print *, "  Total allocated: ", mem_stats%total_allocated, " bytes"
        print *, "  Peak usage: ", mem_stats%peak_usage, " bytes"
        print *, "  Current usage: ", mem_stats%current_usage, " bytes"
        print *, "  Allocations: ", mem_stats%allocation_count
        print *, "  Deallocations: ", mem_stats%deallocation_count
        
        print *, ""
        print *, "Document Management:"
        print *, "  Open documents: ", this%document_count
        print *, "  Cache enabled: ", this%cache_enabled
        print *, "  Monitoring enabled: ", this%monitoring_enabled
        
    end subroutine get_server_stats
    
    ! Find document by URI
    function find_document(this, uri) result(idx)
        type(optimized_lsp_server_t), intent(in) :: this
        character(len=*), intent(in) :: uri
        integer :: idx
        
        integer :: i
        
        idx = 0
        do i = 1, this%document_count
            if (allocated(this%documents(i)%uri)) then
                if (this%documents(i)%uri == uri) then
                    idx = i
                    return
                end if
            end if
        end do
        
    end function find_document
    
    ! Find oldest document for eviction
    function find_oldest_document(this) result(idx)
        type(optimized_lsp_server_t), intent(in) :: this
        integer :: idx
        
        integer :: i, oldest_time
        
        idx = 1
        oldest_time = huge(1)
        
        do i = 1, this%document_count
            if (this%documents(i)%last_modified < oldest_time) then
                oldest_time = this%documents(i)%last_modified
                idx = i
            end if
        end do
        
    end function find_oldest_document
    
end module fluff_lsp_server_optimized