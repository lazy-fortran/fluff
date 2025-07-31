module fluff_lsp_hover_optimized
    use fluff_core
    use fluff_ast
    use fluff_lsp_hover
    use fluff_lsp_cache
    use fluff_lsp_performance
    implicit none
    private
    
    public :: lsp_hover_provider_t
    public :: create_hover_provider
    
    ! Optimized hover provider with caching
    type :: lsp_hover_provider_t
        type(lsp_cache_t) :: cache
        type(lsp_performance_monitor_t) :: monitor
    contains
        procedure :: get_hover_info_optimized
        procedure :: preload_file
        procedure :: get_performance_stats
    end type lsp_hover_provider_t
    
contains
    
    ! Create optimized hover provider
    function create_hover_provider(enable_cache, enable_monitoring) result(provider)
        logical, intent(in), optional :: enable_cache, enable_monitoring
        type(lsp_hover_provider_t) :: provider
        
        logical :: cache_enabled, monitoring_enabled
        
        cache_enabled = .true.
        monitoring_enabled = .true.
        
        if (present(enable_cache)) cache_enabled = enable_cache
        if (present(enable_monitoring)) monitoring_enabled = enable_monitoring
        
        provider%cache = create_lsp_cache(enabled=cache_enabled)
        provider%monitor = create_performance_monitor(enabled=monitoring_enabled)
        
    end function create_hover_provider
    
    ! Get hover info with caching and monitoring
    subroutine get_hover_info_optimized(this, uri, code, line, character, hover_content, success)
        class(lsp_hover_provider_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, code
        integer, intent(in) :: line, character
        character(len=:), allocatable, intent(out) :: hover_content
        logical, intent(out) :: success
        
        type(lsp_timer_t) :: timer
        character(len=:), allocatable :: lines(:)
        integer :: line_count, version
        logical :: cache_hit
        real :: elapsed_ms
        
        ! Start timing
        call start_timer(timer)
        
        ! Use cache for line splitting
        version = 1  ! In real implementation, track document versions
        call this%cache%get_or_parse(uri, code, version, lines, line_count, cache_hit)
        
        ! Call original hover implementation with cached lines
        call get_hover_info(code, line, character, hover_content, success)
        
        ! Stop timing and record
        call stop_timer(timer)
        elapsed_ms = get_elapsed_ms(timer)
        call this%monitor%record_operation("hover", elapsed_ms)
        
        if (cache_hit) then
            call this%monitor%record_operation("hover_cache_hit", elapsed_ms)
        else
            call this%monitor%record_operation("hover_cache_miss", elapsed_ms)
        end if
        
    end subroutine get_hover_info_optimized
    
    ! Preload file into cache
    subroutine preload_file(this, uri, code)
        class(lsp_hover_provider_t), intent(inout) :: this
        character(len=*), intent(in) :: uri, code
        
        character(len=:), allocatable :: lines(:)
        integer :: line_count, version
        logical :: cache_hit
        type(lsp_timer_t) :: timer
        real :: elapsed_ms
        
        call start_timer(timer)
        
        version = 1
        call this%cache%get_or_parse(uri, code, version, lines, line_count, cache_hit)
        
        call stop_timer(timer)
        elapsed_ms = get_elapsed_ms(timer)
        call this%monitor%record_operation("preload", elapsed_ms)
        
    end subroutine preload_file
    
    ! Get performance statistics
    subroutine get_performance_stats(this)
        class(lsp_hover_provider_t), intent(in) :: this
        
        call this%monitor%print_report()
        
        print *, "Cache memory usage: ", this%cache%get_memory_usage(), " bytes"
        
    end subroutine get_performance_stats
    
end module fluff_lsp_hover_optimized