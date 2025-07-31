module fluff_lsp_performance
    use fluff_core
    use iso_fortran_env, only: int64
    implicit none
    private
    
    public :: lsp_timer_t
    public :: lsp_performance_monitor_t
    public :: start_timer, stop_timer, get_elapsed_ms
    public :: create_performance_monitor
    
    ! Timer for measuring execution time
    type :: lsp_timer_t
        integer(int64) :: start_time
        integer(int64) :: end_time
        logical :: is_running
    end type lsp_timer_t
    
    ! Performance statistics
    type :: lsp_operation_stats_t
        character(len=:), allocatable :: operation_name
        integer :: call_count
        real :: total_time_ms
        real :: min_time_ms
        real :: max_time_ms
        real :: avg_time_ms
    end type lsp_operation_stats_t
    
    ! Performance monitor
    type :: lsp_performance_monitor_t
        type(lsp_operation_stats_t), allocatable :: stats(:)
        integer :: operation_count
        logical :: enabled
    contains
        procedure :: record_operation
        procedure :: get_stats
        procedure :: reset_stats
        procedure :: print_report
    end type lsp_performance_monitor_t
    
contains
    
    ! Create a new performance monitor
    function create_performance_monitor(enabled) result(monitor)
        logical, intent(in), optional :: enabled
        type(lsp_performance_monitor_t) :: monitor
        
        monitor%operation_count = 0
        allocate(monitor%stats(100))  ! Pre-allocate for performance
        
        if (present(enabled)) then
            monitor%enabled = enabled
        else
            monitor%enabled = .true.
        end if
        
    end function create_performance_monitor
    
    ! Start a timer
    subroutine start_timer(timer)
        type(lsp_timer_t), intent(out) :: timer
        
        call system_clock(timer%start_time)
        timer%is_running = .true.
        timer%end_time = 0
        
    end subroutine start_timer
    
    ! Stop a timer
    subroutine stop_timer(timer)
        type(lsp_timer_t), intent(inout) :: timer
        
        if (timer%is_running) then
            call system_clock(timer%end_time)
            timer%is_running = .false.
        end if
        
    end subroutine stop_timer
    
    ! Get elapsed time in milliseconds
    function get_elapsed_ms(timer) result(elapsed_ms)
        type(lsp_timer_t), intent(in) :: timer
        real :: elapsed_ms
        integer(int64) :: count_rate, current_time
        
        if (timer%is_running) then
            call system_clock(current_time, count_rate)
            elapsed_ms = real(current_time - timer%start_time) / real(count_rate) * 1000.0
        else
            call system_clock(count_rate=count_rate)
            elapsed_ms = real(timer%end_time - timer%start_time) / real(count_rate) * 1000.0
        end if
        
    end function get_elapsed_ms
    
    ! Record an operation's performance
    subroutine record_operation(this, operation_name, time_ms)
        class(lsp_performance_monitor_t), intent(inout) :: this
        character(len=*), intent(in) :: operation_name
        real, intent(in) :: time_ms
        
        integer :: i, found_idx
        
        if (.not. this%enabled) return
        
        ! Find existing operation or create new entry
        found_idx = 0
        do i = 1, this%operation_count
            if (allocated(this%stats(i)%operation_name)) then
                if (this%stats(i)%operation_name == operation_name) then
                    found_idx = i
                    exit
                end if
            end if
        end do
        
        if (found_idx == 0) then
            ! New operation
            this%operation_count = this%operation_count + 1
            found_idx = this%operation_count
            
            this%stats(found_idx)%operation_name = operation_name
            this%stats(found_idx)%call_count = 0
            this%stats(found_idx)%total_time_ms = 0.0
            this%stats(found_idx)%min_time_ms = huge(1.0)
            this%stats(found_idx)%max_time_ms = 0.0
        end if
        
        ! Update statistics
        this%stats(found_idx)%call_count = this%stats(found_idx)%call_count + 1
        this%stats(found_idx)%total_time_ms = this%stats(found_idx)%total_time_ms + time_ms
        this%stats(found_idx)%min_time_ms = min(this%stats(found_idx)%min_time_ms, time_ms)
        this%stats(found_idx)%max_time_ms = max(this%stats(found_idx)%max_time_ms, time_ms)
        this%stats(found_idx)%avg_time_ms = this%stats(found_idx)%total_time_ms / &
                                           real(this%stats(found_idx)%call_count)
        
    end subroutine record_operation
    
    ! Get statistics for an operation
    function get_stats(this, operation_name) result(stats)
        class(lsp_performance_monitor_t), intent(in) :: this
        character(len=*), intent(in) :: operation_name
        type(lsp_operation_stats_t) :: stats
        
        integer :: i
        
        ! Initialize empty stats
        stats%operation_name = operation_name
        stats%call_count = 0
        stats%total_time_ms = 0.0
        stats%min_time_ms = 0.0
        stats%max_time_ms = 0.0
        stats%avg_time_ms = 0.0
        
        ! Find operation
        do i = 1, this%operation_count
            if (allocated(this%stats(i)%operation_name)) then
                if (this%stats(i)%operation_name == operation_name) then
                    stats = this%stats(i)
                    exit
                end if
            end if
        end do
        
    end function get_stats
    
    ! Reset all statistics
    subroutine reset_stats(this)
        class(lsp_performance_monitor_t), intent(inout) :: this
        
        integer :: i
        
        do i = 1, this%operation_count
            if (allocated(this%stats(i)%operation_name)) then
                deallocate(this%stats(i)%operation_name)
            end if
        end do
        
        this%operation_count = 0
        
    end subroutine reset_stats
    
    ! Print performance report
    subroutine print_report(this)
        class(lsp_performance_monitor_t), intent(in) :: this
        
        integer :: i
        
        print *, ""
        print *, "=== LSP Performance Report ==="
        print *, "Operation                    | Calls | Avg (ms) | Min (ms) | Max (ms) | Total (ms)"
        print *, "----------------------------|-------|----------|----------|----------|----------"
        
        do i = 1, this%operation_count
            if (allocated(this%stats(i)%operation_name)) then
                write(*, '(A28,A1,I6,A1,F9.3,A1,F9.3,A1,F9.3,A1,F10.3)') &
                    this%stats(i)%operation_name(1:min(28,len(this%stats(i)%operation_name))), &
                    "|", this%stats(i)%call_count, &
                    "|", this%stats(i)%avg_time_ms, &
                    "|", this%stats(i)%min_time_ms, &
                    "|", this%stats(i)%max_time_ms, &
                    "|", this%stats(i)%total_time_ms
            end if
        end do
        
        print *, ""
        
    end subroutine print_report
    
end module fluff_lsp_performance