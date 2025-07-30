module fluff_metrics
    ! Performance metrics and statistics for rule execution
    use fluff_core
    implicit none
    private
    
    ! Rule execution statistics
    type, public :: rule_stats_t
        character(len=:), allocatable :: rule_code
        integer :: execution_count = 0
        integer :: violation_count = 0
        real :: total_time = 0.0
        real :: min_time = huge(0.0)
        real :: max_time = 0.0
        real :: avg_time = 0.0
    contains
        procedure :: update => stats_update
        procedure :: reset => stats_reset
        procedure :: to_string => stats_to_string
    end type rule_stats_t
    
    ! Metrics collector
    type, public :: metrics_collector_t
        type(rule_stats_t), allocatable :: rule_stats(:)
        integer :: rule_count = 0
        real :: total_execution_time = 0.0
        integer :: total_violations = 0
        logical :: enabled = .true.
    contains
        procedure :: start_rule => collector_start_rule
        procedure :: end_rule => collector_end_rule
        procedure :: get_stats => collector_get_stats
        procedure :: reset => collector_reset
        procedure :: report => collector_report
        procedure :: enable => collector_enable
        procedure :: disable => collector_disable
    end type metrics_collector_t
    
    ! Timing helper
    type, public :: timer_t
        real :: start_time = 0.0
        logical :: is_running = .false.
    contains
        procedure :: start => timer_start
        procedure :: stop => timer_stop
        procedure :: elapsed => timer_elapsed
    end type timer_t
    
    ! Public procedures
    public :: create_metrics_collector
    
contains
    
    ! Create a new metrics collector
    function create_metrics_collector() result(collector)
        type(metrics_collector_t) :: collector
        
        collector%rule_count = 0
        collector%total_execution_time = 0.0
        collector%total_violations = 0
        collector%enabled = .true.
        allocate(collector%rule_stats(0))
        
    end function create_metrics_collector
    
    ! Update rule statistics
    subroutine stats_update(this, execution_time, violations)
        class(rule_stats_t), intent(inout) :: this
        real, intent(in) :: execution_time
        integer, intent(in) :: violations
        
        this%execution_count = this%execution_count + 1
        this%violation_count = this%violation_count + violations
        this%total_time = this%total_time + execution_time
        this%min_time = min(this%min_time, execution_time)
        this%max_time = max(this%max_time, execution_time)
        
        if (this%execution_count > 0) then
            this%avg_time = this%total_time / real(this%execution_count)
        end if
        
    end subroutine stats_update
    
    ! Reset statistics
    subroutine stats_reset(this)
        class(rule_stats_t), intent(inout) :: this
        
        this%execution_count = 0
        this%violation_count = 0
        this%total_time = 0.0
        this%min_time = huge(0.0)
        this%max_time = 0.0
        this%avg_time = 0.0
        
    end subroutine stats_reset
    
    ! Convert stats to string
    function stats_to_string(this) result(str)
        class(rule_stats_t), intent(in) :: this
        character(len=:), allocatable :: str
        
        character(len=500) :: buffer
        
        write(buffer, '(A, ": executions=", I0, ", violations=", I0, &
            &", total_time=", F0.3, "s, avg_time=", F0.3, "ms")') &
            this%rule_code, this%execution_count, this%violation_count, &
            this%total_time, this%avg_time * 1000.0
            
        str = trim(buffer)
        
    end function stats_to_string
    
    ! Start timing a rule
    subroutine collector_start_rule(this, rule_code, timer)
        class(metrics_collector_t), intent(inout) :: this
        character(len=*), intent(in) :: rule_code
        type(timer_t), intent(out) :: timer
        
        if (this%enabled) then
            call timer%start()
        end if
        
    end subroutine collector_start_rule
    
    ! End timing a rule
    subroutine collector_end_rule(this, rule_code, timer, violations)
        class(metrics_collector_t), intent(inout) :: this
        character(len=*), intent(in) :: rule_code
        type(timer_t), intent(inout) :: timer
        integer, intent(in) :: violations
        
        real :: elapsed_time
        integer :: i
        logical :: found
        type(rule_stats_t), allocatable :: temp(:)
        
        if (.not. this%enabled) return
        
        elapsed_time = timer%elapsed()
        
        ! Find or create stats for this rule
        found = .false.
        do i = 1, this%rule_count
            if (this%rule_stats(i)%rule_code == rule_code) then
                call this%rule_stats(i)%update(elapsed_time, violations)
                found = .true.
                exit
            end if
        end do
        
        if (.not. found) then
            ! Add new rule stats
            allocate(temp(this%rule_count + 1))
            if (this%rule_count > 0) then
                temp(1:this%rule_count) = this%rule_stats
            end if
            
            this%rule_count = this%rule_count + 1
            temp(this%rule_count)%rule_code = rule_code
            call temp(this%rule_count)%update(elapsed_time, violations)
            
            call move_alloc(temp, this%rule_stats)
        end if
        
        this%total_execution_time = this%total_execution_time + elapsed_time
        this%total_violations = this%total_violations + violations
        
    end subroutine collector_end_rule
    
    ! Get statistics for a rule
    function collector_get_stats(this, rule_code) result(stats)
        class(metrics_collector_t), target, intent(in) :: this
        character(len=*), intent(in) :: rule_code
        type(rule_stats_t), pointer :: stats
        
        integer :: i
        
        stats => null()
        
        do i = 1, this%rule_count
            if (this%rule_stats(i)%rule_code == rule_code) then
                stats => this%rule_stats(i)
                return
            end if
        end do
        
    end function collector_get_stats
    
    ! Reset all metrics
    subroutine collector_reset(this)
        class(metrics_collector_t), intent(inout) :: this
        
        integer :: i
        
        do i = 1, this%rule_count
            call this%rule_stats(i)%reset()
        end do
        
        this%total_execution_time = 0.0
        this%total_violations = 0
        
    end subroutine collector_reset
    
    ! Generate metrics report
    function collector_report(this) result(report)
        class(metrics_collector_t), intent(in) :: this
        character(len=:), allocatable :: report
        
        character(len=2000) :: buffer
        integer :: i
        
        write(buffer, '("=== Rule Execution Metrics ===", A, &
            &"Total execution time: ", F0.3, "s", A, &
            &"Total violations found: ", I0, A, &
            &"Rules executed: ", I0)')  &
            new_line('a'), this%total_execution_time, new_line('a'), &
            this%total_violations, new_line('a'), this%rule_count
            
        report = trim(buffer)
        
        if (this%rule_count > 0) then
            report = report // new_line('a') // "Rule Statistics:"
            do i = 1, this%rule_count
                report = report // new_line('a') // "  " // this%rule_stats(i)%to_string()
            end do
        end if
        
    end function collector_report
    
    ! Enable metrics collection
    subroutine collector_enable(this)
        class(metrics_collector_t), intent(inout) :: this
        this%enabled = .true.
    end subroutine collector_enable
    
    ! Disable metrics collection
    subroutine collector_disable(this)
        class(metrics_collector_t), intent(inout) :: this
        this%enabled = .false.
    end subroutine collector_disable
    
    ! Timer methods
    subroutine timer_start(this)
        class(timer_t), intent(inout) :: this
        
        call cpu_time(this%start_time)
        this%is_running = .true.
        
    end subroutine timer_start
    
    subroutine timer_stop(this)
        class(timer_t), intent(inout) :: this
        
        this%is_running = .false.
        
    end subroutine timer_stop
    
    function timer_elapsed(this) result(elapsed)
        class(timer_t), intent(in) :: this
        real :: elapsed
        real :: current_time
        
        if (this%is_running) then
            call cpu_time(current_time)
            elapsed = current_time - this%start_time
        else
            elapsed = 0.0
        end if
        
    end function timer_elapsed
    
end module fluff_metrics