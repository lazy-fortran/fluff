module fluff_incremental_types
    implicit none
    private

    public :: incremental_config_t
    public :: analysis_results_t
    public :: cache_stats_t
    public :: work_schedule_t
    public :: resource_stats_t
    public :: dependency_node_t
    public :: cached_result_t

    ! Configuration for incremental analysis
    type :: incremental_config_t
        logical :: enable_caching = .true.
        integer :: max_cache_size = 1000
        logical :: enable_parallel = .false.
        integer :: max_parallel_jobs = 4
        real :: cache_timeout_hours = 24.0
    end type incremental_config_t

    ! Analysis results
    type :: analysis_results_t
        integer :: file_count = 0
        integer :: error_count = 0
        integer :: warning_count = 0
        character(len=:), allocatable :: files(:)
        logical :: is_valid = .true.
    end type analysis_results_t

    ! Cache statistics
    type :: cache_stats_t
        real :: hit_rate = 0.0
        integer :: total_requests = 0
        integer :: cache_hits = 0
        integer :: cache_misses = 0
    end type cache_stats_t

    ! Work schedule for parallel processing
    type :: work_schedule_t
        integer :: task_count = 0
        character(len=:), allocatable :: tasks(:)
        integer, allocatable :: priorities(:)
    end type work_schedule_t

    ! Resource usage statistics
    type :: resource_stats_t
        integer :: memory_usage = 0
        real :: cpu_usage = 0.0
        integer :: active_jobs = 0
    end type resource_stats_t

    ! Dependency node
    type :: dependency_node_t
        character(len=:), allocatable :: file_path
        character(len=:), allocatable :: dependencies(:)
        integer :: dependency_count = 0
        logical :: is_up_to_date = .false.
        logical :: requires_analysis = .false.
    end type dependency_node_t

    ! Cached analysis result
    type :: cached_result_t
        character(len=:), allocatable :: file_path
        type(analysis_results_t) :: results
        integer :: timestamp
        logical :: is_valid = .true.
    end type cached_result_t

end module fluff_incremental_types
