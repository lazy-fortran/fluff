module fluff_incremental_analyzer
    use fluff_core
    use fluff_lsp_performance
    use fluff_incremental_types
    use fortfront, only: ast_arena_t, semantic_context_t, token_t, &
                         lex_source, parse_tokens, analyze_semantics, &
                         create_ast_arena, create_semantic_context, &
                         get_identifiers_in_subtree
    implicit none
    private

    public :: incremental_analyzer_t
    public :: create_incremental_analyzer
    public :: incremental_config_t
    public :: analysis_results_t
    public :: cache_stats_t
    public :: work_schedule_t
    public :: resource_stats_t

    ! Incremental analyzer
    type :: incremental_analyzer_t
        logical :: initialized = .false.
        type(incremental_config_t) :: config

        type(dependency_node_t), allocatable :: nodes(:)
        integer :: node_count = 0
        logical :: dependency_tracking_enabled = .false.

        character(len=:), allocatable :: changed_files(:)
        integer :: changed_count = 0
        logical :: needs_full_rebuild = .false.

        type(cached_result_t), allocatable :: cache(:)
        integer :: cache_count = 0
        type(cache_stats_t) :: cache_stats

        type(lsp_performance_monitor_t) :: monitor

        logical :: parallel_enabled = .false.
        type(work_schedule_t) :: current_schedule
        type(resource_stats_t) :: resource_stats

    contains
        procedure :: is_initialized
        procedure :: initialize_dependency_tracking
        procedure :: is_dependency_tracking_enabled
        procedure :: build_dependency_graph
        procedure :: get_node_count
        procedure :: update_dependencies
        procedure :: is_up_to_date
        procedure :: add_dependency
        procedure :: has_circular_dependencies
        procedure :: get_transitive_dependencies
        procedure :: file_changed
        procedure :: get_affected_files
        procedure :: interface_changed
        procedure :: config_changed
        procedure :: requires_full_rebuild
        procedure :: get_files_to_analyze
        procedure :: mark_file_analyzed
        procedure :: merge_results
        procedure :: cache_results
        procedure :: has_cached_results
        procedure :: invalidate_cache
        procedure :: get_cache_stats
        procedure :: get_cache_memory_usage
        procedure :: enable_parallel_analysis
        procedure :: is_parallel_enabled
        procedure :: create_work_schedule
        procedure :: get_resource_stats

        procedure, private :: mark_file_for_analysis
    end type incremental_analyzer_t

contains

    function create_incremental_analyzer(config) result(analyzer)
        type(incremental_config_t), intent(in), optional :: config
        type(incremental_analyzer_t) :: analyzer

        analyzer%initialized = .true.

        if (present(config)) then
            analyzer%config = config
        else
            analyzer%config%enable_caching = .true.
            analyzer%config%max_cache_size = 1000
            analyzer%config%enable_parallel = .false.
        end if

        allocate (analyzer%nodes(100))
        allocate (character(len=256) :: analyzer%changed_files(50))
        allocate (analyzer%cache(analyzer%config%max_cache_size))

        analyzer%monitor = create_performance_monitor(.true.)

        analyzer%cache_stats%hit_rate = 0.0
        analyzer%cache_stats%total_requests = 0
        analyzer%cache_stats%cache_hits = 0
        analyzer%cache_stats%cache_misses = 0

        analyzer%resource_stats%memory_usage = 0
        analyzer%resource_stats%cpu_usage = 0.0
        analyzer%resource_stats%active_jobs = 0

    end function create_incremental_analyzer

    function is_initialized(this) result(initialized)
        class(incremental_analyzer_t), intent(in) :: this
        logical :: initialized

        initialized = this%initialized

    end function is_initialized

    subroutine initialize_dependency_tracking(this)
        class(incremental_analyzer_t), intent(inout) :: this

        this%dependency_tracking_enabled = .true.
        this%node_count = 0

    end subroutine initialize_dependency_tracking

    function is_dependency_tracking_enabled(this) result(enabled)
        class(incremental_analyzer_t), intent(in) :: this
        logical :: enabled

        enabled = this%dependency_tracking_enabled

    end function is_dependency_tracking_enabled

    subroutine build_dependency_graph(this, files)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: files(:)

        type(lsp_timer_t) :: timer
        real :: elapsed_ms
        integer :: i

        call start_timer(timer)

        this%node_count = 0

        do i = 1, size(files)
            if (this%node_count < size(this%nodes)) then
                this%node_count = this%node_count + 1
                this%nodes(this%node_count)%file_path = files(i)
                this%nodes(this%node_count)%dependency_count = 0
                this%nodes(this%node_count)%is_up_to_date = .false.
                this%nodes(this%node_count)%requires_analysis = .true.

                allocate (character(len=256) :: &
                          this%nodes(this%node_count)%dependencies(10))
            end if
        end do

        call stop_timer(timer)
        elapsed_ms = get_elapsed_ms(timer)
        call this%monitor%record_operation("build_dependency_graph", elapsed_ms)

    end subroutine build_dependency_graph

    function get_node_count(this) result(count)
        class(incremental_analyzer_t), intent(in) :: this
        integer :: count

        count = this%node_count

    end function get_node_count

    subroutine update_dependencies(this, file_path)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path

        type(ast_arena_t) :: arena
        type(semantic_context_t) :: semantic_ctx
        character(len=:), allocatable :: source_code, error_msg
        character(len=:), allocatable :: identifiers(:)
        type(token_t), allocatable :: tokens(:)
        integer :: root_index, i, j, file_unit
        logical :: file_exists

        inquire (file=file_path, exist=file_exists)
        if (.not. file_exists) then
            do i = 1, this%node_count
                if (allocated(this%nodes(i)%file_path)) then
                    if (this%nodes(i)%file_path == file_path) then
                        this%nodes(i)%is_up_to_date = .true.
                        this%nodes(i)%requires_analysis = .false.
                        exit
                    end if
                end if
            end do
            return
        end if

        open (newunit=file_unit, file=file_path, status='old', action='read')
        source_code = ""
        block
            character(len=1000) :: line
            integer :: ios
            do
                read (file_unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                source_code = source_code//trim(line)//new_line('a')
            end do
        end block
        close (file_unit)

        arena = create_ast_arena()
        call lex_source(source_code, tokens, error_msg)
        if (error_msg /= "") return

        call parse_tokens(tokens, arena, root_index, error_msg)
        if (error_msg /= "") return

        call create_semantic_context(semantic_ctx)
        call analyze_semantics(arena, root_index)

        identifiers = get_identifiers_in_subtree(arena, root_index)

        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                if (this%nodes(i)%file_path == file_path) then
                    this%nodes(i)%is_up_to_date = .true.
                    this%nodes(i)%dependency_count = size(identifiers)

                    j = 0
                    do while (j < size(identifiers) .and. j < &
                              size(this%nodes(i)%dependencies))
                        j = j + 1
                        this%nodes(i)%dependencies(j) = identifiers(j)
                    end do
                    this%nodes(i)%dependency_count = j
                    this%nodes(i)%requires_analysis = .false.
                    exit
                end if
            end if
        end do

    end subroutine update_dependencies

    function is_up_to_date(this, file_path) result(up_to_date)
        class(incremental_analyzer_t), intent(in) :: this
        character(len=*), intent(in) :: file_path
        logical :: up_to_date

        integer :: i

        up_to_date = .false.
        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                if (this%nodes(i)%file_path == file_path) then
                    up_to_date = this%nodes(i)%is_up_to_date
                    exit
                end if
            end if
        end do

    end function is_up_to_date

    subroutine add_dependency(this, dependent_file, dependency_file)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: dependent_file, dependency_file

        integer :: i, node_idx

        node_idx = 0
        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                if (this%nodes(i)%file_path == dependent_file) then
                    node_idx = i
                    exit
                end if
            end if
        end do

        if (node_idx == 0) then
            if (this%node_count < size(this%nodes)) then
                this%node_count = this%node_count + 1
                node_idx = this%node_count
                this%nodes(node_idx)%file_path = dependent_file
                allocate (character(len=256) :: this%nodes(node_idx)%dependencies(10))
                this%nodes(node_idx)%dependency_count = 0
                this%nodes(node_idx)%is_up_to_date = .false.
                this%nodes(node_idx)%requires_analysis = .false.
            end if
        end if

        block
            logical :: dep_exists
            dep_exists = .false.
            do i = 1, this%node_count
                if (allocated(this%nodes(i)%file_path)) then
                    if (this%nodes(i)%file_path == dependency_file) then
                        dep_exists = .true.
                        exit
                    end if
                end if
            end do

            if (.not. dep_exists) then
                if (this%node_count < size(this%nodes)) then
                    this%node_count = this%node_count + 1
                    this%nodes(this%node_count)%file_path = dependency_file
                    allocate (character(len=256) :: &
                              this%nodes(this%node_count)%dependencies(10))
                    this%nodes(this%node_count)%dependency_count = 0
                    this%nodes(this%node_count)%is_up_to_date = .false.
                    this%nodes(this%node_count)%requires_analysis = .false.
                end if
            end if
        end block

        if (node_idx > 0) then
            if (this%nodes(node_idx)%dependency_count < &
                size(this%nodes(node_idx)%dependencies)) then
                this%nodes(node_idx)%dependency_count = &
                    this%nodes(node_idx)%dependency_count + 1
                this%nodes(node_idx)%dependencies( &
                    this%nodes(node_idx)%dependency_count) = dependency_file
            end if
        end if

    end subroutine add_dependency

    function has_circular_dependencies(this) result(has_cycles)
        class(incremental_analyzer_t), intent(in) :: this
        logical :: has_cycles

        integer :: i, j, k, l

        has_cycles = .false.

        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                do j = 1, this%nodes(i)%dependency_count
                    if (.not. allocated(this%nodes(i)%dependencies)) cycle
                    if (j > size(this%nodes(i)%dependencies)) cycle
                    do k = 1, this%node_count
                        if (allocated(this%nodes(k)%file_path)) then
                            if (this%nodes(k)%file_path == &
                                this%nodes(i)%dependencies(j)) then
                                if (allocated(this%nodes(k)%dependencies)) then
                                    do l = 1, this%nodes(k)%dependency_count
                                        if (l <= size(this%nodes(k)%dependencies)) then
                                            if (this%nodes(k)%dependencies(l) == &
                                                this%nodes(i)%file_path) then
                                                has_cycles = .true.
                                                return
                                            end if
                                        end if
                                    end do
                                end if
                                if (any(this%nodes(k)%dependencies( &
                                    1:this%nodes(k)%dependency_count) == &
                                    this%nodes(i)%file_path)) then
                                    has_cycles = .true.
                                    return
                                end if
                            end if
                        end if
                    end do
                end do
            end if
        end do

    end function has_circular_dependencies

    function get_transitive_dependencies(this, file_path) result(deps)
        class(incremental_analyzer_t), intent(in) :: this
        character(len=*), intent(in) :: file_path
        character(len=:), allocatable :: deps(:)

        integer :: i, j, k, node_idx, count, max_deps
        character(len=256), allocatable :: temp_deps(:), queue(:)
        logical :: already_added
        integer :: queue_start, queue_end

        max_deps = this%node_count
        allocate (temp_deps(max_deps))
        allocate (queue(max_deps))
        count = 0
        queue_start = 1
        queue_end = 0

        node_idx = 0
        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                if (this%nodes(i)%file_path == file_path) then
                    node_idx = i
                    exit
                end if
            end if
        end do

        if (node_idx > 0) then
            do i = 1, this%nodes(node_idx)%dependency_count
                if (i <= size(this%nodes(node_idx)%dependencies)) then
                    queue_end = queue_end + 1
                    queue(queue_end) = this%nodes(node_idx)%dependencies(i)
                    count = count + 1
                    temp_deps(count) = this%nodes(node_idx)%dependencies(i)
                end if
            end do

            do while (queue_start <= queue_end)
                do i = 1, this%node_count
                    if (allocated(this%nodes(i)%file_path)) then
                        if (this%nodes(i)%file_path == queue(queue_start)) then
                            do j = 1, this%nodes(i)%dependency_count
                                if (j <= size(this%nodes(i)%dependencies)) then
                                    already_added = .false.
                                    do k = 1, count
                                        if (temp_deps(k) == &
                                            this%nodes(i)%dependencies(j)) then
                                            already_added = .true.
                                            exit
                                        end if
                                    end do

                                    if (.not. already_added .and. count < max_deps) then
                                        queue_end = queue_end + 1
                                        if (queue_end <= max_deps) then
                                            queue(queue_end) = &
                                                this%nodes(i)%dependencies(j)
                                        end if
                                        count = count + 1
                                        temp_deps(count) = this%nodes(i)%dependencies(j)
                                    end if
                                end if
                            end do
                            exit
                        end if
                    end if
                end do
                queue_start = queue_start + 1
            end do

            allocate (character(len=256) :: deps(count))
            do i = 1, count
                deps(i) = temp_deps(i)
            end do
        else
            allocate (character(len=1) :: deps(0))
        end if

    end function get_transitive_dependencies

    subroutine file_changed(this, file_path)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path

        integer :: i
        logical :: node_exists

        if (this%changed_count < size(this%changed_files)) then
            this%changed_count = this%changed_count + 1
            this%changed_files(this%changed_count) = file_path
        end if

        node_exists = .false.
        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                if (this%nodes(i)%file_path == file_path) then
                    node_exists = .true.
                    exit
                end if
            end if
        end do

        if (.not. node_exists) then
            if (this%node_count < size(this%nodes)) then
                this%node_count = this%node_count + 1
                this%nodes(this%node_count)%file_path = file_path
                allocate (character(len=256) :: &
                          this%nodes(this%node_count)%dependencies(10))
                this%nodes(this%node_count)%dependency_count = 0
                this%nodes(this%node_count)%is_up_to_date = .false.
                this%nodes(this%node_count)%requires_analysis = .true.
            end if
        else
            call this%mark_file_for_analysis(file_path)
        end if

    end subroutine file_changed

    function get_affected_files(this) result(files)
        class(incremental_analyzer_t), intent(in) :: this
        character(len=:), allocatable :: files(:)

        integer :: i, j, k, count, max_files
        character(len=256), allocatable :: temp_files(:)
        logical :: already_added

        max_files = this%changed_count + this%node_count
        allocate (temp_files(max_files))
        count = 0

        do i = 1, this%changed_count
            if (len_trim(this%changed_files(i)) > 0) then
                count = count + 1
                temp_files(count) = this%changed_files(i)
            end if
        end do

        do i = 1, this%changed_count
            if (len_trim(this%changed_files(i)) > 0) then
                do j = 1, this%node_count
                    if (allocated(this%nodes(j)%file_path) .and. &
                        allocated(this%nodes(j)%dependencies)) then
                        do k = 1, this%nodes(j)%dependency_count
                            if (k <= size(this%nodes(j)%dependencies)) then
                                if (this%nodes(j)%dependencies(k) == &
                                    this%changed_files(i)) then
                                    already_added = .false.
                                    block
                                        integer :: m
                                        do m = 1, count
                                            if (temp_files(m) == &
                                                this%nodes(j)%file_path) then
                                                already_added = .true.
                                                exit
                                            end if
                                        end do
                                    end block
                                    if (.not. already_added .and. count < &
                                        max_files) then
                                        count = count + 1
                                        temp_files(count) = this%nodes(j)%file_path
                                    end if
                                end if
                            end if
                        end do
                    end if
                end do
            end if
        end do

        allocate (character(len=256) :: files(max(count, 1)))
        if (count > 0) then
            do i = 1, count
                files(i) = temp_files(i)
            end do
        else
            files(1) = ""
        end if

    end function get_affected_files

    subroutine interface_changed(this, file_path)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path

        type(ast_arena_t) :: arena
        type(semantic_context_t) :: semantic_ctx
        character(len=:), allocatable :: source_code, error_msg
        character(len=:), allocatable :: current_interface(:), cached_interface(:)
        type(token_t), allocatable :: tokens(:)
        integer :: root_index, file_unit, i
        logical :: file_exists, interface_differs

        inquire (file=file_path, exist=file_exists)
        if (.not. file_exists) then
            this%needs_full_rebuild = .true.
            call this%file_changed(file_path)
            return
        end if

        open (newunit=file_unit, file=file_path, status='old', action='read')
        source_code = ""
        block
            character(len=1000) :: line
            integer :: ios
            do
                read (file_unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                source_code = source_code//trim(line)//new_line('a')
            end do
        end block
        close (file_unit)

        arena = create_ast_arena()
        call lex_source(source_code, tokens, error_msg)
        if (error_msg /= "") then
            this%needs_full_rebuild = .true.
            call this%file_changed(file_path)
            return
        end if

        call parse_tokens(tokens, arena, root_index, error_msg)
        if (error_msg /= "") then
            this%needs_full_rebuild = .true.
            call this%file_changed(file_path)
            return
        end if

        call create_semantic_context(semantic_ctx)
        call analyze_semantics(arena, root_index)

        current_interface = extract_interface_signatures(arena, root_index)

        cached_interface = get_cached_interface(this, file_path)
        interface_differs = .not. interfaces_equal(current_interface, cached_interface)

        if (interface_differs) then
            this%needs_full_rebuild = .true.
            call cache_interface(this, file_path, current_interface)
        end if

        call this%file_changed(file_path)

    end subroutine interface_changed

    subroutine config_changed(this, config_file)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: config_file

        this%needs_full_rebuild = .true.
        call this%file_changed(config_file)

    end subroutine config_changed

    function requires_full_rebuild(this) result(requires_full)
        class(incremental_analyzer_t), intent(in) :: this
        logical :: requires_full

        requires_full = this%needs_full_rebuild

    end function requires_full_rebuild

    function get_files_to_analyze(this) result(files)
        class(incremental_analyzer_t), intent(in) :: this
        character(len=:), allocatable :: files(:)

        integer :: i, count

        count = 0
        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                if (this%nodes(i)%requires_analysis) then
                    count = count + 1
                end if
            end if
        end do

        allocate (character(len=256) :: files(max(count, 1)))

        count = 0
        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                if (this%nodes(i)%requires_analysis) then
                    count = count + 1
                    files(count) = this%nodes(i)%file_path
                end if
            end if
        end do

        if (count == 0) then
            files(1) = ""
        end if

    end function get_files_to_analyze

    subroutine mark_file_analyzed(this, file_path)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path

        integer :: i

        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                if (this%nodes(i)%file_path == file_path) then
                    this%nodes(i)%requires_analysis = .false.
                    this%nodes(i)%is_up_to_date = .true.
                    exit
                end if
            end if
        end do

    end subroutine mark_file_analyzed

    subroutine merge_results(this, results1, results2, merged)
        class(incremental_analyzer_t), intent(inout) :: this
        type(analysis_results_t), intent(in) :: results1, results2
        type(analysis_results_t), intent(out) :: merged

        merged%file_count = results1%file_count + results2%file_count
        merged%error_count = results1%error_count + results2%error_count
        merged%warning_count = results1%warning_count + results2%warning_count
        merged%is_valid = results1%is_valid .and. results2%is_valid

    end subroutine merge_results

    subroutine cache_results(this, file_path, results)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        type(analysis_results_t), intent(in) :: results

        if (this%cache_count < size(this%cache)) then
            this%cache_count = this%cache_count + 1
            this%cache(this%cache_count)%file_path = file_path
            this%cache(this%cache_count)%results = results
            call system_clock(this%cache(this%cache_count)%timestamp)
            this%cache(this%cache_count)%is_valid = .true.
        end if

    end subroutine cache_results

    function has_cached_results(this, file_path) result(has_cache)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        logical :: has_cache

        integer :: i

        has_cache = .false.
        do i = 1, this%cache_count
            if (allocated(this%cache(i)%file_path)) then
                if (this%cache(i)%file_path == file_path .and. &
                    this%cache(i)%is_valid) then
                    has_cache = .true.
                    exit
                end if
            end if
        end do

        this%cache_stats%total_requests = this%cache_stats%total_requests + 1
        if (has_cache) then
            this%cache_stats%cache_hits = this%cache_stats%cache_hits + 1
        else
            this%cache_stats%cache_misses = this%cache_stats%cache_misses + 1
        end if

        if (this%cache_stats%total_requests > 0) then
            this%cache_stats%hit_rate = real(this%cache_stats%cache_hits)/ &
                                        real(this%cache_stats%total_requests)
        end if

    end function has_cached_results

    subroutine invalidate_cache(this, file_path)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path

        integer :: i

        do i = 1, this%cache_count
            if (allocated(this%cache(i)%file_path)) then
                if (this%cache(i)%file_path == file_path) then
                    this%cache(i)%is_valid = .false.
                    exit
                end if
            end if
        end do

    end subroutine invalidate_cache

    function get_cache_stats(this) result(stats)
        class(incremental_analyzer_t), intent(in) :: this
        type(cache_stats_t) :: stats

        stats = this%cache_stats

    end function get_cache_stats

    function get_cache_memory_usage(this) result(usage)
        class(incremental_analyzer_t), intent(in) :: this
        integer :: usage

        usage = this%cache_count*1024

    end function get_cache_memory_usage

    subroutine enable_parallel_analysis(this, enabled)
        class(incremental_analyzer_t), intent(inout) :: this
        logical, intent(in) :: enabled

        this%parallel_enabled = enabled
        this%config%enable_parallel = enabled

    end subroutine enable_parallel_analysis

    function is_parallel_enabled(this) result(enabled)
        class(incremental_analyzer_t), intent(in) :: this
        logical :: enabled

        enabled = this%parallel_enabled

    end function is_parallel_enabled

    function create_work_schedule(this) result(schedule)
        class(incremental_analyzer_t), intent(in) :: this
        type(work_schedule_t) :: schedule

        integer :: i, count

        count = 0
        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                if (this%nodes(i)%requires_analysis) then
                    count = count + 1
                end if
            end if
        end do

        schedule%task_count = count
        allocate (character(len=256) :: schedule%tasks(count))
        allocate (schedule%priorities(count))

        count = 0
        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                if (this%nodes(i)%requires_analysis) then
                    count = count + 1
                    schedule%tasks(count) = this%nodes(i)%file_path
                    schedule%priorities(count) = 1
                end if
            end if
        end do

    end function create_work_schedule

    function get_resource_stats(this) result(stats)
        class(incremental_analyzer_t), intent(in) :: this
        type(resource_stats_t) :: stats

        stats = this%resource_stats
        stats%memory_usage = this%get_cache_memory_usage() + (this%node_count*512)

    end function get_resource_stats

    subroutine mark_file_for_analysis(this, file_path)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path

        integer :: i, j

        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path)) then
                if (this%nodes(i)%file_path == file_path) then
                    this%nodes(i)%requires_analysis = .true.
                    this%nodes(i)%is_up_to_date = .false.
                    exit
                end if
            end if
        end do

        do i = 1, this%node_count
            if (allocated(this%nodes(i)%file_path) .and. &
                allocated(this%nodes(i)%dependencies)) then
                do j = 1, this%nodes(i)%dependency_count
                    if (j <= size(this%nodes(i)%dependencies)) then
                        if (this%nodes(i)%dependencies(j) == file_path) then
                            this%nodes(i)%requires_analysis = .true.
                            this%nodes(i)%is_up_to_date = .false.
                        end if
                    end if
                end do
            end if
        end do

    end subroutine mark_file_for_analysis

    function extract_interface_signatures(arena, root_index) result(signatures)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        character(len=:), allocatable :: signatures(:)

        character(len=:), allocatable :: identifiers(:)
        character(len=256) :: signature
        integer :: i, sig_count

        identifiers = get_identifiers_in_subtree(arena, root_index)

        sig_count = 0
        do i = 1, size(identifiers)
            if (is_public_interface_identifier(identifiers(i))) then
                sig_count = sig_count + 1
            end if
        end do

        allocate (character(len=256) :: signatures(max(sig_count, 1)))

        sig_count = 0
        do i = 1, size(identifiers)
            if (is_public_interface_identifier(identifiers(i))) then
                sig_count = sig_count + 1
                write (signature, '(A)') trim(identifiers(i))
                signatures(sig_count) = signature
            end if
        end do

        if (sig_count == 0) then
            signatures(1) = ""
        end if

    end function extract_interface_signatures

    function is_public_interface_identifier(identifier) result(is_public)
        character(len=*), intent(in) :: identifier
        logical :: is_public

        is_public = len_trim(identifier) > 0 .and. &
                    (index(identifier, 'function') > 0 .or. &
                     index(identifier, 'subroutine') > 0 .or. &
                     index(identifier, 'module') > 0)

    end function is_public_interface_identifier

    function get_cached_interface(this, file_path) result(interface)
        class(incremental_analyzer_t), intent(in) :: this
        character(len=*), intent(in) :: file_path
        character(len=:), allocatable :: interface(:)

        integer :: i

        do i = 1, this%cache_count
            if (allocated(this%cache(i)%file_path)) then
                if (this%cache(i)%file_path == file_path) then
                    allocate (character(len=256) :: interface(1))
                    interface(1) = "cached_interface"
                    return
                end if
            end if
        end do

        allocate (character(len=1) :: interface(0))

    end function get_cached_interface

    function interfaces_equal(interface1, interface2) result(equal)
        character(len=*), intent(in) :: interface1(:), interface2(:)
        logical :: equal

        integer :: i

        equal = .false.

        if (size(interface1) /= size(interface2)) return

        do i = 1, size(interface1)
            if (interface1(i) /= interface2(i)) return
        end do

        equal = .true.

    end function interfaces_equal

    subroutine cache_interface(this, file_path, interface)
        class(incremental_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        character(len=*), intent(in) :: interface(:)

        integer :: i, cache_slot

        cache_slot = 0
        do i = 1, this%cache_count
            if (allocated(this%cache(i)%file_path)) then
                if (this%cache(i)%file_path == file_path) then
                    cache_slot = i
                    exit
                end if
            end if
        end do

        if (cache_slot == 0 .and. this%cache_count < size(this%cache)) then
            this%cache_count = this%cache_count + 1
            cache_slot = this%cache_count
        end if

        if (cache_slot > 0) then
            this%cache(cache_slot)%file_path = file_path
            call system_clock(this%cache(cache_slot)%timestamp)
            this%cache(cache_slot)%is_valid = .true.
            this%cache(cache_slot)%results%file_count = size(interface)
        end if

    end subroutine cache_interface

end module fluff_incremental_analyzer
