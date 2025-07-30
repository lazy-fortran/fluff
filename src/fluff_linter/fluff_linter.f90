module fluff_linter
    ! Linting engine and rule registry
    use fluff_core
    use fluff_ast
    use fluff_diagnostics
    use fluff_rule_types
    use fluff_metrics, only: metrics_collector_t, timer_t, create_metrics_collector
    use fluff_cache, only: ast_cache_t, create_ast_cache
    implicit none
    private
    
    ! Rule registry for managing linting rules
    type, public :: rule_registry_t
        integer :: rule_count = 0
        type(rule_info_t), allocatable :: rules(:)
        integer :: capacity = 0
        type(metrics_collector_t) :: metrics
    contains
        procedure :: register_rule => registry_register_rule
        procedure :: get_enabled_rules => registry_get_enabled_rules
        procedure :: get_rule_count => registry_get_rule_count
        procedure :: discover_builtin_rules => registry_discover_builtin_rules
        procedure :: find_by_code => registry_find_by_code
        procedure :: get_rules_by_priority => registry_get_rules_by_priority
        procedure :: execute_rules => registry_execute_rules
        procedure :: execute_rules_parallel => registry_execute_rules_parallel
        procedure :: get_metrics_report => registry_get_metrics_report
        procedure :: reset_metrics => registry_reset_metrics
    end type rule_registry_t
    
    ! Re-export rule_info_t from fluff_rule_types
    public :: rule_info_t
    
    ! Rule context for easier access to AST and source
    type, public :: rule_context_t
        type(fluff_ast_context_t), pointer :: ast_ctx
        character(len=:), allocatable :: filename
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: lines(:)
    contains
        procedure :: get_filename => context_get_filename
        procedure :: get_source_line => context_get_source_line
        procedure :: get_total_lines => context_get_total_lines
    end type rule_context_t
    
    
    ! Linter engine type
    type, public :: linter_engine_t
        logical :: is_initialized = .false.
        type(rule_registry_t) :: rule_registry
        type(ast_cache_t) :: ast_cache
    contains
        procedure :: initialize => linter_initialize
        procedure :: lint_file => linter_lint_file
        procedure :: lint_ast => linter_lint_ast
    end type linter_engine_t
    
    ! Public procedures
    public :: create_linter_engine
    public :: create_rule_context
    public :: registry_get_metrics_report
    public :: registry_reset_metrics
    
contains
    
    ! Create a new linter engine
    function create_linter_engine() result(linter)
        type(linter_engine_t) :: linter
        call linter%initialize()
    end function create_linter_engine
    
    ! Initialize linter engine
    subroutine linter_initialize(this)
        class(linter_engine_t), intent(inout) :: this
        
        this%is_initialized = .true.
        this%rule_registry%rule_count = 0
        this%rule_registry%metrics = create_metrics_collector()
        this%ast_cache = create_ast_cache(max_size=50, ttl=600.0)  ! 10 minute TTL
        
        ! Register built-in rules
        call this%rule_registry%discover_builtin_rules()
        
    end subroutine linter_initialize
    
    ! Lint a file
    subroutine linter_lint_file(this, filename, diagnostics, error_msg)
        use iso_fortran_env, only: iostat_end
        class(linter_engine_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(diagnostic_t), allocatable, intent(out) :: diagnostics(:)
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(fluff_ast_context_t) :: ast_ctx
        character(len=:), allocatable :: source_code
        character(len=1000) :: line
        integer :: unit, iostat, cache_index
        
        ! Read file contents
        open(newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Failed to open file: " // filename
            allocate(diagnostics(0))
            return
        end if
        
        source_code = ""
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat == iostat_end) exit
            if (iostat /= 0) then
                close(unit)
                error_msg = "Failed to read file: " // filename
                allocate(diagnostics(0))
                return
            end if
            
            if (len(source_code) > 0) then
                source_code = source_code // new_line('a') // trim(line)
            else
                source_code = trim(line)
            end if
        end do
        close(unit)
        
        ! Check cache first
        cache_index = this%ast_cache%get(filename, source_code)
        
        if (cache_index > 0) then
            ! Use cached AST
            ast_ctx = this%ast_cache%get_ast(cache_index)
        else
            ! Parse AST
            call ast_ctx%from_source(source_code, error_msg)
            
            if (allocated(error_msg) .and. len(error_msg) > 0) then
                allocate(diagnostics(0))
                return
            end if
            
            ! Cache the parsed AST
            call this%ast_cache%put(filename, source_code, ast_ctx)
        end if
        
        ! Lint the AST
        call this%lint_ast(ast_ctx, diagnostics)
        
        error_msg = ""
        
    end subroutine linter_lint_file
    
    ! Lint an AST
    subroutine linter_lint_ast(this, ast_ctx, diagnostics)
        class(linter_engine_t), intent(inout) :: this
        type(fluff_ast_context_t), intent(in) :: ast_ctx
        type(diagnostic_t), allocatable, intent(out) :: diagnostics(:)
        
        ! Run enabled rules on AST
        call this%rule_registry%execute_rules(ast_ctx, diagnostics=diagnostics)
        
    end subroutine linter_lint_ast
    
    ! Register a rule
    subroutine registry_register_rule(this, rule, success)
        class(rule_registry_t), intent(inout) :: this
        type(rule_info_t), intent(in) :: rule
        logical, intent(out), optional :: success
        
        type(rule_info_t), allocatable :: temp(:)
        integer :: i
        
        if (present(success)) success = .true.
        
        ! Check for duplicate
        do i = 1, this%rule_count
            if (this%rules(i)%code == rule%code) then
                if (present(success)) success = .false.
                return
            end if
        end do
        
        ! Grow array if needed
        if (.not. allocated(this%rules)) then
            allocate(this%rules(10))
            this%capacity = 10
        else if (this%rule_count >= this%capacity) then
            allocate(temp(this%capacity * 2))
            temp(1:this%rule_count) = this%rules(1:this%rule_count)
            call move_alloc(temp, this%rules)
            this%capacity = this%capacity * 2
        end if
        
        ! Add rule
        this%rule_count = this%rule_count + 1
        this%rules(this%rule_count) = rule
        
    end subroutine registry_register_rule
    
    ! Get enabled rules
    function registry_get_enabled_rules(this, selection) result(rules)
        use fluff_config, only: rule_selection_t
        class(rule_registry_t), intent(in) :: this
        type(rule_selection_t), intent(in), optional :: selection
        type(rule_info_t), allocatable :: rules(:)
        
        integer :: i, count
        logical :: enabled
        
        ! Count enabled rules
        count = 0
        do i = 1, this%rule_count
            enabled = this%rules(i)%default_enabled
            if (present(selection)) then
                enabled = selection%is_rule_enabled(this%rules(i)%code)
            end if
            if (enabled) count = count + 1
        end do
        
        ! Allocate result
        allocate(rules(count))
        
        ! Copy enabled rules
        count = 0
        do i = 1, this%rule_count
            enabled = this%rules(i)%default_enabled
            if (present(selection)) then
                enabled = selection%is_rule_enabled(this%rules(i)%code)
            end if
            if (enabled) then
                count = count + 1
                rules(count) = this%rules(i)
            end if
        end do
        
    end function registry_get_enabled_rules
    
    ! Get rule count
    function registry_get_rule_count(this) result(count)
        class(rule_registry_t), intent(in) :: this
        integer :: count
        
        count = this%rule_count
        
    end function registry_get_rule_count
    
    ! Discover built-in rules
    subroutine registry_discover_builtin_rules(this)
        use fluff_rules, only: get_all_builtin_rules
        class(rule_registry_t), intent(inout) :: this
        
        type(rule_info_t), allocatable :: rules(:)
        integer :: i
        
        ! Get all built-in rules
        rules = get_all_builtin_rules()
        
        ! Register each rule
        do i = 1, size(rules)
            call this%register_rule(rules(i))
        end do
        
    end subroutine registry_discover_builtin_rules
    
    ! Find rule by code
    function registry_find_by_code(this, code) result(rule_ptr)
        class(rule_registry_t), intent(in), target :: this
        character(len=*), intent(in) :: code
        type(rule_info_t), pointer :: rule_ptr
        
        integer :: i
        
        rule_ptr => null()
        
        do i = 1, this%rule_count
            if (this%rules(i)%code == code) then
                rule_ptr => this%rules(i)
                return
            end if
        end do
        
    end function registry_find_by_code
    
    ! Get rules ordered by priority
    function registry_get_rules_by_priority(this) result(rules)
        class(rule_registry_t), intent(in) :: this
        type(rule_info_t), allocatable :: rules(:)
        
        integer :: i, j
        type(rule_info_t) :: temp
        
        ! Copy all rules
        allocate(rules(this%rule_count))
        rules = this%rules(1:this%rule_count)
        
        ! Sort by category priority (correctness > performance > style)
        do i = 1, this%rule_count - 1
            do j = i + 1, this%rule_count
                if (rule_priority(rules(j)%category) > rule_priority(rules(i)%category)) then
                    temp = rules(i)
                    rules(i) = rules(j)
                    rules(j) = temp
                end if
            end do
        end do
        
    end function registry_get_rules_by_priority
    
    ! Execute all enabled rules
    subroutine registry_execute_rules(this, ast_ctx, selection, diagnostics)
        use fluff_config, only: rule_selection_t
        class(rule_registry_t), intent(inout) :: this
        type(fluff_ast_context_t), intent(in) :: ast_ctx
        type(rule_selection_t), intent(in), optional :: selection
        type(diagnostic_t), allocatable, intent(out) :: diagnostics(:)
        
        type(rule_info_t), allocatable :: enabled_rules(:)
        type(diagnostic_t), allocatable :: rule_violations(:)
        type(diagnostic_t), allocatable :: all_violations(:)
        integer :: i, total_violations
        
        ! Get enabled rules
        enabled_rules = this%get_enabled_rules(selection)
        
        ! Initialize
        total_violations = 0
        allocate(all_violations(0))
        
        ! Execute each rule
        do i = 1, size(enabled_rules)
            if (associated(enabled_rules(i)%check)) then
                block
                    type(timer_t) :: rule_timer
                    ! Start timing
                    call this%metrics%start_rule(enabled_rules(i)%code, rule_timer)
                
                ! Execute rule
                call enabled_rules(i)%check(ast_ctx, 1, rule_violations)
                
                    ! End timing
                    if (allocated(rule_violations)) then
                        call this%metrics%end_rule(enabled_rules(i)%code, rule_timer, size(rule_violations))
                        ! Append violations
                        all_violations = [all_violations, rule_violations]
                        total_violations = total_violations + size(rule_violations)
                    else
                        call this%metrics%end_rule(enabled_rules(i)%code, rule_timer, 0)
                    end if
                end block
            end if
        end do
        
        ! Return all violations
        diagnostics = all_violations
        
    end subroutine registry_execute_rules
    
    ! Get rule priority
    function rule_priority(category) result(priority)
        character(len=*), intent(in) :: category
        integer :: priority
        
        select case (category)
        case ("correctness")
            priority = 3
        case ("performance")
            priority = 2
        case ("style")
            priority = 1
        case default
            priority = 0
        end select
        
    end function rule_priority
    
    ! Create rule context
    function create_rule_context(ast_ctx, filename, source_code) result(ctx)
        type(fluff_ast_context_t), intent(in), target :: ast_ctx
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: source_code
        type(rule_context_t) :: ctx
        
        integer :: i, line_start, line_count
        
        ctx%ast_ctx => ast_ctx
        ctx%filename = filename
        ctx%source_code = source_code
        
        ! Split source into lines
        line_count = count([(source_code(i:i) == new_line('a'), i=1, len(source_code))]) + 1
        allocate(character(len=200) :: ctx%lines(line_count))
        
        ! Simple line splitting
        line_start = 1
        line_count = 0
        do i = 1, len(source_code)
            if (source_code(i:i) == new_line('a')) then
                line_count = line_count + 1
                ctx%lines(line_count) = source_code(line_start:i-1)
                line_start = i + 1
            end if
        end do
        if (line_start <= len(source_code)) then
            line_count = line_count + 1
            ctx%lines(line_count) = source_code(line_start:)
        end if
        
    end function create_rule_context
    
    ! Rule context methods
    function context_get_filename(this) result(filename)
        class(rule_context_t), intent(in) :: this
        character(len=:), allocatable :: filename
        
        filename = this%filename
        
    end function context_get_filename
    
    function context_get_source_line(this, line_num) result(line)
        class(rule_context_t), intent(in) :: this
        integer, intent(in) :: line_num
        character(len=:), allocatable :: line
        
        if (line_num >= 1 .and. line_num <= size(this%lines)) then
            line = trim(this%lines(line_num))
        else
            line = ""
        end if
        
    end function context_get_source_line
    
    function context_get_total_lines(this) result(count)
        class(rule_context_t), intent(in) :: this
        integer :: count
        
        count = size(this%lines)
        
    end function context_get_total_lines
    
    ! Execute all enabled rules in parallel
    subroutine registry_execute_rules_parallel(this, ast_ctx, selection, diagnostics)
        use fluff_config, only: rule_selection_t
        !$ use omp_lib
        class(rule_registry_t), intent(inout) :: this
        type(fluff_ast_context_t), intent(in) :: ast_ctx
        type(rule_selection_t), intent(in), optional :: selection
        type(diagnostic_t), allocatable, intent(out) :: diagnostics(:)
        
        type(rule_info_t), allocatable :: enabled_rules(:)
        type(diagnostic_t), allocatable :: rule_violations(:)
        type(diagnostic_t), allocatable :: all_violations(:)
        integer :: i, total_violations
        integer :: num_threads
        
        ! Get enabled rules
        enabled_rules = this%get_enabled_rules(selection)
        
        if (size(enabled_rules) == 0) then
            allocate(diagnostics(0))
            return
        end if
        
        ! Determine number of threads
        num_threads = 1
        !$ num_threads = omp_get_max_threads()
        
        ! Fall back to serial execution for small rule sets or no OpenMP
        if (size(enabled_rules) < 3 .or. num_threads == 1) then
            call this%execute_rules(ast_ctx, selection, diagnostics)
            return
        end if
        
        ! Initialize results
        allocate(all_violations(0))
        
        ! Execute rules in parallel with critical section for results
        !$omp parallel do private(i, rule_violations) shared(all_violations, enabled_rules, ast_ctx)
        do i = 1, size(enabled_rules)
            if (associated(enabled_rules(i)%check)) then
                call enabled_rules(i)%check(ast_ctx, 1, rule_violations)
                
                if (allocated(rule_violations)) then
                    !$omp critical
                    all_violations = [all_violations, rule_violations]
                    !$omp end critical
                end if
            end if
        end do
        !$omp end parallel do
        
        ! Return all violations
        diagnostics = all_violations
        
    end subroutine registry_execute_rules_parallel
    
    ! Get metrics report
    function registry_get_metrics_report(this) result(report)
        class(rule_registry_t), intent(in) :: this
        character(len=:), allocatable :: report
        
        report = this%metrics%report()
        
    end function registry_get_metrics_report
    
    ! Reset metrics
    subroutine registry_reset_metrics(this)
        class(rule_registry_t), intent(inout) :: this
        
        call this%metrics%reset()
        
    end subroutine registry_reset_metrics
    
end module fluff_linter