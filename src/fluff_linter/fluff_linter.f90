module fluff_linter
    ! Linting engine and rule registry
    use fluff_core
    use fluff_ast
    use fluff_diagnostics
    implicit none
    private
    
    ! Rule registry for managing linting rules
    type, public :: rule_registry_t
        integer :: rule_count = 0
        ! TODO: Add rule storage
    contains
        procedure :: register_rule => registry_register_rule
        procedure :: get_enabled_rules => registry_get_enabled_rules
    end type rule_registry_t
    
    ! Rule information
    type, public :: rule_info_t
        character(len=:), allocatable :: code        ! e.g., "F001"
        character(len=:), allocatable :: name        ! e.g., "missing-implicit-none"
        character(len=:), allocatable :: description
        character(len=:), allocatable :: category    ! style, performance, correctness
        ! procedure(rule_check_interface), pointer, nopass :: check => null()
        ! TODO: Add procedure pointer when interface is working
    end type rule_info_t
    
    ! Abstract rule interface
    abstract interface
        subroutine rule_check_interface(ctx, node_index, violations)
            import :: fluff_ast_context_t, diagnostic_t
            type(fluff_ast_context_t), intent(in) :: ctx
            integer, intent(in) :: node_index
            type(diagnostic_t), allocatable, intent(out) :: violations(:)
        end subroutine rule_check_interface
    end interface
    
    ! Linter engine type
    type, public :: linter_engine_t
        logical :: is_initialized = .false.
        type(rule_registry_t) :: rule_registry
    contains
        procedure :: initialize => linter_initialize
        procedure :: lint_file => linter_lint_file
        procedure :: lint_ast => linter_lint_ast
    end type linter_engine_t
    
    ! Public procedures
    public :: create_linter_engine
    
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
        
        ! TODO: Register built-in rules
        
    end subroutine linter_initialize
    
    ! Lint a file
    subroutine linter_lint_file(this, filename, diagnostics, error_msg)
        class(linter_engine_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(diagnostic_t), allocatable, intent(out) :: diagnostics(:)
        character(len=:), allocatable, intent(out) :: error_msg
        
        ! TODO: Read file and lint
        allocate(diagnostics(0))
        error_msg = ""
        
    end subroutine linter_lint_file
    
    ! Lint an AST
    subroutine linter_lint_ast(this, ast_ctx, diagnostics)
        class(linter_engine_t), intent(in) :: this
        type(fluff_ast_context_t), intent(in) :: ast_ctx
        type(diagnostic_t), allocatable, intent(out) :: diagnostics(:)
        
        ! TODO: Run enabled rules on AST
        allocate(diagnostics(0))
        
    end subroutine linter_lint_ast
    
    ! Register a rule
    subroutine registry_register_rule(this, rule)
        class(rule_registry_t), intent(inout) :: this
        type(rule_info_t), intent(in) :: rule
        
        ! TODO: Add rule to registry
        this%rule_count = this%rule_count + 1
        
    end subroutine registry_register_rule
    
    ! Get enabled rules
    function registry_get_enabled_rules(this) result(rules)
        class(rule_registry_t), intent(in) :: this
        type(rule_info_t), allocatable :: rules(:)
        
        ! TODO: Return enabled rules based on configuration
        allocate(rules(0))
        
    end function registry_get_enabled_rules
    
end module fluff_linter