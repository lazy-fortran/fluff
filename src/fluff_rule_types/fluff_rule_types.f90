module fluff_rule_types
    ! Common types for rules to avoid circular dependencies
    use fluff_core
    use fluff_ast
    use fluff_diagnostics
    implicit none
    private
    
    ! Rule information
    type, public :: rule_info_t
        character(len=:), allocatable :: code        ! e.g., "F001"
        character(len=:), allocatable :: name        ! e.g., "missing-implicit-none"
        character(len=:), allocatable :: description
        character(len=:), allocatable :: category    ! style, performance, correctness
        character(len=:), allocatable :: subcategory
        logical :: default_enabled = .true.
        logical :: fixable = .false.
        integer :: severity = SEVERITY_WARNING
        procedure(rule_check_interface), pointer, nopass :: check => null()
    contains
        procedure :: to_json => rule_to_json
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
    
contains
    
    ! Convert rule info to JSON
    function rule_to_json(this) result(json)
        class(rule_info_t), intent(in) :: this
        character(len=:), allocatable :: json
        
        character(len=1000) :: buffer
        character(len=10) :: severity_str
        character(len=5) :: fixable_str
        
        write(severity_str, '(I0)') this%severity
        if (this%fixable) then
            fixable_str = "true"
        else
            fixable_str = "false"
        end if
        
        buffer = '{"code": "' // trim(this%code) // '", ' // &
                 '"name": "' // trim(this%name) // '", ' // &
                 '"description": "' // trim(this%description) // '", ' // &
                 '"category": "' // trim(this%category) // '", ' // &
                 '"severity": ' // trim(severity_str) // ', ' // &
                 '"fixable": ' // trim(fixable_str) // '}'
            
        json = trim(buffer)
        
    end function rule_to_json
    
end module fluff_rule_types