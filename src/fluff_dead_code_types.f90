module fluff_dead_code_types
    use fluff_core
    use fluff_diagnostics
    implicit none
    private

    public :: unused_variable_t
    public :: unreachable_code_t
    public :: dead_code_visitor_t

    ! Unused variable information
    type :: unused_variable_t
        character(len=:), allocatable :: variable_name
        character(len=:), allocatable :: scope_name
        integer :: declaration_line = 0
        integer :: declaration_column = 0
        logical :: is_parameter = .false.
        logical :: is_dummy_argument = .false.
        logical :: has_initialization = .false.
        character(len=:), allocatable :: variable_type
    contains
        procedure :: to_diagnostic => unused_variable_to_diagnostic
    end type unused_variable_t

    ! Unreachable code information
    type :: unreachable_code_t
        integer :: start_line = 0
        integer :: end_line = 0
        integer :: start_column = 0
        integer :: end_column = 0
        character(len=:), allocatable :: reason
        character(len=:), allocatable :: code_snippet
    contains
        procedure :: to_diagnostic => unreachable_code_to_diagnostic
    end type unreachable_code_t

    ! Dead code analyzer for AST-based analysis
    type :: dead_code_visitor_t
        type(unused_variable_t), allocatable :: unused_variables(:)
        type(unreachable_code_t), allocatable :: unreachable_code_blocks(:)
        integer :: unused_count = 0
        integer :: unreachable_count = 0
        character(len=:), allocatable :: declared_variables(:)
        character(len=:), allocatable :: used_variables(:)
        character(len=:), allocatable :: self_assigned_only(:)
        integer :: declared_count = 0
        integer :: used_count = 0
        integer :: self_assigned_count = 0
        logical :: after_terminating_statement = .false.
    contains
        procedure :: add_declared_variable => dc_add_declared_variable
        procedure :: add_used_variable => dc_add_used_variable
        procedure :: add_self_assigned_only => dc_add_self_assigned_only
        procedure :: is_variable_used => dc_is_variable_used
        procedure :: is_only_self_assigned => dc_is_only_self_assigned
        procedure :: add_unreachable_code => dc_add_unreachable_code
        procedure :: finalize_analysis => dc_finalize_analysis
        procedure :: clear => dc_clear
    end type dead_code_visitor_t

contains

    ! Unused variable diagnostic conversion
    function unused_variable_to_diagnostic(this) result(diag)
        class(unused_variable_t), intent(in) :: this
        type(diagnostic_t) :: diag

        type(source_range_t) :: location

        diag%code = "D001"
        diag%message = "Unused variable '"//this%variable_name//"'"
        diag%category = "dead_code"
        diag%severity = SEVERITY_WARNING

        location%start%line = this%declaration_line
        location%start%column = this%declaration_column
        location%end%line = this%declaration_line
        location%end%column = this%declaration_column + len(this%variable_name) - 1
        diag%location = location

        if (allocated(diag%fixes)) deallocate (diag%fixes)

    end function unused_variable_to_diagnostic

    ! Unreachable code diagnostic conversion
    function unreachable_code_to_diagnostic(this) result(diag)
        class(unreachable_code_t), intent(in) :: this
        type(diagnostic_t) :: diag

        type(source_range_t) :: location

        diag%code = "D002"
        diag%message = "Unreachable code detected ("//this%reason//")"
        diag%category = "dead_code"
        diag%severity = SEVERITY_WARNING

        location%start%line = this%start_line
        location%start%column = this%start_column
        location%end%line = this%end_line
        location%end%column = this%end_column
        diag%location = location

        if (allocated(diag%fixes)) deallocate (diag%fixes)

    end function unreachable_code_to_diagnostic

    subroutine dc_add_declared_variable(this, var_name)
        class(dead_code_visitor_t), intent(inout) :: this
        character(len=*), intent(in) :: var_name

        character(len=:), allocatable :: temp(:)
        integer :: n, max_len, i

        if (.not. allocated(this%declared_variables)) then
            allocate (character(len=len(var_name)) :: this%declared_variables(1))
            this%declared_variables(1) = var_name
            this%declared_count = 1
        else
            n = this%declared_count
            max_len = max(len(this%declared_variables), len(var_name))
            allocate (character(len=max_len) :: temp(n + 1))

            do i = 1, n
                temp(i) = this%declared_variables(i)
            end do
            temp(n + 1) = var_name

            call move_alloc(temp, this%declared_variables)
            this%declared_count = n + 1
        end if

    end subroutine dc_add_declared_variable

    subroutine dc_add_used_variable(this, var_name)
        class(dead_code_visitor_t), intent(inout) :: this
        character(len=*), intent(in) :: var_name

        character(len=:), allocatable :: temp(:)
        integer :: n, max_len, i

        if (.not. allocated(this%used_variables)) then
            allocate (character(len=len(var_name)) :: this%used_variables(1))
            this%used_variables(1) = var_name
            this%used_count = 1
        else
            n = this%used_count
            max_len = max(len(this%used_variables), len(var_name))
            allocate (character(len=max_len) :: temp(n + 1))

            do i = 1, n
                temp(i) = this%used_variables(i)
            end do
            temp(n + 1) = var_name

            call move_alloc(temp, this%used_variables)
            this%used_count = n + 1
        end if

    end subroutine dc_add_used_variable

    function dc_is_variable_used(this, var_name) result(used)
        class(dead_code_visitor_t), intent(in) :: this
        character(len=*), intent(in) :: var_name
        logical :: used
        integer :: i

        used = .false.
        if (allocated(this%used_variables)) then
            do i = 1, this%used_count
                if (this%used_variables(i) == var_name) then
                    used = .true.
                    return
                end if
            end do
        end if

    end function dc_is_variable_used

    subroutine dc_add_self_assigned_only(this, var_name)
        class(dead_code_visitor_t), intent(inout) :: this
        character(len=*), intent(in) :: var_name

        character(len=:), allocatable :: temp(:)
        integer :: n, max_len, i

        if (.not. allocated(this%self_assigned_only)) then
            allocate (character(len=len(var_name)) :: this%self_assigned_only(1))
            this%self_assigned_only(1) = var_name
            this%self_assigned_count = 1
        else
            n = this%self_assigned_count
            max_len = max(len(this%self_assigned_only), len(var_name))
            allocate (character(len=max_len) :: temp(n + 1))

            do i = 1, n
                temp(i) = this%self_assigned_only(i)
            end do
            temp(n + 1) = var_name

            call move_alloc(temp, this%self_assigned_only)
            this%self_assigned_count = n + 1
        end if

    end subroutine dc_add_self_assigned_only

    function dc_is_only_self_assigned(this, var_name) result(is_self_only)
        class(dead_code_visitor_t), intent(in) :: this
        character(len=*), intent(in) :: var_name
        logical :: is_self_only
        integer :: i

        is_self_only = .false.
        if (allocated(this%self_assigned_only)) then
            do i = 1, this%self_assigned_count
                if (this%self_assigned_only(i) == var_name) then
                    is_self_only = .true.
                    return
                end if
            end do
        end if

    end function dc_is_only_self_assigned

    subroutine dc_add_unreachable_code(this, start_line, end_line, start_col, end_col, &
                                       reason, snippet)
        class(dead_code_visitor_t), intent(inout) :: this
        integer, intent(in) :: start_line, end_line, start_col, end_col
        character(len=*), intent(in) :: reason, snippet

        type(unreachable_code_t), allocatable :: temp(:)
        integer :: n

        if (.not. allocated(this%unreachable_code_blocks)) then
            allocate (this%unreachable_code_blocks(1))
            n = 0
        else
            n = size(this%unreachable_code_blocks)
            allocate (temp(n + 1))
            temp(1:n) = this%unreachable_code_blocks
            call move_alloc(temp, this%unreachable_code_blocks)
        end if

        this%unreachable_code_blocks(n + 1)%start_line = start_line
        this%unreachable_code_blocks(n + 1)%end_line = end_line
        this%unreachable_code_blocks(n + 1)%start_column = start_col
        this%unreachable_code_blocks(n + 1)%end_column = end_col
        this%unreachable_code_blocks(n + 1)%reason = reason
        this%unreachable_code_blocks(n + 1)%code_snippet = snippet

        this%unreachable_count = n + 1

    end subroutine dc_add_unreachable_code

    subroutine dc_finalize_analysis(this)
        class(dead_code_visitor_t), intent(inout) :: this
        integer :: i

        if (allocated(this%declared_variables)) then
            do i = 1, this%declared_count
                if (.not. this%is_variable_used(this%declared_variables(i)) .or. &
                    this%is_only_self_assigned(this%declared_variables(i))) then
                    call add_unused_variable_to_visitor(this, &
                                                        this%declared_variables(i), &
                                                        "program", 1, 1, &
                                                        .false., .false.)
                end if
            end do
        end if

    end subroutine dc_finalize_analysis

    subroutine add_unused_variable_to_visitor(visitor, var_name, scope, line, col, &
                                              is_param, is_dummy)
        type(dead_code_visitor_t), intent(inout) :: visitor
        character(len=*), intent(in) :: var_name, scope
        integer, intent(in) :: line, col
        logical, intent(in) :: is_param, is_dummy

        type(unused_variable_t), allocatable :: temp(:)
        integer :: n

        if (.not. allocated(visitor%unused_variables)) then
            allocate (visitor%unused_variables(1))
            n = 1
        else
            n = size(visitor%unused_variables)
            allocate (temp(n + 1))
            temp(1:n) = visitor%unused_variables
            call move_alloc(temp, visitor%unused_variables)
            n = n + 1
        end if

        visitor%unused_variables(n)%variable_name = var_name
        visitor%unused_variables(n)%scope_name = scope
        visitor%unused_variables(n)%declaration_line = line
        visitor%unused_variables(n)%declaration_column = col
        visitor%unused_variables(n)%is_parameter = is_param
        visitor%unused_variables(n)%is_dummy_argument = is_dummy
        visitor%unused_count = visitor%unused_count + 1

    end subroutine add_unused_variable_to_visitor

    subroutine dc_clear(this)
        class(dead_code_visitor_t), intent(inout) :: this

        if (allocated(this%unused_variables)) deallocate (this%unused_variables)
        if (allocated(this%unreachable_code_blocks)) deallocate &
            (this%unreachable_code_blocks)
        if (allocated(this%declared_variables)) deallocate (this%declared_variables)
        if (allocated(this%used_variables)) deallocate (this%used_variables)
        if (allocated(this%self_assigned_only)) deallocate (this%self_assigned_only)

        this%unused_count = 0
        this%unreachable_count = 0
        this%declared_count = 0
        this%used_count = 0
        this%self_assigned_count = 0
        this%after_terminating_statement = .false.

    end subroutine dc_clear

end module fluff_dead_code_types
