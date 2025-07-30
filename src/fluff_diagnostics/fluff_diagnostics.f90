module fluff_diagnostics
    ! Error reporting and fixes
    use fluff_core
    implicit none
    private
    
    ! Diagnostic severity levels
    enum, bind(c)
        enumerator :: SEVERITY_ERROR = 1
        enumerator :: SEVERITY_WARNING = 2
        enumerator :: SEVERITY_INFO = 3
        enumerator :: SEVERITY_HINT = 4
    end enum
    
    ! Diagnostic type
    type, public :: diagnostic_t
        character(len=:), allocatable :: code        ! e.g., "F001"
        character(len=:), allocatable :: message
        character(len=:), allocatable :: category    ! style, performance, etc.
        integer :: severity = SEVERITY_WARNING
        type(source_range_t) :: location
        type(fix_suggestion_t), allocatable :: fixes(:)
    contains
        procedure :: to_string => diagnostic_to_string
        procedure :: to_json => diagnostic_to_json
    end type diagnostic_t
    
    ! Fix suggestion
    type, public :: fix_suggestion_t
        character(len=:), allocatable :: description
        type(text_edit_t), allocatable :: edits(:)
        logical :: is_safe = .true.
    contains
        procedure :: apply => fix_apply
    end type fix_suggestion_t
    
    ! Text edit for fixes
    type, public :: text_edit_t
        type(source_range_t) :: range
        character(len=:), allocatable :: new_text
    end type text_edit_t
    
    ! Diagnostic collection
    type, public :: diagnostic_collection_t
        type(diagnostic_t), allocatable :: diagnostics(:)
        integer :: count = 0
    contains
        procedure :: add => collection_add
        procedure :: clear => collection_clear
        procedure :: sort => collection_sort
        procedure :: to_json => collection_to_json
        procedure :: to_sarif => collection_to_sarif
    end type diagnostic_collection_t
    
    ! Public procedures
    public :: create_diagnostic
    public :: create_fix_suggestion
    public :: severity_to_string
    
contains
    
    ! Create a diagnostic
    function create_diagnostic(code, message, location, severity) result(diag)
        character(len=*), intent(in) :: code
        character(len=*), intent(in) :: message
        type(source_range_t), intent(in) :: location
        integer, intent(in), optional :: severity
        type(diagnostic_t) :: diag
        
        diag%code = code
        diag%message = message
        diag%location = location
        
        if (present(severity)) then
            diag%severity = severity
        else
            diag%severity = SEVERITY_WARNING
        end if
        
        ! Determine category from code prefix
        select case (code(1:1))
        case ("F")
            diag%category = "style"
        case ("P")
            diag%category = "performance"
        case ("C")
            diag%category = "correctness"
        case default
            diag%category = "general"
        end select
        
    end function create_diagnostic
    
    ! Create a fix suggestion
    function create_fix_suggestion(description, edits, is_safe) result(fix)
        character(len=*), intent(in) :: description
        type(text_edit_t), intent(in) :: edits(:)
        logical, intent(in), optional :: is_safe
        type(fix_suggestion_t) :: fix
        
        fix%description = description
        allocate(fix%edits, source=edits)
        
        if (present(is_safe)) then
            fix%is_safe = is_safe
        else
            fix%is_safe = .true.
        end if
        
    end function create_fix_suggestion
    
    ! Convert severity to string
    function severity_to_string(severity) result(str)
        integer, intent(in) :: severity
        character(len=:), allocatable :: str
        
        select case (severity)
        case (SEVERITY_ERROR)
            str = "error"
        case (SEVERITY_WARNING)
            str = "warning"
        case (SEVERITY_INFO)
            str = "info"
        case (SEVERITY_HINT)
            str = "hint"
        case default
            str = "unknown"
        end select
        
    end function severity_to_string
    
    ! Convert diagnostic to string
    function diagnostic_to_string(this) result(str)
        class(diagnostic_t), intent(in) :: this
        character(len=:), allocatable :: str
        character(len=100) :: buffer
        
        write(buffer, '(A,":",I0,":",I0,": ",A," [",A,"] ",A)') &
            "file", this%location%start%line, this%location%start%column, &
            severity_to_string(this%severity), this%code, this%message
            
        str = trim(buffer)
        
    end function diagnostic_to_string
    
    ! Convert diagnostic to JSON
    function diagnostic_to_json(this) result(json)
        class(diagnostic_t), intent(in) :: this
        character(len=:), allocatable :: json
        character(len=1000) :: buffer
        
        write(buffer, '("{",/"  ""code"": """,A,""",",/"  ""message"": """,A,""",",/"  ""severity"": """,A,""",",/"  ""category"": """,A,""",",/"  ""location"": {",/"    ""start"": {""line"": ",I0,", ""column"": ",I0,"},",/"    ""end"": {""line"": ",I0,", ""column"": ",I0,"}",/"  }",/"}")') &
            this%code, this%message, severity_to_string(this%severity), this%category, &
            this%location%start%line, this%location%start%column, &
            this%location%end%line, this%location%end%column
            
        json = trim(buffer)
        
    end function diagnostic_to_json
    
    ! Apply fix to source code
    subroutine fix_apply(this, source_code, fixed_code)
        class(fix_suggestion_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: fixed_code
        
        ! TODO: Implement fix application
        fixed_code = source_code
        
    end subroutine fix_apply
    
    ! Add diagnostic to collection
    subroutine collection_add(this, diagnostic)
        class(diagnostic_collection_t), intent(inout) :: this
        type(diagnostic_t), intent(in) :: diagnostic
        
        type(diagnostic_t), allocatable :: temp(:)
        
        if (.not. allocated(this%diagnostics)) then
            allocate(this%diagnostics(10))
        else if (this%count >= size(this%diagnostics)) then
            ! Grow array
            allocate(temp(size(this%diagnostics) * 2))
            temp(1:this%count) = this%diagnostics(1:this%count)
            call move_alloc(temp, this%diagnostics)
        end if
        
        this%count = this%count + 1
        this%diagnostics(this%count) = diagnostic
        
    end subroutine collection_add
    
    ! Clear collection
    subroutine collection_clear(this)
        class(diagnostic_collection_t), intent(inout) :: this
        this%count = 0
    end subroutine collection_clear
    
    ! Sort diagnostics by location
    subroutine collection_sort(this)
        class(diagnostic_collection_t), intent(inout) :: this
        
        ! TODO: Implement sorting by file and line number
        
    end subroutine collection_sort
    
    ! Convert collection to JSON
    function collection_to_json(this) result(json)
        class(diagnostic_collection_t), intent(in) :: this
        character(len=:), allocatable :: json
        
        integer :: i
        character(len=:), allocatable :: diag_json
        
        json = "["
        
        do i = 1, this%count
            if (i > 1) json = json // ","
            diag_json = this%diagnostics(i)%to_json()
            json = json // new_line('a') // "  " // diag_json
        end do
        
        json = json // new_line('a') // "]"
        
    end function collection_to_json
    
    ! Convert collection to SARIF format
    function collection_to_sarif(this) result(sarif)
        class(diagnostic_collection_t), intent(in) :: this
        character(len=:), allocatable :: sarif
        
        ! TODO: Implement SARIF format conversion
        sarif = '{"version": "2.1.0", "runs": []}'
        
    end function collection_to_sarif
    
end module fluff_diagnostics