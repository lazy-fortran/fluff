module fluff_diagnostics
    ! Error reporting and fixes
    use fluff_core
    implicit none
    private
    
    ! Diagnostic severity levels (higher number = higher severity)
    enum, bind(c)
        enumerator :: SEVERITY_HINT = 1
        enumerator :: SEVERITY_INFO = 2
        enumerator :: SEVERITY_WARNING = 3
        enumerator :: SEVERITY_ERROR = 4
    end enum
    
    ! Output format constants
    enum, bind(c)
        enumerator :: OUTPUT_FORMAT_TEXT = 1
        enumerator :: OUTPUT_FORMAT_JSON = 2
        enumerator :: OUTPUT_FORMAT_SARIF = 3
        enumerator :: OUTPUT_FORMAT_XML = 4
    end enum
    
    ! Export severity levels
    public :: SEVERITY_ERROR, SEVERITY_WARNING, SEVERITY_INFO, SEVERITY_HINT
    
    ! Export output format constants
    public :: OUTPUT_FORMAT_TEXT, OUTPUT_FORMAT_JSON, OUTPUT_FORMAT_SARIF, OUTPUT_FORMAT_XML
    
    ! Diagnostic type
    type, public :: diagnostic_t
        character(len=:), allocatable :: code        ! e.g., "F001"
        character(len=:), allocatable :: message
        character(len=:), allocatable :: category    ! style, performance, etc.
        character(len=:), allocatable :: file_path   ! source file path
        integer :: severity = SEVERITY_WARNING
        type(source_range_t) :: location
        type(fix_suggestion_t), allocatable :: fixes(:)
    contains
        procedure :: to_string => diagnostic_to_string
        procedure :: to_json => diagnostic_to_json
        procedure :: print => diagnostic_print
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
    
    ! Diagnostic statistics
    type, public :: diagnostic_stats_t
        integer :: total_created = 0
        integer :: total_formatted = 0
        real :: total_creation_time = 0.0
        real :: total_formatting_time = 0.0
        integer :: cache_hits = 0
        integer :: cache_misses = 0
    contains
        procedure :: record_creation => stats_record_creation
        procedure :: record_formatting => stats_record_formatting
        procedure :: record_cache_hit => stats_record_cache_hit
        procedure :: record_cache_miss => stats_record_cache_miss
        procedure :: get_summary => stats_get_summary
    end type diagnostic_stats_t
    
    ! Diagnostic collection
    type, public :: diagnostic_collection_t
        type(diagnostic_t), allocatable :: diagnostics(:)
        integer :: count = 0
        type(diagnostic_stats_t) :: stats
    contains
        procedure :: add => collection_add
        procedure :: clear => collection_clear
        procedure :: sort => collection_sort
        procedure :: to_json => collection_to_json
        procedure :: to_sarif => collection_to_sarif
        procedure :: get_stats => collection_get_stats
        procedure :: get_count => collection_count
        procedure :: has_errors => collection_has_errors
    end type diagnostic_collection_t
    
    ! Public procedures
    public :: create_diagnostic
    public :: create_fix_suggestion
    public :: severity_to_string
    public :: format_diagnostic
    public :: format_diagnostic_with_source
    
contains
    
    ! Create a diagnostic
    function create_diagnostic(code, message, file_path, location, severity) result(diag)
        character(len=*), intent(in) :: code
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: file_path
        type(source_range_t), intent(in) :: location
        integer, intent(in), optional :: severity
        type(diagnostic_t) :: diag
        
        diag%code = code
        diag%message = message
        diag%file_path = file_path
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
        character(len=1000) :: buffer

        write(buffer, '(A,":",I0,":",I0,": ",A," [",A,"] ",A)') &
            this%file_path, this%location%start%line, this%location%start%column, &
            severity_to_string(this%severity), this%code, this%message

        str = trim(buffer)

    end function diagnostic_to_string
    
    ! Convert diagnostic to JSON
    function diagnostic_to_json(this) result(json)
        class(diagnostic_t), intent(in) :: this
        character(len=:), allocatable :: json
        character(len=20) :: line_str, col_str, end_line_str, end_col_str
        
        ! Convert integers to strings to avoid format issues
        write(line_str, '(I0)') this%location%start%line
        write(col_str, '(I0)') this%location%start%column
        write(end_line_str, '(I0)') this%location%end%line
        write(end_col_str, '(I0)') this%location%end%column
        
        json = '{' // new_line('a') // &
               '  "code": "' // this%code // '",' // new_line('a') // &
               '  "message": "' // this%message // '",' // new_line('a') // &
               '  "severity": "' // severity_to_string(this%severity) // '",' // new_line('a') // &
               '  "category": "' // this%category // '",' // new_line('a') // &
               '  "location": {' // new_line('a') // &
               '    "start": {"line": ' // trim(line_str) // ', "column": ' // trim(col_str) // '},' // new_line('a') // &
               '    "end": {"line": ' // trim(end_line_str) // ', "column": ' // trim(end_col_str) // '}' // new_line('a') // &
               '  }' // new_line('a') // &
               '}'
        
    end function diagnostic_to_json
    
    ! Print diagnostic to stdout
    subroutine diagnostic_print(this)
        class(diagnostic_t), intent(in) :: this

        character(len=:), allocatable :: severity_str

        select case (this%severity)
        case (SEVERITY_ERROR)
            severity_str = "ERROR"
        case (SEVERITY_WARNING)
            severity_str = "WARNING"
        case (SEVERITY_INFO)
            severity_str = "INFO"
        case (SEVERITY_HINT)
            severity_str = "HINT"
        end select

        if (allocated(this%code)) then
            print '(a,":",i0,":",i0," [",a,"] ",a," (",a,")")', &
                this%file_path, this%location%start%line, this%location%start%column, &
                severity_str, this%message, this%code
        else
            print '(a,":",i0,":",i0," [",a,"] ",a)', &
                this%file_path, this%location%start%line, this%location%start%column, &
                severity_str, this%message
        end if

    end subroutine diagnostic_print
    
    ! Apply fix to source code
    subroutine fix_apply(this, source_code, fixed_code)
        class(fix_suggestion_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: fixed_code
        
        character(len=:), allocatable :: temp_code, result_code
        integer :: i
        type(text_edit_t) :: edit
        
        ! Start with original source
        temp_code = source_code
        
        ! Apply each edit in forward order
        do i = 1, size(this%edits)
            edit = this%edits(i)
            call apply_single_edit(temp_code, edit, result_code)
            temp_code = result_code
        end do
        
        fixed_code = temp_code
        
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
        
        ! Implement sorting by file and line number using simple bubble sort
        integer :: i, j
        type(diagnostic_t) :: temp_diag
        logical :: swapped
        
        if (this%count <= 1) return
        
        ! Simple bubble sort - good enough for typical diagnostic counts
        do i = 1, this%count - 1
            swapped = .false.
            do j = 1, this%count - i
                ! Compare file names first, then line numbers
                if (should_swap_diagnostics(this%diagnostics(j), this%diagnostics(j+1))) then
                    ! Swap diagnostics
                    temp_diag = this%diagnostics(j)
                    this%diagnostics(j) = this%diagnostics(j+1)
                    this%diagnostics(j+1) = temp_diag
                    swapped = .true.
                end if
            end do
            if (.not. swapped) exit  ! Already sorted
        end do
        
    end subroutine collection_sort
    
    ! Convert collection to JSON using efficient string building
    function collection_to_json(this) result(json)
        class(diagnostic_collection_t), intent(in) :: this
        character(len=:), allocatable :: json

        character(len=:), allocatable :: parts(:)
        character(len=:), allocatable :: diag_json
        integer :: i, total_len, pos

        if (this%count == 0) then
            json = "[]"
            return
        end if

        allocate (character(len=4096) :: parts(this%count))
        total_len = 2

        do i = 1, this%count
            diag_json = this%diagnostics(i)%to_json()
            parts(i) = diag_json
            total_len = total_len + len(diag_json) + 5
        end do

        allocate (character(len=total_len) :: json)
        json(1:1) = "["
        pos = 2

        do i = 1, this%count
            if (i > 1) then
                json(pos:pos) = ","
                pos = pos + 1
            end if
            json(pos:pos) = new_line('a')
            pos = pos + 1
            json(pos:pos + 1) = "  "
            pos = pos + 2
            json(pos:pos + len_trim(parts(i)) - 1) = trim(parts(i))
            pos = pos + len_trim(parts(i))
        end do

        json(pos:pos) = new_line('a')
        pos = pos + 1
        json(pos:pos) = "]"
        json = json(1:pos)

    end function collection_to_json
    
    ! Convert collection to SARIF format using efficient string building
    function collection_to_sarif(this) result(sarif)
        class(diagnostic_collection_t), intent(in) :: this
        character(len=:), allocatable :: sarif

        character(len=:), allocatable :: parts(:)
        character(len=:), allocatable :: diag_sarif
        character(len=:), allocatable :: results_array
        integer :: i, total_len, pos

        if (this%count == 0) then
            sarif = '{"version": "2.1.0", "runs": [{"tool": {"driver": ' // &
                    '{"name": "fluff"}}, "results": []}]}'
            return
        end if

        allocate (character(len=4096) :: parts(this%count))
        total_len = 0

        do i = 1, this%count
            diag_sarif = format_diagnostic_sarif(this%diagnostics(i))
            parts(i) = diag_sarif
            total_len = total_len + len(diag_sarif) + 7
        end do

        allocate (character(len=total_len) :: results_array)
        pos = 1

        do i = 1, this%count
            if (i > 1) then
                results_array(pos:pos) = ","
                pos = pos + 1
            end if
            results_array(pos:pos) = new_line('a')
            pos = pos + 1
            results_array(pos:pos + 3) = "    "
            pos = pos + 4
            results_array(pos:pos + len_trim(parts(i)) - 1) = trim(parts(i))
            pos = pos + len_trim(parts(i))
        end do

        results_array = results_array(1:pos - 1)

        sarif = '{' // new_line('a') // &
                '  "version": "2.1.0",' // new_line('a') // &
                '  "runs": [{' // new_line('a') // &
                '    "tool": {' // new_line('a') // &
                '      "driver": {' // new_line('a') // &
                '        "name": "fluff",' // new_line('a') // &
                '        "version": "0.1.0",' // new_line('a') // &
                '        "informationUri": "https://github.com/krystophny/fluff"' // &
                new_line('a') // &
                '      }' // new_line('a') // &
                '    },' // new_line('a') // &
                '    "results": [' // results_array // new_line('a') // &
                '    ]' // new_line('a') // &
                '  }]' // new_line('a') // &
                '}'

    end function collection_to_sarif
    
    ! Format diagnostic based on output format
    function format_diagnostic(diagnostic, output_format) result(formatted)
        type(diagnostic_t), intent(in) :: diagnostic
        integer, intent(in) :: output_format
        character(len=:), allocatable :: formatted
        
        select case (output_format)
        case (OUTPUT_FORMAT_TEXT)
            formatted = format_diagnostic_text(diagnostic)
        case (OUTPUT_FORMAT_JSON)
            formatted = diagnostic%to_json()
        case (OUTPUT_FORMAT_SARIF)
            formatted = format_diagnostic_sarif(diagnostic)
        case (OUTPUT_FORMAT_XML)
            formatted = format_diagnostic_xml(diagnostic)
        case default
            formatted = diagnostic%to_string()
        end select
        
    end function format_diagnostic
    
    ! Format diagnostic with source context
    function format_diagnostic_with_source(diagnostic, source_lines, output_format) result(formatted)
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=*), intent(in) :: source_lines
        integer, intent(in) :: output_format
        character(len=:), allocatable :: formatted
        
        select case (output_format)
        case (OUTPUT_FORMAT_TEXT)
            formatted = format_diagnostic_text_with_source(diagnostic, source_lines)
        case (OUTPUT_FORMAT_JSON)
            formatted = format_diagnostic_json_with_source(diagnostic, source_lines)
        case (OUTPUT_FORMAT_SARIF)
            formatted = format_diagnostic_sarif_with_source(diagnostic, source_lines)
        case default
            formatted = format_diagnostic_text_with_source(diagnostic, source_lines)
        end select
        
    end function format_diagnostic_with_source
    
    ! Format diagnostic as text
    function format_diagnostic_text(diagnostic) result(formatted)
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=:), allocatable :: formatted
        character(len=1000) :: buffer
        character(len=:), allocatable :: severity_str
        
        severity_str = severity_to_string(diagnostic%severity)
        
        write(buffer, '(A,":",I0,":",I0,": ",A," [",A,"] ",A)') &
            diagnostic%file_path, diagnostic%location%start%line, diagnostic%location%start%column, &
            severity_str, diagnostic%code, diagnostic%message
        
        formatted = trim(buffer)
        
        ! Add fix suggestions if present  
        if (allocated(diagnostic%fixes)) then
            if (size(diagnostic%fixes) > 0) then
                formatted = formatted // new_line('a') // &
                          "  Fix: " // diagnostic%fixes(1)%description
            end if
        end if
        
    end function format_diagnostic_text
    
    ! Format diagnostic as text with source context
    function format_diagnostic_text_with_source(diagnostic, source_lines) result(formatted)
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=*), intent(in) :: source_lines
        character(len=:), allocatable :: formatted
        character(len=:), allocatable :: lines(:)
        character(len=1000) :: buffer
        integer :: target_line
        
        ! Split source into lines
        call split_lines(source_lines, lines)
        
        target_line = diagnostic%location%start%line
        
        ! Format basic diagnostic
        formatted = format_diagnostic_text(diagnostic)
        
        ! Add source context
        if (target_line > 0 .and. target_line <= size(lines)) then
            formatted = formatted // new_line('a')
            
            ! Show context lines (before)
            if (target_line > 1) then
                write(buffer, '(I4," | ",A)') target_line - 1, lines(target_line - 1)
                formatted = formatted // trim(buffer) // new_line('a')
            end if
            
            ! Show the problematic line
            write(buffer, '(I4," | ",A)') target_line, lines(target_line)
            formatted = formatted // trim(buffer) // new_line('a')
            
            ! Show context lines (after)
            if (target_line < size(lines)) then
                write(buffer, '(I4," | ",A)') target_line + 1, lines(target_line + 1)
                formatted = formatted // trim(buffer) // new_line('a')
            end if
        end if
        
    end function format_diagnostic_text_with_source
    
    ! Format diagnostic as SARIF
    function format_diagnostic_sarif(diagnostic) result(formatted)
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=:), allocatable :: formatted
        character(len=20) :: start_line_str, start_col_str, end_line_str, end_col_str
        
        ! Convert integers to strings
        write(start_line_str, '(I0)') diagnostic%location%start%line
        write(start_col_str, '(I0)') diagnostic%location%start%column
        write(end_line_str, '(I0)') diagnostic%location%end%line
        write(end_col_str, '(I0)') diagnostic%location%end%column
        
        formatted = '{' // new_line('a') // &
                   '  "ruleId": "' // diagnostic%code // '",' // new_line('a') // &
                   '  "message": {"text": "' // diagnostic%message // '"},' // new_line('a') // &
                   '  "level": "' // severity_to_sarif_level(diagnostic%severity) // '",' // new_line('a') // &
                   '  "locations": [{' // new_line('a') // &
                   '    "physicalLocation": {' // new_line('a') // &
                   '      "artifactLocation": {"uri": "' // diagnostic%file_path // '"},' // new_line('a') // &
                   '      "region": {"startLine": ' // trim(start_line_str) // &
                   ', "startColumn": ' // trim(start_col_str) // &
                   ', "endLine": ' // trim(end_line_str) // &
                   ', "endColumn": ' // trim(end_col_str) // '}' // new_line('a') // &
                   '    }' // new_line('a') // &
                   '  }]' // new_line('a') // &
                   '}'
        
    end function format_diagnostic_sarif
    
    ! Format diagnostic as SARIF with source
    function format_diagnostic_sarif_with_source(diagnostic, source_lines) result(formatted)
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=*), intent(in) :: source_lines
        character(len=:), allocatable :: formatted
        
        ! For now, just return basic SARIF - could be enhanced with source snippets
        formatted = format_diagnostic_sarif(diagnostic)
        
    end function format_diagnostic_sarif_with_source
    
    ! Format diagnostic as JSON with source
    function format_diagnostic_json_with_source(diagnostic, source_lines) result(formatted)
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=*), intent(in) :: source_lines
        character(len=:), allocatable :: formatted
        
        ! For now, just return basic JSON - could be enhanced with source snippets
        formatted = diagnostic%to_json()
        
    end function format_diagnostic_json_with_source
    
    ! Format diagnostic as XML
    function format_diagnostic_xml(diagnostic) result(formatted)
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=:), allocatable :: formatted
        character(len=1000) :: buffer
        
        write(buffer, '("<diagnostic code=""",A,""" severity=""",A,&
            &""" category=""",A,""">",/"  <message>",A,"</message>",/&
            &"  <location line=""",I0,""" column=""",I0,"""/>",/&
            &"</diagnostic>")') &
            diagnostic%code, severity_to_string(diagnostic%severity), &
            diagnostic%category, diagnostic%message, &
            diagnostic%location%start%line, diagnostic%location%start%column
        
        formatted = trim(buffer)
        
    end function format_diagnostic_xml
    
    ! Convert severity to SARIF level
    function severity_to_sarif_level(severity) result(level)
        integer, intent(in) :: severity
        character(len=:), allocatable :: level
        
        select case (severity)
        case (SEVERITY_ERROR)
            level = "error"
        case (SEVERITY_WARNING)
            level = "warning"
        case (SEVERITY_INFO)
            level = "note"
        case (SEVERITY_HINT)
            level = "note"
        case default
            level = "note"
        end select
        
    end function severity_to_sarif_level
    
    ! Split source text into lines
    subroutine split_lines(source_text, lines)
        character(len=*), intent(in) :: source_text
        character(len=:), allocatable, intent(out) :: lines(:)
        
        integer :: i, line_count, start_pos, end_pos, max_line_len
        character(len=:), allocatable :: temp_lines(:)
        
        if (len(source_text) == 0) then
            allocate(character(len=0) :: lines(1))
            lines(1) = ""
            return
        end if
        
        ! Count lines
        line_count = 1
        do i = 1, len(source_text)
            if (source_text(i:i) == new_line('a')) then
                line_count = line_count + 1
            end if
        end do
        
        ! Find maximum line length to allocate properly
        max_line_len = len(source_text)  ! Conservative upper bound
        
        ! Allocate with enough space
        allocate(character(len=max_line_len) :: temp_lines(line_count))
        
        ! Split into lines
        line_count = 0
        start_pos = 1
        
        do i = 1, len(source_text)
            if (source_text(i:i) == new_line('a') .or. i == len(source_text)) then
                line_count = line_count + 1
                if (source_text(i:i) == new_line('a')) then
                    end_pos = i - 1
                else
                    end_pos = i
                end if
                
                if (end_pos >= start_pos) then
                    temp_lines(line_count) = source_text(start_pos:end_pos)
                else
                    temp_lines(line_count) = ""
                end if
                
                start_pos = i + 1
            end if
        end do
        
        ! Copy to result with proper length
        allocate(character(len=max_line_len) :: lines(line_count))
        lines(1:line_count) = temp_lines(1:line_count)
        
    end subroutine split_lines
    
    ! Apply a single text edit to source code
    subroutine apply_single_edit(source_text, edit, result_text)
        character(len=*), intent(in) :: source_text
        type(text_edit_t), intent(in) :: edit
        character(len=:), allocatable, intent(out) :: result_text
        
        character(len=:), allocatable :: lines(:)
        character(len=:), allocatable :: line_text
        integer :: target_line, start_col, end_col
        
        ! Split source into lines
        call split_lines(source_text, lines)
        
        target_line = edit%range%start%line
        start_col = edit%range%start%column
        end_col = edit%range%end%column
        
        ! Apply edit to the target line
        if (target_line > 0 .and. target_line <= size(lines)) then
            line_text = lines(target_line)
            
            ! Handle insertion (start_col == end_col)
            if (start_col == end_col) then
                if (start_col == 1) then
                    ! Insert at beginning of line
                    lines(target_line) = edit%new_text // line_text
                else if (start_col > len(line_text)) then
                    ! Insert at end of line
                    lines(target_line) = line_text // edit%new_text
                else
                    ! Insert in middle of line
                    lines(target_line) = line_text(1:start_col-1) // edit%new_text // line_text(start_col:)
                end if
            else
                ! Handle replacement (start_col != end_col)
                if (start_col == 1 .and. end_col > len(line_text)) then
                    ! Replace entire line
                    lines(target_line) = edit%new_text
                else if (start_col == 1) then
                    ! Replace from beginning
                    if (end_col <= len(line_text)) then
                        lines(target_line) = edit%new_text // line_text(end_col+1:)
                    else
                        lines(target_line) = edit%new_text
                    end if
                else if (end_col > len(line_text)) then
                    ! Replace to end
                    lines(target_line) = line_text(1:start_col-1) // edit%new_text
                else
                    ! Replace middle section
                    lines(target_line) = line_text(1:start_col-1) // edit%new_text // line_text(end_col+1:)
                end if
            end if
        end if
        
        ! Reconstruct source text from lines
        call reconstruct_from_lines(lines, result_text)
        
    end subroutine apply_single_edit
    
    ! Reconstruct source text from lines array
    subroutine reconstruct_from_lines(lines, result_text)
        character(len=:), allocatable, intent(in) :: lines(:)
        character(len=:), allocatable, intent(out) :: result_text
        
        integer :: i, total_len
        
        ! Calculate total length needed
        total_len = 0
        do i = 1, size(lines)
            total_len = total_len + len(lines(i))
            if (i < size(lines)) total_len = total_len + 1  ! For newlines
        end do
        
        ! Allocate result
        allocate(character(len=total_len) :: result_text)
        
        ! Build result
        result_text = ""
        do i = 1, size(lines)
            result_text = result_text // lines(i)
            if (i < size(lines)) then
                result_text = result_text // new_line('a')
            end if
        end do
        
    end subroutine reconstruct_from_lines
    
    ! Statistics tracking procedures
    subroutine stats_record_creation(this, creation_time)
        class(diagnostic_stats_t), intent(inout) :: this
        real, intent(in) :: creation_time
        
        this%total_created = this%total_created + 1
        this%total_creation_time = this%total_creation_time + creation_time
        
    end subroutine stats_record_creation
    
    subroutine stats_record_formatting(this, formatting_time)
        class(diagnostic_stats_t), intent(inout) :: this
        real, intent(in) :: formatting_time
        
        this%total_formatted = this%total_formatted + 1
        this%total_formatting_time = this%total_formatting_time + formatting_time
        
    end subroutine stats_record_formatting
    
    subroutine stats_record_cache_hit(this)
        class(diagnostic_stats_t), intent(inout) :: this
        this%cache_hits = this%cache_hits + 1
    end subroutine stats_record_cache_hit
    
    subroutine stats_record_cache_miss(this)
        class(diagnostic_stats_t), intent(inout) :: this  
        this%cache_misses = this%cache_misses + 1
    end subroutine stats_record_cache_miss
    
    function stats_get_summary(this) result(summary)
        class(diagnostic_stats_t), intent(in) :: this
        character(len=:), allocatable :: summary
        character(len=500) :: buffer
        real :: avg_creation_time, avg_formatting_time, cache_hit_rate
        
        if (this%total_created > 0) then
            avg_creation_time = this%total_creation_time / this%total_created
        else
            avg_creation_time = 0.0
        end if
        
        if (this%total_formatted > 0) then
            avg_formatting_time = this%total_formatting_time / this%total_formatted
        else
            avg_formatting_time = 0.0
        end if
        
        if (this%cache_hits + this%cache_misses > 0) then
            cache_hit_rate = real(this%cache_hits) / (this%cache_hits + this%cache_misses) * 100.0
        else
            cache_hit_rate = 0.0
        end if
        
        ! Build summary using concatenation to avoid format issues
        summary = "Diagnostic Stats:" // new_line('a') // &
                 "  Created: " // trim(adjustl(int_to_string(this%total_created))) // &
                 " (" // trim(adjustl(real_to_string(avg_creation_time))) // "s avg)" // new_line('a') // &
                 "  Formatted: " // trim(adjustl(int_to_string(this%total_formatted))) // &
                 " (" // trim(adjustl(real_to_string(avg_formatting_time))) // "s avg)" // new_line('a') // &
                 "  Cache hits: " // trim(adjustl(int_to_string(this%cache_hits))) // &
                 " (" // trim(adjustl(real_to_string(cache_hit_rate))) // "%)"
            
        ! summary already set above
        
    end function stats_get_summary
    
    function collection_get_stats(this) result(stats)
        class(diagnostic_collection_t), intent(in) :: this
        type(diagnostic_stats_t) :: stats
        stats = this%stats
    end function collection_get_stats
    
    ! Get count of diagnostics in collection
    function collection_count(this) result(count)
        class(diagnostic_collection_t), intent(in) :: this
        integer :: count
        count = this%count
    end function collection_count
    
    ! Check if collection has error-level diagnostics
    function collection_has_errors(this) result(has_errors)
        class(diagnostic_collection_t), intent(in) :: this
        logical :: has_errors
        integer :: i
        
        has_errors = .false.
        do i = 1, this%count
            if (this%diagnostics(i)%severity == SEVERITY_ERROR) then
                has_errors = .true.
                return
            end if
        end do
    end function collection_has_errors
    
    ! Helper functions for string conversion
    function int_to_string(val) result(str)
        integer, intent(in) :: val
        character(len=:), allocatable :: str
        character(len=20) :: buffer
        write(buffer, '(I0)') val
        str = trim(buffer)
    end function int_to_string
    
    function real_to_string(val) result(str)
        real, intent(in) :: val
        character(len=:), allocatable :: str
        character(len=20) :: buffer
        write(buffer, '(F0.6)') val
        str = trim(buffer)
    end function real_to_string
    
    ! Helper function to determine if two diagnostics should be swapped
    function should_swap_diagnostics(diag1, diag2) result(should_swap)
        type(diagnostic_t), intent(in) :: diag1, diag2
        logical :: should_swap
        
        ! Compare file paths first
        if (diag1%file_path /= diag2%file_path) then
            should_swap = diag1%file_path > diag2%file_path
        else
            ! Same file, compare line numbers
            if (diag1%location%start%line /= diag2%location%start%line) then
                should_swap = diag1%location%start%line > diag2%location%start%line
            else
                ! Same line, compare column numbers
                should_swap = diag1%location%start%column > diag2%location%start%column
            end if
        end if
    end function should_swap_diagnostics
    
end module fluff_diagnostics