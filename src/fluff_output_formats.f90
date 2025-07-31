module fluff_output_formats
    use fluff_core
    use fluff_diagnostics
    implicit none
    private
    
    public :: output_formatter_t
    public :: json_formatter_t
    public :: sarif_formatter_t
    public :: xml_formatter_t
    public :: github_actions_formatter_t
    public :: template_formatter_t
    public :: create_formatter
    public :: format_diagnostics
    public :: filter_options_t
    public :: template_config_t
    public :: sarif_metadata_t
    
    ! Filter options for output
    type :: filter_options_t
        character(len=:), allocatable :: severity_filter
        character(len=:), allocatable :: rule_codes(:)
        character(len=:), allocatable :: file_patterns(:)
        character(len=:), allocatable :: categories(:)
        integer :: line_start = 0
        integer :: line_end = 0
        logical :: include_fixes = .true.
    end type filter_options_t
    
    ! Template configuration
    type :: template_config_t
        character(len=:), allocatable :: template_file
        character(len=:), allocatable :: variables(:,:)  ! key-value pairs
        integer :: variable_count = 0
        logical :: strict_mode = .true.
    end type template_config_t
    
    ! SARIF metadata
    type :: sarif_metadata_t
        character(len=:), allocatable :: tool_name
        character(len=:), allocatable :: tool_version
        character(len=:), allocatable :: schema_uri
        character(len=:), allocatable :: semantic_version
    end type sarif_metadata_t
    
    ! Abstract output formatter interface
    type, abstract :: output_formatter_t
        type(filter_options_t) :: filters
        logical :: pretty_print = .false.
        logical :: include_metadata = .true.
        
    contains
        procedure(format_interface), deferred :: format_output
        procedure :: apply_filters
        procedure :: should_include_diagnostic
    end type output_formatter_t
    
    abstract interface
        function format_interface(this, diagnostics) result(output)
            import :: output_formatter_t, diagnostic_t
            class(output_formatter_t), intent(in) :: this
            type(diagnostic_t), intent(in) :: diagnostics(:)
            character(len=:), allocatable :: output
        end function format_interface
    end interface
    
    ! JSON formatter
    type, extends(output_formatter_t) :: json_formatter_t
        logical :: compact_mode = .false.
        
    contains
        procedure :: format_output => format_json_output
        procedure :: serialize_diagnostic_json
        procedure :: escape_json_string
        procedure :: format_json_array
    end type json_formatter_t
    
    ! SARIF formatter
    type, extends(output_formatter_t) :: sarif_formatter_t
        type(sarif_metadata_t) :: metadata
        
    contains
        procedure :: format_output => format_sarif_output
        procedure :: create_sarif_run
        procedure :: create_sarif_tool
        procedure :: create_sarif_result
        procedure :: map_severity_to_sarif
    end type sarif_formatter_t
    
    ! XML formatter
    type, extends(output_formatter_t) :: xml_formatter_t
        character(len=:), allocatable :: xml_format  ! "junit", "checkstyle", "generic"
        character(len=:), allocatable :: namespace_uri
        
    contains
        procedure :: format_output => format_xml_output
        procedure :: format_junit_xml
        procedure :: format_checkstyle_xml
        procedure :: format_generic_xml
        procedure :: escape_xml_string
        procedure :: create_xml_header
    end type xml_formatter_t
    
    ! GitHub Actions formatter
    type, extends(output_formatter_t) :: github_actions_formatter_t
        logical :: use_grouping = .true.
        
    contains
        procedure :: format_output => format_github_actions_output
        procedure :: format_github_annotation
        procedure :: get_github_command
        procedure :: group_by_file
    end type github_actions_formatter_t
    
    ! Template formatter
    type, extends(output_formatter_t) :: template_formatter_t
        type(template_config_t) :: template_config
        character(len=:), allocatable :: template_content
        
    contains
        procedure :: format_output => format_template_output
        procedure :: load_template
        procedure :: substitute_variables
        procedure :: process_conditionals
        procedure :: process_loops
        procedure :: validate_template
    end type template_formatter_t
    
contains
    
    ! Factory function to create formatters
    function create_formatter(format_type, options) result(formatter)
        character(len=*), intent(in) :: format_type
        character(len=*), intent(in), optional :: options
        class(output_formatter_t), allocatable :: formatter
        
        select case (trim(format_type))
        case ("json")
            allocate(json_formatter_t :: formatter)
            
        case ("sarif")
            allocate(sarif_formatter_t :: formatter)
            select type (formatter)
            type is (sarif_formatter_t)
                formatter%metadata%tool_name = "fluff"
                formatter%metadata%tool_version = "0.1.0"
                formatter%metadata%schema_uri = &
                    "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json"
                formatter%metadata%semantic_version = "2.1.0"
            end select
            
        case ("xml")
            allocate(xml_formatter_t :: formatter)
            select type (formatter)
            type is (xml_formatter_t)
                formatter%xml_format = "generic"
                if (present(options)) then
                    if (index(options, "junit") > 0) formatter%xml_format = "junit"
                    if (index(options, "checkstyle") > 0) formatter%xml_format = "checkstyle"
                end if
            end select
            
        case ("github")
            allocate(github_actions_formatter_t :: formatter)
            
        case ("template")
            allocate(template_formatter_t :: formatter)
            
        case default
            allocate(json_formatter_t :: formatter)  ! Default to JSON
        end select
        
    end function create_formatter
    
    ! Main formatting function
    function format_diagnostics(formatter, diagnostics) result(output)
        class(output_formatter_t), intent(in) :: formatter
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: output
        
        type(diagnostic_t), allocatable :: filtered_diagnostics(:)
        
        ! Apply filters
        filtered_diagnostics = formatter%apply_filters(diagnostics)
        
        ! Format output
        output = formatter%format_output(filtered_diagnostics)
        
    end function format_diagnostics
    
    ! Apply filtering to diagnostics
    function apply_filters(this, diagnostics) result(filtered)
        class(output_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        type(diagnostic_t), allocatable :: filtered(:)
        
        integer :: i, count, filtered_count
        
        ! Count diagnostics that pass filters
        count = size(diagnostics)
        filtered_count = 0
        
        do i = 1, count
            if (this%should_include_diagnostic(diagnostics(i))) then
                filtered_count = filtered_count + 1
            end if
        end do
        
        ! Create filtered array
        allocate(filtered(filtered_count))
        
        filtered_count = 0
        do i = 1, count
            if (this%should_include_diagnostic(diagnostics(i))) then
                filtered_count = filtered_count + 1
                ! Copy diagnostic properly handling allocatable components
                filtered(filtered_count)%code = diagnostics(i)%code
                filtered(filtered_count)%message = diagnostics(i)%message
                filtered(filtered_count)%category = diagnostics(i)%category
                filtered(filtered_count)%file_path = diagnostics(i)%file_path
                filtered(filtered_count)%severity = diagnostics(i)%severity
                filtered(filtered_count)%location = diagnostics(i)%location
            end if
        end do
        
    end function apply_filters
    
    ! Check if diagnostic should be included based on filters
    function should_include_diagnostic(this, diagnostic) result(include)
        class(output_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostic
        logical :: include
        
        include = .true.
        
        ! Severity filter
        if (allocated(this%filters%severity_filter)) then
            select case (trim(this%filters%severity_filter))
            case ("error")
                include = include .and. (diagnostic%severity == SEVERITY_ERROR)
            case ("warning")
                include = include .and. (diagnostic%severity <= SEVERITY_WARNING)
            case ("info")
                include = include .and. (diagnostic%severity <= SEVERITY_INFO)
            end select
        end if
        
        ! Rule code filter
        if (allocated(this%filters%rule_codes)) then
            if (size(this%filters%rule_codes) > 0) then
                include = include .and. any(this%filters%rule_codes == diagnostic%code)
            end if
        end if
        
        ! Line range filter
        if (this%filters%line_start > 0 .or. this%filters%line_end > 0) then
            if (this%filters%line_start > 0) then
                include = include .and. (diagnostic%location%start%line >= this%filters%line_start)
            end if
            if (this%filters%line_end > 0) then
                include = include .and. (diagnostic%location%start%line <= this%filters%line_end)
            end if
        end if
        
    end function should_include_diagnostic
    
    ! JSON formatter implementation
    function format_json_output(this, diagnostics) result(output)
        class(json_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: output
        
        character(len=:), allocatable :: json_array
        character(len=1000) :: temp_output
        integer :: i
        
        if (size(diagnostics) == 0) then
            output = "[]"
            return
        end if
        
        ! Build JSON array
        json_array = "["
        
        do i = 1, size(diagnostics)
            if (i > 1) json_array = json_array // ","
            
            if (this%pretty_print) json_array = json_array // new_line('a') // "  "
            
            json_array = json_array // this%serialize_diagnostic_json(diagnostics(i))
        end do
        
        if (this%pretty_print) json_array = json_array // new_line('a')
        json_array = json_array // "]"
        
        output = json_array
        
    end function format_json_output
    
    ! Serialize single diagnostic to JSON
    function serialize_diagnostic_json(this, diagnostic) result(json)
        class(json_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=:), allocatable :: json
        
        character(len=2000) :: temp_json
        character(len=:), allocatable :: escaped_message, escaped_file
        
        escaped_message = this%escape_json_string(diagnostic%message)
        escaped_file = this%escape_json_string(diagnostic%file_path)
        
        write(temp_json, '(A)') &
            '{"code":"' // trim(diagnostic%code) // '",' // &
            '"message":"' // escaped_message // '",' // &
            '"severity":"' // get_severity_name(diagnostic%severity) // '",' // &
            '"category":"' // trim(diagnostic%category) // '",' // &
            '"file":"' // escaped_file // '",' // &
            '"line":' // trim(int_to_string(diagnostic%location%start%line)) // ',' // &
            '"column":' // trim(int_to_string(diagnostic%location%start%column)) // '}'
        
        json = trim(temp_json)
        
    end function serialize_diagnostic_json
    
    ! Escape JSON string
    function escape_json_string(this, input) result(escaped)
        class(json_formatter_t), intent(in) :: this
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: escaped
        
        character(len=len(input)*2) :: temp_escaped
        integer :: i, j, input_len
        
        input_len = len_trim(input)
        j = 0
        
        do i = 1, input_len
            j = j + 1
            select case (input(i:i))
            case ('"')
                temp_escaped(j:j+1) = '\"'
                j = j + 1
            case ('\')
                temp_escaped(j:j+1) = '\\'
                j = j + 1
            case (char(10))  ! newline
                temp_escaped(j:j+1) = '\n'
                j = j + 1
            case (char(9))   ! tab
                temp_escaped(j:j+1) = '\t'
                j = j + 1
            case default
                temp_escaped(j:j) = input(i:i)
            end select
        end do
        
        escaped = temp_escaped(1:j)
        
    end function escape_json_string
    
    ! Format JSON array (helper)
    function format_json_array(this, items) result(array)
        class(json_formatter_t), intent(in) :: this
        character(len=*), intent(in) :: items(:)
        character(len=:), allocatable :: array
        
        integer :: i
        character(len=1000) :: temp_array
        
        temp_array = "["
        do i = 1, size(items)
            if (i > 1) temp_array = trim(temp_array) // ","
            temp_array = trim(temp_array) // '"' // trim(items(i)) // '"'
        end do
        temp_array = trim(temp_array) // "]"
        
        array = trim(temp_array)
        
    end function format_json_array
    
    ! SARIF formatter implementation
    function format_sarif_output(this, diagnostics) result(output)
        class(sarif_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: output
        
        character(len=5000) :: sarif_output
        character(len=:), allocatable :: tool_section, results_section
        integer :: i
        
        ! Create SARIF structure
        tool_section = this%create_sarif_tool()
        results_section = "["
        
        do i = 1, size(diagnostics)
            if (i > 1) results_section = results_section // ","
            results_section = results_section // this%create_sarif_result(diagnostics(i))
        end do
        
        results_section = results_section // "]"
        
        write(sarif_output, '(A)') &
            '{"version":"2.1.0",' // &
            '"$schema":"' // this%metadata%schema_uri // '",' // &
            '"runs":[' // this%create_sarif_run(tool_section, results_section) // ']}'
        
        output = trim(sarif_output)
        
    end function format_sarif_output
    
    ! Create SARIF tool section
    function create_sarif_tool(this) result(tool)
        class(sarif_formatter_t), intent(in) :: this
        character(len=:), allocatable :: tool
        
        character(len=1000) :: temp_tool
        
        write(temp_tool, '(A)') &
            '{"driver":{"name":"' // this%metadata%tool_name // '",' // &
            '"version":"' // this%metadata%tool_version // '",' // &
            '"semanticVersion":"' // this%metadata%semantic_version // '"}}'
        
        tool = trim(temp_tool)
        
    end function create_sarif_tool
    
    ! Create SARIF run section
    function create_sarif_run(this, tool_section, results_section) result(run)
        class(sarif_formatter_t), intent(in) :: this
        character(len=*), intent(in) :: tool_section, results_section
        character(len=:), allocatable :: run
        
        character(len=5000) :: temp_run
        
        write(temp_run, '(A)') &
            '{"tool":' // tool_section // ',' // &
            '"results":' // results_section // '}'
        
        run = trim(temp_run)
        
    end function create_sarif_run
    
    ! Create SARIF result
    function create_sarif_result(this, diagnostic) result(result)
        class(sarif_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=:), allocatable :: result
        
        character(len=2000) :: temp_result
        character(len=:), allocatable :: sarif_level
        
        sarif_level = this%map_severity_to_sarif(diagnostic%severity)
        
        write(temp_result, '(A)') &
            '{"ruleId":"' // trim(diagnostic%code) // '",' // &
            '"level":"' // sarif_level // '",' // &
            '"message":{"text":"' // trim(diagnostic%message) // '"},' // &
            '"locations":[{"physicalLocation":{"artifactLocation":' // &
            '{"uri":"' // trim(diagnostic%file_path) // '"},' // &
            '"region":{"startLine":' // trim(int_to_string(diagnostic%location%start%line)) // &
            ',"startColumn":' // trim(int_to_string(diagnostic%location%start%column)) // '}}}]}'
        
        result = trim(temp_result)
        
    end function create_sarif_result
    
    ! Map severity to SARIF level
    function map_severity_to_sarif(this, severity) result(sarif_level)
        class(sarif_formatter_t), intent(in) :: this
        integer, intent(in) :: severity
        character(len=:), allocatable :: sarif_level
        
        select case (severity)
        case (SEVERITY_ERROR)
            sarif_level = "error"
        case (SEVERITY_WARNING)
            sarif_level = "warning"
        case (SEVERITY_INFO)
            sarif_level = "note"
        case default
            sarif_level = "warning"
        end select
        
    end function map_severity_to_sarif
    
    ! XML formatter implementation
    function format_xml_output(this, diagnostics) result(output)
        class(xml_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: output
        
        select case (trim(this%xml_format))
        case ("junit")
            output = this%format_junit_xml(diagnostics)
        case ("checkstyle")
            output = this%format_checkstyle_xml(diagnostics)
        case default
            output = this%format_generic_xml(diagnostics)
        end select
        
    end function format_xml_output
    
    ! Format JUnit XML
    function format_junit_xml(this, diagnostics) result(output)
        class(xml_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: output
        
        character(len=5000) :: xml_output
        integer :: error_count, warning_count
        
        error_count = count(diagnostics%severity == SEVERITY_ERROR)
        warning_count = count(diagnostics%severity == SEVERITY_WARNING)
        
        write(xml_output, '(A)') &
            this%create_xml_header() // &
            '<testsuite name="fluff" tests="1" failures="' // &
            trim(int_to_string(error_count)) // '" errors="' // &
            trim(int_to_string(warning_count)) // '">' // &
            '<testcase name="linting" classname="fluff.check">' // &
            '</testcase></testsuite>'
        
        output = trim(xml_output)
        
    end function format_junit_xml
    
    ! Format CheckStyle XML
    function format_checkstyle_xml(this, diagnostics) result(output)
        class(xml_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: output
        
        character(len=5000) :: xml_output
        integer :: i
        
        xml_output = this%create_xml_header() // '<checkstyle version="fluff">'
        
        do i = 1, size(diagnostics)
            xml_output = trim(xml_output) // &
                '<file name="' // trim(diagnostics(i)%file_path) // '">' // &
                '<error line="' // trim(int_to_string(diagnostics(i)%location%start%line)) // '"' // &
                ' column="' // trim(int_to_string(diagnostics(i)%location%start%column)) // '"' // &
                ' severity="' // get_severity_name(diagnostics(i)%severity) // '"' // &
                ' message="' // this%escape_xml_string(diagnostics(i)%message) // '"' // &
                ' source="' // trim(diagnostics(i)%code) // '"/></file>'
        end do
        
        xml_output = trim(xml_output) // '</checkstyle>'
        output = trim(xml_output)
        
    end function format_checkstyle_xml
    
    ! Format generic XML
    function format_generic_xml(this, diagnostics) result(output)
        class(xml_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: output
        
        character(len=5000) :: xml_output
        integer :: i
        
        xml_output = this%create_xml_header() // '<fluff-results>'
        
        do i = 1, size(diagnostics)
            xml_output = trim(xml_output) // &
                '<diagnostic code="' // trim(diagnostics(i)%code) // '"' // &
                ' severity="' // get_severity_name(diagnostics(i)%severity) // '"' // &
                ' category="' // trim(diagnostics(i)%category) // '">' // &
                '<location file="' // trim(diagnostics(i)%file_path) // '"' // &
                ' line="' // trim(int_to_string(diagnostics(i)%location%start%line)) // '"' // &
                ' column="' // trim(int_to_string(diagnostics(i)%location%start%column)) // '"/>' // &
                '<message>' // this%escape_xml_string(diagnostics(i)%message) // '</message>' // &
                '</diagnostic>'
        end do
        
        xml_output = trim(xml_output) // '</fluff-results>'
        output = trim(xml_output)
        
    end function format_generic_xml
    
    ! Escape XML string
    function escape_xml_string(this, input) result(escaped)
        class(xml_formatter_t), intent(in) :: this
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: escaped
        
        character(len=len(input)*6) :: temp_escaped  ! Account for entity expansion
        integer :: i, j, input_len
        
        input_len = len_trim(input)
        j = 0
        
        do i = 1, input_len
            select case (input(i:i))
            case ('<')
                temp_escaped(j+1:j+4) = '&lt;'
                j = j + 4
            case ('>')
                temp_escaped(j+1:j+4) = '&gt;'
                j = j + 4
            case ('&')
                temp_escaped(j+1:j+5) = '&amp;'
                j = j + 5
            case ('"')
                temp_escaped(j+1:j+6) = '&quot;'
                j = j + 6
            case ("'")
                temp_escaped(j+1:j+6) = '&apos;'
                j = j + 6
            case default
                j = j + 1
                temp_escaped(j:j) = input(i:i)
            end select
        end do
        
        escaped = temp_escaped(1:j)
        
    end function escape_xml_string
    
    ! Create XML header
    function create_xml_header(this) result(header)
        class(xml_formatter_t), intent(in) :: this
        character(len=:), allocatable :: header
        
        header = '<?xml version="1.0" encoding="UTF-8"?>'
        
    end function create_xml_header
    
    ! GitHub Actions formatter implementation
    function format_github_actions_output(this, diagnostics) result(output)
        class(github_actions_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: output
        
        character(len=5000) :: gh_output
        integer :: i
        
        gh_output = ""
        
        if (this%use_grouping) then
            gh_output = "::group::Fluff Diagnostics" // new_line('a')
        end if
        
        do i = 1, size(diagnostics)
            gh_output = trim(gh_output) // this%format_github_annotation(diagnostics(i)) // new_line('a')
        end do
        
        if (this%use_grouping) then
            gh_output = trim(gh_output) // "::endgroup::" // new_line('a')
        end if
        
        output = trim(gh_output)
        
    end function format_github_actions_output
    
    ! Format GitHub Actions annotation
    function format_github_annotation(this, diagnostic) result(annotation)
        class(github_actions_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=:), allocatable :: annotation
        
        character(len=1000) :: temp_annotation
        character(len=:), allocatable :: gh_command
        
        gh_command = this%get_github_command(diagnostic%severity)
        
        write(temp_annotation, '(A)') &
            "::" // gh_command // " file=" // trim(diagnostic%file_path) // &
            ",line=" // trim(int_to_string(diagnostic%location%start%line)) // &
            ",col=" // trim(int_to_string(diagnostic%location%start%column)) // &
            ",title=" // trim(diagnostic%code) // "::" // trim(diagnostic%message)
        
        annotation = trim(temp_annotation)
        
    end function format_github_annotation
    
    ! Get GitHub Actions command for severity
    function get_github_command(this, severity) result(command)
        class(github_actions_formatter_t), intent(in) :: this
        integer, intent(in) :: severity
        character(len=:), allocatable :: command
        
        select case (severity)
        case (SEVERITY_ERROR)
            command = "error"
        case (SEVERITY_WARNING)
            command = "warning"
        case (SEVERITY_INFO)
            command = "notice"
        case default
            command = "notice"
        end select
        
    end function get_github_command
    
    ! Group diagnostics by file
    function group_by_file(this, diagnostics) result(grouped)
        class(github_actions_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        type(diagnostic_t), allocatable :: grouped(:)
        
        ! Simple implementation - just return as-is
        ! Real implementation would sort by file
        grouped = diagnostics
        
    end function group_by_file
    
    ! Template formatter implementation
    function format_template_output(this, diagnostics) result(output)
        class(template_formatter_t), intent(in) :: this
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: output
        
        character(len=:), allocatable :: processed_template
        
        if (.not. allocated(this%template_content)) then
            output = "Error: No template loaded"
            return
        end if
        
        processed_template = this%template_content
        processed_template = this%substitute_variables(processed_template, diagnostics)
        processed_template = this%process_conditionals(processed_template, diagnostics)
        processed_template = this%process_loops(processed_template, diagnostics)
        
        output = processed_template
        
    end function format_template_output
    
    ! Load template from file
    subroutine load_template(this, template_file)
        class(template_formatter_t), intent(inout) :: this
        character(len=*), intent(in) :: template_file
        
        ! Simplified template loading
        this%template_content = "Template: {{count}} diagnostics found" // new_line('a') // &
                               "{{#each diagnostics}}" // new_line('a') // &
                               "{{file}}:{{line}}: {{message}}" // new_line('a') // &
                               "{{/each}}"
        
    end subroutine load_template
    
    ! Substitute template variables
    function substitute_variables(this, template, diagnostics) result(substituted)
        class(template_formatter_t), intent(in) :: this
        character(len=*), intent(in) :: template
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: substituted
        
        character(len=len(template)*2) :: temp_template
        
        temp_template = template
        
        ! Replace {{count}} with diagnostic count
        call replace_string(temp_template, "{{count}}", int_to_string(size(diagnostics)))
        
        substituted = trim(temp_template)
        
    end function substitute_variables
    
    ! Process template conditionals
    function process_conditionals(this, template, diagnostics) result(processed)
        class(template_formatter_t), intent(in) :: this
        character(len=*), intent(in) :: template
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: processed
        
        ! Simplified - just return as-is
        processed = template
        
    end function process_conditionals
    
    ! Process template loops
    function process_loops(this, template, diagnostics) result(processed)
        class(template_formatter_t), intent(in) :: this
        character(len=*), intent(in) :: template
        type(diagnostic_t), intent(in) :: diagnostics(:)
        character(len=:), allocatable :: processed
        
        character(len=len(template)*10) :: temp_processed
        integer :: i, each_start, each_end
        character(len=:), allocatable :: loop_content, loop_result
        
        temp_processed = template
        
        ! Find {{#each diagnostics}} ... {{/each}} blocks
        each_start = index(temp_processed, "{{#each diagnostics}}")
        each_end = index(temp_processed, "{{/each}}")
        
        if (each_start > 0 .and. each_end > each_start) then
            loop_content = temp_processed(each_start+21:each_end-1)  ! Extract loop content
            loop_result = ""
            
            do i = 1, size(diagnostics)
                loop_result = loop_result // replace_diagnostic_vars(loop_content, diagnostics(i))
            end do
            
            ! Replace the entire loop with the result
            temp_processed = temp_processed(1:each_start-1) // loop_result // &
                           temp_processed(each_end+9:)
        end if
        
        processed = trim(temp_processed)
        
    end function process_loops
    
    ! Validate template syntax
    function validate_template(this) result(is_valid)
        class(template_formatter_t), intent(in) :: this
        logical :: is_valid
        
        ! Simplified validation
        is_valid = allocated(this%template_content) .and. len_trim(this%template_content) > 0
        
    end function validate_template
    
    ! Helper functions
    
    ! Get severity name
    function get_severity_name(severity) result(name)
        integer, intent(in) :: severity
        character(len=:), allocatable :: name
        
        select case (severity)
        case (SEVERITY_ERROR)
            name = "error"
        case (SEVERITY_WARNING)
            name = "warning"
        case (SEVERITY_INFO)
            name = "info"
        case default
            name = "unknown"
        end select
        
    end function get_severity_name
    
    ! Convert integer to string
    function int_to_string(value) result(str)
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        
        character(len=20) :: temp_str
        write(temp_str, '(I0)') value
        str = trim(temp_str)
        
    end function int_to_string
    
    ! Replace string in text
    subroutine replace_string(text, old_str, new_str)
        character(len=*), intent(inout) :: text
        character(len=*), intent(in) :: old_str, new_str
        
        integer :: pos
        
        pos = index(text, old_str)
        if (pos > 0) then
            text = text(1:pos-1) // new_str // text(pos+len(old_str):)
        end if
        
    end subroutine replace_string
    
    ! Replace diagnostic variables in template
    function replace_diagnostic_vars(template, diagnostic) result(replaced)
        character(len=*), intent(in) :: template
        type(diagnostic_t), intent(in) :: diagnostic
        character(len=:), allocatable :: replaced
        
        character(len=len(template)*2) :: temp_template
        
        temp_template = template
        
        call replace_string(temp_template, "{{file}}", diagnostic%file_path)
        call replace_string(temp_template, "{{line}}", int_to_string(diagnostic%location%start%line))
        call replace_string(temp_template, "{{column}}", int_to_string(diagnostic%location%start%column))
        call replace_string(temp_template, "{{message}}", diagnostic%message)
        call replace_string(temp_template, "{{code}}", diagnostic%code)
        call replace_string(temp_template, "{{severity}}", get_severity_name(diagnostic%severity))
        
        replaced = trim(temp_template)
        
    end function replace_diagnostic_vars
    
end module fluff_output_formats