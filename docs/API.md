# fluff API Documentation

This document describes the fluff API for programmatic usage and integration.

## Core Modules

### Main Entry Point

#### `fluff_linter`
Main linting engine that orchestrates rule execution.

```fortran
use fluff_linter
type(linter_t) :: linter
type(diagnostic_t), allocatable :: diagnostics(:)

! Initialize linter with configuration
call linter%initialize(config)

! Lint a file
call linter%lint_file("src/main.f90", diagnostics)

! Lint multiple files
call linter%lint_files(file_list, diagnostics)
```

### Configuration API

#### `fluff_config`
Configuration management with namelist support.

```fortran
use fluff_config
type(fluff_config_t) :: config
character(len=:), allocatable :: error_msg

! Create default configuration
config = create_default_config()

! Load from file
call config%from_file("fluff.toml", error_msg)

! Load from namelist string
namelist_str = "&fluff_config fix=.true. line_length=100 /"
call config%from_toml_string(namelist_str, error_msg)

! Apply CLI overrides
call config%from_cli_args(cli_overrides)

! Validate configuration
if (.not. config%validate(error_msg)) then
    print *, "Config error: ", error_msg
end if
```

**Configuration Properties:**
- `fix: logical` - Enable automatic fixing
- `show_fixes: logical` - Show fix suggestions
- `line_length: integer` - Maximum line length (40-200)
- `target_version: string` - "2008", "2018", or "2023"
- `output_format: string` - "text", "json", "sarif", "xml", "github"
- `rules: rule_selection_t` - Rule selection configuration

### Diagnostic API

#### `fluff_diagnostics`
Diagnostic results and fix suggestions.

```fortran
use fluff_diagnostics
type(diagnostic_t) :: diagnostic
type(fix_suggestion_t) :: fix

! Create diagnostic
diagnostic = create_diagnostic( &
    code="F001", &
    message="Missing 'implicit none' statement", &
    file_path="src/main.f90", &
    location=location, &
    severity=SEVERITY_WARNING)

! Add fix suggestion
fix%description = "Add 'implicit none' statement"
fix%is_safe = .true.
allocate(fix%edits(1))
fix%edits(1) = text_edit_t(range=edit_range, new_text="    implicit none")

allocate(diagnostic%fixes(1))
diagnostic%fixes(1) = fix

! Apply fix
call fix%apply(source_code, fixed_code)
```

**Severity Levels:**
- `SEVERITY_ERROR = 4`
- `SEVERITY_WARNING = 3`
- `SEVERITY_INFO = 2`
- `SEVERITY_HINT = 1`

### Rule System

#### `fluff_rules`
Rule registry and execution framework.

```fortran
use fluff_rule_types
use fluff_rules

type(rule_registry_t) :: registry
type(rule_info_t) :: rule_info

! Discover built-in rules
call registry%discover_builtin_rules()

! Register custom rule
rule_info%code = "C001"
rule_info%name = "custom-check"
rule_info%description = "My custom rule"
rule_info%category = "custom"
rule_info%severity = SEVERITY_WARNING
rule_info%fixable = .true.

call registry%register_rule(rule_info)

! Execute rules (serial or parallel)
call registry%execute_rules(ast_context, selection, diagnostics)
call registry%execute_rules_parallel(ast_context, selection, diagnostics)
```

**Built-in Rule Categories:**
- **F-rules (F001-F015)**: Style and formatting rules
- **P-rules (P001-P007)**: Performance optimization rules
- **W-rules**: General warnings
- **C-rules**: Correctness checks
- **S-rules**: Security checks

### AST Integration

#### `fluff_ast`
AST context wrapper around fortfront.

```fortran
use fluff_ast
use fortfront

type(fluff_ast_context_t) :: ast_ctx
character(len=:), allocatable :: source_code
integer :: root_node

! Initialize AST context
call ast_ctx%initialize()

! Parse source code
call ast_ctx%parse(source_code, success)

! Get root node
root_node = ast_ctx%get_root()

! Navigate AST
children = ast_ctx%get_children(node_index)
node_type = ast_ctx%get_node_type(node_index)
location = ast_ctx%get_node_location(node_index)

! Get semantic information
call ast_ctx%get_semantic_context(semantic_ctx)
```

### Output Formats

#### `fluff_output_formats`
Multiple output format support.

```fortran
use fluff_output_formats
class(output_formatter_t), allocatable :: formatter
character(len=:), allocatable :: output

! Create formatter
formatter = create_formatter("json")  ! or "sarif", "xml", "github", "template"

! Configure filters
formatter%filters%severity_filter = "error"
formatter%filters%line_start = 10
formatter%filters%line_end = 100

! Format diagnostics
output = format_diagnostics(formatter, diagnostics)

! JSON-specific options
select type (formatter)
type is (json_formatter_t)
    formatter%pretty_print = .true.
end select

! SARIF metadata
select type (formatter)
type is (sarif_formatter_t)
    formatter%metadata%tool_name = "fluff"
    formatter%metadata%tool_version = "0.1.0"
end select
```

### Language Server Protocol

#### `fluff_lsp_server`
Full LSP implementation.

```fortran
use fluff_lsp_server
type(lsp_server_t) :: server

! Initialize server
call server%initialize()

! Main message loop
do
    call server%handle_message(input_msg, output_msg)
    if (server%should_shutdown) exit
end do
```

**Supported LSP Methods:**
- `initialize` - Server capabilities
- `initialized` - Client ready notification
- `textDocument/didOpen` - File opened
- `textDocument/didChange` - File changed
- `textDocument/didSave` - File saved
- `textDocument/didClose` - File closed
- `textDocument/publishDiagnostics` - Push diagnostics
- `textDocument/hover` - Hover information
- `textDocument/codeAction` - Code actions/fixes
- `textDocument/formatting` - Format document
- `textDocument/definition` - Go to definition
- `workspace/didChangeConfiguration` - Config changed

### Caching System

#### `fluff_cache`
Analysis result caching for performance.

```fortran
use fluff_cache
type(analysis_cache_t) :: cache

! Initialize cache
call cache%initialize(max_entries=1000)

! Store result
call cache%put(file_path, checksum, analysis_result)

! Retrieve result
call cache%get(file_path, checksum, cached_result, found)

! Invalidate entry
call cache%invalidate(file_path)

! Clear all
call cache%clear()
```

### Incremental Analysis

#### `fluff_incremental_analyzer`
Analyze only changed portions of code.

```fortran
use fluff_incremental_analyzer
type(incremental_analyzer_t) :: analyzer
type(change_event_t) :: change

! Initialize analyzer
call analyzer%initialize(initial_files)

! Process file change
change%file_path = "src/main.f90"
change%change_type = CHANGE_TYPE_MODIFIED
call analyzer%process_change(change)

! Get files needing re-analysis
affected_files = analyzer%get_affected_files()
```

### Metrics and Statistics

#### `fluff_metrics`
Performance metrics and rule statistics.

```fortran
use fluff_metrics
type(metrics_collector_t) :: metrics
type(rule_metrics_t) :: rule_stats

! Start timing
call metrics%start_timer("analysis")

! Record rule execution
call metrics%record_rule_execution("F001", execution_time, violation_count)

! Stop timing
call metrics%stop_timer("analysis")

! Get report
report = metrics%generate_report()
print *, report
```

## Integration Examples

### GitHub Actions Integration

```fortran
use fluff_output_formats
type(github_actions_formatter_t) :: formatter
character(len=:), allocatable :: annotations

! Create GitHub formatter
formatter = create_formatter("github")

! Generate annotations
annotations = format_diagnostics(formatter, diagnostics)
! Output: ::error file=src/main.f90,line=10,col=5::Missing implicit none
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Get staged Fortran files
files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(f90|f95|f03|f08)$')

if [ -n "$files" ]; then
    # Run fluff with auto-fix
    fluff check --fix $files
    
    # Re-stage fixed files
    git add $files
fi
```

### Custom Rule Implementation

```fortran
module my_custom_rules
    use fluff_rule_types
    use fluff_ast
    implicit none
    
    type, extends(rule_check_t) :: check_my_style_t
    contains
        procedure :: invoke => check_my_style
    end type
    
contains
    
    subroutine check_my_style(this, ctx, node_index, violations)
        class(check_my_style_t), intent(in) :: this
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)
        
        integer :: node_type
        type(diagnostic_t) :: violation
        
        node_type = ctx%get_node_type(node_index)
        
        ! Your rule logic here
        if (node_type == NODE_FUNCTION_DEF) then
            ! Check something about functions
            violation = create_diagnostic( &
                code="C001", &
                message="Function violates custom style", &
                file_path="", &
                location=ctx%get_node_location(node_index), &
                severity=SEVERITY_WARNING)
            
            allocate(violations(1))
            violations(1) = violation
        else
            allocate(violations(0))
        end if
        
    end subroutine check_my_style
    
end module my_custom_rules
```

## Error Handling

All API functions use error codes and optional error messages:

```fortran
character(len=:), allocatable :: error_msg
logical :: success

! Most operations return success/failure
success = operation(args, error_msg)
if (.not. success) then
    print *, "Error: ", error_msg
end if

! Some operations use error codes
integer :: error_code
error_code = perform_operation(args)
select case (error_code)
case (0)
    ! Success
case (ERROR_FILE_NOT_FOUND)
    print *, "File not found"
case (ERROR_PARSE_FAILED)
    print *, "Parse error"
case default
    print *, "Unknown error"
end select
```

## Thread Safety

- Rule execution can be parallelized with OpenMP
- Cache operations are thread-safe with critical sections
- AST operations should be performed on separate contexts per thread
- Diagnostic collection uses proper synchronization

## Performance Considerations

1. **Use incremental analysis** for large codebases
2. **Enable caching** to avoid redundant parsing
3. **Use parallel rule execution** when checking many files
4. **Filter diagnostics** at the formatter level to reduce output
5. **Batch file operations** to minimize I/O overhead

## Version Compatibility

- Fortran 2008 or later required
- OpenMP 3.0+ for parallel execution
- fortfront AST library v0.1.0+
- fpm (Fortran Package Manager) for building