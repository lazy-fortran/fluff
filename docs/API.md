# API Documentation

This document describes the fluff API for programmatic usage.

## Core API

### Main Entry Points

#### `fluff_check(files, config)`
Analyze Fortran files and return diagnostics.

**Parameters:**
- `files: string[]` - List of file paths to analyze
- `config: FluffConfig` - Configuration object

**Returns:**
- `DiagnosticResult[]` - Array of diagnostic results

#### `fluff_format(content, config)`
Format Fortran code according to style rules.

**Parameters:**
- `content: string` - Fortran source code to format
- `config: FluffConfig` - Configuration object

**Returns:**
- `string` - Formatted source code

### Configuration API

#### `class FluffConfig`
Configuration container for fluff settings.

**Properties:**
- `line_length: integer` - Maximum line length (default: 88)
- `indent_width: integer` - Indentation width (default: 4)
- `target_dirs: string[]` - Directories to analyze
- `include_patterns: string[]` - File patterns to include
- `exclude_patterns: string[]` - File patterns to exclude
- `enabled_rules: string[]` - List of enabled rule codes
- `disabled_rules: string[]` - List of disabled rule codes

**Methods:**
- `load_from_file(path: string)` - Load configuration from TOML file
- `validate()` - Validate configuration settings
- `get_rule_config(rule_code: string)` - Get configuration for specific rule

### Diagnostic API

#### `class Diagnostic`
Represents a single diagnostic result.

**Properties:**
- `code: string` - Rule code (e.g., "F001")
- `message: string` - Diagnostic message
- `file_path: string` - Path to the file
- `line: integer` - Line number (1-based)
- `column: integer` - Column number (1-based)
- `severity: string` - Severity level ("error", "warning", "info")
- `category: string` - Rule category ("format", "performance", "style")

#### `class SourceRange`
Represents a range in source code.

**Properties:**
- `start_line: integer` - Starting line number
- `start_column: integer` - Starting column number
- `end_line: integer` - Ending line number  
- `end_column: integer` - Ending column number

### Rule API

#### `class Rule`
Base class for implementing custom rules.

**Abstract Methods:**
- `check(ast_node)` - Analyze AST node and return diagnostics
- `get_code()` - Return rule code string
- `get_description()` - Return rule description

#### Rule Registration
```fortran
! Register custom rule
call rule_registry%register_rule(my_custom_rule)

! Get available rules
rules = rule_registry%get_all_rules()
```

### AST API

#### `class ASTNode`
Represents a node in the Abstract Syntax Tree.

**Properties:**
- `node_type: string` - Type of AST node
- `source_range: SourceRange` - Location in source code
- `children: ASTNode[]` - Child nodes
- `metadata: map` - Additional node metadata

**Methods:**
- `find_nodes_by_type(type: string)` - Find child nodes of specific type
- `get_text()` - Get source text for this node
- `visit(visitor: ASTVisitor)` - Accept visitor pattern

### Formatter API

#### `class Formatter`
Code formatting engine.

**Methods:**
- `format_file(file_path: string, config: FluffConfig)` - Format single file
- `format_string(content: string, config: FluffConfig)` - Format string content
- `check_formatting(content: string, config: FluffConfig)` - Check if formatting is needed

### Cache API

#### `class CacheManager`
Manages analysis result caching.

**Methods:**
- `get_cached_result(file_path: string, checksum: string)` - Get cached analysis
- `store_result(file_path: string, checksum: string, result: DiagnosticResult)` - Store result
- `invalidate_cache(file_path: string)` - Remove cached result
- `clear_all()` - Clear entire cache

## Language Server Protocol (LSP)

### LSP Methods

#### `textDocument/publishDiagnostics`
Push diagnostics to client.

#### `textDocument/codeAction`
Provide code actions for diagnostics.

#### `textDocument/formatting`
Format document.

#### `textDocument/hover`
Provide hover information.

### Custom LSP Extensions

#### `fluff/ruleInfo`
Get detailed information about a specific rule.

#### `fluff/formatOptions`
Get available formatting options.

## Integration APIs

### GitHub Actions Integration

#### `create_annotations(diagnostics: Diagnostic[])`
Create GitHub Actions annotations format.

#### `generate_workflow(config: WorkflowConfig)`
Generate GitHub Actions workflow file.

### Pre-commit Integration

#### `run_precommit_check(staged_files: string[])`
Run fluff on staged files only.

#### `install_hook(hook_path: string)`
Install pre-commit hook.

## Error Handling

All API functions may raise the following exceptions:

- `FluffConfigError` - Configuration validation failed
- `FluffParseError` - Failed to parse Fortran source
- `FluffRuleError` - Rule execution failed
- `FluffIOError` - File I/O operation failed

## Usage Examples

### Basic Analysis
```fortran
use fluff
type(fluff_config_t) :: config
type(diagnostic_t), allocatable :: diagnostics(:)

call config%load_from_file("fluff.toml")
diagnostics = fluff_check(["src/main.f90"], config)
```

### Custom Rule
```fortran
type, extends(rule_t) :: my_rule_t
contains
    procedure :: check => my_rule_check
    procedure :: get_code => my_rule_get_code
end type

function my_rule_check(this, node) result(diagnostics)
    class(my_rule_t), intent(in) :: this
    type(ast_node_t), intent(in) :: node
    type(diagnostic_t), allocatable :: diagnostics(:)
    
    ! Rule implementation
end function
```