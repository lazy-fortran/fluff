# fluff Architecture Design

**MANDATORY REQUIREMENTS**
- **fortfront Foundation**: All analysis MUST use fortfront AST API - no text-based parsing
- **fpm Dependency Management**: fpm handles fortfront integration and static linking
- **Plugin Architecture**: All rules implement via fortfront semantic pipeline plugins
- **Incremental Analysis**: Support for caching and partial re-analysis

## Table of Contents
1. [Core Architecture](#core-architecture)
2. [Rule System Design](#rule-system-design)
3. [AST-Based Analysis](#ast-based-analysis)
4. [Formatter Architecture](#formatter-architecture)
5. [LSP Server Design](#lsp-server-design)
6. [Performance Optimization](#performance-optimization)
7. [Configuration System](#configuration-system)
8. [Plugin Integration](#plugin-integration)
9. [Implementation Roadmap](#implementation-roadmap)

---

# Core Architecture

## Foundation Layer

fluff is built as a **statically-linked executable** using fpm's automatic dependency management:

```
fluff (single executable - built by fpm)
    └── fortfront (managed by fpm dependency system)
        ├── AST/CST infrastructure
        ├── Semantic analysis pipeline
        ├── Type inference system
        └── Code generation
```

**Build Architecture Elegance**:
- **fpm.toml Configuration**: fortfront is declared as a git dependency
- **Automatic Linking**: fpm handles all static linking without manual configuration
- **Zero Build Complexity**: Single `fpm build` command produces complete executable
- **Dependency Resolution**: fpm automatically resolves and builds fortfront

## Key Design Principles

1. **AST-First Analysis**: ALL rules operate on fortfront's typed AST
2. **Zero Text Processing**: No regex, no string matching - pure AST traversal
3. **Plugin-Based Rules**: Each rule is a semantic analyzer plugin
4. **Incremental by Design**: Cache ASTs and analysis results
5. **Format Preservation**: Use CST for exact source reconstruction (when available)

## Module Organization

```
src/
├── fluff_ast/              # fortfront AST wrapper
│   └── fluff_ast.f90       # Arena and semantic context management
├── fluff_rules/            # Rule implementations
│   └── fluff_rules.f90     # All F001-F015, P001-P007 rules
├── fluff_rule_types/       # Rule type definitions
│   └── fluff_rule_types.f90 # Rule interfaces and base types
├── fluff_formatter/        # AST-based code formatter
│   ├── fluff_formatter_visitor.f90 # AST visitor for formatting
│   ├── fluff_formatter.f90         # Main formatter engine
│   ├── fluff_format_quality.f90    # Format quality analysis
│   └── fluff_user_feedback.f90     # User-facing formatting feedback
├── fluff_lsp_*.f90         # LSP server components (in src/ root)
│   ├── fluff_lsp_server.f90        # Main LSP server
│   ├── fluff_json_rpc.f90          # JSON-RPC protocol
│   ├── fluff_lsp_code_actions.f90  # Code actions
│   ├── fluff_lsp_hover.f90         # Hover information
│   └── fluff_lsp_goto_definition.f90 # Navigation features
└── fluff_cli/              # Command-line interface
    └── fluff_cli.f90       # Main entry point
```

---

# Rule System Design

**NOTE**: This section describes the **target architecture** for full fortfront integration. Current implementation (December 2025) uses procedure pointers in `rule_info_t` rather than the plugin pattern shown below, as fortfront's semantic analyzer plugin system is not yet fully integrated.

## Rule Categories

### Style Rules (F-prefix)
- **F001-F005**: Basic style (implicit none, indentation, line length)
- **F006-F009**: Variable and intent analysis
- **F010-F015**: Code structure and conventions

### Performance Rules (P-prefix)
- **P001-P003**: Array and loop optimization
- **P004-P007**: Memory and precision optimization

### Correctness Rules (C-prefix)
- **C001+**: Type safety and semantic correctness

## Rule Implementation Pattern

Each rule is implemented as a fortfront semantic analyzer plugin:

```fortran
type, extends(base_analyzer_t) :: rule_f001_analyzer_t
contains
    procedure :: analyze_node => check_implicit_none
    procedure :: get_diagnostics => get_f001_diagnostics
end type

function check_implicit_none(this, node, context) result(continue)
    class(rule_f001_analyzer_t), intent(inout) :: this
    integer, intent(in) :: node
    type(semantic_context_t), intent(inout) :: context
    logical :: continue
    
    ! Use fortfront AST API to check for implicit none
    if (ast_node_type(node) == PROGRAM_NODE) then
        if (.not. has_implicit_none(node, context)) then
            call this%add_diagnostic(node, "F001", "Missing implicit none")
        end if
    end if
    continue = .true.
end function
```

## Rule Registration

Rules register with fortfront's semantic pipeline:

```fortran
subroutine register_fluff_rules(pipeline)
    type(semantic_pipeline_t), intent(inout) :: pipeline
    
    ! Style rules
    call pipeline%add_analyzer(rule_f001_analyzer_t())
    call pipeline%add_analyzer(rule_f002_analyzer_t())
    ! ... etc
    
    ! Performance rules  
    call pipeline%add_analyzer(rule_p001_analyzer_t())
    ! ... etc
end subroutine
```

---

# AST-Based Analysis

## Leveraging fortfront AST API

All analysis uses fortfront's comprehensive AST infrastructure:

```fortran
! Example: Detecting unused variables (F006)
function find_unused_variables(ast_arena, semantic_ctx) result(diagnostics)
    type(ast_arena_t), intent(in) :: ast_arena
    type(semantic_context_t), intent(in) :: semantic_ctx
    type(diagnostic_t), allocatable :: diagnostics(:)
    
    ! Use fortfront's usage tracker
    usage_info = semantic_ctx%get_usage_info()
    
    ! Traverse AST for declarations
    call ast_arena%traverse_depth(check_declaration_usage)
    
    ! Report unused variables
    do i = 1, usage_info%num_symbols
        if (usage_info%is_declared(i) .and. .not. usage_info%is_used(i)) then
            call add_diagnostic("F006", "Unused variable", usage_info%location(i))
        end if
    end do
end function
```

## AST Visitor Pattern

Complex rules use the visitor pattern:

```fortran
type, extends(ast_visitor_t) :: indentation_checker_t
    integer :: expected_indent = 0
contains
    procedure :: visit_if_node => check_if_indentation
    procedure :: visit_do_loop_node => check_loop_indentation
end type
```

---

# Formatter Architecture

## AST-Based Formatting

The formatter reconstructs code from the AST with style rules applied:

```fortran
type :: fluff_formatter_t
    type(format_options_t) :: options
    type(ast_arena_t), pointer :: arena
contains
    procedure :: format_file
    procedure :: format_ast
    procedure :: emit_formatted
end type

function format_ast(this, root_node) result(formatted_code)
    class(fluff_formatter_t), intent(inout) :: this
    integer, intent(in) :: root_node
    character(len=:), allocatable :: formatted_code
    
    ! When CST available: preserve user formatting
    if (has_cst_links(root_node)) then
        formatted_code = reconstruct_from_cst(root_node, this%options)
    else
        ! Otherwise: use AST with formatting rules
        formatted_code = emit_fortran(root_node, this%options)
    end if
end function
```

## Format Options

```fortran
type :: format_options_t
    integer :: line_length = 88
    integer :: indent_width = 4
    logical :: use_tabs = .false.
    logical :: preserve_line_breaks = .true.
    character(len=32) :: indent_style = "space"
end type
```

---

# LSP Server Design

## Protocol Implementation

The LSP server provides real-time analysis and code actions:

```fortran
type :: lsp_server_t
    type(ast_cache_t) :: ast_cache
    type(diagnostic_cache_t) :: diagnostic_cache
    type(semantic_pipeline_t) :: pipeline
contains
    procedure :: handle_request
    procedure :: on_document_change
    procedure :: get_diagnostics
    procedure :: get_code_actions
end type
```

## Incremental Analysis

```fortran
subroutine on_document_change(this, uri, changes)
    class(lsp_server_t), intent(inout) :: this
    character(len=*), intent(in) :: uri
    type(text_change_t), intent(in) :: changes(:)
    
    ! Update AST incrementally
    ast_handle = this%ast_cache%get_or_parse(uri)
    
    ! Re-run only affected analyzers
    call this%pipeline%analyze_incremental(ast_handle, changes)
    
    ! Send diagnostics to client
    call this%send_diagnostics(uri)
end subroutine
```

---

# Performance Optimization

## Caching Strategy

### AST Cache
- Cache parsed ASTs with file modification times
- Invalidate on file changes
- Share ASTs across rules

### Analysis Cache
- Cache rule results per AST node
- Invalidate selectively on AST changes
- Reuse results across LSP requests

### Parallel Execution
```fortran
!$OMP PARALLEL DO
do i = 1, num_files
    call analyze_file(files(i))
end do
!$OMP END PARALLEL DO
```

## Memory Management

- Use fortfront's arena allocators
- Batch deallocations
- Minimize temporary allocations

---

# Configuration System

## TOML Configuration

```toml
# fluff.toml
[fluff]
line_length = 88
indent_width = 4
target_version = "2018"

[rules]
select = ["F", "P"]  # Enable style and performance rules
ignore = ["F003"]    # Ignore specific rules

[rules.F002]
indent_style = "space"
indent_width = 4

[formatter]
preserve_line_breaks = true
magic_trailing_ampersand = true
```

## Configuration Priority

1. Command-line arguments (highest)
2. Project fluff.toml
3. User ~/.config/fluff/fluff.toml
4. System defaults (lowest)

---

# Plugin Integration

## External Tool Integration

fluff can be used as a library by other tools:

```fortran
! Example: Using fluff in a build system
program build_with_lint
    use fluff_core
    use fortfront
    
    type(fluff_analyzer_t) :: analyzer
    type(diagnostic_t), allocatable :: diagnostics(:)
    
    ! Initialize with custom rules
    call analyzer%init(config_file="project.toml")
    
    ! Analyze source
    diagnostics = analyzer%check_file("src/module.f90")
    
    ! Handle results
    if (size(diagnostics) > 0) then
        call report_diagnostics(diagnostics)
        if (has_errors(diagnostics)) stop 1
    end if
end program
```

## Custom Rule Development

Users can add custom rules:

```fortran
module custom_rules
    use fluff_rule_types
    use fortfront
    
    type, extends(base_rule_t) :: custom_rule_t
    contains
        procedure :: check => check_custom_pattern
    end type
    
contains
    function check_custom_pattern(this, node, context) result(diagnostic)
        ! Custom analysis logic using fortfront AST
    end function
end module
```

---

# Implementation Roadmap

## Phase 1: Core Infrastructure ✅ COMPLETE
- Build system working (fpm with fortfront dependency)
- fortfront integration via fpm.toml path dependency
- AST wrapper (fluff_ast.f90) with node location API
- Basic rule framework operational

## Phase 2: Rule Implementation 🔄 PARTIAL
- ✅ F002: Indentation (AST-based)
- ✅ F006: Unused variables (AST-based)
- ✅ F007: Undefined variables (AST-based)
- ✅ F008: Missing intent (AST-based)
- ⏳ F001: Implicit none (needs enhanced AST detection)
- ⏳ F003-F005: Whitespace rules (blocked by fortfront CST trivia API)
- ⏳ F009-F015: Style rules (stubs exist, need AST implementation)
- ⏳ P001-P007: Performance rules (stubs exist, need semantic analysis)

## Phase 3: LSP Server ✅ MOSTLY COMPLETE
- ✅ Document synchronization (open/change/save/close)
- ✅ Diagnostic publishing
- ✅ Hover information
- ✅ Code actions
- ⏳ Go-to-definition (blocked by fortfront API - issue #2599)

## Phase 4: Formatter 🔄 PARTIAL
- ✅ Basic emit_fortran integration working
- ✅ Indentation and spacing
- ⏳ CST preservation for comments/whitespace (blocked by fortfront trivia API)
- ⏳ Advanced expression formatting

## Phase 5: Advanced Features ⏳ PENDING
- Custom rule API (infrastructure exists)
- Project-wide analysis (partial)
- Auto-fix suggestions (stubs exist)

## Success Metrics

### Quality Metrics
- **Zero false positives**: All diagnostics must be accurate
- **Performance**: <1s analysis for 10K LOC
- **Memory**: <100MB for typical projects

### Coverage Metrics
- **Rule coverage**: 100% of documented rules
- **Language coverage**: Full Fortran 2018 support
- **Test coverage**: >90% line coverage

---

# Migration from Text-Based to AST-Based

## Current State (December 2025)

**Build**: ✅ Working (json-fortran removed, fpm handles fortfront)
**Tests**: ✅ All core test suites passing
**Rules**: 4 fully AST-based (F002, F006-F008), others have stubs

### Completed Migrations
- F002: Indentation - uses AST node depth
- F006: Unused variables - uses semantic context
- F007: Undefined variables - uses semantic context
- F008: Missing intent - uses AST parameter nodes

### Pending Migrations (Unblocked Upstream)
- F001: implicit none detection → Use AST implicit_statement nodes
- F003: Line length → Use CST with position information + source spans
- F004: Trailing whitespace → Use CST trivia nodes
- F005: Mixed tabs/spaces → Use CST whitespace trivia

### Migration Pattern
```fortran
! OLD: Text-based
if (index(line, "implicit none") > 0) then

! NEW: AST-based
if (ast_node_type(node) == IMPLICIT_STATEMENT_NODE) then
    if (is_implicit_none(node)) then
```

## Performance Considerations

### DO
- Use fortfront's built-in traversal methods
- Cache analysis results
- Process nodes once per pass

### DON'T  
- Parse text with regex
- Traverse AST multiple times
- Store redundant information

---

# Upstream Dependencies

The core upstream APIs fluff needed are now available in fortfront:

- #2599 (CLOSED): CST trivia traversal API
- #2600 (CLOSED): source text retrieval API
- #2612 (CLOSED): parent_index propagation (enables real AST child traversal)
- #2613 (CLOSED): symbol table query API

## Tracking

- fluff EPIC: #77 (Full MVP Implementation Path)
- fluff P0: #79 (Expose missing fortfront APIs)
- fluff P1: #80-83 (Core rule implementations)

---

# Appendix: fortfront AST API Usage

## Key Functions Available

```fortran
! From fortfront
use fortfront, only: &
    ast_arena_t, semantic_context_t, token_t, &
    lex_source, parse_tokens, analyze_semantics, &
    create_ast_arena, create_semantic_context, &
    emit_fortran, get_node_type_id_from_arena, &
    get_node_location, get_children

! From fluff_ast wrapper
use fluff_ast, only: fluff_ast_context_t, create_ast_context

! AST operations via context
call context%from_source(source_code, error_msg)
call context%traverse(visitor, pre_order)
node_type = context%get_node_type(node_index)
children = context%get_children(node_index)
location = context%get_node_location(node_index)
```

## Integration Points

1. **Parsing**: Use fortfront to parse and build AST
2. **Analysis**: Implement rules as semantic analyzers
3. **Formatting**: Use fortfront's emit_fortran with options
4. **Caching**: Store fortfront arena handles

This architecture ensures fluff is a modern, performant linter that fully leverages fortfront's capabilities while maintaining zero external dependencies.
