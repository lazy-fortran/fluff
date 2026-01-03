# CLAUDE.md - fluff Project Instructions

**MANDATORY REQUIREMENTS**
- **fortfront AST Only**: ALL analysis MUST use fortfront AST API - NO text processing
- **fpm Dependency Management**: fpm handles the fortfront dependency and static linking
- **Incremental Changes**: Every PR must leave fluff in working state
- **Clean Legacy**: Remove old code in same PR that adds replacement

## Project Purpose

fluff is a Fortran linter and formatter inspired by Python's ruff, providing:
- Fast, reliable static analysis using fortfront's AST
- Automatic code formatting preserving user style where possible
- LSP server for real-time IDE integration
- Zero-dependency static executable

## Architecture Overview

```
fluff (static executable - built by fpm)
    └── fortfront (managed by fpm as dependency)
        ├── AST/CST infrastructure
        ├── Semantic analysis pipeline  
        ├── Type inference
        └── Code generation
```

**Key Architectural Simplifications**:
- **fpm Dependency Management**: fpm.toml declares fortfront as a git dependency
- **Automatic Static Linking**: fpm handles all compilation and linking automatically
- **Zero Build Complexity**: No manual build configuration required
- **Single Command Build**: `fpm build` produces the complete static executable

## Development Workflow

### Before Starting Work
1. **Read DESIGN.md** - Understand the architecture
2. **Check BACKLOG.md** - Find next priority issue
3. **Review fortfront API** - Use existing capabilities

### Implementation Requirements

#### MANDATORY: Use fortfront AST API
```fortran
! ❌ NEVER do text processing
if (index(line, "implicit none") > 0) then  ! WRONG

! ✅ ALWAYS use AST nodes
if (ast_node_type(node) == IMPLICIT_STATEMENT_NODE) then  ! RIGHT
```

#### Rule Implementation Pattern
Every rule MUST be a fortfront semantic analyzer plugin:

```fortran
type, extends(base_analyzer_t) :: rule_f001_analyzer_t
contains
    procedure :: analyze_node => check_implicit_none
    procedure :: get_diagnostics => get_f001_diagnostics
end type
```

#### Available fortfront API Functions
```fortran
use fortfront, only: &
    ast_arena_t, semantic_context_t, &
    lex_source, parse_tokens, analyze_semantics, &
    create_ast_arena, create_semantic_context, &
    emit_fortran, ast_visitor_t, &
    ast_node_type, get_children, traverse_depth
```

### Testing Requirements

- **Build**: `fpm build`
- **Test**: `fpm test`
- **Specific test**: `fpm test test_rule_f001`
- **NEVER** compile test programs manually
- Create tests in `test/` with names `test_*.f90`

### Code Organization

```
src/
├── fluff_ast/           # fortfront AST wrapper (NO changes)
├── fluff_rules/         # Rule implementations (ADD here)
├── fluff_formatter/     # Formatter (USE emit_fortran)
├── fluff_lsp_server/    # LSP implementation
└── fluff_cli/           # Command interface
```

## Migration Checklist

### Text-Based Rules to Migrate (PRIORITY 1)
- [ ] F001: implicit none → Use implicit_statement nodes
- [ ] F003: Line length → Use CST positions
- [ ] F004: Trailing whitespace → Use CST trivia
- [ ] F005: Mixed tabs/spaces → Use CST trivia

### AST-Based Rules to Complete
- [x] F002: Indentation (AST-based)
- [x] F006: Unused variables (AST-based)
- [x] F007: Undefined variables (AST-based)
- [x] F008: Missing intent (AST-based)
- [ ] F009-F015: Remaining style rules
- [ ] P001-P007: Performance rules
- [ ] C001+: Correctness rules

## Common Patterns

### AST Traversal
```fortran
! Use visitor pattern for complex analysis
type, extends(ast_visitor_t) :: my_analyzer_t
contains
    procedure :: visit_if_node
    procedure :: visit_do_loop_node
end type

! Or direct traversal for simple checks
call ast_arena%traverse_depth(check_node)
```

### Semantic Queries
```fortran
! Get type information
type_info = semantic_context%get_type(node)

! Check scope
scope = semantic_context%get_scope(node)

! Get usage information
usage = semantic_context%get_usage_info()
```

### Diagnostic Reporting
```fortran
type :: diagnostic_t
    character(len=5) :: code      ! e.g., "F001"
    character(len=:), allocatable :: message
    integer :: line, column
    character(len=:), allocatable :: fix_suggestion
end type
```

## Performance Guidelines

### DO
- Cache AST handles between analyses
- Use fortfront's built-in traversal
- Process each node once per pass
- Reuse semantic context

### DON'T
- Parse text with regex
- Traverse AST multiple times
- Store redundant information
- Create unnecessary temporaries

## Issue Implementation Guide

### For Each Issue
1. **Read issue description** in BACKLOG.md
2. **Identify legacy code** to remove
3. **Use fortfront API** for implementation
4. **Write tests first** (TDD)
5. **Update documentation**
6. **Remove legacy** in same PR

### PR Requirements
- Title: `fix: [Issue #XX] Brief description`
- Body: 
  - What changed
  - What legacy was removed
  - Test coverage added
- All tests must pass
- No new text-based analysis

## Common Mistakes to Avoid

### ❌ Text Processing
```fortran
! NEVER parse Fortran as text
line = read_line(file)
if (contains_keyword(line, "program")) then
```

### ❌ Manual AST Construction
```fortran
! NEVER build AST nodes manually
node = create_node(IF_NODE)  ! WRONG
```

### ❌ Ignoring fortfront Capabilities
```fortran
! NEVER reimplement what fortfront provides
my_type_inference()  ! WRONG - use semantic_context
```

### ✅ Correct Approach
```fortran
! ALWAYS use fortfront API
ast_handle = parse_file("test.f90")
semantic_ctx = analyze_semantics(ast_handle)
node_type = ast_node_type(node)
```

## Testing Strategy

### Unit Tests (per rule)
```fortran
program test_rule_f001
    use fluff_rules
    use fortfront
    
    ! Test missing implicit none
    ast = parse_source("program main; end program")
    diagnostics = check_f001(ast)
    if (size(diagnostics) /= 1) error stop "F001 failed"
end program
```

### Integration Tests
```fortran
! Test complete analysis pipeline
diagnostics = analyze_file("sample.f90")
! Verify all expected diagnostics
```

### Performance Tests
```fortran
! Ensure <1s for 10K LOC
call analyze_large_file("10k_lines.f90")
! Check timing and memory
```

## Release Checklist

### Before Release
- [ ] All rules use AST API
- [ ] No text processing remains
- [ ] Tests achieve >90% coverage
- [ ] Performance <1s for 10K LOC
- [ ] Memory usage <100MB
- [ ] Documentation complete

### Release Process
1. Update VERSION
2. Update CHANGELOG.md
3. Run full test suite
4. Build release binary
5. Test on sample projects
6. Create GitHub release

## Getting Help

### fortfront API Questions
- Check `/home/ert/code/fortfront/docs/`
- Review fortfront test files for examples
- File issues in fortfront for missing APIs

### fluff Architecture
- Read DESIGN.md for architecture
- Check BACKLOG.md for roadmap
- Review existing rule implementations

## Critical Reminders

1. **NEVER use text processing** - Always use AST
2. **NEVER add external dependencies** - Only fortfront
3. **ALWAYS remove legacy code** - Clean as you go
4. **ALWAYS write tests first** - TDD approach
5. **ALWAYS check fortfront API first** - Don't reinvent

This project aims to be the "ruff for Fortran" - fast, reliable, and zero-dependency.
