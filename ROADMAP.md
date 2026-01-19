# fluff Development Roadmap

> **Note**: This project will be retired in favor of [LFortran](https://lfortran.org/).
> See README.md for recommended alternatives.

**Core Principles**
- **fortfront FIRST**: Use fortfront public APIs for all analysis (no text parsing)
- **AST-Based Analysis**: All rules must use fortfront AST API
- **fpm Dependency Management**: fpm handles fortfront integration and linking

## Completed Work

The following phases have been completed:

### Phase 0: fortfront Integration Foundation
- fortfront integration via fpm dependency
- AST-based infrastructure
- Text-based analysis removed

### Phase 1: AST-Based Rules
- F001-F015 style rules implemented
- P001-P007 performance rules implemented
- C001 correctness rule implemented

### Phase 2: Formatter Core
- AST-based formatter using fortfront emit_fortran
- Basic formatting capabilities working

### Phase 3: LSP Server
- Basic LSP server implementation
- Hover, diagnostics, code actions

### Phase 4: Plugin Architecture
- Plugin infrastructure for semantic analyzers
- Call graph, control flow, and variable usage analyzers

### Phase 5: Caching System
- AST caching system implemented

## Remaining Work

Track progress via [GitHub Issues](https://github.com/lazy-fortran/fluff/issues).

### Priority 1: Documentation Cleanup
- [#104](https://github.com/lazy-fortran/fluff/issues/104): Remove or rewrite obsolete status docs

### Priority 2: Feature Completion
- [#85](https://github.com/lazy-fortran/fluff/issues/85): Complete LSP server implementation
- [#86](https://github.com/lazy-fortran/fluff/issues/86): Complete F009-F015 style rules with AST-based analysis
- [#101](https://github.com/lazy-fortran/fluff/issues/101): Improve P004-P007 accuracy using semantic info
- [#106](https://github.com/lazy-fortran/fluff/issues/106): Implement C001 correctness rule
- [#28](https://github.com/lazy-fortran/fluff/issues/28): Implement magic leading & for format preservation
- [#108](https://github.com/lazy-fortran/fluff/issues/108): Release workflow and artifacts

### Polish Items
See issues with the `polish` label for non-blocking improvements.

## EPIC Tracking

- [#77](https://github.com/lazy-fortran/fluff/issues/77): Full MVP Implementation Path

## Critical Constraints

### What fluff Uses from fortfront
- `ast_arena_t` for AST node storage and traversal
- `semantic_context_t` for type information
- `ast_visitor_t` pattern for AST analysis
- `emit_fortran` for code generation

### What fluff Does NOT Do
- Parse Fortran text directly
- Use regex for code analysis
- Implement its own parser
- Access fortfront internals not in public API
