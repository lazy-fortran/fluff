# fluff Development Roadmap

**MANDATORY FOUNDATION REQUIREMENTS**
- **fortfront FIRST**: Use fortfront public APIs for all analysis (no text parsing)
- **AST-Based Analysis**: All rules must use fortfront AST API - NO text-based parsing
- **Clean Architecture**: Separate rule engine from fortfront integration cleanly
- **fpm Dependency Management**: fpm handles fortfront integration and linking

This roadmap ensures fluff leverages fortfront's AST infrastructure properly.

## 🔥 **PHASE 0: fortfront Integration Foundation** (ABSOLUTE PRIORITY)

**CRITICAL**: Wait for fortfront static library, then integrate properly

### Step 1: fortfront Foundation
- Keep fortfront integration on the public API surface.
- Track remaining glue work in fluff issue #79.

**Build Architecture**: fpm.toml declares fortfront as a git dependency, which handles build and linking.

### Step 2: Remove ALL Text-Based Analysis (#64)
- **#64**: Remove all text-based analysis from fluff (Technical Debt)
- **Delete**: All regex-based rule implementations
- **Delete**: Line-by-line text scanning code
- **Clean**: Remove string manipulation utilities for analysis
- **Keep**: Only fortfront AST-based infrastructure

### Step 3: Fix Critical Test Failures
- **#57**: Fix LSP modules systematic ERROR STOP 1
- **#56**: Fix intelligent_caching module ERROR STOP 1  
- **#55**: Fix integration_quality module ERROR STOP 1
- **#54**: Fix format_validation module ERROR STOP 1
- **Ensure**: All tests pass before proceeding

**DELIVERABLE**: Clean fluff codebase using ONLY fortfront AST API, all tests passing

## 🏗️ **PHASE 1: AST-Based Rule Migration** (After Phase 0)

### Core Rules Migration (Must use fortfront AST)
1. **#58** - Migrate F001 (implicit none) from text-based to AST-based detection
2. **#59** - Migrate F003 (line length) to CST-based position detection
3. **#60** - Migrate F004-F005 (whitespace rules) to CST trivia analysis

### Performance Rules Implementation  
4. **#61** - Implement P001: Detect inefficient column-major array access
5. **#37** - Implement P001-P003: Core performance analysis rules
6. **#42** - Implement P004-P007: Advanced performance rules

## 🏗️ **PHASE 2: Formatter Core** (Requires fortfront CST)

### Build Formatter Infrastructure
7. **#62** - Build AST-based formatter core using fortfront emit_fortran
8. **#28** - Implement magic leading & for format preservation (CST trivia)

## 📡 **PHASE 3: LSP Server Implementation**

### Language Server Protocol
9. **#63** - Implement LSP server message handling and protocol
10. Integration with VSCode extension (use fortfront LSP patterns)

## 🔌 **PHASE 4: Plugin Architecture** (After fortfront plugins ready)

### Semantic Analysis Plugins
11. **#31** - Create plugin infrastructure for semantic analyzers
12. **#32** - Port call graph analyzer from fortfront to fluff plugin
13. **#33** - Port control flow analyzer from fortfront to fluff plugin  
14. **#34** - Port variable usage tracker from fortfront to fluff plugin

### Advanced Analyzers
15. **#45** - Create source reconstruction analyzer plugin
16. **#46** - Create interface comparison analyzer plugin

## ⚡ **PHASE 5: Performance & Optimization**

### Caching System
17. **#65** - Create comprehensive AST caching system for performance
18. Benchmark against ruff performance targets

## 🛡️ **PHASE 6: Robustness & Error Handling**

### Error Recovery
19. **#38** - Add parser error recovery for incomplete constructs
20. **#39** - Add defensive programming checks for arrays and strings
21. **#40** - Implement F009: Detect inconsistent intent usage patterns

## 📚 **EPIC TRACKING** (Don't Process Directly)
- **#16** - Epic: Complete rule engine integration with fortfront AST
- **#18** - Epic: Add comprehensive error handling and recovery
- **#30** - Epic: Migrate fortfront Analysis to fluff Plugin Architecture

---

## 🎯 **GETTING STARTED**

**START HERE**: Issue #64 (Remove all text-based analysis) is CRITICAL - must be done first after fortfront foundation is ready.

## **PROCESSING PRINCIPLES**

This roadmap ensures:

✅ **AST-Only Analysis**: NO text-based parsing, only fortfront AST API
✅ **Clean Architecture**: Clear separation between fluff and fortfront
✅ **Incremental Progress**: Each issue completable in one PR  
✅ **System Integrity**: fluff remains functional after every merge
✅ **No Legacy Code**: Complete removal of text-based analysis
✅ **Performance First**: Leverage fortfront's arena architecture

## **PHASE DEPENDENCIES**

- **Phase 0** MUST complete before ANY other work
- **Phase 1** depends on Phase 0 and fortfront CST availability
- **Phase 2** depends on Phase 1 and fortfront CST/AST split
- **Phase 3** can start after Phase 1
- **Phase 4** depends on fortfront plugin architecture
- **Phase 5** can run in parallel after Phase 2
- **Phase 6** can run in parallel after Phase 1

## **SUCCESS METRICS**

Each phase completion should achieve:
- All tests passing
- No text-based analysis remaining
- Full fortfront AST integration
- Performance within 10% of ruff
- Complete documentation

## **CRITICAL CONSTRAINTS**

### What fluff MUST Use from fortfront
- `ast_arena_t` for AST node storage and traversal
- `semantic_context_t` for type information
- `ast_visitor_t` pattern for AST analysis
- `emit_fortran` for code generation
- CST nodes for formatting preservation (when available)

### What fluff MUST NOT Do
- ❌ Parse Fortran text directly
- ❌ Use regex for code analysis  
- ❌ Implement its own parser
- ❌ Create duplicate AST infrastructure
- ❌ Access fortfront internals not in public API

## **INTEGRATION REQUIREMENTS**

### Build Configuration (Already Complete)
```toml
# fluff/fpm.toml (CURRENT STATE)
[dependencies]
fortfront = { git = "ssh://git@github.com/lazy-fortran/fortfront.git", branch = "main" }
stdlib = "*"

[[executable]]
name = "fluff"
source-dir = "app" 
main = "main.f90"
```

**fpm Automatically Handles**:
- Static linking of fortfront dependency
- All compilation flags and optimization
- Dependency resolution and building
- Production of single static executable

### API Usage Pattern
```fortran
! fluff must use fortfront's public API
use fortfront, only: ast_arena_t, semantic_context_t, &
                     lex_source, parse_tokens, analyze_semantics, &
                     create_ast_arena, create_semantic_context, &
                     emit_fortran, ast_visitor_t

! NOT allowed - direct module access
use ast_nodes_core  ! ❌ Internal module
use parser_core     ! ❌ Internal module
```

## **MIGRATION CHECKLIST**

For each text-based rule being migrated:
- [ ] Identify AST nodes needed for analysis
- [ ] Implement using ast_visitor_t pattern
- [ ] Access semantic info via semantic_context_t
- [ ] Remove ALL regex/text parsing code
- [ ] Add comprehensive tests using AST
- [ ] Document AST-based approach
- [ ] Verify no text analysis remains
