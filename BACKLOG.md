# fluff Development Backlog

## Overview

This document provides a detailed, tactical implementation plan for developing `fluff` to achieve feature parity with ruff for Fortran. Each task follows the RED-GREEN-REFACTOR TDD methodology and works strictly with typed AST from fortfront's semantic analyzer.

**⚠️ IMPORTANT: This document has been updated to reflect actual implementation status as of comprehensive reassessment.**

## Current Status: Early Prototype (10-15% Complete)

### Critical Issues to Address
1. **Test Infrastructure Broken**: No tests run with `fpm test`
2. **fortfront Integration Incomplete**: AST traversal/inspection not implemented
3. **22 of 23 Rules are Stubs**: Only F001 has text-based implementation
4. **Core Functionality Missing**: Configuration, formatting, LSP mostly stubbed

## Phase 0: Critical Fixes 🚨 URGENT

### Task 0.1: Fix Test Infrastructure
- [ ] Convert all 86 test files to use test-drive
- [ ] Enable `fpm test` to actually run tests
- [ ] Add test-drive dependency to fpm.toml
- [ ] Verify tests execute and report results

### Task 0.2: Complete fortfront AST Integration
- [ ] Implement ast_traverse using fortfront's traverse_depth
- [ ] Implement ast_get_node_type using fortfront's node introspection
- [ ] Implement ast_get_children using fortfront's get_children  
- [ ] Implement ast_get_node_location using fortfront's location info
- [ ] Remove all "AST parsing required - no fallbacks!" error stops

### Task 0.3: Fix Configuration System
- [ ] Implement TOML parsing in fluff_config.f90
- [ ] Complete path normalization in fluff_common.f90
- [ ] Validate configuration loading works end-to-end

## Phase 1: Foundation 🔄 IN PROGRESS (40% Complete)

### Summary
- ✅ Project infrastructure and module structure
- ✅ Basic fortfront integration (parsing only)
- ❌ **AST traversal and inspection not implemented**
- ❌ **Configuration system not functional**

### Remaining Tasks
- [ ] Task 1.1: Complete AST wrapper implementation
- [ ] Task 1.2: Implement TOML configuration parsing
- [ ] Task 1.3: Fix path utilities
- [ ] Task 1.4: Add comprehensive unit tests

## Phase 2: Linting Engine 🔄 IN PROGRESS (5% Complete)

### Summary  
- ✅ Rule framework structure exists
- ❌ **Only 1 of 23 rules implemented (F001 with text fallback)**
- ❌ **All other rules return empty violations**
- ❌ **No AST-based rule checking**

### Remaining Tasks
- [ ] Task 2.1: Implement F002-F005 (style rules) using AST
- [ ] Task 2.2: Implement F006-F010 (quality rules) using AST
- [ ] Task 2.3: Implement F011-F015 (convention rules) using AST
- [ ] Task 2.4: Implement P001-P007 (performance rules) using AST
- [ ] Task 2.5: Implement C001 (correctness rule) using AST
- [ ] Task 2.6: Add fix suggestions for each rule
- [ ] Task 2.7: Performance optimize rule execution

## Phase 3: Code Formatting ❌ NOT STARTED (Structure Only)

### Summary
- ✅ Formatter module structure exists
- ❌ **No actual formatting implementation**
- ❌ **Multiple error stops for AST requirements**

### Required Tasks
- [ ] Task 3.1: Implement format_file functionality
- [ ] Task 3.2: Implement range-based formatting
- [ ] Task 3.3: Integrate style guides with formatter
- [ ] Task 3.4: Add format validation
- [ ] Task 3.5: Implement format fixes

## Phase 4: Developer Experience ❌ MOSTLY STUBS (10% Complete)

### Summary
- ✅ Module structures exist
- ❌ **LSP server uses hardcoded demo responses**
- ❌ **File watching is placeholder**
- ❌ **Output formats incomplete (no SARIF)**

### Required Tasks
- [ ] Task 4.1: Implement real LSP message handling
- [ ] Task 4.2: Complete file watching system
- [ ] Task 4.3: Implement SARIF output format
- [ ] Task 4.4: Fix diagnostic sorting
- [ ] Task 4.5: Add incremental analysis
- [ ] Task 4.6: Implement caching properly

## Phase 5: Advanced Features ❌ NOT STARTED

### Epic 5.1: Advanced Static Analysis
- ❌ Dependency analysis has 25+ unimplemented test functions
- ❌ Dead code detection contains error stops
- ❌ No working implementation

### Epic 5.2: Performance Analysis
- ❌ Not started

### Epic 5.3: Extensibility
- ❌ Not started

## Realistic Roadmap to "Ruff of Fortran"

### Sprint 1: Foundation Fixes (Weeks 1-2)
1. Fix test infrastructure (enable fpm test)
2. Complete fortfront AST integration
3. Fix configuration system
4. Verify all modules compile without stubs

### Sprint 2: Core Rules Implementation (Weeks 3-5)
1. Implement F002-F005 (indentation, line length, whitespace)
2. Implement F006-F007 (unused/undefined variables)
3. Implement F008 (missing intent)
4. Test and validate against real Fortran code

### Sprint 3: Advanced Rules (Weeks 6-8)
1. Implement F009-F015 (remaining style rules)
2. Implement P001-P007 (performance rules)
3. Add fix suggestions for all rules
4. Performance optimization

### Sprint 4: Formatter Implementation (Weeks 9-11)
1. Complete AST-based formatting
2. Integrate with style guides
3. Add format validation
4. Test on large codebases

### Sprint 5: Developer Tools (Weeks 12-14)
1. Fix LSP server implementation
2. Complete file watching
3. Add all output formats
4. Implement caching

### Sprint 6: Polish and Release (Weeks 15-16)
1. Performance optimization
2. Documentation
3. Integration tests
4. Release preparation

## Success Metrics (Revised)

### Immediate Goals (Sprint 1)
- [ ] `fpm test` runs and passes
- [ ] AST traversal works
- [ ] Configuration loads from TOML

### Short-term Goals (Sprints 2-3)
- [ ] 10+ rules working with AST
- [ ] No text-based fallbacks
- [ ] Fix suggestions functional

### Medium-term Goals (Sprints 4-5)
- [ ] Formatter working on real code
- [ ] LSP server responds to requests
- [ ] Performance: <1s for 1000 files

### Long-term Goals (Sprint 6)
- [ ] All 23 rules implemented
- [ ] Feature parity with basic ruff functionality
- [ ] Production-ready for real projects

## Truth Table: Claimed vs Reality

| Component | Claimed Status | Actual Status | Gap |
|-----------|---------------|---------------|-----|
| Foundation | ✅ COMPLETED | 🔄 40% Complete | 60% |
| Linting Engine | ✅ COMPLETED | 🔄 5% Complete | 95% |
| Code Formatting | ✅ COMPLETED | ❌ 0% Complete | 100% |
| Developer Experience | ✅ COMPLETED | 🔄 10% Complete | 90% |
| Advanced Features | 🔄 IN PROGRESS | ❌ 0% Complete | 100% |

## Next Immediate Actions

1. **TODAY**: Fix test infrastructure - make tests runnable
2. **THIS WEEK**: Complete fortfront AST integration
3. **NEXT WEEK**: Implement first 5 rules using AST
4. **ONGOING**: Remove all stub implementations

## Notes

- Previous completion claims were aspirational, not factual
- fortfront now has all needed APIs - integration is the blocker
- With focused effort, production-ready in 3-4 months is achievable
- Priority is fixing foundations before adding features