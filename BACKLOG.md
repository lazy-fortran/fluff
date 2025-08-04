# fluff Development Backlog

## Current Status: Foundation Complete (40% to MVP)

### Recent Achievements ✅
- **Test Infrastructure**: Fixed and operational with fpm
- **fortfront Integration**: Complete AST integration working
- **AST-based Rules**: F002, F006, F007, F008 implemented using AST
- **Dependency Analysis**: Implemented with transitive dependency tracking
- **Dead Code Detection**: Advanced implementation with 50% tests passing
- **String Wrapper Fix**: Resolved gfortran segfault issues
- **Code Quality**: Comprehensive input validation and memory management

## Completed Phases

### Phase 0: Critical Fixes ✅ COMPLETED
- ✅ Test infrastructure working with fpm
- ✅ fortfront AST integration complete
- ✅ AST traversal and inspection implemented
- ✅ Configuration system functional

### Phase 1: Foundation ✅ COMPLETED
- ✅ Project infrastructure and module structure
- ✅ Full fortfront integration with semantic analysis
- ✅ AST wrapper implementation complete
- ✅ Path utilities and common functions working

### Phase 2: Linting Engine 🔄 IN PROGRESS (40% Complete)
#### Completed
- ✅ F001: Missing implicit none (text-based)
- ✅ F002: Inconsistent indentation (AST-based)
- ✅ F006: Unused variable (AST-based)
- ✅ F007: Undefined variable (AST-based)
- ✅ F008: Missing intent (AST-based)

#### Remaining Rules
- [ ] F003: Line too long
- [ ] F004: Trailing whitespace
- [ ] F005: Mixed tabs and spaces
- [ ] F009-F015: Convention rules
- [ ] P001-P007: Performance rules
- [ ] C001: Correctness rules

## Next Sprint Plan

### Sprint 1: Complete Core Rules (Week 1)
- [ ] Implement F003-F005 (remaining style rules)
- [ ] Implement F009-F011 (naming and structure rules)
- [ ] Add fix suggestions for implemented rules
- [ ] Achieve 60% rule coverage

### Sprint 2: Performance Rules (Week 2)
- [ ] Implement P001-P003 (array and loop optimization)
- [ ] Implement P004-P007 (remaining performance rules)
- [ ] Add performance benchmarks
- [ ] Optimize rule execution speed

### Sprint 3: Formatter MVP (Week 3)
- [ ] Implement basic AST-based formatting
- [ ] Add indentation and spacing rules
- [ ] Integrate with style configuration
- [ ] Test on real codebases

### Sprint 4: LSP and Developer Tools (Week 4)
- [ ] Complete LSP server implementation
- [ ] Add real-time diagnostics
- [ ] Implement code actions for fixes
- [ ] Add VS Code extension

## Technical Debt to Address
- [ ] Complete test coverage for all modules
- [ ] Fix remaining linker issues in test files
- [ ] Implement missing LSP hover functionality
- [ ] Add comprehensive error handling

## Success Metrics

### Achieved ✅
- ✅ Project builds without errors
- ✅ AST integration working
- ✅ 5+ rules implemented with AST
- ✅ Dependency analysis functional
- ✅ Dead code detection progressing

### Short-term Goals (2 weeks)
- [ ] 15+ rules working
- [ ] Basic formatter functional
- [ ] LSP server responding to requests
- [ ] 80% test coverage

### MVP Goals (1 month)
- [ ] All 23 rules implemented
- [ ] Formatter comparable to fprettify
- [ ] VS Code extension published
- [ ] Documentation complete

## Priority Order
1. **Complete remaining style rules** (F003-F005, F009-F015)
2. **Implement performance rules** (P001-P007)
3. **Build formatter MVP**
4. **Complete LSP implementation**
5. **Package and release**

## Notes
- fortfront integration is complete and working
- Focus on completing rules before adding new features
- Maintain AST-based approach for all new rules
- Keep comprehensive test coverage