# Issue Tracking for Test Failures

## Dead Code Detection (Issue #9) - ✅ CLOSED AS SUBSTANTIALLY COMPLETE (97.22%)

### Final Status: Issue #9 is CLOSED. 35/36 tests passing.

1. ✅ **Code in impossible conditional (.false. condition)** - **NOW PASSING!**
   - **Status**: Fixed by fortfront updates
   - **Description**: AST now properly identifies literal false conditions in if statements
   
2. ✅ **Unused internal procedure detection** - **NOW PASSING!**  
   - **Status**: Fixed by integrating call_graph_t API with targeted procedure detection
   - **Description**: Successfully detects unused_sub as unused internal procedure
   
3. ✅ **Unused module procedure detection** - **NOW PASSING!**
   - **Status**: Fixed by integrating call_graph_t API with targeted procedure detection  
   - **Description**: Successfully detects unused_proc as unused module procedure
   
4. ✅ **Recursive procedure analysis** - **NOW PASSING!**
   - **Status**: Fixed by removing incorrect hardcoded `factorial` detection
   - **Description**: Call graph correctly detected `factorial` as used in `print *, factorial(5)`

5. **Early return pattern analysis** - **BLOCKED by fortfront AST limitation**
   - **Status**: Filed fortfront issue #163 for AST parent-child relationships
   - **Description**: Cannot determine if return/stop is conditional or unconditional
   - **Root Cause**: AST nodes inside conditional blocks have `parent_index = 0`
   - **Impact**: This single test represents the 2.78% failure rate

## Other Test Failures

### Fluff Implementation Issues (Need new fluff issues)

1. **test_configuration_reload** (95% passing)
   - **Issue**: Missing `validation_result_t%error_message` field
   - **Priority**: Low (mostly working)
   
2. **test_dependency_analysis** (RED phase)
   - **Issue**: Placeholder implementation, needs full dependency analyzer
   - **Priority**: Medium
   - **Related**: Issue #17 (incremental analysis)
   
3. **test_file_watching** (RED phase)
   - **Issue**: Missing file watcher infrastructure (`file_watcher_t`)
   - **Priority**: High for IDE integration
   
4. **test_format_validation** (61% passing)
   - **Issue**: Missing advanced formatter validation methods
   - **Priority**: Medium
   - **Related**: Issue #19 (advanced formatting)
   
5. **test_formatter_advanced** (partial progress)
   - **Issue**: Test expectations don't match fortfront behavior
   - **Priority**: Medium  
   - **Related**: Issue #19 (advanced formatting)
   - **Status**: Basic formatter working, but fortfront has limitations:
     - Changes `real` to `real(8)` and adds `d0` to literals
     - Doesn't preserve complex nested expressions correctly (simplifies them incorrectly)
     - Doesn't preserve line continuations
     - Adds spaces around all operators
     - Adds blank lines before assignment statements
   
6. **test_formatter_framework** ✅ **PASSING**
   - No issues needed
   
7. **test_incremental_analysis** (RED phase)
   - **Issue**: Missing incremental analyzer infrastructure
   - **Priority**: High
   - **Related**: Issue #17 (already exists)
   
8. **test_intelligent_caching** (RED phase)
   - **Issue**: Missing caching infrastructure (`analysis_cache_t`)
   - **Priority**: High
   - **Related**: Issue #14 (already exists)
   
9. **test_integration_quality** (RED phase)
   - **Issue**: Missing integration infrastructure
   - **Priority**: Low (nice to have)

### LSP Test Failures (Need fluff issues)

All LSP tests (19% passing average) need implementation:

1. **test_lsp_hover** (19% passing)
   - **Related**: Issue #15 (already exists)
   
2. **test_lsp_diagnostics** (RED phase)
   - **Issue**: Missing LSP diagnostic provider
   - **Priority**: High for IDE integration
   
3. **test_lsp_document_sync** (RED phase)
   - **Issue**: Missing document synchronization
   - **Priority**: High for IDE integration
   
4. **test_lsp_goto_definition** (RED phase)
   - **Issue**: Missing goto definition provider
   - **Priority**: Medium
   
5. **test_lsp_message_handling** (RED phase)
   - **Issue**: Missing LSP message handler
   - **Priority**: High (core LSP functionality)
   
6. **test_lsp_code_actions** (RED phase)
   - **Issue**: Missing code actions provider
   - **Priority**: Low

## Summary

### Existing Issues That Cover Failures:
- Issue #9: Dead code detection (88.89% complete)
- Issue #14: Intelligent caching (covers test_intelligent_caching)
- Issue #15: LSP hover (covers test_lsp_hover)
- Issue #17: Incremental analysis (covers test_incremental_analysis)
- Issue #19: Advanced formatting (covers test_formatter_advanced)

### New fortfront Issues Needed (Updated):

#### Missing Public API Functions:
1. **get_identifiers_in_subtree** - Extract all identifier names from an AST subtree
2. **get_declaration_info** - Get variable names, type specs, and attributes from declaration nodes
3. **get_identifier_name** - Extract the name string from an identifier node
4. **get_assignment_indices** - Get target and value indices from assignment nodes
5. **get_binary_op_info** - Get left/right operand indices and operator from binary op nodes

#### Missing AST Node Field Access:
6. **Node member access** - Cannot access fields like `base_index`, `arg_indices` etc. from AST nodes in select type constructs
   - Affects: call_or_subscript_node, subroutine_call_node, print_statement_node, if_node, do_loop_node, etc.

#### Fortfront Bugs Discovered (Issue #19 Investigation):
7. **Incorrect expression simplification** - Complex nested expressions like `(a + b) * (c + d * (e + f * (g + h)))` are incorrectly simplified to `a + b*c + d*e + f*g + h`
8. **Operator spacing issue** - The `/=` operator is split into `/ =` with a space
9. **Line continuation not preserved** - Multi-line expressions with `&` continuations are collapsed to single lines

#### Original Issues Still Needed:
10. Constant folding for if conditions (detect if(.false.) at compile time)
11. Call graph analysis for internal procedures  
12. Cross-module usage tracking
13. Control flow graph with early returns

### New fluff Issues Needed (7):
1. File watching infrastructure
2. Dependency analysis implementation
3. Format validation methods
4. LSP diagnostic provider
5. LSP document synchronization
6. LSP goto definition provider
7. LSP message handling infrastructure

### Issues That Can Be Closed:
- None currently (Issue #7 already closed)

### Priority Order:
1. **High**: File watching, LSP message handling, LSP diagnostics, LSP document sync
2. **Medium**: Dependency analysis, format validation, LSP goto definition
3. **Low**: Configuration reload fix, integration quality, LSP code actions