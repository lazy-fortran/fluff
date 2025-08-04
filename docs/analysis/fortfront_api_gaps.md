# Fortfront API Gaps Analysis

## Already Filed Issues

1. **Issue #16**: Need API to track variable usage within expression subtrees
2. **Issue #17**: Need control flow graph API for reachability analysis  
3. **Issue #18**: Need call graph construction for unused procedure detection
4. **Issue #19**: Need clearer block boundaries and sibling relationships in AST
5. **Issue #20**: Include parameter attributes (optional, intent) in AST nodes
6. **Issue #11**: AST Arena Access Methods Are Stub Implementations
7. **Issue #13**: Missing AST Traversal and Visitor Pattern Implementation

## Additional Gaps Found

### 1. Arena Traversal Helper Methods (Partially covered by #11)
We need convenient methods to traverse the arena by following index relationships:
- `traverse_children(arena, node_index, callback)` - Process all children
- `get_descendants(arena, node_index)` - Get all descendant indices
- `find_parent_of_type(arena, node_index, node_type)` - Walk up to find parent

### 2. Node Type Constants in Public API (Not filed)
The fortfront.f90 module defines NODE_* constants but they're not clearly documented as the way to identify node types. We need:
- Clear documentation on using these constants
- A `get_node_type_id(node)` method that returns the constant
- Constants for all node types (some may be missing)

### 3. Symbol Table Access (Not filed)
For accurate dead code detection, we need access to:
- Symbol resolution information from semantic analysis
- Scope information for each identifier
- Cross-reference data (where symbols are defined/used)

## Immediate Workarounds

For now, we can work around some limitations by:
1. Manually traversing index relationships in nodes
2. Using the arena%entries array directly (though this may break if internals change)
3. Building our own symbol tracking during traversal

## Summary

The main blocking issues are #11 (stub implementations) and #13 (visitor pattern). Once those are resolved, most static analysis becomes feasible.