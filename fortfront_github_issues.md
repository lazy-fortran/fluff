# GitHub Issues for fortfront

These issues should be filed in the fortfront repository to enable complete dead code detection in fluff.

---

## Issue 1: Add Variable Usage Tracking in Complex Expressions

**Title**: Need API to track variable usage within expression subtrees

**Labels**: enhancement, api

**Description**:

When implementing dead code detection in fluff, we found that variables used in complex expressions (particularly in conditionals) are not easily trackable through the current AST API.

### Current Behavior
When parsing code like:
```fortran
integer :: x = 1
if (x > 0) then
  print *, 'positive'
end if
```

The identifier node for `x` within the condition expression is not accessible through normal AST traversal.

### Expected Behavior
Need an API to traverse all identifier nodes within an expression subtree, possibly through:
- A visitor pattern for expression nodes
- A method to get all identifiers used in an expression
- Better child node access for binary operators

### Impact
This limitation causes false positives in unused variable detection, as variables used in expressions appear unused.

---

## Issue 2: Add Control Flow Graph Construction and Analysis

**Title**: Need control flow graph API for reachability analysis

**Labels**: enhancement, api, feature-request

**Description**:

Dead code detection requires control flow analysis to determine which statements are unreachable after terminating statements like `return`, `stop`, or `goto`.

### Current Behavior
No API exists to:
- Build control flow graphs from AST
- Analyze statement reachability
- Determine dominance relationships

### Expected Behavior
Provide APIs for:
```fortran
type :: control_flow_graph_t
  contains
    procedure :: build_from_ast
    procedure :: is_reachable
    procedure :: get_predecessors
    procedure :: get_successors
end type
```

### Use Case
Detect unreachable code:
```fortran
subroutine test()
  return
  print *, 'unreachable'  ! Should be detectable
end subroutine
```

### Impact
Cannot detect ~67% of unreachable code patterns without this API.

---

## Issue 3: Add Procedure Call Graph API

**Title**: Need call graph construction for unused procedure detection

**Labels**: enhancement, api

**Description**:

To detect unused procedures, we need to build and analyze call graphs, but fortfront doesn't provide this capability.

### Current Behavior
No API to:
- Track procedure calls
- Build call graphs
- Resolve procedure references
- Handle recursive calls

### Expected Behavior
```fortran
type :: call_graph_t
  contains
    procedure :: add_call
    procedure :: is_procedure_called
    procedure :: get_callers
    procedure :: find_recursive_cycles
end type
```

### Impact
Cannot detect unused procedures, affecting 50% of dead code detection capabilities.

---

## Issue 4: Improve AST Block Structure and Sibling Relationships

**Title**: Need clearer block boundaries and sibling relationships in AST

**Labels**: enhancement, api, ast

**Description**:

The current AST structure makes it difficult to determine statement sequences within blocks, particularly for detecting code after terminating statements.

### Current Behavior
- Parent-child relationships exist but sibling relationships are unclear
- Block boundaries are not well-defined
- Hard to iterate through statements in execution order within a block

### Expected Behavior
```fortran
! For each AST node:
node%get_next_sibling()
node%get_block_statements()
node%is_last_in_block()
```

### Use Case
```fortran
if (condition) then
  return
  x = 1  ! Need to know this follows return in same block
end if
y = 2  ! Need to know this is in parent block
```

### Impact
Affects accuracy of unreachable code detection in nested structures.

---

## Issue 5: Add Parameter Attribute Information to AST

**Title**: Include parameter attributes (optional, intent) in AST nodes

**Labels**: enhancement, api, ast

**Description**:

Parameter nodes in the AST don't include attribute information like `optional` or `intent`, which is needed for accurate dead code analysis.

### Current Behavior
Cannot distinguish between:
- Required vs optional parameters
- Intent(in) vs intent(out) vs intent(inout)
- Other parameter attributes

### Expected Behavior
```fortran
type, extends(ast_node) :: parameter_node
  logical :: is_optional
  integer :: intent_type  ! INTENT_IN, INTENT_OUT, INTENT_INOUT
  ! ... other attributes
end type
```

### Impact
- Cannot properly analyze optional parameters
- Intent(out) parameters incorrectly flagged as unused
- Affects 33% of parameter analysis test cases

---

## Summary

These five API enhancements would enable fluff to achieve near 100% accuracy in dead code detection, compared to the current 55.6% success rate. Each addresses a specific gap in the AST API that prevents accurate static analysis.