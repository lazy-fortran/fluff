# Fortfront API Analysis for Dead Code Detection

## Current State

The fortfront API has removed `get_node` functionality but still provides:

1. **Direct arena access**: `arena%entries` array is accessible
2. **Index-based node relationships**: Nodes reference children by indices
   - `if_node%condition_index`
   - `binary_op_node%left_index`, `right_index`
   - `assignment_node%target_index`, `value_index`
   - `function_def_node%param_indices`
   - etc.

3. **Arena methods** (though some are stubs):
   - `find_by_type(node_type)` - find nodes by type
   - `get_children(parent_index)` - get child indices
   - `traverse_depth(target_depth, visitor)` - stub
   
4. **Visitor pattern exists but is not public**

## Key Issue: Recursive Traversal

Our current implementation only processes top-level nodes. To fix the "variable used in conditionals" test failure, we need to recursively traverse expression trees.

Example problem:
```fortran
if (x > 0) then  ! x is not detected as used
```

The AST structure is:
```
if_node (condition_index = 2)
  └─> binary_op_node (index 2, operator ">") 
      ├─> identifier_node (left_index = 3, name = "x")  <-- We miss this!
      └─> literal_node (right_index = 4, value = "0")
```

## Solution Approaches

### 1. Enhanced Recursive Traversal (Current API)
```fortran
! Add recursive traversal for expression nodes
subroutine process_expression_indices(this, indices)
    class(dead_code_detector_t), intent(inout) :: this
    integer, intent(in) :: indices(:)
    integer :: i
    
    do i = 1, size(indices)
        if (indices(i) > 0 .and. indices(i) <= this%arena%size) then
            call this%process_node(this%arena%entries(indices(i)), indices(i))
        end if
    end do
end subroutine

! Enhanced process_node to handle index relationships
subroutine detector_process_node(this, entry, node_index)
    ! ... existing code ...
    
    select type (node => entry%node)
    type is (if_node)
        ! Process condition expression
        if (node%condition_index > 0) then
            call this%process_node(this%arena%entries(node%condition_index), node%condition_index)
        end if
        
    type is (binary_op_node)
        ! Process both operands
        if (node%left_index > 0) then
            call this%process_node(this%arena%entries(node%left_index), node%left_index)
        end if
        if (node%right_index > 0) then
            call this%process_node(this%arena%entries(node%right_index), node%right_index)
        end if
        
    type is (assignment_node)
        ! Process value (RHS) for usage, not target (LHS)
        if (node%value_index > 0) then
            call this%process_node(this%arena%entries(node%value_index), node%value_index)
        end if
    ! ... etc for other nodes with children
    end select
end subroutine
```

### 2. Requested Public API Enhancements

For the GitHub issues already filed, we need:

1. **Public visitor pattern** - Export `ast_visitor_t` 
2. **Working traverse methods** - Implement arena traversal methods
3. **Expression visitor** - Specific visitor for expression subtrees
4. **Control flow graph** - For unreachable code detection
5. **Call graph** - For unused procedure detection

## Immediate Action

We can improve our test success rate by implementing recursive traversal with the current API while waiting for fortfront enhancements.