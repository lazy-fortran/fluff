# Fortfront API Limitations for Dead Code Detection

This document lists the fortfront API limitations discovered while implementing dead code detection in fluff.

## Issue 1: Variable Usage Tracking in Complex Expressions

**Problem**: The AST doesn't provide complete information about variable usage within complex expressions. When a variable is used in an if condition like `if (x > 0)`, the identifier node for `x` is not easily accessible through the current AST traversal.

**Impact**: Cannot accurately detect whether variables are used in conditional expressions, leading to false positives for unused variables.

**Test Case**:
```fortran
program test
  integer :: x = 1
  if (x > 0) then
    print *, 'positive'
  end if
end program
```

**Suggested Solution**: Provide a visitor pattern or API to traverse all identifier nodes within an expression subtree.

## Issue 2: Control Flow Graph Analysis

**Problem**: fortfront doesn't provide control flow graph construction or reachability analysis. This makes it difficult to determine which statements are unreachable after terminating statements like `return`, `stop`, or `goto`.

**Impact**: Cannot detect unreachable code after control flow statements.

**Test Cases**:
```fortran
subroutine test_sub()
  print *, 'before return'
  return
  print *, 'after return'  ! This should be detected as unreachable
end subroutine
```

**Suggested Solution**: Add APIs for:
- Building control flow graphs from AST
- Analyzing statement reachability
- Determining dominance relationships

## Issue 3: Procedure Call Graph

**Problem**: No API exists to track procedure calls and build call graphs. This prevents detection of unused procedures.

**Impact**: Cannot determine if a procedure is called anywhere in the codebase.

**Test Cases**:
```fortran
module test_mod
contains
  subroutine used_sub()
    call helper()
  end subroutine
  
  subroutine unused_sub()  ! Should be detected as unused
    print *, 'never called'
  end subroutine
  
  subroutine helper()
    print *, 'helper'
  end subroutine
end module
```

**Suggested Solution**: Provide APIs for:
- Building procedure call graphs
- Resolving procedure references
- Tracking recursive calls

## Issue 4: Statement Block Relationships

**Problem**: The current AST structure makes it difficult to determine which statements follow terminating statements within the same block. The parent-child and sibling relationships are not clear enough.

**Impact**: Cannot accurately detect code after terminating statements in complex nested structures.

**Test Cases**:
```fortran
subroutine test()
  if (condition) then
    return
    x = 1  ! Should be unreachable
    y = 2  ! Should be unreachable
  end if
  z = 3  ! This is reachable
end subroutine
```

**Suggested Solution**: Enhance AST with:
- Clear sibling node relationships
- Block boundary information
- Statement sequence within blocks

## Issue 5: Parameter Attribute Information

**Problem**: The AST doesn't provide clear information about parameter attributes like `optional`, `intent(in/out/inout)`, making it difficult to apply different dead code rules to different parameter types.

**Impact**: Cannot distinguish between required and optional parameters, or properly analyze intent(out) parameters.

**Test Cases**:
```fortran
subroutine test(required, opt, output)
  integer, intent(in) :: required
  integer, intent(in), optional :: opt  ! Different rules apply
  integer, intent(out) :: output        ! Should not be flagged as unused
  
  output = required * 2
end subroutine
```

**Suggested Solution**: Include parameter attributes in AST nodes:
- Optional attribute
- Intent specification
- Other parameter attributes

## Summary

These limitations prevent full implementation of dead code detection as specified in the fluff requirements. The current implementation achieves 55.6% test success rate, with most failures directly attributable to these API limitations.