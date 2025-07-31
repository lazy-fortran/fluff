# Issue: Assignment statements are dropped by emit_fortran

## Description
The emit_fortran function drops assignment statements from the AST, even when all variables are properly declared and initialized. This makes it impossible to format code that contains assignments.

## Steps to Reproduce
```fortran
program test
    implicit none
    real :: a = 1.0
    real :: b = 2.0
    real :: result
    result = a * b
end program test
```

## Expected Behavior
The emit_fortran function should preserve all statements including assignments:
```fortran
program test
    implicit none
    real :: a = 1.0
    real :: b = 2.0
    real :: result
    result = a * b
end program test
```

## Actual Behavior
The emit_fortran function only outputs declarations, dropping the assignment:
```fortran
program test
    implicit none
    real(8) :: a = 1.0d0
    real(8) :: b = 2.0d0
    real(8) :: result
end program test
```

## Impact
This is a critical issue that prevents fortfront from being used as a code formatter. Without the ability to preserve assignment statements (and likely other executable statements), the formatter cannot maintain the semantics of the original code.

## Test Case
See `/home/ert/code/fluff/test/test_fortfront_init_vars.f90` for a minimal reproducible example.

## Analysis
It appears that emit_fortran might only be emitting declaration nodes and not executable statements from the AST. The parser seems to be parsing them correctly (no errors), but the code generator is not outputting them.