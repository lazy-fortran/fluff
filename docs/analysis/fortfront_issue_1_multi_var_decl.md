# Issue: Multi-variable declarations not parsed correctly

## Description
When parsing Fortran code with multi-variable declarations (e.g., `real :: a, b, c`), fortfront only preserves the first variable and drops the rest.

## Steps to Reproduce
```fortran
program test
    implicit none
    real :: result, a, b, c, d, e, f
    result = a * b + c * d
end program test
```

## Expected Behavior
The emit_fortran function should output:
```fortran
program test
    implicit none
    real :: result, a, b, c, d, e, f
    result = a * b + c * d
end program test
```

## Actual Behavior
The emit_fortran function outputs:
```fortran
program test
    implicit none
    real(8) :: result
end program test
```

Note that:
1. Only the first variable `result` is preserved
2. Variables `a, b, c, d, e, f` are dropped
3. The assignment statement is also dropped (likely because the variables it references don't exist)

## Impact
This prevents proper formatting of Fortran code with multi-variable declarations, which are very common in Fortran programs.

## Test Case
See `/home/ert/code/fluff/test/test_fortfront_multi_vars.f90` for a minimal reproducible test case.