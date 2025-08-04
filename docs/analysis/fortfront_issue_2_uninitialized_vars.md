# Issue: Assignment statements with uninitialized variables are dropped

## Description
When parsing Fortran code, fortfront's semantic analyzer drops assignment statements that reference uninitialized variables, even though this is valid Fortran code.

## Steps to Reproduce
```fortran
program test
    implicit none
    real :: result
    real :: a
    real :: b
    real :: c
    result = a * b + c
end program test
```

## Expected Behavior
The emit_fortran function should preserve all statements:
```fortran
program test
    implicit none
    real :: result
    real :: a
    real :: b
    real :: c
    result = a * b + c
end program test
```

## Actual Behavior
The emit_fortran function drops the assignment statement:
```fortran
program test
    implicit none
    real(8) :: result
    real(8) :: a
    real(8) :: b
    real(8) :: c
end program test
```

## Impact
This prevents proper formatting and analysis of Fortran code that uses uninitialized variables. While using uninitialized variables is bad practice, it's syntactically valid Fortran and the formatter should preserve the code structure.

## Workaround
None currently. The semantic analyzer needs to be more lenient during parsing/formatting operations.

## Test Case
See the test cases in `/home/ert/code/fluff/test/test_formatter_advanced.f90`