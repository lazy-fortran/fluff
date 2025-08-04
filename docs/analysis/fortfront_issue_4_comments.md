# Issue: Comments are not preserved in emit_fortran output

## Description
The emit_fortran function does not preserve comments from the original source code. Both standalone comments and inline comments are lost during the parsing/emission process.

## Steps to Reproduce
```fortran
program test
    ! This is a comment
    implicit none
    integer :: i  ! Inline comment
    ! Another comment
    i = 42
end program test
```

## Expected Behavior
Comments should be preserved in their original positions:
```fortran
program test
    ! This is a comment
    implicit none
    integer :: i  ! Inline comment
    ! Another comment
    i = 42
end program test
```

## Actual Behavior
All comments are stripped from the output:
```fortran
program test
    implicit none
    integer :: i

    i = 42
end program test
```

## Impact
Comments are essential for code documentation and understanding. A code formatter that strips comments would be unusable in practice. This is a critical feature for any source-to-source transformation tool.

## Test Case
See `/home/ert/code/fluff/test/test_fortfront_comments.f90` for a test case demonstrating the issue.

## Possible Solutions
1. Preserve comments as special nodes in the AST
2. Attach comments to the nearest AST node
3. Store comments separately with position information and re-insert during emission