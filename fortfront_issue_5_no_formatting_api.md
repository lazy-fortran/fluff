# Issue: No API for configurable code formatting

## Description
The emit_fortran function does not provide any parameters for controlling code formatting options such as:
- Indentation size
- Use of tabs vs spaces
- Line length limits
- Expression breaking strategies
- Style preferences

## Current API
```fortran
subroutine emit_fortran(arena, prog_index, fortran_code)
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(inout) :: prog_index
    character(len=:), allocatable, intent(out) :: fortran_code
end subroutine
```

## Requested API
Something like:
```fortran
type :: emit_options_t
    integer :: indent_size = 4
    logical :: use_spaces = .true.
    integer :: line_length = 88
    ! ... other options
end type

subroutine emit_fortran_with_options(arena, prog_index, fortran_code, options)
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(inout) :: prog_index
    character(len=:), allocatable, intent(out) :: fortran_code
    type(emit_options_t), intent(in), optional :: options
end subroutine
```

## Impact
Without configurable formatting options, fortfront cannot be used as the backend for a flexible code formatter like fluff. Different projects and organizations have different style guides and formatting preferences.

## Note
This might be intentional - fortfront may be designed to emit canonical/standard Fortran rather than provide formatting flexibility. If so, fluff would need to implement its own AST traversal and code generation, using fortfront only for parsing/semantic analysis.