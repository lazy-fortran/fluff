# fortfront Public API Requirements

## Overview

This document specifies the public API that `fortfront` should provide to enable `fluff` to perform comprehensive static analysis, linting, and formatting of Standard and Lazy Fortran code. The API should be accessible through a single high-level facade module `fortfront` that exposes all necessary functionality.

## Core Pipeline API

### Module: `fortfront`

The main public interface should provide access to all phases of the compilation pipeline:

```fortran
module fortfront
    use frontend, only: lex_source, parse_tokens, analyze_semantics, emit_fortran, &
                       transform_lazy_fortran_string, compilation_options_t
    use ast_core, only: ast_arena_t, ast_node, program_node, assignment_node, &
                        binary_op_node, function_def_node, identifier_node, &
                        literal_node, array_literal_node
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use lexer_core, only: token_t, tokenize_core
    ! ... other imports
    implicit none
    public
end module fortfront
```

## 1. Lexical Analysis

### Types
```fortran
type :: token_t
    integer :: kind          ! Token type (TK_KEYWORD, TK_IDENTIFIER, etc.)
    character(len=:), allocatable :: text
    integer :: line
    integer :: column
end type token_t
```

### Procedures
```fortran
subroutine lex_source(source_code, tokens, error_msg)
    character(len=*), intent(in) :: source_code
    type(token_t), allocatable, intent(out) :: tokens(:)
    character(len=:), allocatable, intent(out) :: error_msg
end subroutine

! Low-level tokenization
subroutine tokenize_core(source, tokens)
    character(len=*), intent(in) :: source
    type(token_t), allocatable, intent(out) :: tokens(:)
end subroutine
```

## 2. AST Types and Arena

### Core AST Arena
```fortran
type :: ast_arena_t
    ! High-performance arena storage for AST nodes
    ! Internal implementation details hidden
contains
    procedure :: push        ! Add node to arena
    procedure :: get_node    ! Retrieve node by index
    procedure :: get_parent  ! Get parent node
    procedure :: get_children ! Get child nodes
    procedure :: traverse    ! Depth/breadth-first traversal
    procedure :: find_by_type ! Find nodes of specific type
    procedure :: get_stats   ! Performance statistics
end type ast_arena_t
```

### AST Node Types
All AST nodes should extend the base `ast_node` type and include:

```fortran
! Base AST node with position and type information
type, abstract :: ast_node
    integer :: line = 1
    integer :: column = 1
    type(mono_type_t), allocatable :: inferred_type  ! From semantic analysis
contains
    procedure(visit_interface), deferred :: accept   ! Visitor pattern
    procedure(to_json_interface), deferred :: to_json ! Serialization
end type ast_node

! Essential node types for fluff
type, extends(ast_node) :: program_node
    character(len=:), allocatable :: name
    integer, allocatable :: body_indices(:)
end type program_node

type, extends(ast_node) :: assignment_node  
    integer :: target_index
    integer :: value_index
    logical :: type_was_inferred = .false.
    character(len=:), allocatable :: inferred_type_name
end type assignment_node

type, extends(ast_node) :: binary_op_node
    integer :: left_index
    integer :: right_index  
    character(len=:), allocatable :: operator
end type binary_op_node

type, extends(ast_node) :: function_def_node
    character(len=:), allocatable :: name
    integer, allocatable :: param_indices(:)
    character(len=:), allocatable :: return_type
    integer, allocatable :: body_indices(:)
end type function_def_node

type, extends(ast_node) :: identifier_node
    character(len=:), allocatable :: name
end type identifier_node

type, extends(ast_node) :: literal_node
    character(len=:), allocatable :: value
    integer :: literal_kind  ! INTEGER_LITERAL, REAL_LITERAL, etc.
end type literal_node

! Additional node types needed for comprehensive analysis
type, extends(ast_node) :: if_statement_node
type, extends(ast_node) :: do_loop_node  
type, extends(ast_node) :: variable_declaration_node
type, extends(ast_node) :: array_literal_node
type, extends(ast_node) :: call_or_subscript_node
type, extends(ast_node) :: module_node
type, extends(ast_node) :: use_statement_node
```

## 3. Parsing API

```fortran
subroutine parse_tokens(tokens, arena, prog_index, error_msg)
    type(token_t), intent(in) :: tokens(:)
    type(ast_arena_t), intent(inout) :: arena  
    integer, intent(out) :: prog_index          ! Root program node index
    character(len=*), intent(out) :: error_msg
end subroutine

! Arena factory
function create_ast_arena() result(arena)
    type(ast_arena_t) :: arena
end function
```

## 4. Semantic Analysis

### Types
```fortran
type :: semantic_context_t
    ! Encapsulates type environment, scope management, and inference state
    ! Internal details hidden
contains
    procedure :: get_type_for_node      ! Get inferred type for AST node
    procedure :: get_scope_info         ! Get variable scope information  
    procedure :: get_symbol_definition  ! Find where symbol is defined
    procedure :: get_symbol_references  ! Find all references to symbol
    procedure :: is_variable_used       ! Check if variable is used
    procedure :: get_function_signature ! Get function parameter/return types
end type semantic_context_t
```

### Procedures
```fortran
function create_semantic_context() result(ctx)
    type(semantic_context_t) :: ctx
end function

subroutine analyze_semantics(arena, prog_index)
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(in) :: prog_index
end subroutine

! Alternative with explicit context for advanced use
subroutine analyze_program(ctx, arena, prog_index)
    type(semantic_context_t), intent(inout) :: ctx
    type(ast_arena_t), intent(inout) :: arena  
    integer, intent(in) :: prog_index
end subroutine
```

## 5. Type System Integration

### Type Information
```fortran
! Monomorphic types from Hindley-Milner system
type :: mono_type_t
    integer :: kind  ! TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY, etc.
    ! Additional type information hidden in implementation
contains
    procedure :: to_string    ! Human-readable type representation
    procedure :: is_numeric   ! Check if numeric type
    procedure :: is_array     ! Check if array type
    procedure :: array_rank   ! Get array dimensions
    procedure :: element_type ! Get array element type
    procedure :: is_compatible_with ! Type compatibility checking
end type mono_type_t

! Access to type constants
integer, parameter :: TINT = 1, TREAL = 2, TCHAR = 3, TLOGICAL = 4
integer, parameter :: TFUN = 5, TARRAY = 6, TVAR = 7
```

## 6. Code Generation and Formatting

```fortran
subroutine emit_fortran(arena, prog_index, fortran_code)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: prog_index
    character(len=:), allocatable, intent(out) :: fortran_code
end subroutine

! High-level transformation pipeline
subroutine transform_lazy_fortran_string(input, output, error_msg)
    character(len=*), intent(in) :: input
    character(len=:), allocatable, intent(out) :: output
    character(len=:), allocatable, intent(out) :: error_msg
end subroutine
```

## 7. Source Location and Ranges

For precise error reporting and IDE integration:

```fortran
type :: source_location_t
    integer :: line
    integer :: column  
    integer :: byte_offset  ! For efficient text manipulation
end type source_location_t

type :: source_range_t
    type(source_location_t) :: start
    type(source_location_t) :: end
end type source_range_t

! Extract source ranges from AST nodes
function get_node_range(arena, node_index) result(range)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: node_index
    type(source_range_t) :: range
end function
```

## 8. Error Handling and Diagnostics

```fortran
type :: diagnostic_t
    integer :: severity  ! ERROR, WARNING, INFO, HINT
    character(len=:), allocatable :: message
    type(source_range_t) :: location
    character(len=:), allocatable :: code      ! Error code (e.g., "F001")
    character(len=:), allocatable :: category  ! Error category
end type diagnostic_t

! Collect diagnostics during analysis
function get_diagnostics(ctx) result(diagnostics)
    type(semantic_context_t), intent(in) :: ctx
    type(diagnostic_t), allocatable :: diagnostics(:)
end function
```

## 9. Visitor Pattern Support

For AST traversal by fluff rules:

```fortran
abstract interface
    subroutine visitor_interface(this, visitor)
        import :: ast_node
        class(ast_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine
end interface

! Generic traversal procedures
subroutine traverse_ast(arena, root_index, visitor, pre_order)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: root_index
    class(*), intent(inout) :: visitor
    logical, intent(in), optional :: pre_order  ! Default: true
end subroutine
```

## 10. JSON Serialization

For debugging and tool integration:

```fortran
subroutine ast_to_json(arena, root_index, json_string)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: root_index
    character(len=:), allocatable, intent(out) :: json_string
end subroutine

subroutine semantic_info_to_json(ctx, json_string)
    type(semantic_context_t), intent(in) :: ctx
    character(len=:), allocatable, intent(out) :: json_string
end subroutine
```

## 11. Configuration and Options

```fortran
type :: compilation_options_t
    logical :: debug_tokens = .false.
    logical :: debug_ast = .false.
    logical :: debug_semantic = .false.
    logical :: debug_standardize = .false.
    logical :: debug_codegen = .false.
    character(len=:), allocatable :: output_file
end type compilation_options_t
```

## 12. Performance and Caching

```fortran
type :: ast_arena_stats_t
    integer :: total_nodes
    integer :: max_depth
    integer :: capacity
    integer :: memory_usage  ! Approximate bytes
end type ast_arena_stats_t

function get_arena_stats(arena) result(stats)
    type(ast_arena_t), intent(in) :: arena
    type(ast_arena_stats_t) :: stats
end function
```

## Usage Patterns for fluff

### Basic Analysis Pipeline
```fortran
use fortfront

type(token_t), allocatable :: tokens(:)
type(ast_arena_t) :: arena
type(semantic_context_t) :: ctx
integer :: prog_index
character(len=:), allocatable :: error_msg, code

! 1. Lexical analysis
call lex_source(source_code, tokens, error_msg)
if (error_msg /= "") return

! 2. Parse to AST
arena = create_ast_arena()
call parse_tokens(tokens, arena, prog_index, error_msg)
if (error_msg /= "") return

! 3. Semantic analysis  
ctx = create_semantic_context()
call analyze_program(ctx, arena, prog_index)

! 4. Access typed AST for linting rules
call my_linting_rule(arena, ctx, prog_index)

! 5. Generate formatted code
call emit_fortran(arena, prog_index, code)
```

### Rule Development Pattern
```fortran
subroutine check_unused_variables(arena, ctx, root_index, violations)
    type(ast_arena_t), intent(in) :: arena
    type(semantic_context_t), intent(in) :: ctx
    integer, intent(in) :: root_index
    type(diagnostic_t), allocatable, intent(out) :: violations(:)
    
    ! Traverse AST and use semantic context to find unused variables
    ! Implementation can access full type information and scope data
end subroutine
```

## Implementation Priority

1. **Core pipeline** - Complete lex/parse/semantic/codegen chain
2. **AST types** - All essential AST node types
3. **Type system** - Mono-type access and introspection
4. **Visitor pattern** - Generic AST traversal
5. **Source locations** - Precise position tracking
6. **Diagnostics** - Error collection and reporting
7. **JSON serialization** - Tool integration support
8. **Performance APIs** - Statistics and caching hooks

This API design enables fluff to perform comprehensive static analysis while maintaining clean separation of concerns and allowing for high-performance implementation.