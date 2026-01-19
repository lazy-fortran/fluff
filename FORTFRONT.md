# Fortfront API Reference

## Overview

fluff uses fortfront for all AST analysis. This document describes the fortfront public API used by fluff.

## Core Pipeline Functions

- `lex_source` - Lexical analysis
- `parse_tokens` - AST construction from tokens
- `analyze_semantics` - Semantic analysis with type inference
- `emit_fortran` - Code generation
- `transform_lazy_fortran_string` - Lazy Fortran to standard Fortran

## AST Arena Management

- `ast_arena_t` - AST storage and management
- `create_ast_arena()` - Arena creation
- `get_node(arena, index)` - Node access by index
- `get_children(arena, index)` - Child node retrieval
- `get_parent(arena, index)` - Parent node retrieval
- `traverse_ast(arena, root, callback)` - AST traversal

## Semantic Analysis

- `semantic_context_t` - Semantic analysis context
- `create_semantic_context()` - Context creation
- `analyze_program(ctx, arena, prog_index)` - Program analysis
- `get_type_for_node(arena, index, type, found)` - Type information access
- `lookup_symbol(ctx, name, scope)` - Symbol table queries

## AST Node Types

36 node types supported including:
- Program, Function, Assignment, Binary operations
- Identifiers, Literals, Array literals, Function calls
- Control flow (if, do, select case, where)
- Declarations, Modules, I/O statements
- Memory management (allocate/deallocate)

## Type System

- `mono_type_t`, `poly_type_t` - Type representations
- `TINT`, `TREAL`, `TCHAR`, `TLOGICAL` - Base types
- `TFUN`, `TARRAY`, `TVAR` - Complex types

## Diagnostic System

- `diagnostic_t` - Error/warning representation
- `source_location_t`, `source_range_t` - Position tracking
- Severity levels: ERROR, WARNING, INFO, HINT

## Intrinsic Function Support

- `is_intrinsic_function(name)` - Intrinsic detection
- `get_intrinsic_signature(name)` - Function signatures
- Comprehensive intrinsic library (sin, cos, len, etc.)

## Utility Functions

- `ast_to_json()` - AST serialization
- `get_arena_stats()` - Memory usage statistics
- `find_nodes_by_type()` - Node searching
- `get_max_depth()` - AST depth analysis

## Usage in fluff Rules

### Style Rules (F001-F015)

- **F001 (implicit none)**: AST traversal to find program/function nodes
- **F006 (unused variables)**: Symbol table lookup and usage tracking
- **F007 (undefined variables)**: Semantic context symbol resolution
- **F008 (missing intent)**: Parameter declaration node inspection

### Performance Rules (P001-P007)

- **P001 (array access)**: Array subscript pattern analysis
- **P004 (pure/elemental)**: Function definition node attributes
- **P007 (mixed precision)**: Type inference for precision checks

### Correctness Rules (C001)

- **C001 (undefined variables)**: Semantic analyzer symbol resolution

## API Usage Pattern

```fortran
use fortfront, only: ast_arena_t, semantic_context_t, &
                     lex_source, parse_tokens, analyze_semantics, &
                     traverse_ast, get_node, get_children, &
                     get_type_for_node, lookup_symbol
```

For more details, see the [fortfront repository](https://github.com/lazy-fortran/fortfront).
