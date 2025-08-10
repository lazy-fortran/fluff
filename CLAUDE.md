## Project Purpose

The purpose of this project is to provide a tool like "ruff" for Python in the form of "fluff" for Fortran. We will also support lazy fortran in addition - a dialect that can be converted to usual Fortran via the fortfront CLI or API. fortfront can generate an AST and do semantic analysis for type inference for a typed AST, meaning that we just have to use this information and do further static analysis and advanced code formatting.

## Development

- Build with `fpm build`
- Test with `fpm test` - **ALWAYS USE FPM TEST FOR TESTING!**
- Run tests early and often
- **NEVER compile test programs manually** - use fpm test structure
- Create test files in `test/` directory with names like `test_*.f90`

## Fortfront AST API Integration

### Available AST API Components

The fortfront library provides a comprehensive AST API located in `/home/ert/code/fortfront/` with the following key modules:

#### Core AST Components
- **ast_core.f90**: Main compatibility module that re-exports all AST functionality
- **ast_arena.f90**: High-performance arena-based AST storage system with traversal methods
- **ast_visitor.f90**: Abstract visitor pattern for AST traversal and analysis
- **ast_nodes_*.f90**: Complete node type definitions (core, control, procedure, data, io, misc)

#### Semantic Analysis
- **semantic_analyzer.f90**: Hindley-Milner type inference with semantic context
- **scope_manager.f90**: Hierarchical scope management
- **type_checker.f90**: Type checking and validation
- **parameter_tracker.f90**: Parameter attribute tracking

#### Key Public API Functions Available
```fortran
use fortfront, only: ast_arena_t, semantic_context_t, token_t, &
                     lex_source, parse_tokens, analyze_semantics, &
                     create_ast_arena, create_semantic_context, &
                     emit_fortran, transform_lazy_fortran_string_with_format, &
                     format_options_t
```

#### AST Analysis Capabilities
- **Node Traversal**: `ast_arena_t` provides `traverse_depth`, `find_by_type`, `get_children`
- **Semantic Context**: Full type information and scope resolution available
- **Visitor Pattern**: Abstract `ast_visitor_t` for implementing custom analysis
- **Node Types**: Complete coverage of Fortran constructs including:
  - Control flow: if_node, do_loop_node, select_case_node, etc.
  - Declarations: declaration_node, parameter_declaration_node, etc.
  - Procedures: function_def_node, subroutine_def_node, etc.
  - Expressions: binary_op_node, call_or_subscript_node, etc.

### Integration Status in fluff

Current fluff modules already using fortfront AST API:
- **fluff_ast.f90**: Wrapper around fortfront arena and semantic context
- **fluff_formatter.f90**: Uses fortfront's formatting and emission capabilities
- Multiple test files demonstrating AST parsing, analysis, and code generation

### Implementation Requirements

**CRITICAL**: All rule implementations must use the available fortfront AST API instead of text-based analysis. The infrastructure is already in place:

1. Parse source to AST using `lex_source` → `parse_tokens` → `analyze_semantics`
2. Use `ast_visitor_t` pattern or direct arena traversal for analysis
3. Access semantic information through `semantic_context_t`
4. Implement rule logic using typed AST nodes and semantic context

### API Guidelines

- **ALWAYS** use the fortfront AST API for static analysis - it's comprehensive and available
- Access semantic information through the existing `semantic_context_t` 
- Use the visitor pattern for complex AST traversals
- File GitHub issues in ../fortfront only if specific public API functions are missing
- Text-based analysis is only acceptable as a temporary workaround with clear TODO comments