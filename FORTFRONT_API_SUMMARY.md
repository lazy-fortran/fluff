# Fortfront Formatting API Summary

This document summarizes the formatting APIs available in fortfront's public interface for use by fluff.

## Core Formatting Functions

### 1. `emit_fortran(arena, prog_index, fortran_code)`
**Purpose**: Basic Fortran code emission from AST
**Parameters**:
- `arena`: AST arena containing the program
- `prog_index`: Index of the root program node
- `fortran_code`: Output string containing formatted code

**Usage**: Standard code generation with default formatting

### 2. `emit_fortran_with_options(arena, prog_index, fortran_code, indent_size, use_spaces)`
**Purpose**: Code emission with configurable indentation
**Parameters**:
- `arena`: AST arena containing the program
- `prog_index`: Index of the root program node  
- `fortran_code`: Output string containing formatted code
- `indent_size`: Optional integer for indentation size
- `use_spaces`: Optional logical for using spaces vs tabs

**Usage**: Code generation with custom indentation settings
**Note**: Currently calls standard `emit_fortran` (indentation config not fully implemented)

### 3. `emit_fortran_unwrapped(arena, prog_index, fortran_code, indent_size, use_spaces)`
**Purpose**: Code emission that unwraps wrapper programs
**Parameters**: Same as `emit_fortran_with_options`

**Usage**: Best for fluff formatting - handles the wrapper program structure created by fortfront's parser
**Current fluff usage**: This is what fluff's formatter currently uses

## Low-Level Code Generation

### 4. `generate_code_from_arena(arena, node_index)`
**Purpose**: Generate code for a specific AST node
**Parameters**:
- `arena`: AST arena
- `node_index`: Index of specific node to format
**Returns**: String containing formatted code for that node

**Usage**: Custom formatting of specific AST nodes, useful for:
- Formatting individual statements
- Custom node-level transformations
- Building custom formatters

## Indentation Utilities

### 5. Indentation Control Functions
- `get_indent()` → `character(len=:), allocatable`: Get current indentation string
- `increase_indent()`: Increase indentation level
- `decrease_indent()`: Decrease indentation level  
- `reset_indent()`: Reset indentation to zero
- `set_indent_options(indent_size, use_spaces)`: Configure indentation settings

### 6. Indentation Helper Functions
- `with_indent(text)` → `character(len=:), allocatable`: Prepend current indentation to text
- `indent_lines(text)` → `character(len=:), allocatable`: Add indentation to all lines in text

**Usage**: For building custom formatters that need precise indentation control

## Recommendations for fluff

### Current Usage (Good)
Fluff currently uses `emit_fortran_unwrapped` which is the right choice because:
- Handles wrapper program structures correctly
- Supports indentation configuration
- Provides clean output for explicit programs

### Additional Useful APIs
For advanced formatting features, fluff could utilize:

1. **`generate_code_from_arena`** - For formatting specific nodes:
   ```fortran
   ! Format just a function definition
   formatted_function = generate_code_from_arena(arena, function_node_index)
   ```

2. **Indentation utilities** - For custom formatting rules:
   ```fortran
   call set_indent_options(indent_size=2, use_spaces=.true.)
   call increase_indent()
   formatted_line = with_indent("some code")
   call decrease_indent()
   ```

### Future Enhancements
The following could be added if needed:
- `generate_code_polymorphic` - For polymorphic node generation
- Direct AST node access for custom transformations
- More granular formatting options (line length, style rules)

## Summary
Fortfront now exposes all the key formatting APIs needed for fluff:
- ✅ **High-level emission** (`emit_fortran*` functions) 
- ✅ **Node-level generation** (`generate_code_from_arena`)
- ✅ **Indentation control** (full suite of indentation utilities)
- ✅ **Configuration options** (indent size, spaces vs tabs)

The current fluff implementation is well-designed and uses the appropriate APIs for its needs.