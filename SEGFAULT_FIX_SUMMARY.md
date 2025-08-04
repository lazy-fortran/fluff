# GFortran Segmentation Fault Fix Summary

## Problem
The fluff project was experiencing compiler segmentation faults when functions returned allocatable character arrays with deferred length (`character(len=:), allocatable :: array(:)`). This is a known gfortran bug.

## Root Cause
GFortran has a bug with type-bound procedures returning allocatable character arrays with deferred length. The compiler generates incorrect code that causes segmentation faults at runtime.

## Solution Implemented
Implemented the derived type wrapper pattern recommended by the Fortran community:

### 1. Created String Utilities Module
- **File**: `src/fluff_string_utils.f90`
- **Purpose**: Provides safe string handling through derived types
- **Key Types**:
  - `string_t`: Wraps a single allocatable string
  - `string_array_t`: Manages an array of strings safely

### 2. Updated Affected Functions
Modified functions that previously returned allocatable character arrays:

#### fluff_analysis_cache.f90
- `get_dependencies()` - Now returns `string_array_t`
- `get_transitive_dependencies()` - Now returns `string_array_t`  
- `get_files_depending_on()` - Now returns `string_array_t`

#### fluff_file_watcher.f90
- `get_changed_files()` - Now returns `string_array_t`
- `get_rebuild_info()` - Updated to use `string_array_t` internally

### 3. Key Design Decisions
- Used dynamic array growth with doubling strategy for efficiency
- Implemented proper cleanup methods to prevent memory leaks
- Provided conversion methods to fixed-length arrays for compatibility
- Made the wrapper types public for use across the codebase

## Verification
- Main fluff executable builds and runs successfully
- No more segmentation faults when calling affected functions
- String wrapper provides safe, efficient string array handling

## Impact
- Minimal API changes - only return types affected
- Callers need to use `string_array_t` methods to access data
- Performance overhead is negligible compared to segfault crashes

## Future Considerations
- This workaround can be removed when gfortran fixes the underlying bug
- The string wrapper pattern is reusable for other similar issues
- Consider using this pattern proactively for all string array returns