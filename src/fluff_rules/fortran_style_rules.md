# Fortran Style Rules for fluff

## Memory Management Rules

### F101: Avoid Unnecessary Explicit Deallocation
**Rule**: Do not explicitly deallocate allocatable variables when Fortran's automatic scope handling will do it.

**Rationale**: Fortran automatically deallocates allocatable variables when they go out of scope. Explicit deallocation is only needed when:
- You need to free memory before the variable goes out of scope
- You're reassigning an allocatable that already holds data
- You're in a long-running procedure and need to manage memory carefully

**Bad Example**:
```fortran
subroutine process_data()
    real, allocatable :: temp_array(:)
    
    allocate(temp_array(1000))
    ! ... use temp_array ...
    
    deallocate(temp_array)  ! Unnecessary - will be deallocated automatically
end subroutine
```

**Good Example**:
```fortran
subroutine process_data()
    real, allocatable :: temp_array(:)
    
    allocate(temp_array(1000))
    ! ... use temp_array ...
    
    ! No explicit deallocation needed - Fortran handles it
end subroutine
```

**Good Example (when deallocation IS needed)**:
```fortran
subroutine process_large_datasets()
    real, allocatable :: dataset1(:,:), dataset2(:,:)
    
    ! Process first dataset
    allocate(dataset1(10000, 10000))
    ! ... process dataset1 ...
    
    ! Free memory before processing second dataset
    deallocate(dataset1)  ! Good - freeing memory for reuse
    
    allocate(dataset2(10000, 10000))
    ! ... process dataset2 ...
end subroutine
```

### Implementation for fluff

To check this automatically:
1. Track allocatable variable declarations
2. Check for deallocate statements at end of scope
3. Flag unnecessary deallocations unless:
   - Variable is reallocated later
   - There's a comment explaining memory management needs
   - It's in a loop or long-running procedure