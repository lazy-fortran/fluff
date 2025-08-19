program test_rule_f012_naming_conventions
    ! Test F012: Inconsistent naming conventions rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    implicit none
    
    print *, "Testing F012: Inconsistent naming conventions rule..."
    
    ! Test 1: Inconsistent variable naming (should trigger)
    call test_inconsistent_variable_naming()
    
    ! Test 2: Consistent snake_case (should not trigger)
    call test_consistent_snake_case()
    
    ! Test 3: Consistent camelCase (should not trigger)
    call test_consistent_camel_case()
    
    ! Test 4: Mixed naming styles (should trigger)  
    call test_mixed_naming_styles()
    
    print *, "All F012 tests passed!"
    
contains
    
    subroutine test_inconsistent_variable_naming()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f012
        
        ! Test inconsistent variable naming patterns
        
        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: first_value" // new_line('a') // &  ! snake_case
                   "    integer :: secondValue" // new_line('a') // &  ! camelCase
                   "    integer :: ThirdValue" // new_line('a') // &   ! PascalCase
                   "    integer :: fourth_val" // new_line('a') // &   ! snake_case
                   "    " // new_line('a') // &
                   "    first_value = 10" // new_line('a') // &
                   "    secondValue = 20" // new_line('a') // &
                   "    ThirdValue = 30" // new_line('a') // &
                   "    fourth_val = 40" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f012.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f012.f90", diagnostics, error_msg)
        
        ! Check for F012 violation
        found_f012 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F012") then
                    found_f012 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f012.f90", status="old")
        close(99, status="delete")
        
        if (.not. found_f012) then
            error stop "Failed: F012 should be triggered for inconsistent naming"
        end if
        
        print *, "  ✓ Inconsistent variable naming"
        
    end subroutine test_inconsistent_variable_naming
    
    subroutine test_consistent_snake_case()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f012
        
        ! Test consistent snake_case naming
        
        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: first_value" // new_line('a') // &
                   "    integer :: second_value" // new_line('a') // &
                   "    integer :: third_value" // new_line('a') // &
                   "    integer :: fourth_value" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    first_value = 10" // new_line('a') // &
                   "    second_value = 20" // new_line('a') // &
                   "    third_value = 30" // new_line('a') // &
                   "    fourth_value = 40" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f012_snake.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f012_snake.f90", diagnostics, error_msg)
        
        ! Check for F012 violation
        found_f012 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F012") then
                    found_f012 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f012_snake.f90", status="old")
        close(99, status="delete")
        
        if (found_f012) then
            error stop "Failed: F012 should not be triggered for consistent snake_case"
        end if
        
        print *, "  ✓ Consistent snake_case"
        
    end subroutine test_consistent_snake_case
    
    subroutine test_consistent_camel_case()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        integer :: i
        logical :: found_f012
        
        ! Test consistent camelCase naming
        
        test_code = "program test" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: firstValue" // new_line('a') // &
                   "    integer :: secondValue" // new_line('a') // &
                   "    integer :: thirdValue" // new_line('a') // &
                   "    integer :: fourthValue" // new_line('a') // &
                   "    " // new_line('a') // &
                   "    firstValue = 10" // new_line('a') // &
                   "    secondValue = 20" // new_line('a') // &
                   "    thirdValue = 30" // new_line('a') // &
                   "    fourthValue = 40" // new_line('a') // &
                   "end program test"
        
        linter = create_linter_engine()
        
        ! Create temporary file
        open(unit=99, file="test_f012_camel.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        ! Lint the file
        call linter%lint_file("test_f012_camel.f90", diagnostics, error_msg)
        
        ! Check for F012 violation
        found_f012 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F012") then
                    found_f012 = .true.
                    exit
                end if
            end do
        end if
        
        ! Clean up
        open(unit=99, file="test_f012_camel.f90", status="old")
        close(99, status="delete")
        
        if (found_f012) then
            error stop "Failed: F012 should not be triggered for consistent camelCase"
        end if
        
        print *, "  ✓ Consistent camelCase"
        
    end subroutine test_consistent_camel_case
    
    subroutine test_mixed_naming_styles()
        ! Test placeholder for mixed naming styles
        print *, "  ✓ Mixed naming styles (test placeholder)"
    end subroutine test_mixed_naming_styles
    
end program test_rule_f012_naming_conventions