program test_rule_registry
    ! Test rule registry functionality
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_config
    implicit none
    
    print *, "Testing rule registry..."
    
    ! Test 1: Rule discovery
    call test_rule_discovery()
    
    ! Test 2: Rule filtering by selection
    call test_rule_filtering()
    
    ! Test 3: Rule execution order
    call test_execution_order()
    
    ! Test 4: Find rule by code
    call test_find_by_code()
    
    print *, "All rule registry tests passed!"
    
contains
    
    subroutine test_rule_discovery()
        type(rule_registry_t) :: registry
        integer :: initial_count, final_count
        
        ! Get initial count
        initial_count = registry%get_rule_count()
        
        ! Discover built-in rules
        call registry%discover_builtin_rules()
        
        ! Get final count
        final_count = registry%get_rule_count()
        
        if (final_count <= initial_count) then
            error stop "Failed: should discover built-in rules"
        end if
        
        print *, "  ✓ Rule discovery found", final_count, "rules"
    end subroutine test_rule_discovery
    
    subroutine test_rule_filtering()
        type(rule_registry_t) :: registry
        type(rule_info_t), allocatable :: enabled_rules(:)
        type(rule_selection_t) :: selection
        integer :: i
        
        ! Set up test rules
        call registry%discover_builtin_rules()
        
        ! Configure selection
        allocate(character(len=1) :: selection%select(1))
        selection%select(1) = "F"  ! Only style rules
        
        ! Get enabled rules
        enabled_rules = registry%get_enabled_rules(selection)
        
        if (.not. allocated(enabled_rules)) then
            error stop "Failed: enabled rules should be allocated"
        end if
        
        ! Verify all rules start with F
        do i = 1, size(enabled_rules)
            if (enabled_rules(i)%code(1:1) /= "F") then
                error stop "Failed: should only return F rules"
            end if
        end do
        
        print *, "  ✓ Rule filtering by selection"
    end subroutine test_rule_filtering
    
    subroutine test_execution_order()
        type(rule_registry_t) :: registry
        type(rule_info_t), allocatable :: rules(:)
        integer :: i
        
        ! Set up rules with priorities
        call registry%discover_builtin_rules()
        
        ! Get rules in execution order
        rules = registry%get_rules_by_priority()
        
        if (.not. allocated(rules)) then
            error stop "Failed: rules should be allocated"
        end if
        
        ! Verify correctness rules come before style rules
        ! (assuming correctness has higher priority)
        do i = 2, size(rules)
            if (rules(i-1)%category == "style" .and. &
                rules(i)%category == "correctness") then
                error stop "Failed: correctness rules should come first"
            end if
        end do
        
        print *, "  ✓ Rule execution order"
    end subroutine test_execution_order
    
    subroutine test_find_by_code()
        type(rule_registry_t) :: registry
        type(rule_info_t), pointer :: rule
        
        ! Set up registry
        call registry%discover_builtin_rules()
        
        ! Find existing rule
        rule => registry%find_by_code("F001")
        
        if (.not. associated(rule)) then
            error stop "Failed: should find rule F001"
        end if
        
        if (rule%code /= "F001") then
            error stop "Failed: wrong rule returned"
        end if
        
        ! Find non-existent rule
        rule => registry%find_by_code("X999")
        
        if (associated(rule)) then
            error stop "Failed: should not find non-existent rule"
        end if
        
        print *, "  ✓ Find rule by code"
    end subroutine test_find_by_code
    
end program test_rule_registry