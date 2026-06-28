program test_rules_command
    use fluff_rules, only: get_all_builtin_rules
    use fluff_rule_types, only: rule_info_t
    implicit none

    type(rule_info_t), allocatable :: rules(:)
    integer :: i, found_f001

    found_f001 = 0

    rules = get_all_builtin_rules()

    if (.not. allocated(rules)) then
        print *, "FAIL: rules array not allocated"
        stop 1
    end if

    if (size(rules) == 0) then
        print *, "FAIL: rules array is empty"
        stop 1
    end if

    do i = 1, size(rules)
        if (trim(rules(i)%code) == "F001") then
            found_f001 = 1
        end if
    end do

    if (found_f001 == 0) then
        print *, "FAIL: F001 rule not found"
        stop 1
    end if

    print *, "PASS"
    stop 0

end program test_rules_command
