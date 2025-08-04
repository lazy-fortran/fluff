program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_core_basics, only: collect_core_basics
    use test_diagnostics, only: collect_diagnostics
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'
    
    stat = 0
    
    testsuites = [ &
        new_testsuite("core_basics", collect_core_basics), &
        new_testsuite("diagnostics", collect_diagnostics) &
    ]
    
    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do
    
    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
    
end program tester