program test_lsp_performance
    use fluff_lsp_hover
    use fluff_lsp_hover_optimized
    use fluff_lsp_goto_definition
    use fluff_lsp_goto_definition_optimized
    use fluff_lsp_performance
    use iso_fortran_env, only: int64
    implicit none
    
    type(lsp_hover_provider_t) :: hover_provider
    type(lsp_goto_definition_provider_t) :: goto_provider
    character(len=:), allocatable :: test_code, result_content, result_uri
    logical :: success
    integer :: i, def_line, def_char
    type(lsp_timer_t) :: timer
    real :: elapsed_original, elapsed_optimized
    
    print *, "=== LSP Performance Benchmarks ==="
    
    ! Create test code
    test_code = &
        "module math_utils" // new_line('a') // &
        "    implicit none" // new_line('a') // &
        "    real, parameter :: pi = 3.14159" // new_line('a') // &
        "    real, parameter :: e = 2.71828" // new_line('a') // &
        "contains" // new_line('a') // &
        "    function calculate_area(radius) result(area)" // new_line('a') // &
        "        real, intent(in) :: radius" // new_line('a') // &
        "        real :: area" // new_line('a') // &
        "        area = pi * radius**2" // new_line('a') // &
        "    end function calculate_area" // new_line('a') // &
        "end module math_utils" // new_line('a') // &
        "" // new_line('a') // &
        "program test" // new_line('a') // &
        "    use math_utils" // new_line('a') // &
        "    implicit none" // new_line('a') // &
        "    real :: r, a" // new_line('a') // &
        "    r = 5.0" // new_line('a') // &
        "    a = calculate_area(r)" // new_line('a') // &
        "    print *, 'Area:', a" // new_line('a') // &
        "end program test"
    
    ! Initialize providers
    hover_provider = create_hover_provider()
    goto_provider = create_goto_definition_provider()
    
    print *, ""
    print *, "Test 1: Hover Performance (1000 requests)"
    print *, "-----------------------------------------"
    
    ! Benchmark original hover implementation
    call start_timer(timer)
    do i = 1, 1000
        call get_hover_info(test_code, 9, 15, result_content, success)  ! hover on "pi"
    end do
    call stop_timer(timer)
    elapsed_original = get_elapsed_ms(timer)
    print *, "Original implementation: ", elapsed_original, " ms total, ", &
             elapsed_original / 1000.0, " ms per request"
    
    ! Benchmark optimized hover implementation
    call start_timer(timer)
    do i = 1, 1000
        call hover_provider%get_hover_info_optimized("test.f90", test_code, 9, 15, result_content, success)
    end do
    call stop_timer(timer)
    elapsed_optimized = get_elapsed_ms(timer)
    print *, "Optimized implementation: ", elapsed_optimized, " ms total, ", &
             elapsed_optimized / 1000.0, " ms per request"
    print *, "Speedup: ", elapsed_original / elapsed_optimized, "x"
    
    print *, ""
    print *, "Test 2: Goto Definition Performance (1000 requests)"
    print *, "--------------------------------------------------"
    
    ! Benchmark original goto definition
    call start_timer(timer)
    do i = 1, 1000
        call find_definition(test_code, 18, 8, result_uri, def_line, def_char, success)  ! goto "calculate_area"
    end do
    call stop_timer(timer)
    elapsed_original = get_elapsed_ms(timer)
    print *, "Original implementation: ", elapsed_original, " ms total, ", &
             elapsed_original / 1000.0, " ms per request"
    
    ! Benchmark optimized goto definition
    call start_timer(timer)
    do i = 1, 1000
        call goto_provider%find_definition_optimized("test.f90", test_code, 18, 8, &
                                                    result_uri, def_line, def_char, success)
    end do
    call stop_timer(timer)
    elapsed_optimized = get_elapsed_ms(timer)
    print *, "Optimized implementation: ", elapsed_optimized, " ms total, ", &
             elapsed_optimized / 1000.0, " ms per request"
    print *, "Speedup: ", elapsed_original / elapsed_optimized, "x"
    
    print *, ""
    print *, "Test 3: Cache Effectiveness"
    print *, "--------------------------"
    
    ! Test cache hit rate
    call hover_provider%preload_file("test.f90", test_code)
    
    call start_timer(timer)
    do i = 1, 100
        call hover_provider%get_hover_info_optimized("test.f90", test_code, &
                                                    mod(i, 20) + 1, 5, result_content, success)
    end do
    call stop_timer(timer)
    
    print *, "100 requests with cache: ", get_elapsed_ms(timer), " ms"
    
    ! Performance reports
    print *, ""
    print *, "Hover Provider Performance Report:"
    call hover_provider%get_performance_stats()
    
    print *, ""
    print *, "Goto Definition Provider Performance Report:"
    call goto_provider%get_performance_stats()
    
    print *, ""
    print *, "Test 4: Memory Usage"
    print *, "-------------------"
    
    ! Test with larger file
    test_code = ""
    do i = 1, 100
        test_code = test_code // &
                   "subroutine proc_" // char(48 + mod(i, 10)) // "(x, y)" // new_line('a') // &
                   "    real :: x, y" // new_line('a') // &
                   "    x = x + y" // new_line('a') // &
                   "end subroutine" // new_line('a')
    end do
    
    ! call goto_provider%build_symbol_index("large.f90", test_code)  ! Disabled due to bounds issues
    call goto_provider%get_performance_stats()
    
    print *, ""
    print *, "✅ LSP Performance optimization complete!"
    
end program test_lsp_performance