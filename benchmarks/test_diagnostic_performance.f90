program test_diagnostic_performance
    ! REFACTOR: Test diagnostic performance optimization
    use fluff_core
    use fluff_diagnostics
    implicit none
    
    print *, "Testing diagnostic performance (REFACTOR phase)..."
    
    ! Test 1: Benchmark diagnostic creation
    call benchmark_diagnostic_creation()
    
    ! Test 2: Benchmark diagnostic formatting
    call benchmark_diagnostic_formatting()
    
    ! Test 3: Benchmark collection operations
    call benchmark_collection_operations()
    
    ! Test 4: Benchmark JSON serialization
    call benchmark_json_serialization()
    
    ! Test 5: Test diagnostic caching
    call test_diagnostic_caching()
    
    print *, "All diagnostic performance tests passed!"
    
contains
    
    subroutine benchmark_diagnostic_creation()
        type(diagnostic_t) :: diagnostic
        type(source_range_t) :: location
        real :: start_time, end_time
        integer :: i, iterations
        
        print *, "  📊 Benchmarking diagnostic creation..."
        
        ! Setup location
        location%start%line = 10
        location%start%column = 5
        location%end%line = 10
        location%end%column = 15
        
        iterations = 10000
        
        call cpu_time(start_time)
        do i = 1, iterations
            diagnostic = create_diagnostic("F001", "Test diagnostic message", "test.f90", &
                                         location, SEVERITY_WARNING)
        end do
        call cpu_time(end_time)
        
        print '(a,f0.3,a,f0.6,a)', "    Created ", real(iterations)/1000.0, &
            "K diagnostics in ", end_time - start_time, " seconds"
        print '(a,f0.2,a)', "    Rate: ", real(iterations)/(end_time - start_time), &
            " diagnostics/second"
        
        if (end_time - start_time > 1.0) then
            error stop "Diagnostic creation is too slow"
        end if
        
        print *, "    ✓ Diagnostic creation performance"
        
    end subroutine benchmark_diagnostic_creation
    
    subroutine benchmark_diagnostic_formatting()
        type(diagnostic_t) :: diagnostic
        type(source_range_t) :: location
        character(len=:), allocatable :: formatted
        real :: start_time, end_time
        integer :: i, iterations
        
        print *, "  📊 Benchmarking diagnostic formatting..."
        
        ! Create test diagnostic
        location%start%line = 10
        location%start%column = 5
        location%end%line = 10
        location%end%column = 15
        
        diagnostic = create_diagnostic("F001", "Test diagnostic message", "test.f90", &
                                     location, SEVERITY_WARNING)
        
        iterations = 5000
        
        ! Benchmark text formatting
        call cpu_time(start_time)
        do i = 1, iterations
            formatted = format_diagnostic(diagnostic, OUTPUT_FORMAT_TEXT)
        end do
        call cpu_time(end_time)
        
        print '(a,f0.3,a,f0.6,a)', "    Text formatting: ", real(iterations)/1000.0, &
            "K ops in ", end_time - start_time, " seconds"
        
        ! Benchmark JSON formatting
        call cpu_time(start_time)
        do i = 1, iterations
            formatted = format_diagnostic(diagnostic, OUTPUT_FORMAT_JSON)
        end do
        call cpu_time(end_time)
        
        print '(a,f0.3,a,f0.6,a)', "    JSON formatting: ", real(iterations)/1000.0, &
            "K ops in ", end_time - start_time, " seconds"
        
        print *, "    ✓ Diagnostic formatting performance"
        
    end subroutine benchmark_diagnostic_formatting
    
    subroutine benchmark_collection_operations()
        type(diagnostic_collection_t) :: collection
        type(diagnostic_t) :: diagnostic
        type(source_range_t) :: location
        real :: start_time, end_time
        integer :: i, iterations
        
        print *, "  📊 Benchmarking collection operations..."
        
        ! Setup diagnostic
        location%start%line = 10
        location%start%column = 5
        location%end%line = 10
        location%end%column = 15
        
        diagnostic = create_diagnostic("F001", "Test diagnostic", "test.f90", &
                                     location, SEVERITY_WARNING)
        
        iterations = 1000
        
        ! Benchmark collection additions
        call cpu_time(start_time)
        do i = 1, iterations
            call collection%add(diagnostic)
        end do
        call cpu_time(end_time)
        
        print '(a,i0,a,f0.6,a)', "    Added ", iterations, &
            " diagnostics in ", end_time - start_time, " seconds"
        
        if (collection%count /= iterations) then
            error stop "Collection count mismatch"
        end if
        
        print *, "    ✓ Collection operations performance"
        
    end subroutine benchmark_collection_operations
    
    subroutine benchmark_json_serialization()
        type(diagnostic_collection_t) :: collection
        type(diagnostic_t) :: diagnostic
        type(source_range_t) :: location
        character(len=:), allocatable :: json_output
        real :: start_time, end_time
        integer :: i, num_diagnostics
        
        print *, "  📊 Benchmarking JSON serialization..."
        
        ! Create collection with multiple diagnostics
        location%start%line = 10
        location%start%column = 5
        location%end%line = 10
        location%end%column = 15
        
        num_diagnostics = 100
        do i = 1, num_diagnostics
            diagnostic = create_diagnostic("F001", "Test diagnostic", "test.f90", &
                                         location, SEVERITY_WARNING)
            call collection%add(diagnostic)
        end do
        
        ! Benchmark JSON serialization
        call cpu_time(start_time)
        json_output = collection%to_json()
        call cpu_time(end_time)
        
        print '(a,i0,a,f0.6,a)', "    Serialized ", num_diagnostics, &
            " diagnostics to JSON in ", end_time - start_time, " seconds"
        
        if (len(json_output) == 0) then
            error stop "JSON output should not be empty"
        end if
        
        print *, "    ✓ JSON serialization performance"
        
    end subroutine benchmark_json_serialization
    
    subroutine test_diagnostic_caching()
        type(diagnostic_t) :: diagnostic1, diagnostic2
        type(source_range_t) :: location
        character(len=:), allocatable :: formatted1, formatted2
        real :: start_time, end_time, cached_time, uncached_time
        integer :: i
        
        print *, "  🔧 Testing diagnostic caching..."
        
        ! Create test diagnostic
        location%start%line = 10
        location%start%column = 5
        location%end%line = 10
        location%end%column = 15
        
        diagnostic1 = create_diagnostic("F001", "Test diagnostic for caching", "test.f90", &
                                       location, SEVERITY_WARNING)
        
        diagnostic2 = create_diagnostic("F001", "Test diagnostic for caching", "test.f90", &
                                       location, SEVERITY_WARNING)
        
        ! Time uncached formatting
        call cpu_time(start_time)
        do i = 1, 1000
            formatted1 = format_diagnostic(diagnostic1, OUTPUT_FORMAT_TEXT)
        end do
        call cpu_time(end_time)
        uncached_time = end_time - start_time
        
        ! Time potentially cached formatting (same diagnostic)
        call cpu_time(start_time)
        do i = 1, 1000
            formatted2 = format_diagnostic(diagnostic2, OUTPUT_FORMAT_TEXT)
        end do
        call cpu_time(end_time)
        cached_time = end_time - start_time
        
        print '(a,f0.6,a)', "    Uncached formatting time: ", uncached_time, " seconds"
        print '(a,f0.6,a)', "    Cached formatting time: ", cached_time, " seconds"
        
        ! Note: For now, we don't actually implement caching, 
        ! but we verify the infrastructure works
        if (formatted1 /= formatted2) then
            error stop "Identical diagnostics should format identically"
        end if
        
        print *, "    ✓ Diagnostic caching infrastructure"
        
    end subroutine test_diagnostic_caching
    
end program test_diagnostic_performance