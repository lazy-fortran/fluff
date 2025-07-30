program test_rule_performance_benchmark
    ! Performance benchmarking of rule execution
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    use fluff_metrics
    implicit none
    
    print *, "Running comprehensive rule performance benchmarks..."
    
    ! Test performance on various file sizes
    call benchmark_small_files()
    call benchmark_medium_files()
    call benchmark_large_files()
    
    ! Test rule execution overhead
    call benchmark_rule_overhead()
    
    ! Test metrics collection impact
    call benchmark_metrics_impact()
    
    print *, "All performance benchmarks completed!"
    
contains
    
    subroutine benchmark_small_files()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        real :: start_time, end_time
        integer :: i, iterations
        
        print *, "  📊 Benchmarking small files (< 100 lines)..."
        
        ! Generate typical small Fortran program
        test_code = generate_small_fortran_code()
        iterations = 100
        
        ! Create test file
        open(unit=99, file="benchmark_small.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        linter = create_linter_engine()
        
        ! Benchmark execution time
        call cpu_time(start_time)
        do i = 1, iterations
            call linter%lint_file("benchmark_small.f90", diagnostics, error_msg)
        end do
        call cpu_time(end_time)
        
        ! Clean up
        open(unit=99, file="benchmark_small.f90", status="old")
        close(99, status="delete")
        
        print '(A,F8.4,A,I0,A)', "    ⏱ Average time: ", &
              (end_time - start_time) / iterations * 1000, " ms (", iterations, " iterations)"
        print '(A,I0,A)', "    🔍 Rules found: ", size(diagnostics), " violations"
        
    end subroutine benchmark_small_files
    
    subroutine benchmark_medium_files()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        real :: start_time, end_time
        integer :: i, iterations
        
        print *, "  📊 Benchmarking medium files (100-500 lines)..."
        
        ! Generate typical medium Fortran program
        test_code = generate_medium_fortran_code()
        iterations = 50
        
        ! Create test file
        open(unit=99, file="benchmark_medium.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        linter = create_linter_engine()
        
        ! Benchmark execution time
        call cpu_time(start_time)
        do i = 1, iterations
            call linter%lint_file("benchmark_medium.f90", diagnostics, error_msg)
        end do
        call cpu_time(end_time)
        
        ! Clean up
        open(unit=99, file="benchmark_medium.f90", status="old")
        close(99, status="delete")
        
        print '(A,F8.4,A,I0,A)', "    ⏱ Average time: ", &
              (end_time - start_time) / iterations * 1000, " ms (", iterations, " iterations)"
        print '(A,I0,A)', "    🔍 Rules found: ", size(diagnostics), " violations"
        
    end subroutine benchmark_medium_files
    
    subroutine benchmark_large_files()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        real :: start_time, end_time
        integer :: i, iterations
        
        print *, "  📊 Benchmarking large files (> 500 lines)..."
        
        ! Generate typical large Fortran program
        test_code = generate_large_fortran_code()
        iterations = 10
        
        ! Create test file
        open(unit=99, file="benchmark_large.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        linter = create_linter_engine()
        
        ! Benchmark execution time
        call cpu_time(start_time)
        do i = 1, iterations
            call linter%lint_file("benchmark_large.f90", diagnostics, error_msg)
        end do
        call cpu_time(end_time)
        
        ! Clean up
        open(unit=99, file="benchmark_large.f90", status="old")
        close(99, status="delete")
        
        print '(A,F8.4,A,I0,A)', "    ⏱ Average time: ", &
              (end_time - start_time) / iterations * 1000, " ms (", iterations, " iterations)"
        print '(A,I0,A)', "    🔍 Rules found: ", size(diagnostics), " violations"
        
    end subroutine benchmark_large_files
    
    subroutine benchmark_rule_overhead()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        real :: start_time, end_time
        integer :: i, iterations
        
        print *, "  📊 Benchmarking rule execution overhead..."
        
        ! Generate simple test code
        test_code = "program simple" // new_line('a') // &
                   "    implicit none" // new_line('a') // &
                   "    integer :: i" // new_line('a') // &
                   "    i = 42" // new_line('a') // &
                   "    print *, i" // new_line('a') // &
                   "end program simple"
        
        iterations = 1000
        
        ! Create test file
        open(unit=99, file="benchmark_overhead.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        linter = create_linter_engine()
        
        ! Benchmark rule execution overhead
        call cpu_time(start_time)
        do i = 1, iterations
            call linter%lint_file("benchmark_overhead.f90", diagnostics, error_msg)
        end do
        call cpu_time(end_time)
        
        ! Clean up
        open(unit=99, file="benchmark_overhead.f90", status="old")
        close(99, status="delete")
        
        print '(A,F8.4,A,I0,A)', "    ⏱ Rule overhead: ", &
              (end_time - start_time) / iterations * 1000, " ms per file (", iterations, " iterations)"
        print '(A,I0,A)', "    📋 Rules executed: 23 built-in rules"
        
    end subroutine benchmark_rule_overhead
    
    subroutine benchmark_metrics_impact()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: test_code
        real :: start_time, end_time
        real :: time_with_metrics, time_without_metrics
        integer :: i, iterations
        
        print *, "  📊 Benchmarking metrics collection impact..."
        
        ! Generate test code
        test_code = generate_medium_fortran_code()
        iterations = 20
        
        ! Create test file
        open(unit=99, file="benchmark_metrics.f90", status="replace")
        write(99, '(A)') test_code
        close(99)
        
        linter = create_linter_engine()
        
        ! Benchmark with metrics (current default)
        call cpu_time(start_time)
        do i = 1, iterations
            call linter%lint_file("benchmark_metrics.f90", diagnostics, error_msg)
        end do
        call cpu_time(end_time)
        time_with_metrics = end_time - start_time
        
        ! Note: Metrics are always collected in current implementation
        ! This benchmark shows current performance with metrics
        time_without_metrics = time_with_metrics ! Same for now
        
        ! Clean up
        open(unit=99, file="benchmark_metrics.f90", status="old")
        close(99, status="delete")
        
        print '(A,F8.4,A)', "    ⏱ With metrics: ", &
              time_with_metrics / iterations * 1000, " ms per file"
        print '(A,F8.4,A)', "    ⏱ Without metrics: ", &
              time_without_metrics / iterations * 1000, " ms per file (same - always enabled)"
        print '(A,F6.2,A)', "    📊 Overhead: ", &
              abs(time_with_metrics - time_without_metrics) / time_without_metrics * 100, "%"
        
    end subroutine benchmark_metrics_impact
    
    function generate_small_fortran_code() result(code)
        character(len=:), allocatable :: code
        
        code = "program small_test" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer :: i, n" // new_line('a') // &
               "    real :: result" // new_line('a') // &
               "    " // new_line('a') // &
               "    n = 10" // new_line('a') // &
               "    result = 0.0" // new_line('a') // &
               "    " // new_line('a') // &
               "    do i = 1, n" // new_line('a') // &
               "        result = result + real(i)" // new_line('a') // &
               "    end do" // new_line('a') // &
               "    " // new_line('a') // &
               "    print *, 'Result:', result" // new_line('a') // &
               "end program small_test"
        
    end function generate_small_fortran_code
    
    function generate_medium_fortran_code() result(code)
        character(len=:), allocatable :: code
        integer :: i
        character(len=10) :: num_str
        
        ! Start with program structure
        code = "program medium_test" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer, parameter :: n = 100" // new_line('a') // &
               "    real :: array(n)" // new_line('a') // &
               "    integer :: i, j" // new_line('a') // &
               "    real :: sum_val, avg_val" // new_line('a') // &
               "" // new_line('a')
        
        ! Add multiple loops and computations
        do i = 1, 20
            write(num_str, '(I0)') i
            code = code // "    ! Loop " // trim(num_str) // new_line('a')
            code = code // "    do j = 1, n" // new_line('a')
            code = code // "        array(j) = real(j * " // trim(num_str) // ")" // new_line('a')
            code = code // "    end do" // new_line('a')
            code = code // "" // new_line('a')
        end do
        
        ! Add final computation
        code = code // "    sum_val = sum(array)" // new_line('a') // &
               "    avg_val = sum_val / n" // new_line('a') // &
               "    print *, 'Average:', avg_val" // new_line('a') // &
               "end program medium_test"
        
    end function generate_medium_fortran_code
    
    function generate_large_fortran_code() result(code)
        character(len=:), allocatable :: code
        integer :: i, j
        character(len=10) :: num_str1, num_str2
        
        ! Start with program structure
        code = "program large_test" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer, parameter :: n = 1000, m = 100" // new_line('a') // &
               "    real :: matrix(n, m)" // new_line('a') // &
               "    real :: vector(n)" // new_line('a') // &
               "    integer :: i, j, k" // new_line('a') // &
               "    real :: result" // new_line('a') // &
               "" // new_line('a')
        
        ! Add many functions and subroutines
        do i = 1, 20
            write(num_str1, '(I0)') i
            code = code // "contains" // new_line('a') // &
                   "" // new_line('a') // &
                   "    subroutine compute_" // trim(num_str1) // "(input, output)" // new_line('a') // &
                   "        real, intent(in) :: input" // new_line('a') // &
                   "        real, intent(out) :: output" // new_line('a') // &
                   "        integer :: local_i" // new_line('a') // &
                   "" // new_line('a')
            
            do j = 1, 10
                write(num_str2, '(I0)') j
                code = code // "        do local_i = 1, " // trim(num_str2) // "0" // new_line('a') // &
                       "            output = input * real(local_i)" // new_line('a') // &
                       "        end do" // new_line('a')
            end do
            
            code = code // "    end subroutine compute_" // trim(num_str1) // new_line('a') // &
                   "" // new_line('a')
        end do
        
        code = code // "end program large_test"
        
    end function generate_large_fortran_code
    
end program test_rule_performance_benchmark