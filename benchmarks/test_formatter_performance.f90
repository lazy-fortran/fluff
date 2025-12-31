program test_formatter_performance
    use fluff_formatter
    use fluff_core
    implicit none
    
    type(formatter_engine_t) :: formatter
    character(len=:), allocatable :: source_code, formatted_code, error_msg
    integer :: start_time, end_time, count_rate
    real :: elapsed_time
    integer :: i, num_iterations
    
    print *, "=== Formatter Performance Profiling ==="
    
    call formatter%initialize()
    
    ! Test 1: Small file performance
    call test_small_file_performance()
    
    ! Test 2: Medium file performance  
    call test_medium_file_performance()
    
    ! Test 3: Large file performance
    call test_large_file_performance()
    
    ! Test 4: Multiple file processing
    call test_multiple_files_performance()
    
    print *, "Performance profiling completed!"
    
contains
    
    subroutine test_small_file_performance()
        print *, ""
        print *, "Testing small file performance (10 lines)..."
        
        source_code = generate_small_fortran_code()
        num_iterations = 1000
        
        call system_clock(start_time, count_rate)
        do i = 1, num_iterations
            call formatter%format_source(source_code, formatted_code, error_msg)
            if (error_msg /= "") then
                print *, "Error: ", error_msg
                return
            end if
        end do
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time) / real(count_rate)
        print *, "  Iterations: ", num_iterations
        print *, "  Total time: ", elapsed_time, " seconds"
        print *, "  Avg per file: ", (elapsed_time / num_iterations) * 1000, " ms"
        print *, "  Files/second: ", num_iterations / elapsed_time
        
    end subroutine test_small_file_performance
    
    subroutine test_medium_file_performance()
        print *, ""
        print *, "Testing medium file performance (50 lines)..."
        
        source_code = generate_medium_fortran_code()
        num_iterations = 200
        
        call system_clock(start_time, count_rate)
        do i = 1, num_iterations
            call formatter%format_source(source_code, formatted_code, error_msg)
            if (error_msg /= "") then
                print *, "Error: ", error_msg
                return
            end if
        end do
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time) / real(count_rate)
        print *, "  Iterations: ", num_iterations
        print *, "  Total time: ", elapsed_time, " seconds"
        print *, "  Avg per file: ", (elapsed_time / num_iterations) * 1000, " ms"
        print *, "  Files/second: ", num_iterations / elapsed_time
        
    end subroutine test_medium_file_performance
    
    subroutine test_large_file_performance()
        print *, ""
        print *, "Testing large file performance (200 lines)..."
        
        source_code = generate_large_fortran_code()
        num_iterations = 50
        
        call system_clock(start_time, count_rate)
        do i = 1, num_iterations
            call formatter%format_source(source_code, formatted_code, error_msg)
            if (error_msg /= "") then
                print *, "Error: ", error_msg
                return
            end if
        end do
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time) / real(count_rate)
        print *, "  Iterations: ", num_iterations
        print *, "  Total time: ", elapsed_time, " seconds"
        print *, "  Avg per file: ", (elapsed_time / num_iterations) * 1000, " ms"
        print *, "  Files/second: ", num_iterations / elapsed_time
        
    end subroutine test_large_file_performance
    
    subroutine test_multiple_files_performance()
        print *, ""
        print *, "Testing multiple files processing..."
        
        num_iterations = 100
        
        call system_clock(start_time, count_rate)
        do i = 1, num_iterations
            ! Mix of different file sizes
            if (mod(i, 3) == 0) then
                source_code = generate_large_fortran_code()
            else if (mod(i, 2) == 0) then
                source_code = generate_medium_fortran_code()
            else
                source_code = generate_small_fortran_code()
            end if
            
            call formatter%format_source(source_code, formatted_code, error_msg)
            if (error_msg /= "") then
                print *, "Error: ", error_msg
                return
            end if
        end do
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time) / real(count_rate)
        print *, "  Mixed files: ", num_iterations
        print *, "  Total time: ", elapsed_time, " seconds"
        print *, "  Avg per file: ", (elapsed_time / num_iterations) * 1000, " ms"
        print *, "  Files/second: ", num_iterations / elapsed_time
        
    end subroutine test_multiple_files_performance
    
    function generate_small_fortran_code() result(code)
        character(len=:), allocatable :: code
        
        code = "program small_test" // new_line('a') // &
               "implicit none" // new_line('a') // &
               "integer :: i, j" // new_line('a') // &
               "real :: x, y" // new_line('a') // &
               "i = 1" // new_line('a') // &
               "j = 2" // new_line('a') // &
               "x = i + j" // new_line('a') // &
               "y = x * 2.0" // new_line('a') // &
               "print *, x, y" // new_line('a') // &
               "end program small_test"
               
    end function generate_small_fortran_code
    
    function generate_medium_fortran_code() result(code)
        character(len=:), allocatable :: code
        integer :: i
        character(len=10) :: line_num
        
        code = "program medium_test" // new_line('a') // &
               "implicit none" // new_line('a') // &
               "integer, parameter :: n = 20" // new_line('a') // &
               "real :: array(n)" // new_line('a') // &
               "real :: sum_result" // new_line('a') // &
               "integer :: i" // new_line('a') // &
               "" // new_line('a') // &
               "sum_result = 0.0" // new_line('a')
               
        do i = 1, 30
            write(line_num, '(I0)') i
            code = code // "array(" // trim(line_num) // ") = " // trim(line_num) // ".0" // new_line('a')
        end do
        
        code = code // "do i = 1, n" // new_line('a') // &
               "    sum_result = sum_result + array(i)" // new_line('a') // &
               "end do" // new_line('a') // &
               "print *, 'Sum:', sum_result" // new_line('a') // &
               "end program medium_test"
               
    end function generate_medium_fortran_code
    
    function generate_large_fortran_code() result(code)
        character(len=:), allocatable :: code
        integer :: i, j
        character(len=10) :: line_num, col_num
        
        code = "program large_test" // new_line('a') // &
               "implicit none" // new_line('a') // &
               "integer, parameter :: rows = 10, cols = 10" // new_line('a') // &
               "real :: matrix(rows, cols)" // new_line('a') // &
               "real :: result_matrix(rows, cols)" // new_line('a') // &
               "real :: sum_total" // new_line('a') // &
               "integer :: i, j" // new_line('a') // &
               "" // new_line('a') // &
               "sum_total = 0.0" // new_line('a')
               
        ! Generate matrix initialization
        do i = 1, 10
            write(line_num, '(I0)') i
            do j = 1, 10
                write(col_num, '(I0)') j
                code = code // "matrix(" // trim(line_num) // "," // trim(col_num) // ") = " // &
                       trim(line_num) // "." // trim(col_num) // new_line('a')
            end do
        end do
        
        ! Generate computation loops
        code = code // "do i = 1, rows" // new_line('a') // &
               "    do j = 1, cols" // new_line('a') // &
               "        result_matrix(i,j) = matrix(i,j) * 2.0" // new_line('a') // &
               "        sum_total = sum_total + result_matrix(i,j)" // new_line('a') // &
               "    end do" // new_line('a') // &
               "end do" // new_line('a') // &
               "print *, 'Total sum:', sum_total" // new_line('a') // &
               "end program large_test"
               
    end function generate_large_fortran_code
    
end program test_formatter_performance