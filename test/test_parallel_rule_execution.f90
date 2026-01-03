program test_parallel_rule_execution
    ! Test parallel rule execution
    use fluff_core
    use fluff_linter
    use fluff_diagnostics
    use fluff_ast
    use fluff_config
    implicit none

    print *, "Testing parallel rule execution..."

    ! Test 1: Parallel execution performance
    call test_parallel_performance()

    ! Test 2: Thread safety
    call test_thread_safety()

    ! Test 3: Result consistency
    call test_result_consistency()

    print *, "All parallel rule execution tests passed!"

contains

    subroutine test_parallel_performance()
        type(rule_registry_t) :: registry
        type(fluff_ast_context_t) :: ast_ctx
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: error_msg
        real :: start_time, end_time
        real :: serial_time, parallel_time

        ! Discover rules
        call registry%discover_builtin_rules()

        call ast_ctx%from_source("program test"//new_line('a')// &
                                 "    integer :: x"//new_line('a')// &
                                 "    x = 1"//new_line('a')// &
                                 "end program test", error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            error stop "Failed to create AST: "//error_msg
        end if

        ! Serial execution
        call cpu_time(start_time)
        call registry%execute_rules(ast_ctx, diagnostics=diagnostics)
        call cpu_time(end_time)
        serial_time = end_time - start_time

        ! Parallel execution
        call cpu_time(start_time)
        call registry%execute_rules_parallel(ast_ctx, diagnostics=diagnostics)
        call cpu_time(end_time)
        parallel_time = end_time - start_time

        if (serial_time < 0.0 .or. parallel_time < 0.0) then
            error stop "Failed: invalid timing results"
        end if

        print *, "  ✓ Parallel execution performance"

    end subroutine test_parallel_performance

    subroutine test_thread_safety()
        ! Test that parallel execution is thread-safe
        type(rule_registry_t) :: registry
        type(fluff_ast_context_t) :: ast_ctx
        type(diagnostic_t), allocatable :: diagnostics1(:), diagnostics2(:)
        character(len=:), allocatable :: error_msg
        integer :: i, iterations

        ! Initialize
        call registry%discover_builtin_rules()
        iterations = 5

        call ast_ctx%from_source("program test"//new_line('a')// &
                                 "    integer :: x"//new_line('a')// &
                                 "    x = 1"//new_line('a')// &
                                 "end program test", error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            error stop "Failed to create AST: "//error_msg
        end if

        ! Run multiple parallel executions to test for race conditions
        do i = 1, iterations
            call registry%execute_rules_parallel(ast_ctx, diagnostics=diagnostics1)
            call registry%execute_rules_parallel(ast_ctx, diagnostics=diagnostics2)

            if (.not. diagnostics_equal(diagnostics1, diagnostics2)) then
                error stop "Failed: parallel execution results are inconsistent"
            end if
        end do

        print *, "  ✓ Thread safety"

    end subroutine test_thread_safety

    subroutine test_result_consistency()
        ! Test that parallel execution gives same results as serial
        type(rule_registry_t) :: registry
        type(fluff_ast_context_t) :: ast_ctx
        type(diagnostic_t), allocatable :: serial_diagnostics(:), &
                                           parallel_diagnostics(:)
        character(len=:), allocatable :: error_msg

        ! Initialize
        call registry%discover_builtin_rules()

        call ast_ctx%from_source("program test"//new_line('a')// &
                                 "    integer :: x"//new_line('a')// &
                                 "    x = 1"//new_line('a')// &
                                 "end program test", error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            error stop "Failed to create AST: "//error_msg
        end if

        ! Run serial execution
        call registry%execute_rules(ast_ctx, diagnostics=serial_diagnostics)

        ! Run parallel execution
        call registry%execute_rules_parallel(ast_ctx, diagnostics=parallel_diagnostics)

        if (.not. diagnostics_equal(serial_diagnostics, parallel_diagnostics)) then
            error stop "Failed: serial and parallel results differ"
        end if

        print *, "  ✓ Result consistency"

    end subroutine test_result_consistency

    logical function diagnostics_equal(a, b) result(equal)
        type(diagnostic_t), allocatable, intent(in) :: a(:)
        type(diagnostic_t), allocatable, intent(in) :: b(:)

        character(len=5), allocatable :: a_codes(:)
        character(len=5), allocatable :: b_codes(:)
        integer :: i

        equal = .false.
        if (.not. allocated(a) .and. .not. allocated(b)) then
            equal = .true.
            return
        end if
        if (.not. allocated(a) .or. .not. allocated(b)) return
        if (size(a) /= size(b)) return

        allocate (a_codes(size(a)))
        allocate (b_codes(size(b)))

        do i = 1, size(a)
            a_codes(i) = a(i)%code
            b_codes(i) = b(i)%code
        end do

        call sort_codes(a_codes)
        call sort_codes(b_codes)

        equal = all(a_codes == b_codes)
    end function diagnostics_equal

    subroutine sort_codes(codes)
        character(len=5), intent(inout) :: codes(:)
        integer :: i, j
        character(len=5) :: tmp

        do i = 1, size(codes) - 1
            do j = i + 1, size(codes)
                if (codes(j) < codes(i)) then
                    tmp = codes(i)
                    codes(i) = codes(j)
                    codes(j) = tmp
                end if
            end do
        end do
    end subroutine sort_codes

end program test_parallel_rule_execution
