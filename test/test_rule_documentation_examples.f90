program test_rule_documentation_examples
    ! Generate comprehensive documentation examples for all rules
    implicit none
    
    print *, "Generating rule documentation and examples..."
    
    ! Generate style rule examples
    call generate_style_rule_examples()
    
    ! Generate performance rule examples
    call generate_performance_rule_examples()
    
    ! Generate correctness rule examples
    call generate_correctness_rule_examples()
    
    print *, "Rule documentation examples generated!"
    
contains
    
    subroutine generate_style_rule_examples()
        print *, "  DOCS Generating style rule examples..."
        
        ! F001: Missing implicit none
        call show_rule_example("F001", "missing-implicit-none", &
                              "Missing 'implicit none' statement", &
                              generate_f001_bad_example(), &
                              generate_f001_good_example())
        
        ! F002: Inconsistent indentation
        call show_rule_example("F002", "inconsistent-indentation", &
                              "Inconsistent indentation detected", &
                              generate_f002_bad_example(), &
                              generate_f002_good_example())
        
        ! F003: Line too long
        call show_rule_example("F003", "line-too-long", &
                              "Line exceeds maximum length", &
                              generate_f003_bad_example(), &
                              generate_f003_good_example())
        
        ! F004: Trailing whitespace
        call show_rule_example("F004", "trailing-whitespace", &
                              "Trailing whitespace detected", &
                              generate_f004_bad_example(), &
                              generate_f004_good_example())
        
        ! F005: Mixed tabs and spaces
        call show_rule_example("F005", "mixed-tabs-spaces", &
                              "Mixed tabs and spaces in indentation", &
                              generate_f005_bad_example(), &
                              generate_f005_good_example())
        
        ! F006: Unused variable
        call show_rule_example("F006", "unused-variable", &
                              "Unused variable declaration", &
                              generate_f006_bad_example(), &
                              generate_f006_good_example())
        
        ! F007: Undefined variable
        call show_rule_example("F007", "undefined-variable", &
                              "Undefined variable usage", &
                              generate_f007_bad_example(), &
                              generate_f007_good_example())
        
        ! F008: Missing intent
        call show_rule_example("F008", "missing-intent", &
                              "Missing intent declarations", &
                              generate_f008_bad_example(), &
                              generate_f008_good_example())

        print *, "[OK] Style rule examples completed (F001-F008)"
        
    end subroutine generate_style_rule_examples
    
    subroutine generate_performance_rule_examples()
        print *, "  DOCS Generating performance rule examples..."
        
        ! P001: Column-major array access
        call show_rule_example("P001", "column-major-array-access", &
                              "Leftmost array index varies in an outer loop", &
                              generate_p001_bad_example(), &
                              generate_p001_good_example())
        
        ! P002: Inefficient loop ordering
        call show_rule_example("P002", "inefficient-loop-ordering", &
                              "Inefficient loop ordering", &
                              generate_p002_bad_example(), &
                              generate_p002_good_example())
        
        ! P003: Unnecessary array temporaries
        call show_rule_example("P003", "unnecessary-array-temporaries", &
                              "Unnecessary array temporaries", &
                              generate_p003_bad_example(), &
                              generate_p003_good_example())
        
        ! P004: Missing pure/elemental
        call show_rule_example("P004", "missing-pure-elemental", &
                              "Missing pure/elemental declarations", &
                              generate_p004_bad_example(), &
                              generate_p004_good_example())
        
        ! P005: Inefficient string operations
        call show_rule_example("P005", "inefficient-string-operations", &
                              "Inefficient string operations", &
                              generate_p005_bad_example(), &
                              generate_p005_good_example())
        
        ! P006: Unnecessary allocations in loops
        call show_rule_example("P006", "unnecessary-allocations-in-loops", &
                              "Unnecessary allocations in loops", &
                              generate_p006_bad_example(), &
                              generate_p006_good_example())
        
        ! P007: Mixed precision arithmetic
        call show_rule_example("P007", "mixed-precision-arithmetic", &
                              "Mixed precision arithmetic", &
                              generate_p007_bad_example(), &
                              generate_p007_good_example())
        
        print *, "[OK] Performance rule examples completed (P001-P007)"
        
    end subroutine generate_performance_rule_examples
    
    subroutine generate_correctness_rule_examples()
        print *, "  DOCS Generating correctness rule examples..."
        
        ! C001: Undefined variable
        call show_rule_example("C001", "undefined-variable", &
                              "Use of undefined variable", &
                              generate_c001_bad_example(), &
                              generate_c001_good_example())
        
        print *, "[OK] Correctness rule examples completed (C001)"
        
    end subroutine generate_correctness_rule_examples
    
    ! Show rule example with bad and good code
    subroutine show_rule_example(code, name, description, bad_example, good_example)
        character(len=*), intent(in) :: code, name, description
        character(len=*), intent(in) :: bad_example, good_example
        
        print *, ""
        print '(A,A,A)', "## ", code, ": " // name
        print '(A)', description
        print *, ""
        print *, "### [FAIL] Bad Example (triggers rule):"
        print *, "```fortran"
        print '(A)', bad_example
        print *, "```"
        print *, ""
        print *, "### [OK] Good Example (follows best practices):"
        print *, "```fortran"
        print '(A)', good_example
        print *, "```"
        print *, ""
        
    end subroutine show_rule_example
    
    ! Style rule examples
    function generate_f001_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program bad_example" // new_line('a') // &
               "    integer :: i  ! No implicit none" // new_line('a') // &
               "    i = 42" // new_line('a') // &
               "    j = i + 1  ! j is implicitly declared" // new_line('a') // &
               "    print *, j" // new_line('a') // &
               "end program bad_example"
    end function generate_f001_bad_example
    
    function generate_f001_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program good_example" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer :: i, j" // new_line('a') // &
               "    i = 42" // new_line('a') // &
               "    j = i + 1" // new_line('a') // &
               "    print *, j" // new_line('a') // &
               "end program good_example"
    end function generate_f001_good_example
    
    function generate_f002_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program bad_indentation" // new_line('a') // &
               "implicit none" // new_line('a') // &
               "  integer :: i" // new_line('a') // &
               "      do i = 1, 10" // new_line('a') // &  ! Inconsistent indentation
               "    print *, i" // new_line('a') // &
               "        end do" // new_line('a') // &
               "end program bad_indentation"
    end function generate_f002_bad_example
    
    function generate_f002_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program good_indentation" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer :: i" // new_line('a') // &
               "    do i = 1, 10" // new_line('a') // &
               "        print *, i" // new_line('a') // &
               "    end do" // new_line('a') // &
               "end program good_indentation"
    end function generate_f002_good_example
    
    function generate_f003_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program line_too_long" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real :: very_long_variable_name_that_makes_this_" // &
               "line_exceed_the_maximum_length_limit = 3.14159" // new_line('a') // &
               "end program line_too_long"
    end function generate_f003_bad_example
    
    function generate_f003_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program proper_length" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real :: pi" // new_line('a') // &
               "    pi = 3.14159" // new_line('a') // &
               "end program proper_length"
    end function generate_f003_good_example
    
    function generate_f004_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program trailing_spaces" // new_line('a') // &
               "    implicit none  " // new_line('a') // &  ! Trailing spaces
               "    integer :: i   " // new_line('a') // &  ! Trailing spaces
               "    i = 42" // new_line('a') // &
               "end program trailing_spaces"
    end function generate_f004_bad_example
    
    function generate_f004_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program no_trailing_spaces" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer :: i" // new_line('a') // &
               "    i = 42" // new_line('a') // &
               "end program no_trailing_spaces"
    end function generate_f004_good_example
    
    function generate_f005_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program mixed_indentation" // new_line('a') // &
               char(9) // "implicit none" // new_line('a') // &  ! Tab
               "    integer :: i" // new_line('a') // &          ! Spaces
               char(9) // "    do i = 1, 10" // new_line('a') // &  ! Mixed
               "        print *, i" // new_line('a') // &
               "    end do" // new_line('a') // &
               "end program mixed_indentation"
    end function generate_f005_bad_example
    
    function generate_f005_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program consistent_spaces" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer :: i" // new_line('a') // &
               "    do i = 1, 10" // new_line('a') // &
               "        print *, i" // new_line('a') // &
               "    end do" // new_line('a') // &
               "end program consistent_spaces"
    end function generate_f005_good_example
    
    function generate_f006_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program unused_variable" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer :: used_var, unused_var" // new_line('a') // &
               "    used_var = 42" // new_line('a') // &
               "    print *, used_var" // new_line('a') // &
               "end program unused_variable"
    end function generate_f006_bad_example
    
    function generate_f006_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program all_variables_used" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer :: var1, var2" // new_line('a') // &
               "    var1 = 42" // new_line('a') // &
               "    var2 = var1 * 2" // new_line('a') // &
               "    print *, var1, var2" // new_line('a') // &
               "end program all_variables_used"
    end function generate_f006_good_example
    
    function generate_f007_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program undefined_variable" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer :: defined_var" // new_line('a') // &
               "    defined_var = 42" // new_line('a') // &
               "    print *, undefined_var  ! Error: not declared" // new_line('a') // &
               "end program undefined_variable"
    end function generate_f007_bad_example
    
    function generate_f007_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program all_variables_defined" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer :: var1, var2" // new_line('a') // &
               "    var1 = 42" // new_line('a') // &
               "    var2 = var1 * 2" // new_line('a') // &
               "    print *, var1, var2" // new_line('a') // &
               "end program all_variables_defined"
    end function generate_f007_good_example
    
    function generate_f008_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "subroutine missing_intent(input, output)" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real :: input, output  ! Missing intent " // &
               "declarations" // new_line('a') // &
               "    output = input * 2.0" // new_line('a') // &
               "end subroutine missing_intent"
    end function generate_f008_bad_example
    
    function generate_f008_good_example() result(code)
        character(len=:), allocatable :: code
        code = "subroutine with_intent(input, output)" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real, intent(in) :: input" // new_line('a') // &
               "    real, intent(out) :: output" // new_line('a') // &
               "    output = input * 2.0" // new_line('a') // &
               "end subroutine with_intent"
    end function generate_f008_good_example
    
    ! Performance rule examples
    function generate_p001_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program bad_array_access" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real :: matrix(1000, 1000)" // new_line('a') // &
               "    integer :: i, j" // new_line('a') // &
               "    ! Bad: leftmost index varies in outer loop" // new_line('a') // &
               "    do i = 1, 1000" // new_line('a') // &
               "        do j = 1, 1000" // new_line('a') // &
               "            matrix(i, j) = real(i * j)" // new_line('a') // &
               "        end do" // new_line('a') // &
               "    end do" // new_line('a') // &
               "end program bad_array_access"
    end function generate_p001_bad_example
    
    function generate_p001_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program good_array_access" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real :: matrix(1000, 1000)" // new_line('a') // &
               "    integer :: i, j" // new_line('a') // &
               "    ! Good: leftmost index varies in inner loop" // new_line('a') // &
               "    do i = 1, 1000" // new_line('a') // &
               "        do j = 1, 1000" // new_line('a') // &
               "            matrix(j, i) = real(i * j)" // new_line('a') // &
               "        end do" // new_line('a') // &
               "    end do" // new_line('a') // &
               "end program good_array_access"
    end function generate_p001_good_example
    
    function generate_p002_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program bad_loop_order" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer, parameter :: n = 10, m = 10" // new_line('a') // &
               "    real :: a(n, m)" // new_line('a') // &
               "    integer :: i, j" // new_line('a') // &
               "    do i = 1, n" // new_line('a') // &
               "        do j = 1, m" // new_line('a') // &
               "            a(i, j) = real(i * j)" // new_line('a') // &
               "        end do" // new_line('a') // &
               "    end do" // new_line('a') // &
               "end program bad_loop_order"
    end function generate_p002_bad_example
    
    function generate_p002_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program good_loop_order" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer, parameter :: n = 10, m = 10" // new_line('a') // &
               "    real :: a(n, m)" // new_line('a') // &
               "    integer :: i, j" // new_line('a') // &
               "    do i = 1, n" // new_line('a') // &
               "        do j = 1, m" // new_line('a') // &
               "            a(j, i) = real(i * j)" // new_line('a') // &
               "        end do" // new_line('a') // &
               "    end do" // new_line('a') // &
               "end program good_loop_order"
    end function generate_p002_good_example
    
    function generate_p003_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program bad_temporaries" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer, parameter :: n = 10" // new_line('a') // &
               "    real :: a(n), b(n), c(n)" // new_line('a') // &
               "    a = b + c" // new_line('a') // &
               "end program bad_temporaries"
    end function generate_p003_bad_example
    
    function generate_p003_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program good_no_temporaries" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer, parameter :: n = 10" // new_line('a') // &
               "    real :: a(n), b(n), c(n)" // new_line('a') // &
               "    integer :: i" // new_line('a') // &
               "    do i = 1, n" // new_line('a') // &
               "        a(i) = b(i) + c(i)" // new_line('a') // &
               "    end do" // new_line('a') // &
               "end program good_no_temporaries"
    end function generate_p003_good_example
    
    function generate_p004_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "function compute_square(x) result(y)" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real, intent(in) :: x" // new_line('a') // &
               "    real :: y" // new_line('a') // &
               "    y = x * x  ! Could be pure/elemental" // new_line('a') // &
               "end function compute_square"
    end function generate_p004_bad_example
    
    function generate_p004_good_example() result(code)
        character(len=:), allocatable :: code
        code = "pure elemental function compute_square(x) " // &
               "result(y)" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real, intent(in) :: x" // new_line('a') // &
               "    real :: y" // new_line('a') // &
               "    y = x * x" // new_line('a') // &
               "end function compute_square"
    end function generate_p004_good_example
    
    function generate_p005_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program bad_string_build" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    character(len=256) :: s" // new_line('a') // &
               "    integer :: i" // new_line('a') // &
               "    s = ''" // new_line('a') // &
               "    do i = 1, 100" // new_line('a') // &
               "        s = trim(s)//'x'" // new_line('a') // &
               "    end do" // new_line('a') // &
               "    print *, len_trim(s)" // new_line('a') // &
               "end program bad_string_build"
    end function generate_p005_bad_example
    
    function generate_p005_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program good_string_build" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    character(len=256) :: s" // new_line('a') // &
               "    s = repeat('x', 100)" // new_line('a') // &
               "    print *, len_trim(s)" // new_line('a') // &
               "end program good_string_build"
    end function generate_p005_good_example
    
    function generate_p006_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program bad_allocations" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real, allocatable :: temp(:)" // new_line('a') // &
               "    integer :: i" // new_line('a') // &
               "    ! Bad: allocating in loop" // new_line('a') // &
               "    do i = 1, 1000" // new_line('a') // &
               "        allocate(temp(100))" // new_line('a') // &
               "        temp = real(i)" // new_line('a') // &
               "        print *, sum(temp)" // new_line('a') // &
               "        deallocate(temp)" // new_line('a') // &
               "    end do" // new_line('a') // &
               "end program bad_allocations"
    end function generate_p006_bad_example
    
    function generate_p006_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program good_allocations" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real, allocatable :: temp(:)" // new_line('a') // &
               "    integer :: i" // new_line('a') // &
               "    ! Good: allocate once outside loop" // new_line('a') // &
               "    allocate(temp(100))" // new_line('a') // &
               "    do i = 1, 1000" // new_line('a') // &
               "        temp = real(i)" // new_line('a') // &
               "        print *, sum(temp)" // new_line('a') // &
               "    end do" // new_line('a') // &
               "    deallocate(temp)" // new_line('a') // &
               "end program good_allocations"
    end function generate_p006_good_example
    
    function generate_p007_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program mixed_precision" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real :: single_val" // new_line('a') // &
               "    double precision :: double_val" // new_line('a') // &
               "    real :: result" // new_line('a') // &
               "    ! Bad: mixing precisions causes conversions" // new_line('a') // &
               "    single_val = 3.14" // new_line('a') // &
               "    double_val = 2.71828d0" // new_line('a') // &
               "    result = single_val + double_val" // new_line('a') // &
               "end program mixed_precision"
    end function generate_p007_bad_example
    
    function generate_p007_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program consistent_precision" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    real :: val1, val2, result" // new_line('a') // &
               "    ! Good: consistent precision" // new_line('a') // &
               "    val1 = 3.14" // new_line('a') // &
               "    val2 = 2.71828" // new_line('a') // &
               "    result = val1 + val2" // new_line('a') // &
               "end program consistent_precision"
    end function generate_p007_good_example
    
    ! Correctness rule examples
    function generate_c001_bad_example() result(code)
        character(len=:), allocatable :: code
        code = "program undefined_usage" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer :: defined_var" // new_line('a') // &
               "    defined_var = 42" // new_line('a') // &
               "    print *, undefined_var  ! Error: not declared" // new_line('a') // &
               "end program undefined_usage"
    end function generate_c001_bad_example
    
    function generate_c001_good_example() result(code)
        character(len=:), allocatable :: code
        code = "program proper_usage" // new_line('a') // &
               "    implicit none" // new_line('a') // &
               "    integer :: defined_var" // new_line('a') // &
               "    defined_var = 42" // new_line('a') // &
               "    print *, defined_var" // new_line('a') // &
               "end program proper_usage"
    end function generate_c001_good_example
    
end program test_rule_documentation_examples
