program test_operator_spacing_preserves_operators
    !! The operator-spacing pass must never split a Fortran operator whose first
    !! character is itself an operator. It previously turned `a /= b` into
    !! `a / = b` and `x**2` into `x* * 2`, which is not valid Fortran and, if it
    !! were, would mean something else. See issue #270.
    use fluff_format_quality_improve, only: apply_aesthetic_improvements
    use fluff_format_quality_types, only: aesthetic_settings_t, &
        create_aesthetic_settings
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call require_operator_survives('a /= b', '/=', 'not-equal')
    call require_operator_survives('x = x**2', '**', 'power')
    call require_operator_survives('s = "ab"//"cd"', '//', 'concatenation')

    ! Single-character operators must still be spaced, or the pass would be
    ! doing nothing and these tests would pass vacuously.
    call require_spacing_applied('x=a+b', 'a + b', 'addition is spaced')
    call require_spacing_applied('x=a*b', 'a * b', 'multiplication is spaced')

    write (*, '(a,i0,a,i0,a)') 'operator_spacing: ', n_pass, ' pass, ', &
        n_fail, ' fail'
    if (n_fail > 0) error stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (*, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine require_operator_survives(source, op, name)
        !! The operator must appear verbatim in the output, with its two
        !! characters still adjacent.
        character(len=*), intent(in) :: source, op, name
        character(len=:), allocatable :: code

        call apply_aesthetic_improvements(source, code, &
            create_aesthetic_settings())
        call assert(index(code, op) > 0, &
            name//' operator survives spacing: got "'//trim(code)//'"')
    end subroutine require_operator_survives

    subroutine require_spacing_applied(source, expected, name)
        character(len=*), intent(in) :: source, expected, name
        character(len=:), allocatable :: code

        call apply_aesthetic_improvements(source, code, &
            create_aesthetic_settings())
        call assert(index(code, expected) > 0, &
            name//': expected "'//expected//'" in "'//trim(code)//'"')
    end subroutine require_spacing_applied

end program test_operator_spacing_preserves_operators
