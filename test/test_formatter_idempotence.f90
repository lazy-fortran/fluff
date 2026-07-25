program test_formatter_idempotence
    !! Idempotence is the property a formatter must have: run over source the
    !! formatter itself produced, it must change nothing. Without it
    !! `fluff format` cannot run in CI, cannot back a --check mode, and adds
    !! diff noise on every invocation.
    !!
    !! Issue #260 reported two instances: a leading space prepended to the first
    !! line, and `print *, i` rewritten to `print * , i`. Both are asserted by
    !! name below, but the point of this program is the general property, over a
    !! corpus:
    !!
    !!   1. formatting already-canonical source returns it byte for byte, and
    !!   2. formatting twice gives the same bytes as formatting once.
    !!
    !! Property 2 applies to every corpus entry. Property 1 applies only to the
    !! canonical corpus, because the rough corpus holds constructs the formatter
    !! still rewrites in ways nobody would call canonical (see rough_entry).
    use fluff_cli, only: write_formatted_output
    use fluff_format_quality_improve, only: apply_aesthetic_improvements
    use fluff_format_quality_types, only: create_aesthetic_settings
    use fluff_formatter, only: formatter_engine_t
    use test_support, only: make_temp_fortran_path, read_text_file, &
        delete_file_if_exists
    implicit none

    integer, parameter :: N_CANONICAL = 3
    integer, parameter :: N_ROUGH = 4

    integer :: n_fail

    n_fail = 0

    call check_canonical_corpus()
    call check_rough_corpus()
    call check_spacing_boundaries()
    call check_reported_defects()
    call check_stdout_is_verbatim()

    if (n_fail > 0) then
        write (*, '(a,i0,a)') 'FAILED: ', n_fail, ' idempotence check(s)'
        error stop 1
    end if
    write (*, '(a)') '[OK] formatter idempotence'

contains

    subroutine check_canonical_corpus()
        !! Each entry is already in the formatter's output form, so a run over it
        !! must be a no-op and a second run must agree with the first.
        integer :: i

        do i = 1, N_CANONICAL
            call assert_unchanged(canonical_entry(i), canonical_name(i))
            call assert_twice_equals_once(canonical_entry(i), canonical_name(i))
            call assert_no_stray_spacing(canonical_entry(i), canonical_name(i))
        end do
    end subroutine check_canonical_corpus

    subroutine check_rough_corpus()
        !! These the formatter does rewrite. It may not settle on the form a
        !! Fortran programmer would choose, but it must settle: the second run
        !! has to agree with the first, and neither may emit the spacing
        !! artefacts of #260.
        integer :: i

        do i = 1, N_ROUGH
            call assert_twice_equals_once(rough_entry(i), rough_name(i))
            call assert_no_stray_spacing(rough_entry(i), rough_name(i))
        end do
    end subroutine check_rough_corpus

    function canonical_name(i) result(name)
        integer, intent(in) :: i
        character(len=:), allocatable :: name

        select case (i)
        case (1)
            name = 'list-directed print in a loop'
        case (2)
            name = 'arithmetic operators'
        case default
            name = 'array constructor and subscripts'
        end select
    end function canonical_name

    function canonical_entry(i) result(source)
        integer, intent(in) :: i
        character(len=:), allocatable :: source
        character(len=1) :: nl

        nl = new_line('a')

        select case (i)
        case (1)
            ! Verbatim from issue #260.
            source = &
                'program p'//nl// &
                '    implicit none'//nl// &
                '    integer :: i'//nl// &
                '    do i = 1, 3'//nl// &
                '        print *, i'//nl// &
                '    end do'//nl// &
                'end program p'
        case (2)
            source = &
                'program arith'//nl// &
                '    implicit none'//nl// &
                '    integer :: a, b, c'//nl// &
                '    a = 2'//nl// &
                '    b = 3'//nl// &
                '    c = a * b + a / b - a**2'//nl// &
                '    print *, c'//nl// &
                'end program arith'
        case default
            source = &
                'program arrays'//nl// &
                '    implicit none'//nl// &
                '    integer :: v(3)'//nl// &
                '    integer :: k'//nl// &
                '    v = [1, 2, 3]'//nl// &
                '    k = v(1) + v(2) * v(3)'//nl// &
                '    print *, k, v'//nl// &
                'end program arrays'
        end select
    end function canonical_entry

    function rough_name(i) result(name)
        integer, intent(in) :: i
        character(len=:), allocatable :: name

        select case (i)
        case (1)
            name = 'module with an intent-bearing subroutine'
        case (2)
            name = 'assumed-length character dummy'
        case (3)
            name = 'if/else inside a loop'
        case default
            name = 'formatted write to the default unit'
        end select
    end function rough_name

    function rough_entry(i) result(source)
        !! Constructs the formatter does not leave alone. Entry 1 loses the body
        !! indentation of a module procedure and gains an `implicit none` inside
        !! it; entry 2 additionally spaces the `=` of `len=*`; entry 3 gains a
        !! blank line before `else` and `end if`. Those are separate defects from
        !! #260 and are not fixed here, so their output is not pinned; what is
        !! pinned is that the formatter reaches a fixed point on them and does
        !! not corrupt the `*` of a unit or format specifier.
        integer, intent(in) :: i
        character(len=:), allocatable :: source
        character(len=1) :: nl

        nl = new_line('a')

        select case (i)
        case (1)
            source = &
                'module m'//nl// &
                '    implicit none'//nl// &
                'contains'//nl// &
                '    subroutine scale_value(x, factor)'//nl// &
                '        integer, intent(inout) :: x'//nl// &
                '        integer, intent(in) :: factor'//nl// &
                '        x = x*factor'//nl// &
                '    end subroutine scale_value'//nl// &
                'end module m'
        case (2)
            source = &
                'module names'//nl// &
                '    implicit none'//nl// &
                'contains'//nl// &
                '    subroutine emit(text)'//nl// &
                '        character(len=*), intent(in) :: text'//nl// &
                '        print *, text'//nl// &
                '    end subroutine emit'//nl// &
                'end module names'
        case (3)
            source = &
                'program nested'//nl// &
                '    implicit none'//nl// &
                '    integer :: i, total'//nl// &
                '    total = 0'//nl// &
                '    do i = 1, 10'//nl// &
                '        if (i > 5) then'//nl// &
                '            total = total + i'//nl// &
                '        else'//nl// &
                '            total = total - i'//nl// &
                '        end if'//nl// &
                '    end do'//nl// &
                '    print *, total'//nl// &
                'end program nested'
        case default
            source = &
                'program shout'//nl// &
                '    implicit none'//nl// &
                '    write (*, ''(a)'') "hello"'//nl// &
                'end program shout'
        end select
    end function rough_entry

    subroutine check_spacing_boundaries()
        !! The operator-spacing pass now asks whether an operand stands on each
        !! side of `+ - * /`. These are the cases either side of that decision:
        !! the first three must still be spaced, the rest must be left alone.
        call assert_spacing('x=a*b', 'a * b', 'multiplication is still spaced')
        call assert_spacing('x=a+b', 'a + b', 'addition is still spaced')
        call assert_spacing('y=a*(b + c)', 'a * (b', 'operand may start with (')
        call assert_spacing('print *, i', 'print *, i', 'list-directed print')
        call assert_spacing('write (*, fmt) x', '(*, fmt)', 'unit specifier')
        call assert_spacing('call emit(a, *)', ', *)', 'alternate return label')
        call assert_spacing('x = -1', '= -1', 'unary minus is not an operand')
    end subroutine check_spacing_boundaries

    subroutine assert_spacing(source, expected, name)
        character(len=*), intent(in) :: source, expected, name
        character(len=:), allocatable :: code

        call apply_aesthetic_improvements(source, code, &
            create_aesthetic_settings())
        if (index(code, expected) == 0) then
            call fail(name//': expected "'//expected//'" in "'//show(code)// &
                '" (from "'//source//'")')
        end if
    end subroutine assert_spacing

    subroutine check_reported_defects()
        !! The two instances named in issue #260, asserted directly so a
        !! regression names itself.
        character(len=:), allocatable :: source, formatted

        source = canonical_entry(1)
        call format_checked(source, formatted)

        if (len(formatted) > 0) then
            if (formatted(1:1) == ' ') then
                call fail('first line gained a leading space: "'// &
                    first_line(formatted)//'"')
            end if
        end if

        if (index(formatted, 'print * ,') > 0) then
            call fail('a space was inserted before the comma of a '// &
                'list-directed print')
        end if
        if (index(formatted, 'print *, i') == 0) then
            call fail('the list-directed print did not survive formatting')
        end if
    end subroutine check_reported_defects

    subroutine check_stdout_is_verbatim()
        !! `fluff format` writes the formatted source to stdout. The bytes it
        !! writes must be the formatted source and nothing else: list-directed
        !! output prepends a blank to the record, which is where the leading
        !! space of issue #260 came from.
        character(len=:), allocatable :: path, written, error_msg
        character(len=:), allocatable :: payload
        integer :: unit

        payload = canonical_entry(1)

        call make_temp_fortran_path('fmt_stdout', path)
        open (newunit=unit, file=path, status='replace', action='write')
        call write_formatted_output(unit, payload)
        close (unit)

        call read_text_file(path, written, error_msg)
        call delete_file_if_exists(path)

        if (error_msg /= '') then
            call fail('could not read back the written output: '//error_msg)
            return
        end if

        if (written /= payload//new_line('a')) then
            call fail('formatted output was not written verbatim; first line '// &
                'came out as "'//first_line(written)//'"')
        end if
    end subroutine check_stdout_is_verbatim

    subroutine assert_unchanged(source, name)
        character(len=*), intent(in) :: source, name
        character(len=:), allocatable :: formatted

        call format_checked(source, formatted)
        if (formatted /= source) then
            call fail(name//': already-formatted source was rewritten'// &
                new_line('a')//'  expected: '//show(source)// &
                new_line('a')//'  actual:   '//show(formatted))
        end if
    end subroutine assert_unchanged

    subroutine assert_twice_equals_once(source, name)
        character(len=*), intent(in) :: source, name
        character(len=:), allocatable :: once, twice

        call format_checked(source, once)
        call format_checked(once, twice)
        if (twice /= once) then
            call fail(name//': second run differs from the first'// &
                new_line('a')//'  run 1: '//show(once)// &
                new_line('a')//'  run 2: '//show(twice))
        end if
    end subroutine assert_twice_equals_once

    subroutine assert_no_stray_spacing(source, name)
        !! No Fortran style writes a blank before a comma or a closing paren.
        !! Either one means the operator-spacing pass mistook punctuation, or a
        !! `*` that is a unit or format specifier, for a binary operator.
        character(len=*), intent(in) :: source, name
        character(len=:), allocatable :: formatted

        call format_checked(source, formatted)
        if (index(formatted, ' ,') > 0) then
            call fail(name//': output has a space before a comma: '// &
                show(formatted))
        end if
        if (index(formatted, ' )') > 0) then
            call fail(name//': output has a space before a closing paren: '// &
                show(formatted))
        end if
    end subroutine assert_no_stray_spacing

    subroutine format_checked(source, formatted)
        character(len=*), intent(in) :: source
        character(len=:), allocatable, intent(out) :: formatted

        type(formatter_engine_t) :: formatter
        character(len=:), allocatable :: error_msg

        call formatter%initialize()
        call formatter%format_source(source, formatted, error_msg)
        if (error_msg /= '') then
            call fail('formatting failed: '//error_msg)
            formatted = ''
        end if
    end subroutine format_checked

    function show(text) result(display)
        !! Newlines shown as \n so a failure stays on one diagnostic line.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: display
        integer :: i

        display = ''
        do i = 1, len(text)
            if (text(i:i) == new_line('a')) then
                display = display//'\n'
            else
                display = display//text(i:i)
            end if
        end do
    end function show

    function first_line(text) result(head)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: head
        integer :: nl

        nl = index(text, new_line('a'))
        if (nl == 0) then
            head = text
        else
            head = text(1:nl - 1)
        end if
    end function first_line

    subroutine fail(message)
        character(len=*), intent(in) :: message

        n_fail = n_fail + 1
        write (*, '(a)') 'FAIL: '//message
    end subroutine fail

end program test_formatter_idempotence
