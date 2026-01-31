program test_rule_c001_undefined_variable
    use fluff_diagnostics, only: diagnostic_t
    use fluff_linter, only: create_linter_engine, linter_engine_t
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked, &
                            assert_has_diagnostic_code, assert_diagnostic_location
    implicit none

    print *, "Testing C001: Undefined variable detection (correctness rule)..."

    call test_undefined_variable_basic()
    call test_defined_variable_no_violation()
    call test_multiple_undefined_variables()
    call test_diagnostic_location()
    call test_scope_visibility()
    call test_intrinsic_functions_allowed()
    call test_parameter_constants()
    call test_multi_declaration()

    print *, "[OK] All C001 tests passed!"

contains

    subroutine test_undefined_variable_basic()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        call make_temp_fortran_path("fluff_test_c001_basic", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = y + 10"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)

        call assert_has_diagnostic_code(diagnostics, "C001", .true., &
                                        "C001 should trigger for undefined variable y")

        call delete_file_if_exists(tmpfile)

        print *, "[OK] Basic undefined variable detection"
    end subroutine test_undefined_variable_basic

    subroutine test_defined_variable_no_violation()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        call make_temp_fortran_path("fluff_test_c001_ok", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x, y"//new_line('a')// &
                    "    y = 20"//new_line('a')// &
                    "    x = y + 10"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)

        call assert_has_diagnostic_code(diagnostics, "C001", .false., &
                                        "C001: no violation when all vars defined")

        call delete_file_if_exists(tmpfile)

        print *, "[OK] Defined variable no violation"
    end subroutine test_defined_variable_no_violation

    subroutine test_multiple_undefined_variables()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile
        integer :: i, c001_count

        call make_temp_fortran_path("fluff_test_c001_multi", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = y + z"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)

        c001_count = 0
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "C001") c001_count = c001_count + 1
            end do
        end if

        call delete_file_if_exists(tmpfile)

        if (c001_count < 2) then
            error stop "Failed: expected 2+ C001 diagnostics for y and z"
        end if

        print *, "[OK] Multiple undefined variables detected"
    end subroutine test_multiple_undefined_variables

    subroutine test_diagnostic_location()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        call make_temp_fortran_path("fluff_test_c001_loc", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    x = undefined_var"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)

        call assert_diagnostic_location(diagnostics, "C001", 4, 9, &
                                        "C001 should point to line 4, column 9")

        call delete_file_if_exists(tmpfile)

        print *, "[OK] Diagnostic location is correct"
    end subroutine test_diagnostic_location

    subroutine test_scope_visibility()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        call make_temp_fortran_path("fluff_test_c001_scope", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: x"//new_line('a')// &
                    "    call sub()"//new_line('a')// &
                    "    x = y"//new_line('a')// &
                    "    print *, x"//new_line('a')// &
                    "contains"//new_line('a')// &
                    "    subroutine sub()"//new_line('a')// &
                    "        integer :: y"//new_line('a')// &
                    "        y = 1"//new_line('a')// &
                    "    end subroutine sub"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)

        call assert_has_diagnostic_code(diagnostics, "C001", .true., &
                                        "C001: trigger for y outside its scope")

        call delete_file_if_exists(tmpfile)

        print *, "[OK] Scope visibility enforced"
    end subroutine test_scope_visibility

    subroutine test_intrinsic_functions_allowed()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        call make_temp_fortran_path("fluff_test_c001_intrinsic", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    real :: x, y"//new_line('a')// &
                    "    x = 2.0"//new_line('a')// &
                    "    y = sin(x) + cos(x) + sqrt(abs(x))"//new_line('a')// &
                    "    print *, y"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)

        call assert_has_diagnostic_code(diagnostics, "C001", .false., &
                                        "C001: no violation for intrinsic functions")

        call delete_file_if_exists(tmpfile)

        print *, "[OK] Intrinsic functions allowed"
    end subroutine test_intrinsic_functions_allowed

    subroutine test_parameter_constants()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        call make_temp_fortran_path("fluff_test_c001_param", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer, parameter :: n = 10"//new_line('a')// &
                    "    integer :: arr(n)"//new_line('a')// &
                    "    arr(1) = n"//new_line('a')// &
                    "    print *, arr(1)"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)

        call assert_has_diagnostic_code(diagnostics, "C001", .false., &
                                        "C001: no violation for parameter constants")

        call delete_file_if_exists(tmpfile)

        print *, "[OK] Parameter constants recognized"
    end subroutine test_parameter_constants

    subroutine test_multi_declaration()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: tmpfile

        call make_temp_fortran_path("fluff_test_c001_multi_decl", tmpfile)

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: a, b, c"//new_line('a')// &
                    "    a = 1"//new_line('a')// &
                    "    b = 2"//new_line('a')// &
                    "    c = a + b"//new_line('a')// &
                    "    print *, c"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()
        call write_text_file(tmpfile, test_code)
        call lint_file_checked(linter, tmpfile, diagnostics)

        call assert_has_diagnostic_code(diagnostics, "C001", .false., &
                                        "C001: recognize multi-declaration vars")

        call delete_file_if_exists(tmpfile)

        print *, "[OK] Multi-declaration variables recognized"
    end subroutine test_multi_declaration

end program test_rule_c001_undefined_variable
