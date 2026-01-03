program test_rule_f012_naming_conventions
    ! Test F012: Inconsistent naming conventions rule
    use fluff_core
    use fluff_linter
    use fluff_rules
    use fluff_diagnostics
    use fluff_ast
    use test_support, only: make_temp_fortran_path, write_text_file, &
                            delete_file_if_exists, lint_file_checked
    implicit none

    print *, "Testing F012: Inconsistent naming conventions rule..."

    ! Test 1: Inconsistent variable naming (should trigger)
    call test_inconsistent_variable_naming()

    ! Test 2: Consistent snake_case (should not trigger)
    call test_consistent_snake_case()

    ! Test 3: Consistent camelCase (should not trigger)
    call test_consistent_camel_case()

    ! Test 4: Mixed naming styles (should trigger)
    call test_mixed_naming_styles()

    print *, "All F012 tests passed!"

contains

    subroutine test_inconsistent_variable_naming()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f012

        ! Test inconsistent variable naming

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: first_value"//new_line('a')// &  ! snake_case
                    "    integer :: secondValue"//new_line('a')// &  ! camelCase
                    "    integer :: ThirdValue"//new_line('a')// &   ! PascalCase
                    "    integer :: fourth_val"//new_line('a')// &   ! snake_case
                    "    "//new_line('a')// &
                    "    first_value = 10"//new_line('a')// &
                    "    secondValue = 20"//new_line('a')// &
                    "    ThirdValue = 30"//new_line('a')// &
                    "    fourth_val = 40"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f012", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F012 violation
        found_f012 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F012") then
                    found_f012 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f012) then
            error stop "Failed: F012 should be triggered for inconsistent naming"
        end if

        print *, "  Inconsistent variable naming"

    end subroutine test_inconsistent_variable_naming

    subroutine test_consistent_snake_case()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f012

        ! Test consistent snake_case

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: first_value"//new_line('a')// &
                    "    integer :: second_value"//new_line('a')// &
                    "    integer :: third_value"//new_line('a')// &
                    "    integer :: fourth_value"//new_line('a')// &
                    "    "//new_line('a')// &
                    "    first_value = 10"//new_line('a')// &
                    "    second_value = 20"//new_line('a')// &
                    "    third_value = 30"//new_line('a')// &
                    "    fourth_value = 40"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f012_snake", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F012 violation
        found_f012 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F012") then
                    found_f012 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f012) then
            error stop "Failed: F012 should not be triggered for consistent snake_case"
        end if

        print *, "  Consistent snake_case"

    end subroutine test_consistent_snake_case

    subroutine test_consistent_camel_case()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f012

        ! Test consistent camelCase

        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: firstValue"//new_line('a')// &
                    "    integer :: secondValue"//new_line('a')// &
                    "    integer :: thirdValue"//new_line('a')// &
                    "    integer :: fourthValue"//new_line('a')// &
                    "    "//new_line('a')// &
                    "    firstValue = 10"//new_line('a')// &
                    "    secondValue = 20"//new_line('a')// &
                    "    thirdValue = 30"//new_line('a')// &
                    "    fourthValue = 40"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f012_camel", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F012 violation
        found_f012 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F012") then
                    found_f012 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (found_f012) then
            error stop "Failed: F012 should not be triggered for consistent camelCase"
        end if

        print *, "  Consistent camelCase"

    end subroutine test_consistent_camel_case

    subroutine test_mixed_naming_styles()
        type(linter_engine_t) :: linter
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: test_code
        character(len=:), allocatable :: path
        integer :: i
        logical :: found_f012

        ! Test mixed naming styles
        test_code = "program test"//new_line('a')// &
                    "    implicit none"//new_line('a')// &
                    "    integer :: snake_case_var"//new_line('a')// &
                    "    integer :: camelCaseVar"//new_line('a')// &
                    "    integer :: PascalCaseVar"//new_line('a')// &
                    "    integer :: UPPERCASE_VAR"//new_line('a')// &
                    "    "//new_line('a')// &
                    "    snake_case_var = 10"//new_line('a')// &
                    "    camelCaseVar = 20"//new_line('a')// &
                    "    PascalCaseVar = 30"//new_line('a')// &
                    "    UPPERCASE_VAR = 40"//new_line('a')// &
                    "end program test"

        linter = create_linter_engine()

        ! Create temporary file
        call make_temp_fortran_path("fluff_test_f012_mixed", path)
        call write_text_file(path, test_code)

        ! Lint the file
        call lint_file_checked(linter, path, diagnostics)

        ! Check for F012 violation
        found_f012 = .false.
        if (allocated(diagnostics)) then
            do i = 1, size(diagnostics)
                if (diagnostics(i)%code == "F012") then
                    found_f012 = .true.
                    exit
                end if
            end do
        end if

        call delete_file_if_exists(path)

        if (.not. found_f012) then
            error stop "Failed: F012 should be triggered for mixed naming styles"
        end if

        print *, "  Mixed naming styles"

    end subroutine test_mixed_naming_styles

end program test_rule_f012_naming_conventions
