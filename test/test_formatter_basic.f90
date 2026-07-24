program test_formatter_basic
    use fluff_formatter, only: formatter_engine_t
    use test_support, only: delete_file_if_exists, make_temp_fortran_path, &
        write_text_file
    implicit none

    type(formatter_engine_t) :: formatter
    character(len=:), allocatable :: source_code, formatted_code, error_msg
    character(len=:), allocatable :: temp_path
    character(len=:), allocatable :: long_payload

    print *, "=== Testing Basic Formatter Functionality ==="

    ! Initialize formatter
    call formatter%initialize()

    ! Test 1: Simple program formatting
    source_code = "program test"//new_line('a')// &
        "implicit none"//new_line('a')// &
        "integer :: i"//new_line('a')// &
        "i = 42"//new_line('a')// &
        "end program test"

    call formatter%format_source(source_code, formatted_code, error_msg)

    if (error_msg /= "") then
        print *, "ERROR: "//error_msg
        error stop
    end if

    print *, "Input:"
    print *, source_code
    print *, ""
    print *, "Output:"
    print *, formatted_code
    print *, ""

    ! Test 2: With variables
    source_code = "program vars"//new_line('a')// &
        "real :: x, y, z"//new_line('a')// &
        "x = 1.0"//new_line('a')// &
        "y = 2.0"//new_line('a')// &
        "z = x + y"//new_line('a')// &
        "end program vars"

    call formatter%format_source(source_code, formatted_code, error_msg)

    if (error_msg /= "") then
        print *, "ERROR: "//error_msg
        error stop
    end if

    print *, "Test 2 Output:"
    print *, formatted_code

    ! Test 3: format_file reads long lines without truncation
    call make_temp_fortran_path("fluff_format_file_long_line", temp_path)

    long_payload = repeat("a", 1200)//"TAILMARK"
    source_code = "program p"//new_line('a')// &
        "implicit none"//new_line('a')// &
        "print *, '"//long_payload//"'"//new_line('a')// &
        "end program p"//new_line('a')

    call write_text_file(temp_path, source_code)

    call formatter%format_file(temp_path, formatted_code, error_msg)
    if (error_msg /= "") then
        print *, "ERROR: format_file failed: "//error_msg
        call delete_file_if_exists(temp_path)
        error stop
    end if

    if (index(formatted_code, "TAILMARK") == 0) then
        print *, "ERROR: formatted output missing long-line marker"
        call delete_file_if_exists(temp_path)
        error stop
    end if

    call delete_file_if_exists(temp_path)

    ! Test 4: list-directed I/O keeps "*," together (no "print * ,")
    source_code = "program p"//new_line('a')// &
        "implicit none"//new_line('a')// &
        "integer :: i"//new_line('a')// &
        "i = 1"//new_line('a')// &
        "print *, i"//new_line('a')// &
        "end program p"

    call formatter%format_source(source_code, formatted_code, error_msg)

    if (error_msg /= "") then
        print *, "ERROR: "//error_msg
        error stop
    end if

    if (index(formatted_code, "print * ,") > 0) then
        print *, "Output:"
        print *, formatted_code
        error stop "Formatter split the list-directed format marker: print * ,"
    end if

    if (index(formatted_code, "print *, i") == 0) then
        print *, "Output:"
        print *, formatted_code
        error stop "Formatter did not emit 'print *, i'"
    end if

    ! Test 5: a use-rename arrow stays a single token (no "dp = > real64")
    source_code = "module m"//new_line('a')// &
        "use iso_fortran_env, only: dp => real64"//new_line('a')// &
        "implicit none"//new_line('a')// &
        "real(dp) :: x"//new_line('a')// &
        "end module m"

    call formatter%format_source(source_code, formatted_code, error_msg)

    if (error_msg /= "") then
        print *, "ERROR: "//error_msg
        error stop
    end if

    if (index(formatted_code, "= >") > 0) then
        print *, "Output:"
        print *, formatted_code
        error stop "Formatter split the rename arrow into '= >'"
    end if

    if (index(formatted_code, "=> real64") == 0) then
        print *, "Output:"
        print *, formatted_code
        error stop "Formatter did not preserve '=> real64'"
    end if

    print *, ""
    print *, "Basic formatter tests completed successfully!"

end program test_formatter_basic
