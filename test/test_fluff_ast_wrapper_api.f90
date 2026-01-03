program test_fluff_ast_wrapper_api
    use fluff_ast, only: create_ast_context, fluff_ast_context_t, fluff_trivia_t, &
                         NODE_ASSIGNMENT
    use fortfront, only: CST_COMMENT, CST_NEWLINE, CST_WHITESPACE
    use fortfront, only: symbol_info_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_source_text_api()) all_passed = .false.
    if (.not. test_children_and_trivia_api()) all_passed = .false.
    if (.not. test_symbol_query_api()) all_passed = .false.

    if (all_passed) stop 0
    stop 1

contains

    logical function test_source_text_api()
        type(fluff_ast_context_t) :: ctx
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: stored
        character(len=:), allocatable :: line
        character(len=:), allocatable :: text
        logical :: found
        character(len=*), parameter :: crlf = char(13)//char(10)
        character(len=:), allocatable :: source

        test_source_text_api = .true.

        source = "program p" // crlf // &
                 "implicit none" // crlf // &
                 "integer :: x" // crlf // &
                 "x = 1" // crlf // &
                 "end program p" // crlf

        ctx = create_ast_context()
        call ctx%from_source(source, error_msg)
        if (len(error_msg) > 0) then
            print *, "FAIL: from_source unexpected error: ", trim(error_msg)
            test_source_text_api = .false.
            return
        end if

        call ctx%get_source_text(stored, found)
        if (.not. found) then
            print *, "FAIL: get_source_text found=false"
            test_source_text_api = .false.
            return
        end if

        if (index(stored, char(13)) > 0) then
            print *, "FAIL: stored source contains CR character"
            test_source_text_api = .false.
            return
        end if

        call ctx%get_source_line(1, line, found)
        if (.not. found) then
            print *, "FAIL: get_source_line(1) found=false"
            test_source_text_api = .false.
            return
        end if
        if (line /= "program p") then
            print *, "FAIL: get_source_line(1) mismatch: ", trim(line)
            test_source_text_api = .false.
            return
        end if

        call ctx%get_source_line(4, line, found)
        if (.not. found) then
            print *, "FAIL: get_source_line(4) found=false"
            test_source_text_api = .false.
            return
        end if

        if (line /= "x = 1") then
            print *, "FAIL: get_source_line(4) mismatch: ", trim(line)
            test_source_text_api = .false.
            return
        end if

        call ctx%get_source_line(6, line, found)
        if (.not. found) then
            print *, "FAIL: get_source_line(6) found=false"
            test_source_text_api = .false.
            return
        end if

        if (len(line) /= 0) then
            print *, "FAIL: expected trailing empty line; got length=", len(line)
            test_source_text_api = .false.
            return
        end if

        call ctx%get_source_range(6, 1, 6, 1, text, found)
        if (.not. found) then
            print *, "FAIL: get_source_range found=false"
            test_source_text_api = .false.
            return
        end if

        if (len(text) /= 0) then
            print *, "FAIL: expected empty range; got length=", len(text)
            test_source_text_api = .false.
            return
        end if
    end function test_source_text_api

    logical function test_children_and_trivia_api()
        type(fluff_ast_context_t) :: ctx_children
        type(fluff_ast_context_t) :: ctx_trivia
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: source_children
        character(len=:), allocatable :: source_trivia
        integer, allocatable :: children(:)
        integer :: i
        integer :: assignment_index
        type(fluff_trivia_t), allocatable :: leading(:)
        type(fluff_trivia_t), allocatable :: trailing(:)
        logical :: found

        test_children_and_trivia_api = .true.

        source_children = "program p" // new_line('A') // &
                          "implicit none" // new_line('A') // &
                          "integer :: x" // new_line('A') // &
                          "x = 1" // new_line('A') // &
                          "end program p" // new_line('A')

        ctx_children = create_ast_context()
        call ctx_children%from_source(source_children, error_msg)
        if (len(error_msg) > 0) then
            print *, "FAIL: from_source unexpected error: ", trim(error_msg)
            test_children_and_trivia_api = .false.
            return
        end if

        children = ctx_children%get_children(ctx_children%root_index)
        if (size(children) <= 0) then
            print *, "FAIL: root has no children"
            test_children_and_trivia_api = .false.
            return
        end if

        source_trivia = "! header" // new_line('A') // &
                        "   x = 1" // new_line('A')

        ctx_trivia = create_ast_context()
        call ctx_trivia%from_source(source_trivia, error_msg)
        if (len(error_msg) > 0) then
            print *, "FAIL: from_source unexpected error: ", trim(error_msg)
            test_children_and_trivia_api = .false.
            return
        end if

        assignment_index = 0
        do i = 1, ctx_trivia%arena%size
            if (ctx_trivia%get_node_type(i) == NODE_ASSIGNMENT) then
                assignment_index = i
                exit
            end if
        end do

        if (assignment_index == 0) then
            print *, "FAIL: did not find assignment node"
            test_children_and_trivia_api = .false.
            return
        end if

        call ctx_trivia%get_trivia_for_node(assignment_index, leading, trailing, found)
        if (.not. found) then
            print *, "FAIL: get_trivia_for_node found=false"
            test_children_and_trivia_api = .false.
            return
        end if

        if (size(leading) /= 3) then
            print *, "FAIL: expected 3 leading trivia tokens; got=", size(leading)
            test_children_and_trivia_api = .false.
            return
        end if

        if (leading(1)%kind /= CST_COMMENT) then
            print *, "FAIL: expected first trivia CST_COMMENT; got kind=", &
                leading(1)%kind
            test_children_and_trivia_api = .false.
            return
        end if

        if (.not. allocated(leading(1)%text)) then
            test_children_and_trivia_api = .false.
            return
        end if

        if (trim(leading(1)%text) /= "! header") then
            print *, "FAIL: unexpected comment text: ", trim(leading(1)%text)
            test_children_and_trivia_api = .false.
            return
        end if

        if (leading(2)%kind /= CST_NEWLINE) then
            print *, "FAIL: expected second trivia CST_NEWLINE; got kind=", &
                leading(2)%kind
            test_children_and_trivia_api = .false.
            return
        end if

        if (leading(3)%kind /= CST_WHITESPACE) then
            print *, "FAIL: expected third trivia CST_WHITESPACE; got kind=", &
                leading(3)%kind
            test_children_and_trivia_api = .false.
            return
        end if

        if (leading(3)%text /= "   ") then
            print *, "FAIL: unexpected indentation trivia: ", &
                trim(leading(3)%text)
            test_children_and_trivia_api = .false.
            return
        end if

        if (size(trailing) <= 0) then
            print *, "FAIL: expected trailing trivia tokens"
            test_children_and_trivia_api = .false.
            return
        end if
    end function test_children_and_trivia_api

    logical function test_symbol_query_api()
        type(fluff_ast_context_t) :: ctx
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: source
        type(symbol_info_t), allocatable :: symbols(:)
        type(symbol_info_t) :: x_info
        logical :: defined

        test_symbol_query_api = .true.

        source = "program p" // new_line('A') // &
                 "implicit none" // new_line('A') // &
                 "integer :: x" // new_line('A') // &
                 "x = 1" // new_line('A') // &
                 "end program p" // new_line('A')

        ctx = create_ast_context()
        call ctx%from_source(source, error_msg)
        if (len(error_msg) > 0) then
            print *, "FAIL: from_source unexpected error: ", trim(error_msg)
            test_symbol_query_api = .false.
            return
        end if

        defined = ctx%is_symbol_defined("x")
        if (.not. defined) then
            print *, "FAIL: expected x to be defined"
            test_symbol_query_api = .false.
            return
        end if

        defined = ctx%is_symbol_defined("does_not_exist")
        if (defined) then
            print *, "FAIL: expected does_not_exist to be undefined"
            test_symbol_query_api = .false.
            return
        end if

        x_info = ctx%lookup_symbol("x")
        if (.not. x_info%is_defined) then
            print *, "FAIL: lookup_symbol(x) returned is_defined=false"
            test_symbol_query_api = .false.
            return
        end if

        symbols = ctx%get_all_symbols()
        if (size(symbols) <= 0) then
            print *, "FAIL: expected non-empty symbol list"
            test_symbol_query_api = .false.
            return
        end if
    end function test_symbol_query_api

end program test_fluff_ast_wrapper_api
