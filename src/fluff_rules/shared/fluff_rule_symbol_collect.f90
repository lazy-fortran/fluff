module fluff_rule_symbol_collect
    use fluff_ast, only: fluff_ast_context_t
    use fortfront, only: ast_arena_t, declaration_node, do_loop_node, &
                         function_def_node, get_children, identifier_node, &
                         parameter_declaration_node, program_node, &
                         subroutine_def_node
    implicit none
    private

    public :: collect_declared_names
    public :: collect_used_names
    public :: is_in_same_unit
    public :: is_in_subtree
    public :: name_in_list

contains

    pure logical function name_in_list(names, name) result(found)
        character(len=*), intent(in) :: names(:)
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        do i = 1, size(names)
            if (trim(names(i)) == trim(name)) then
                found = .true.
                return
            end if
        end do
    end function name_in_list

    pure logical function is_in_subtree(arena, root_index, node_index) result(in_tree)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        integer, intent(in) :: node_index
        integer :: current

        in_tree = .false.
        if (root_index <= 0) return
        if (node_index == root_index) then
            in_tree = .true.
            return
        end if

        current = node_index
        do while (current > 0 .and. current <= arena%size)
            current = arena%entries(current)%parent_index
            if (current == root_index) then
                in_tree = .true.
                return
            end if
        end do
    end function is_in_subtree

    pure integer function enclosing_unit_index(arena, node_index) result(unit_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current

        unit_index = 0
        current = node_index
        do while (current > 0 .and. current <= arena%size)
            if (allocated(arena%entries(current)%node)) then
                select type (node => arena%entries(current)%node)
                type is (program_node)
                    unit_index = current
                    return
                type is (function_def_node)
                    unit_index = current
                    return
                type is (subroutine_def_node)
                    unit_index = current
                    return
                end select
            end if
            current = arena%entries(current)%parent_index
        end do
    end function enclosing_unit_index

    pure logical function is_in_same_unit(arena, root_index, node_index) result(is_same)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        integer, intent(in) :: node_index

        is_same = enclosing_unit_index(arena, node_index) == root_index
    end function is_in_same_unit

    subroutine grow_names_arrays(names, lines, columns, is_parameter, new_size)
        character(len=:), allocatable, intent(inout) :: names(:)
        integer, allocatable, intent(inout) :: lines(:)
        integer, allocatable, intent(inout) :: columns(:)
        logical, allocatable, intent(inout) :: is_parameter(:)
        integer, intent(in) :: new_size

        character(len=:), allocatable :: tmp_names(:)
        integer, allocatable :: tmp_lines(:)
        integer, allocatable :: tmp_columns(:)
        logical, allocatable :: tmp_is_parameter(:)
        integer :: old_size
        integer :: name_len

        old_size = size(names)
        if (new_size <= old_size) return
        name_len = len(names)

        allocate (character(len=name_len) :: tmp_names(new_size))
        allocate (tmp_lines(new_size))
        allocate (tmp_columns(new_size))
        allocate (tmp_is_parameter(new_size))

        tmp_names = ""
        tmp_lines = 0
        tmp_columns = 0
        tmp_is_parameter = .false.

        tmp_names(1:old_size) = names
        tmp_lines(1:old_size) = lines
        tmp_columns(1:old_size) = columns
        tmp_is_parameter(1:old_size) = is_parameter

        call move_alloc(tmp_names, names)
        call move_alloc(tmp_lines, lines)
        call move_alloc(tmp_columns, columns)
        call move_alloc(tmp_is_parameter, is_parameter)
    end subroutine grow_names_arrays

    subroutine collect_declared_names(ctx, root_index, names, lines, columns, &
                                      is_parameter)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: root_index
        character(len=:), allocatable, intent(out) :: names(:)
        integer, allocatable, intent(out) :: lines(:)
        integer, allocatable, intent(out) :: columns(:)
        logical, allocatable, intent(out) :: is_parameter(:)

        character(len=:), allocatable :: tmp_names(:)
        integer, allocatable :: tmp_lines(:)
        integer, allocatable :: tmp_columns(:)
        logical, allocatable :: tmp_is_param(:)
        integer :: i, j, count, capacity, max_len

        capacity = 64
        allocate (character(len=256) :: names(capacity))
        allocate (lines(capacity))
        allocate (columns(capacity))
        allocate (is_parameter(capacity))
        count = 0

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            if (.not. is_in_subtree(ctx%arena, root_index, i)) cycle
            if (.not. is_in_same_unit(ctx%arena, root_index, i)) cycle

            select type (node => ctx%arena%entries(i)%node)
            type is (declaration_node)
                if (node%is_multi_declaration .and. allocated(node%var_names)) then
                    do j = 1, size(node%var_names)
                        if (len_trim(node%var_names(j)) == 0) cycle
                        if (count >= capacity) then
                            call grow_names_arrays(names, lines, columns, &
                                                   is_parameter, capacity*2)
                            capacity = size(names)
                        end if
                        count = count + 1
                        names(count) = node%var_names(j)
                        lines(count) = node%line
                        columns(count) = node%column
                        is_parameter(count) = node%is_parameter
                    end do
                else if (allocated(node%var_name)) then
                    if (len_trim(node%var_name) == 0) cycle
                    if (count >= capacity) then
                        call grow_names_arrays(names, lines, columns, is_parameter, &
                                               capacity*2)
                        capacity = size(names)
                    end if
                    count = count + 1
                    names(count) = node%var_name
                    lines(count) = node%line
                    columns(count) = node%column
                    is_parameter(count) = node%is_parameter
                end if
            type is (parameter_declaration_node)
                if (allocated(node%name)) then
                    if (len_trim(node%name) == 0) cycle
                    if (count >= capacity) then
                        call grow_names_arrays(names, lines, columns, is_parameter, &
                                               capacity*2)
                        capacity = size(names)
                    end if
                    count = count + 1
                    names(count) = node%name
                    lines(count) = node%line
                    columns(count) = node%column
                    is_parameter(count) = .true.
                end if
            end select
        end do

        if (count == 0) then
            deallocate (names, lines, columns, is_parameter)
            allocate (character(len=0) :: names(0))
            allocate (lines(0))
            allocate (columns(0))
            allocate (is_parameter(0))
            return
        end if

        max_len = maxval([(len_trim(names(i)), i=1, count)])

        allocate (character(len=max_len) :: tmp_names(count))
        allocate (tmp_lines(count))
        allocate (tmp_columns(count))
        allocate (tmp_is_param(count))

        tmp_names = names(1:count)
        tmp_lines = lines(1:count)
        tmp_columns = columns(1:count)
        tmp_is_param = is_parameter(1:count)

        call move_alloc(tmp_names, names)
        call move_alloc(tmp_lines, lines)
        call move_alloc(tmp_columns, columns)
        call move_alloc(tmp_is_param, is_parameter)
    end subroutine collect_declared_names

    subroutine collect_used_names(ctx, root_index, names)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: root_index
        character(len=:), allocatable, intent(out) :: names(:)

        character(len=256), allocatable :: tmp(:)
        character(len=256), allocatable :: grown(:)
        integer :: i, count, max_len
        character(len=:), allocatable :: var_name

        allocate (tmp(64))
        tmp = ""
        count = 0

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            if (.not. is_in_subtree(ctx%arena, root_index, i)) cycle
            if (.not. is_in_same_unit(ctx%arena, root_index, i)) cycle

            var_name = ""
            select type (node => ctx%arena%entries(i)%node)
            type is (identifier_node)
                if (allocated(node%name)) var_name = node%name
            end select

            if (len_trim(var_name) == 0) cycle
            call add_unique_name(tmp, count, var_name, grown)
        end do

        call collect_loop_var_names_recursive(ctx%arena, root_index, tmp, count, grown)

        if (count == 0) then
            allocate (character(len=0) :: names(0))
            return
        end if

        max_len = maxval([(len_trim(tmp(i)), i=1, count)])
        allocate (character(len=max_len) :: names(count))
        names = tmp(1:count)
    end subroutine collect_used_names

    subroutine add_unique_name(tmp, count, var_name, grown)
        character(len=256), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: count
        character(len=*), intent(in) :: var_name
        character(len=256), allocatable, intent(inout) :: grown(:)

        if (len_trim(var_name) == 0) return
        if (count > 0) then
            if (name_in_list(tmp(1:count), var_name)) return
        end if

        if (count >= size(tmp)) then
            allocate (grown(size(tmp)*2))
            grown = ""
            grown(1:size(tmp)) = tmp
            call move_alloc(grown, tmp)
        end if
        count = count + 1
        tmp(count) = var_name
    end subroutine add_unique_name

    recursive subroutine collect_loop_var_names_recursive(arena, node_index, tmp, count, &
                                                          grown)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=256), allocatable, intent(inout) :: tmp(:)
        integer, intent(inout) :: count
        character(len=256), allocatable, intent(inout) :: grown(:)

        integer :: i
        integer, allocatable :: children(:)

        if (node_index <= 0) return
        if (node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (do_loop_node)
            if (allocated(node%var_name)) then
                call add_unique_name(tmp, count, node%var_name, grown)
            end if
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call collect_loop_var_names_recursive(arena, node%body_indices(i), &
                                                          tmp, count, grown)
                end do
            end if
        class default
            children = get_children(arena, node_index)
            do i = 1, size(children)
                if (children(i) > 0) then
                    call collect_loop_var_names_recursive(arena, children(i), tmp, &
                                                          count, grown)
                end if
            end do
        end select
    end subroutine collect_loop_var_names_recursive

end module fluff_rule_symbol_collect
