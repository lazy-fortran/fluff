module fluff_rule_f012
    use fluff_ast, only: fluff_ast_context_t
    use fluff_core, only: source_range_t
    use fluff_diagnostics, only: diagnostic_t, create_diagnostic, SEVERITY_INFO
    use fluff_rule_diagnostic_utils, only: push_diagnostic
    use fluff_rule_file_context, only: current_filename
    use fluff_text_helpers, only: has_lowercase, has_uppercase, is_lowercase_letter, &
                                  is_uppercase_letter
    use fortfront, only: declaration_node
    implicit none
    private

    public :: check_f012_naming_conventions

contains

    subroutine check_f012_naming_conventions(ctx, node_index, violations)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(in) :: node_index
        type(diagnostic_t), allocatable, intent(out) :: violations(:)

        type(diagnostic_t), allocatable :: tmp(:)
        integer :: violation_count

        type(source_range_t) :: first_location
        logical :: has_first_location
        integer :: snake_count, camel_count, pascal_count

        allocate (tmp(0))
        violation_count = 0

        call count_declared_naming_styles(ctx, snake_count, camel_count, pascal_count, &
                                          first_location, has_first_location)

        if (has_mixed_naming_styles(snake_count, camel_count, pascal_count)) then
            if (.not. has_first_location) then
                first_location%start%line = 1
                first_location%start%column = 1
                first_location%end%line = 1
                first_location%end%column = 1
            end if
            call push_diagnostic(tmp, violation_count, create_diagnostic( &
                                 code="F012", &
                                 message="Inconsistent naming convention", &
                                 file_path=current_filename, &
                                 location=first_location, &
                                 severity=SEVERITY_INFO))
        end if

        allocate (violations(violation_count))
        if (violation_count > 0) violations = tmp(1:violation_count)
    end subroutine check_f012_naming_conventions

    subroutine count_declared_naming_styles(ctx, snake_count, camel_count, &
                                            pascal_count, first_location, &
                                            has_first_location)
        type(fluff_ast_context_t), intent(in) :: ctx
        integer, intent(out) :: snake_count
        integer, intent(out) :: camel_count
        integer, intent(out) :: pascal_count
        type(source_range_t), intent(out) :: first_location
        logical, intent(out) :: has_first_location

        integer :: i
        integer :: j
        character(len=:), allocatable :: name

        snake_count = 0
        camel_count = 0
        pascal_count = 0
        has_first_location = .false.

        do i = 1, ctx%arena%size
            if (.not. allocated(ctx%arena%entries(i)%node)) cycle
            select type (n => ctx%arena%entries(i)%node)
            type is (declaration_node)
                if (n%is_multi_declaration .and. allocated(n%var_names)) then
                    do j = 1, size(n%var_names)
                        name = trim(n%var_names(j))
                        call count_one(name, snake_count, camel_count, pascal_count)
                        if (.not. has_first_location .and. len_trim(name) > 0) then
                            first_location = ctx%get_node_location(i)
                            has_first_location = .true.
                        end if
                    end do
                else if (allocated(n%var_name)) then
                    name = trim(n%var_name)
                    call count_one(name, snake_count, camel_count, pascal_count)
                    if (.not. has_first_location .and. len_trim(name) > 0) then
                        first_location = ctx%get_node_location(i)
                        has_first_location = .true.
                    end if
                end if
            end select
        end do
    end subroutine count_declared_naming_styles

    subroutine count_one(name, snake_count, camel_count, pascal_count)
        character(len=*), intent(in) :: name
        integer, intent(inout) :: snake_count
        integer, intent(inout) :: camel_count
        integer, intent(inout) :: pascal_count

        if (len_trim(name) <= 0) return
        if (is_snake_case(name)) snake_count = snake_count + 1
        if (is_camel_case(name)) camel_count = camel_count + 1
        if (is_pascal_case(name)) pascal_count = pascal_count + 1
    end subroutine count_one

    pure logical function has_mixed_naming_styles(snake_count, camel_count, &
                                                  pascal_count) result(has_mixed)
        integer, intent(in) :: snake_count
        integer, intent(in) :: camel_count
        integer, intent(in) :: pascal_count

        has_mixed = (snake_count > 0 .and. camel_count > 0) .or. &
                    (snake_count > 0 .and. pascal_count > 0) .or. &
                    (camel_count > 0 .and. pascal_count > 0)
    end function has_mixed_naming_styles

    pure logical function is_snake_case(name) result(is_snake)
        character(len=*), intent(in) :: name

        is_snake = .false.
        if (len_trim(name) <= 0) return
        if (index(name, "_") <= 0) return
        if (has_uppercase(name)) return
        is_snake = .true.
    end function is_snake_case

    pure logical function is_camel_case(name) result(is_camel)
        character(len=*), intent(in) :: name

        is_camel = .false.
        if (len_trim(name) <= 0) return
        if (index(name, "_") > 0) return
        if (.not. is_lowercase_letter(name(1:1))) return
        if (.not. has_uppercase(name)) return
        is_camel = .true.
    end function is_camel_case

    pure logical function is_pascal_case(name) result(is_pascal)
        character(len=*), intent(in) :: name

        is_pascal = .false.
        if (len_trim(name) <= 0) return
        if (index(name, "_") > 0) return
        if (.not. is_uppercase_letter(name(1:1))) return
        if (.not. has_lowercase(name)) return
        is_pascal = .true.
    end function is_pascal_case

end module fluff_rule_f012
