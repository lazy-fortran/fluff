module fluff_formatter_style
    use fluff_ast, only: NODE_MODULE, NODE_DERIVED_TYPE, NODE_INTERFACE_BLOCK, &
                         NODE_USE_STATEMENT, get_use_module_name
    use fortfront, only: lex_source, parse_tokens, create_ast_arena, token_t, &
                         ast_arena_t, get_node_type_id_from_arena, get_children, &
                         format_options_t
    implicit none
    private

    public :: detect_style_guide_from_source
    public :: configure_clean_style
    public :: configure_standard_style
    public :: configure_modern_style
    public :: configure_hpc_style
    public :: configure_custom_style

contains

    subroutine detect_style_guide_from_source(source_code, detected_style)
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: detected_style
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg
        integer :: prog_index
        logical :: has_class_types
        logical :: has_modules
        logical :: has_interfaces
        logical :: has_mpi
        logical :: has_openmp
        logical :: has_iso_env

        call lex_source(source_code, tokens, error_msg)
        if (error_msg /= "") then
            call report_fortfront_failure("lex_source", error_msg)
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            call report_fortfront_failure("parse_tokens", error_msg)
        end if

        has_class_types = .false.
        has_modules = .false.
        has_interfaces = .false.
        has_mpi = .false.
        has_openmp = .false.
        has_iso_env = .false.

        call scan_style_indicators_recursive(arena, prog_index, 0, has_class_types, &
                                             has_modules, has_interfaces, has_mpi, &
                                             has_openmp, has_iso_env)
        call select_style_guide(has_class_types, has_modules, has_interfaces, &
                                has_mpi, has_openmp, has_iso_env, detected_style)
    end subroutine detect_style_guide_from_source

    subroutine configure_clean_style(options)
        type(format_options_t), intent(inout) :: options

        options%indent_size = 4
        options%use_tabs = .false.
        options%indent_char = ' '
        options%standardize_types = .false.
        options%line_length = 88
    end subroutine configure_clean_style

    subroutine configure_standard_style(options)
        type(format_options_t), intent(inout) :: options

        options%indent_size = 4
        options%use_tabs = .false.
        options%indent_char = ' '
        options%standardize_types = .true.
        options%line_length = 88
    end subroutine configure_standard_style

    subroutine configure_modern_style(options)
        type(format_options_t), intent(inout) :: options

        options%indent_size = 4
        options%use_tabs = .false.
        options%indent_char = ' '
        options%standardize_types = .false.
        options%line_length = 88
    end subroutine configure_modern_style

    subroutine configure_hpc_style(options)
        type(format_options_t), intent(inout) :: options

        options%indent_size = 2
        options%use_tabs = .false.
        options%indent_char = ' '
        options%standardize_types = .true.
        options%line_length = 88
    end subroutine configure_hpc_style

    subroutine configure_custom_style(options)
        type(format_options_t), intent(inout) :: options

        options%indent_size = 4
        options%use_tabs = .false.
        options%indent_char = ' '
        options%standardize_types = .false.
        options%line_length = 88
    end subroutine configure_custom_style

    recursive subroutine scan_style_indicators_recursive(arena, node_index, depth, &
                                                         has_class_types, &
                                                         has_modules, &
                                                         has_interfaces, &
                                                         has_mpi, has_openmp, &
                                                         has_iso_env)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(in) :: depth
        logical, intent(inout) :: has_class_types
        logical, intent(inout) :: has_modules
        logical, intent(inout) :: has_interfaces
        logical, intent(inout) :: has_mpi
        logical, intent(inout) :: has_openmp
        logical, intent(inout) :: has_iso_env

        integer, parameter :: MAX_DEPTH = 1000
        integer :: node_type
        integer, allocatable :: children(:)
        character(len=:), allocatable :: module_name
        integer :: i

        if (node_index <= 0) return
        if (depth > MAX_DEPTH) return

        node_type = get_node_type_id_from_arena(arena, node_index)

        select case (node_type)
        case (NODE_MODULE)
            has_modules = .true.
        case (NODE_DERIVED_TYPE)
            has_class_types = .true.
        case (NODE_INTERFACE_BLOCK)
            has_interfaces = .true.
        case (NODE_USE_STATEMENT)
            call get_use_module_name(arena, node_index, module_name)
            if (allocated(module_name)) then
                call check_use_module(module_name, has_mpi, has_openmp, has_iso_env)
            end if
        end select

        children = get_children(arena, node_index)
        do i = 1, size(children)
            if (children(i) > 0) then
                call scan_style_indicators_recursive(arena, children(i), depth + 1, &
                                                     has_class_types, has_modules, &
                                                     has_interfaces, has_mpi, &
                                                     has_openmp, has_iso_env)
            end if
        end do
    end subroutine scan_style_indicators_recursive

    subroutine check_use_module(module_name, has_mpi, has_openmp, has_iso_env)
        character(len=*), intent(in) :: module_name
        logical, intent(inout) :: has_mpi
        logical, intent(inout) :: has_openmp
        logical, intent(inout) :: has_iso_env

        select case (module_name)
        case ("mpi", "mpi_f08")
            has_mpi = .true.
        case ("omp_lib")
            has_openmp = .true.
        case ("iso_fortran_env")
            has_iso_env = .true.
        end select
    end subroutine check_use_module

    subroutine select_style_guide(has_class_types, has_modules, has_interfaces, &
                                  has_mpi, has_openmp, has_iso_env, detected_style)
        logical, intent(in) :: has_class_types
        logical, intent(in) :: has_modules
        logical, intent(in) :: has_interfaces
        logical, intent(in) :: has_mpi
        logical, intent(in) :: has_openmp
        logical, intent(in) :: has_iso_env
        character(len=:), allocatable, intent(out) :: detected_style

        if (has_mpi .or. has_openmp) then
            detected_style = "hpc"
        else if (has_class_types .or. has_interfaces) then
            detected_style = "modern"
        else if (has_iso_env .and. has_modules) then
            detected_style = "clean"
        else if (.not. has_modules .and. .not. has_interfaces) then
            detected_style = "standard"
        else
            detected_style = "standard"
        end if
    end subroutine select_style_guide

    subroutine report_fortfront_failure(stage, error_msg)
        character(len=*), intent(in) :: stage
        character(len=*), intent(in) :: error_msg

        print *, "ERROR: fortfront "//trim(stage)// &
            " failed in formatter style detection"
        print *, "Error: ", error_msg
        print *, "File a GitHub issue at https://github.com/fortfront/fortfront"
        error stop "AST parsing required - no fallbacks"
    end subroutine report_fortfront_failure

end module fluff_formatter_style
