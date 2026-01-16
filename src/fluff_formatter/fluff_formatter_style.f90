module fluff_formatter_style
    use fluff_ast, only: NODE_MODULE
    use fortfront, only: lex_source, parse_tokens, create_ast_arena, token_t, &
                         ast_arena_t, use_statement_node, derived_type_node, &
                         interface_block_node, get_node_type_id_from_arena, &
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

        call scan_style_indicators(arena, has_class_types, has_modules, &
                                   has_interfaces, has_mpi, has_openmp, &
                                   has_iso_env)
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

    subroutine scan_style_indicators(arena, has_class_types, has_modules, &
                                     has_interfaces, has_mpi, has_openmp, &
                                     has_iso_env)
        type(ast_arena_t), intent(in) :: arena
        logical, intent(out) :: has_class_types
        logical, intent(out) :: has_modules
        logical, intent(out) :: has_interfaces
        logical, intent(out) :: has_mpi
        logical, intent(out) :: has_openmp
        logical, intent(out) :: has_iso_env
        integer :: i

        has_class_types = .false.
        has_modules = .false.
        has_interfaces = .false.
        has_mpi = .false.
        has_openmp = .false.
        has_iso_env = .false.

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            call update_style_flags(arena, i, has_class_types, has_modules, &
                                    has_interfaces, has_mpi, has_openmp, &
                                    has_iso_env)
        end do
    end subroutine scan_style_indicators

    subroutine update_style_flags(arena, index, has_class_types, has_modules, &
                                  has_interfaces, has_mpi, has_openmp, &
                                  has_iso_env)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical, intent(inout) :: has_class_types
        logical, intent(inout) :: has_modules
        logical, intent(inout) :: has_interfaces
        logical, intent(inout) :: has_mpi
        logical, intent(inout) :: has_openmp
        logical, intent(inout) :: has_iso_env

        if (get_node_type_id_from_arena(arena, index) == NODE_MODULE) then
            has_modules = .true.
        end if

        select type (node => arena%entries(index)%node)
        class is (derived_type_node)
            has_class_types = .true.
        class is (interface_block_node)
            has_interfaces = .true.
        class is (use_statement_node)
            call check_use_module(node%module_name, has_mpi, has_openmp, has_iso_env)
        end select
    end subroutine update_style_flags

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
