program fluff_main
    ! Main entry point for fluff CLI
    use fluff_cli
    implicit none

    integer :: exit_code
    type(cli_app_t) :: app

    ! Create and run CLI application
    app = create_cli_app()

    ! Parse command line arguments
    call parse_command_line(app%args)

    ! Run the application
    call app%run(exit_code)

    ! Exit with appropriate code
    call exit(exit_code)

contains

    subroutine parse_command_line(args)
        type(cli_args_t), intent(inout) :: args
        integer :: argc, i, length, longest
        character(len=:), allocatable :: argv(:)

        ! Get command line arguments
        argc = command_argument_count()
        longest = 1
        do i = 1, argc
            call get_command_argument(i, length=length)
            longest = max(longest, length)
        end do
        allocate(character(len=longest) :: argv(argc))

        do i = 1, argc
            call get_command_argument(i, argv(i))
        end do

        ! Parse arguments
        call args%parse(argc, argv(1:argc))

    end subroutine parse_command_line

end program fluff_main
