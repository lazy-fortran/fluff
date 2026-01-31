module fluff_config
    ! Configuration management using Fortran namelist format
    use fluff_core
    implicit none
    private

    ! Configuration override type for CLI arguments
    type, public :: config_override_t
        logical :: has_fix = .false.
        logical :: fix = .false.
        logical :: has_show_fixes = .false.
        logical :: show_fixes = .false.
        logical :: has_output_format = .false.
        character(len=:), allocatable :: output_format
    end type config_override_t

    ! Per-file ignore patterns
    type, public :: per_file_ignore_t
        character(len=:), allocatable :: pattern
        character(len=:), allocatable :: rules(:)
    end type per_file_ignore_t

    ! Rule selection configuration
    type, public :: rule_selection_t
        character(len=:), allocatable :: select(:)
        character(len=:), allocatable :: ignore(:)
        character(len=:), allocatable :: extend_select(:)
        type(per_file_ignore_t), allocatable :: per_file_ignores(:)
    contains
        procedure :: is_rule_enabled => selection_is_rule_enabled
    end type rule_selection_t

    ! Configuration type
    type, public :: fluff_config_t
        logical :: fix = .false.
        logical :: show_fixes = .false.
        integer :: line_length = 88
        integer :: tab_width = 4
        character(len=:), allocatable :: target_version
        character(len=:), allocatable :: output_format
        type(rule_selection_t) :: rules
    contains
        procedure :: from_file => config_from_file
        procedure :: from_namelist_string => config_from_namelist_string
        procedure :: from_cli_args => config_from_cli_args
        procedure :: validate => config_validate
        procedure :: merge => config_merge
    end type fluff_config_t

    public :: create_default_config
    public :: load_config
    public :: load_config_profile
    public :: get_config_schema_doc
    public :: find_config_file

contains

    function create_default_config() result(config)
        type(fluff_config_t) :: config

        config%fix = .false.
        config%show_fixes = .false.
        config%line_length = 88
        config%tab_width = 4
        config%target_version = "2018"
        config%output_format = "text"

        allocate (character(len=4) :: config%rules%select(1))
        config%rules%select(1) = "F"

    end function create_default_config

    function load_config(config_file) result(config)
        character(len=*), intent(in), optional :: config_file
        type(fluff_config_t) :: config
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: found_file

        config = create_default_config()

        if (present(config_file)) then
            call config%from_file(config_file, error_msg)
        else
            call find_config_file(found_file)
            if (allocated(found_file)) then
                call config%from_file(found_file, error_msg)
            end if
        end if

        if (.not. config%validate(error_msg)) then
            config = create_default_config()
        end if

    end function load_config

    subroutine find_config_file(found_file)
        character(len=:), allocatable, intent(out) :: found_file
        logical :: exists

        inquire (file=".fluff.nml", exist=exists)
        if (exists) then
            found_file = ".fluff.nml"
            return
        end if

        inquire (file="fluff.nml", exist=exists)
        if (exists) then
            found_file = "fluff.nml"
            return
        end if

    end subroutine find_config_file

    subroutine config_from_file(this, filename, error_msg)
        class(fluff_config_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out), optional :: error_msg

        integer :: unit, iostat
        logical :: fix, show_fixes
        integer :: line_length, tab_width
        character(len=20) :: target_version, output_format

        namelist /fluff/ fix, show_fixes, line_length, tab_width, &
            target_version, output_format

        if (present(error_msg)) error_msg = ""

        fix = this%fix
        show_fixes = this%show_fixes
        line_length = this%line_length
        tab_width = this%tab_width
        target_version = ""
        output_format = ""
        if (allocated(this%target_version)) target_version = this%target_version
        if (allocated(this%output_format)) output_format = this%output_format

        open (newunit=unit, file=filename, status='old', action='read', &
              iostat=iostat)
        if (iostat /= 0) then
            if (present(error_msg)) error_msg = "Cannot open config file: "//filename
            return
        end if

        read (unit, nml=fluff, iostat=iostat)
        close (unit)

        if (iostat > 0) then
            if (present(error_msg)) error_msg = "Invalid namelist format in: "//filename
            return
        end if

        this%fix = fix
        this%show_fixes = show_fixes
        this%line_length = line_length
        this%tab_width = tab_width
        if (len_trim(target_version) > 0) this%target_version = trim(target_version)
        if (len_trim(output_format) > 0) this%output_format = trim(output_format)

    end subroutine config_from_file

    subroutine config_from_namelist_string(this, config_str, error_msg)
        class(fluff_config_t), intent(inout) :: this
        character(len=*), intent(in) :: config_str
        character(len=:), allocatable, intent(out) :: error_msg

        integer :: unit, iostat
        logical :: fix, show_fixes
        integer :: line_length, tab_width
        character(len=20) :: target_version, output_format

        namelist /fluff/ fix, show_fixes, line_length, tab_width, &
            target_version, output_format

        error_msg = ""

        fix = this%fix
        show_fixes = this%show_fixes
        line_length = this%line_length
        tab_width = this%tab_width
        target_version = ""
        output_format = ""
        if (allocated(this%target_version)) target_version = this%target_version
        if (allocated(this%output_format)) output_format = this%output_format

        open (newunit=unit, status='scratch', form='formatted', action='readwrite')
        write (unit, '(A)') config_str
        rewind (unit)

        read (unit, nml=fluff, iostat=iostat)
        close (unit)

        if (iostat > 0) then
            error_msg = "Invalid namelist format"
            return
        end if

        this%fix = fix
        this%show_fixes = show_fixes
        this%line_length = line_length
        this%tab_width = tab_width
        if (len_trim(target_version) > 0) this%target_version = trim(target_version)
        if (len_trim(output_format) > 0) this%output_format = trim(output_format)

    end subroutine config_from_namelist_string

    subroutine config_from_cli_args(this, cli_args)
        class(fluff_config_t), intent(inout) :: this
        class(*), intent(in) :: cli_args

        select type (args => cli_args)
        type is (config_override_t)
            if (args%has_fix) this%fix = args%fix
            if (args%has_show_fixes) this%show_fixes = args%show_fixes
            if (args%has_output_format .and. allocated(args%output_format)) then
                this%output_format = args%output_format
            end if
        class default
            this%fix = .true.
            this%output_format = "json"
        end select

    end subroutine config_from_cli_args

    function config_validate(this, error_msg) result(valid)
        class(fluff_config_t), intent(in) :: this
        character(len=:), allocatable, intent(out), optional :: error_msg
        logical :: valid
        character(len=:), allocatable :: invalid_code

        valid = .true.
        if (present(error_msg)) error_msg = ""

        if (this%line_length < 40 .or. this%line_length > 200) then
            valid = .false.
            if (present(error_msg)) then
                error_msg = "line_length must be between 40 and 200"
            end if
            return
        end if

        if (this%tab_width < 1 .or. this%tab_width > 16) then
            valid = .false.
            if (present(error_msg)) then
                error_msg = "tab_width must be between 1 and 16"
            end if
            return
        end if

        if (allocated(this%target_version)) then
            select case (this%target_version)
            case ("2008", "2018", "2023")
            case default
                valid = .false.
                if (present(error_msg)) then
                    error_msg = "invalid target_version: "//this%target_version
                end if
                return
            end select
        end if

        if (allocated(this%output_format)) then
            select case (this%output_format)
            case ("text", "json", "sarif")
            case default
                valid = .false.
                if (present(error_msg)) then
                    error_msg = "invalid output_format: "//this%output_format
                end if
                return
            end select
        end if

        if (allocated(this%rules%select)) then
            if (.not. validate_rule_codes(this%rules%select, invalid_code)) then
                valid = .false.
                if (present(error_msg)) then
                    if (allocated(invalid_code)) then
                        error_msg = "invalid rule code: "//invalid_code
                    else
                        error_msg = "invalid rule code in select"
                    end if
                end if
                return
            end if
        end if

    end function config_validate

    subroutine config_merge(this, base, override)
        class(fluff_config_t), intent(inout) :: this
        type(fluff_config_t), intent(in) :: base
        type(fluff_config_t), intent(in), optional :: override

        this%fix = base%fix
        this%show_fixes = base%show_fixes
        this%line_length = base%line_length
        this%tab_width = base%tab_width

        if (allocated(base%target_version)) this%target_version = base%target_version
        if (allocated(base%output_format)) this%output_format = base%output_format
        if (allocated(base%rules%select)) this%rules%select = base%rules%select
        if (allocated(base%rules%ignore)) this%rules%ignore = base%rules%ignore

        if (present(override)) then
            if (override%fix) this%fix = override%fix
            if (override%show_fixes) this%show_fixes = override%show_fixes
            if (override%line_length /= 88) this%line_length = override%line_length
            if (override%tab_width /= 4) this%tab_width = override%tab_width
            if (allocated(override%target_version)) then
                this%target_version = override%target_version
            end if
            if (allocated(override%output_format)) then
                this%output_format = override%output_format
            end if
            if (allocated(override%rules%select)) then
                this%rules%select = override%rules%select
            end if
            if (allocated(override%rules%ignore)) then
                this%rules%ignore = override%rules%ignore
            end if
        end if

    end subroutine config_merge

    function selection_is_rule_enabled(this, rule_code) result(enabled)
        class(rule_selection_t), intent(in) :: this
        character(len=*), intent(in) :: rule_code
        logical :: enabled

        integer :: i

        enabled = .false.

        if (allocated(this%select)) then
            do i = 1, size(this%select)
                if (rule_matches_pattern(rule_code, this%select(i))) then
                    enabled = .true.
                    exit
                end if
            end do
        end if

        if (enabled .and. allocated(this%ignore)) then
            do i = 1, size(this%ignore)
                if (rule_matches_pattern(rule_code, this%ignore(i))) then
                    enabled = .false.
                    exit
                end if
            end do
        end if

    end function selection_is_rule_enabled

    function rule_matches_pattern(rule_code, pattern) result(matches)
        character(len=*), intent(in) :: rule_code
        character(len=*), intent(in) :: pattern
        logical :: matches

        if (len(pattern) <= len(rule_code)) then
            matches = rule_code(1:len(pattern)) == pattern
        else
            matches = .false.
        end if

    end function rule_matches_pattern

    function validate_rule_codes(codes, invalid_code) result(valid)
        character(len=*), intent(in) :: codes(:)
        character(len=:), allocatable, intent(out), optional :: invalid_code
        logical :: valid

        integer :: i

        valid = .true.
        if (present(invalid_code)) invalid_code = ""

        do i = 1, size(codes)
            select case (codes(i) (1:1))
            case ("F", "W", "C", "P", "S")
            case default
                if (codes(i) /= "ALL") then
                    valid = .false.
                    if (present(invalid_code)) invalid_code = codes(i)
                    return
                end if
            end select
        end do

    end function validate_rule_codes

    function load_config_profile(profile_name) result(config)
        character(len=*), intent(in) :: profile_name
        type(fluff_config_t) :: config

        config = create_default_config()

        select case (profile_name)
        case ("strict")
            config%line_length = 80
            config%target_version = "2023"
            if (allocated(config%rules%select)) deallocate (config%rules%select)
            allocate (character(len=3) :: config%rules%select(1))
            config%rules%select(1) = "ALL"

        case ("performance")
            if (allocated(config%rules%select)) deallocate (config%rules%select)
            allocate (character(len=1) :: config%rules%select(2))
            config%rules%select(1) = "P"
            config%rules%select(2) = "C"

        case ("legacy")
            config%line_length = 132
            config%target_version = "2008"
            if (allocated(config%rules%ignore)) deallocate (config%rules%ignore)
            allocate (character(len=1) :: config%rules%ignore(1))
            config%rules%ignore(1) = "F"

        end select

    end function load_config_profile

    function get_config_schema_doc() result(doc)
        character(len=:), allocatable :: doc

        doc = "# fluff Configuration (Fortran Namelist Format)"//new_line('a')// &
              new_line('a')// &
              "Configuration file: `.fluff.nml` or `fluff.nml`"//new_line('a')// &
              new_line('a')// &
              "```fortran"//new_line('a')// &
              "&fluff"//new_line('a')// &
              "  fix = .false."//new_line('a')// &
              "  show_fixes = .false."//new_line('a')// &
              "  line_length = 88"//new_line('a')// &
              "  tab_width = 4"//new_line('a')// &
              '  target_version = "2018"'//new_line('a')// &
              '  output_format = "text"'//new_line('a')// &
              "/"//new_line('a')// &
              "```"//new_line('a')// &
              new_line('a')// &
              "## Options"//new_line('a')// &
              "- fix: Auto-fix violations (.true./.false.)"//new_line('a')// &
              "- show_fixes: Show suggested fixes"//new_line('a')// &
              "- line_length: Max line length (40-200)"//new_line('a')// &
              "- tab_width: Tab width for columns (1-16)"//new_line('a')// &
              '- target_version: "2008", "2018", "2023"'//new_line('a')// &
              '- output_format: "text", "json", "sarif"'//new_line('a')

    end function get_config_schema_doc

end module fluff_config
