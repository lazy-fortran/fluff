module fluff_format_quality_types
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    private

    type, public :: format_quality_t
        real(dp) :: indentation_score = 0.0_dp
        real(dp) :: spacing_score = 0.0_dp
        real(dp) :: readability_score = 0.0_dp
        real(dp) :: structure_score = 0.0_dp
        real(dp) :: consistency_score = 0.0_dp
        real(dp) :: line_length_score = 0.0_dp
        real(dp) :: overall_score = 0.0_dp
        integer :: total_lines = 0
        integer :: blank_lines = 0
        integer :: long_lines = 0
        character(len=:), allocatable :: recommendations(:)
    contains
        procedure :: calculate_overall_score
        procedure :: generate_recommendations
        procedure :: print_report
    end type format_quality_t

    type, public :: aesthetic_settings_t
        logical :: add_blank_lines = .true.
        logical :: align_declarations = .true.
        logical :: align_assignments = .true.
        logical :: group_related_statements = .true.
        logical :: improve_operator_spacing = .true.
        logical :: optimize_line_breaks = .true.
        logical :: combine_short_lines = .false.
        logical :: preserve_leading_ampersand = .true.
        integer :: max_line_length = 88
        integer :: indent_size = 4
        real(dp) :: blank_line_ratio = 0.15_dp
    end type aesthetic_settings_t

    public :: create_quality_metrics
    public :: create_aesthetic_settings

contains

    function create_quality_metrics() result(quality)
        type(format_quality_t) :: quality
    end function create_quality_metrics

    function create_aesthetic_settings() result(settings)
        type(aesthetic_settings_t) :: settings

        settings%add_blank_lines = .true.
        settings%align_declarations = .true.
        settings%align_assignments = .true.
        settings%group_related_statements = .true.
        settings%improve_operator_spacing = .true.
        settings%optimize_line_breaks = .true.
        settings%combine_short_lines = .false.
        settings%preserve_leading_ampersand = .true.
        settings%max_line_length = 88
        settings%indent_size = 4
        settings%blank_line_ratio = 0.15_dp

    end function create_aesthetic_settings

    subroutine calculate_overall_score(this)
        class(format_quality_t), intent(inout) :: this

        this%overall_score = (this%indentation_score*0.25_dp + &
                              this%spacing_score*0.20_dp + &
                              this%readability_score*0.25_dp + &
                              this%structure_score*0.15_dp + &
                              this%consistency_score*0.10_dp + &
                              this%line_length_score*0.05_dp)

    end subroutine calculate_overall_score

    subroutine generate_recommendations(this)
        class(format_quality_t), intent(inout) :: this

        character(len=50), allocatable :: temp_recommendations(:)
        integer :: count

        count = 0
        allocate (temp_recommendations(10))

        if (this%indentation_score < 8.0_dp) then
            count = count + 1
            temp_recommendations(count) = &
                "Improve indentation consistency (use 4 spaces)"
        end if

        if (this%spacing_score < 8.0_dp) then
            count = count + 1
            temp_recommendations(count) = "Add spaces around operators (=, +, -, *, /)"
        end if

        if (this%readability_score < 7.0_dp) then
            count = count + 1
            temp_recommendations(count) = "Add blank lines to separate logical sections"
        end if

        if (this%line_length_score < 8.0_dp) then
            count = count + 1
            temp_recommendations(count) = "Break long lines (keep under 88 characters)"
        end if

        if (this%structure_score < 8.0_dp) then
            count = count + 1
            temp_recommendations(count) = &
                "Add implicit none and proper end statements"
        end if

        if (count == 0) then
            count = 1
            temp_recommendations(count) = "Code quality is excellent!"
        end if

        if (allocated(this%recommendations)) then
            deallocate (this%recommendations)
        end if

        allocate (character(len=50) :: this%recommendations(count))
        this%recommendations = temp_recommendations(1:count)

    end subroutine generate_recommendations

    subroutine print_report(this)
        class(format_quality_t), intent(in) :: this
        integer :: index

        print *, "=== Format Quality Report ==="
        print *, "Overall Score:    ", this%overall_score, "/10"
        print *, ""
        print *, "Detailed Scores:"
        print *, "  Indentation:   ", this%indentation_score, "/10"
        print *, "  Spacing:       ", this%spacing_score, "/10"
        print *, "  Readability:   ", this%readability_score, "/10"
        print *, "  Structure:     ", this%structure_score, "/10"
        print *, "  Consistency:   ", this%consistency_score, "/10"
        print *, "  Line Length:   ", this%line_length_score, "/10"
        print *, ""
        print *, "Metrics:"
        print *, "  Total Lines:   ", this%total_lines
        print *, "  Blank Lines:   ", this%blank_lines
        print *, "  Long Lines:    ", this%long_lines
        print *, ""
        print *, "Recommendations:"
        do index = 1, size(this%recommendations)
            print *, "  ", index, ". ", trim(this%recommendations(index))
        end do

    end subroutine print_report

end module fluff_format_quality_types
