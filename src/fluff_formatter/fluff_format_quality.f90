module fluff_format_quality
    use fluff_format_quality_assess, only: assess_format_quality
    use fluff_format_quality_improve, only: apply_aesthetic_improvements, &
                                            combine_short_lines, &
                                            optimize_line_breaks
    use fluff_format_quality_types, only: aesthetic_settings_t, &
                                          create_aesthetic_settings, &
                                          create_quality_metrics, &
                                          format_quality_t
    implicit none
    private

    public :: aesthetic_settings_t
    public :: apply_aesthetic_improvements
    public :: assess_format_quality
    public :: combine_short_lines
    public :: create_aesthetic_settings
    public :: create_quality_metrics
    public :: format_quality_t
    public :: optimize_line_breaks

end module fluff_format_quality
