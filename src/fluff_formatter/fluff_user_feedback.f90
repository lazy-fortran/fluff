module fluff_user_feedback
    ! User feedback integration for format quality
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fluff_format_quality, only: format_quality_t
    use fluff_rule_diagnostic_utils, only: to_lower_ascii
    implicit none
    private
    
    ! User feedback types
    type, public :: user_feedback_t
        integer :: quality_rating = 0           ! 1-10 user rating
        character(len=:), allocatable :: comments
        character(len=:), allocatable :: preferred_style
        logical :: formatting_helpful = .false.
        logical :: would_recommend = .false.
        character(len=:), allocatable :: improvement_suggestions(:)
    contains
        procedure :: is_valid => feedback_is_valid
        procedure :: print_summary => feedback_print_summary
    end type user_feedback_t
    
    ! Feedback collection system
    type, public :: feedback_collector_t
        integer :: total_feedback_count = 0
        real(dp) :: average_rating = 0.0_dp
        integer :: positive_feedback = 0
        integer :: negative_feedback = 0
        character(len=:), allocatable :: common_issues(:)
        logical :: is_initialized = .false.
    contains
        procedure :: initialize => collector_initialize
        procedure :: collect_feedback => collector_collect_feedback
        procedure :: analyze_trends => collector_analyze_trends
        procedure :: generate_insights => collector_generate_insights
        procedure :: print_report => collector_print_report
    end type feedback_collector_t
    
    ! Public interface
    public :: create_user_feedback, create_feedback_collector
    public :: collect_interactive_feedback, suggest_improvements_from_feedback
    
contains
    
    ! Create default user feedback
    function create_user_feedback() result(feedback)
        type(user_feedback_t) :: feedback
        feedback%comments = ""
        feedback%preferred_style = "clean"
        allocate(character(len=100) :: feedback%improvement_suggestions(0))
    end function create_user_feedback
    
    ! Create feedback collector
    function create_feedback_collector() result(collector)
        type(feedback_collector_t) :: collector
        call collector%initialize()
    end function create_feedback_collector
    
    ! Collect interactive feedback from user
    subroutine collect_interactive_feedback(original_code, formatted_code, quality, feedback)
        character(len=*), intent(in) :: original_code, formatted_code
        type(format_quality_t), intent(in) :: quality
        type(user_feedback_t), intent(out) :: feedback
        
        character(len=10) :: rating_input, helpful_input, recommend_input
        character(len=200) :: comment_input, style_input
        integer :: ios
        
        feedback = create_user_feedback()
        
        print *, ""
        print *, "=== Format Quality Feedback ==="
        print *, "Overall quality score: ", quality%overall_score, "/10"
        print *, ""
        
        ! Show brief before/after comparison
        call show_code_comparison(original_code, formatted_code)
        
        print *, ""
        print *, "Please provide your feedback:"
        
        ! Get quality rating
        do
            print *, "Rate the formatting quality (1-10): "
            read(*, '(A)', iostat=ios) rating_input
            if (ios == 0) then
                read(rating_input, *, iostat=ios) feedback%quality_rating
                if (ios == 0 .and. feedback%quality_rating >= 1 .and. feedback%quality_rating <= 10) exit
            end if
            print *, "Please enter a number between 1 and 10."
        end do
        
        ! Get helpfulness feedback
        print *, "Was the formatting helpful? (y/n): "
        read(*, '(A)') helpful_input
        feedback%formatting_helpful = is_yes_response(helpful_input)

        ! Get recommendation feedback
        print *, "Would you recommend this formatter? (y/n): "
        read(*, '(A)') recommend_input
        feedback%would_recommend = is_yes_response(recommend_input)
        
        ! Get comments
        print *, "Any additional comments (press Enter to skip): "
        read(*, '(A)') comment_input
        feedback%comments = trim(comment_input)
        
        ! Get preferred style
        print *, "Preferred style guide (clean/standard/modern/hpc): "
        read(*, '(A)') style_input
        if (len_trim(style_input) > 0) then
            feedback%preferred_style = trim(style_input)
        end if
        
        print *, ""
        print *, "Thank you for your feedback!"
        
    end subroutine collect_interactive_feedback
    
    ! Suggest improvements based on feedback
    subroutine suggest_improvements_from_feedback(feedback, quality, suggestions)
        type(user_feedback_t), intent(in) :: feedback
        type(format_quality_t), intent(in) :: quality
        character(len=:), allocatable, intent(out) :: suggestions(:)
        
        character(len=100), allocatable :: temp_suggestions(:)
        integer :: count
        
        count = 0
        allocate(temp_suggestions(10))
        
        ! Analyze feedback and quality to generate suggestions
        if (feedback%quality_rating < 7) then
            if (quality%spacing_score < 8.0_dp) then
                count = count + 1
                temp_suggestions(count) = "Focus on improving operator spacing consistency"
            end if
            
            if (quality%readability_score < 7.0_dp) then
                count = count + 1
                temp_suggestions(count) = "Add more blank lines for logical separation"
            end if
            
            if (quality%indentation_score < 8.0_dp) then
                count = count + 1
                temp_suggestions(count) = "Ensure consistent 4-space indentation"
            end if
        end if
        
        if (.not. feedback%formatting_helpful) then
            count = count + 1
            temp_suggestions(count) = "Consider adjusting aesthetic settings for your preferred style"
            
            if (feedback%preferred_style /= "clean") then
                count = count + 1
                temp_suggestions(count) = "Try the '" // trim(feedback%preferred_style) // "' style guide"
            end if
        end if
        
        if (len_trim(feedback%comments) > 0) then
            ! Parse comments for specific issues (simplified)
            if (index(feedback%comments, 'spacing') > 0) then
                count = count + 1
                temp_suggestions(count) = "Adjust spacing preferences in aesthetic settings"
            end if
            
            if (index(feedback%comments, 'indentation') > 0 .or. index(feedback%comments, 'indent') > 0) then
                count = count + 1
                temp_suggestions(count) = "Customize indentation settings (spaces vs tabs, size)"
            end if
        end if
        
        if (count == 0) then
            count = 1
            temp_suggestions(count) = "Current formatting appears to meet your needs well!"
        end if
        
        allocate(character(len=100) :: suggestions(count))
        suggestions = temp_suggestions(1:count)
        
    end subroutine suggest_improvements_from_feedback
    
    ! Show code comparison
    subroutine show_code_comparison(original, formatted)
        character(len=*), intent(in) :: original, formatted
        
        print *, "Before formatting:"
        call show_code_preview(original, 3)
        print *, ""
        print *, "After formatting:"
        call show_code_preview(formatted, 3)
        
    end subroutine show_code_comparison
    
    ! Show preview of code (first N lines)
    subroutine show_code_preview(code, max_lines)
        character(len=*), intent(in) :: code
        integer, intent(in) :: max_lines
        
        integer :: i, line_count, line_start, line_end
        
        line_count = 0
        line_start = 1
        
        do i = 1, len(code)
            if (code(i:i) == new_line('a') .or. i == len(code)) then
                line_end = i - 1
                if (i == len(code)) line_end = i
                
                line_count = line_count + 1
                print *, "  ", code(line_start:line_end)
                
                if (line_count >= max_lines) then
                    if (i < len(code)) print *, "  ..."
                    exit
                end if
                
                line_start = i + 1
            end if
        end do
        
    end subroutine show_code_preview
    
    ! User feedback methods
    function feedback_is_valid(this) result(valid)
        class(user_feedback_t), intent(in) :: this
        logical :: valid
        
        valid = (this%quality_rating >= 1 .and. this%quality_rating <= 10)
        
    end function feedback_is_valid
    
    subroutine feedback_print_summary(this)
        class(user_feedback_t), intent(in) :: this
        integer :: i
        
        print *, "=== User Feedback Summary ==="
        print *, "Quality Rating: ", this%quality_rating, "/10"
        print *, "Formatting Helpful: ", this%formatting_helpful
        print *, "Would Recommend: ", this%would_recommend
        print *, "Preferred Style: ", trim(this%preferred_style)
        
        if (len_trim(this%comments) > 0) then
            print *, "Comments: ", trim(this%comments)
        end if
        
        if (size(this%improvement_suggestions) > 0) then
            print *, "Suggestions:"
            do i = 1, size(this%improvement_suggestions)
                print *, "  - ", trim(this%improvement_suggestions(i))
            end do
        end if
        
    end subroutine feedback_print_summary
    
    ! Feedback collector methods
    subroutine collector_initialize(this)
        class(feedback_collector_t), intent(inout) :: this
        
        this%total_feedback_count = 0
        this%average_rating = 0.0_dp
        this%positive_feedback = 0
        this%negative_feedback = 0
        
        allocate(character(len=100) :: this%common_issues(0))
        this%is_initialized = .true.
        
    end subroutine collector_initialize
    
    subroutine collector_collect_feedback(this, feedback)
        class(feedback_collector_t), intent(inout) :: this
        type(user_feedback_t), intent(in) :: feedback
        
        if (.not. feedback%is_valid()) return
        
        ! Update statistics
        this%total_feedback_count = this%total_feedback_count + 1
        
        ! Update average rating
        this%average_rating = (this%average_rating * (this%total_feedback_count - 1) + &
                              feedback%quality_rating) / this%total_feedback_count
        
        ! Count positive/negative feedback
        if (feedback%quality_rating >= 7) then
            this%positive_feedback = this%positive_feedback + 1
        else
            this%negative_feedback = this%negative_feedback + 1
        end if
        
    end subroutine collector_collect_feedback
    
    subroutine collector_analyze_trends(this)
        class(feedback_collector_t), intent(in) :: this
        
        real(dp) :: positive_ratio
        
        if (this%total_feedback_count == 0) then
            print *, "No feedback data available for analysis."
            return
        end if
        
        positive_ratio = real(this%positive_feedback, dp) / real(this%total_feedback_count, dp)
        
        print *, "=== Feedback Trend Analysis ==="
        print *, "Total feedback entries: ", this%total_feedback_count
        print *, "Average rating: ", this%average_rating, "/10"
        print *, "Positive feedback ratio: ", positive_ratio * 100.0_dp, "%"
        print *, "User satisfaction level: ", get_satisfaction_level(this%average_rating)
        
    end subroutine collector_analyze_trends
    
    subroutine collector_generate_insights(this, insights)
        class(feedback_collector_t), intent(in) :: this
        character(len=:), allocatable, intent(out) :: insights(:)
        
        character(len=100), allocatable :: temp_insights(:)
        integer :: count
        real(dp) :: positive_ratio
        
        count = 0
        allocate(temp_insights(5))
        
        if (this%total_feedback_count == 0) then
            count = 1
            temp_insights(count) = "Insufficient feedback data for insights"
        else
            positive_ratio = real(this%positive_feedback, dp) / real(this%total_feedback_count, dp)
            
            if (this%average_rating >= 8.0_dp) then
                count = count + 1
                temp_insights(count) = "Users are highly satisfied with formatting quality"
            else if (this%average_rating >= 6.0_dp) then
                count = count + 1
                temp_insights(count) = "Formatting quality is good but has room for improvement"
            else
                count = count + 1
                temp_insights(count) = "Formatting quality needs significant improvement"
            end if
            
            if (positive_ratio >= 0.8_dp) then
                count = count + 1
                temp_insights(count) = "Strong user approval indicates effective formatting"
            else if (positive_ratio >= 0.6_dp) then
                count = count + 1
                temp_insights(count) = "Mixed user feedback suggests need for customization"
            else
                count = count + 1
                temp_insights(count) = "Low user satisfaction requires immediate attention"
            end if
            
            if (this%total_feedback_count >= 10) then
                count = count + 1
                temp_insights(count) = "Sufficient data available for statistical analysis"
            else
                count = count + 1
                temp_insights(count) = "More feedback needed for reliable insights"
            end if
        end if
        
        allocate(character(len=100) :: insights(count))
        insights = temp_insights(1:count)
        
    end subroutine collector_generate_insights
    
    subroutine collector_print_report(this)
        class(feedback_collector_t), intent(in) :: this
        character(len=:), allocatable :: insights(:)
        integer :: i
        
        call this%analyze_trends()
        call this%generate_insights(insights)
        
        print *, ""
        print *, "=== Key Insights ==="
        do i = 1, size(insights)
            print *, "- ", trim(insights(i))
        end do
        
    end subroutine collector_print_report
    
    ! Helper functions
    pure function is_yes_response(input) result(is_yes)
        character(len=*), intent(in) :: input
        logical :: is_yes
        character(len=10) :: normalized

        normalized = to_lower_ascii(trim(input))
        is_yes = (trim(normalized) == 'y' .or. trim(normalized) == 'yes')

    end function is_yes_response

    function get_satisfaction_level(rating) result(level)
        real(dp), intent(in) :: rating
        character(len=20) :: level
        
        if (rating >= 9.0_dp) then
            level = "Excellent"
        else if (rating >= 8.0_dp) then
            level = "Very Good"
        else if (rating >= 7.0_dp) then
            level = "Good"
        else if (rating >= 6.0_dp) then
            level = "Fair"
        else if (rating >= 5.0_dp) then
            level = "Poor"
        else
            level = "Very Poor"
        end if
        
    end function get_satisfaction_level
    
end module fluff_user_feedback
