program test_user_feedback_demo
    use fluff_formatter
    use fluff_format_quality
    use fluff_user_feedback
    implicit none
    
    type(formatter_engine_t) :: formatter
    type(format_quality_t) :: quality
    type(user_feedback_t) :: feedback
    type(feedback_collector_t) :: collector
    character(len=:), allocatable :: formatted_code, error_msg
    character(len=:), allocatable :: suggestions(:), insights(:)
    integer :: i
    
    print *, "=== User Feedback Integration Demo ==="
    
    call formatter%initialize()
    call collector%initialize()
    
    ! Demo 1: Simulate user feedback on formatting
    call demo_feedback_simulation()
    
    ! Demo 2: Show feedback analysis and insights
    call demo_feedback_analysis()
    
    ! Demo 3: Show improvement suggestions based on feedback
    call demo_improvement_suggestions()
    
contains
    
    subroutine demo_feedback_simulation()
        print *, ""
        print *, "=== Demo 1: Simulated User Feedback ==="
        
        ! Format some example code
        call formatter%format_with_quality( &
            "program test" // new_line('a') // &
            "integer::x,y" // new_line('a') // &
            "x=1;y=2" // new_line('a') // &
            "print*,x+y" // new_line('a') // &
            "end program", &
            formatted_code, error_msg, quality)
            
        if (error_msg /= "") then
            print *, "Format error: ", error_msg
            return
        end if
        
        print *, "Original code quality score: ", quality%overall_score, "/10"
        
        ! Simulate different types of user feedback
        call simulate_positive_feedback()
        call simulate_constructive_feedback()
        call simulate_negative_feedback()
        
    end subroutine demo_feedback_simulation
    
    subroutine simulate_positive_feedback()
        print *, ""
        print *, "=== Simulating Positive User Feedback ==="
        
        feedback = create_user_feedback()
        feedback%quality_rating = 9
        feedback%formatting_helpful = .true.
        feedback%would_recommend = .true.
        feedback%comments = "Great formatting! Very clean and readable."
        feedback%preferred_style = "clean"
        
        call collector%collect_feedback(feedback)
        call feedback%print_summary()
        
        call suggest_improvements_from_feedback(feedback, quality, suggestions)
        print *, ""
        print *, "Improvement suggestions:"
        do i = 1, size(suggestions)
            print *, "  ", i, ". ", trim(suggestions(i))
        end do
        
    end subroutine simulate_positive_feedback
    
    subroutine simulate_constructive_feedback()
        print *, ""
        print *, "=== Simulating Constructive User Feedback ==="
        
        feedback = create_user_feedback()
        feedback%quality_rating = 6
        feedback%formatting_helpful = .true.
        feedback%would_recommend = .true.
        feedback%comments = "Good but could use better spacing around operators."
        feedback%preferred_style = "modern"
        
        call collector%collect_feedback(feedback)
        call feedback%print_summary()
        
        call suggest_improvements_from_feedback(feedback, quality, suggestions)
        print *, ""
        print *, "Improvement suggestions:"
        do i = 1, size(suggestions)
            print *, "  ", i, ". ", trim(suggestions(i))
        end do
        
    end subroutine simulate_constructive_feedback
    
    subroutine simulate_negative_feedback()
        print *, ""
        print *, "=== Simulating Critical User Feedback ==="
        
        feedback = create_user_feedback()
        feedback%quality_rating = 3
        feedback%formatting_helpful = .false.
        feedback%would_recommend = .false.
        feedback%comments = "Too cramped, needs better indentation and spacing."
        feedback%preferred_style = "standard"
        
        call collector%collect_feedback(feedback)
        call feedback%print_summary()
        
        call suggest_improvements_from_feedback(feedback, quality, suggestions)
        print *, ""
        print *, "Improvement suggestions:"
        do i = 1, size(suggestions)
            print *, "  ", i, ". ", trim(suggestions(i))
        end do
        
    end subroutine simulate_negative_feedback
    
    subroutine demo_feedback_analysis()
        print *, ""
        print *, "=== Demo 2: Feedback Analysis and Trends ==="
        
        call collector%print_report()
        
    end subroutine demo_feedback_analysis
    
    subroutine demo_improvement_suggestions()
        print *, ""
        print *, "=== Demo 3: Quality Improvement Recommendations ==="
        
        ! Show how feedback insights can guide improvements
        call collector%generate_insights(insights)
        
        print *, "Based on collected feedback, here are key insights:"
        do i = 1, size(insights)
            print *, "  ", i, ". ", trim(insights(i))
        end do
        
        print *, ""
        print *, "=== Recommended Actions ==="
        print *, "1. Focus on operator spacing improvements"
        print *, "2. Enhance indentation consistency"
        print *, "3. Add configuration options for user preferences"
        print *, "4. Provide style guide recommendations based on user feedback"
        print *, "5. Continue collecting feedback to improve quality metrics"
        
    end subroutine demo_improvement_suggestions
    
end program test_user_feedback_demo
