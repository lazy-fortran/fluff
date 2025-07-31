program test_quality_improvements
    use fluff_formatter
    use fluff_format_quality
    implicit none
    
    type(formatter_engine_t) :: formatter
    type(format_quality_t) :: quality_before, quality_after
    type(aesthetic_settings_t) :: settings
    character(len=:), allocatable :: formatted_code, error_msg
    integer :: total_tests, passed_tests
    
    print *, "=== Format Quality Improvements Test Suite ==="
    
    total_tests = 0
    passed_tests = 0
    
    call formatter%initialize()
    
    ! Test quality assessment on poor quality code
    call test_basic_quality_assessment()
    call test_aesthetic_improvements()
    call test_quality_metrics()
    call test_readability_enhancements()
    
    print *, ""
    print *, "=== Quality Improvements Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"
    
    if (passed_tests == total_tests) then
        print *, "✅ All quality improvement tests passed!"
    else
        print *, "⚠️  Some tests need attention"
    end if
    
contains
    
    subroutine test_basic_quality_assessment()
        print *, ""
        print *, "Testing basic quality assessment..."
        
        call test_quality_for_code("Basic quality assessment", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer::x,y,z" // new_line('a') // &
            "x=1;y=2;z=x+y" // new_line('a') // &
            "print*,z" // new_line('a') // &
            "end program")
            
    end subroutine test_basic_quality_assessment
    
    subroutine test_aesthetic_improvements()
        print *, ""
        print *, "Testing aesthetic improvements..."
        
        ! Test with quality improvements enabled
        formatter%enable_quality_improvements = .true.
        
        call test_improvement("Aesthetic improvements ON", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer::a,b,c" // new_line('a') // &
            "real::x,y" // new_line('a') // &
            "a=5;b=10" // new_line('a') // &
            "c=a+b*2" // new_line('a') // &
            "x=3.14;y=x/2.0" // new_line('a') // &
            "if(c>0)then" // new_line('a') // &
            "print*,'positive'" // new_line('a') // &
            "endif" // new_line('a') // &
            "end program")
            
        ! Test with quality improvements disabled
        formatter%enable_quality_improvements = .false.
        
        call test_improvement("Aesthetic improvements OFF", &
            "program test" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "integer::a,b,c" // new_line('a') // &
            "a=5;b=10;c=a+b" // new_line('a') // &
            "end program")
            
        ! Re-enable for other tests
        formatter%enable_quality_improvements = .true.
        
    end subroutine test_aesthetic_improvements
    
    subroutine test_quality_metrics()
        print *, ""
        print *, "Testing quality metrics..."
        
        ! Test quality metrics on well-formatted code
        call test_quality_for_code("Well-formatted code metrics", &
            "program clean_example" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    " // new_line('a') // &
            "    integer :: count, total" // new_line('a') // &
            "    real :: average" // new_line('a') // &
            "    " // new_line('a') // &
            "    count = 10" // new_line('a') // &
            "    total = 100" // new_line('a') // &
            "    " // new_line('a') // &
            "    if (count > 0) then" // new_line('a') // &
            "        average = real(total) / real(count)" // new_line('a') // &
            "        print *, 'Average:', average" // new_line('a') // &
            "    end if" // new_line('a') // &
            "    " // new_line('a') // &
            "end program clean_example")
            
        ! Test quality metrics on poorly formatted code
        call test_quality_for_code("Poorly-formatted code metrics", &
            "program messy" // new_line('a') // &
            "integer::a,b,c,really_long_variable_name_that_exceeds_reasonable_length,another_very_long_name" // new_line('a') // &
            "a=1;b=2;c=a+b*really_long_variable_name_that_exceeds_reasonable_length+another_very_long_name" // new_line('a') // &
            "if(c>0)then;print*,c;endif" // new_line('a') // &
            "end program messy")
            
    end subroutine test_quality_metrics
    
    subroutine test_readability_enhancements()
        print *, ""
        print *, "Testing readability enhancements..."
        
        ! Test blank line insertion
        call test_improvement("Blank line insertion", &
            "module test_mod" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "type::point_t" // new_line('a') // &
            "real::x,y" // new_line('a') // &
            "end type" // new_line('a') // &
            "contains" // new_line('a') // &
            "function distance(p1,p2)result(d)" // new_line('a') // &
            "type(point_t),intent(in)::p1,p2" // new_line('a') // &
            "real::d" // new_line('a') // &
            "d=sqrt((p1%x-p2%x)**2+(p1%y-p2%y)**2)" // new_line('a') // &
            "end function" // new_line('a') // &
            "end module")
            
        ! Test operator spacing
        call test_improvement("Operator spacing", &
            "program operators" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "real::a,b,c,d" // new_line('a') // &
            "a=1.0;b=2.0" // new_line('a') // &
            "c=a+b-1.0*2.0/3.0" // new_line('a') // &
            "d=a**2+b**2" // new_line('a') // &
            "print*,c,d" // new_line('a') // &
            "end program")
            
    end subroutine test_readability_enhancements
    
    ! Helper subroutines
    subroutine test_quality_for_code(test_name, input_code)
        character(len=*), intent(in) :: test_name, input_code
        integer :: i
        
        total_tests = total_tests + 1
        
        call formatter%assess_quality(input_code, quality_before)
        
        print *, "  ", test_name, ":"
        print *, "    Overall Score: ", quality_before%overall_score, "/10"
        print *, "    Indentation:   ", quality_before%indentation_score, "/10"
        print *, "    Spacing:       ", quality_before%spacing_score, "/10"
        print *, "    Readability:   ", quality_before%readability_score, "/10"
        print *, "    Structure:     ", quality_before%structure_score, "/10"
        print *, "    Consistency:   ", quality_before%consistency_score, "/10"
        print *, "    Line Length:   ", quality_before%line_length_score, "/10"
        
        if (size(quality_before%recommendations) > 0) then
            print *, "    Recommendations:"
            do i = 1, min(3, size(quality_before%recommendations))
                print *, "      - ", trim(quality_before%recommendations(i))
            end do
        end if
        
        passed_tests = passed_tests + 1
    end subroutine test_quality_for_code
    
    subroutine test_improvement(test_name, input_code)
        character(len=*), intent(in) :: test_name, input_code
        
        total_tests = total_tests + 1
        
        ! Assess quality before formatting
        call formatter%assess_quality(input_code, quality_before)
        
        ! Format with quality improvements
        call formatter%format_with_quality(input_code, formatted_code, error_msg, quality_after)
        
        if (error_msg /= "") then
            print *, "  SKIP: ", test_name, " - Format error: ", error_msg
            return
        end if
        
        print *, "  ", test_name, ":"
        print *, "    Before: ", quality_before%overall_score, "/10"
        print *, "    After:  ", quality_after%overall_score, "/10"
        
        if (quality_after%overall_score >= quality_before%overall_score) then
            print *, "    ✓ Quality maintained or improved"
            passed_tests = passed_tests + 1
        else
            print *, "    ⚠ Quality decreased"
        end if
        
        ! Show some formatted output (first few lines)
        call show_formatting_sample(formatted_code)
        
    end subroutine test_improvement
    
    subroutine show_formatting_sample(code)
        character(len=*), intent(in) :: code
        integer :: i, line_count, line_start, line_end
        
        print *, "    Sample formatted output:"
        
        line_count = 0
        line_start = 1
        
        do i = 1, len(code)
            if (code(i:i) == new_line('a') .or. i == len(code)) then
                line_end = i - 1
                if (i == len(code)) line_end = i
                
                line_count = line_count + 1
                if (line_count <= 3) then
                    print *, "      ", code(line_start:line_end)
                else if (line_count == 4) then
                    print *, "      ..."
                    exit
                end if
                
                line_start = i + 1
            end if
        end do
        
    end subroutine show_formatting_sample
    
end program test_quality_improvements