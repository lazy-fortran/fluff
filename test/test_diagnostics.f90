module test_diagnostics
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use fluff_core
    use fluff_diagnostics
    implicit none
    private
    
    public :: collect_diagnostics
    
contains
    
    !> Collect all tests
    subroutine collect_diagnostics(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        
        testsuite = [ &
            new_unittest("diagnostic_creation", test_diagnostic_creation), &
            new_unittest("diagnostic_formatting", test_diagnostic_formatting), &
            new_unittest("diagnostic_collection", test_diagnostic_collection), &
            new_unittest("fix_suggestion", test_fix_suggestion) &
        ]
        
    end subroutine collect_diagnostics
    
    subroutine test_diagnostic_creation(error)
        type(error_type), allocatable, intent(out) :: error
        type(diagnostic_t) :: diag
        type(source_range_t) :: loc
        
        loc%start%line = 10
        loc%start%column = 5
        loc%end%line = 10
        loc%end%column = 15
        
        diag = create_diagnostic( &
            code = "F001", &
            message = "Missing implicit none", &
            severity = SEVERITY_ERROR, &
            location = loc, &
            file_path = "test.f90" &
        )
        
        call check(error, diag%code == "F001", "Code should be F001")
        if (allocated(error)) return
        
        call check(error, diag%severity == SEVERITY_ERROR, "Should be error severity")
        if (allocated(error)) return
        
        call check(error, diag%file_path == "test.f90", "File path should match")
        
    end subroutine test_diagnostic_creation
    
    subroutine test_diagnostic_formatting(error)
        type(error_type), allocatable, intent(out) :: error
        type(diagnostic_t) :: diag
        type(source_range_t) :: loc
        character(len=:), allocatable :: formatted
        
        loc%start%line = 1
        loc%start%column = 1
        loc%end%line = 1
        loc%end%column = 10
        
        diag = create_diagnostic( &
            code = "F002", &
            message = "Inconsistent indentation", &
            severity = SEVERITY_WARNING, &
            location = loc, &
            file_path = "test.f90" &
        )
        
        formatted = format_diagnostic(diag)
        
        call check(error, index(formatted, "test.f90") > 0, &
            "Formatted output should contain file path")
        if (allocated(error)) return
        
        call check(error, index(formatted, "F002") > 0, &
            "Formatted output should contain diagnostic code")
        
    end subroutine test_diagnostic_formatting
    
    subroutine test_diagnostic_collection(error)
        type(error_type), allocatable, intent(out) :: error
        type(diagnostic_collection_t) :: collection
        type(diagnostic_t) :: diag1, diag2
        type(source_range_t) :: loc
        
        collection = create_diagnostic_collection()
        
        loc%start%line = 1
        loc%start%column = 1
        loc%end%line = 1
        loc%end%column = 10
        
        diag1 = create_diagnostic("F001", "Test 1", SEVERITY_ERROR, loc, "test1.f90")
        diag2 = create_diagnostic("F002", "Test 2", SEVERITY_WARNING, loc, "test2.f90")
        
        call collection%add(diag1)
        call collection%add(diag2)
        
        call check(error, collection%count() == 2, "Collection should have 2 diagnostics")
        if (allocated(error)) return
        
        call check(error, collection%has_errors(), "Collection should have errors")
        
    end subroutine test_diagnostic_collection
    
    subroutine test_fix_suggestion(error)
        type(error_type), allocatable, intent(out) :: error
        type(fix_suggestion_t) :: fix
        type(source_range_t) :: loc
        
        loc%start%line = 5
        loc%start%column = 1
        loc%end%line = 5
        loc%end%column = 20
        
        fix%description = "Add implicit none"
        fix%location = loc
        fix%replacement = "implicit none"
        
        call check(error, fix%description == "Add implicit none", &
            "Fix description should match")
        if (allocated(error)) return
        
        call check(error, fix%replacement == "implicit none", &
            "Fix replacement should match")
        
    end subroutine test_fix_suggestion
    
end module test_diagnostics