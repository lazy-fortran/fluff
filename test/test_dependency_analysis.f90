program test_dependency_analysis
    use fluff_core
    use fluff_diagnostics
    use fluff_dependency_analysis
    implicit none
    
    integer :: total_tests, passed_tests
    
    print *, "=== Dependency Analysis Test Suite (GREEN Phase) ==="
    
    total_tests = 0
    passed_tests = 0
    
    ! Test different dependency analysis features
    call test_module_import_analysis()
    call test_circular_dependency_detection()
    call test_unused_import_detection() 
    call test_dependency_graph_generation()
    call test_import_organization()
    call test_complex_module_hierarchies()
    
    print *, ""
    print *, "=== Dependency Analysis Test Summary ==="
    print *, "Total tests: ", total_tests
    print *, "Passed tests: ", passed_tests
    print *, "Success rate: ", real(passed_tests) / real(total_tests) * 100.0, "%"
    
    if (passed_tests == total_tests) then
        print *, "✅ All dependency analysis tests passed!"
    else
        print *, "❌ Some tests failed"
        error stop 1
    end if
    
contains
    
    subroutine test_module_import_analysis()
        print *, ""
        print *, "Testing module import analysis..."
        
        ! Test 1: Basic module import detection
        call run_dependency_test("Basic module import detection", &
            test_basic_import_detection, .true.)
        
        ! Test 2: Module dependency chain analysis
        call run_dependency_test("Module dependency chain analysis", &
            test_dependency_chain_analysis, .true.)
        
        ! Test 3: Import statement parsing
        call run_dependency_test("Import statement parsing", &
            test_import_statement_parsing, .true.)
        
        ! Test 4: Module availability checking
        call run_dependency_test("Module availability checking", &
            test_module_availability_checking, .true.)
        
        ! Test 5: Standard library module handling
        call run_dependency_test("Standard library module handling", &
            test_standard_library_modules, .true.)
        
        ! Test 6: Module name resolution
        call run_dependency_test("Module name resolution", &
            test_module_name_resolution, .true.)
        
    end subroutine test_module_import_analysis
    
    subroutine test_circular_dependency_detection()
        print *, ""
        print *, "Testing circular dependency detection..."
        
        ! Test 1: Direct circular dependency
        call run_dependency_test("Direct circular dependency detection", &
            test_direct_circular_dependency, .true.)
        
        ! Test 2: Indirect circular dependency
        call run_dependency_test("Indirect circular dependency detection", &
            test_indirect_circular_dependency, .true.)
        
        ! Test 3: Self-referential module
        call run_dependency_test("Self-referential module detection", &
            test_self_referential_module, .true.)
        
        ! Test 4: Complex circular chains
        call run_dependency_test("Complex circular chain detection", &
            test_complex_circular_chains, .true.)
        
        ! Test 5: Circular dependency error reporting
        call run_dependency_test("Circular dependency error reporting", &
            test_circular_dependency_reporting, .true.)
        
        ! Test 6: Circular dependency path tracing
        call run_dependency_test("Circular dependency path tracing", &
            test_circular_path_tracing, .true.)
        
    end subroutine test_circular_dependency_detection
    
    subroutine test_unused_import_detection()
        print *, ""
        print *, "Testing unused import detection..."
        
        ! Test 1: Unused use statement detection
        call run_dependency_test("Unused use statement detection", &
            test_unused_use_statements, .true.)
        
        ! Test 2: Partially used module detection
        call run_dependency_test("Partially used module detection", &
            test_partially_used_modules, .true.)
        
        ! Test 3: Only-list unused symbols
        call run_dependency_test("Only-list unused symbols", &
            test_only_list_unused_symbols, .true.)
        
        ! Test 4: Rename-list unused symbols
        call run_dependency_test("Rename-list unused symbols", &
            test_rename_list_unused_symbols, .true.)
        
        ! Test 5: Implicit usage detection
        call run_dependency_test("Implicit usage detection", &
            test_implicit_usage_detection, .true.)
        
        ! Test 6: Conditional usage analysis
        call run_dependency_test("Conditional usage analysis", &
            test_conditional_usage_analysis, .true.)
        
    end subroutine test_unused_import_detection
    
    subroutine test_dependency_graph_generation()
        print *, ""
        print *, "Testing dependency graph generation..."
        
        ! Test 1: Basic dependency graph creation
        call run_dependency_test("Basic dependency graph creation", &
            test_basic_dependency_graph, .true.)
        
        ! Test 2: Graph node representation
        call run_dependency_test("Graph node representation", &
            test_graph_node_representation, .true.)
        
        ! Test 3: Graph edge relationships
        call run_dependency_test("Graph edge relationships", &
            test_graph_edge_relationships, .true.)
        
        ! Test 4: Dependency graph traversal
        call run_dependency_test("Dependency graph traversal", &
            test_dependency_graph_traversal, .true.)
        
        ! Test 5: Graph serialization
        call run_dependency_test("Graph serialization", &
            test_dependency_graph_serialization, .true.)
        
        ! Test 6: Graph visualization output
        call run_dependency_test("Graph visualization output", &
            test_graph_visualization_output, .true.)
        
    end subroutine test_dependency_graph_generation
    
    subroutine test_import_organization()
        print *, ""
        print *, "Testing import organization suggestions..."
        
        ! Test 1: Import ordering suggestions
        call run_dependency_test("Import ordering suggestions", &
            test_import_ordering_suggestions, .true.)
        
        ! Test 2: Import grouping suggestions
        call run_dependency_test("Import grouping suggestions", &
            test_import_grouping_suggestions, .true.)
        
        ! Test 3: Redundant import elimination
        call run_dependency_test("Redundant import elimination", &
            test_redundant_import_elimination, .true.)
        
        ! Test 4: Import consolidation suggestions
        call run_dependency_test("Import consolidation suggestions", &
            test_import_consolidation_suggestions, .true.)
        
        ! Test 5: Standard library separation
        call run_dependency_test("Standard library separation", &
            test_standard_library_separation, .true.)
        
        ! Test 6: Import formatting consistency
        call run_dependency_test("Import formatting consistency", &
            test_import_formatting_consistency, .true.)
        
    end subroutine test_import_organization
    
    subroutine test_complex_module_hierarchies()
        print *, ""
        print *, "Testing complex module hierarchies..."
        
        ! Test 1: Nested module dependencies
        call run_dependency_test("Nested module dependencies", &
            test_nested_module_dependencies, .true.)
        
        ! Test 2: Module interface dependencies
        call run_dependency_test("Module interface dependencies", &
            test_module_interface_dependencies, .true.)
        
        ! Test 3: Submodule dependency tracking
        call run_dependency_test("Submodule dependency tracking", &
            test_submodule_dependency_tracking, .true.)
        
        ! Test 4: Generic interface dependencies
        call run_dependency_test("Generic interface dependencies", &
            test_generic_interface_dependencies, .true.)
        
        ! Test 5: Module procedure dependencies
        call run_dependency_test("Module procedure dependencies", &
            test_module_procedure_dependencies, .true.)
        
        ! Test 6: Cross-file dependency resolution
        call run_dependency_test("Cross-file dependency resolution", &
            test_cross_file_dependency_resolution, .true.)
        
    end subroutine test_complex_module_hierarchies
    
    ! Helper subroutine for running tests
    subroutine run_dependency_test(test_name, test_proc, should_succeed)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: should_succeed
        
        interface
            function test_proc() result(success)
                logical :: success
            end function test_proc
        end interface
        
        logical :: success
        
        total_tests = total_tests + 1
        success = test_proc()
        
        if (success .eqv. should_succeed) then
            print *, "  PASS: ", test_name
            passed_tests = passed_tests + 1
        else
            print *, "  FAIL: ", test_name
        end if
        
    end subroutine run_dependency_test
    
    ! Individual test functions (GREEN phase - using implemented functionality)
    
    ! Module Import Analysis Tests
    function test_basic_import_detection() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        success = analyzer%analyze_imports("use iso_fortran_env", "test.f90")
    end function test_basic_import_detection
    
    function test_dependency_chain_analysis() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        character(len=256) :: files(2)
        files = ["file1.f90", "file2.f90"]
        success = analyzer%analyze_file_dependencies(files)
    end function test_dependency_chain_analysis
    
    function test_import_statement_parsing() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_import_statement_parsing
    
    function test_module_availability_checking() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_module_availability_checking
    
    function test_standard_library_modules() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_standard_library_modules
    
    function test_module_name_resolution() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_module_name_resolution
    
    ! Circular Dependency Detection Tests
    function test_direct_circular_dependency() result(success)
        logical :: success
        type(circular_dependency_detector_t) :: detector
        call detector%graph%add_node("mod_a", "a.f90")
        call detector%graph%add_node("mod_b", "b.f90")
        call detector%graph%add_edge(1, 2, "use")
        call detector%graph%add_edge(2, 1, "use")
        success = detector%detect_circular_dependencies()
    end function test_direct_circular_dependency
    
    function test_indirect_circular_dependency() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_indirect_circular_dependency
    
    function test_self_referential_module() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_self_referential_module
    
    function test_complex_circular_chains() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_complex_circular_chains
    
    function test_circular_dependency_reporting() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_circular_dependency_reporting
    
    function test_circular_path_tracing() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_circular_path_tracing
    
    ! Unused Import Detection Tests
    function test_unused_use_statements() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        character(len=:), allocatable :: unused(:)
        unused = analyzer%find_unused_imports()
        success = allocated(unused) .and. size(unused) > 0
    end function test_unused_use_statements
    
    function test_partially_used_modules() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_partially_used_modules
    
    function test_only_list_unused_symbols() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_only_list_unused_symbols
    
    function test_rename_list_unused_symbols() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_rename_list_unused_symbols
    
    function test_implicit_usage_detection() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_implicit_usage_detection
    
    function test_conditional_usage_analysis() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_conditional_usage_analysis
    
    ! Dependency Graph Generation Tests
    function test_basic_dependency_graph() result(success)
        logical :: success
        type(dependency_graph_t) :: graph
        call graph%add_node("test_module", "test.f90")
        success = graph%node_count == 1
    end function test_basic_dependency_graph
    
    function test_graph_node_representation() result(success)
        logical :: success
        type(dependency_graph_t) :: graph
        call graph%add_node("test_module", "test.f90")
        success = allocated(graph%nodes) .and. size(graph%nodes) > 0
    end function test_graph_node_representation
    
    function test_graph_edge_relationships() result(success)
        logical :: success
        type(dependency_graph_t) :: graph
        call graph%add_node("mod_a", "a.f90")
        call graph%add_node("mod_b", "b.f90")
        call graph%add_edge(1, 2, "use")
        success = graph%edge_count == 1
    end function test_graph_edge_relationships
    
    function test_dependency_graph_traversal() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_dependency_graph_traversal
    
    function test_dependency_graph_serialization() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_dependency_graph_serialization
    
    function test_graph_visualization_output() result(success)
        logical :: success
        type(dependency_graph_t) :: graph
        character(len=:), allocatable :: dot_output
        call graph%add_node("test_module", "test.f90")
        dot_output = graph%serialize_to_dot()
        success = index(dot_output, "digraph") > 0
    end function test_graph_visualization_output
    
    ! Import Organization Tests
    function test_import_ordering_suggestions() result(success)
        logical :: success
        type(import_organizer_t) :: organizer
        type(module_dependency_t) :: deps(1)
        character(len=:), allocatable :: ordered(:)
        deps(1)%module_name = "test_module"
        ordered = organizer%suggest_import_ordering(deps)
        success = allocated(ordered) .and. size(ordered) > 0
    end function test_import_ordering_suggestions
    
    function test_import_grouping_suggestions() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_import_grouping_suggestions
    
    function test_redundant_import_elimination() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_redundant_import_elimination
    
    function test_import_consolidation_suggestions() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_import_consolidation_suggestions
    
    function test_standard_library_separation() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_standard_library_separation
    
    function test_import_formatting_consistency() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_import_formatting_consistency
    
    ! Complex Module Hierarchy Tests
    function test_nested_module_dependencies() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_nested_module_dependencies
    
    function test_module_interface_dependencies() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_module_interface_dependencies
    
    function test_submodule_dependency_tracking() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_submodule_dependency_tracking
    
    function test_generic_interface_dependencies() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_generic_interface_dependencies
    
    function test_module_procedure_dependencies() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_module_procedure_dependencies
    
    function test_cross_file_dependency_resolution() result(success)
        logical :: success
        success = .false.  ! RED phase - not implemented yet
    end function test_cross_file_dependency_resolution
    
end program test_dependency_analysis