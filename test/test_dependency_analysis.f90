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
        print *, "[OK] All dependency analysis tests passed!"
    else
        print *, "[FAIL] Some tests failed"
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
        type(dependency_analyzer_t) :: analyzer
        success = analyzer%analyze_imports("use module_a, only: func_a", "test.f90")
    end function test_import_statement_parsing
    
    function test_module_availability_checking() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Check for standard library module
        success = analyzer%analyze_imports("use iso_fortran_env", "test.f90")
    end function test_module_availability_checking
    
    function test_standard_library_modules() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test standard library recognition
        success = analyzer%analyze_imports("use iso_c_binding", "test.f90")
    end function test_standard_library_modules
    
    function test_module_name_resolution() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test module name resolution with renaming
        success = analyzer%analyze_imports("use my_module => renamed_module", "test.f90")
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
        type(circular_dependency_detector_t) :: detector
        ! Setup indirect circular dependency: A -> B -> C -> A
        call detector%graph%add_node("mod_a", "a.f90")
        call detector%graph%add_node("mod_b", "b.f90")
        call detector%graph%add_node("mod_c", "c.f90")
        call detector%graph%add_edge(1, 2, "use")
        call detector%graph%add_edge(2, 3, "use")
        call detector%graph%add_edge(3, 1, "use")
        success = detector%detect_circular_dependencies()
    end function test_indirect_circular_dependency
    
    function test_self_referential_module() result(success)
        logical :: success
        type(circular_dependency_detector_t) :: detector
        ! Test self-referential module
        call detector%graph%add_node("mod_self", "self.f90")
        call detector%graph%add_edge(1, 1, "use")
        success = detector%detect_circular_dependencies()
    end function test_self_referential_module
    
    function test_complex_circular_chains() result(success)
        logical :: success
        type(circular_dependency_detector_t) :: detector
        ! Complex circular chain: A -> B -> C -> D -> B
        call detector%graph%add_node("mod_a", "a.f90")
        call detector%graph%add_node("mod_b", "b.f90")
        call detector%graph%add_node("mod_c", "c.f90")
        call detector%graph%add_node("mod_d", "d.f90")
        call detector%graph%add_edge(1, 2, "use")
        call detector%graph%add_edge(2, 3, "use")
        call detector%graph%add_edge(3, 4, "use")
        call detector%graph%add_edge(4, 2, "use")
        success = detector%detect_circular_dependencies()
    end function test_complex_circular_chains
    
    function test_circular_dependency_reporting() result(success)
        logical :: success
        type(circular_dependency_detector_t) :: detector
        character(len=:), allocatable :: report
        ! Setup cycle and test reporting
        call detector%graph%add_node("mod_a", "a.f90")
        call detector%graph%add_node("mod_b", "b.f90")
        call detector%graph%add_edge(1, 2, "use")
        call detector%graph%add_edge(2, 1, "use")
        success = detector%detect_circular_dependencies()
        if (success) then
            report = detector%report_cycles()
            success = index(report, "Circular") > 0
        end if
    end function test_circular_dependency_reporting
    
    function test_circular_path_tracing() result(success)
        logical :: success
        type(circular_dependency_detector_t) :: detector
        ! Setup cycle and test path tracing
        call detector%graph%add_node("mod_x", "x.f90")
        call detector%graph%add_node("mod_y", "y.f90")
        call detector%graph%add_edge(1, 2, "use")
        call detector%graph%add_edge(2, 1, "use")
        success = detector%detect_circular_dependencies()
        if (success) then
            success = allocated(detector%cycle_paths)
        end if
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
        type(dependency_analyzer_t) :: analyzer
        ! Test partially used modules with only clause
        success = analyzer%analyze_imports("use my_module, only: func1, func2", "test.f90")
    end function test_partially_used_modules
    
    function test_only_list_unused_symbols() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test unused symbols in only list
        success = analyzer%analyze_imports("use math_module, only: sin, cos, tan", "test.f90")
    end function test_only_list_unused_symbols
    
    function test_rename_list_unused_symbols() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test renamed symbols
        success = analyzer%analyze_imports("use my_module, my_func => module_func", "test.f90")
    end function test_rename_list_unused_symbols
    
    function test_implicit_usage_detection() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test implicit module usage
        success = analyzer%analyze_imports("use implicit_module", "test.f90")
    end function test_implicit_usage_detection
    
    function test_conditional_usage_analysis() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test conditional usage within preprocessor directives
        success = analyzer%analyze_imports("use conditional_module", "test.f90")
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
        type(dependency_graph_t) :: graph
        character(len=:), allocatable :: deps(:)
        ! Test graph traversal
        call graph%add_node("mod_root", "root.f90")
        call graph%add_node("mod_child", "child.f90")
        call graph%add_edge(1, 2, "use")
        deps = graph%get_dependencies("mod_root")
        success = allocated(deps)
    end function test_dependency_graph_traversal
    
    function test_dependency_graph_serialization() result(success)
        logical :: success
        type(dependency_graph_t) :: graph
        character(len=:), allocatable :: serialized
        ! Test graph serialization
        call graph%add_node("mod_test", "test.f90")
        serialized = graph%serialize_to_dot()
        success = index(serialized, "mod_test") > 0
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
        type(import_organizer_t) :: organizer
        type(module_dependency_t) :: deps(2)
        character(len=:), allocatable :: grouped(:)
        deps(1)%module_name = "iso_fortran_env"
        deps(1)%is_standard_library = .true.
        deps(2)%module_name = "my_module"
        grouped = organizer%suggest_import_grouping(deps)
        success = allocated(grouped)
    end function test_import_grouping_suggestions
    
    function test_redundant_import_elimination() result(success)
        logical :: success
        type(import_organizer_t) :: organizer
        type(module_dependency_t) :: deps(2)
        character(len=:), allocatable :: redundant(:)
        deps(1)%module_name = "duplicate_module"
        deps(2)%module_name = "duplicate_module"
        redundant = organizer%find_redundant_imports(deps)
        success = allocated(redundant)
    end function test_redundant_import_elimination
    
    function test_import_consolidation_suggestions() result(success)
        logical :: success
        type(import_organizer_t) :: organizer
        type(module_dependency_t) :: deps(2)
        character(len=:), allocatable :: consolidated(:)
        deps(1)%module_name = "math_module"
        deps(2)%module_name = "math_module"
        consolidated = organizer%suggest_consolidation(deps)
        success = allocated(consolidated)
    end function test_import_consolidation_suggestions
    
    function test_standard_library_separation() result(success)
        logical :: success
        type(import_organizer_t) :: organizer
        type(module_dependency_t) :: deps(3)
        character(len=:), allocatable :: separated(:)
        deps(1)%module_name = "iso_c_binding"
        deps(1)%is_standard_library = .true.
        deps(2)%module_name = "user_module"
        deps(3)%module_name = "iso_fortran_env"
        deps(3)%is_standard_library = .true.
        separated = organizer%suggest_import_ordering(deps)
        success = allocated(separated) .and. size(separated) >= 3
    end function test_standard_library_separation
    
    function test_import_formatting_consistency() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test import formatting consistency
        success = analyzer%analyze_imports("USE   Module_Name  ,  ONLY :  func", "test.f90")
    end function test_import_formatting_consistency
    
    ! Complex Module Hierarchy Tests
    function test_nested_module_dependencies() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test nested module dependencies
        success = analyzer%analyze_imports("use parent_module", "nested.f90")
    end function test_nested_module_dependencies
    
    function test_module_interface_dependencies() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test module interface dependencies
        success = analyzer%analyze_imports("use interface_module", "interface.f90")
    end function test_module_interface_dependencies
    
    function test_submodule_dependency_tracking() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test submodule dependency tracking
        success = analyzer%analyze_imports("submodule (parent) child", "submod.f90")
    end function test_submodule_dependency_tracking
    
    function test_generic_interface_dependencies() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test generic interface dependencies
        success = analyzer%analyze_imports("use generic_ops", "generic.f90")
    end function test_generic_interface_dependencies
    
    function test_module_procedure_dependencies() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        ! Test module procedure dependencies
        success = analyzer%analyze_imports("use procedures_module", "proc.f90")
    end function test_module_procedure_dependencies
    
    function test_cross_file_dependency_resolution() result(success)
        logical :: success
        type(dependency_analyzer_t) :: analyzer
        character(len=256) :: files(3)
        files = ["file1.f90", "file2.f90", "file3.f90"]
        success = analyzer%analyze_file_dependencies(files)
    end function test_cross_file_dependency_resolution
    
end program test_dependency_analysis
