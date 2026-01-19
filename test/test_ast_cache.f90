program test_ast_cache
    ! Test AST caching functionality
    use fluff_core
    use fluff_ast
    use fluff_cache
    implicit none
    
    print *, "Testing AST cache..."
    
    ! Test 1: Basic cache operations
    call test_cache_operations()
    
    ! Test 2: Cache invalidation
    call test_cache_invalidation()
    
    ! Test 3: Cache capacity and eviction
    call test_cache_eviction()
    
    ! Test 4: Content hash verification
    call test_content_hash()
    
    print *, "[OK] All AST cache tests passed!"
    
contains
    
    subroutine test_cache_operations()
        type(ast_cache_t) :: cache
        type(fluff_ast_context_t) :: ast1, ast2, cached_ast
        character(len=:), allocatable :: content1, content2
        integer :: index
        
        cache = create_ast_cache()
        
        content1 = "program test" // new_line('a') // "end program"
        content2 = "module test" // new_line('a') // "end module"
        
        ! Test empty cache
        index = cache%get("test1.f90", content1)
        
        if (index /= 0) then
            error stop "Failed: cache should be empty"
        end if
        
        ! Put and get
        call cache%put("test1.f90", content1, ast1)
        index = cache%get("test1.f90", content1)
        
        if (index == 0) then
            error stop "Failed: should find cached AST"
        end if
        
        ! Get the actual AST
        cached_ast = cache%get_ast(index)
        
        ! Different content, same file
        index = cache%get("test1.f90", content2)
        
        if (index /= 0) then
            error stop "Failed: should not match different content"
        end if
        
        print *, "[OK] Basic cache operations"
        
    end subroutine test_cache_operations
    
    subroutine test_cache_invalidation()
        type(ast_cache_t) :: cache
        type(fluff_ast_context_t) :: ast
        character(len=:), allocatable :: content
        integer :: index
        
        cache = create_ast_cache()
        content = "program test" // new_line('a') // "end program"
        
        ! Put AST in cache
        call cache%put("test.f90", content, ast)
        
        ! Invalidate
        call cache%invalidate("test.f90")
        
        ! Try to get invalidated entry
        index = cache%get("test.f90", content)
        
        if (index /= 0) then
            error stop "Failed: invalidated entry should not be returned"
        end if
        
        ! Clear cache
        call cache%put("test1.f90", content, ast)
        call cache%put("test2.f90", content, ast)
        call cache%clear()
        
        index = cache%get("test1.f90", content)
        if (index /= 0) then
            error stop "Failed: cache should be cleared"
        end if
        
        print *, "[OK] Cache invalidation"
        
    end subroutine test_cache_invalidation
    
    subroutine test_cache_eviction()
        type(ast_cache_t) :: cache
        type(fluff_ast_context_t) :: ast
        character(len=:), allocatable :: content
        character(len=10) :: filename
        integer :: i
        
        ! Create small cache
        cache = create_ast_cache(max_size=5)
        content = "program test" // new_line('a') // "end program"
        
        ! Fill cache beyond capacity
        do i = 1, 10
            write(filename, '("test", I0, ".f90")') i
            call cache%put(trim(filename), content, ast)
        end do
        
        ! Cache should not exceed max capacity
        if (cache%size > 5) then
            print *, "Cache size:", cache%size, "Max capacity:", cache%max_capacity
            error stop "Failed: cache exceeded max capacity"
        end if
        
        print *, "[OK] Cache capacity and eviction"
        
    end subroutine test_cache_eviction
    
    subroutine test_content_hash()
        type(ast_cache_t) :: cache
        character(len=:), allocatable :: hash1, hash2, hash3
        
        cache = create_ast_cache()
        
        ! Same content should produce same hash
        hash1 = cache%compute_hash("test content")
        hash2 = cache%compute_hash("test content")
        
        if (hash1 /= hash2) then
            error stop "Failed: same content should have same hash"
        end if
        
        ! Different content should produce different hash
        hash3 = cache%compute_hash("different content")
        
        if (hash1 == hash3) then
            error stop "Failed: different content should have different hash"
        end if
        
        print *, "[OK] Content hash verification"
        
    end subroutine test_content_hash
    
end program test_ast_cache
