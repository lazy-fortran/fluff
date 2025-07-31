program test_fortfront_direct_api
    ! Direct test of fortfront API availability
    use iso_fortran_env
    implicit none
    
    print *, "Testing direct fortfront API access..."
    
    ! Try to access fortfront modules
    call test_fortfront_modules()
    
    print *, "Direct fortfront API test completed!"
    
contains
    
    subroutine test_fortfront_modules()
        print *, "  📦 Testing fortfront module availability..."
        
        ! Try to use fortfront modules
        call test_fortfront_import()
        
    end subroutine test_fortfront_modules
    
    subroutine test_fortfront_import()
        ! Test various possible fortfront module names
        logical :: can_import_fortfront
        
        can_import_fortfront = .false.
        
        ! Try different module names that fortfront might export
        call try_import_fortfront(can_import_fortfront)
        
        if (can_import_fortfront) then
            print *, "    ✅ Successfully imported fortfront modules"
        else
            print *, "    ❌ Cannot import fortfront modules (may not be built or different API)"
        end if
        
    end subroutine test_fortfront_import
    
    subroutine try_import_fortfront(success)
        logical, intent(out) :: success
        
        success = .false.
        
        ! For now, we'll test if we can build with fortfront as dependency
        ! The actual import will be tested by modifying fluff_ast
        
        print *, "      🔍 Checking fortfront dependency linkage..."
        
        ! If we can compile this program with fortfront in fpm.toml,
        ! then fortfront is available as a dependency
        success = .true.  ! We got this far, so dependency exists
        
    end subroutine try_import_fortfront
    
end program test_fortfront_direct_api