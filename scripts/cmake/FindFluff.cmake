# FindFluff.cmake - CMake module to find and use fluff linter
#
# This module defines:
#  FLUFF_FOUND - True if fluff is found
#  FLUFF_EXECUTABLE - Path to the fluff executable
#  FLUFF_VERSION - Version of fluff found
#
# Functions provided:
#  fluff_check_fortran(target) - Add fluff checking to a Fortran target
#  fluff_format_fortran(target) - Add fluff formatting to a Fortran target

cmake_minimum_required(VERSION 3.10)

# Find the fluff executable
find_program(FLUFF_EXECUTABLE
    NAMES fluff
    HINTS
        ${FLUFF_ROOT}
        $ENV{FLUFF_ROOT}
    PATH_SUFFIXES bin
    DOC "Path to fluff executable"
)

# Get version if executable found
if(FLUFF_EXECUTABLE)
    execute_process(
        COMMAND ${FLUFF_EXECUTABLE} --version
        OUTPUT_VARIABLE FLUFF_VERSION_OUTPUT
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    
    if(FLUFF_VERSION_OUTPUT)
        string(REGEX MATCH "fluff ([0-9]+\\.[0-9]+\\.[0-9]+)" 
               FLUFF_VERSION_MATCH "${FLUFF_VERSION_OUTPUT}")
        if(FLUFF_VERSION_MATCH)
            set(FLUFF_VERSION "${CMAKE_MATCH_1}")
        endif()
    endif()
endif()

# Handle standard find_package arguments
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Fluff
    REQUIRED_VARS FLUFF_EXECUTABLE
    VERSION_VAR FLUFF_VERSION
)

# Function to add fluff checking to a target
function(fluff_check_fortran target)
    if(NOT FLUFF_FOUND)
        message(WARNING "fluff not found, skipping linting for target ${target}")
        return()
    endif()

    # Get source files from target
    get_target_property(TARGET_SOURCES ${target} SOURCES)
    
    # Filter for Fortran files
    set(FORTRAN_SOURCES "")
    foreach(source ${TARGET_SOURCES})
        get_filename_component(ext ${source} EXT)
        if(ext MATCHES "\\.(f|f90|f95|f03|f08|F|F90|F95|F03|F08)$")
            list(APPEND FORTRAN_SOURCES ${source})
        endif()
    endforeach()
    
    if(FORTRAN_SOURCES)
        # Create custom target for linting
        add_custom_target(${target}_fluff_check
            COMMAND ${FLUFF_EXECUTABLE} check ${FORTRAN_SOURCES}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            COMMENT "Running fluff linter on ${target}"
            SOURCES ${FORTRAN_SOURCES}
        )
        
        # Make it depend on the main target
        add_dependencies(${target} ${target}_fluff_check)
        
        # Add to custom "lint" target if it exists
        if(TARGET lint)
            add_dependencies(lint ${target}_fluff_check)
        endif()
    endif()
endfunction()

# Function to add fluff formatting to a target
function(fluff_format_fortran target)
    if(NOT FLUFF_FOUND)
        message(WARNING "fluff not found, skipping formatting for target ${target}")
        return()
    endif()

    # Get source files from target
    get_target_property(TARGET_SOURCES ${target} SOURCES)
    
    # Filter for Fortran files
    set(FORTRAN_SOURCES "")
    foreach(source ${TARGET_SOURCES})
        get_filename_component(ext ${source} EXT)
        if(ext MATCHES "\\.(f|f90|f95|f03|f08|F|F90|F95|F03|F08)$")
            list(APPEND FORTRAN_SOURCES ${source})
        endif()
    endforeach()
    
    if(FORTRAN_SOURCES)
        # Create custom target for formatting
        add_custom_target(${target}_fluff_format
            COMMAND ${FLUFF_EXECUTABLE} format --fix ${FORTRAN_SOURCES}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            COMMENT "Running fluff formatter on ${target}"
            SOURCES ${FORTRAN_SOURCES}
        )
        
        # Add to custom "format" target if it exists
        if(TARGET format)
            add_dependencies(format ${target}_fluff_format)
        endif()
    endif()
endfunction()

# Create convenience targets if not already defined
if(FLUFF_FOUND AND NOT TARGET lint)
    add_custom_target(lint
        COMMENT "Running all linting checks"
    )
endif()

if(FLUFF_FOUND AND NOT TARGET format)
    add_custom_target(format
        COMMENT "Running all code formatters"
    )
endif()

mark_as_advanced(FLUFF_EXECUTABLE)