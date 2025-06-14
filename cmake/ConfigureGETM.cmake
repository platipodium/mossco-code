# ConfigureGETM.cmake - Complete GETM configuration and integration
# Usage: include(ConfigureGETM)
#        configure_getm()
# 
# Required variables:
#   USE_GETM - Boolean to enable GETM
#   USE_FABM - Boolean for FABM configuration (used to toggle GETM_USE_FABM)
#
# Optional variables:
#   GETM_PREFIX - Path to pre-built GETM installation
#   GETM_BASE - Path to GETM source base directory

function(configure_getm)
    if(NOT USE_GETM)
        return()
    endif()
    
    mark_as_advanced(FORCE GETM_PREFIX GETM_BASE)
    
    if(GETM_PREFIX)
        find_package(GETM REQUIRED CONFIG HINTS "${GETM_PREFIX}" NO_DEFAULT_PATH)
        mark_as_advanced(CLEAR GETM_PREFIX)
    else()
        # Determine GETM_BASE
        if(NOT DEFINED GETM_BASE)
            if(DEFINED ENV{GETM_BASE})
                set(GETM_BASE $ENV{GETM_BASE})
            else()
                set(GETM_BASE "${CMAKE_CURRENT_LIST_DIR}/external/getm/code")
            endif()
        endif()
        message(STATUS "Using GETM_BASE=${GETM_BASE}")
        
        # Use separate variable to locate GETM source
        find_path(GETM_SOURCE_DIR src/getm/main.F90
            HINTS "${GETM_BASE}"
            DOC "Path to GETM source directory."
            NO_CMAKE_FIND_ROOT_PATH
        )
        
        if(NOT GETM_SOURCE_DIR)
            message(FATAL_ERROR "Could not find GETM source at or near ${GETM_BASE}")
        endif()
        
        set(GETM_USE_FABM NOT ${USE_FABM} CACHE BOOL "Toggle FABM in GETM" FORCE)
        
        # Determine the directory containing the CMakeLists.txt for GETM
        set(GETM_CMAKE_DIR "")
        set(GETM_BINARY_DIR "external/getm/code")
        
        # Check if CMakeLists.txt exists in the root of GETM_SOURCE_DIR
        if(EXISTS "${GETM_SOURCE_DIR}/CMakeLists.txt")
            set(GETM_CMAKE_DIR "${GETM_SOURCE_DIR}")
            message(STATUS "Found GETM CMakeLists.txt in root: ${GETM_SOURCE_DIR}")
        # Check if CMakeLists.txt exists in the src subdirectory
        elseif(EXISTS "${GETM_SOURCE_DIR}/src/CMakeLists.txt")
            set(GETM_CMAKE_DIR "${GETM_SOURCE_DIR}/src")
            set(GETM_BINARY_DIR "external/getm/code/src")
            message(STATUS "Found GETM CMakeLists.txt in src: ${GETM_SOURCE_DIR}/src")
        else()
            message(FATAL_ERROR "Could not find CMakeLists.txt in GETM source directory: ${GETM_SOURCE_DIR}")
        endif()
        
        # Add the subdirectory with the correct paths
        add_subdirectory("${GETM_CMAKE_DIR}" "${GETM_BINARY_DIR}")
        
        mark_as_advanced(CLEAR GETM_BASE)
    endif()
endfunction()

# Convenience macro that calls the function if USE_GETM is set
macro(configure_getm_if_enabled)
    if(USE_GETM)
        configure_getm()
    endif()
endmacro()