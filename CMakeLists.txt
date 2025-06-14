# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: CC0-1.0
# SPDX-FileContributor: Carsten Lemmen <carsten.lemmen@hereon.de>

cmake_minimum_required(VERSION 3.10)

if(NOT PROJECT_NAME STREQUAL mossco)
    project(mossco VERSION 1.2.0
        DESCRIPTION "Modular System for Shelves and Coasts"
        LANGUAGES Fortran C
    )
endif()

# Homepage URL
set(PROJECT_HOMEPAGE_URL "http://www.mossco.de")

# Find CMake modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

# Use solution folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Use use position-independent code (-fPIC) everywhere if building shared libraries
if(BUILD_SHARED_LIBS)
   set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

# Options for third-party packages that we an link into
option(USE_HAMSOM "Enable HAMSOM-based drivers" OFF)
option(USE_CMAQ "Enable CMAQ-based drivers" OFF)
option(USE_GETM "Enable GETM-based drivers" ON)
option(USE_GOTM "Enable GOTM-based drivers" ON)
option(USE_FABM "Enable FABM-based drivers" ON)
option(USE_EROSED "Enable EROSED-related drivers (biotypes, benthoseffect_class)" OFF)

if(USE_FABM)
  mark_as_advanced(FORCE FABM_PREFIX FABM_BASE)

  if(FABM_PREFIX)
    # Check for pre-built FABM package
    find_package(fabm REQUIRED CONFIG HINTS "${FABM_PREFIX}" NO_DEFAULT_PATH)
    mark_as_advanced(CLEAR FABM_PREFIX)
  else()
    # Determine FABM_BASE from -D, env, or fallback to local directory
    if(NOT DEFINED FABM_BASE)
      if(DEFINED ENV{FABM_BASE})
        set(FABM_BASE $ENV{FABM_BASE})
      else()
        set(FABM_BASE "${CMAKE_CURRENT_LIST_DIR}/external/fabm/code")
      endif()
    endif()

    message(STATUS "Using FABM_BASE=${FABM_BASE}")

    # Use a separate variable to locate the FABM source
    find_path(FABM_SOURCE_DIR src/fabm.F90
      HINTS "${FABM_BASE}"
      DOC "Path to FABM source directory."
      NO_CMAKE_FIND_ROOT_PATH
    )

    if(NOT FABM_SOURCE_DIR)
      message(FATAL_ERROR "Could not find FABM source at or near ${FABM_BASE}")
    endif()

    set(FABM_FORCED_HOST mossco)
    cmake_policy(PUSH)
    cmake_policy(VERSION 3.5)  
    add_subdirectory("${FABM_SOURCE_DIR}" external/fabm/code)
    cmake_policy(POP)
    mark_as_advanced(CLEAR FABM_BASE)
  endif()
endif()





include(GitVersion)
include(FindESMF)

get_version_from_git()

# Add subdirectories
#add_subdirectory(src)
#add_subdirectory(examples)
#add_subdirectory(external)

# Enable testing with CTest
enable_testing()
