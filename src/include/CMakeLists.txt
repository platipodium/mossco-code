# CMakeLists.txt for src/include

# Ensure the output directory for headers exists
set(GENERATED_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)
file(MAKE_DIRECTORY ${GENERATED_INCLUDE_DIR})

# --- version.h ---
# Assuming PROJECT_VERSION_MAJOR, PROJECT_VERSION_MINOR, PROJECT_VERSION_PATCH are set in the root CMakeLists.txt
# Example: set(PROJECT_VERSION_MAJOR 1) set(PROJECT_VERSION_MINOR 0) set(PROJECT_VERSION_PATCH 0)
if(NOT DEFINED PROJECT_VERSION_MAJOR) set(PROJECT_VERSION_MAJOR 0) endif()
if(NOT DEFINED PROJECT_VERSION_MINOR) set(PROJECT_VERSION_MINOR 0) endif()
if(NOT DEFINED PROJECT_VERSION_PATCH) set(PROJECT_VERSION_PATCH 0) endif()

# Create a version.h.in if it doesn't exist (basic example)
set(VERSION_H_IN_CONTENT "
#ifndef VERSION_H
#define VERSION_H

#define MOSSCO_VERSION_MAJOR @PROJECT_VERSION_MAJOR@
#define MOSSCO_VERSION_MINOR @PROJECT_VERSION_MINOR@
#define MOSSCO_VERSION_PATCH @PROJECT_VERSION_PATCH@

#define MOSSCO_VERSION_STRING \"@PROJECT_VERSION_MAJOR@.@PROJECT_VERSION_MINOR@.@PROJECT_VERSION_PATCH@\"

#endif /* VERSION_H */
")
# A real version.h.in should be part of the source repository.
# For this task, we'll ensure one exists for configure_file.
if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/version.h.in")
    file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/version.h.in.tmp" "${VERSION_H_IN_CONTENT}")
    set(VERSION_H_IN_PATH "${CMAKE_CURRENT_BINARY_DIR}/version.h.in.tmp")
else()
    set(VERSION_H_IN_PATH "${CMAKE_CURRENT_SOURCE_DIR}/version.h.in")
endif()

set(VERSION_H_FILE ${GENERATED_INCLUDE_DIR}/version.h)
configure_file(${VERSION_H_IN_PATH} ${VERSION_H_FILE})
if(EXISTS "${CMAKE_CURRENT_BINARY_DIR}/version.h.in.tmp")
    file(REMOVE "${CMAKE_CURRENT_BINARY_DIR}/version.h.in.tmp")
endif()
message(STATUS "Configured ${VERSION_H_FILE}")

# --- git-sha.h ---
find_package(Git QUIET)
find_package(Subversion QUIET)

set(GIT_SHA_H_FILE ${GENERATED_INCLUDE_DIR}/git-sha.h)
set(GENERATE_GIT_SHA_SCRIPT ${CMAKE_CURRENT_LIST_DIR}/generate_git_sha.cmake)

# These variables need to be passed to the script if they are not cache variables
# or defined in a way the script (run with -P) can access them.
# For simplicity, ensure they are CACHE variables or set them explicitly here before the command.
# Example: -DUSE_FABM=${USE_FABM} -Dfabm_SOURCE_DIR=${fabm_SOURCE_DIR} ... etc.
# The script currently assumes they are available in the CMake variable space.
# This might require them to be CACHE variables or set globally.

# For FetchContent, source dirs are usually <lcname>_SOURCE_DIR (e.g. fabm_SOURCE_DIR)
# For ExternalProject, they are what you define in SOURCE_DIR param (e.g. EROSED_EP_SOURCE_DIR)
# GOTM_GIT_TAG and GETM_GIT_TAG are also assumed to be available.

# The add_custom_command will be run if ${GIT_SHA_H_FILE} is missing or out of date.
# The script generate_git_sha.cmake itself handles not overwriting the file if content is identical.
add_custom_command(
    OUTPUT ${GIT_SHA_H_FILE}
    COMMAND ${CMAKE_COMMAND}
            -DGEN_SCRIPT_OUTPUT_FILE=${GIT_SHA_H_FILE}
            -DPROJECT_SOURCE_DIR=${CMAKE_SOURCE_DIR} # Pass CMAKE_SOURCE_DIR as PROJECT_SOURCE_DIR
            # Pass other necessary variables:
            -DUSE_FABM=${USE_FABM} -Dfabm_SOURCE_DIR=${fabm_SOURCE_DIR}
            -DUSE_GOTM=${USE_GOTM} -DGOTM_GIT_TAG=${GOTM_GIT_TAG} # GOTM_SOURCE_DIR not needed if only tag used
            -DUSE_GETM=${USE_GETM} -DGETM_GIT_TAG=${GETM_GIT_TAG} -Dgetm_SOURCE_DIR=${getm_SOURCE_DIR}
            -DUSE_EROSED=${USE_EROSED} -DEROSED_EP_SOURCE_DIR=${EROSED_EP_SOURCE_DIR} # From external/CMakeLists.txt
            -P ${GENERATE_GIT_SHA_SCRIPT}
    DEPENDS ${GENERATE_GIT_SHA_SCRIPT}
    # Add dependencies on external source dir configure/checkout stamps if possible and reliable
    # e.g., if fabm_SOURCE_DIR is known and stable: DEPENDS ${fabm_SOURCE_DIR}/.git/HEAD (or similar)
    # This is complex with FetchContent as it might re-download.
    # For ExternalProject, can depend on stamp files.
    COMMENT "Generating ${GIT_SHA_H_FILE}"
    VERBATIM
)

# Custom target to ensure the headers are generated
add_custom_target(generate_version_files
    DEPENDS ${VERSION_H_FILE} ${GIT_SHA_H_FILE}
    COMMENT "Ensuring version.h and git-sha.h are up to date."
)

# --- Set Global Properties ---
set_property(GLOBAL PROPERTY MOSSCO_GENERATED_INCLUDE_DIR ${GENERATED_INCLUDE_DIR})
set_property(GLOBAL PROPERTY GIT_SHA_H_PATH ${GIT_SHA_H_FILE}) # For src/utilities
set_property(GLOBAL PROPERTY VERSION_H_PATH ${VERSION_H_FILE}) # For src/utilities

message(STATUS "MOSSCO Generated Include Directory: ${GENERATED_INCLUDE_DIR}")
message(STATUS "git-sha.h path: ${GIT_SHA_H_FILE}")
message(STATUS "version.h path: ${VERSION_H_FILE}")

# Make this directory's targets depend on generate_version_files
# This is important if other CMakeLists.txt files add targets that consume these headers
# and are processed before this directory, though typically include dirs are added first.
# For safety, any target *in this file* that might need headers would depend on it.
# However, usually, targets in other directories (like src/utilities) will depend on this.
# It's good practice for any library/executable in this project to depend on 'generate_version_files'.
# This can be done in the root CMakeLists.txt by adding dependency to main targets.
# add_dependencies(<main_mossco_target> generate_version_files)
