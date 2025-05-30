# Script to generate git-sha.h
# Expects GEN_SCRIPT_OUTPUT_FILE to be defined via -D argument

if(NOT DEFINED GEN_SCRIPT_OUTPUT_FILE)
    message(FATAL_ERROR "GEN_SCRIPT_OUTPUT_FILE is not defined. Call this script with -DGEN_SCRIPT_OUTPUT_FILE=<path_to_git-sha.h>")
endif()

find_package(Git QUIET)
find_package(Subversion QUIET)

set(MOSSCO_GIT_SHA "unknown")
if(GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/.git")
    execute_process(
        COMMAND ${GIT_EXECUTABLE} log -n1 --pretty=format:%H
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE MOSSCO_GIT_SHA_OUT
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    if(MOSSCO_GIT_SHA_OUT)
        set(MOSSCO_GIT_SHA ${MOSSCO_GIT_SHA_OUT})
    endif()
endif()

set(GIT_SHA_H_CONTENT "#ifndef GIT_SHA_H\n")
string(APPEND GIT_SHA_H_CONTENT "#define GIT_SHA_H\n\n")
string(APPEND GIT_SHA_H_CONTENT "#define MOSSCO_GIT_SHA \"${MOSSCO_GIT_SHA}\"\n")

# --- FABM ---
if(USE_FABM AND DEFINED fabm_SOURCE_DIR AND EXISTS "${fabm_SOURCE_DIR}/.git")
    set(FABM_GIT_SHA "unknown")
    if(GIT_FOUND)
        execute_process(
            COMMAND ${GIT_EXECUTABLE} log -n1 --pretty=format:%H
            WORKING_DIRECTORY ${fabm_SOURCE_DIR}
            OUTPUT_VARIABLE FABM_GIT_SHA_OUT
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
        )
        if(FABM_GIT_SHA_OUT)
            set(FABM_GIT_SHA ${FABM_GIT_SHA_OUT})
        endif()
    endif()
    string(APPEND GIT_SHA_H_CONTENT "#define FABM_GIT_SHA \"${FABM_GIT_SHA}\"\n")
else()
    string(APPEND GIT_SHA_H_CONTENT "/* FABM not used or source directory not found */\n")
endif()

# --- GOTM ---
# GOTM_GIT_TAG is set in external/CMakeLists.txt (it's a commit hash 7de74f)
if(USE_GOTM AND DEFINED GOTM_GIT_TAG)
    string(APPEND GIT_SHA_H_CONTENT "#define GOTM_GIT_SHA \"${GOTM_GIT_TAG}\"\n")
else()
    string(APPEND GIT_SHA_H_CONTENT "/* GOTM not used or GIT_TAG not defined */\n")
endif()

# --- GETM ---
# GETM_GIT_TAG is set in external/CMakeLists.txt (it's a branch name "iow")
if(USE_GETM AND DEFINED GETM_GIT_TAG AND DEFINED getm_SOURCE_DIR AND EXISTS "${getm_SOURCE_DIR}/.git")
    set(GETM_GIT_SHA "unknown (branch: ${GETM_GIT_TAG})")
    if(GIT_FOUND)
         execute_process(
            COMMAND ${GIT_EXECUTABLE} rev-parse HEAD
            WORKING_DIRECTORY ${getm_SOURCE_DIR}
            OUTPUT_VARIABLE GETM_REV_PARSE_OUT
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
        )
        if(GETM_REV_PARSE_OUT)
            set(GETM_GIT_SHA ${GETM_REV_PARSE_OUT})
        endif()
    endif()
    string(APPEND GIT_SHA_H_CONTENT "#define GETM_GIT_SHA \"${GETM_GIT_SHA}\"\n")
else()
    string(APPEND GIT_SHA_H_CONTENT "/* GETM not used, source dir not found, or GIT_TAG not a branch */\n")
endif()

# --- EROSED ---
# EROSED_EP_SOURCE_DIR is defined in external/CMakeLists.txt for ExternalProject
if(USE_EROSED AND DEFINED EROSED_EP_SOURCE_DIR AND EXISTS "${EROSED_EP_SOURCE_DIR}/.svn") # Check for .svn for SVN repos
    set(EROSED_SVN_REVISION "unknown")
    if(Subversion_SVN_EXECUTABLE)
        execute_process(
            COMMAND ${Subversion_SVN_EXECUTABLE} info --show-item revision .
            WORKING_DIRECTORY ${EROSED_EP_SOURCE_DIR}
            OUTPUT_VARIABLE EROSED_SVN_REV_OUT
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
        )
        if(EROSED_SVN_REV_OUT)
            set(EROSED_SVN_REVISION ${EROSED_SVN_REV_OUT})
        endif()
    endif()
    string(APPEND GIT_SHA_H_CONTENT "#define EROSED_SVN_REVISION \"${EROSED_SVN_REVISION}\"\n")
else()
    string(APPEND GIT_SHA_H_CONTENT "/* EROSED not used or source directory not SVN repo */\n")
endif()


string(APPEND GIT_SHA_H_CONTENT "\n#endif /* GIT_SHA_H */\n")

# Write the file, but only if content has changed
get_filename_component(OUTPUT_DIR ${GEN_SCRIPT_OUTPUT_FILE} DIRECTORY)
file(MAKE_DIRECTORY ${OUTPUT_DIR}) # Ensure output directory exists

if(EXISTS ${GEN_SCRIPT_OUTPUT_FILE})
    file(READ ${GEN_SCRIPT_OUTPUT_FILE} OLD_CONTENT)
else()
    set(OLD_CONTENT "")
endif()

if(NOT "${OLD_CONTENT}" STREQUAL "${GIT_SHA_H_CONTENT}")
    file(WRITE ${GEN_SCRIPT_OUTPUT_FILE} "${GIT_SHA_H_CONTENT}")
    message(STATUS "Generated git-sha.h")
else()
    message(STATUS "git-sha.h content unchanged, not rewritten.")
endif()
