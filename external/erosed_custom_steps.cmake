# This script is executed by ExternalProject_Add for EROSED
# It handles the specific SVN operations needed for EROSED.
# It assumes its Current Working Directory is <SOURCE_DIR> of the ExternalProject.

find_package(Subversion REQUIRED)

message(STATUS "EROSED Custom Steps: Starting SVN operations in CWD: ${CMAKE_CURRENT_LIST_DIR}") # CMAKE_CURRENT_LIST_DIR is where this script is located. CWD is different.

# Update specific subdirectories to depth infinity
set(SUBDIRS_TO_UPDATE include modules source)
foreach(subdir ${SUBDIRS_TO_UPDATE})
    message(STATUS "EROSED Custom Steps: Updating ${subdir} with --set-depth infinity")
    execute_process(
        COMMAND ${Subversion_SVN_EXECUTABLE} update --set-depth infinity ${subdir}
        # WORKING_DIRECTORY is implicitly <SOURCE_DIR>
        RESULT_VARIABLE svn_res
        OUTPUT_VARIABLE svn_out
        ERROR_VARIABLE svn_err
    )
    if(NOT svn_res EQUAL 0)
        message(FATAL_ERROR "EROSED Custom Steps: svn update ${subdir} failed.\nResult: ${svn_res}\nOutput: ${svn_out}\nError: ${svn_err}")
    endif()
endforeach()

# Export files from the Delft3D repository
set(DELFT3D_BASE_URL "https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/2399/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment")
set(DELFT3D_COMMON_URL "https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/2399/src/utils_lgpl/deltares_common/packages/deltares_common/src")
set(EROSED_TARGET_SUBDIR_SOURCE source) # Relative path to the source sub-directory

# Ensure target directory exists for exported files
if(NOT EXISTS ${EROSED_TARGET_SUBDIR_SOURCE})
    # This should have been created by "svn update --set-depth infinity source"
    message(WARNING "EROSED Custom Steps: Target source directory ./${EROSED_TARGET_SUBDIR_SOURCE} does not exist after svn update. Attempting to create.")
    file(MAKE_DIRECTORY ${EROSED_TARGET_SUBDIR_SOURCE})
endif()

set(FILES_TO_EXPORT
    bedbc1993.f90;${DELFT3D_BASE_URL}/bedbc1993.f90
    soursin_3d.f90;${DELFT3D_BASE_URL}/soursin_3d.f90
    compbsskin.f90;${DELFT3D_BASE_URL}/compbsskin.f90
    mathconsts.f90;${DELFT3D_COMMON_URL}/mathconsts.f90
)

foreach(file_info ${FILES_TO_EXPORT})
    string(SPLIT ";" parts ${file_info})
    list(GET parts 0 filename)
    list(GET parts 1 fileurl)

    set(TARGET_FILE_PATH "${EROSED_TARGET_SUBDIR_SOURCE}/${filename}")
    message(STATUS "EROSED Custom Steps: Exporting ${filename} from ${fileurl} to ./${TARGET_FILE_PATH}")
    execute_process(
        COMMAND ${Subversion_SVN_EXECUTABLE} export --force ${fileurl} ${TARGET_FILE_PATH}
        # WORKING_DIRECTORY is implicitly <SOURCE_DIR>
        RESULT_VARIABLE svn_res
        OUTPUT_VARIABLE svn_out
        ERROR_VARIABLE svn_err
    )
    if(NOT svn_res EQUAL 0)
        message(FATAL_ERROR "EROSED Custom Steps: svn export ${filename} failed.\nResult: ${svn_res}\nURL: ${fileurl}\nOutput: ${svn_out}\nError: ${svn_err}")
    endif()
endforeach()

message(STATUS "EROSED Custom Steps: SVN operations completed.")
