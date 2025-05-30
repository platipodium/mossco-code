# Define the library target
add_library(mossco_utilities "")

# List of source files
set(UTILITIES_SOURCES
    ESMF_GridCreate3D.F90
    mossco.F90
    mossco_attribute.F90
    mossco_component.F90
    mossco_config.F90
    mossco_db.F90
    mossco_dictionary.F90
    mossco_field.F90
    mossco_geom.F90
    mossco_grid.F90
    mossco_gridspec.F90
    mossco_info.F90
    mossco_loc.F90
    mossco_locstream.F90
    mossco_logging.F90
    mossco_memory.F90
    mossco_mesh.F90
    mossco_netcdf.F90
    mossco_parameter.F90
    mossco_routehandle.F90
    mossco_state.F90
    mossco_strings.F90
    mossco_time.F90
    mossco_variable_types.F90
    nc_extract_variable.c
    nc_extract_variable_lasttime.c
    solver_library.F90
)

target_sources(mossco_utilities PRIVATE ${UTILITIES_SOURCES})

# Set Fortran module output directory
# This needs to be consistent across the project and with how dependent libraries are found.
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules) # Use PROJECT_BINARY_DIR for uniqueness
file(MAKE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}) # Ensure the directory exists
# Also add it to include path for the target itself if modules are used internally
# and for other targets that might depend on these modules directly.
target_include_directories(mossco_utilities INTERFACE ${CMAKE_Fortran_MODULE_DIRECTORY})
target_include_directories(mossco_utilities PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})


# Attempt to find ESMF
# This is a placeholder. Proper ESMF finding might require a FindESMF.cmake module
# or parsing ESMFMKFILE if it's the intended way for this project.
find_package(ESMF QUIET)

if(ESMF_FOUND)
    message(STATUS "ESMF found")
    # Prefer using targets if available (ESMF::ESMF or similar)
    if(TARGET ESMF::ESMF)
        target_link_libraries(mossco_utilities PRIVATE ESMF::ESMF)
    else()
        # Fallback to variables if target is not found
        target_include_directories(mossco_utilities PRIVATE ${ESMF_INCLUDE_DIRS})
        target_link_libraries(mossco_utilities PRIVATE ${ESMF_LIBRARIES})
    endif()
else()
    message(WARNING "ESMF not found by find_package(ESMF). ESMFMKFILE logic might be needed.")
    # Placeholder for ESMFMKFILE logic or manual path setup
    # Example: Check for environment variable ESMFMKFILE
    if(DEFINED ENV{ESMFMKFILE} AND EXISTS "$ENV{ESMFMKFILE}")
        message(STATUS "ESMFMKFILE found at $ENV{ESMFMKFILE}")
        # Here you would typically parse ESMFMKFILE to get flags.
        # This is complex and often requires a dedicated CMake script.
        # For now, we'll just message and expect manual configuration or a FindESMF.cmake module.
        # Example of how one might try to extract include paths (needs refinement and error checking):
        # execute_process(COMMAND ${CMAKE_COMMAND} -E echo "Reading ESMFMKFILE for F90 paths")
        # execute_process(
        #     COMMAND awk "/^ESMF_F90COMPILEPATHS[ ]*=/ {gsub(\"^ESMF_F90COMPILEPATHS[ ]*=[ ]*\", \"\"); print}" "$ENV{ESMFMKFILE}"
        #     OUTPUT_VARIABLE ESMF_F90_PATHS_FROM_MKFILE
        #     OUTPUT_STRIP_TRAILING_WHITESPACE
        # )
        # if(ESMF_F90_PATHS_FROM_MKFILE)
        #    message(STATUS "ESMF F90 Paths from MKFILE: ${ESMF_F90_PATHS_FROM_MKFILE}")
        #    target_include_directories(mossco_utilities PRIVATE ${ESMF_F90_PATHS_FROM_MKFILE})
        # endif()
        # Similar for libraries (ESMF_F90ESMFLINKLIBS) and link paths (ESMF_F90LINKPATHS).
    else()
        message(WARNING "ESMFMKFILE environment variable not set or file does not exist.")
    endif()
endif()

# Find and link NetCDF
# Using REQUIRED will cause CMake to fail if NetCDF is not found.
find_package(NetCDF REQUIRED COMPONENTS Fortran C)

if(NetCDF_FOUND)
    message(STATUS "NetCDF found: Fortran='${NetCDF_Fortran_FOUND}', C='${NetCDF_C_FOUND}'")
    # For modern NetCDF CMake configurations, linking these targets handles includes and libraries.
    target_link_libraries(mossco_utilities PRIVATE NetCDF::NetCDF_Fortran NetCDF::NetCDF_C)
else()
    # This block should ideally not be reached if REQUIRED is used.
    message(FATAL_ERROR "NetCDF was REQUIRED but not found. Check NetCDF installation and CMAKE_PREFIX_PATH.")
endif()

# Set other include directories
# src/include contains mossco_sha.F90 which might be generated, so refer to PROJECT_BINARY_DIR
target_include_directories(mossco_utilities PUBLIC
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/include> # For finding headers in source tree
    $<INSTALL_INTERFACE:include>                         # For installed package (e.g. include/mossco)
)
target_include_directories(mossco_utilities PRIVATE
    ${PROJECT_SOURCE_DIR}/src/include                    # For compiling mossco_utilities itself
    ${PROJECT_BINARY_DIR}/src/include                    # For generated headers like mossco_sha.F90
)

# Compiler flags
# Specific flags like -J or -module are handled by CMAKE_Fortran_MODULE_DIRECTORY
# General optimization flags:
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-O3 -fallow-argument-mismatch>) # Added -fallow-argument-mismatch based on Rules.make for gfortran 10
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    target_compile_options(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-O3 -module ${CMAKE_Fortran_MODULE_DIRECTORY}>) # Intel might need -module explicitly if CMAKE_Fortran_MODULE_DIRECTORY isn't enough
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
    target_compile_options(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-O3 -module ${CMAKE_Fortran_MODULE_DIRECTORY}>)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "XL")
    target_compile_options(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-O3 -qmoddir=${CMAKE_Fortran_MODULE_DIRECTORY} -qstrict>) # From Rules.make
    target_compile_definitions(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:NO_ISO_FORTRAN_ENV>) # From Rules.make for XLF
endif()

# Add preprocessor definitions based on Rules.make findings
# These are examples; actual values depend on ESMF version and MPI/OMP status
# This should ideally be set by FindESMF.cmake or higher-level configuration
# target_compile_definitions(mossco_utilities PRIVATE
#     $<$<COMPILE_LANGUAGE:Fortran>:ESMF_VERSION_MAJOR=${ESMF_VERSION_MAJOR}> # Replace with actual value
#     $<$<COMPILE_LANGUAGE:Fortran>:ESMF_VERSION_MINOR=${ESMF_VERSION_MINOR}> # Replace with actual value
# )
# if(MOSSCO_MPI_FOUND) # This variable would need to be set based on ESMF check
#    target_compile_definitions(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:MOSSCO_MPI>)
# endif()
# if(MOSSCO_OMP_FOUND) # This variable would need to be set based on ESMF check
#    target_compile_definitions(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:MOSSCO_OMP>)
# endif()


message(STATUS "Configured mossco_utilities library target.")
