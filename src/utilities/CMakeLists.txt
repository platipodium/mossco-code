# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: CC0-1.0
# SPDX-FileContributor: Carsten Lemmen <carsten.lemmen@hereon.de>

# Ensure the directory for generated mossco_sha.F90 exists
file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/src/include)

# Custom command to generate mossco_sha.F90
set(MOSSCO_SHA_SCRIPT ${PROJECT_SOURCE_DIR}/scripts/git-sha.sh)
set(MOSSCO_SHA_OUTPUT ${PROJECT_BINARY_DIR}/src/include/mossco_sha.F90)

add_custom_command(
    OUTPUT ${MOSSCO_SHA_OUTPUT}
    COMMAND bash ${MOSSCO_SHA_SCRIPT} ${MOSSCO_SHA_OUTPUT}
    DEPENDS ${MOSSCO_SHA_SCRIPT} # If the script itself changes, regenerate
    COMMENT "Generating ${MOSSCO_SHA_OUTPUT} from git revision"
    VERBATIM
)

# Define the library target
add_library(mossco_utilities OBJECT ${UTILITIES_SOURCES})

# List of source files
set(UTILITIES_SOURCES
    ESMF_GridCreate3D.F90
    mossco.F90
    mossco_attribute.F90
    mossco_component.F90
    mossco_config.F90
    #mossco_db.F90
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
    #nc_extract_variable.c
    #nc_extract_variable_lasttime.c
    solver_library.F90
)

target_sources(mossco_utilities PRIVATE ${UTILITIES_SOURCES})

# Set Fortran module output directory
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
file(MAKE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}) # Ensure the directory exists

target_include_directories(mossco_utilities
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}> # For sources in this dir
        $<INSTALL_INTERFACE:include/mossco/utilities>   # For installed headers of this lib
        $<INSTALL_INTERFACE:include>                    # For project-wide installed headers (e.g. from src/include)
    PRIVATE
        ${PROJECT_SOURCE_DIR}/src/include       # Project-wide source includes
        ${PROJECT_BINARY_DIR}/src/include       # For generated headers like mossco_sha.F90
        ${CMAKE_Fortran_MODULE_DIRECTORY}      # For Fortran modules produced by this target
    INTERFACE
        ${CMAKE_Fortran_MODULE_DIRECTORY}      # Consumers need access to .mod files
        ${PROJECT_BINARY_DIR}/src/include       # Consumers might need generated headers
)

# Attempt to find ESMF
find_package(ESMF QUIET)

if(ESMF_FOUND)
    message(STATUS "ESMF found by find_package(ESMF). Linking with ESMF::ESMF.")
    target_link_libraries(mossco_utilities PRIVATE ESMF::ESMF)
    # Modern ESMF::ESMF target should propagate its include directories.
    # If not, explicitly add:
    # target_include_directories(mossco_utilities PRIVATE $<TARGET_PROPERTY:ESMF::ESMF,INTERFACE_INCLUDE_DIRECTORIES>)
else()
    message(WARNING "ESMF not found by find_package(ESMF). Placeholder for ESMFMKFILE logic or manual setup if needed.")
    # Placeholder for ESMFMKFILE logic:
    # if(DEFINED ENV{ESMFMKFILE} AND EXISTS "$ENV{ESMFMKFILE}")
    #     message(STATUS "ESMFMKFILE found at $ENV{ESMFMKFILE}. Parsing for flags (not implemented).")
    #     # Add commands to parse $ENV{ESMFMKFILE} for:
    #     # ESMF_F90COMPILEPATHS, ESMF_CXXCOMPILEPATHS, ESMF_F90ESMFLINKLIBS, etc.
    #     # And apply them using target_include_directories and target_link_libraries.
    #     # E.g., target_include_directories(mossco_utilities PRIVATE <parsed_esmf_include_paths>)
    #     # E.g., target_link_libraries(mossco_utilities PRIVATE <parsed_esmf_libraries>)
    # else()
    #     message(WARNING "ESMFMKFILE environment variable not set or file does not exist.")
    # endif()
endif()

# Find and link NetCDF
find_package(NetCDF REQUIRED COMPONENTS Fortran C)

if(NetCDF_FOUND)
    message(STATUS "NetCDF found. Linking with NetCDF::NetCDF_Fortran and NetCDF::NetCDF_C.")
    #target_link_libraries(mossco_utilities PRIVATE NetCDF::NetCDF_Fortran NetCDF::NetCDF_C)
    # Modern NetCDF targets should propagate their include directories.
    # If not, explicitly add:
    # target_include_directories(mossco_utilities PRIVATE ${NetCDF_INCLUDE_DIRS})
else()
    message(FATAL_ERROR "NetCDF (Fortran and C) was REQUIRED but not found. Check NetCDF installation and CMAKE_PREFIX_PATH.")
endif()

# Compiler flags based on src/Rules.make
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-O3>)
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10 OR CMAKE_Fortran_COMPILER_VERSION VERSION_EQUAL 10)
        target_compile_options(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fallow-argument-mismatch>)
    endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    target_compile_options(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-O3 -module ${CMAKE_Fortran_MODULE_DIRECTORY}>)
    # Potentially add -DNO_ISO_FORTRAN_ENV if needed, based on Rules.make EXTRA_CPP for IFORT
    # target_compile_definitions(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:NO_ISO_FORTRAN_ENV>)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "PGI") # For NVIDIA HPC SDK (nvfortran)
    target_compile_options(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-O3 -module ${CMAKE_Fortran_MODULE_DIRECTORY}>)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "XL" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IBM")
    target_compile_options(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-O3 -qmoddir=${CMAKE_Fortran_MODULE_DIRECTORY} -qstrict>)
    target_compile_definitions(mossco_utilities PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:NO_ISO_FORTRAN_ENV>)
endif()

# Add a dependency to ensure mossco_sha.F90 is generated before mossco_utilities is compiled
#add_dependencies(mossco_utilities ${MOSSCO_SHA_OUTPUT}) # This is not the right way for source files.
# The custom command output being part of UTILITIES_SOURCES handles this dependency.

message(STATUS "Configured mossco_utilities library.")
