# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: CC0-1.0
# SPDX-FileContributor: Carsten Lemmen <carsten.lemmen@hereon.de>

# Placeholder for external CMake definitions
# This directory will contain external dependencies.
# Use FetchContent or find_package to include them.

# Option to include FABM
option(USE_FABM "Include FABM (Framework for Aquatic Biogeochemical Models)" ON)

if(USE_FABM)
  include(FetchContent)

  FetchContent_Declare(
    fabm
    GIT_REPOSITORY https://github.com/fabm-model/fabm.git
    GIT_TAG        v2.1.5 # Latest release as of 2024-03-15
  )

  # Note: FABM uses a custom variable FABM_SOURCE_DIR for its source directory
  # and FABM_BINARY_DIR for its build directory.
  # FetchContent_MakeAvailable will define fabm_SOURCE_DIR and fabm_BINARY_DIR.
  # We might need to set the FABM_SOURCE_DIR and FABM_BINARY_DIR to the values
  # used in the original Make build if FABM's CMake scripts rely on these exact names.
  # However, standard FetchContent practice is to use <lowercasename>_SOURCE_DIR.
  # Let's try with the standard variables first.
  FetchContent_MakeAvailable(fabm)

  # The fabm.mk file mentions MOSSCO_FABM_BINARY_DIR and MOSSCO_FABM_PREFIX.
  # If FABM's CMake build system doesn't pick up standard CMake variables
  # (like CMAKE_INSTALL_PREFIX for prefix or relying on its own build tree for binaries),
  # we might need to pass options during the add_subdirectory call implicitly done by
  # FetchContent_MakeAvailable, or set variables before it.
  # For now, assume FABM's CMakeLists.txt is well-behaved.

  # No explicit add_subdirectory(fabm) is needed here as FetchContent_MakeAvailable does that.
  # If FABM requires specific CMake variables to be set before its CMakeLists.txt is processed,
  # those would need to be set before FetchContent_MakeAvailable or passed via
  # CMAKE_ARGS to FetchContent_Declare.
  # For example, if FABM needed a specific install prefix:
  # set(FABM_PREFIX ${CMAKE_BINARY_DIR}/fabm_install CACHE PATH "FABM install prefix")
  # list(APPEND CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${FABM_PREFIX})
  # And then use CMAKE_ARGS in FetchContent_Declare.
  # The fabm.mk mentions FABM_PREFIX and MOSSCO_FABM_BINARY_DIR.
  # Let's assume FABM's own CMake handles its build and output directories correctly for now.
endif()

# Option to include GOTM
option(USE_GOTM "Include GOTM (General Ocean Turbulence Model)" ON)

if(USE_GOTM)
  # Ensure FetchContent is included (it might have been included by FABM already)
  include(FetchContent)

  FetchContent_Declare(
    gotm
    GIT_REPOSITORY https://github.com/gotm-model/gotm.git
    GIT_TAG        7de74f # Specific commit from gotm.mk
    GIT_SUBMODULES_RECURSE TRUE # Initialize and update submodules like extern/flexout
                                # For older CMake versions, GIT_SUBMODULES might be needed.
                                # Using GIT_SUBMODULES_RECURSE to match `git submodule update --recursive`
  )

  # Similar to FABM, FetchContent_MakeAvailable will define gotm_SOURCE_DIR and gotm_BINARY_DIR.
  # The gotm.mk file mentions external_GOTMDIR, GOTM_BINARY_DIR, and GOTM_PREFIX.
  # We assume GOTM's CMakeLists.txt handles its build and installation correctly.
  FetchContent_MakeAvailable(gotm)

  # If GOTM requires specific CMake variables to be set (e.g. for Fortran compiler flags,
  # paths, etc.), they would be set here before FetchContent_MakeAvailable or passed
  # via CMAKE_ARGS. The gotm.mk doesn't show specific build options being passed,
  # mostly git operations and directory setup.
endif()

# Option to include GETM
option(USE_GETM "Include GETM (General Estuarine Transport Model)" ON)

if(USE_GETM)
  # Ensure FetchContent is included
  include(FetchContent)

  # NOTE: The subtask specified https://github.com/getm-model/getm.git, but this URL
  # returned a 404 error. The original getm.mk uses git://git.code.sf.net/p/getm/code
  # (or https://git.code.sf.net/p/getm/code).
  # Proceeding with the SourceForge URL and 'iow' branch from getm.mk.
  FetchContent_Declare(
    getm
    GIT_REPOSITORY https://git.code.sf.net/p/getm/code.git # Using .git suffix, common for HTTPS git
    GIT_TAG        iow # Specific branch from getm.mk
    # No GIT_SUBMODULES specified as getm.mk does not show submodule commands for GETM.
  )

  # Similar to FABM and GOTM, FetchContent_MakeAvailable will define getm_SOURCE_DIR and getm_BINARY_DIR.
  # The getm.mk file mentions external_GETMDIR.
  # We assume GETM's CMakeLists.txt (if it has one) handles its build correctly.
  # The getm.mk command `$(MAKE) -C $(external_GETMDIR) distclean` suggests GETM might
  # use its own Makefile system. If GETM does not have a CMakeLists.txt at its root,
  # FetchContent_MakeAvailable (which implies add_subdirectory) might fail or not work as expected.
  # This might require using ExternalProject_Add instead. For now, following subtask's
  # implication of using add_subdirectory.
  FetchContent_MakeAvailable(getm)

  # If GETM requires specific CMake variables, they would be set here.
  # The getm.mk does not show specific build options beyond directory setup and its own make clean.
endif()

# Option to include EROSED
option(USE_EROSED "Include EROSED (Handles sediment processes)" ON)

if(USE_EROSED)
  include(ExternalProject)

  # Define the source directory for EROSED. This is where ExternalProject will download it.
  set(EROSED_EP_SOURCE_DIR ${CMAKE_BINARY_DIR}/erosed-src)
  set(EROSED_EP_BINARY_DIR ${CMAKE_BINARY_DIR}/erosed-build) # Build dir for EP, even if no actual build

  # Define paths that the main project can use. These will point inside EROSED_EP_SOURCE_DIR
  # These are set at CMake configure time, but files will only exist at build time
  # after the EROSED_ep target has run.
  set(EROSED_SOURCE_DIR ${EROSED_EP_SOURCE_DIR}/source CACHE INTERNAL "Path to EROSED source files")
  set(EROSED_INCLUDE_DIR ${EROSED_EP_SOURCE_DIR}/include CACHE INTERNAL "Path to EROSED include files")
  # The modules directory might also be needed if it contains Fortran modules.
  set(EROSED_MODULES_DIR ${EROSED_EP_SOURCE_DIR}/modules CACHE INTERNAL "Path to EROSED modules files")

  # Add EROSED as an external project
  ExternalProject_Add(EROSED_ep # Suffix _ep to distinguish the ExternalProject target name
    PREFIX            erosed-ep-prefix # Directory for EP temporary files, stamps, etc.
    SVN_REPOSITORY    https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example
    SVN_ARGS          --depth=empty # Initial checkout with empty depth
    # SVN_TRUST_CERT    TRUE # Add if SSL certificate issues arise with the SVN server

    SOURCE_DIR        ${EROSED_EP_SOURCE_DIR}
    BINARY_DIR        ${EROSED_EP_BINARY_DIR}

    # Custom steps to update subdirs and export files from Delft3D
    # The erosed_custom_steps.cmake script expects to be run from <SOURCE_DIR>
    PATCH_COMMAND     ${CMAKE_COMMAND} -E echo "EROSED_ep: Running custom SVN steps" &&
                      ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_LIST_DIR}/erosed_custom_steps.cmake

    # EROSED is likely a collection of source files, no specific build system
    CONFIGURE_COMMAND ""
    BUILD_COMMAND     ""
    INSTALL_COMMAND   ""
    BUILD_BYPRODUCTS  ${EROSED_SOURCE_DIR}/bedbc1993.f90 # Ensure patch command has run
                      ${EROSED_SOURCE_DIR}/soursin_3d.f90
                      ${EROSED_SOURCE_DIR}/compbsskin.f90
                      ${EROSED_SOURCE_DIR}/mathconsts.f90
                      ${EROSED_INCLUDE_DIR} # Placeholder for include dir existence
                      ${EROSED_MODULES_DIR} # Placeholder for modules dir existence

    # Ensure that the custom SVN commands are re-run if the script changes (optional but good practice)
    # This might require listing erosed_custom_steps.cmake as a dependency of the patch step,
    # ExternalProject doesn't directly support this for PATCH_COMMAND in older CMakes.
    # For simplicity, this is omitted, but for robustness, one might use UPDATE_COMMAND
    # or a more complex PATCH_COMMAND that checks script modification times.
  )

  # Create an interface library or similar to represent EROSED's include/source dirs
  # This is just one way to propagate usage requirements.
  # However, since EROSED source files are likely compiled directly into MOSSCO targets,
  # the main MOSSCO CMakeLists.txt will use EROSED_SOURCE_DIR and EROSED_INCLUDE_DIR directly.
  # For now, just ensuring the directories are set is the main goal.
  # Adding a target that depends on EROSED_ep can ensure it's built.
  add_library(erosed_placeholder INTERFACE)
  add_dependencies(erosed_placeholder EROSED_ep)
  # target_include_directories(erosed_placeholder INTERFACE ${EROSED_INCLUDE_DIR} ${EROSED_MODULES_DIR})
  # Sources would be added to the consuming target directly using ${EROSED_SOURCE_DIR}/*.f90 etc.

  message(STATUS "EROSED configured using ExternalProject_Add.")
  message(STATUS "EROSED source directory (available at build time): ${EROSED_SOURCE_DIR}")
  message(STATUS "EROSED include directory (available at build time): ${EROSED_INCLUDE_DIR}")
  message(STATUS "EROSED modules directory (available at build time): ${EROSED_MODULES_DIR}")

endif()

# Option to include JSON-Fortran
option(USE_JSON_FORTRAN "Include JSON-Fortran library" ON)

if(USE_JSON_FORTRAN)
  # Ensure FetchContent is included (might have been by previous dependencies)
  include(FetchContent)

  FetchContent_Declare(
    jsonfortran # Using a name that FetchContent_MakeAvailable will use for variables
    GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran.git
    GIT_TAG        9.0.3 # Latest release as of March 2024 from GitHub page
                         # Original Makefile clones default branch with depth 1.
                         # Using a specific tag is better for reproducibility.
  )

  # json-fortran is a well-behaved CMake project.
  # FetchContent_MakeAvailable will download, configure, and add it as a subdirectory.
  # It will create targets like jsonfortran::jsonfortran-static or jsonfortran::jsonfortran (shared).
  # The main MOSSCO build can then link against these.
  FetchContent_MakeAvailable(jsonfortran)

  message(STATUS "JSON-Fortran configured using FetchContent. Version: 9.0.3")
  # The actual targets (e.g., jsonfortran::jsonfortran-static) are made available by
  # json-fortran's own CMakeLists.txt when FetchContent_MakeAvailable is called.
  # The main project can link to these targets.
  # The main project can link to these targets.
endif()

# Option to include SQLite (from flibs-cvs, including Fortran wrapper)
option(USE_SQLITE "Include SQLite library with Fortran wrapper (from flibs-cvs)" ON)

if(USE_SQLITE)
  include(ExternalProject) # Ensure ExternalProject is included

  set(SQLITE_FLIBS_SOURCE_DIR ${CMAKE_BINARY_DIR}/flibs-cvs-sqlite-src)
  set(SQLITE_FLIBS_BUILD_DIR ${CMAKE_BINARY_DIR}/flibs-cvs-sqlite-build) # For build logs etc.
  set(SQLITE_FLIBS_INSTALL_LIB_DIR ${CMAKE_BINARY_DIR}/lib/sqlite_flibs)
  set(SQLITE_FLIBS_INSTALL_MOD_DIR ${CMAKE_BINARY_DIR}/modules/sqlite_flibs_fortran_mods)

  file(MAKE_DIRECTORY ${SQLITE_FLIBS_INSTALL_LIB_DIR})
  file(MAKE_DIRECTORY ${SQLITE_FLIBS_INSTALL_MOD_DIR})

  # Define output locations for CMake variables
  set(SQLITE_C_OBJECT_FILE ${SQLITE_FLIBS_INSTALL_LIB_DIR}/csqlite.o CACHE INTERNAL "Path to csqlite.o from flibs")
  set(SQLITE_Fortran_OBJECT_FILE ${SQLITE_FLIBS_INSTALL_LIB_DIR}/fsqlite.o CACHE INTERNAL "Path to fsqlite.o from flibs")
  set(SQLITE_Fortran_MODULE_DIR ${SQLITE_FLIBS_INSTALL_MOD_DIR} CACHE INTERNAL "Path to fsqlite fortran modules")
  # Set a global property that src/utilities/CMakeLists.txt can query
  set_property(GLOBAL PROPERTY MOSSCO_SQLITE_C_OBJECT_FILE ${SQLITE_C_OBJECT_FILE})
  set_property(GLOBAL PROPERTY MOSSCO_SQLITE_Fortran_OBJECT_FILE ${SQLITE_Fortran_OBJECT_FILE})
  set_property(GLOBAL PROPERTY MOSSCO_SQLITE_Fortran_MODULE_DIR ${SQLITE_Fortran_MODULE_DIR})


  ExternalProject_Add(sqlite_flibs_ep
    PREFIX            sqlite_flibs_ep_prefix
    CVS_REPOSITORY    :pserver:anonymous@flibs.cvs.sourceforge.net:/cvsroot/flibs
    CVS_MODULE        src/sqlite # Module to checkout
    # CVS_TAG           # No specific tag mentioned in Makefiles, gets HEAD

    SOURCE_DIR        ${SQLITE_FLIBS_SOURCE_DIR}
    BINARY_DIR        ${SQLITE_FLIBS_BUILD_DIR} # Log dir for build, not building in-source

    # Uses the Makefile within flibs/src/sqlite
    CONFIGURE_COMMAND "" # No configure step mentioned
    BUILD_COMMAND     $(MAKE) -C ${SQLITE_FLIBS_SOURCE_DIR} # Runs make in the checked-out sqlite dir
    INSTALL_COMMAND   ${CMAKE_COMMAND} -E copy ${SQLITE_FLIBS_SOURCE_DIR}/fsqlite.o ${SQLITE_Fortran_OBJECT_FILE} &&
                      ${CMAKE_COMMAND} -E copy ${SQLITE_FLIBS_SOURCE_DIR}/csqlite.o ${SQLITE_C_OBJECT_FILE} &&
                      ${CMAKE_COMMAND} -E copy_if_different ${SQLITE_FLIBS_SOURCE_DIR}/*.mod ${SQLITE_Fortran_MODULE_DIR}/
                      # copy_if_different for .mod files as their names might be compiler dependent

    # Ensure these products are considered by dependent targets
    BUILD_BYPRODUCTS  ${SQLITE_C_OBJECT_FILE} ${SQLITE_Fortran_OBJECT_FILE}
                      # Add specific .mod file if name is known and stable, e.g. ${SQLITE_Fortran_MODULE_DIR}/fsqlite.mod
                      # otherwise, the directory itself can be a byproduct if CMake supports it, or a stamp file.
  )

  # Create an interface library to represent the compiled SQLite objects and module path
  add_library(sqlite_flibs INTERFACE)
  add_dependencies(sqlite_flibs sqlite_flibs_ep) # Ensures sqlite_flibs_ep builds before anything using sqlite_flibs
  # The actual object files SQLITE_C_OBJECT_FILE and SQLITE_Fortran_OBJECT_FILE
  # will be added to the mossco_db library target in src/utilities/CMakeLists.txt
  # The include path for modules is SQLITE_Fortran_MODULE_DIR
  target_include_directories(sqlite_flibs INTERFACE ${SQLITE_Fortran_MODULE_DIR})
  # Add object files to the interface if that's how we want to propagate usage
  # target_sources(sqlite_flibs INTERFACE ${SQLITE_C_OBJECT_FILE} ${SQLITE_Fortran_OBJECT_FILE}) This is not standard for INTERFACE targets with objects.
  # Instead, src/utilities/CMakeLists.txt will directly use the OBJECT_FILE variables.

  message(STATUS "SQLite (from flibs-cvs) configured using ExternalProject_Add.")
  message(STATUS "SQLite C object file (build time): ${SQLITE_C_OBJECT_FILE}")
  message(STATUS "SQLite Fortran object file (build time): ${SQLITE_Fortran_OBJECT_FILE}")
  message(STATUS "SQLite Fortran module dir (build time): ${SQLITE_Fortran_MODULE_DIR}")

endif()
