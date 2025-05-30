# Try to locate FABM's installation prefix.
# if provided in GOTM_PREFIX, use FABM modules used by GOTM
find_path(FABM_PREFIX_PATH
  NAMES include/fabm_driver.h
  HINTS "$ENV{FABM_PREFIX}" "${GOTM_PREFIX}" "$ENV{FABM_PREFIX}"
  PATHS "$ENV{LOCALAPPDATA}/fabm/gotm" "$ENV{APPDATA}/fabm/gotm" "$ENV{HOME}/local/fabm/gotm"
  DOC "Installation prefix for Framework for Aquatic Biogeochemical Models - fabm.net"
)

# Find FABM library
find_library(FABM_LIBRARIES NAMES fabm
             HINTS ${FABM_PREFIX_PATH}/lib
             DOC "FABM libraries")

# Store configurable path of FABM include directory
find_path(FABM_INCLUDE_DIRS
          NAMES fabm_driver.h
          HINTS ${FABM_PREFIX_PATH}/include
          DOC "FABM include directory"
)

mark_as_advanced(FABM_LIBRARIES FABM_INCLUDE_DIRS)

if(NOT EXISTS ${FABM_INCLUDE_DIRS}/fabm_v0_compatibility.mod)
add_definitions(-D_OLD_FABM_)
endif()
if(NOT EXISTS ${FABM_INCLUDE_DIRS}/fabm_version.h)
add_definitions(-D_FABM_API_VERSION_=0)
endif()


# Process default arguments (QUIET, REQUIRED)
include(FindPackageHandleStandardArgs) 
find_package_handle_standard_args (FABM DEFAULT_MSG FABM_LIBRARIES FABM_INCLUDE_DIRS) 

# For backward compatibility:
set(FABM_LIBRARY FABM_LIBRARIES)
set(FABM_INCLUDE_DIR FABM_INCLUDE_DIRS)

message(STATUS "FABM library found: ${FABM_LIBRARIES}")
message(STATUS "FABM include directory found: ${FABM_INCLUDE_DIRS}")
