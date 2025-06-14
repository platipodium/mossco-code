# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: CC0-1.0
# SPDX-FileContributor: Carsten Lemmen <carsten.lemmen@hereon.de>

add_library(mossco_connectors STATIC)

set(CONNECTOR_SOURCES
    flux_connector.F90
    link_connector.F90
    nudge_connector.F90
    rename_connector.F90
    transport_connector.F90
)

target_sources(mossco_connectors PRIVATE ${CONNECTOR_SOURCES})

target_include_directories(mossco_connectors PUBLIC 
    ${CMAKE_BINARY_DIR}/src/utilities
)
target_link_libraries(mossco_connectors PUBLIC mossco_utilities)
target_link_libraries(mossco_connectors PUBLIC ESMF::ESMF)

target_include_directories(mossco_connectors
    PUBLIC # Changed from INTERFACE in prompt to PUBLIC for BUILD_INTERFACE and INSTALL_INTERFACE consistency
        # For consumers finding .mod files from this target
        "$<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>"
        "$<INSTALL_INTERFACE:include>" # Assuming .mod files might be installed to 'include/modules' or similar
                                       # This part needs alignment with actual install rules for modules.
                                       # For now, generic 'include' is a placeholder.
    PRIVATE
        # For mossco_drivers' own source files to find modules they produce (if any interdependencies)
        ${CMAKE_Fortran_MODULE_DIRECTORY}
)


