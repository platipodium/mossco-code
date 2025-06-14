# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: CC0-1.0
# SPDX-FileContributor: Carsten Lemmen <carsten.lemmen@hereon.de>

add_library(mossco_mediators OBJECT ${MEDIATOR_SOURCES})

set(MEDIATOR_SOURCES
benthic_pelagic_coupler.F90
#calculator_old.F90
calculator.F90
copy_coupler.F90
inout_coupler.F90
ocean_sediment_coupler.F90
pelagic_benthic_coupler.F90
#pelagic_benthic_mediator.F90
pelagic_soil_connector.F90
redist_coupler.F90
regrid_coupler.F90
remtc_atmos_ocean_coupler.F90
soil_pelagic_connector.F90
#soil_pelagic_mediator.F90
surfaces_coupler.F90
vertical_reduction.F90
xgrid_coupler.F90
)

target_sources(mossco_mediators PRIVATE ${MEDIATOR_SOURCES})

target_include_directories(mossco_mediators PUBLIC 
    ${CMAKE_BINARY_DIR}/src/utilities
)
target_link_libraries(mossco_mediators PUBLIC mossco_utilities)
target_link_libraries(mossco_mediators PUBLIC ESMF::ESMF)

target_include_directories(mossco_mediators
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


