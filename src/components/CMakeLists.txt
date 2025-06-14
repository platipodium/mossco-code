# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: CC0-1.0
# SPDX-FileContributor: Carsten Lemmen <carsten.lemmen@hereon.de>

add_library(mossco_components OBJECT ${COMPONENT_SOURCES})

set(COMPONENT_SOURCES
    #clm_netcdf_component.F90
    dummy_component.F90
    empty_component.F90
    filtration_component.F90
    #geom_component.F90
    grid_component.F90
    inout_component.F90
    #location_component.F90
    netcdf_component.F90
    netcdf_input_component.F90
    particle_component.F90
    #random_component.F90
    #river_component.F90
    simplewave_component.F90
    # test_component.F90 auto-generated file, do not edit!
    time_aggregation_component.F90
    #vertical_macrobenthos_component.F90
)

if(USE_EROSED)
    list(APPEND COMPONENT_SOURCES
        biotypes.F90
        benthos_component.F90
        benthoseffect_class.F90
        erosed_component.F90
    )
endif()

if(USE_CMAQ)
    list(APPEND COMPONENT_SOURCES
        cmaq_component.F90
    )
endif()

if(USE_GETM)
    list(APPEND COMPONENT_SOURCES
        getm_component.F90
    )
endif()

if(USE_HAMSOM)
    list(APPEND COMPONENT_SOURCES
        hamsom_component.F90
    )
endif()

if(USE_FABM)
    list(APPEND COMPONENT_SOURCES
        fabm_pelagic_component.F90
        fabm_sediment_component.F90
        #fabm_benthic_component.F90
        #fabm0d_component.F90
        #fabmhv_component.F90
    )
    target_include_directories(mossco_components PRIVATE
            ${CMAKE_BINARY_DIR}/fabm/include
            ${CMAKE_BINARY_DIR}/fabm/modules
    )
endif()

if(USE_SCHISM)
    list(APPEND COMPONENT_SOURCES
        schism_component.F90
    )
endif()

if(USE_XIOS)
    list(APPEND COMPONENT_SOURCES
        xios_component.F90
    )
endif()

if(USE_OASIS)
    list(APPEND COMPONENT_SOURCES
        oasis_component.F90
    )
endif()

target_sources(mossco_components PRIVATE ${COMPONENT_SOURCES})

target_include_directories(mossco_components PUBLIC 
    ${CMAKE_BINARY_DIR}/src/drivers 
    ${CMAKE_BINARY_DIR}/src/utilities
)
target_link_libraries(mossco_components PUBLIC mossco_drivers mossco_utilities)
target_link_libraries(mossco_components PUBLIC ESMF::ESMF)

target_include_directories(mossco_components
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


