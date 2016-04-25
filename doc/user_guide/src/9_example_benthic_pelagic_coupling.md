Benthic-pelagic coupling is a central feature, that is supported by MOSSCO. In order to keep most flexibility and modularity, the coupling is distribituted to a number of coupler components. MOSSCO aims to support 1-way and 2-way coupling in the following configurations:

  a) only sediment
  b) only water column
  c) water column and sediment 2-way coupled
  d) database based name matching (under development)

The strategy of the gridded component is to create field in the importState for the expected forcing data. The **fabm_pelagic_component** calls these fields *STATE_VARIABLE_NAME_upward_flux_at_soil_surface*. The **fabm_sediment_component** calls these fields *STATE_VARIABLE_NAME_at_soil_surface* for mass concentrations in the water phase and *STATE_VARIABLE_NAME_z_velocity_at_soil_surface* for the vertical velocity of particulate state variables.

a) only sediment
-----------

Here, we use the **fabm_sediment_component** together with the **pelagic_soil_connector**. The import state for pelagic boundary concentrations (*_at_soil_surface*) has to be filled. The **pelagic_soil_connector** can translate netcdf ouput of a simulation with fabm_pelagic into the required fields for the standalone sediment simulation. Missing information in the pelagic data may be filled with the **constant_component**. It is recommended to use *bcup_dissolved_variables=2* in run_sed.nml, since the forcing intervals might be large and matter fluxes are then updated every timestep based on the sediment state.

b) only water-column
-----------

Here, we use the **fabm_pelagic_component** together with the **soil_pelagic_connector**. The import state for boundary fluxes from the sediment (*_upward_flux_at_soil_surface*) has to be filled. The **soil_pelagic_connector** can translate netcdf ouput of a simulation with fabm_sediment into the required fields for the standalone sediment simulation

c) 2-way coupled simulation
-----------

The most common configuration: MOSSCO runs **fabm_pelagic_component** and **fabm_sediment_component** and exchanges information between both components. MOSSCO then uses both coupler components, the **soil_pelagic_component** to fill the importState of the **fabm_pelagic_component**, and the **pelagic_soil_component** to fill the importState of the **fabm_sediment_component**. Missing data fields (e.g. dissolved_oxygen_at_soil_surface for a pelagic NPZD model) may be filled with the **constant_component** and are not set through the connectors.
In the case of special pelagic-sediment model configurations, it may be easier to write a special coupler component instead of using the generic MOSSCO components.

*mass conservation issue - flux integration*

The **fabm_sediment_driver** is written such, that it can handle boundary conditions for dissolved variables as fluxes or as concentrations. This is controlled by the namelist parameter **bcup_dissolved_variables** in run_sed.nml (1 - fluxes are used, 2 - concentrations are used). The **fabm_sediment_driver** will adjust the boundary flux of dissolved variables due to its internal sediment state at every internal timestep for *bcup_dissolved_variables=2*. In this case, the boundary fluxes for dissolved state variabels are calculated in the **fabm_sediment_driver**. The **fabm_sediment_driver** can perform a pre-simulation and uses flux boundary conditions in this case.

However, it is recommended to fix the fluxes at coupling times for the pelagic and sediment component and therefore use *bcup_dissolved_variables=1*, in order to achieve mass conservation. In this case, the boundary fluxes for dissolved state variables are calculated in the **fabm_sediment_component** using the pelagic and sediment state.

*mass conservation issue - stoichiometric limits*

The **pelagic_soil_connector** so far exports particulate matter fluxes in carbon units, only. The amount of particulate matter is derived from the available detrital nitrogen pool because many coastal, pelagic models are nitrogen based. The coupler's strategy is therefore to take all available detritus nitrogen and let this sink (depending on turbulence and other resuspension-related forcing) into the sediments, distributed into two carbon pools. At the distribution stage, the flux is limited carbon-wise, such that at maximum all the available detritus carbon sinks out. The respective detritus nitrogen flux, to be integrated as sink in the pelagic model, is calculated in the **soil_pelagic_connector**.
This infrastructural overhead requires to set the N:C quota of the sediment particulate organic matter (POM) classes 3 times: in the sediment component and in the two connector components (through the input namelists).

d) database based name-matching
--------

A database based name-matching coupler component is prepared in **mediators/soil_pelagic_mediator**. More documentation can be found in the reference_manual under *soil_pelagic_coupler*
