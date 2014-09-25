This example runs mainly the fabm sediment component and is forced by constant
input fields from the constant component.

It is meant as a template for MOSSCO sediment modeling.


# Components

- fabm_sediment_component
- constant_component
- netcdf_component

# Couplers
- pelagic_benthic_coupler


# States
- only one exchange state passed through all components/couplers

# Initialization

constant -> fabm_seimdent -> constant -> netcdf

Constant component is initialized twice, because fabm_sediment replaced boundary fields.

# Run

The FABM sediment component is run with 1 h intervals (and it's internal timestep specified in run_sed.nml), every 24th time step, the netcdf component runs.

# How to use

Run `make` in this directory, an exectuable will be created

Go to a setup folder cointaining the input files (namelists and data files), and execute this example from that setup folder.
