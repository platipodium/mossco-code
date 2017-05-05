## Component Filtraion

The `filtration_component` describes the instantaneous filtration of matter
within the water column.  It creates a (negative) matter flux for filtered
items.

### Description

The component obtains 3D-concentration of a variable to filter,  and delivers
the 3D-concentration flux of this variable as output.  

### Implementation

The filtration component is realized in the file `filtration_component.F90`. It uses
MOSSCO infrastructure from `$MOSSCO_DIR/src/utilities`, i.e. it depends on the
`libmossco_utils` library.

The filtration component  is an `ESMF_GridComp`.  There is no internal timestep to this
component, the clock is advanced to its `stopTime` every timestep.

There can be multiple instances of this component.

The component relies on a foreign grid delivered at initialize time.  This
requirement can be met by specifying a suitable `dependencies` entry in the
coupling configuration.

At run time, the component relies on velocities and concentrations, as well as
on mussel abundance.

### Configuration

The component can be configured by the file `filtration.cfg` (or a
respective configuration file upon name change of the component).  In this file,
the following key--value pairs are recognized:

    mass: <mass per individual mussel>
    minimum_food_flux: <lower threshold for food supply rate in mmol C s-1 m-3>
    filter: <name of material to filter>

- The `mass:` is the dry weight of an individual mussel. The default is
0.6 g
- the `minimum_food_flux:` is the minimal food suply rate above which the filter feeders
  start to filter.  Default is 0.6166697552 mmol C s-1 m-3, equiv to 20 mg C
- the `filter:` is the name of the material/chemical species to filter.  The default
is phytoplankton
- the `other:` is a white-space separated list of other species that are co-filtered with
the same relative loss rate as the main `filter:` species.

### History

The filtration component was introduced in February 2016 as a 2D component
- revision to 3D in March 2016
- bug-fixing and testing in April 2016

### Verification

This section calculates the model results with typical values and references the
respective code sections

        minimumFoodFlux  = 0.6166697552  ! mmol C s-1 m-3, equiv to 20 mg C
        mussel_mass = 0.6 ! g gross DW / individual
        mmolPermg = 0.03083348776  ! This is the ratio of C to sugar CH2O, and N,P at Redfield
        mgPermmol = 1./mmolPermg  ! 32,432269998 molar weight

We assume phytoplankton as the `filter:` varialble and get its concentration

        call ESMF_FieldGet(fieldList(1), farrayPtr=concentration) ! in mmol m-3
