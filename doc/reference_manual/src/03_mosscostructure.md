# MOSSCO modular structure

MOSSCO itself is structured in a modular fashion, with usually three levels in a hierarchy.  These hierarchies are **drivers**, **components**, and **examples**.

@image latex mossco_levels.png "Hierarchy of examples, components, drivers, and external models" width=.6\hsize
@image html mossco_levels.png "Hierarchy of examples, components, drivers, and external models" width=.6\hsize

@subsection structure-driver Driver
At the driver level, MOSSCO includes code that is *specific to an external model*.  The driver level code serves as an interface level between the external model and the component. Typically

- a driver *uses* modules from an external model
- does *not use esmf* 
- replaces or complements external model codes
- is hand-coded

@subsection structure-component Component
At the component level, MOSSCO includes code that exhibits the data from external models wrapped in an ESMF component.  The component level code is a major product of MOSSCO, and should be integrateable into other ESMF coupled codes. 

- a component *does not use* modules from an external model
- a component *uses* a driver
- is a fully specified ESMF component, i.e. contains SetServices, Initialize, Finalize, and Run routines
- handles an externally provided clock
- creates fields on which the external model data is represented
- is hand-coded or automatically processed (with the `src/scripts/create_component.py` script)

@subsection structure-example Example
At the example level, MOSSCO provides examples of ESMF coupled modular systems.  The example level code is a major product of MOSSCO, providing both scientifically exploitable coupled systems, as well as template code for user defined couplings.

- an example represents a complete compile-time coupled system
- results in a application, that is executed in a *setup* (see below) 
- automatically processed (with the `examples/generic/create_coupling.py` script), or, for reference, hand-coded.
 
 @section coupling-strategy Coupling strategy

[This section is experimental and a Request For Comments] Analogous to FABM, all components to MOSSCO that are available at compile time will be compiled and aggregated in [a single, multiple?] library in  $MOSSCO_DIR/lib/$FORTRAN_COMPILER.  At run-time, only those components that are used, are executed [this calls for dynamic loadable libraries].  The coupling itself is described in a text file, with a defined YAML structure [also add namelist, SiSi, CDL as allowed meta specifications].  A couping could be described as follows

~~~~.yaml
coupling:
  - components: [GOTM FABM0d]
    interval: 40 min
  - components: [GOTM FABMSED]
    interval: 60 min
  - components: [FABM0D GOTM]
    interval: 90 min
    direction: forward
  - components: [FABMSED FABM0d]
    interval: 60 min
    direction: both
~~~~

This example describes a system of 3 coupled components [find a shorter way to specify this?] and the data exchange intervals between these components. Within the coupling structure, a pair of components is specified with an update interval of data exchange and (optionally) the direction of coupling.  Direction can be any of forward, backward, and both, the default value is forward coupling.

In this example, the GOTM component provides data to FABM0D at 40 min intervals and to FABMSED at 60 min intervals.  FABMSED provides data to FABM0D at 60 min intervals, and FABM0D provides data to GOTM at 90 minutes, and to FABMSED at 60 minutes intervals (note the direction "both" argument).

During initialize, the components add alarms with the specified interval to the parent clock [demonstrate that this works], each alarm obtains as attributes the meta-information about each two-way coupling.  

The parent clock in the top level component, which calls FABM0D, GOTM and FABMSED repeatedly in its  Run() routine, examines its alarms for those next ringing ones and calls each component with a time duration until the next alarm suitable for that component. It then advance its own clock to the next alarm time, exchanges data, and calls the Run() routines of the two components whos alarm had triggered with a time duration until the next alarm related to each component. 

In our example above.  
1) GOTM and FABM0D are run for 40 mins, FABMSED for 60 mins
2) at t=40 GOTM gives data to FABM0D, GOTM and FABM0D are run for another 20 minutes 
3) at t=60 GOTM and FABM0D give data to FABMSED, FABMSED gives data to FABM0D; GOTM and FABM0D are run for 20 mins, FABMSED for 60 mins.
4) at t=80 GOTM gives data to FABM0D, GOTM and FABM0D  are run for 10 mins
5) at t=90 FABM0D gives data to GOTM, both are run for another 30 minutes
6) at t=120, steps (1) and (2) are repeated.

@subsection coupling-strategy-time Timers and alarms
