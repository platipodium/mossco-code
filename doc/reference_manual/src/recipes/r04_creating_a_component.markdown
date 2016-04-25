# Recipe #04: Creating a component

To create a component, you have to obey some rules.  All those rules are implemented in the examples provided with MOSSCO.

## ESMF Component Rules (obligatory)
(see also [NUOPC Rules](https://earthsystemcog.org/projects/nuopc/esmf2nuopc))

All MOSSCO components have to follow the generic rules for ESMF components. These are:

1. There are two types of components: Gridded Components (`ESMF_GridComp`) and Coupler Components (`ESMF_CplComp`).
2. A component makes available standard `Initialize`, `Run`, and `Finalize` methods. 
3. Gridded Components are connected through Coupler Components.
4. Coupler Components span the union of PETs of the Gridded Components they connect.
5. Only the data inside a Component's ImportState and ExportState is visible to other Components.
6. Time stepping is controlled by the parent component passing in a Clock object to its children's Run() methods.
7. Coupled components return from their Run() methods according to the coupling interval received from the parent.

## MOSSCO Component Rules (obligatory)

MOSSCO specific rules are similar to NUOPC rules.  They ensure that component interaction is facilitated

1. Every ESMF_GridComp instance has its own ImportState and ExportState. This means that each ESMF_GridComp instance is the sole constructor of the members (Fields, FieldBundles, Attributes) of its two externally visible states. 
2. ESMF_CplComps are not associated with their own ImportState and ExportState. Instead they are used to make pair connections between ESMF_GridComps,
3. Components are not allowed to modify the incoming (driver/parent) Clock.
4. ESMF_GridComps must keep an internal Clock.
5. ESMF_GridComps must implement compatibility checking ( “current time” and “time step”) between their internal Clock and the incoming driver/parent Clock. This check must be executed each time the Run() method is executed.
6. The coupling interval is defined by the parent's Clock: The beginning of the current interval is given by the parent's "current time" and the end of the interval is defined by the parent's "current time + time step".
7. An ESMF_GridComp's Run() method must take one coupling interval step forward, as defined by the incoming parent Clock, before returning.
8. Data between ESMF_GridComps is exchanged through Fields (ESMF_Field).
9. The public entry point of a MOSSCO compliant component must be called "SetServices".

## NUOPC Component Rules (optional)
You may choose to also follow the NUPOC Rules layed down in [this document](https://earthsystemcog.org/projects/nuopc/esmf2nuopc)

