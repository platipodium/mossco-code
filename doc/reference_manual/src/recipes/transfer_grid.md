# Recipe: how to transfer a grid

This recipe builds on a test example from ESMF, see `$ESMF_DIR/src/system_tests/ESMF_TransferGrid` of your local ESMF installation. 

This example demonstrates, how a field created on PET 1-4 with one component can inform the creation of a grid on PET 5-6 of another component.  Key to this task is the use of `phase` keywords in the coupler component.

## Coupler

#### Initialize Phase 1:
1. As import state, get the export state from component 1, as export state the import state of component.
2. Extract the field, its grid and its distgrid information from import state
3. Extract the field and vm from export state
4. Create a new distgrid as a copy of the import distgrid but with the export vm
5. Create a new grid with distgrid and export vm
6. Set the grid for the export field 

#### Initialize Phase 2:
1. As import state, get the export state from component 1, as export state the import state of component.
2. Reconcile import and export states
3. Extrat the field and its grid from import state
4. Extract the field, grid, and distgrid from export state
5. Create a new grid from import grid with export distgrid
6. Set the grid for the export field

#### Initialize Phase 3:
1. As import state, get the export state from component 1, as export state the import state of component.
2. Reconcile import and export states
3. Set the route handles for redist operations and add to import state.





-