<!--
SPDX-FileCopyrightText 2021-2022 Helmholtz-Zentrum Hereon
SPDX-FileCopyrightText 2013-2021 Helmholtz-Zentrum Geesthacht
SPDX-License-Identifier: CC0-1.0
SPDX-FileContributor Carsten Lemmen <carsten.lemmen@hereon.de
-->

# Link coupler

The Link coupler links all fields contained in its import state into its export state.


## Methods and phases
The Link coupler contains a multi-phase `Initializate`, and single-phase `Run`and `Finalize` routines

- `Initialize` Phase 0: set InitializePhaseMap
- `Initialize` Phase 1: link fields in import state into export state
- `Run`: link fields in import state into export state
- `Finalize`: empty

## What it does

This coupler searches the list of ESMF Fields and ESMF FieldBundles in its import state. It

 1. skips fields/bundles that are identical in the export state
 2. replaces fields/bundles of the same name in the export state
 3. adds fields/bundles that are not in the export state
 
## Implementation notes
- Both `Initialize` phase 1 and `Run` perform the same linking operation

## Further considerations
- Attributes should be transferred between the import and export states
- Should the `replace` operation depend on the field status?

