# Recipe #18: Adding boundary conditions

MOSSCO includes a generic way to include boundary conditions by using a combination of the `netcdf_input` component with the `nudge_connector` and some other target component that receives the boundary conditions.

## Assessing your boundary needs

From the target component, create a list of variable names and the locations where these variables should have boundary conditions (lateral, top, bottom, points)

## Creating a boundary file and configuration

Create a netcdf file suitable for the `netcdf_input_component`, which contains information on the target variables.  Mask all points that should *not* be forced by setting a `missing_value`.  For example, if you want to force at the left edge (index i=1) only in a 3D field (i,j,k), then set all values at (2:n,:,:) to missing_value.

Create the configuration file for your `netcdf_input_component`, i.e. a file containing at least the  `filename:` key to point to your boundary netcdf file.

> Optionally, you can use the `include:` and `exclude:` lists to limit the names of variables read, and you can rename variables with the `alias:` key, and you can choose the temporal interpolation: method

## Creating/appending to the coupling specification

In your coupling specification, add the lines

		- components:
			- boundary_input
			- nudge_connector
			- target_component

(e.g., assuming your target component is named `target_component` and your `boundary_input`  is an instance of `netcdf_input_component`).  You might want to specify the coupling interval as well.

## Examples

In `$MOSSCO_SETUPDIR/sns/Forcing`, there is an example boundary files with box shape biogeochemical forcing data.
