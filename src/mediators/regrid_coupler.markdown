## Component NetCDF input

The regrid coupler is used to interpolate values from one grid, mesh or 
locstream to another.


### Implementation

The regrid coupler is realized in the file `regrid_coupler.F90`. It depends on 
the `mossco_netcdf` utilities, as well as other mossco objects like 
'mossco_component', 'mossco_field', etc.


### Default behavior

The regrid coupler uses bilinear regridding by default. Other ESMF
regridding methods are also available, such as first- and second-order 
conservative, patch, and nearest neighbor. Masking is not enabled by default,
but can be set in the regrid configuration to use specific integer-valued mask 
values set on the underlying Grid of the Field that is to be masked. If there 
are points in the target domain that lie outside of the source domain, they 
can be ignored by setting the unmappedAction flag to ESMF_FieldRegridStore(). 
Extrapolation is available by passing the extrapMethod flag to the 
ESMF_FieldRegridStore() call.


### Configuration

I don't know this part very well, Carsten can you fill this in?

### Regridding Status

#### Regular grid status table

| Target      | CF     | SCRIP |
| ----------- | ------ | ----- |
| sns         | works  | fails |
| nsbs        | works  | fails |
| esacci      | works1 | fails |
| remo        | works  | fails |
| sns3D       | fails  | n/a   |
| wilson      | t.b.d  | t.b.d |
| alphaventus | works  | n/a   |
| clm         | n/a    | works |
| gb300       | n/a    | n/a   |
| oderhaff    | n/a    | fail  |
| wbs         | n/a    | fail  |

Status
 - regridding to CF is complete for sns, nsbs, clm, remo, alphaventus,
   regridding onto itself (esacci) swaps latitude in ncview
 - regridding to SCRIP works only for clm and fails for all others
 - grid files are needed for gb300 test case
 - the sns 3d only produces 2d output

#### Unstructured and location stream status table

 | Target                 | UGRID          | SCRIP | UGRID | ESMF |
 | ---------------------- | -------------- | ----- | ----- | ---- |
 | ices                   | works on nodes |       |       |      |
 | bsh                    |                |       |       |      |
 | elbeweser_locstream    |                | t.b.d |       |      |
 | elbeweser_unstructured |                | t.b.d |       |      |

- regridding to the ices_boxes UGRID currently projects onto nodes, but should
  ultimately project on elements.
  - grid file with face coordinate information is needed, this can be done
    by adding data with the location attribute set to face.

% #### NSBS porosity mapped onto all other grids
% 
% The third example tests regridding from the entire domain (nsbs) to all of
% the specified subregions (sns, wbs, oderhaff) from grids read from file.
% Data on deposition and porosity are read from nsbs_porosity/nsbs_deposition and
% interpolated to other grids.
% Some of these may be commented out to increase processing speed.  Work is
% required to save the internal state / global storage of the coupler to
% memorize coupling fields handle across ESMF phases. Please comment/uncomment
% to enable more or less regridding tasks.
% 
%  | Target                 | status                             |
%  | ---------------------- | ---------------------------------- |
%  | sns                    | works                              |
%  | nsbs                   | works                              |
%  | esacci                 | works*                             |
%  | remo                   | works                              |
%  | sns3D                  | fail, ESMF not implemented         |
%  | wilson                 | fail, SCRIP reading                |
%  | alphaventus            | works                              |
%  | clm                    | works                              |
%  | gb300                  | fail SCRIP reading                 |
%  | oderhaff               | fail result garbled                |
%  | wbs                    | fail result garbled                |
%  | bsh                    | fail ESMF not implemented          |
%  | elbeweser_unstructured | works, results need checking       |
%  | elbeweser_locstream    | fail MOSSCO netcdf not implemented |
%  |                        |                                    |
% 


### History

The regrid coupler was added in ??
- regridding was partially enabled in March 2018

### Todo

- Configuration flags to modify the regridding method and other 
  parameters (.i.e. extrapolation, masking)
- Conservative regridding on unstructured Meshes
- 3D Grid creation from file (ESMF ticket 3613628)
- Dynamic masking
- Read and write weight files
