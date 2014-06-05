This directory contains patches that are necessary to external
software to run in the MOSSCO context.  Ideally, this directory
should be empty. Any patch indicates that the upstream external software
has not yet implemented the necessary change/bug requested by 
MOSSCO

# Erosed
In the parameterization for non-cohesive erosion, the roughness coefficient rksc
should be bounded within 1 % and 20 % of the height of the water column. As the
original erosed code has not vertical dimension, that is not a problem in the original
code but only when run within the 1d/3d MOSSCO context.  

The patch is erosed_vanRijn84.patch and should be applied as follows

	patch < $MOSSCO_DIR/src/patch/ersoed_vanRijn84.patch $EROSED_DIR/source/vanRijn84.f90
 


