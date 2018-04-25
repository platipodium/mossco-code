In this directory, you'll find a sample MOSSCO-coupled application that uses the 
SCHISM ESMF component in a coupled context.

You will need to first install and compile the following:
1) SCHISM trunk
2) ESMF
3) schism-esmf component wrapper
4) MOSSCO 

This directory should reside next to the schism/trunk and schism/esmf directories, e.g schism/mossco.

Then, set environment variables to point to these installations:

1) ESMFMKFILE to locate esmf.mk
2) SCHISM_DIR to point to the build of SCHISM
3) PARMETIS_DIR to point to the parmetis library directory
4) MOSSCO_DIR to point to the MOSSCO source/installation top level directory

