In this directory, you'll find a sample MOSSCO-coupled application that uses the 
SCHISM ESMF component in a coupled context.

You will need to first install and compile the following:
1) SCHISM trunk
3) schism-esmf component wrapper

Then, set environment variables to point to these installations:

1) SCHISM_DIR to point to the build of SCHISM
2) SCHISM_ESMF_DIR to the source of schism-esmf
3) PARMETIS_DIR to point to the parmetis library directory

Full instructions are available online at www.mossco.de/doc.  If you run into trouble with lapack, try
objcopy --redefine-sym xerbla_=schism_lap_xerbla_ $SCHISM_DIR/lib/libhydro.a



