


@section testcases Running the testcases

There are several examples that you can run, amongst them
- a FABM standalone sediment driver (in examples/standalone/omexdia_p)
- a FABM/ESMF sediment component (in examples/esmf_sediment)
- a GOTM/ESMF test case driver (in examples/esmf_gotm)
- an empty ESMF test case driver (in examples/esmf_empty)
- an erosed test case (in examples/standalone/erosed)
- a coupled A/O test case (in examples/esmf_remtc)
- a FABM0D / FABM sediment combined test case (non-functional yet)

@subsection testcases-fabm The FABM standalone sediment driver

All configuration files are contained in the example.  Run

~~~~
(cd $MOSSCO_DIR/examples/standalone/omexdia_p ; ./omexdia_p_test)
~~~~

@subsection testcases-esmf-sediment The ESMF/FABM sediment driver

All configuration files are contained in the example.  Run

~~~~
(cd $MOSSCO_DIR/examples/esmf_sediment ; ./esmf_sediment_example)
~~~~

@subsection testcases-gotm The GOTM testcase driver

You need to download the GOTM test cases first to a directory of your choice.  Let's assume your GOTM
test cases should be installed in the directory $GOTMCASEDIR, then run

~~~~
git clone git://git.code.sf.net/p/gotm/gotm-cases $GOTMCASEDIR
~~~~
  
To run the northern North Sea annual testcase, issue the following

~~~~
(cd $GOTMCASEDIR/nns_annual ; $MOSSCO_DIR/examples/esmf_gotm/esmf_gotm_example)
~~~~


@subsection testcases-fabm0d The FABM 0d driver

Prerequisites are both FABM and GOTM. FABM has to have the mossco driver compiled, in order to use MOSSCO's FABM-0d driver:

~~~~
make -C $FABMDIR/src mossco
~~~~
  
Then compile MOSSCO, go to $MOSSCO_DIR/examples/esmf_fabm0d and run the example:

~~~~
(cd $MOSSCO_DIR/examples/esmf_fabm0d ; ./esmf_fabm0d)
~~~~

All configuration files are contained in the example.

@subsection Delft3D erosion-sedimentation (erosed) test case

Prerequisite for this test case is ESMF.  You also need to obtain original Delft3D routines.  
Create a directory  $EROSED_DIR, and get up-to-date sources via subversion

~~~~sh
svn checkout https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example $EROSED_DIR
~~~~

Access to the Delft3D open source repository is restricted to registered users.  You can register at 
[oss.deltares.nl](http://oss.deltares.nl) (look at the top right)

After compilation of the example, you can run it in the local directory.  
