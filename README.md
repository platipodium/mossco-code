This is the main directory of MOSSCO, also called `$MOSSCO_DIR`

To *quickly start*, read the file `QuickStart.md` or `QuickStart.pdf`

# Why MOSSCO?

MOSSCO, the "Modular System for Shelves and Coasts" is a framework for coupling processes or domains that are originally developed in standalone numerical models.

The software MOSSCO implements this infrastructure in the form of a library of components and couplers, and of example coupled applications.  The components "wrap" external models used in coastal and shelf sciences, such as the Framework for Aquatic Biogeochemistry (FABM), the General Ocean Turbulence Model (GOTM), or the Delft3D erosion model (EROSED).  These wrapped components are then coupled to each other in the Earth System Modeling Framework (ESMF). 

MOSSCO is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License v3+.  MOSSCO is distributed in the  hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file  `doc/license/LICENSE.GPL` or http://www.gnu.org/licenses/gpl-3.0.txt for the full license terms.

# What is here? 

This main directory $MOSSCO_DIR contains three subdirectories
 
- `./src` for the library of drivers, components, and couplers.  You should not have to change this unless you want to develop your own components 
- `./doc` for documentation. You really should read this or the online documentation at http://www.mossco.de/doc
- `./examples` for example applications to be used as templates

## What is not here?

This directory produces only the libraries (from `./src`) and some example executables (from `./examples`).  It does *not* contain forcing files or parameters to run a scientifically usable simulation.   Please `git clone` the separate repository located at `http://git.code.sf.net/p/mossco/setups` into a directory of your choice (which you should point to with the environment variable `$MOSSCO_SETUPDIR`).


# The documentation
To create the documentation with full installation instructions, type

	make doc

This generates a pdf file in `$MOSSCO_DIR/doc/mossco_reference_manual.pdf` and a html version at `$MOSSCO_DIR/doc/reference_manual/html/index.html`. There is also a pre-built online documentation at `http://www.mossco.de/doc` with detailed installation instructions, if your system is missing latex/doxygen for building the documentation yourself.

# Installation

To make the MOSSCO libraries, i.e. drivers, components, and utilities, you need to have a compiled version of the Earth System Modeling Framework (ESMF), with known location of the `esmf.mk` Makefile pointed to by the variable `$ESMFMKFILE` 

Then, simply type

	make

To make some examples, type

	make examples

If you want to learn what you can do with MOSSCO examples, read the very short tutorial in the file `$MOSSCO_DIR/QuickStart.md`

Should you encounter errors or annoyances (such that this just does not work out of the box,  please consult the documentation, and visit the bugs database at http://www.mossco.de/bugs.
 
More components and examples are built when external models, like FABM, GOTM, GETM, or EROSED are installed  on your system.  Please consult the documentation for information on how to build MOSSCO with any of these external models.

# Running an example

In the folder `$MOSSCO_DIR/examples` several executable files have been built describing different coupled systems, see `QuickStart.md` to learn quickly about one example. 

The example executables should *not* be executed locally, but instead, you should download/create a set of setups and run the examples within a setup.  Again, see `QuickStart.md` or the full documentation for more information.
