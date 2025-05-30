<!--
SPDX-FileCopyrightText 2021-2022 Helmholtz-Zentrum Hereon
SPDX-FileCopyrightText 2013-2021 Helmholtz-Zentrum Geesthacht
SPDX-License-Identifier: CC0-1.0
SPDX-FileContributor Carsten Lemmen <carsten.lemmen@hereon.de
-->

@section what What is MOSSCO?

1. The Modular System for Shelves and Coasts.
2. A coupling infrastructure that facilitates the coupling of heterogeneous software components describing aspects of the coastal Earth System.
3. A project as well as a software system.

This user guide describes the software system MOSSCO.

@section can-use Can I use MOSSCO?

You can make good use of MOSSCO, if you are

1. A model developer or scientific computer engineer who would like to couple different models on high-performance computing architectures.
2. A scientific modeler with a specific model that should be integrated with other models.
3. An experimentalist who would like to make data available to the modeling community and have her data assimilated/used by computer models.

@section what-for What do we get funding for?

Goals and objectives from our funding proposal

@subsection goal Goal

> The overarching goal of the project Modular System for Shelves and Coasts (MOSSCO, <http://www.mossco.de>) is to bring together the distributed capacities in coastal ocean modelling in Germany and to build a common infrastructure for the national and international coastal research community.

@subection objectives Objectives

> 1. MOSSCO will build a framework for a modular data and model system, an infrastructure, which has been identified as research demand by the Forschungsagenda Nord- und Ostsee within framework of the Research Agenda for Sustainable Development (FONA, [www.fona.de](http://www.fona.de/ "Forschung für Nachhaltige Entwicklung")).

> 2. MOSSCO integrates physical, biological, chemical and geological modules in an exchangeable way. This modular infrastructure supports synoptic studies of coastal and shelf seas including their interfaces to adjacent Earth-system components, such as sediment, atmosphere, rivers and the open ocean. Benefits of this approach are most apparent for the description of cohesive sediments where strong interactions between the benthic and pelagic compartment and between hydrodynamic, biogeochemical (BGC), ecological and sedimentary processes are largely unresolved within state-of-the-art models.

> 3. MOSSCO will be applied to cohesive sediment dynamics and transport problems on two spatial coastal scales: In an application to the entire German North Sea and Baltic Sea, we will demonstrate the modular concept of MOSSCO, study sediment fluxes along the coast, and support other FONA projects. In a study of two estuaries, we will focus on the impact of maintenance dredging and channel deepening on water quality and ecosystem services, including potential effects of climate change. Using a webGIS frontend of the modular system, the estuarine results will be evaluated for their compliance with coastal policies.
# Acknowledgements

- The MOSSCO project is funded primarly by the German Ministry for Education and Research (BMBF) in the frame of its Coastal research agenda North and Baltic Sea (KüNO) within the Research Agenda for Sustainability (FONA).

- The participating institutes---HZG, IOW, and BAW---all contribute with institutional funding to the success of MOSSCO.

- The Friends of Helmholtz-Zentrum Geesthacht helped finance the Modular Coupling Workshop held in 2013.

- The MOSSCO web site and development platform is hosted on Sourceforge (<http://sf.net>) for free.  We thank the sponsors of this site for enabling this service to operate.

- MOSSCO makes use of many free and open source software projects, among  them the GNU project (<http://www.gnu.org>) and the Earth System Modeling Framework (<http://www.earthsystemmodeling.org>), the Python scripting language (<https://www.python.org>) and the Inkscape vector drawing program (<https://inkscape.org>).

- In particular we express our thanks to the developers of the various models integrated into MOSSCO. Without their continuous effort to provide state-of-the-art model code to the community, the MOSSCO project would be impossible. Currently MOSSCO integrates:
  - the Framework for Aquatic Biogeochemical Models (<http://sf.net/projects/fabm>),
  - the General Ocean Turbulence Model (<http://www.gotm.net>),
  - the General Estuarine Transport Model (<http://www.getm.eu>),
  - the SandMudBedModule from the OpenEarthTools (<http://www.openearth.nl>).

MOSSCO is mainly developed on Ubuntu Linux workstations.

# Obtaining and installing MOSSCO

MOSSCO is distributed *only* as source code. There is no binary package available that we distribute.

@section obtain How to obtain the source code

While the public web site with documentation on the MOSSCO project is hosted at <http://www.mossco.de>, the software, blog, and bug tracker is hosted on <http://sf.net/p/mossco>.  To obtain the code, change to the `git` tab at that site, or follow this link <http://www.sf.net/p/mossco/code>.

The web site interface could look as follows (depending on your browser):

![Screenshot of using sourceforge](../../img/sf_git_screenshot.png "Screenshot of using sourceforge")

Choose a local directory `$MOSSCO_DIR`, where you would like the mossco code to
reside;  you must declare this as an environment variable, as our build system
depends on the correct setting of `$MOSSCO_DIR`.

		export MOSSCO_DIR=$HOME/some/local/directory # for bash users
		setenv MOSSCO_DIR $HOME/some/local/directory # for csh users

The code is available via `git` for read-only access. If you don't have  `git`,
you can download and install a suitable version from <http://git-scm.com>.

	git clone git://git.code.sf.net/p/mossco/code $MOSSCO_DIR

or, alternatively using the `http` protocol

	git clone http://git.code.sf.net/p/mossco/code $MOSSCO_DIR

If you are a developer and registered as a participant of the MOSSCO project,
you may use a modified version of this command to gain write access. Please
contact us to become a member of the project.

> Note: If you do not have  `git`, you may also download a zipped version of the
> source code from the `Files` section at <https://sf.net/p/mossco/files/>. Note,
> however, that these snapshots may be outdated at the time of your download.
> Download the file to the directory where `$MOSSCO_DIR` points to, then unzip it.

@section building-mossco Building and installing MOSSCO

@subsection prereqs MOSSCO prerequisites

MOSSCO has some necessary and some optional prerequisites.  Please make sure you
meet these prerequisites before you try to compile MOSSCO.

@subsubsection system-prereqs Necessary system prerequisites

| Program               | Minimum version and alternatives          |
| --------------------- | ----------------------------------------- |
| GNU `make`            | >= 3.0                                    |
| Fortran 2003 Compiler | GCC  >= 4.8.2, Intel >= 12.0, PGI >= 13.2 |
| ESMF                  | >= 7.1.0r                                 |
| NetCDF                | >= 3.6, preferably version 4              |
| Python YAML           | exactly 2.7                               |

> We would like to use Python 3, if you have some experience with that, please
> feel welcome to contribute

### Optional system prerequisites

| Program                   | Minimum version and alternatives           |
| ------------------------- | ------------------------------------------ |
| Message Passing Interface | OpenMPI >= 1.7 or MPICH >= 3.0.4 or others, IntelMPI |

We recommend strongly that you delegate the installation of ESMF (including
Fortran compiler, MPI, NetCDF) to your local tech support or IT department. Other
versions of the necessary and optional required programs may also work, but have
not necessarily been tested.

You may also try to go ahead with an existing lower version of any of the required
software packages, report possible errors, and argue why you would like us to
support your version.

For help and hints on installing ESMF, MPI, and NetCDF, see the appropriate
sections below.  Please make sure to use the same compiler for all your software
components.

1. Choose a FORTRAN compiler
2. Compile MPI for this specific compiler
3. Compile NetCDF libraries with this specific compiler and MPI support
4. Compile ESMF with matching NetCDF and MPI library settings

### Optional external model installations

| Program | Minimum version and alternatives |
| ------- | -------------------------------- |
| GOTM    | > 13 August 2014                 |
| FABM    | > 13 August 2014                 |
| GETM    | > 13 August 2014                 |

> GOTM's interface changed on 13 August 2014.  If you use an earlier version of
> GOTM, the 0D driver will not compile.  FABM development is usually synchronized
> with GOTM, so make sure you use versions of similar date.

### Documentation prerequisites

| Program | Minimum version and alternatives |
| ------- | -------------------------------- |
| Doxygen | >= 1.8                           |
| Latex   | with pdf latex support           |

There is a pre-built documentation available online at <http://www.mossco.de/doc>

## Installing MPI and NetCDF

Quite likely, a version MPI and NetCDF are preinstalled on your system.  If not,
it is preferable to use your package manager (Yast, zypper, apt, port, etc. ...)
to install these programs.  Make sure that both MPI and NetCDF are compiled with
the *same* Fortran 2003 compiler that you would like to use for ESMF and MOSSCO
subsequently. Often, this is not the case (unless you're using a very recent
operating system), so please check carefully.  We experienced a lot of
difficulties and bugs with outdated compilers, and mismatches between the compiler
(and respective libraries) used for the different requirements for MOSSCO. You
may also consult the bug data base <http://www.mossco.de/bugs> for solving these problems.

For ESMF, you need the NetCDF C library.  For GOTM and the NetCDF data components
in MOSSCO, you need to install the NetCDF Fortran library (if using NetCDF > 4.1.3).

## Installing ESMF

Decide on a directory where to put the ESMF source, and set the environment variable
`$ESMF_DIR` to point to this directory.

1. download the `git` repository of esmf

		git clone git://git.code.sf.net/p/esmf/esmf $ESMF_DIR

2. change to this directory

		cd $ESMF_DIR

3. checkout the ESMF_7_1_0r tagged version

		git checkout ESMF_7_1_0r

4. set ESMF's environment variables (these can be a lot, see below)

An example environment for ESMF could look like this:

| Environment variable | value              |
| -------------------- | ------------------ |
| ESMF_DIR             | $HOME/opt/src/esmf |
| ESMF_COMM            | mpich2             |
| ESMF_NETCDF          | split              |
| ESMF_NETCDF_INCLUDE  | /opt/local/include |
| ESMF_NETCDF_LIBPATH  | /opt/local/lib     |
| ESMF_LAPACK          | netlib             |
| ESMF_PIO             | internal           |

See the ESMF manual for a complete list of ESMF environment variables and their
relevance. On our own testing systems, we regulary run with `ESMF_COMPILER` set
to `intel`, `gfortran`, or `gfortranclang` and with ESMF_COMM set to `intelmpi`, `openmpi`,
and `mpich`.

Build and install ESMF by issuing  `make` followed by `make check` and `make install`.
For MOSSCO and the ESMF tools to work, you need to define the environment variable
`$ESMFMKFKILE`, e.g.

| Environment variable | value                                                                |
| -------------------- | -------------------------------------------------------------------- |
| ESMFMKFILE           | $HOME/opt/src/esmf/lib/libg/Linux.gfortran.64.mpich2.default/esmf.mk |

## FABM, GOTM, GETM

MOSSCO takes care of obtaining a current FABM, GOTM, and GETM.  Go to `$MOSSCO_DIR`
and execute

	make external

This will download FABM, GETM, and GOTM to your system in subdirectories of
`$MOSSCO_DIR/external`.

Alternatively, you can set environment variables `$FABMDIR`, `$GOTMDIR`, and
`$GETMDIR` and then clone the respective `git` repositories

	git clone git://git.code.sf.net/p/fabm/code $FABMDIR
  git clone git://git.code.sf.net/p/gotm/code $GOTMDIR
  git clone git://git.code.sf.net/p/getm/code $GETMDIR

### Building FABM, GOTM, GETM within MOSSCO

MOSSCO takes care of the FABM and GOTM builts via `cmake` and of the GETM built
via configure/make/install, if they are installed in `$MOSSCO_DIR/external` or
if their location has been defined by environment variables pointing to the
respective installation.

### Building FABM, GOTM, GETM outside of MOSSCO

Alternatively, you can build FABM, GOTM and GETM yourself by specifying the
following environment variables (adapt to your local system):

| Environment variable | value      |
| -------------------- | ---------- |
| FORTRAN_COMPILER     | GFORTRAN   |
| NETCDFHOME           | /opt/local |
| NETCDF4              | true       |
| NETCDF_VERSION       | NETCDF4    |

Then change to the respective directories and issue `make`

  (cd $GOTMDIR/src ; make distclean all)
  (cd $GOTMDIR/src ; make distclean all)
	(cd $GETMDIR/src ; make distclean all)

Help MOSSCO to see these external software packages by specifiying the following
environment variables:

| Environment variable | value    |
| -------------------- | -------- |
| MOSSCO_FABMDIR       | $FABMDIR |
| MOSSCO_GOTMDIR       | $GOTMDIR |
| MOSSCO_GETMDIR       | $GETMDIR |

## Building MOSSCO

To create the MOSSCO libraries,  simply issue `make` in your  `$MOSSCO_DIR`.
This will automaticall build the src target and provide the libraries in
`$MOSSCO_DIR/lib/$FORTRAN_COMPILER`

	cd $MOSSCO_DIR ; make

To create the documentation (the file you're reading right now), issue

	make doc

(don't worry too much about the warning messages that occur with outdated `doxygen`
version.  Also, some of the heading and table markup may not render correctly with
old doxygen versions). If you do not have `doxygen` installed, you can alternatively
consult the fairly recent online documentation at <http://www.mossco.de/doc>, or
download a - probably outdated -  pdf of the documentation from
<https://sf.net/p/mossco/files/Reference%20Manual/>.

To create the examples and test your installation, issue

	make examples

Then change to the examples subdirectories and run the testcases (see later
sections below)

To clean everything and start anew, type

	make mossco_clean

## Summary settings

It may be convenient for you to assemble all variable settings in a shell resource
file or add variable settings to your shell's login commands.  Only very few
environment variables should be set after your first installation.

Sample setting for `bash` could look as follows

        export MOSSCO_DIR=${HOME}/opt/mossco/code
        export NETCDF_VERSION=NETCDF4
        export ESMFMKFILE=/opt/esmf/lib/libO/Linux.gfortran.64.openmpi.ESMF_7_1_0r/esmf.mk

For `csh` you would, in a similar fashion, say

        setenv MOSSCO_DIR ${HOME}/opt/mossco/code
        setenv NETCDF_VERSION NETCDF4
        setenv ESMFMKFILE /opt/esmf/lib/libO/Linux.gfortran.64.openmpi.ESMF_7_1_0r/esmf.mk

# Reporting errors

Bugs as well as annoyances and feature requests are collected in a bug tracker
located at <https://sf.net/p/mossco/tickets/>.  Please search for an existing
ticket before reporting a new one.

Do report any issue that you observe, even if this later turns out to be a problem
related to your local computing environment and not to the MOSSCO code itself.
We encourage to document any problem that you encountered during the installation
of MOSSCO: someone else might have the same issue and could profit from your experience.

You are also welcome to fix errors yourself, commit them in your local repository,
and upload the patch to the bug tracker.  If you would like to get involved more,
you are welcome to join our team.  Please contact us via the sourceforge project
web site.

# MOSSCO status

The MOSSCO system was successfully tested on the following machines and environments.
Please report successful testing on your machine (e.g., in the project wiki
<http://www.mossco.de/wiki>), and issue a new tag, if you are a developer.

## Successful operation

| Operating System            | Compiler               | Machine               |
| --------------------------- | ---------------------- | --------------------- |
| Ubuntu/artful x86-64 12 CPU | clang/gfortran/openmpi | ksez8002 (HZG, cl)    |
| RedHat Linux Xeon    48 CPU | intel/parastation      | jureca (FZJ, cl)      |
| Ubuntu Linux 3.8.0          | gcc/gfortran/mpich2    | phy-203 (IOW, kk)     |
| SUSE Linux 2.6.16           | ifort-12.1             | bicegate (HLRN, kk)   |
| SUSE Linux 3.0              | ifort-12.1.5           | ICE-X (BAW, hm)       |
| Debian Linux 7.1            | gfortran/mpich2        | grpsrv09 (HZG, hk)    |
| macOS 10.9                  | gfortran/mpich2        | possum (HZG, cl)      |
| macOS 10.13                 | clang/gfortran/openmpi | sugarglider (HZG, cl) |
| CentOS Linux AMD 16 CPU     | intel/openmpi          | ocean (HZG, cl)       |

We are working on testing the pgfortran compiler on different systems.

@section references Model system references

- Lemmen, C., Hofmeister, R., Klingbeil, K., Nasermoaddeli, M.H., Kerimoglu, O., Burchard, H., Kösters, F., Wirtz, K.W., 2018. Modular System for Shelves and Coasts (MOSSCO v1.0) – a flexible and multi-component framework for coupled coastal ocean ecosystem modelling. Geosci. Model Dev. 11, 915–935. doi:10.5194/gmd-11-915-2018

- Lemmen, C. et al. (2013): "Das Modulare System für Schelfmeere und Küsten (MOSSCO) - Konzepte und Infrastruktur zum Zusammenwirken verschiedener Modelle für die Küstenforschung", in KFKI Newsletter 2013 (2), edited by R. Lehfeldt, Kuratorium für Forschung im Küsteningenieurwesen, available online http://www.kfki.de/files/kfki-aktuell/0/13-2-DE.pdf

- Wirtz, K.W. et al. (2012): Modular System for Shelves and Coasts (MOSSCO), Proposal for a project in response to the BMBF call Küstenmeerforschung in Nord- und Ostsee in the framework of Forschung für nachhaltige Entwicklungen (FONA).  Available online http://www.mossco.de/Leitantrag_MOSSCO.pdf.

- von Bodungen, B. et al. (2011): Küstenforschungsagenda für Nord- und Ostsee Im Rahmen des BMBF-Programms „Forschungen für nachhaltige Entwicklungen,   Projektträger Jülich (PTJ). Available online http://www.ptj.de/lw_resource/datapool/_items/item_3404/kstenforschung02122011_internet.pdf

- von Bodungen, B. et al. (2007): Küstenmeere im Wandel: Forschungsbedarf der deutschen Küsten- und Randmeerforschung, Konsortium Deutsche Meeresforschung (KDM). Available online http://www.deutsche-meeresforschung.de/docs/KDM_Kuestenschrift_3.pdf (English translation at http://www.deutsche-meeresforschung.de/docs/KDM_Coastal_Seas_Memorandum.pdf)

@subsection use-references Publications that use the infrastructure

- Slavik, K., Lemmen, C., Zhang, W., Kerimoglu, O., Klingbeil, K., Wirtz, K.W., 2018. The large scale impact of offshore windfarm structures on pelagic primary production in the southern North Sea. Hydrobiologia submitted.

- Nasermoaddeli, M.H., Lemmen, C., Kösters, F., Stigge, G., Kerimoglu, O., Burchard, H., Klingbeil, K., Hofmeister, R., Kreus, M., Wirtz, K.W., 2017. A model study on the large-scale effect of macrofauna on the suspended sediment concentration in a shallow shelf sea. Estuar. Coast. Shelf Sci. in press. doi:10.1016/j.ecss.2017.11.002

# MOSSCO modular structure

MOSSCO itself is structured in a modular fashion, with usually three levels in a hierarchy.  These hierarchies are **drivers**, **components**, and **examples**.

@image latex mossco_levels.png "Hierarchy of examples, components, drivers, and external models" width=.6\hsize
@image html mossco_levels.png "Hierarchy of examples, components, drivers, and external models" width=.6\hsize

@subsection structure-driver Driver
At the driver level, MOSSCO includes code that is *specific to an external model*.  The driver level code serves as an interface level between the external model and the component. Typically

- a driver *uses* modules from an external model
- does *not use esmf*
- replaces or complements external model codes
- is hand-coded

@subsection structure-component Component
At the component level, MOSSCO includes code that exhibits the data from external models wrapped in an ESMF component.  The component level code is a major product of MOSSCO, and should be integrateable into other ESMF coupled codes.

- a component *does not use* modules from an external model
- a component *uses* a driver
- is a fully specified ESMF component, i.e. contains SetServices, Initialize, Finalize, and Run routines
- handles an externally provided clock
- creates fields on which the external model data is represented
- is hand-coded or automatically processed (with the `scripts/create_component.py` script)

@subsection structure-example Example
At the example level, MOSSCO provides examples of ESMF coupled modular systems.  The example level code is a major product of MOSSCO, providing both scientifically exploitable coupled systems, as well as template code for user defined couplings.

- an example represents a complete compile-time coupled system
- results in a application, that is executed in a *setup* (see below)
- automatically processed (with the `examples/generic/create_coupling.py` script), or, for reference, hand-coded.

 @section coupling-strategy Coupling strategy

[This section is experimental and a Request For Comments] Analogous to FABM, all components to MOSSCO that are available at compile time will be compiled and aggregated in [a single, multiple?] library in  $MOSSCO_DIR/lib/$FORTRAN_COMPILER.  At run-time, only those components that are used, are executed [this calls for dynamic loadable libraries].  The coupling itself is described in a text file, with a defined YAML structure [also add namelist, SiSi, CDL as allowed meta specifications].  A couping could be described as follows

~~~~.yaml
coupling:
  - components: [GOTM FABM0d]
    interval: 40 min
  - components: [GOTM FABMSED]
    interval: 60 min
  - components: [FABM0D GOTM]
    interval: 90 min
    direction: forward
  - components: [FABMSED FABM0d]
    interval: 60 min
    direction: both
~~~~

This example describes a system of 3 coupled components [find a shorter way to specify this?] and the data exchange intervals between these components. Within the coupling structure, a pair of components is specified with an update interval of data exchange and (optionally) the direction of coupling.  Direction can be any of forward, backward, and both, the default value is forward coupling.

In this example, the GOTM component provides data to FABM0D at 40 min intervals and to FABMSED at 60 min intervals.  FABMSED provides data to FABM0D at 60 min intervals, and FABM0D provides data to GOTM at 90 minutes, and to FABMSED at 60 minutes intervals (note the direction "both" argument).

During initialize, the components add alarms with the specified interval to the parent clock [demonstrate that this works], each alarm obtains as attributes the meta-information about each two-way coupling.

The parent clock in the top level component, which calls FABM0D, GOTM and FABMSED repeatedly in its  Run() routine, examines its alarms for those next ringing ones and calls each component with a time duration until the next alarm suitable for that component. It then advance its own clock to the next alarm time, exchanges data, and calls the Run() routines of the two components whos alarm had triggered with a time duration until the next alarm related to each component.

In our example above.
1) GOTM and FABM0D are run for 40 mins, FABMSED for 60 mins
2) at t=40 GOTM gives data to FABM0D, GOTM and FABM0D are run for another 20 minutes
3) at t=60 GOTM and FABM0D give data to FABMSED, FABMSED gives data to FABM0D; GOTM and FABM0D are run for 20 mins, FABMSED for 60 mins.
4) at t=80 GOTM gives data to FABM0D, GOTM and FABM0D  are run for 10 mins
5) at t=90 FABM0D gives data to GOTM, both are run for another 30 minutes
6) at t=120, steps (1) and (2) are repeated.

@subsection coupling-strategy-time Timers and alarms



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
