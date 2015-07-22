# Recipe 14: integration of SQLITE databases

For physical quantity and chemical stoichiometry matching, MOSSCO integrates
a database of typical quantities and their relations.  This feature is used
in the new `soil_pelagic_mediator.F90`, that in turn depends on the `mossco_db.F90`.

This Recipe explains how to install the database.

## System installation of `libsqlite3`

Your system may already have the development files installed, or may have a facility
to easily install them through a package manager.  If not, please proceed to the next section.

### Ubuntu

Install the `libsqlite3-dev` package through your package manager, e.g.

    sudo apt-get install libsqlite3-dev

Verify that your have the installed header file and library file

    sudo updatedb
    locate libsqlite3.a sqlite3.h

### OSX

Install the `sqlite3` package thorugh your package manager, e.g.

    sudo port install sqlite3

Verify that your have the installed header file and library file

        mdfind -name sqlite3.h

## Local library

MOSSCO provides a local copy of `sqlite`.

    make external
    make info

`MOSSCO_SQLITE` should be true.

## Compiling the library

    make -C $MOSSCO_DIR/src/utilities mossco_db.o

Please report problems in the bug tracker http://www.mossco.de/bugs
