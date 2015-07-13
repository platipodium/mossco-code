!> @brief Implementation of ESMF database utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Nils Weiher <stu95021@uni-kiel.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "mossco_db.F90"

module mossco_db

!INITIALIZATION
use esmf
use sqlite

implicit none

!DECLARATIONS
private
    character(len=ESMF_MAXSTR)                  :: dbname = "mossco.db"
    type(SQLITE_DATABASE)                       :: db

    !Declare SQL States
    character(len=ESMF_MAXSTR), parameter :: sql_GetSubstanceNames &
        = "SELECT name FROM tblsubstances JOIN tblnames on &
        tblsubstances.id= tblnames.substance_id WHERE &
        tblnames.alias=~name;"

    character(len=ESMF_MAXSTR), parameter :: sql_StateName &
        = "SELECT ;"



    public get_substance_list

contains

!----------------------------------------------------------------------
!------------------- Substance Tables Routines ------------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "get_substance_list"
subroutine get_substance_list(alias) !Test-Case: alias = "TN"
    !INITIALIZATION
    implicit none

    !DECLARATIONS
    character(len=ESMF_MAXSTR), intent(in) :: alias
    character(len=ESMF_MAXSTR)             :: sql,Replace_String
    logical                                :: err

    !Receive names
    sql=Replace_String(sql_GetSubstanceNames,"~name",alias)
    !call sqlite3_do( db, sql )

    !err = sqlite3_error( db )
    !if (err == .true.) then call end_connection(.true.)

    call sqlite3_commit( db )

    !call end_connection(.false.)

end subroutine get_substance_list
!
!
!#undef  ESMF_METHOD
!#define ESMF_METHOD "start_connection"
!subroutine start_connection
!    !INITIALIZATION
!    implicit none
!
!    call sqlite3_open( "mossco.db", db )
!
!end subroutine start_connection
!
!
!#undef  ESMF_METHOD
!#define ESMF_METHOD "end_connection"
!subroutine end_connection(abort)
!    !INITIALIZATION
!    implicit none
!
!    logical, intent(in), optional     :: abort
!    if not (present(abort)) then abort = .false.
!
!    !@Error Undo changes made to database
!    if abort then call sqlite3_rollback( db )
!
!    !Quit connection
!    call sqlite3_close( db )
!
!    if abort then call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!end subroutine start_connection
!
!
!
!
!
end module mossco_db



!Part of http://fortranwiki.org/fortran/show/String_Functions
!Created on August 30, 2013 00:43:41 by Jason Blevins (174.101.45.6) (5815 characters / 2.0 pages)
FUNCTION Replace_String (s,text,rep)  RESULT(outs)
    CHARACTER(*)        :: s,text,rep
    CHARACTER(LEN(s)+100) :: outs     ! provide outs with extra 100 char len
    INTEGER             :: i, nt, nr

    outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
    DO
       i = INDEX(outs,text(:nt)) ; IF (i == 0) EXIT
       outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
    END DO
END FUNCTION Replace_String

!@todo: Umschreiben zu LoadSQL Funktion, die ein benanntes state läd und eine beliebige Anzahl ~VARS ersetzt



!---sqlite functions (http://flibs.sourceforge.net/fsqlite.html)

!Database:                              type(SQLITE_DATABASE)
!SQLite Command:                        type(SQLITE_STATEMENT)
!Variable type values:                  type(SQLITE_COLUMN)

!start transaction:                     call sqlite3_begin( db )
!close connection:                      call sqlite3_close( db )
!get error message:                     sqlite3_errmsg( db )
!commit all changes in transaction:     call sqlite3_commit( db )
!all changes in transaction:            call sqlite3_rollback( db )
!last command error (true/false):       sqlite3_error( db )

!run an sql command:                    call sqlite3_do( db, command )
!set select properties:                 call sqlite3_column_query( column, name, type, length, function )
!set value:                             call sqlite3_set_column( column, value )
!get value:                             call sqlite3_get_column( column, value )

!state for later execution:             call sqlite3_prepare_select( db, tablename, columns, stmt, extra_clause )
!state for later execution:             call sqlite3_prepare( db, command, stmt, columns )
!run prepared state:                    call sqlite3_step( stmt, completion )
!re-prepare state:                      call sqlite3_reset( stmt )
!release prepared state:                call sqlite3_finalize( stmt )
!next row of select state:              call sqlite3_next_row( stmt, columns, finished )

!query table structure:                 call sqlite3_query_table( db, tablename, columns )
!Define a new column of a table         call sqlite3_column_props( column, name, type, length )
!create a new table:                    call sqlite3_create_table( db )
!delete a table:                        call sqlite3_delete_table( db )
!insert row to table:                   call sqlite3_insert( db, tablename, columns )
