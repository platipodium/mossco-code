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

private
    !MODULE VARS
    character(len=ESMF_MAXSTR)      :: dbfile = "mossco.db"
    !@dev: atm file must be present in the EYXECUTING folder (i.e. in src/test)
    type(SQLITE_DATABASE)           :: db
    logical                         :: session_active=.false.
    logical                         :: con_active=.false.

    !@dev: shift all sql states as parameter to here



    public get_substance_list

contains

!----------------------------------------------------------------------
!------------------- Substance Tables Routines ------------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "get_substance_list"
subroutine get_substance_list(alias,listout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in) &
                                       :: alias
    !character(len=ESMF_MAXSTR),dimension(:),pointer,intent(out) &
    character(ESMF_MAXSTR) &
                                       :: listout

    !LOCAL VARS
    character(255)                     :: sql &
       = "SELECT Alias FROM tblnames WHERE tblnames.Alias='TN';"
!        = "SELECT ts.SubstanceName &
!        FROM (tblSubstances JOIN tblNames &
!        ON tblSubstances.ID=tblNames.Substance_ID) ts &
!        WHERE tblNames.Alias='~name';"
!        = "SELECT SubstanceName FROM tblSubstances WHERE SubstanceName='N';"

    character(len=ESMF_MAXSTR)         :: search_list="~name"

    type(SQLITE_STATEMENT)             :: stmt
    integer                            :: completion
    type(SQLITE_COLUMN), dimension(:), pointer &
                                       :: col
    character(ESMF_MAXSTR)             :: SubstanceName
    logical                            :: finished
    !------------------------------------------------------------------

    !Construct recordset for return values
    allocate( col(1) )
    call sqlite3_column_query( col(1), 'SubstanceName', SQLITE_CHAR, ESMF_MAXSTR )


!@dev: automation in sql_select_state
 !problem: connection closes in select state, recordset must be automatically built before closing the connection
!......................................................................
    !connect to database
    call sqlite3_open( 'mossco.db', db )

    if (.not. (search_list=="")) sql=Replace_String(sql,search_list,alias)

    !@temp
    write (*,*) sql

    call sqlite3_column_query( col(1), 'SubstanceName', SQLITE_CHAR, ESMF_MAXSTR )
    call sqlite3_prepare( db, sql, stmt, col )
    call sqlite3_step( stmt, completion )

    !@todo: Loop zum Schreiben aller Werte in den Array
    call sqlite3_next_row( stmt, col, finished )
    call sqlite3_get_column( col(1), SubstanceName)

    listout=SubstanceName

    call sqlite3_close( db )
!......................................................................

    !call sql_select_state(sql,"","",listout)

    !@todo: listout umwandeln in MOSSCO-ARRAY

end subroutine get_substance_list

!----------------------------------------------------------------------
!------------------- Basic SQL Routines -------------------------------
!----------------------------------------------------------------------

!Manage multiple commands in one transaction
!Commits pending transaction and starts new session
#undef  ESMF_METHOD
#define ESMF_METHOD "load_session"
subroutine load_session
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS

    !LOCAL VARS

    !------------------------------------------------------------------

    !Init connection to database file given by module
    if (con_active .eqv. .false.) then
        call sqlite3_open( dbfile, db )
        con_active=.true.
    end if

    !Execute previous session commands and reinit
    if (session_active .eqv. .true.) call finalize_session(.true.,.false.)
    session_active=.true.

    !@todo: start async timer to terminate connection

end subroutine load_session




#undef  ESMF_METHOD
#define ESMF_METHOD "finalize_session"
subroutine finalize_session(hold_con,abort)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    logical, intent(in), optional     :: hold_con,abort

    !LOCAL VARS
    logical                           :: hcon
    integer                           :: localrc
    !------------------------------------------------------------------

    hcon=hold_con

    !Catch wrong call, end session
    if ((session_active .neqv. .true.) .and. (con_active .neqv. .true.)) return
    session_active=.false.

    !Commit current changes             @todo: muss hier vorher geprüft werden ob etwas vorhanden ist?
    if (abort .eqv. .false.) call sqlite3_commit( db )

    !check external error flag / current errors and treat them
    if ((abort .eqv. .true.) .OR. (sqlite3_error( db ) .eqv. .true.)) then  !@todo: Kann es hier zu einem Fehler kommen, wenn noch nichts getan wurde?
        !@Error Undo changes made to database
        call sqlite3_rollback( db )
        hcon = .false.
        !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) @todo: Meldung aktivieren
        !@todo: @Frage: funktioniert der Aufruf so und läuft die Funktion danach weiter oder wird alles abgebrochen?
    end if

    !Quit connection and clear flag for regular shutdowns and errors
    if (hcon .eqv. .false.) then
        con_active = .false.
        call sqlite3_close( db )
    end if

end subroutine finalize_session

!----------------------------------------------------------------------
!------------------- IN DEV - BASIC SQL ROUTINES ----------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "sql_select_state"
subroutine sql_select_state(sql,search_list,replace_list,rsout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS / OUTPUTS
    character(len=ESMF_MAXSTR),intent(in),optional &
                                           :: search_list, replace_list !@todo: Als Arrays
    type(SQLITE_COLUMN),dimension(:),pointer &
                                           :: col
    character(255)                         :: sql
    character(len=ESMF_MAXSTR), dimension(:), intent(out) &
                                           :: rsout

    !LOCAL VARS
    !character(*),dimension(:),intent(in)  :: search_list
    !character(*),dimension(:),intent(in)  :: replace_list
    !@todo: als array
    type(SQLITE_STATEMENT)                 :: stmt
    logical                                :: err, finished
    integer                                :: i, completion
    !------------------------------------------------------------------

    !@temp
    return

    !Replace tags with values given by variables
    if (.not. (search_list=="")) sql=Replace_String(sql,search_list,replace_list)
    !@todo: dynamische Länge - dim(search_list)
!    do i=1,20
!        if (search_list(i)=="") exit !temp Lsg
!        sql=Replace_String(sql,search_list(i),replace_list(i))
!    end do

    !Init connection and start a new Transaction
    call load_session

    call sqlite3_prepare( db, sql, stmt, col )
    call sqlite3_step( stmt, completion )

    call finalize_session(.false.,(completion .ne. SQLITE_DONE))

    !@dev: auto-create fitting "column" for query

end subroutine sql_select_state


!----------------------------------------------------------------------
!------------------- GENERAL FUNCTIONS --------------------------------
!----------------------------------------------------------------------

!Part of http://fortranwiki.org/fortran/show/String_Functions
!Created on August 30, 2013 00:43:41 by Jason Blevins (174.101.45.6) (5815 characters / 2.0 pages)
FUNCTION Replace_String (s,text,rep)  RESULT(outs)
    CHARACTER(*),intent(in)         :: s,text,rep
    CHARACTER(LEN(s)+100)           :: outs     ! provide outs with extra 100 char len
    INTEGER                         :: i, nt, nr

    outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
    DO
       i = INDEX(outs,text(:nt)) ; IF (i == 0) EXIT
       outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
    END DO
END FUNCTION Replace_String

end module mossco_db





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