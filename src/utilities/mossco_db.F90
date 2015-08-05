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

    public get_substance_name

contains

!----------------------------------------------------------------------
!------------------- Substance Tables Routines ------------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "get_substance_name"
!> @brief
!> @param
subroutine get_substance_name(equivalent,rulesets,name)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in) &
                                :: equivalent
    character(len=ESMF_MAXSTR), dimension (:), intent(in) &
                                :: rulesets
    character(len=ESMF_MAXSTR), intent(out) &
                                :: name

    !LOCAL VARS
    character(1000)              :: sql &
        ="SELECT t.SubstanceName  FROM (tblEquivalents &
        JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID &
        JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID &
        JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID) t &
        WHERE tblRulesets.RulesetName='General' AND tblEquivalents.EquivalentName='oxygen';"

!    = "SELECT t.SubstanceName  FROM (tblEquivalents &
!    JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID &
!    JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID &
!    JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID) t &
!    WHERE tblRulesets.RulesetName='General' AND tblEquivalents.EquivalentName='~equivalent';"

    !> @todo: Implement as Vector (then include ruleset)
    character(len=ESMF_MAXSTR)  :: search_list="~equivalent", res

    type(SQLITE_STATEMENT)      :: stmt
    type(SQLITE_COLUMN), dimension(:), pointer &
                                :: col =>null()
    logical                     :: finished=.false.
    !------------------------------------------------------------------

    !Construct recordset for return values
    allocate( col(1) )
    call sqlite3_column_query( col(1), 'SubstanceName', SQLITE_CHAR, ESMF_MAXSTR )

    call sql_select_state(sql,search_list,equivalent,stmt,col)

    !@todo: Loop zum Schreiben aller Werte in den Array

!***@temp fix for test
    call sqlite3_next_row( stmt, col, finished )
    finished = .false.

    do while (finished .eqv. .false.)
        call sqlite3_next_row( stmt, col, finished )
        call sqlite3_get_column( col(1), name)
    end do

    call sqlite3_finalize( stmt )

    !@todo: listout umwandeln in MOSSCO-ARRAY

end subroutine get_substance_name



#undef  ESMF_METHOD
#define ESMF_METHOD "get_substance_list"
!> @subsubsection get_substance_list "Get Substance List"
!> @brief Receives list of all known substances from the database name
!> @detail list by unique identifier: Substance name
!> @param listout dim (:) char(ESMF_MAXSTR) Array with all Substance Names
subroutine get_substance_list(listout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
!***@temp
    character(len=ESMF_MAXSTR), dimension (:), intent(out) &
                                :: listout

    !LOCAL VARS

    !------------------------------------------------------------------

!***@temp
    listout(1) = "TN"

end subroutine get_substance_list



#undef  ESMF_METHOD
#define ESMF_METHOD "get_substance_alias_list"
!> @subsubsection get_substance_alias_list "Get Substance Alias List"
!> @brief Receives list of all alias for the substance name from the database
!> @param name char(ESMF_MAXSTR) Name or Alias of Substance
!> @param listout dim (:) char(ESMF_MAXSTR) Array with all aliases
subroutine get_substance_alias_list(name, listout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR)  :: name
    character(len=ESMF_MAXSTR), dimension (:), pointer, intent(out) &
                                :: listout

    !LOCAL VARS

    !------------------------------------------------------------------

    !> Check if name


!***@temp
    listout(1) = "TN"

end subroutine get_substance_alias_list



!----------------------------------------------------------------------
!------------------- Basic SQL Routines -------------------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "load_session"
!> @brief Manage multiple commands in one transaction
!> @details Commits pending transaction and starts new session
!> @param
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
!> @brief
!> @param
subroutine finalize_session(hold_con,abort)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    logical, intent(in), optional     :: hold_con,abort

    !LOCAL VARS
    logical                           :: hcon, critical
    integer                           :: localrc
    !------------------------------------------------------------------

    hcon=hold_con

    !Catch wrong call, end session
    if ((session_active .eqv. .false.) .and. (con_active .eqv. .false.)) return

    !> Commit current changes
    if (abort .eqv. .false.) call sqlite3_commit( db )

    !check external error flag / current errors and treat them
    if ((abort .eqv. .true.) .OR. (sqlite3_error( db ) .eqv. .true.)) then  !@todo: Kann es hier zu einem Fehler kommen, wenn noch nichts getan wurde?
        !> Undo changes made to database
        call sqlite3_rollback( db )
        hcon = .false.
        critical = .true.
    end if

    !> End session
    session_active=.false.

    !> Quit connection and clear flag for regular shutdowns and errors
    if (hcon .eqv. .false.) then
        con_active = .false.
        call sqlite3_close( db )
    end if

    !> On critical error run ESMF_END_ABORT routine after connection has been shut down
    !if (critical .eqv. .true.) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!***@temp: @todo: Meldung aktivieren!
end subroutine finalize_session



!----------------------------------------------------------------------
!------------------- BASIC SQL ROUTINES -------------------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "sql_select_state"
!> @brief
!> @param
subroutine sql_select_state(sql,search_list,replace_list,stmt,col)
    !------------------------------------------------------------------
    implicit none

    !INPUTS / OUTPUTS
    character(len=*)                         :: sql
!***@todo: Als Arrays
    character(len=ESMF_MAXSTR),intent(in),optional &
                                           :: search_list, replace_list
    type(SQLITE_STATEMENT), intent(out)    :: stmt
    !character(len=ESMF_MAXSTR), dimension(:), intent(out) &
!***@temp
    type(SQLITE_COLUMN),dimension(:),pointer, intent(out) &
                                           :: col =>null()

    character(len=ESMF_MAXSTR)             :: name

    !LOCAL VARS
    !character(*),dimension(:),intent(in)  :: search_list
    !character(*),dimension(:),intent(in)  :: replace_list
    !@todo: als array
    logical                                :: err, finished
    integer                                :: i, completion
    !------------------------------------------------------------------

!***@temp
    !return

    !Replace tags with values given by variables
    if (.not. (search_list=="")) sql=Replace_String(sql,search_list,replace_list)

!***@todo: dynamische Länge - dim(search_list)
!    do i=1,20
!        if (search_list(i)=="") exit !temp Lsg
!        sql=Replace_String(sql,search_list(i),replace_list(i))
!    end do

    !Init connection and start a new Transaction
    call load_session

    call sqlite3_prepare( db, sql, stmt, col )
    call sqlite3_step( stmt, completion )


!***@temp
    write(*,*) "--- database info ---"
    write (*,*) sql
    write(*,*) sqlite3_errmsg( db )
    write(*,*) "----------------------------"

    call finalize_session(.false.,(completion .ne. SQLITE_DONE))

!    write(*,*) name

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
