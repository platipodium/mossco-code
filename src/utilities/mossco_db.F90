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
    logical                         :: DEBUG = .false.

    !@dev: shift all sql states as parameter to here

    public get_equivalent_name, &
           get_alias_name, &
           get_substances_list, &
           get_substance_aliases_list, &
           get_substance_appendices_list, &
           get_substance_appendix_aliases_list

contains

!----------------------------------------------------------------------
!------------------- Substance Tables Routines ------------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "get_equivalent_name"
!> @brief
!> @param
subroutine get_equivalent_name(equivalent,rulesets,nameout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in), pointer  :: equivalent
    character(len=ESMF_MAXSTR), intent(in), pointer  :: rulesets
    character(len=ESMF_MAXSTR), intent(out),pointer  :: nameout

    !LOCAL VARS
    character(len=ESMF_MAXSTR), dimension(2)         :: search_list, &
                                                        replace_list

    character(1000)                                  :: sql

    integer                                          :: columns = 1

    !LOCAL POINTER/ALLOCS
    type(SQLITE_COLUMN), dimension(:), allocatable, target :: col

    character(len=ESMF_MAXSTR),dimension(:,:),pointer:: dba
    !------------------------------------------------------------------
    nameout=>null()
    sql = "SELECT t.SubstanceName  FROM (tblEquivalents &
        JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID &
        JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID &
        JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID) t &
        WHERE tblRulesets.RulesetName IN (~rulesets) AND tblEquivalents.EquivalentName='~equivalent';"

    search_list = [character(len=ESMF_MAXSTR) :: "~rulesets", "~equivalent"]
    replace_list = [character(len=ESMF_MAXSTR) :: rulesets, equivalent]

    !Construct recordset for return values
    allocate( col(columns) )
    call sqlite3_column_query( col(1), 'SubstanceName', SQLITE_CHAR, ESMF_MAXSTR )

    call sql_select_state(sql,col,1,search_list,replace_list,dba)

    if (associated(dba)) nameout=>dba(1,1)
    !write(*,*) nameout
    !if (associated(dba)) write(*,*) "yes"
    !write(*,*) dba(1,1)

    deallocate(col)

end subroutine get_equivalent_name

#undef  ESMF_METHOD
#define ESMF_METHOD "get_alias_name"
!> @brief
!> @param
subroutine get_alias_name(alias,rulesets,nameout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in), pointer  :: alias
    character(len=ESMF_MAXSTR), intent(in), pointer  :: rulesets
    character(len=ESMF_MAXSTR), intent(out),pointer  :: nameout

    !LOCAL VARS
    character(len=ESMF_MAXSTR), dimension(2)         :: search_list, &
                                                        replace_list
    character(len=ESMF_MAXSTR), target               :: empty = ""

    character(1000)                                  :: sql

    integer                                          :: columns = 1

    !LOCAL POINTER/ALLOCS
    type(SQLITE_COLUMN), dimension(:), allocatable, target :: col

    character(len=ESMF_MAXSTR),dimension(:,:),pointer:: dba
    !------------------------------------------------------------------
    nameout=>null()
    sql = "SELECT t.SubstanceName || coalesce(t.Condition,"") || coalesce(t.Location,"") &
            FROM (tblAppendix &
            JOIN tblSubstances ON tblAppendix.Substance_ID=tblSubstances.ID &
            JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblSubstances.ID &
            JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID &
            JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t &
            WHERE tblRulesets.RulesetName in ('~rulesets') &
            AND t.EquivalentName || coalesce(t.Condition,"") || coalesce(t.Location,"") == '~alias';"

    search_list = [character(len=ESMF_MAXSTR) :: "~rulesets", "~alias"]
    replace_list = [character(len=ESMF_MAXSTR) :: rulesets, alias]

    !Construct recordset for return values
    allocate( col(columns) )
    call sqlite3_column_query( col(1), 'Name', SQLITE_CHAR, ESMF_MAXSTR )

    call sql_select_state(sql,col,1,search_list,replace_list,dba)
    if (associated(dba)) nameout=>dba(1,1)

    deallocate(col)

end subroutine get_alias_name


#undef  ESMF_METHOD
#define ESMF_METHOD "get_substances_list"
!> @subsubsection get_substance_list "Get Substance List"
!> @brief Receives list of all known substances from the database name
!> @detail list by unique identifier: Substance name
!> @param dbaout: Array with all Substance Names
subroutine get_substances_list(dbaout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR),dimension(:,:),pointer,intent(out) &
                                                     :: dbaout
    !LOCAL VARS
    integer                                          :: columns = 1
    type(SQLITE_COLUMN), dimension(:), pointer       :: col =>null()

    character(1000)                                  :: sql

    !------------------------------------------------------------------
    dbaout=>null()
    sql = "SELECT SubstanceName FROM tblSubstances;"
    !Construct recordset for return values
    allocate( col(columns) )
    call sqlite3_column_query( col(1), 'SubstanceName', SQLITE_CHAR, ESMF_MAXSTR )

    call sql_select_state(sql,col,1,dba=dbaout)

    deallocate(col)

end subroutine get_substances_list



#undef  ESMF_METHOD
#define ESMF_METHOD "get_substance_aliases_list"
!> @subsubsection get_substance_alias_list "Get Substance Alias List"
!> @brief Receives list of all aliases of the substance from the database
!> @param name char(ESMF_MAXSTR) Name or Alias of Substance
!> @param listout dim (:) char(ESMF_MAXSTR) Array with all aliases
subroutine get_substance_aliases_list(name, rulesets, dbaout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in)           :: name
    character(len=ESMF_MAXSTR), intent(in), pointer  :: rulesets
    character(len=ESMF_MAXSTR),dimension(:,:),pointer,intent(out) &
                                                     :: dbaout

    !LOCAL VARS
    integer                                          :: columns = 1
    type(SQLITE_COLUMN), dimension(:), pointer       :: col =>null()
    character(len=ESMF_MAXSTR), dimension(2)         :: search_list, &
                                                        replace_list

    character(1000)                                  :: sql
    !------------------------------------------------------------------
    dbaout=>null()
    sql = "SELECT t.EquivalentName || coalesce(t.Condition,'') || coalesce(t.Location,'') &
        FROM (tblAppendix &
        JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblAppendix.Substance_ID &
        JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID &
        JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID &
        JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t &
        WHERE tblRulesets.RulesetName IN(~rulesets) AND tblSubstances.SubstanceName='~name';"

    search_list  = [character(len=ESMF_MAXSTR) :: "~rulesets", "~name"]
    replace_list = [character(len=ESMF_MAXSTR) :: rulesets, name]

    !Construct recordset for return values
    allocate( col(columns) )
    !call sqlite3_column_query( col(1), 'Substance-Appendix', SQLITE_CHAR, ESMF_MAXSTR )
    call sqlite3_column_query( col(1), 'Equivalent-Appendix', SQLITE_CHAR, ESMF_MAXSTR )

    call sql_select_state(sql,col,1,search_list,replace_list,dbaout)

    deallocate(col)

    sql=""

end subroutine get_substance_aliases_list


#undef  ESMF_METHOD
#define ESMF_METHOD "get_substance_appendices_list"
!> @subsubsection get_substance_appendices_list "Get Substance Appendices List"
!> @brief Receives list of all known appendices-IDs for a substance
!> @detail list by unique identifier: Substance name
!> @param name: Array with all Substance Names
!> @param dbaout: Array with all Substance Names
subroutine get_substance_appendices_list(name, dbaout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in), pointer  :: name
    character(len=ESMF_MAXSTR),dimension(:,:),pointer,intent(out) &
                                                     :: dbaout

    !LOCAL VARS
    integer                                          :: columns = 1
    type(SQLITE_COLUMN), dimension(:), pointer       :: col =>null()
    character(len=ESMF_MAXSTR), dimension(1)         :: search_list, &
                                                        replace_list

    character(1000)                                  :: sql
    !------------------------------------------------------------------

    sql = "SELECT DISTINCT tblAppendix.ID &
            FROM (tblAppendix &
            JOIN tblSubstances ON tblSubstances.ID=tblAppendix.Substance_ID) t &
            WHERE tblSubstances.SubstanceName='~name';"   

    search_list = [character(len=ESMF_MAXSTR) :: "~name"]
    replace_list = [character(len=ESMF_MAXSTR) :: name]

    !Construct recordset for return values
    allocate( col(columns) )
    call sqlite3_column_query( col(1), 'Appendix ID', SQLITE_CHAR, ESMF_MAXSTR )

    call sql_select_state(sql,col,1,search_list,replace_list,dbaout)

    deallocate(col)

end subroutine get_substance_appendices_list



#undef  ESMF_METHOD
#define ESMF_METHOD "get_substance_appendix_aliases_list"
!> @subsubsection get_substance_appendix_aliases_list "Get Substance Appendix-Aliases List"
!> @brief Receives list of all aliases of the substance connected with one appendix
!> @param
subroutine get_substance_appendix_aliases_list(SubstanceName, apdxID, rulesets, dbaout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in)           :: SubstanceName
    character(len=*), intent(in)                     :: rulesets, apdxID
    character(len=ESMF_MAXSTR),dimension(:,:),pointer,intent(out) &
                                                     :: dbaout


    !LOCAL VARS
    integer                                          :: columns = 1
    type(SQLITE_COLUMN), dimension(:), pointer       :: col =>null()
    character(len=ESMF_MAXSTR), dimension(3)         :: search_list, &
                                                        replace_list

    character(1000)                                  :: sql

    !------------------------------------------------------------------
    sql = "SELECT DISTINCT t.EquivalentName || coalesce(t.Condition,'') || coalesce(t.Location,'') &
            FROM (tblAppendix &
            JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblAppendix.Substance_ID &
            JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID &
            JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID &
            JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t &
            WHERE tblRulesets.RulesetName IN(~rulesets) &
            AND tblSubstances.SubstanceName='~name' &
            AND tblAppendix.ID=~apdxID; "   

    search_list = [character(len=ESMF_MAXSTR) :: "~rulesets", "~name", "~apdxID"]
    replace_list = [character(len=ESMF_MAXSTR) :: rulesets, SubstanceName, apdxID]

    !Construct recordset for return values
    allocate( col(columns) )
    call sqlite3_column_query( col(1), 'Substance aliases', SQLITE_CHAR, ESMF_MAXSTR )

    call sql_select_state(sql,col,1,search_list,replace_list,dbaout)

    deallocate(col)

end subroutine get_substance_appendix_aliases_list



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
!> @brief Runs a sql select state and returns the result
!> @param sql: SQL Command ready to roll
!> @param columns: number of columns in resulting array
!> @param search_list,replace_list: Replaces values from sl with rl
!> @param dba: allocatable array, returns results of select state
subroutine sql_select_state(sql,col,columns,search_list,replace_list,dba)
    !------------------------------------------------------------------
    implicit none

    !INPUTS / OUTPUTS
    character(len=*)                                    :: sql
    type(SQLITE_COLUMN),dimension(:),pointer,intent(in) :: col

    character(len=*),dimension(:),optional,intent(in)   :: search_list, &
                                                           replace_list
    integer, intent(in)                                 :: columns
    Character(len=ESMF_MAXSTR),dimension(:,:),&
        pointer,intent(out)                             :: dba

    !LOCAL VARS
    !Character(len=ESMF_MAXSTR),dimension(:,:),&
    !    allocatable,target                              :: dba_local
    logical                     :: err, finished
    type(SQLITE_STATEMENT)      :: stmt
    integer                     :: i, j, completion, rows

    character(len=ESMF_MAXSTR) :: test

    !------------------------------------------------------------------

    !Replace tags with values given by variables
    if (present(search_list) .and. present(replace_list)) then
        !write(*,*) "ping"
        do i=1, size(search_list)
            !write(*,*) "pong"
            if ((.not. search_list(i) == "") .and. (.not. replace_list(i)=="")) &
                sql=Replace_String(sql,search_list(i),replace_list(i))
        end do
    end if

    !write(*,*) sql

    !> Init connection and start a new Transaction
    call load_session

!    if (debug) then
!        write(*,*) "> Completition: ", completion
!    end if

    !> Run the statement
    call sqlite3_prepare( db, sql, stmt, col )
    call sqlite3_step( stmt, completion )

    if (completion==100) then
        !> Count rows in result
        !> @todo: better way to get row number!?
        !Reset position in database array
        do while (finished .eqv. .false.)
            call sqlite3_next_row( stmt, col, finished )
        end do
        finished=.false.
        rows=0
        !Loop and count
        do while (finished .eqv. .false.)
            rows=rows+1
            call sqlite3_next_row( stmt, col, finished )
        end do

        allocate(dba(columns, rows))
        if (DEBUG .eqv. .true.) then
            write(*,*) ""
            write(*,*) "> Shape of dba: ", shape(dba)
        end if

        do j=1, rows
            !write(*,*) "ping"
            call sqlite3_next_row( stmt, col, finished )
                do i=1, columns
                    !write(*,*) "pong"
                    call sqlite3_get_column( col(i), dba(j,i))
                end do
        end do

!    j=0
!    do while (finished .eqv. .false.)
!        j=j+1
!        call sqlite3_next_row( stmt, col, finished )
!        do i=1, columns
!            !write(*,*) "pong"
!            call sqlite3_get_column( col(i), test)
!            write(*,'(A)') test
!        end do
!    end do


        if (DEBUG .eqv. .true.) then
            write(*,*) ""
            write(*,*) ""
            write(*,*) "> cols/rows: ", columns, rows
            write (*,*) "> SQL-State: ", sql
            write(*,*) "> Errors: ", sqlite3_errmsg( db )
            write(*,*) ""
        end if
    else
        if (DEBUG .eqv. .true.) then
            write(*,*) ""
            write (*,*) "> SQL-State: ", sql
            write(*,*) "> NO RESULTS"
            write(*,*) "> Errors: ", sqlite3_errmsg( db )
            write(*,*) ""
        end if
    end if

    call finalize_session(.false.,(completion .ne. SQLITE_DONE))

!    write(*,*) name

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
