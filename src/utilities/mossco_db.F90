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
    !@todo: atm file must be present in the EXECUTING folder (i.e. in src/test)
    type(SQLITE_DATABASE)           :: db
    logical                         :: session_active=.false.
    logical                         :: con_active=.false.
    logical                         :: DEBUG = .false.

    interface get_substance_appendix_aliases_list
        module procedure get_substance_appendix_aliases_list_1
        module procedure get_substance_appendix_aliases_list_2
    end interface

    public get_equivalent_name, &
           get_equivalent_appendix_name, &
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
!> @subsubsection get_equivalent_name "Get equivalent name"
!> @brief Get the primary name of the substance
!> @param equivalent: Equivalent name for the substance [e.g. dissolved_oxygen]
!> @param rulesets: Names of the active rulesets [e.g. 'General']
!> @param nameout: Primary name of the substance [e.g. O_2]
subroutine get_equivalent_name(equivalent,rulesets,nameout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in)           :: equivalent
    character(len=ESMF_MAXSTR), intent(in)           :: rulesets
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

    deallocate(col)

end subroutine get_equivalent_name


#undef  ESMF_METHOD
#define ESMF_METHOD "get_equivalent_appendix_name"
!> @subsubsection get_equivalent_appendix_name "Get equivalent-appendix name"
!> @brief Get the primary name of the equivalent-appendix combination
!> @param alias: Equivalent name for the substance with appendix
!! [e.g. dissolved_oxygen_at_soil_surface]
!> @param rulesets: Names of the active rulesets [e.g. 'General']
!> @param nameout: Primary name of the substance with appendix
!! [e.g. O_2_at_soil_surface]
subroutine get_equivalent_appendix_name(alias,rulesets,nameout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in)           :: alias
    character(len=ESMF_MAXSTR), intent(in)           :: rulesets
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
    sql = "SELECT t.SubstanceName || coalesce(t.Condition,'') || coalesce(t.Location,'') &
            FROM (tblAppendix &
            JOIN tblSubstances ON tblAppendix.Substance_ID=tblSubstances.ID &
            JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblSubstances.ID &
            JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID &
            JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t &
            WHERE tblRulesets.RulesetName in (~rulesets) &
            AND t.EquivalentName || coalesce(t.Condition,'') || coalesce(t.Location,'') == '~alias';"

    search_list = [character(len=ESMF_MAXSTR) :: "~rulesets", "~alias"]
    replace_list = [character(len=ESMF_MAXSTR) :: rulesets, alias]

    !Construct recordset for return values
    allocate( col(columns) )
    call sqlite3_column_query( col(1), 'Name', SQLITE_CHAR, ESMF_MAXSTR )

    call sql_select_state(sql,col,1,search_list,replace_list,dba)
    if (associated(dba)) nameout=>dba(1,1)

    deallocate(col)

end subroutine get_equivalent_appendix_name


#undef  ESMF_METHOD
#define ESMF_METHOD "get_substances_list"
!> @subsubsection get_substance_list "Get Substance List"
!> @brief Receives list of all known substances from the database
!> @param dbaout: 2D Array with all Substance Names
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
!> @subsubsection get_substance_aliases_list "Get Substance Aliases List"
!> @brief Receives list of all equivalent-appendix combinations
!! of the substance from the database
!> @param name: Primary name of the substance [e.g. "O_2"]
!> @param rulesets: Names of the active rulesets,
!! each in apostrophe seperated by comma [e.g. "'General','...'"]
!> @param dbaout: 2D Array with all aliases
subroutine get_substance_aliases_list(name, rulesets, dbaout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in)           :: name
    character(len=ESMF_MAXSTR), intent(in)           :: rulesets
    character(len=ESMF_MAXSTR),dimension(:,:),pointer,intent(out) &
                                                     :: dbaout

    !LOCAL VARS
    integer                                          :: columns = 2
    type(SQLITE_COLUMN), dimension(:), pointer       :: col =>null()
    character(len=ESMF_MAXSTR), dimension(2)         :: search_list, &
                                                        replace_list

    character(1000)                                  :: sql
    !------------------------------------------------------------------
    dbaout=>null()
    sql = "SELECT DISTINCT t.EquivalentName || coalesce(t.Condition,'') || coalesce(t.Location,''), &
        t.Unit &
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
    call sqlite3_column_query( col(1), 'Equivalent-Appendix', SQLITE_CHAR, ESMF_MAXSTR )
    call sqlite3_column_query( col(2), 'Unit', SQLITE_CHAR, ESMF_MAXSTR )

    call sql_select_state(sql,col,2,search_list,replace_list,dbaout)

    deallocate(col)

    sql=""

end subroutine get_substance_aliases_list


#undef  ESMF_METHOD
#define ESMF_METHOD "get_substance_appendices_list"
!> @subsubsection get_substance_appendices_list "Get Substance Appendices List"
!> @brief Receives list of all known appendices-IDs for a substance
!> @detail list by unique identifier: Substance name
!> @param name: Primary name of the substance
!> @param dbaout: 2D Array with all Substance Names
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
#define ESMF_METHOD "get_substance_appendix_aliases_list_1"
!> @subsubsection get_substance_appendix_aliases_list "Get Substance Appendix-Aliases List"
!> @brief Receives list of all aliases of the substance connected with one appendix
!> @param SubstanceName: Primary name of the substance [e.g. O_2]
!> @param apdxID: Database ID of the appendix
!> @param rulesets: Names of the active rulesets,
!! each in apostrophe seperated by comma [e.g. "'General','...'"]
!> @param dbaout: 2D Array with the equivalent-appendix combinations
subroutine get_substance_appendix_aliases_list_1(SubstanceName, apdxID, rulesets, dbaout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in)           :: SubstanceName
    character(len=*), intent(in)                     :: rulesets, apdxID
    character(len=ESMF_MAXSTR),dimension(:,:),pointer,intent(out) &
                                                     :: dbaout


    !LOCAL VARS
    integer                                          :: columns = 2
    type(SQLITE_COLUMN), dimension(:), pointer       :: col =>null()
    character(len=ESMF_MAXSTR), dimension(3)         :: search_list, &
                                                        replace_list

    character(1000)                                  :: sql

    !------------------------------------------------------------------
    sql = "SELECT DISTINCT t.EquivalentName || coalesce(t.Condition,'') || coalesce(t.Location,''), &
            t.Unit &
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
    call sqlite3_column_query( col(2), 'Unit', SQLITE_CHAR, ESMF_MAXSTR )

    call sql_select_state(sql,col,2,search_list,replace_list,dbaout)

    deallocate(col)

end subroutine get_substance_appendix_aliases_list_1


#undef  ESMF_METHOD
#define ESMF_METHOD "get_substance_appendix_aliases_list_2"
!> @subsubsection get_substance_appendix_aliases_list "Get Substance Appendix-Aliases List"
!> @brief Receives list of all aliases of the substance connected with one appendix
!> @param SubstanceAppendix: Primary name of the
!! substance connected with one appendix [e.g. O_2_at_soil_surface]
!> @param rulesets: Names of the active rulesets,
!! each in apostrophe seperated by comma [e.g. "'General','...'"]
!> @param dbaout: 2D Array with the equivalent-appendix combinations
subroutine get_substance_appendix_aliases_list_2(SubstanceAppendix, rulesets, dbaout)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    character(len=ESMF_MAXSTR), intent(in)           :: SubstanceAppendix
    character(len=*), intent(in)                     :: rulesets
    character(len=ESMF_MAXSTR),dimension(:,:),pointer,intent(out) &
                                                     :: dbaout

    !LOCAL VARS
    integer                                          :: columns = 2
    type(SQLITE_COLUMN), dimension(:), pointer       :: col =>null()
    character(len=ESMF_MAXSTR), dimension(2)         :: search_list, &
                                                        replace_list

    character(1000)                                  :: sql

    !------------------------------------------------------------------
    sql = "SELECT DISTINCT t.EquivalentName || coalesce(t.Condition,'') || coalesce(t.Location,''), & 
    t.Unit &
    FROM (tblAppendix &
    JOIN tblSubstancesEquivalents ON tblSubstancesEquivalents.Substance_ID=tblAppendix.Substance_ID &
    JOIN tblSubstances ON tblSubstances.ID=tblSubstancesEquivalents.Substance_ID &
    JOIN tblRulesets ON tblRulesets.ID=tblSubstancesEquivalents.Ruleset_ID &
    JOIN tblEquivalents ON tblSubstancesEquivalents.Equivalent_ID=tblEquivalents.ID) t &
    WHERE tblRulesets.RulesetName IN(~rulesets) &
    AND tblSubstances.SubstanceName || coalesce(tblAppendix.Condition,'') || coalesce(tblAppendix.Location,'') == '~SubstanceAppendix';" 

    search_list = [character(len=ESMF_MAXSTR) :: "~rulesets", "~SubstanceAppendix"]
    replace_list = [character(len=ESMF_MAXSTR) :: rulesets, SubstanceAppendix]

    !Construct recordset for return values
    allocate( col(columns) )
    call sqlite3_column_query( col(1), 'Substance aliases', SQLITE_CHAR, ESMF_MAXSTR )
    call sqlite3_column_query( col(2), 'Unit', SQLITE_CHAR, ESMF_MAXSTR )

    call sql_select_state(sql,col,2,search_list,replace_list,dbaout)

    deallocate(col)

end subroutine get_substance_appendix_aliases_list_2



!----------------------------------------------------------------------
!------------------- Basic SQL Routines -------------------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "load_session"
!> @subsubsection load_session "Load Session"
!> @brief Commits pending transaction and starts new session
subroutine load_session
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS

    !LOCAL VARS

    !------------------------------------------------------------------
    !Init connection to database file given by module
    if (con_active .eqv. .false.) then
        if (debug) write(*,*) "> Opening Session"
        call sqlite3_open( dbfile, db )
        con_active=.true.
    end if

    !Execute previous session commands and reinit
    if (session_active .eqv. .true.) then
        if (debug) write(*,*) "> Found open session, finalize"
        call finalize_session(.true.,.false.)
    end if
    session_active=.true.

end subroutine load_session




#undef  ESMF_METHOD
#define ESMF_METHOD "finalize_session"
!> @subsubsection finalize_session "Finalize Session"
!> @brief Quits the current session
!> @param hold_con: keeps the connection open if .true.
!> @param abort: rolls back changes during session if .true.
subroutine finalize_session(hold_con,abort)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    logical, intent(in), optional     :: hold_con,abort

    !LOCAL VARS
    logical                           :: hcon, critical = .false.
    integer                           :: localrc
    !------------------------------------------------------------------
    hcon=hold_con

    !> Catch wrong call, end session
    if ((session_active .eqv. .false.) .and. (con_active .eqv. .false.)) return

    !check external error flag / current errors and treat them
    if (abort .eqv. .true.) then
        !> Undo changes made to database
        call sqlite3_rollback( db )
        hcon = .false.
        critical = .true.
        if (debug) write(*,*) "> Abort current database session"
    else
        call sqlite3_commit( db )
        if (debug) write(*,*) "> Commit current changes"
    end if

    !> End session
    session_active=.false.
    if (debug) write(*,*) "> Closing current session"

    !> Quit connection and clear flag for regular shutdowns and errors
    if (hcon .eqv. .false.) then
        con_active = .false.
        call sqlite3_close( db )
        if (debug) write(*,*) "> Closing database connection"
    end if

    !> On critical error run ESMF_END_ABORT routine after connection has been shut down
    !if (critical) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !> @todo: Call causes error
end subroutine finalize_session



!----------------------------------------------------------------------
!------------------- BASIC SQL ROUTINES -------------------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "sql_select_state"
!> @subsubsection sql_select_state "SQL select state"
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

    !> Init connection and start a new Transaction
    call load_session

    !> Run the statement
    call sqlite3_prepare( db, sql, stmt, col )
    call sqlite3_step( stmt, completion )

    !> Check successfull execution
    if (completion==100) then
        !> Count rows in result
        !! Reset position in database array
        do while (finished .eqv. .false.)
            call sqlite3_next_row( stmt, col, finished )
        end do
        finished=.false.
        rows=0
        !> Loop and count
        do while (finished .eqv. .false.)
            rows=rows+1
            call sqlite3_next_row( stmt, col, finished )
        end do

        allocate(dba(columns, rows))

        if (DEBUG .eqv. .true.) then
            write(*,*) ""
            write(*,*) "> " // test
        end if

        !> Write values into result array
        do j=1, rows
            call sqlite3_next_row( stmt, col, finished )
                do i=1, columns
                    call sqlite3_get_column( col(i), dba(j,i))
                end do
        end do


        if (DEBUG .eqv. .true.) then
            write(*,*) ""
            write(*,*) ""
            write(*,*) "> cols/rows: ", columns, rows
            write (*,*) "> SQL-State: ", sql
            write(*,*) "> Errors: ", sqlite3_errmsg( db )
            write(*,*) ""
        end if
    else
        !> Unallocated return value for failed execution
        dba=>null()
        if (DEBUG .eqv. .true.) then
            write(*,*) ""
            write (*,*) "> SQL-State: ", sql
            write(*,*) "> NO RESULTS"
            write(*,*) "> Errors: ", sqlite3_errmsg( db )
            write(*,*) ""
        end if
    end if

    call finalize_session(.false.,sqlite3_error(db))

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
