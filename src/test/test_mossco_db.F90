!> @file test_mossco_db.F90
!! @brief Tests running a simple sql state to the db
!! @author Nils Weiher
!! @author Carsten Lemmen
!!

program test_mossco_db
    !> ESMF is used here only for the ESMF_MAXSTR macro
    use esmf, only : ESMF_MAXSTR

    !> The sqlite module is used, check that $MOSSCO_DIR/modules/<your-compiler>/
    !> sqlite.mod and sqlite_types.mod are present
    use sqlite
    use mossco_db
    !> @todo: atm database file must be in the EXECUTING folder
    !! fix so that database needs only to be in the utilities folder

    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS

    !LOCAL VARS
    !@temp
    character(len=ESMF_MAXSTR)  :: alias, name="nothing"
    logical                     :: finished
    character(len=ESMF_MAXSTR), dimension(1) &
                                :: rulesets = (/'General'/)
    !------------------------------------------------------------------

    !******************************************************************
    !************* Enter search string for test here: *****************
    !******************************************************************
    alias = "oxygen"
    !******************************************************************
    !******************************************************************

    write (*,*) "Searching db for name & 
    connected to equivalent '" // alias // "', found:"

    call get_substance_name(alias,rulesets,name)

    write (*,*) name

    write (*,*) "test finished"

end program
