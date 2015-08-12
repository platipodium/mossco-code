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

    !CONFIG
!    character(len=ESMF_MAXSTR)               :: rulesets &
!                                                = "'General'"
    character(len=ESMF_MAXSTR)               :: rulesets &
                                                = "'General', &
                                                  'HZG KW'"

    !INPUTS/OUTPUTS

    !LOCAL VARS
    !@temp
    character(len=ESMF_MAXSTR)               :: equivalent, &
                                                name="nothing"
    logical                                  :: finished


    !integer, dimension(:,:), allocatable :: test
    integer :: i
    character(len=ESMF_MAXSTR),dimension(:,:),allocatable :: dba
    !type(ESMF_ARRAY) :: dba_ESMF
    !------------------------------------------------------------------

    !******************************************************************
    !************* Enter search string for test here: *****************
    !******************************************************************
    equivalent = "oxygen"

    !******************************************************************
    !******************************************************************

    write (*,*) "use ruleset: " // rulesets

    !Receive full list of substances saved in db
    write (*,*) "******************************************"
    write(*,*) "Get list of all substances:"
    call get_substances_list(dba)
    write(*,'(A)') dba

    write (*,*) "******************************************"

    !search for manually entered equivalent name
    write (*,*) "Searching db for name & 
    connected to equivalent '" // equivalent // "', found:"

    call get_substance_name(equivalent,rulesets,name)
    write (*,'(A)') name

    write (*,*) "******************************************"

    write(*,'(A)') "Get list of all equivalent-appendix combinations &
    for " // name // ":"

    call get_substance_aliases_list(name, rulesets, dba)
    write(*,'(A)') dba

    write (*,*) "******************************************"

    !allocate(test(2,5))
    !test=reshape((/1,2,1,2,1,2,1,2,1,2/),shape(test))

    !write(*,*) ( test(1,i), i=1,5 )
    !write(*,*) ( test(2,i), i=1,5 )







    write (*,*) "test finished"

end program
