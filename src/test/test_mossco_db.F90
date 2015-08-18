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
                                                name="nothing", &
                                                tmp
    logical                                  :: finished


    integer, dimension(:,:), allocatable :: test, test2
    integer :: i,j
    character(len=ESMF_MAXSTR),dimension(:,:),allocatable :: dba, &
                                                             dba2, &
                                                             dba3
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

    write(*,*) sum(index(dba, "O_2"))
    write(*,*) sum(index(dba, "NH_3"))
    write(*,*) sum(index(dba, "N"))
    write(*,*) sum(index(dba, "efefewgG"))



    write (*,*) "******************************************"

    !search for manually entered equivalent name
    write (*,*) "Searching db for name & 
    connected to equivalent '" // equivalent // "', found:"

    call get_substance_name(equivalent,rulesets,name)
    write (*,'(A)') name

    write (*,*) "******************************************"

    write(*,'(A)') "Get list of all substance-appendix combinations &
    for " // name // ":"


    call get_substance_aliases_list(name, rulesets, dba)
    !write(*,*) size(dba)
    write(*,'(A)') (dba(j,1), j=1, (size(dba)/4))
    !> @todo: why is the size that big???
    !> 2 cols, 6 "hits", but size 24
    !> @todo: Too many results

    write (*,*) "******************************************"
    write(*,'(A)') "Get list of all equivalent-appendix combinations &
    for " // name // ":"

    !call get_substance_aliases_list(name, rulesets, dba)
    write(*,'(A)') (dba(j,2), j=1, (size(dba)/4))

    write (*,*) "******************************************"

    write(*,'(A)') "Get list of all appendix IDs used by '" // name // "':"
    call get_substance_appendices_list(name, dba2)

    write(*,'(A)') dba2
    !> @todo: Bug - one ID too much (double-2)

    write (*,*) "******************************************"

    write(*,'(A)') "Get list of all substance-appendix combinations for '" // name // "':"

    do i=1, size(dba2)
        call get_substance_appendix_aliases_list &
             (name, dba2(i,1), rulesets, dba3)
        write(*,'(A)') dba3
        !> @todo: Bug - doubled resulsts (even if DISTINCT is used)
    end do

    write (*,*) "******************************************"


    write (*,*) "test finished"





!    allocate(test(2,5))
!    test=reshape((/1,2,1,2,1,2,1,2,1,2/),shape(test))
!
!    write(*,*) ( test(1,i), i=1,5 )
!    write(*,*) ( test(2,i), i=1,5 )
!
!    allocate( test2( 1,(size(test)/2) ) )

end program
