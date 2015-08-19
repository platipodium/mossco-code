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
!    character(len=ESMF_MAXSTR)             :: rulesets &
!                                               = "'General'"
    character(len=ESMF_MAXSTR), target      :: rulesets &
                                               = "'General', &
                                                  'HZG KW'"


    !INPUTS/OUTPUTS

    !LOCAL VARS
    !@temp
    character(len=ESMF_MAXSTR), target      :: equivalent, &
                                               name="nothing"
    character(len=ESMF_MAXSTR), pointer     :: res
    character(len=ESMF_MAXSTR)              :: tmp
    logical                                 :: finished


    integer, dimension(:,:), allocatable :: test, test2
    integer :: i,j,h,n,c
    character(len=ESMF_MAXSTR),dimension(:,:),pointer &
                                            :: dba, &
                                               dba2, &
                                               dba3, &
                                               dba_aliases, &
                                               dba_substances

    character(len=ESMF_MAXSTR)              :: str1, str2
    !type(ESMF_ARRAY) :: dba_ESMF
    !------------------------------------------------------------------

    !******************************************************************
    !************* Enter search string for test here: *****************
    !******************************************************************
    equivalent = "oxygen"

    !******************************************************************
    !******************************************************************
    write(*,*) ""
    write(*,*) "- - - - - - - - Starting test mossco_db - - - - - - - - "
    write(*,*) "Using rulesets: ", rulesets

    call get_substances_list(dba_substances)
    write(*,*) ""
    write(*,*) "###### List of all substances ######"
    write(*,'(A)') dba_substances

    write(*,*) "###### List of all aliases ######"
    do i=1, (size(dba_substances))
        call get_substance_aliases_list(dba_substances(i,1),rulesets,dba_aliases)
        write(*,*) ""
        write(*,*) "** List of aliases for substance ", dba_substances(i,1)
        if (.not. associated(dba_aliases)) then
            write(*,*) "nothing found"
        else
            write(*,'(A)') (dba_aliases(j,1), j=1,(size(dba_aliases)))
        end if
    end do

    write(*,*) "- - - - - - - - Finishing test mossco_db - - - - - - - -"


    !----------------------------------------------------------------------------------------------
    !----------------------------------------------------------------------------------------------
    !----------------------------------------------------------------------------------------------


    return

    !search for manually entered equivalent name
    write (*,*) "Searching db for name & 
    connected to equivalent '" , equivalent , "', found:"
    call get_equivalent_name(equivalent,rulesets,res)

    return

    write (*,*) "******************************************"

    !search for manually entered equivalent name
    write (*,*) "Searching db for name & 
    connected to equivalent '" , equivalent , "', found:"

   call get_equivalent_name(equivalent,rulesets,res)

    if (associated(res)) then
        name=res
        write (*,'(A)') name
    else
        write(*,*) "error (res), quitting"
        return
    end if

    return

    write (*,*) "******************************************"

    write(*,'(A)') "Get list of all substance-appendix combinations &
    for " // name // ":"


    call get_substance_aliases_list(name, rulesets, dba)
    write(*,'(A)') (dba(j,1), j=1, (size(dba)/2))

    write (*,*) "******************************************"
    write(*,'(A)') "Get list of all equivalent-appendix combinations &
    for " // name // ":"

    !call get_substance_aliases_list(name, rulesets, dba)
    write(*,'(A)') (dba(j,2), j=1, (size(dba)/2))

    write (*,*) "******************************************"

    write(*,'(A)') "Get list of all appendix IDs used by '" // name // "':"
    call get_substance_appendices_list(name, dba2)

    write(*,'(A)') dba2

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


    return


    write (*,*) "use ruleset: " // rulesets

    !Receive full list of substances saved in db
    write (*,*) "******************************************"
    write(*,*) "Get list of all substances:"
    call get_substances_list(dba)
    write(*,'(A)') dba

!    write(*,*) sum(index(dba, "O_2"))
!    write(*,*) sum(index(dba, "NH_3"))
!    write(*,*) sum(index(dba, "N"))
!    write(*,*) sum(index(dba, "efefewgG"))

    write(*,*) ""
    do i=1, size(dba)
        write(*,*) "+++++++++++++++++++++++++++++++++++++++"
        write(*,'(A)') "Substance: ", dba(i,1)
        call get_substance_aliases_list(dba(i,1), rulesets, dba_aliases)
        do j=1, size(dba_aliases)/2
            write(*,'(A)') "Substance-Alias: ", dba_aliases(i,1)
            write(*,'(A)') "Equivalent-Alias: ", dba_aliases(i,2)
        end do
        write(*,*) "+++++++++++++++++++++++++++++++++++++++"
    end do





!#####################################################################

!        str1="O_2"
!        str2="NH_3"
!        call get_substance_aliases_list(str1, rulesets, dba_aliases)
!        do j=1, size(dba_aliases)
!            write(*,'(A)') "Alias: ", dba_aliases(i,1), dba_aliases(i,2)
!        end do
!
!        call get_substance_aliases_list(str2, rulesets, dba_aliases)
!        do j=1, size(dba_aliases)
!            write(*,'(A)') "Alias: ", dba_aliases(i,1), dba_aliases(i,2)
!        end do


!    allocate(test(2,5))
!    test=reshape((/1,2,1,2,1,2,1,2,1,2/),shape(test))
!
!    write(*,*) ( test(1,i), i=1,5 )
!    write(*,*) ( test(2,i), i=1,5 )
!
!    allocate( test2( 1,(size(test)/2) ) )

end program
