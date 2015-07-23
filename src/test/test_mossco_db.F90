!> @file test_mossco_db.F90
!! @brief Tests running a simple sql state to the db
!! @author Nils Weiher
!!

program test_mossco_db

use esmf
!use sqlite
!@todo: Kann das umgangen werden? Ist hier nötig damit SQLITE_COLUMN bekannt ist
!aber: sqlite liegt mit in libmossco_db und wird dennoch nicht gefunden
use mossco_db

    implicit none
    character(len=ESMF_MAXSTR)                  :: alias
    !type(SQLITE_COLUMN),dimension(:),pointer    :: listout
    !Dieser Datentyp verursacht das problem

    !Enter search string for test here:
    alias = "TN"

    !call get_substance_list(alias,listout)

    write (*,*) listout

end program
