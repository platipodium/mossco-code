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

  implicit none

  character(len=ESMF_MAXSTR)                  :: alias
  type(SQLITE_COLUMN),dimension(:),pointer    :: listout

  !Enter search string for test here:
  alias = "TN"

  call get_substance_list(alias,listout)

  write (*,*) listout

end program
