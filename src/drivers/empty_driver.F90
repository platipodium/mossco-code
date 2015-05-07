!> @brief Implementation of an empty basic model interface
!>
!> @file empty_driver.F90
!!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2015 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "empty_driver.F90"

module empty_driver

  !> Place here use statements of modules from the source of the original model
  !> you are driving with this model interface

  !> Do *not* place here any statements referring to the coupling framework, e.g.,
  !> ESMF, their modules should *not* be used in a basic model interface.

  ! use my_module, only : my_variable, my_function

  implicit none

  !> Declare some public interfaces for init/run/finalize etc.
  public init_empty, run_empty, finalize_empty

  !> Everything else should be private
  private

   contains

   subroutine init_empty()

     implicit none

      !> handle information passed to init_empty, i.e. compute ressources,
      !> time control, such that it fits with your models expectation

      !> call your model's init() subroutine

   end subroutine init_empty

   subroutine run_empty()

     implicit none

      !> handle information passed to run_empty, i.e timestep,
      !> such that it fits with your models expectation

      !> call your model's timestep() subroutine or your model's
      !> timeloop() subroutine

   end subroutine run_empty

   subroutine finalize_empty()

     implicit none

      !> handle information passed to finalize_empty
      !> such that it fits with your models expectation

      !> call your model's shutdown() subroutine

   end subroutine finalize_empty

end module empty_driver
