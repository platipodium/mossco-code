!> @brief Implementation of an ESMF link coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015, Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister
!> @author Carsten Lemmen
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "soil_pelagic_mediator.F90"

module soil_pelagic_mediator

  use esmf
  use mossco_state
  use mossco_field
  use mossco_component
  use mossco_db

  implicit none

end module soil_pelagic_mediator
