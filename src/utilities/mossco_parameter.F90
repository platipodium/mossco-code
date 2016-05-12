!> @brief Implementation of Parameter utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2016 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "mossco_parameter.F90"

module mossco_parameter

use esmf
implicit none

private

type, public :: MOSSCO_ParameterType
  character(len=ESMF_MAXSTR) :: label ! label as it appears in a config or namelist files
  character(len=ESMF_MAXSTR) :: name  ! attribute name for netcdf and ESMF_Attribute
  integer(ESMF_KIND_I4)      :: size
  type(ESMF_TypeKind_Flag)     :: typeKind
  real(ESMF_KIND_R8), pointer :: realValue => null()
  integer(ESMF_KIND_I8), pointer ::  intValue => null()
  character(len=ESMF_MAXSTR), pointer :: stringValue => null()
end type

end module mossco_parameter
