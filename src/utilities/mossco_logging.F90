!> @brief Implementation of MOSSCO logging utilities
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
#define ESMF_FILENAME "mossco_logging.F90"

module mossco_logging

use esmf
use mossco_field
use mossco_state
implicit none

private
public MOSSCO_Log

interface MOSSCO_Log
  module procedure MOSSCO_StateLog
  module procedure MOSSCO_FieldLog
  module procedure MOSSCO_FieldBundleLog
  module procedure MOSSCO_ArrayLog
  module procedure MOSSCO_ArrayBundleLog
  module procedure MOSSCO_GridCompLog
  module procedure MOSSCO_CplCompLog
end interface MOSSCO_Log

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldBundleLog"
subroutine MOSSCO_FieldBundleLog(fieldBundle, kwe, log, rc)

  type(ESMF_FieldBundle)          :: fieldBundle
  logical,intent(in ),optional    :: kwe !keyword-enforcer
  type(ESMF_Log), optional        :: log
  integer(ESMF_KIND_I4), optional :: rc

  character(len=ESMF_MAXSTR)      :: message

  write(message,'(A)') '  no implementation to log a fieldBundle'
  if (present(rc)) rc=ESMF_RC_NOT_IMPL
  return

end subroutine MOSSCO_FieldBundleLog

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ArrayLog"
subroutine MOSSCO_ArrayLog(array, kwe, log, rc)

  type(ESMF_Array)          :: array
  logical,intent(in ),optional    :: kwe !keyword-enforcer
  type(ESMF_Log), optional        :: log
  integer(ESMF_KIND_I4), optional :: rc

  character(len=ESMF_MAXSTR)      :: message

  write(message,'(A)') '  no implementation to log an array'
  if (present(rc)) rc=ESMF_RC_NOT_IMPL
  return

end subroutine MOSSCO_ArrayLog

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ArrayBundleLog"
subroutine MOSSCO_ArrayBundleLog(arrayBundle, kwe, log, rc)

  type(ESMF_ArrayBundle)          :: arrayBundle
  logical,intent(in ),optional    :: kwe !keyword-enforcer
  type(ESMF_Log), optional        :: log
  integer(ESMF_KIND_I4), optional :: rc

  character(len=ESMF_MAXSTR)      :: message

  write(message,'(A)') '  no implementation to log an arrayBundle'
  if (present(rc)) rc=ESMF_RC_NOT_IMPL
  return

end subroutine MOSSCO_ArrayBundleLog

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CplCompLog"
subroutine MOSSCO_CplCompLog(cplComp, kwe, log, rc)

  type(ESMF_CplComp)              :: cplComp
  logical,intent(in ),optional    :: kwe !keyword-enforcer
  type(ESMF_Log), optional        :: log
  integer(ESMF_KIND_I4), optional :: rc

  character(len=ESMF_MAXSTR)      :: message

  write(message,'(A)') '  no implementation to log a cplComp'
  if (present(rc)) rc=ESMF_RC_NOT_IMPL
  return

end subroutine MOSSCO_CplCompLog

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompLog"
subroutine MOSSCO_GridCompLog(gridComp, kwe, log, rc)

  type(ESMF_GridComp)          :: gridComp
  logical,intent(in ),optional    :: kwe !keyword-enforcer
  type(ESMF_Log), optional        :: log
  integer(ESMF_KIND_I4), optional :: rc

  character(len=ESMF_MAXSTR)      :: message

  write(message,'(A)') '  no implementation to log a gridComp'
  if (present(rc)) rc=ESMF_RC_NOT_IMPL
  return

end subroutine MOSSCO_GridCompLog
end module mossco_logging
