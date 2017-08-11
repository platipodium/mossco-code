!> @brief Implementation of test suit for MOSSCO_StateGet
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2017 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#define ESMF_FILENAME "test_MOSSCO_StateGet.F90"
#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define ESMF_METHOD "test_MOSSCO_StateGet"
program test_MOSSCO_StateGet

use esmf
use mossco_state

implicit none

type(ESMF_State)       :: state
type(ESMF_Field)       :: field
type(ESMF_FieldBundle) :: fieldBundle
integer                :: rc, localrc, i, fieldCount
type(ESMF_Field), allocatable :: fieldList(:)

call ESMF_Initialize(rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

state = ESMF_StateCreate(name='state', rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call MOSSCO_StateGet(state, fieldList, rc=localrc)
if (localrc /= ESMF_SUCCESS .and. localrc /= ESMF_RC_NOT_FOUND) then
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
endif

call MOSSCO_StateGet(state, fieldList=fieldList, rc=localrc)
if (localrc /= ESMF_SUCCESS .and. localrc /= ESMF_RC_NOT_FOUND) then
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
endif

call MOSSCO_StateGet(state, fieldList=fieldList, fieldCount=fieldCount, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

if (fieldCount /= 0) localrc=ESMF_RC_OBJ_BAD
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

!> Add empty field and fieldBundle

field = ESMF_FieldEmptyCreate(name='emptyField', rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_StateAddReplace(state,(/field/), rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call MOSSCO_StateGet(state, fieldList=fieldList, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

if (.not.allocated(fieldList)) localrc=ESMF_RC_OBJ_BAD
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

if (ubound(fieldList,1) /= 1) localrc=ESMF_RC_OBJ_BAD
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

fieldBundle=ESMF_FieldBundleCreate(name='fieldBundle', rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_FieldBundleAdd(fieldBundle, (/field,field/), multiFlag=.true., rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_StateAddReplace(state, (/fieldBundle/), rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call MOSSCO_StateGet(state, fieldList=fieldList, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

if (ubound(fieldList,1) /= 3) localrc=ESMF_RC_OBJ_BAD
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)



!> Clean up

call MOSSCO_StateGet(state, fieldList=fieldList, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

do i=lbound(fieldList,1),ubound(fieldList,1)
  call ESMF_FieldDestroy(fieldList(i), rc=localrc)
enddo

call ESMF_StateDestroy(state, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_Finalize()

end program
