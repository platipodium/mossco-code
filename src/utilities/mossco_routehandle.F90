!> @brief Implementation of routeHandle utilities
!!
!! This computer program is part of MOSSCO.
!! @copyright Copyright 2018 Helmholtz-Zentrum Geesthacht
!! @author Carsten Lemmen <carsten.lemmen@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#define ESMF_FILENAME "mossco_routehandle.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module mossco_routeHandle

  use esmf
  use mossco_strings

  implicit none

  public MOSSCO_routeHandleString

  private

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_routeHandleString"
subroutine MOSSCO_RouteHandleString(routeHandle, message, kwe, length, rc)

  type(ESMF_RouteHandle), intent(in)             :: routeHandle
  character(len=ESMF_MAXSTR), intent(inout)      :: message
  logical, intent(in), optional                  :: kwe
  integer(ESMF_KIND_I4), intent(inout), optional :: length
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  integer(ESMF_KIND_I4)   :: rc_, length_, localrc
  character(ESMF_MAXSTR)  :: name, string

  logical                     :: isPresent, isCreated

  rc_ = ESMF_SUCCESS
  if (present(kwe)) rc_ = ESMF_SUCCESS

  isCreated = ESMF_RouteHandleIsCreated(routeHandle, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  if (isCreated) then
    call ESMF_routeHandleGet(routeHandle, name=name, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !call ESMF_AttributeGet(routeHandle, name='creator', isPresent=isPresent, rc=localrc)
    !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    ! if (isPresent) then
    !   call ESMF_AttributeGet(routeHandle, name='creator', value=string, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    !   call MOSSCO_MessageAdd(message, ' ['//string)
    !   call MOSSCO_MessageAdd(message, ']'//name)
    ! else
      call MOSSCO_MessageAdd(message,' '//name)
!    endif
  else
    call MOSSCO_MessageAdd(message, ' anonymous non-created routeHandle ')
  endif

  length_=len_trim(message)
  if (present(length)) length=length_
  if (present(rc)) rc=rc_

end subroutine MOSSCO_routeHandleString

end module mossco_routehandle
