!> @brief Implementation of locstream utilities
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
#define ESMF_FILENAME "mossco_locstream.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module mossco_locstream

  use esmf
  use mossco_strings

  implicit none

  public MOSSCO_LocStreamString

  private

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_LocStreamString"
subroutine MOSSCO_LocStreamString(locStream, message, kwe, length, rc)

  type(ESMF_LocStream), intent(in)               :: locStream
  character(len=ESMF_MAXSTR), intent(inout)      :: message
  logical, intent(in), optional                  :: kwe
  integer(ESMF_KIND_I4), intent(inout), optional :: length
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  integer(ESMF_KIND_I4)   :: rc_, length_, keyCount, localrc, locationCount
  integer(ESMF_KIND_I4)   :: lbnd, ubnd, i
  character(ESMF_MAXSTR)  :: string, name
  character(ESMF_MAXSTR), allocatable :: keyNames(:)
  type(ESMF_Array)        :: array

  logical                     :: isPresent

  rc_ = ESMF_SUCCESS
  if (present(kwe)) rc_ = ESMF_SUCCESS

  call ESMF_LocStreamGet(locStream, name=name, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

#if ESMF_VERSION_MAJOR > 6
#if ESMF_VERSION_MINOR > 0
  call ESMF_AttributeGet(locStream, name='creator', isPresent=isPresent, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  if (isPresent) then
    call ESMF_AttributeGet(locStream, name='creator', value=string, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    call MOSSCO_MessageAdd(message, ' ['//string)
    call MOSSCO_MessageAdd(message, ']'//name)
  else
    call MOSSCO_MessageAdd(message,' '//name)
  endif
#else
  call MOSSCO_MessageAdd(message,' '//name)
#endif
#else
  call MOSSCO_MessageAdd(message,' '//name)
#endif

  call ESMF_LocStreamGet(locStream, keyCount=keyCount, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  if (keyCount < 1) then
    call MOSSCO_MessageAdd(message, '(no keys)')
  else
    allocate(keyNames(keyCount), stat=localrc)
    write(string,*) keyCount
    call MOSSCO_MessageAdd(message, ' '//trim(adjustl(string)))
    call ESMF_LocStreamGet(locStream, keyNames=keyNames, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    ! call MOSSCO_MessageAdd(message, ' ('//trim(keyNames(1)))
    ! do i=2,keyCount
    !   call MOSSCO_MessageAdd(message, ','//keyNames(i))
    ! enddo
    ! call MOSSCO_MessageAdd(message,')')
  endif

  locationCount = -1

  if (keyCount>0) then
    call ESMF_LocStreamGetBounds(locStream, exclusiveLBound=lbnd, &
      exclusiveUbound=ubnd, exclusiveCount=locationCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !call ESMF_LocStreamGetKey(locStream, keyName=keyNames(1), keyArray=array, rc=localrc)
    !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    write(string,*) locationCount
    call MOSSCO_MessageAdd(message,' ('//trim(adjustl(string))//')')
  endif

  if (allocated(keyNames)) deallocate(keyNames)

  length_=len_trim(message)
  if (present(length)) length=length_
  if (present(rc)) rc=rc_

end subroutine MOSSCO_LocStreamString

end module mossco_locstream
