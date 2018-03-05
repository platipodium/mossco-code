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

  integer(ESMF_KIND_I4)   :: rc_, length_, keyCount, localrc
  character(ESMF_MAXSTR)  :: stringValue, name

  logical                     :: isPresent
  integer(ESMF_KIND_I4), allocatable :: ubnd(:), lbnd(:)

  rc_ = ESMF_SUCCESS
  if (present(kwe)) rc_ = ESMF_SUCCESS

  call ESMF_LocStreamGet(locStream, name=name, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  call ESMF_AttributeGet(locStream, name='creator', isPresent=isPresent, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  if (isPresent) then
    call ESMF_AttributeGet(locStream, name='creator', value=stringValue, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    call MOSSCO_MessageAdd(message, ' ['//stringValue)
    call MOSSCO_MessageAdd(message, ']'//name)
  else
    call MOSSCO_MessageAdd(message,' '//name)
  endif

  call ESMF_LocStreamGet(locStream, keyCount=keyCount, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  if (keyCount<10 .and. (len_trim(message) + 3 <=len(message))) then
    write(message,'(A,I1)') trim(message)//' (', keyCount
  elseif (keyCount<100 .and. (len_trim(message) + 4 <=len(message))) then
    write(message,'(A,I2)') trim(message)//' (', keyCount
  elseif (keyCount<1000 .and. (len_trim(message) + 5 <=len(message))) then
    write(message,'(A,I2)') trim(message)//' (', keyCount
  elseif (keyCount<10000 .and. (len_trim(message) + 6 <=len(message))) then
    write(message,'(A,I2)') trim(message)//' (', keyCount
  elseif (keyCount<100000 .and. (len_trim(message) + 7 <=len(message))) then
    write(message,'(A,I2)') trim(message)//' (', keyCount
  elseif (keyCount<1000000 .and. (len_trim(message) + 8 <=len(message))) then
    write(message,'(A,I2)') trim(message)//' (', keyCount
  endif

  if (len_trim(message) + 1 <=len(message)) write(message,'(A)') trim(message)//')'

  length_=len_trim(message)
  if (present(length)) length=length_
  if (present(rc)) rc=rc_

end subroutine MOSSCO_LocStreamString

end module mossco_locstream
