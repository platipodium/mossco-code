!> @brief Implementation of ESMF Config utilities
!
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
#define ESMF_FILENAME "mossco_config.F90"

module mossco_config

use esmf
implicit none

private
public MOSSCO_ConfigGetList

interface MOSSCO_ConfigGetList
  module procedure MOSSCO_ConfigGetListVector
  module procedure MOSSCO_ConfigGetListKeyValue
end interface MOSSCO_ConfigGetList

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListVector"
  subroutine MOSSCO_ConfigGetListVector(config, label, stringList, rc)

    type(ESMF_Config), intent(inout)  :: config
    character(len=*), intent(in)  :: label
    character(len=ESMF_MAXSTR), intent(inout), allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n
    logical                              :: isPresent

    if (present(rc)) rc=ESMF_SUCCESS

    if (allocated(stringList)) deallocate(stringList)

    call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    n=ESMF_ConfigGetLen(config, label=trim(label), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (n<=0) return
    allocate(stringList(n))

    call ESMF_ConfigFindLabel(config, label=trim(label), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, n
      call ESMF_ConfigGetAttribute(config, value=stringList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

  end subroutine MOSSCO_ConfigGetListVector

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListKeyValue"
  subroutine MOSSCO_ConfigGetListKeyValue(config, label, stringList, rc)

    type(ESMF_Config), intent(inout)  :: config
    character(len=*), intent(in)  :: label
    character(len=ESMF_MAXSTR), intent(out), allocatable :: stringList(:,:)
    integer(ESMF_KIND_I4), intent(inout), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, j, n
    logical                              :: isPresent
    character(ESMF_MAXSTR)               :: currString

    if (present(rc)) rc=ESMF_SUCCESS
    if (allocated(stringList)) deallocate(stringList)

    call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    n=ESMF_ConfigGetLen(config, label=trim(label), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (n>0) allocate(stringList(n,2))

    call ESMF_ConfigFindLabel(config, label=trim(label), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, n
      call ESMF_ConfigGetAttribute(config, value=currString, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      j=index(currString,'=')
      if (j>1) then
         stringlist(i,1)=currString(1:j-1)
         stringList(i,2)=currString(j+1:len_trim(currString))
      endif
    enddo

  end subroutine MOSSCO_ConfigGetListKeyValue

end module mossco_config
