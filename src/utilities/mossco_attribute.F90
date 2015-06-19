!> @brief Implementation of ESMF Attribute utilities
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
#define ESMF_FILENAME "mossco_attribute.F90"

module mossco_attribute

use esmf
implicit none

private

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateAttributeGetStringList1"

  subroutine MOSSCO_StateAttributeGetList1(state, attributeName, stringList, rc)

    type(ESMF_State), intent(in)  :: state
    character(len=*), intent(in)  :: attributeName
    character(len=ESMF_MAXSTR), intent(out), allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n, j
    logical                              :: isPresent
    character(len=4096)                  :: attributeString

    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_AttributeGet(state, name=trim(attributeName), isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    call ESMF_AttributeGet(state, trim(attributeName), value=attributeString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    n=1
    do i=1,len_trim(attributeString)
      if (attributeString(i:i)==',') n=n+1
    enddo

    if (n>0) allocate(stringList(n))
    do i=1,n
      j=index(attributeString,',')
      if (j>0) then
        stringList(i)=attributeString(1:j-1)
      else
        stringList(i)=trim(attributeString)
      endif
      write(attributeString,'(A)') attributeString(j+1:len_trim(attributeString))
    enddo

  end subroutine MOSSCO_StateAttributeGetList1

end module mossco_attribute
