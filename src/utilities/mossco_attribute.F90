!> @brief Implementation of ESMF Attribute utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2015, 2016 Helmholtz-Zentrum Geesthacht
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
public MOSSCO_AttributeGetList, MOSSCO_AttributeSetList

interface MOSSCO_AttributeSetList
  module procedure MOSSCO_StateAttributeSetList1
  module procedure MOSSCO_StateAttributeSetList2
  module procedure MOSSCO_CplCompAttributeSetList1
  module procedure MOSSCO_CplCompAttributeSetList2
  module procedure MOSSCO_GridCompAttributeSetList1
  module procedure MOSSCO_GridCompAttributeSetList2
end interface MOSSCO_AttributeSetList

interface MOSSCO_AttributeGetList
  module procedure MOSSCO_StateAttributeGetList1
  module procedure MOSSCO_StateAttributeGetList2
  module procedure MOSSCO_CplCompAttributeGetList1
  module procedure MOSSCO_CplCompAttributeGetList2
  module procedure MOSSCO_GridCompAttributeGetList1
  module procedure MOSSCO_GridCompAttributeGetList2
end interface MOSSCO_AttributeGetList

interface MOSSCO_AttributeGetForeignGrid
  module procedure MOSSCO_StateAttributeGetForeignGrid
end interface MOSSCO_AttributeGetForeignGrid

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateAttributeSetList1"
  subroutine MOSSCO_StateAttributeSetList1(state, attributeName, stringList, rc)

    type(ESMF_State), intent(inout)  :: state
    character(len=*), intent(in)  :: attributeName
    character(len=*), intent(in), allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, j
    character(len=4096)                  :: attributeString

    if (present(rc)) rc=ESMF_SUCCESS
    if (.not.allocated(stringList)) return

    attributeString=''
    do i=lbound(stringList,1), ubound(stringList,1)
      if (len_trim(attributeString)>0) write(attributeString,'(A)') trim(attributeString)//','
      write(attributeString,'(A)') trim(attributeString)//trim(stringlist(i))
    enddo

    call ESMF_AttributeSet(state, trim(attributeName), value=trim(attributeString), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_StateAttributeSetList1

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateAttributeSetList2"
  subroutine MOSSCO_StateAttributeSetList2(state, attributeName, stringList, rc)

    type(ESMF_State), intent(inout)  :: state
    character(len=*), intent(in)  :: attributeName
    character(len=*), intent(in), allocatable :: stringList(:,:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, j
    character(len=4096)                  :: attributeString

    if (present(rc)) rc=ESMF_SUCCESS
    if (.not.allocated(stringList)) return

    attributeString=''
    do i=lbound(stringList,1), ubound(stringList,1)
      if (len_trim(attributeString)>0) write(attributeString,'(A)') trim(attributeString)//','
      write(attributeString,'(A)') trim(attributeString)//trim(stringlist(i,1))//'='//trim(stringlist(i,2))
    enddo

    call ESMF_AttributeSet(state, trim(attributeName), value=attributeString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_StateAttributeSetList2


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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
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

  subroutine MOSSCO_StateAttributeGetList2(state, attributeName, stringList, rc)

    type(ESMF_State), intent(in)  :: state
    character(len=*), intent(in)  :: attributeName
    character(len=ESMF_MAXSTR), intent(out), allocatable :: stringList(:,:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n, j
    logical                              :: isPresent
    character(len=4096)                  :: attributeString
    character(len=ESMF_MAXSTR)           :: currString

    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_AttributeGet(state, name=trim(attributeName), isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    call ESMF_AttributeGet(state, trim(attributeName), value=attributeString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    n=1
    do i=1,len_trim(attributeString)
      if (attributeString(i:i)==',') n=n+1
    enddo

    if (n>0) allocate(stringList(n,2))
    do i=1,n
      j=index(attributeString,',')
      if (j>0) then
        currString=attributeString(1:j-1)
      else
        currString=trim(attributeString)
      endif
      write(attributeString,'(A)') attributeString(j+1:len_trim(attributeString))

      j=index(currString,'=')
      if (j>1) then
        stringList(i,1)=trim(currString(1:j-1))
        stringList(i,2)=trim(currString(j+1:len_trim(currString)))
      endif
    enddo

  end subroutine MOSSCO_StateAttributeGetList2

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateAttributeSetList1"
  subroutine MOSSCO_cplCompAttributeSetList1(cplComp, attributeName, stringList, rc)

    type(ESMF_cplComp), intent(inout)  :: cplComp
    character(len=*), intent(in)  :: attributeName
    character(len=*), intent(in), allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, j
    character(len=4096)                  :: attributeString

    if (present(rc)) rc=ESMF_SUCCESS
    if (.not.allocated(stringList)) return

    attributeString=''
    do i=lbound(stringList,1), ubound(stringList,1)
      if (len_trim(attributeString)>0) write(attributeString,'(A)') trim(attributeString)//','
      write(attributeString,'(A)') trim(attributeString)//trim(stringlist(i))
    enddo

    call ESMF_AttributeSet(cplComp, trim(attributeName), value=trim(attributeString), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_cplCompAttributeSetList1

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_cplCompAttributeSetList2"
  subroutine MOSSCO_cplCompAttributeSetList2(cplComp, attributeName, stringList, rc)

    type(ESMF_cplComp), intent(inout)  :: cplComp
    character(len=*), intent(in)  :: attributeName
    character(len=*), intent(in), allocatable :: stringList(:,:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, j
    character(len=4096)                  :: attributeString

    if (present(rc)) rc=ESMF_SUCCESS
    if (.not.allocated(stringList)) return

    attributeString=''
    do i=lbound(stringList,1), ubound(stringList,1)
      if (len_trim(attributeString)>0) write(attributeString,'(A)') trim(attributeString)//','
      write(attributeString,'(A)') trim(attributeString)//trim(stringlist(i,1))//'='//trim(stringlist(i,2))
    enddo

    call ESMF_AttributeSet(cplComp, trim(attributeName), value=attributeString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_cplCompAttributeSetList2

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_cplCompAttributeGetList1"
  subroutine MOSSCO_cplCompAttributeGetList1(cplComp, attributeName, stringList, rc)

    type(ESMF_cplComp), intent(in)  :: cplComp
    character(len=*), intent(in)  :: attributeName
    character(len=ESMF_MAXSTR), intent(out), allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n, j
    logical                              :: isPresent
    character(len=4096)                  :: attributeString

    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_AttributeGet(cplComp, name=trim(attributeName), isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    call ESMF_AttributeGet(cplComp, trim(attributeName), value=attributeString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
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

  end subroutine MOSSCO_cplCompAttributeGetList1

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_cplCompAttributeGetList2"
  subroutine MOSSCO_cplCompAttributeGetList2(cplComp, attributeName, stringList, rc)

    type(ESMF_cplComp), intent(in)  :: cplComp
    character(len=*), intent(in)  :: attributeName
    character(len=ESMF_MAXSTR), intent(out), allocatable :: stringList(:,:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n, j
    logical                              :: isPresent
    character(len=4096)                  :: attributeString
    character(len=ESMF_MAXSTR)           :: currString

    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_AttributeGet(cplComp, name=trim(attributeName), isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    call ESMF_AttributeGet(cplComp, trim(attributeName), value=attributeString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    n=1
    do i=1,len_trim(attributeString)
      if (attributeString(i:i)==',') n=n+1
    enddo

    if (n>0) allocate(stringList(n,2))
    do i=1,n
      j=index(attributeString,',')
      if (j>0) then
        currString=attributeString(1:j-1)
      else
        currString=trim(attributeString)
      endif
      write(attributeString,'(A)') attributeString(j+1:len_trim(attributeString))

      j=index(currString,'=')
      if (j>1) then
        stringList(i,1)=trim(currString(1:j-1))
        stringList(i,2)=trim(currString(j+1:len_trim(currString)))
      endif
    enddo

  end subroutine MOSSCO_cplCompAttributeGetList2

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompAttributeSetList1"
  subroutine MOSSCO_GridCompAttributeSetList1(gridComp, attributeName, stringList, rc)

    type(ESMF_gridComp), intent(inout)  :: gridComp
    character(len=*), intent(in)  :: attributeName
    character(len=*), intent(in), allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, j
    character(len=4096)                  :: attributeString

    if (present(rc)) rc=ESMF_SUCCESS
    if (.not.allocated(stringList)) return

    attributeString=''
    do i=lbound(stringList,1), ubound(stringList,1)
      if (len_trim(attributeString)>0) write(attributeString,'(A)') trim(attributeString)//','
      write(attributeString,'(A)') trim(attributeString)//trim(stringlist(i))
    enddo

    call ESMF_AttributeSet(gridComp, trim(attributeName), value=trim(attributeString), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_gridCompAttributeSetList1

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_gridCompAttributeSetList2"
  subroutine MOSSCO_gridCompAttributeSetList2(gridComp, attributeName, stringList, rc)

    type(ESMF_gridComp), intent(inout)  :: gridComp
    character(len=*), intent(in)  :: attributeName
    character(len=*), intent(in), allocatable :: stringList(:,:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, j
    character(len=4096)                  :: attributeString

    if (present(rc)) rc=ESMF_SUCCESS
    if (.not.allocated(stringList)) return

    attributeString=''
    do i=lbound(stringList,1), ubound(stringList,1)
      if (len_trim(attributeString)>0) write(attributeString,'(A)') trim(attributeString)//','
      write(attributeString,'(A)') trim(attributeString)//trim(stringlist(i,1))//'='//trim(stringlist(i,2))
    enddo

    call ESMF_AttributeSet(gridComp, trim(attributeName), value=attributeString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_gridCompAttributeSetList2

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompAttributeGetList1"
  subroutine MOSSCO_GridCompAttributeGetList1(gridComp, attributeName, stringList, rc)

    type(ESMF_gridComp), intent(in)  :: gridComp
    character(len=*), intent(in)  :: attributeName
    character(len=ESMF_MAXSTR), intent(out), allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n, j
    logical                              :: isPresent
    character(len=4096)                  :: attributeString

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_AttributeGet(gridComp, name=trim(attributeName), isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    call ESMF_AttributeGet(gridComp, trim(attributeName), value=attributeString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    n=1
    do i=1,len_trim(attributeString)
      if (attributeString(i:i)==',') n=n+1
    enddo

    if (allocated(stringList)) deallocate(stringList)
    if (n>0) allocate(stringList(n), stat=localrc)

    do i=1,n
      j=index(attributeString,',')
      if (j>0) then
        stringList(i)=attributeString(1:j-1)
      else
        stringList(i)=trim(attributeString)
      endif
      write(attributeString,'(A)') attributeString(j+1:len_trim(attributeString))
    enddo

  end subroutine MOSSCO_gridCompAttributeGetList1

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompAttributeGetList2"
  subroutine MOSSCO_GridCompAttributeGetList2(gridComp, attributeName, stringList, rc)

    type(ESMF_gridComp), intent(in)  :: gridComp
    character(len=*), intent(in)  :: attributeName
    character(len=ESMF_MAXSTR), intent(out), allocatable :: stringList(:,:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n, j
    logical                              :: isPresent
    character(len=4096)                  :: attributeString
    character(len=ESMF_MAXSTR)           :: currString

    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_AttributeGet(gridComp, name=trim(attributeName), isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    call ESMF_AttributeGet(gridComp, trim(attributeName), value=attributeString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    n=1
    do i=1,len_trim(attributeString)
      if (attributeString(i:i)==',') n=n+1
    enddo

    if (allocated(stringList)) deallocate(stringList)
    if (n>0) allocate(stringList(n,2), stat=localrc)
    do i=1,n
      j=index(attributeString,',')
      if (j>0) then
        currString=attributeString(1:j-1)
      else
        currString=trim(attributeString)
      endif
      write(attributeString,'(A)') attributeString(j+1:len_trim(attributeString))

      j=index(currString,'=')
      if (j>1) then
        stringList(i,1)=trim(currString(1:j-1))
        stringList(i,2)=trim(currString(j+1:len_trim(currString)))
      endif
    enddo

  end subroutine MOSSCO_GridCompAttributeGetList2

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompAttributeGetForeignGrid"
  subroutine MOSSCO_StateAttributeGetForeignGrid(state, grid, kwe, name, rc)

    type(ESMF_State), intent(inout)           :: state
    type(ESMF_Grid), intent(inout)            :: grid
    logical, intent(in), optional             :: kwe
    character(len=*), intent(in), optional    :: name
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    integer(ESMF_KIND_I4)              :: localrc, rc_
    character(len=ESMF_MAXSTR)         :: name_='foreign_grid_field_name'
    character(len=ESMF_MAXSTR)         :: fieldName, message
    type(ESMF_Field)                   :: field
    logical                            :: isPresent

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    if (present(kwe)) rc = rc_
    if (present(name)) name_ = name

    call ESMF_AttributeGet(state, name=trim(name_), &
      isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      if (present(rc)) rc = ESMF_RC_NOT_FOUND
      return
    endif

    call ESMF_AttributeGet(state, name=trim(name_), value=fieldName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (trim(fieldName) == 'none') return

    call ESMF_StateGet(state,  trim(fieldName), field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message, '(A)') '  obtains grid from field with name '//trim(fieldName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_FieldGet(field, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    return

  end subroutine MOSSCO_StateAttributeGetForeignGrid

end module mossco_attribute
