!> @brief Implementation of additional ESMF memory allocation utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2016, 2017, 2018 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "mossco_memory.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module mossco_memory

  use esmf

  implicit none

  public :: MOSSCO_Reallocate
  private

  interface MOSSCO_Reallocate
    module procedure MOSSCO_FieldListReallocate
    module procedure MOSSCO_ItemTypeListReallocate
    module procedure MOSSCO_StringListReallocate
    module procedure MOSSCO_StringList2Reallocate
  end interface MOSSCO_Reallocate

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldListReallocate"
  subroutine MOSSCO_FieldListReallocate(fieldList, fieldCount, kwe, keep, owner, rc)

    type(ESMF_Field), intent(inout), allocatable     :: fieldList(:)
    integer(ESMF_KIND_I4), intent(in)                :: fieldCount
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional                    :: keep
    integer(ESMF_KIND_I4), intent(out), optional     :: rc
    character(len=*), intent(in), optional           :: owner

    integer(ESMF_KIND_I4)                   :: rc_, localrc, listSize
    logical                                 :: keep_
    character(len=ESMF_MAXPATHLEN)          :: message
    type(ESMF_Field),  allocatable          :: tempList(:)
    character(len=len(owner))               :: owner_

    rc_ = ESMF_SUCCESS
    keep_ = .true.
    owner_ = '--'
    if (present(keep)) keep_ = keep
    if (present(rc)) rc = rc_
    if (present(kwe)) localrc = ESMF_SUCCESS
    if (present(owner)) call stringCopy(owner_, owner)
    localrc = ESMF_SUCCESS

    ! Purposely deallocate a field upon fieldCount < 1
    if (fieldCount < 1) then
      if (allocated(fieldList)) deallocate(fieldList, stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for fieldList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
      endif
      return
    endif

    ! Deallocate if not keep
    if (.not.keep_) then
      if (allocated(fieldList)) deallocate(fieldList, stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for fieldList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
        return
      endif
    endif

    if (allocated(fieldList)) then
      ! Don't do anything if requested size is equal
      listSize = size(fieldList)
      if (listSize == fieldCount) return

      if (allocated(tempList)) deallocate(tempList, stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for temporary fieldList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
      endif

      allocate(tempList(listSize), stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot allocate memory for temporary fieldList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
      endif

      tempList(:) = fieldList(:)
      deallocate(fieldList, stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for fieldList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
      endif

      allocate(fieldList(fieldCount), stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot allocate memory for fieldList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
      endif

      if (fieldCount < listSize) then
        fieldList(1:fieldCount) = tempList(1:fieldCount)
      else
        fieldList(1:listSize) = tempList(1:listSize)
      endif
    else
      allocate(fieldList(fieldCount), stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot allocate memory for fieldList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
      endif
    endif

    if (allocated(tempList)) deallocate(tempList)
    if (localrc /= 0) then
      write(message,'(A)') trim(owner_)//' cannot deallocate memory for temporary fieldList'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
    endif

  end subroutine MOSSCO_FieldListReallocate

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ItemTypeListReallocate"
  subroutine MOSSCO_ItemTypeListReallocate(itemTypeList, itemCount, kwe, keep, owner, rc)

    type(ESMF_StateItem_Flag), intent(inout), allocatable :: itemTypeList(:)
    integer(ESMF_KIND_I4), intent(in)                :: itemCount
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional                    :: keep
    integer(ESMF_KIND_I4), intent(out), optional     :: rc
    character(len=*), intent(in), optional           :: owner

    integer(ESMF_KIND_I4)                   :: rc_, localrc, listSize
    logical                                 :: keep_
    character(len=ESMF_MAXPATHLEN)          :: message
    type(ESMF_StateItem_Flag),  allocatable :: tempList(:)
    character(len=ESMF_MAXSTR)              :: owner_

    owner_ = '--'
    rc_ = ESMF_SUCCESS
    keep_ = .true.
    if (present(keep)) keep_ = keep
    if (present(rc)) rc = rc_
    if (present(kwe)) localrc = ESMF_SUCCESS
    if (present(owner)) call stringCopy(owner_, owner)
    localrc = ESMF_SUCCESS

    ! Purposely deallocate upon itemCount < 1
    if (itemCount < 1) then
      if (allocated(itemTypeList)) deallocate(itemTypeList, stat=localrc)
      if (localrc > 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for itemTypeList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
      endif
      return
    endif

    ! Deallocate if not keep
    if (.not.keep_) then
      if (allocated(itemTypeList)) deallocate(itemTypeList, stat=localrc)
      if (localrc > 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for itemTypeList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
        return
      endif
    endif

    if (allocated(itemTypeList)) then
      ! Don't do anything if requested size is equal
      listSize = size(itemTypeList)
      if (listSize == itemCount) return

      if (allocated(tempList)) deallocate(tempList, stat=localrc)
      if (localrc > 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for temporary itemTypeList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
        return
      endif

      allocate(tempList(listSize), stat=localrc)
      if (localrc > 0) then
        write(message,'(A)') trim(owner_)//' cannot allocate memory for temporary itemTypeList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
        return
      endif

      tempList(:) = itemTypeList(:)
      deallocate(itemTypeList, stat=localrc)
      if (localrc > 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for itemTypeList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
        return
      endif

      allocate(itemTypeList(itemCount), stat=localrc)
      if (localrc > 0) then
        write(message,'(A)') trim(owner_)//' cannot allocate memory for itemTypeList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_ALLOCATE
        return
      endif

      if (itemCount < listSize) then
        itemTypeList(1:itemCount) = tempList(1:itemCount)
      else
        itemTypeList(1:listSize) = tempList(1:listSize)
      endif
    else
      allocate(itemTypeList(itemCount), stat=localrc)
      if (localrc > 0) then
        write(message,'(A)') trim(owner_)//' cannot allocate memory for itemTypeList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_ALLOCATE
        return
      endif
    endif

    if (allocated(tempList)) deallocate(tempList)
    if (localrc > 0) then
      write(message,'(A)') trim(owner_)//' cannot deallocate memory for temporary itemTypeList'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
      return
    endif

  end subroutine MOSSCO_ItemTypeListReallocate

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StringListReallocate"
  subroutine MOSSCO_StringListReallocate(stringList, itemCount, kwe, keep, owner, rc)

    character(len=ESMF_MAXSTR), intent(inout), allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(in)            :: itemCount
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional                :: keep
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    character(len=*), intent(in), optional       :: owner


    integer(ESMF_KIND_I4)                    :: rc_, localrc, listSize, i
    logical                                  :: keep_
    character(len=ESMF_MAXSTR)               :: message
    character(len=ESMF_MAXSTR),  allocatable :: tempList(:)
    character(len=ESMF_MAXSTR)               :: owner_

    owner_ = '--'
    rc_ = ESMF_SUCCESS
    keep_ = .true.
    if (present(keep)) keep_ = keep
    if (present(rc)) rc = rc_
    if (present(kwe)) localrc = ESMF_SUCCESS
    if (present(owner)) call stringCopy(owner_, owner)
    localrc = ESMF_SUCCESS

    ! Purposely deallocate upon itemCount < 1
    if (itemCount < 1) then
      if (allocated(stringList)) deallocate(stringList, stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for stringList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
      endif
      return
    endif

    ! Deallocate if not keep
    if (.not.keep_) then
      if (allocated(stringList)) deallocate(stringList, stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for stringList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
        return
      endif
    endif

    if (allocated(stringList)) then
      ! Don't do anything if requested size is equal
      listSize = size(stringList)
      if (listSize == itemCount) return

      if (allocated(tempList)) deallocate(tempList, stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for temporary stringList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
        return
      endif

      allocate(tempList(listSize), stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot allocate memory for temporary stringList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_ALLOCATE
        return
      endif

      do i=1,minval((/itemCount, listSize/))
        call stringCopy(tempList(i), stringList(i))
      enddo

      deallocate(stringList, stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for stringList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
        return
      endif

      allocate(stringList(itemCount), stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot allocate memory for stringList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_ALLOCATE
        return
      endif

      do i=1,minval((/itemCount, listSize/))
        call stringCopy(stringList(i), tempList(i))
      enddo

    else
      allocate(stringList(itemCount), stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot allocate memory for stringList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_ALLOCATE
        return
      endif
    endif

    if (allocated(tempList)) deallocate(tempList)
    if (localrc /= 0) then
      write(message,'(A)') trim(owner_)//' cannot deallocate memory for temporary stringList'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
      return
    endif

  end subroutine MOSSCO_StringListReallocate

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StringList2Reallocate"
  subroutine MOSSCO_StringList2Reallocate(stringList, itemCount, kwe, keep, owner, rc)

    character(len=ESMF_MAXSTR), intent(inout), allocatable :: stringList(:,:)
    integer(ESMF_KIND_I4), intent(in)            :: itemCount
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional                :: keep
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    character(len=*), intent(in), optional           :: owner

    integer(ESMF_KIND_I4)                    :: rc_, localrc, listSize
    logical                                  :: keep_
    character(len=ESMF_MAXSTR)               :: message
    character(len=ESMF_MAXSTR),  allocatable :: tempList(:,:)
    character(len=ESMF_MAXSTR)               :: owner_

    owner_ = '--'
    rc_ = ESMF_SUCCESS
    keep_ = .true.
    if (present(keep)) keep_ = keep
    if (present(rc)) rc = rc_
    if (present(kwe)) localrc = ESMF_SUCCESS
    localrc = ESMF_SUCCESS

    ! Purposely deallocate upon itemCount < 1
    if (itemCount < 1) then
      if (allocated(stringList)) deallocate(stringList, stat=localrc)
      if (localrc /= 0) then
        write(message,'(A)') trim(owner_)//' cannot deallocate memory for stringList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_MEM_DEALLOCATE
      endif
      return
    endif

    ! Deallocate if not keep
    if (.not.keep_) then
      if (allocated(stringList)) deallocate(stringList)
    endif

    if (allocated(stringList)) then
      ! Don't do anything if requested size is equal
      listSize = size(stringList)
      if (listSize == itemCount) return
      if (allocated(tempList)) deallocate(tempList)
      allocate(tempList(listSize, listSize), stat=localrc)
      tempList(:,:) = stringList(:,:)
      deallocate(stringList)
      allocate(stringList(itemCount,itemCount), stat=localrc)
      if (itemCount < listSize) then
        stringList(1:itemCount,1:itemCount) = tempList(1:itemCount,1:itemCount)
      else
        stringList(1:listSize,1:listSize) = tempList(1:listSize,1:listSize)
      endif
    else
      allocate(stringList(itemCount,itemCount), stat=localrc)
    endif

    if (allocated(tempList)) deallocate(tempList)

    if (localrc /= 0) then
      write(message,'(A,I5)') '  failed to allocate memory for stringList size ',itemCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_StringList2Reallocate

#undef  ESMF_METHOD
#define ESMF_METHOD "stringCopy"
!> The internal subroutine stringCopy is a copy of the publicly available
!> interface MOSSCOstringCopy.  It resides here to avoid circular use of
!> modules, such that the mossco_memory module has no dependencies
  subroutine stringCopy(to, from, rc)

    character(len=*), intent(inout)    :: to
    character(len=*), intent(in)       :: from
    integer(ESMF_KIND_I4), optional    :: rc

    integer(ESMF_KIND_I4)   :: toLen, fromLen

    toLen = len(to)
    fromLen = len(from)
    to(:)=''

    if (toLen >= fromLen) then
      to(1:fromLen) = from(1:fromLen)
      return
    endif

    fromLen = len_trim(from)
    if (toLen >= fromLen) then
      to(1:fromLen) = from(1:fromLen)
      return
    endif

    to(1:toLen) = from(1:toLen)

  end subroutine stringCopy

end module mossco_memory
