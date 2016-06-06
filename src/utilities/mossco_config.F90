!> @brief Implementation of ESMF Config utilities
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
#define ESMF_FILENAME "mossco_config.F90"

module mossco_config

use esmf
use mossco_strings

implicit none

private
public MOSSCO_ConfigGet

interface MOSSCO_ConfigGet
  module procedure MOSSCO_ConfigGetLogical
  module procedure MOSSCO_ConfigGetInt4
  module procedure MOSSCO_ConfigGetInt8
  module procedure MOSSCO_ConfigGetString
  module procedure MOSSCO_ConfigGetReal8
  module procedure MOSSCO_ConfigGetListInt4
  module procedure MOSSCO_ConfigGetListString
  module procedure MOSSCO_ConfigGetListString2
end interface MOSSCO_ConfigGet

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetLogical"
  subroutine MOSSCO_ConfigGetLogical(config, label, value, kwe, defaultValue, rc)

    type(ESMF_Config), intent(inout)       :: config
    character(len=*), intent(in)           :: label
    logical, intent(inout)                 :: value
    type(ESMF_KeywordEnforcer), optional   :: kwe
    logical, intent(in), optional          :: defaultValue
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: message

    rc_ = ESMF_SUCCESS
    if (present(kwe)) localrc = ESMF_SUCCESS

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      if (present(defaultValue)) then
        value=defaultValue
        if (present(rc)) rc = ESMF_SUCCESS
      else
        if (present(rc)) rc = ESMF_RC_NOT_FOUND
      endif
      return
    endif

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ConfigGetAttribute(config, value=value, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A)') '  found '//trim(label)//':'
    write(message,'(A,L)') trim(message)//' ', value
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetLogical

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetInt4"
  subroutine MOSSCO_ConfigGetInt4(config, label, value, rc)

    type(ESMF_Config), intent(inout)       :: config
    character(len=*), intent(in)           :: label
    integer(ESMF_KIND_I4), intent(inout)   :: value
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: message

    rc_ = ESMF_SUCCESS

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ConfigGetAttribute(config, value=value, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A)') '  found '//trim(label)//':'
    write(message,'(A,I5)') trim(message)//' ', value
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetInt4

#undef  ESMF_METHOD
#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetInt8"
  subroutine MOSSCO_ConfigGetInt8(config, label, value, rc)

    type(ESMF_Config), intent(inout)       :: config
    character(len=*), intent(in)           :: label
    integer(ESMF_KIND_I8), intent(inout)   :: value
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: message

    rc_ = ESMF_SUCCESS

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ConfigGetAttribute(config, value=value, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A)') '  found '//trim(label)//':'
    write(message,'(A,ES10.3)') trim(message)//' ', dble(value)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetInt8

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetString"
  subroutine MOSSCO_ConfigGetString(config, label, value, kwe, defaultValue, rc)

    type(ESMF_Config), intent(inout)             :: config
    character(len=*), intent(in)                 :: label
    character(len=*), intent(inout)              :: value
    type(ESMF_KeywordEnforcer), optional         :: kwe
    character(len=*), intent(in), optional       :: defaultValue
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: message

    rc_ = ESMF_SUCCESS
    if (present(kwe)) localrc = ESMF_SUCCESS

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      if (present(defaultValue)) then
        value=trim(defaultValue)
        if (present(rc)) rc = ESMF_SUCCESS
      else
        if (present(rc)) rc = ESMF_RC_NOT_FOUND
      endif
      return
    endif

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ConfigGetAttribute(config, value=value, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A)') '  found '//trim(label)//': '//trim(value)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetString

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetReal8"
  subroutine MOSSCO_ConfigGetReal8(config, label, value, rc)

    type(ESMF_Config), intent(inout)       :: config
    character(len=*), intent(in)           :: label
    real(ESMF_KIND_R8), intent(inout)   :: value
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: message

    rc_ = ESMF_SUCCESS

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ConfigGetAttribute(config, value=value, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A)') '  found '//trim(label)//':'
    write(message,'(A,ES10.3)') trim(message)//' ', value
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetReal8

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListInt4"
  subroutine MOSSCO_ConfigGetListInt4(config, label, valueList, rc)

    type(ESMF_Config), intent(inout)  :: config
    character(len=*), intent(in)  :: label
    integer(ESMF_KIND_I4), intent(inout), allocatable :: valueList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: string

    rc_ = ESMF_SUCCESS

    if (allocated(valueList)) deallocate(valueList)

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    n = ESMF_ConfigGetLen(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (n < 1) then
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    if (allocated(valueList)) deallocate(valueList)
    allocate(valueList(n), stat=localrc)

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, n
      call ESMF_ConfigGetAttribute(config, value=string, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      read(string, *, iostat=localrc) valueList(i)
      !> @todo check return code
    enddo

    if (allocated(valueList)) deallocate(valueList)
    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetListInt4

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListString"
  subroutine MOSSCO_ConfigGetListString(config, label, value, rc)

    type(ESMF_Config), intent(inout)  :: config
    character(len=*), intent(in)  :: label
    character(len=*), intent(inout), allocatable :: value(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n
    logical                              :: isPresent

    if (present(rc)) rc=ESMF_SUCCESS

    if (allocated(value)) deallocate(value)

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    n=ESMF_ConfigGetLen(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (n<=0) return
    allocate(value(n))

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, n
      call ESMF_ConfigGetAttribute(config, value=value(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

  end subroutine MOSSCO_ConfigGetListString

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListStringKeyValue"
  subroutine MOSSCO_ConfigGetListStringKeyValue(config, label, value, kwe, sep, rc)

    type(ESMF_Config), intent(inout)                       :: config
    character(len=*), intent(in)                           :: label
    character(len=ESMF_MAXSTR), intent(inout), allocatable :: value(:,:)
    type(ESMF_KeywordEnforcer), optional                   :: kwe
    character(len=*), intent(in), optional                 :: sep
    integer(ESMF_KIND_I4), intent(inout), optional         :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, j, n
    logical                              :: isPresent
    character(ESMF_MAXSTR)               :: currString, message
    character(len=1)                     :: sep_

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(kwe)) localrc = ESMF_SUCCESS
    if (present(sep)) sep_ = sep(1:1)

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    n=ESMF_ConfigGetLen(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (n < 1) return

    write(message,'(A,I1,A,I1,A)') '  reading key'//sep_//'value list "'//trim(label)//':" (',n,')'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    if (allocated(value)) deallocate(value)
    allocate(value(n, 2), stat=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    value(:,:) = ' '

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, n
      call ESMF_ConfigGetAttribute(config, value=currString, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      j=index(currString,sep_)
      if (j>1) then
         value(i,1)=currString(1:j-1)
         value(i,2)=currString(j+1:len_trim(currString))
      endif
      write(message,'(A)') '  '//trim(value(i,1))
      call MOSSCO_MessageAdd(message, ' '//sep_)
      call MOSSCO_MessageAdd(message, ' '//trim(value(i,2)))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

  end subroutine MOSSCO_ConfigGetListStringKeyValue

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListStringTable2"
!> Obtains a two-dimensional list of strings obtained from a Table in
!> ESMF_Config format
  subroutine MOSSCO_ConfigGetListStringTable2(config, label, value, rc)

    type(ESMF_Config), intent(inout)  :: config
    character(len=*), intent(in)   :: label
    character(len=ESMF_MAXSTR), intent(out), allocatable :: value(:,:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, j, rowCount, columnCount
    logical                              :: isPresent, isTableEnd
    character(len=ESMF_MAXSTR)           :: message

    if (present(rc)) rc=ESMF_SUCCESS
    if (allocated(value)) deallocate(value)

    call ESMF_ConfigFindLabel(config, label=trim(label)//'::', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    call ESMF_ConfigFindLabel(config, label=trim(label)//'::', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ConfigGetDim(config, label=trim(label)//'::', &
      lineCount=rowCount, columnCount=columnCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rowCount * columnCount < 1) return

    if (columnCount /= 2) then
      if (present(rc)) rc = ESMF_RC_ARG_BAD
      return
    endif

    write(message,'(A,I1,A,I1,A)') '  reading table "'//trim(label)//'::" (',rowCount,' x ', columnCount,')'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    if (allocated(value)) deallocate(value)
    allocate(value(rowCount,columnCount), stat=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i = 1, rowCount
      call ESMF_ConfigNextLine(config, tableEnd=isTableEnd, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isTableEnd) exit

      do j = 1, columnCount
        call ESMF_ConfigGetAttribute(config, value(i,j), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_MessageAdd(message, '  '//trim(value(i,j)))
      enddo
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    enddo

    if (present(rc)) rc=localrc

  end subroutine MOSSCO_ConfigGetListStringTable2

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListString2"
!> Obtains a two-dimensional list of strings obtained from a Table in
!> ESMF_Config format or a key/value list
  subroutine MOSSCO_ConfigGetListString2(config, label, value, kwe, sep, rc)

    type(ESMF_Config), intent(inout)                       :: config
    character(len=*), intent(in)                           :: label
    character(len=ESMF_MAXSTR), intent(inout), allocatable :: value(:,:)
    type(ESMF_KeywordEnforcer), optional                   :: kwe
    character(len=*), intent(in), optional                 :: sep
    integer(ESMF_KIND_I4), intent(inout), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_
    logical                              :: isPresent
    character(len=1)                     :: sep_

    if (present(rc))  rc=ESMF_SUCCESS
    if (present(kwe)) localrc = ESMF_SUCCESS
    sep_ = '='
    if (present(sep)) sep_ = sep(1:1)
    if (allocated(value)) deallocate(value)

    ! First check for table
    call ESMF_ConfigFindLabel(config, label=trim(label)//'::', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call MOSSCO_ConfigGetListStringTable2(config, label=trim(label), value=value, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (present(rc)) rc = localrc
      return
    endif

    ! Alternatively check for list
    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call MOSSCO_ConfigGetListStringKeyValue(config, label=trim(label), &
        value=value, sep=sep_, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    endif
    if (present(rc)) rc = localrc

  end subroutine MOSSCO_ConfigGetListString2

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListStringTable1"
!> Obtains a one-dimensional list of strings obtained from a Table in
!> ESMF_Config format
  subroutine MOSSCO_ConfigGetListStringTable1(config, label, value, rc)

    type(ESMF_Config), intent(inout)  :: config
    character(len=*), intent(in)   :: label
    character(len=ESMF_MAXSTR), intent(out), allocatable :: value(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, rowCount, columnCount
    logical                              :: isPresent, isTableEnd
    character(len=ESMF_MAXSTR)           :: message

    if (present(rc)) rc=ESMF_SUCCESS
    if (allocated(value)) deallocate(value)

    call ESMF_ConfigFindLabel(config, label=trim(label)//'::', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    call ESMF_ConfigFindLabel(config, label=trim(label)//'::', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ConfigGetDim(config, label=trim(label)//'::', &
      lineCount=rowCount, columnCount=columnCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rowCount * columnCount < 1) return

    if (columnCount /= 1) then
      if (present(rc)) rc = ESMF_RC_ARG_BAD
      return
    endif

    write(message,'(A,I1,A,I1,A)') '  reading table "'//trim(label)//'::" (',rowCount,' x ', columnCount,')'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    if (allocated(value)) deallocate(value)
    allocate(value(rowCount), stat=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    value(:) = ' '

    do i = 1, rowCount
      call ESMF_ConfigNextLine(config, tableEnd=isTableEnd, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isTableEnd) exit

      call ESMF_ConfigGetAttribute(config, value(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_MessageAdd(message, '  '//trim(value(i)))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

    if (present(rc)) rc=localrc

  end subroutine MOSSCO_ConfigGetListStringTable1

end module mossco_config
