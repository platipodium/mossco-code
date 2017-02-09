!> @brief Implementation of ESMF Config utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
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
use mossco_memory

implicit none

private
public MOSSCO_ConfigGet

interface MOSSCO_ConfigGet
  module procedure MOSSCO_ConfigGetLogical
  module procedure MOSSCO_ConfigGetInt4
  module procedure MOSSCO_ConfigGetInt8
  module procedure MOSSCO_ConfigGetString
  module procedure MOSSCO_ConfigGetReal8
  module procedure MOSSCO_ConfigGetReal4
  module procedure MOSSCO_ConfigGetListInt4
  module procedure MOSSCO_ConfigGetListInt8
  module procedure MOSSCO_ConfigGetListReal4
  module procedure MOSSCO_ConfigGetListReal8
  module procedure MOSSCO_ConfigGetStringList
  module procedure MOSSCO_ConfigGetStringTable
  module procedure MOSSCO_ConfigGetFileStringTable
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
  subroutine MOSSCO_ConfigGetInt4(config, label, value, kwe, defaultValue, rc)

    type(ESMF_Config), intent(inout)       :: config
    character(len=*), intent(in)           :: label
    integer(ESMF_KIND_I4), intent(inout)   :: value
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    integer(ESMF_KIND_I4), intent(in), optional      :: defaultValue
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: message

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      if (present(defaultValue)) then
        value=defaultValue
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
    write(message,'(A,I5)') trim(message)//' ', value
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetInt4

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetInt8"
  subroutine MOSSCO_ConfigGetInt8(config, label, value, kwe, defaultValue, rc)

    type(ESMF_Config), intent(inout)       :: config
    character(len=*), intent(in)           :: label
    integer(ESMF_KIND_I8), intent(inout)   :: value
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    integer(ESMF_KIND_I8), intent(in), optional      :: defaultValue
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: message

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      if (present(defaultValue)) then
        value=defaultValue
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
    write(message,'(A,I9)') trim(message)//' ', value
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

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetReal4"
  subroutine MOSSCO_ConfigGetReal4(config, label, value, kwe, defaultValue, rc)

    type(ESMF_Config), intent(inout)       :: config
    character(len=*), intent(in)           :: label
    real(ESMF_KIND_R4), intent(inout)   :: value
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    real(ESMF_KIND_R4), intent(in), optional      :: defaultValue
    integer(ESMF_KIND_I4), intent(out), optional  :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: message

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      if (present(defaultValue)) then
        value=defaultValue
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
    write(message,'(A,ES10.3)') trim(message)//' ', value
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetReal4

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetReal8"
  subroutine MOSSCO_ConfigGetReal8(config, label, value, kwe, defaultValue, rc)

    type(ESMF_Config), intent(inout)       :: config
    character(len=*), intent(in)           :: label
    real(ESMF_KIND_R8), intent(inout)   :: value
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    real(ESMF_KIND_R8), intent(in), optional      :: defaultValue
    integer(ESMF_KIND_I4), intent(out), optional  :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: message

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      if (present(defaultValue)) then
        value=defaultValue
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
    write(message,'(A,ES10.3)') trim(message)//' ', value
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetReal8

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListInt4"
  subroutine MOSSCO_ConfigGetListInt4(config, label, value, rc)

    type(ESMF_Config), intent(inout)                  :: config
    character(len=*), intent(in)                      :: label
    integer(ESMF_KIND_I4), intent(inout), allocatable :: value(:)
    integer(ESMF_KIND_I4), intent(out), optional      :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: string

    rc_ = ESMF_SUCCESS

    if (allocated(value)) deallocate(value)

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

    if (allocated(value)) deallocate(value)
    allocate(value(n), stat=localrc)

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, n
      call ESMF_ConfigGetAttribute(config, value=string, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      read(string, *, iostat=localrc) value(i)
      !> @todo check return code
    enddo

    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetListInt4

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListReal4"
  subroutine MOSSCO_ConfigGetListReal4(config, label, value, rc)

    type(ESMF_Config), intent(inout)                  :: config
    character(len=*), intent(in)                      :: label
    real(ESMF_KIND_R4), intent(inout), allocatable    :: value(:)
    integer(ESMF_KIND_I4), intent(out), optional      :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: string

    rc_ = ESMF_SUCCESS

    if (allocated(value)) deallocate(value)

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

    if (allocated(value)) deallocate(value)
    allocate(value(n), stat=localrc)

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, n
      call ESMF_ConfigGetAttribute(config, value=string, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      read(string, *, iostat=localrc) value(i)
      !> @todo check return code
    enddo

    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetListReal4

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListReal8"
  subroutine MOSSCO_ConfigGetListReal8(config, label, value, rc)

    type(ESMF_Config), intent(inout)                  :: config
    character(len=*), intent(in)                      :: label
    real(ESMF_KIND_R8), intent(inout), allocatable    :: value(:)
    integer(ESMF_KIND_I4), intent(out), optional      :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: string

    rc_ = ESMF_SUCCESS

    if (allocated(value)) deallocate(value)

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

    if (allocated(value)) deallocate(value)
    allocate(value(n), stat=localrc)

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, n
      call ESMF_ConfigGetAttribute(config, value=string, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      read(string, *, iostat=localrc) value(i)
      !> @todo check return code
    enddo

    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetListReal8

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetListInt8"
  subroutine MOSSCO_ConfigGetListInt8(config, label, value, rc)

    type(ESMF_Config), intent(inout)                  :: config
    character(len=*), intent(in)                      :: label
    integer(ESMF_KIND_I8), intent(inout), allocatable :: value(:)
    integer(ESMF_KIND_I4), intent(out), optional      :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n
    logical                              :: isPresent
    character(len=ESMF_MAXSTR)           :: string

    rc_ = ESMF_SUCCESS

    if (allocated(value)) deallocate(value)

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

    if (allocated(value)) deallocate(value)
    allocate(value(n), stat=localrc)

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, n
      call ESMF_ConfigGetAttribute(config, value=string, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      read(string, *, iostat=localrc) value(i)
      !> @todo check return code
    enddo

    if (present(rc)) rc = rc_

  end subroutine MOSSCO_ConfigGetListInt8

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetStringList"
  subroutine MOSSCO_ConfigGetStringList(config, label, value, rc)

    type(ESMF_Config), intent(inout)  :: config
    character(len=*), intent(in)  :: label
    character(len=*), intent(inout), allocatable :: value(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n
    logical                              :: isPresent

    if (present(rc)) rc=ESMF_SUCCESS

    if (allocated(value)) deallocate(value)

    !> Test whether to read a Table
    call ESMF_ConfigFindLabel(config, label=trim(label)//'::', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call MOSSCO_ConfigGetStringListTable(config, label, value, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      return

    endif

    call ESMF_ConfigFindLabel(config, label=trim(label)//':', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call MOSSCO_ConfigGetStringListList(config, label, value, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      return
    endif

  end subroutine MOSSCO_ConfigGetStringList

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetStringListTable"
  subroutine MOSSCO_ConfigGetStringListTable(config, label, value, rc)

    type(ESMF_Config), intent(inout)  :: config
    character(len=*), intent(in)  :: label
    character(len=*), intent(inout), allocatable :: value(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, j, rowCount, columnCount
    logical                              :: isPresent, isTableEnd
    character(len=ESMF_MAXSTR)           :: message

    if (present(rc)) rc = ESMF_SUCCESS
    if (allocated(value)) deallocate(value)

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
    if (columnCount /= 1) return

    write(message,'(A,I1,A,I1,A)') '  reading table "'//trim(label)//'::" (',rowCount,' x ', columnCount,')'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    allocate(value(rowCount), stat=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ConfigFindLabel(config, label=trim(label)//'::', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i = 1, rowCount
      call ESMF_ConfigNextLine(config, tableEnd=isTableEnd, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isTableEnd) exit

      call ESMF_ConfigGetAttribute(config, value(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message, '(A)') '  '//trim(value(i))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    enddo

    if (present(rc)) rc = localrc

  end subroutine MOSSCO_ConfigGetStringListTable

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetStringListList"
  subroutine MOSSCO_ConfigGetStringListList(config, label, value, rc)

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

  end subroutine MOSSCO_ConfigGetStringListList

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetStringTableKeyValue"
  subroutine MOSSCO_ConfigGetStringTableKeyValue(config, label, value, kwe, sep, rc)

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

  end subroutine MOSSCO_ConfigGetStringTableKeyValue

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetStringTableTable"
!> Obtains a two-dimensional list of strings obtained from a Table in
!> ESMF_Config format
  subroutine MOSSCO_ConfigGetStringTableTable(config, label, value, rc)

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

    call ESMF_ConfigFindLabel(config, label=trim(label)//'::', rc=localrc)
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

        if (j == 1) then
          write(message, '(A)') '  '//trim(value(i,j))
        else
          call MOSSCO_MessageAdd(message, ' '//trim(value(i,j)))
        endif
      enddo
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

    if (present(rc)) rc=localrc

  end subroutine MOSSCO_ConfigGetStringTableTable

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetStringTable"
!> Obtains a two-dimensional list of strings obtained from a Table in
!> ESMF_Config format or a key/value list
  subroutine MOSSCO_ConfigGetStringTable(config, label, value, kwe, sep, rc)

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
      call MOSSCO_ConfigGetStringTableTable(config, label=trim(label), value=value, rc=localrc)
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
      call MOSSCO_ConfigGetStringTableKeyValue(config, label=trim(label), &
        value=value, sep=sep_, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    endif
    if (present(rc)) rc = localrc

  end subroutine MOSSCO_ConfigGetStringTable

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetStringListTable1"
!> Obtains a one-dimensional list of strings obtained from a Table in
!> ESMF_Config format
  subroutine MOSSCO_ConfigGetStringListTable1(config, label, value, rc)

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

  end subroutine MOSSCO_ConfigGetStringListTable1

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ConfigGetFileStringTable"
!> Obtains a two-dimensional list of strings obtained from a Table in
!> ESMF_Config format, but without ESMF_Config routines (workaround)
  subroutine MOSSCO_ConfigGetFileStringTable(fileName, label, value, rc)

    character(len=*), intent(in)   :: fileName
    character(len=*), intent(in)   :: label
    character(len=*), intent(inout), allocatable :: value(:,:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, j, rowCount, columnCount
    integer(ESMF_KIND_I4)                :: lun, bufferSize = 10
    logical                              :: isPresent, isTableEnd
    character(len=ESMF_MAXSTR)           :: message, string
    character(len=ESMF_MAXSTR), allocatable :: stringList(:)

    if (present(rc)) rc=ESMF_SUCCESS
    if (allocated(value)) deallocate(value)

    call ESMF_UtilIOUnitGet(lun, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    open (unit=lun, file=trim(fileName), status='old',    &
       access='sequential', form='formatted', action='read' )

    rowCount = 0
    columnCount = 2
    localrc = ESMF_SUCCESS

    ! Find the label in the file
    do while (localrc == ESMF_SUCCESS)
      read (lun, *, iostat=localrc) string
      if (localrc /= ESMF_SUCCESS) exit

      i = index(string, trim(label)//'::')
      if (i < 1) cycle
      call ESMF_LogWrite('  found label '//trim(string), ESMF_LOGMSG_INFO)
      isPresent = .true.
      exit
    enddo

    if (.not.isPresent) then
      if (present(rc)) rc = ESMF_RC_NOT_FOUND
      return
    endif

    ! Read into a string Buffer
    call MOSSCO_Reallocate(stringList, bufferSize, keep=.false., rc=localrc)

    rowCount = 0
    do while (localrc == ESMF_SUCCESS)
      read (lun, *, iostat=localrc) string
      if (localrc /= ESMF_SUCCESS) exit

      i = index(string, '::')
      if (i > 0) exit
      rowCount = rowCount + 1

      if (rowCount > bufferSize) then
        bufferSize = 2 * bufferSize
        call MOSSCO_Reallocate(stringList, buffersize, keep=.true., rc=localrc)
      endif

      stringList(rowCount) = trim(string)
      write(message,'(A,I2,A)') '  found item ',rowCount,' '//trim(stringList(rowCount))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

    if (rowCount < 0) then
      if (present(rc)) rc = ESMF_RC_NOT_FOUND
      return
    endif

    allocate(value(rowCount,columnCount))

    if (rowCount * columnCount < 1) then
      if (present(rc)) rc = ESMF_RC_NOT_FOUND
      return
    endif

    if (columnCount /= 2) then
      if (present(rc)) rc = ESMF_RC_ARG_BAD
      return
    endif

    do i = 1, rowCount
      string = stringList(i)
      j = index(string,' ')
      value(i,1) = string(1:j-1)
      value(i,2) = trim(adjustl(string(1:j-1)))
    enddo

    if (present(rc)) rc=localrc

  end subroutine MOSSCO_ConfigGetFileStringTable

end module mossco_config
