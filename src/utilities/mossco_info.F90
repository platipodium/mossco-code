
!> @brief Implementation of extensions to the ESMF Info utilities
!
!  This computer program is part of MOSSCO.
!> @copyright 2022 Helmholtz-Zentrum Hereon
!> @author Carsten Lemmen <carsten.lemmen@hereon.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "mossco_info.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifndef VARLEN
#define VARLEN ESMF_MAXSTR
#endif

module mossco_info

use esmf
use mossco_strings
use mossco_memory

implicit none

private
public MOSSCO_InfoLog, MOSSCO_InfoLogObject, MOSSCO_InfoCopy
public MOSSCO_InfoString, MOSSCO_InfoIdentical
public MOSSCO_InfoGet, MOSSCO_InfoSet

interface MOSSCO_InfoLogObject
  module procedure MOSSCO_InfoLogGridComp
  module procedure MOSSCO_InfoLogCplComp
  module procedure MOSSCO_InfoLogField
end interface MOSSCO_InfoLogObject

interface MOSSCO_InfoCopy
    module procedure MOSSCO_InfoCopyAll
    module procedure MOSSCO_InfoCopyKey
end interface MOSSCO_InfoCopy

interface MOSSCO_InfoSet
    module procedure MOSSCO_InfoSetCharPtr
end interface MOSSCO_InfoSet

interface MOSSCO_InfoGet
    module procedure MOSSCO_InfoGetCharPtr
end interface MOSSCO_InfoGet

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoCopyKey"
subroutine MOSSCO_InfoCopyKey(to, from, key, kwe, typeKind, rc)

  type(ESMF_Info), intent(inout)                   :: to
  type(ESMF_Info), intent(in)                      :: from
  character(len=*), intent(in)                     :: key
  type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
  type(ESMF_TypeKind_Flag), intent(in), optional    :: typeKind
  integer(ESMF_KIND_I4), intent(out), optional     :: rc

  integer(ESMF_KIND_I4)        :: localrc, rc_, i, int4
  logical                      :: isPresent, bool
  type(ESMF_TypeKind_Flag)     :: typeKind_
  character(len=ESMF_MAXSTR)   :: string, message

  integer(ESMF_KIND_I8) :: int8
  real(ESMF_KIND_R4)    :: real4
  real(ESMF_KIND_R8)    :: real8

  rc_ = ESMF_SUCCESS
  if (present(rc)) rc = rc_
  if (present(kwe)) rc_ = ESMF_SUCCESS

  if (present(typeKind)) then 
    typeKind_ = typeKind
  else
    call ESMF_InfoGet(from, key=key, typeKind=typeKind_, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  endif 

  if (typeKind_ == ESMF_TYPEKIND_I4) then
    call ESMF_InfoGet(from, key=key, value=int4 , rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(to, key=key, value=int4, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  elseif (typeKind_ == ESMF_TYPEKIND_I8) then
    call ESMF_InfoGet(from, key=key, value=int8 , rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(to, key=key, value=int8, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  elseif (typeKind_ == ESMF_TYPEKIND_R4) then
    call ESMF_InfoGet(from, key=key, value=real4 , rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(to, key=key, value=real4, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  elseif (typeKind_ == ESMF_TYPEKIND_R8) then
    call ESMF_InfoGet(from, key=key, value=real8 , rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(to, key=key, value=real8, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  elseif (typeKind_ == ESMF_TYPEKIND_CHARACTER) then
    call ESMF_InfoGet(from, key=key, value=string , rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(to, key=key, value=trim(string), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    
  elseif (typeKind_ == ESMF_TYPEKIND_LOGICAL) then
    call ESMF_InfoGet(from, key=key, value=bool , rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(to, key=key, value=bool, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  else
    write(message,'(A)') '-- not yet implemented deep copy of attribute '//trim(key)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)

    call ESMF_LogWrite(trim(ESMF_InfoDump(from, rc=localrc)), ESMF_LOGMSG_INFO)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  endif

end subroutine MOSSCO_InfoCopyKey

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoCopyAll"
recursive subroutine MOSSCO_InfoCopyAll(to, from, kwe, root, overwrite, rc)

  type(ESMF_Info), intent(inout)                  :: to
  type(ESMF_Info), intent(in)                     :: from
  type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
  logical, intent(in), optional                    :: overwrite
  integer(ESMF_KIND_I4), intent(out), optional     :: rc
  character(len=*), optional, intent(in)           :: root

  integer(ESMF_KIND_I4)        :: localrc, rc_, i, infoSize
  character(len=ESMF_MAXSTR)   :: key, root_
  logical                      :: overwrite_, isPresent
  type(ESMF_TypeKind_Flag)     :: typeKind 

  overwrite_ = .false.
  localrc = ESMF_SUCCESS
  rc_ = ESMF_SUCCESS
  root_ = ''
  if (present(rc)) rc = rc_
  if (present(kwe)) rc_ = ESMF_SUCCESS
  if (present(overwrite)) overwrite_ = overwrite
  if (present(root)) call MOSSCO_StringCopy(root_, root, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  call ESMF_InfoGet(from, size=infoSize, key=trim(root_), &
    attnestflag=ESMF_ATTNEST_ON, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  !write(0,*) 'At root=',trim(root_), ' there are ',infoSize, ' attributes'

  do i=1, infoSize

    call ESMF_InfoGet(from, key=trim(root_), idx=i, ikey=key, typekind=typeKind, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !write(0,*) 'At root=',trim(root_), ' the',i, 'th attribute has key', key

    !call ESMF_InfoGet(from, key=trim(root_)//'/'//trim(key), isPresent=isPresent, rc=localrc)
    !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    !if (.not.isPresent) cycle

    call ESMF_InfoGet(to, key=trim(root_)//'/'//trim(key), isPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent .and. (.not.overwrite_)) cycle

    if ( & !typeKind == ESMF_TYPEKIND_I1 .or. typeKind == ESMF_TYPEKIND_I2 .or. &
        typeKind == ESMF_TYPEKIND_I4 .or. typeKind == ESMF_TYPEKIND_I8 .or. &
        typeKind == ESMF_TYPEKIND_R4 .or. typeKind == ESMF_TYPEKIND_R8 .or. &
        typeKind == ESMF_TYPEKIND_LOGICAL .or. typeKind == ESMF_TYPEKIND_CHARACTER &
      ) then ! all regular typekinds
      !write(0,*) 'Copying regular attribute ',trim(root_)//'/'//trim(key),' of kind', typeKind
      call MOSSCO_InfoCopy(to, from, key=trim(root_)//'/'//trim(key), typekind=typeKind, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    else 
      ! enter recursion on map
      !write(0,*) 'Calling recursion on root ',trim(root_)//'/'//trim(key)
      call MOSSCO_InfoCopy(to, from, root=trim(root_)//'/'//trim(key), overwrite=overwrite_, rc=localrc )
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif 
  enddo

end subroutine MOSSCO_InfoCopyAll

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoLog"
!> This private subroutine is called through the MOSSCO_InfoLog Interface
  recursive subroutine MOSSCO_InfoLog(info, kwe, prefix, root, log, rc)

    type(ESMF_Info)                  :: info
    logical,intent(in ),optional     :: kwe
    character(len=*), optional       :: prefix
    character(len=*), optional       :: root
    type(ESMF_Log), optional         :: log
    integer(ESMF_KIND_I4), optional  :: rc


    integer(ESMF_KIND_I4)            :: localrc, rc_, infoSize, i, int4
    type(ESMF_Log)                   :: log_
    character(len=ESMF_MAXSTR)       :: key, prefix_, message, string, root_
    character(len=ESMF_MAXSTR)       :: format
    type(ESMF_TypeKind_Flag)         :: typeKind
    integer(ESMF_KIND_I8)            :: int8
    logical                          :: bool
    real(ESMF_KIND_R8)               :: real8
    real(ESMF_KIND_R4)               :: real4
    
    prefix_ = '--'
    if (present(prefix)) call MOSSCO_StringCopy(prefix_, prefix, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    root_ = ''
    if (present(root)) call MOSSCO_StringCopy(root_, root, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, size=infoSize, key=trim(root_), &
      attnestflag=ESMF_ATTNEST_ON, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    write(format,'(A)') '(A,'//intformat(infoSize)//',A)'
    write(message, trim(format)) trim(prefix_)//' has ',infoSize,' attributes'
    if (present(log)) then 
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
    else 
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif 

    do i=1, infoSize
        call ESMF_InfoGet(info, idx=i, ikey=key, typekind=typeKind, &
          attnestflag=ESMF_ATTNEST_ON, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        write(format,'(A)') '(A,'//intformat(i)//',A)'
        write(message, trim(format)) trim(prefix_)//'(',i,'):'//trim(key)
        if (typeKind==ESMF_TYPEKIND_CHARACTER) then 
            call ESMF_InfoGet(info, key=key, value=string, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            write(message,'(A,A)') trim(message)//' (C) = '//trim(string)
        elseif (typeKind==ESMF_TYPEKIND_I4) then 
            call ESMF_InfoGet(info, key=key, value=int4, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

            write(format,'(A)') '(A,'//intformat(int4)//')'
            write(message,trim(format)) trim(message)//' (I4) = ', int4
        elseif (typeKind==ESMF_TYPEKIND_I8) then 
            call ESMF_InfoGet(info, key=key, value=int8, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

            write(format,'(A)') '(A,'//intformat(int8)//')'
            !write(message,trim(format)) trim(message)//' (I8) = ', int8
            write(message,*) trim(message), int8
        elseif (typeKind==ESMF_TYPEKIND_R4) then 
            call ESMF_InfoGet(info, key=key, value=real4, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            write(message,'(A,F3.2)') trim(message)//' (R4) = ', real4
        elseif (typeKind==ESMF_TYPEKIND_R8) then 
            call ESMF_InfoGet(info, key=key, value=real8, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            !write(message,'(A,F3.2)') trim(message)//' (R8) = ', real8
            write(message,*) trim(message), real8
        elseif (typeKind==ESMF_TYPEKIND_LOGICAL) then 
            call ESMF_InfoGet(info, key=key, value=bool, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            if (bool) then 
                write(message,'(A)') trim(message)//' (L) = .true.'
            else
                write(message,'(A)') trim(message)//' (L) = .false.'
            endif
        else 
            write(message,'(A,A)') trim(message)//' (nested)'
            !> @todo recursivion needs to be fixed
        !     write(0,*) 'root=', trim(root_)//'/'//trim(key)
        !     write(0,*) 'prefix=', trim(prefix_)//':'//trim(key)
        !     if (present(log)) then 
        !       call MOSSCO_InfoLog(info, root=trim(root_)//'/'//trim(key), log=log,  &
        !         prefix=trim(prefix_)//':'//trim(key), rc=localrc)
        !       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        !     else 
        !       call MOSSCO_InfoLog(info, root=trim(root_)//'/'//trim(key), &
        !         prefix=trim(prefix_)//':'//trim(key), rc=localrc)
        !       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        !     endif 
        endif 

        if (present(log)) then 
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
        else 
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif 
    enddo

    if (present(rc)) rc=localrc

  end subroutine MOSSCO_InfoLog

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoLogCplComp"
!> This private subroutine is called through the MOSSCO_InfoLogObject Interface
  subroutine MOSSCO_InfoLogCplComp(comp, kwe, log, rc)

    type(ESMF_CplComp)               :: comp
    logical,intent(in ),optional     :: kwe
    type(ESMF_Log), optional         :: log
     integer(ESMF_KIND_I4), optional :: rc

    integer(ESMF_KIND_I4)            :: localrc, rc_
    type(ESMF_Info)                  :: info
    character(len=ESMF_MAXSTR)       :: name
    
    call ESMF_CplCompGet(comp, name=name, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    
    call ESMF_InfoGetFromHost(comp, info=info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_InfoLog(info, prefix=trim(name), log=log, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (present(rc)) rc=localrc

  end subroutine MOSSCO_InfoLogCplComp
  
#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoLogGridComp"
  !> This private subroutine is called through the MOSSCO_InfoLogObject Interface
    subroutine MOSSCO_InfoLogGridComp(comp, kwe, log, rc)
  
      type(ESMF_GridComp)               :: comp
      logical,intent(in ),optional     :: kwe
      type(ESMF_Log), optional         :: log
       integer(ESMF_KIND_I4), optional :: rc
  
      integer(ESMF_KIND_I4)            :: localrc, rc_
      type(ESMF_Info)                  :: info
      character(len=ESMF_MAXSTR)       :: name
      
      call ESMF_GridCompGet(comp, name=name, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      
      call ESMF_InfoGetFromHost(comp, info=info, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  
      call MOSSCO_InfoLog(info, prefix=trim(name), log=log, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  
      if (present(rc)) rc=localrc
  
    end subroutine MOSSCO_InfoLogGridComp

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoLogField"
  !> This private subroutine is called through the MOSSCO_InfoLogObject Interface
    subroutine MOSSCO_InfoLogField(field, kwe, log, rc)
  
      type(ESMF_Field)                 :: field
      logical,intent(in ),optional     :: kwe
      type(ESMF_Log), optional         :: log
       integer(ESMF_KIND_I4), optional :: rc
  
      integer(ESMF_KIND_I4)            :: localrc, rc_
      type(ESMF_Info)                  :: info
      character(len=ESMF_MAXSTR)       :: name
      
      call ESMF_FieldGet(field, name=name, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      
      call ESMF_InfoGetFromHost(field, info=info, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  
      call MOSSCO_InfoLog(info, prefix=trim(name), log=log, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  
      if (present(rc)) rc=localrc
  
    end subroutine MOSSCO_InfoLogField

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoGetCharPtr"
  subroutine MOSSCO_InfoGetCharPtr(info, key, stringPtr, kwe, rc)

    type(ESMF_Info), intent(in)   :: info
    character(len=*), intent(in)  :: key
    character(len=ESMF_MAXSTR), intent(out), pointer :: stringPtr(:)
    logical, intent(in), optional :: kwe
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                :: localrc, rc_, i, n, j
    logical                              :: isPresent
    character(len=ESMF_MAXSTR), allocatable  :: stringList(:)

    localrc = ESMF_SUCCESS
    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_InfoGet(info, key=key, isPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  
    if (.not.isPresent) return

    call ESMF_InfoGetAlloc(info, key, values=stringList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  
    allocate(stringPtr(size(stringList)))
    do i=1, size(stringList)
      call MOSSCO_StringCopy(stringPtr(i), stringList(i), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    enddo 
    deallocate(stringlist)

  end subroutine MOSSCO_InfoGetCharPtr

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoSetCharPtr"
  !> This private subroutine is called through the MOSSCO_InfoLogObject Interface
    subroutine MOSSCO_InfoSetCharPtr(info, key, values, kwe, force, pkey, rc)
  
      type(ESMF_Info), intent(inout)               :: info
      character(len=*), intent(in)                 :: key
      character(len=*), pointer, intent(in)        :: values(:)
      logical, intent(in ),optional                :: kwe
      logical, intent(in), optional                :: force
      character(len=*), intent(in), optional       :: pkey
      integer(ESMF_KIND_I4), intent(out), optional :: rc
  
      integer(ESMF_KIND_I4)      :: localrc, rc_, i
      logical                    :: force_
      character(len=ESMF_MAXSTR) :: pkey_
      character(len=ESMF_MAXSTR), allocatable :: valueList(:)
      
      localrc = ESMF_SUCCESS
      if (present(rc)) rc = localrc 
      if (present(force)) force_ = force
      if (present(pkey)) call MOSSCO_StringCopy(pkey_, pkey, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.associated(values)) return 

      allocate(valueList(size(values)))
      do i=1,size(values)
        call MOSSCO_StringCopy(valueList(i), values(i), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      enddo

      !> @todo complete interface
      !call ESMF_InfoSet(info, key, valueList, force=force_, pkey=pkey_, rc=localrc)
      call ESMF_InfoSet(info, key, valueList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    end subroutine MOSSCO_InfoSetCharPtr

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoString"
  subroutine MOSSCO_InfoString(info, key, string, kwe, rc)

    type(ESMF_Info), intent(in)              :: info
    character(len=*), intent(in)             :: key
    character(len=*), intent(inout)          :: string
    type(ESMF_KeywordEnforcer), optional     :: kwe
    integer(ESMF_KIND_I4), optional, intent(out) :: rc

    integer(ESMF_KIND_I4)                :: localrc, int4, rc_, i
    logical                              :: isPresent
    real(ESMF_KIND_R8)                   :: real8
    real(ESMF_KIND_R4)                   :: real4
    integer(ESMF_KIND_I8)                :: int8
    type(ESMF_TypeKind_Flag)             :: typeKind
    character(len=ESMF_MAXSTR)           :: message
    logical                              :: bool

    localrc = ESMF_SUCCESS

    if (present(kwe)) localrc = ESMF_SUCCESS
    if (present(rc)) rc = localrc

    call ESMF_InfoGet(info, key=key, isPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (.not.isPresent) then 
      localrc = ESMF_RC_NOT_FOUND
      if (present(rc)) rc = localrc
      return
    endif 

    call ESMF_InfoGet(info, key=key, typeKind=typeKind, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (typeKind == ESMF_TYPEKIND_CHARACTER) then
      call ESMF_InfoGet(info, key=key, value=string, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    elseif (typeKind == ESMF_TYPEKIND_R8) then
      call ESMF_InfoGet(info, key=key, value=real8, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      write(string,*) real8
    elseif (typeKind == ESMF_TYPEKIND_R4) then
      call ESMF_InfoGet(info, key=key, value=real4, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      write(string,*) real4
    elseif (typeKind == ESMF_TYPEKIND_I8) then
      call ESMF_InfoGet(info, key=key, value=int8, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      write(string,*) int8
    elseif (typeKind == ESMF_TYPEKIND_I4) then
      call ESMF_InfoGet(info, key=key, value=int4, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      write(string,*) int4
    elseif (typeKind == ESMF_TYPEKIND_LOGICAL) then
      call ESMF_InfoGet(info, key=key, value=bool, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      string = '.false.'
      if (bool) string = '.true.'
    else 
      write(message,'(A)')  'key of non-implemented type '
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    endif

  end subroutine MOSSCO_InfoString

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoIdentical"
  function MOSSCO_InfoIdentical(to, from, kwe, &
    verbose, exclude, owner, differList, rc) result(differCount)

    type(ESMF_Info), intent(in)                 :: to, from
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    character(len=*), dimension(*), optional     :: exclude(:)
    character(len=ESMF_MAXSTR), allocatable, optional, intent(out)   :: differList(:)
    character(len=*), optional, intent(in)       :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    integer(ESMF_KIND_I4)                        :: differCount
    logical, intent(in), optional                :: verbose

    integer(ESMF_KIND_I4)                        :: localrc, rc_
    integer(ESMF_KIND_I4)                        :: fromSize, toSize, i, j, count
    logical                                      :: isPresent, verbose_
    character(len=ESMF_MAXSTR)                   :: message, key
    character(len=ESMF_MAXSTR)                   :: fromString, toString, owner_
    type(ESMF_TypeKind_Flag)                     :: importTypeKind, exportTypeKind
    character(len=ESMF_MAXSTR), allocatable      :: excludeList(:)

    owner_ = '--'
    verbose_ = .false.
    rc_ = ESMF_SUCCESS
    differCount = 0

    if (present(owner)) call MOSSCO_StringCopy(owner_, owner)
    if (present(verbose)) verbose_ = verbose
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    if (present(exclude)) then
      count = ubound(exclude,1)-lbound(exclude,1)+1
      if (count>0) then
        call MOSSCO_Reallocate(excludeList, count, keep=.false., rc=localrc)
        excludeList(1:count) = exclude(:)
      endif
    else
      call MOSSCO_Reallocate(excludeList, 1, keep=.false., rc=localrc)
      !if (allocated(excludeList)) deallocate(excludeList, stat=localrc)
      !allocate(excludeList(1))
      excludeList(1) = 'creator'
    endif
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(from, size=fromSize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    if (fromSize == 0) return

    call ESMF_InfoGet(to, size=toSize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    if (toSize == 0) return

    if (present(differList)) then
      call MOSSCO_Reallocate(differList, fromSize, keep=.false.,  rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    do i = 1, fromSize

      call ESMF_InfoGet(from, idx=i, ikey=key, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      ! If this attribute's name is in the exclude list, then cycle to next attribute
      isPresent = .false.
      do j = lbound(excludeList,1), ubound(excludeList,1)
        if ( trim(excludeList(j)) /= trim(key) ) cycle
        isPresent = .true.
        exit
      enddo
      if (isPresent) cycle

      call ESMF_InfoGet(to, key=key, isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      if (.not.isPresent) cycle

      call MOSSCO_InfoSTring(from, key=key, string=fromString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      
      call MOSSCO_InfoSTring(to, key=key, string=tostring, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      
      if (trim(fromString) == trim(toString)) cycle
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      differCount = differCount + 1

      if (verbose_ .or. present(differList)) then
        write(message, '(A)') trim(owner_)
        call MOSSCO_MessageAdd(message, ' '//owner_)
        call MOSSCO_MessageAdd(message,':'//trim(key))
        call MOSSCO_MessageAdd(message,' '//trim(fromString))
        call MOSSCO_MessageAdd(message,' /= '//trim(toString))
      endif

      if (verbose_) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      if (present(differList)) then
        differList(differCount) = trim(message)
      endif

    enddo

    if (present(differList)) call MOSSCO_Reallocate(differList, differCount, &
      keep=.true., rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_Reallocate(excludeList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  end function MOSSCO_InfoIdentical

end module mossco_info
