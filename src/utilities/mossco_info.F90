
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
use mossco_strings, only : MOSSCO_StringCopy, intformat

implicit none

private
public MOSSCO_InfoLog, MOSSCO_InfoLogObject, MOSSCO_InfoCopy

interface MOSSCO_InfoLogObject
  module procedure MOSSCO_InfoLogGridComp
  module procedure MOSSCO_InfoLogCplComp
  module procedure MOSSCO_InfoLogField
end interface MOSSCO_InfoLogObject

interface MOSSCO_InfoCopy
    module procedure MOSSCO_InfoCopyAll
    module procedure MOSSCO_InfoCopyKey
end interface MOSSCO_InfoCopy

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
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
  endif

end subroutine MOSSCO_InfoCopyKey

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoCopyAll"
subroutine MOSSCO_InfoCopyAll(to, from, kwe, overwrite, rc)

  type(ESMF_Info), intent(inout)                  :: to
  type(ESMF_Info), intent(in)                     :: from
  type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
  logical, intent(in), optional                    :: overwrite
  integer(ESMF_KIND_I4), intent(out), optional     :: rc

  integer(ESMF_KIND_I4)        :: localrc, rc_, i, infoSize
  character(len=ESMF_MAXSTR)   :: key
  logical                      :: overwrite_, isPresent
  type(ESMF_TypeKind_Flag)     :: typeKind 

  overwrite_ = .false.
  rc_ = ESMF_SUCCESS
  if (present(rc)) rc = rc_
  if (present(kwe)) rc_ = ESMF_SUCCESS
  if (present(overwrite)) overwrite_ = overwrite

  call ESMF_InfoGet(from, size=infoSize, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  do i=1, infoSize
    call ESMF_InfoGet(from, idx=i, ikey=key, typekind=typeKind, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(to, key=key, isPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent .and. (.not.overwrite_)) cycle

    call MOSSCO_InfoCopy(to, from, key, typekind=typeKind, rc=localrc)
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

    root_ = '/'
    if (present(root)) call MOSSCO_StringCopy(root_, root, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, size=infoSize, key=trim(root), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    write(format,'(A)') '(A,'//intformat(infoSize)//',A)'
    write(message, trim(format)) trim(prefix_)//' has ',infoSize,' attributes'
    if (present(log)) then 
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
    else 
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif 

    do i=1, infoSize
        call ESMF_InfoGet(info, idx=i, ikey=key, typekind=typeKind, rc=localrc)
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
            !if (present(log)) then 
            !  call MOSSCO_InfoLog(info, root=trim(root_)//trim(key), log=log,  &
            !    prefix=trim(prefix_)//':'//trim(key), rc=localrc)
            !  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            !else 
            !  call MOSSCO_InfoLog(info, root=trim(root_)//trim(key), &
            !    prefix=trim(prefix_)//':'//trim(key), rc=localrc)
            !  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            !endif 
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

end module mossco_info
