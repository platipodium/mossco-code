
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
use mossco_memory
use mossco_strings, only : MOSSCO_StringCopy

implicit none

private
public MOSSCO_InfoLog, MOSSCO_InfoLogObject

interface MOSSCO_InfoLogObject
  module procedure MOSSCO_InfoLogGridComp
  module procedure MOSSCO_InfoLogCplComp
end interface MOSSCO_InfoLogObject

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_InfoLog"
!> This private subroutine is called through the MOSSCO_InfoLog Interface
  subroutine MOSSCO_InfoLog(info, kwe, prefix, log, rc)

    type(ESMF_Info)                  :: info
    logical,intent(in ),optional     :: kwe
    character(len=*), optional       :: prefix
    type(ESMF_Log), optional         :: log
     integer(ESMF_KIND_I4), optional :: rc

    integer(ESMF_KIND_I4)            :: localrc, rc_, infoSize, i, int4
    type(ESMF_Log)                   :: log_
    character(len=ESMF_MAXSTR)       :: key, prefix_, message, string
    type(ESMF_TypeKind_Flag)         :: typeKind
    integer(ESMF_KIND_I8)            :: int8
    logical                          :: bool
    real(ESMF_KIND_R8)               :: real8
    real(ESMF_KIND_R4)               :: real4
    
    if (present(prefix)) call MOSSCO_StringCopy(prefix, prefix_, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, size=infoSize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    write(message, '(A,I3,A)') trim(prefix)//' has ',infoSize,' attributes'
    if (present(log)) then 
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
    else 
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif 

    do i=1, infoSize
        call ESMF_InfoGet(info, idx=i, ikey=key, typekind=typeKind, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        write(message, '(A,I3,A,A,A)') trim(prefix)//':(',i,')',key,'='
        if (typeKind==ESMF_TYPEKIND_CHARACTER) then 
            call ESMF_InfoGet(info, key=key, value=string, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            write(message,'(A,A)') trim(message), trim(string)
        elseif (typeKind==ESMF_TYPEKIND_I4) then 
            call ESMF_InfoGet(info, key=key, value=int4, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            write(message,'(A,I3)') trim(message), int4
        elseif (typeKind==ESMF_TYPEKIND_I8) then 
            call ESMF_InfoGet(info, key=key, value=int8, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            write(message,'(A,I3)') trim(message), int8
        elseif (typeKind==ESMF_TYPEKIND_R4) then 
            call ESMF_InfoGet(info, key=key, value=real4, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            write(message,'(A,F3.2)') trim(message), real4
        elseif (typeKind==ESMF_TYPEKIND_R8) then 
            call ESMF_InfoGet(info, key=key, value=real8, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            write(message,'(A,F3.2)') trim(message), real8
        elseif (typeKind==ESMF_TYPEKIND_LOGICAL) then 
            call ESMF_InfoGet(info, key=key, value=bool, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
            if (bool) then 
                write(message,'(A,A)') trim(message), '.true.'
            else
                write(message,'(A,A)') trim(message), '.false.'
            endif
        else 
            write(message,'(A,A,I2)') trim(message), &
              ' retrieval not implemented for typeKind ', typeKind
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

end module mossco_info
