module mossco_component

use esmf
implicit none

interface MOSSCO_CompExit
    module procedure MOSSCO_CplCompExit
    module procedure MOSSCO_GridCompExit
end interface

contains

  subroutine MOSSCO_CplCompExit(cplComp, rc)
  
    type(ESMF_CplComp), intent(in)    :: cplComp
    integer, intent(out)              :: rc

    integer(ESMF_KIND_I4)   :: phase
    character(ESMF_MAXSTR)  :: message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Clock)        :: clock
    type(ESMF_Method_Flag)  :: method
    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    
    call ESMF_CplCompGet(cplComp, name=name, clockIsPresent=clockIsPresent, &
      currentMethod=method, currentPhase=phase, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Check for clock presence
    if (clockIsPresent) then
      call ESMF_CplCompGet(cplComp, clock=clock, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write(message,'(A)') trim(timestring)//' '//trim(name)
    else
      write(message,'(A)') '------------------- '//trim(name)
    endif
    
    if (method == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' ran'
    elseif (method == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initialized'
    elseif (method == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalized'
    endif

		write(message,'(A,I1)') trim(message)//' phase ',phase
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)  
  
  end subroutine MOSSCO_CplCompExit
  
  subroutine MOSSCO_GridCompExit(GridComp, rc)
  
    type(ESMF_GridComp), intent(in)    :: GridComp
    integer, intent(out)              :: rc

    integer(ESMF_KIND_I4)   :: phase
    character(ESMF_MAXSTR)  :: message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Clock)        :: clock
    type(ESMF_Method_Flag)  :: method
    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    
    call ESMF_GridCompGet(GridComp, name=name, clockIsPresent=clockIsPresent, &
      currentMethod=method, currentPhase=phase, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Check for clock presence
    if (clockIsPresent) then
      call ESMF_GridCompGet(GridComp, clock=clock, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write(message,'(A)') trim(timestring)//' '//trim(name)
    else
      write(message,'(A)') '------------------- '//trim(name)
    endif
    
    if (method == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' ran'
    elseif (method == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initialized'
    elseif (method == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalized'
    endif

		write(message,'(A,I1)') trim(message)//' phase ',phase
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)  
  
  end subroutine MOSSCO_GridCompExit
 
end module mossco_component
