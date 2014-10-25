module mossco_component

use esmf
implicit none

interface MOSSCO_CompExit
    module procedure MOSSCO_CplCompExit
    module procedure MOSSCO_GridCompExit
end interface

interface MOSSCO_CompEntry
    module procedure MOSSCO_CplCompEntry
    module procedure MOSSCO_GridCompEntry
end interface

contains

  subroutine MOSSCO_CplCompEntry(cplComp, parentClock, name, currTime, rc)
  
    type(ESMF_CplComp), intent(inout)    :: cplComp
    type(ESMF_Clock), intent(in)         :: parentClock
    character(ESMF_MAXSTR), intent(out)  :: name
    type(ESMF_Time), intent(out)         :: currTime
    integer, intent(out)                 :: rc

    integer(ESMF_KIND_I4)   :: petCount, localPet, phase
    character(ESMF_MAXSTR)  :: message, timeString
    logical                 :: clockIsPresent, configIsPresent, vmIsPresent
    type(ESMF_Clock)        :: clock
    type(ESMF_Vm)           :: vm
    type(ESMF_Method_Flag)  :: method
    type(ESMF_Context_Flag) :: context
    type(ESMF_Config)       :: config
    
    call ESMF_CplCompGet(cplComp, name=name, clockIsPresent=clockIsPresent, &
      configIsPresent=configIsPresent, vmIsPresent=vmIsPresent, localPet=localPet, &
      petCount=petCount, currentMethod=method, currentPhase=phase, contextFlag=context, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Check for clock presence and add if necessary
    if (clockIsPresent) then
      call ESMF_CplCompGet(cplComp, clock=clock, rc=rc)
    else
      clock = ESMF_ClockCreate(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_CplCompSet(cplComp, clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock, name=trim(name)//' clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Check for config presence
    if (configIsPresent) then
      call ESMF_CplCompGet(cplcomp, config=config, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      !!> @todo: what todo with this information?
    endif
    
    !! Check for vm presence
    if (vmIsPresent) then
      call ESMF_CplCompGet(cplComp, vm=vm, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_VmGet(vm, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      !!> @todo: what todo with this information?
    endif

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)
    
    if (method == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' running'
    elseif (method == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initializing'
    elseif (method == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalizing'
    endif

		write(message,'(A,I1,A)') trim(message)//' phase ',phase,' ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)  

  end subroutine MOSSCO_CplCompEntry
  
  subroutine MOSSCO_GridCompEntry(GridComp, parentClock, name, currTime, rc)
  
    type(ESMF_GridComp), intent(inout)    :: GridComp
    type(ESMF_Clock), intent(in)         :: parentClock
    character(ESMF_MAXSTR), intent(out)  :: name
    type(ESMF_Time), intent(out)         :: currTime
    integer, intent(out)                 :: rc

    integer(ESMF_KIND_I4)   :: petCount, localPet, phase
    character(ESMF_MAXSTR)  :: message, timeString
    logical                 :: clockIsPresent, configIsPresent, vmIsPresent
    type(ESMF_Clock)        :: clock
    type(ESMF_Vm)           :: vm
    type(ESMF_Method_Flag)  :: method
    type(ESMF_Context_Flag) :: context
    type(ESMF_Config)       :: config
    type(ESMF_TimeInterval) :: timeStep
    integer(ESMF_KIND_I8)   :: advanceCount
    
    call ESMF_GridCompGet(GridComp, name=name, clockIsPresent=clockIsPresent, &
      configIsPresent=configIsPresent, vmIsPresent=vmIsPresent, localPet=localPet, &
      petCount=petCount, currentMethod=method, currentPhase=phase, contextFlag=context, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Check for clock presence and add if necessary
    if (clockIsPresent) then
      call ESMF_GridCompGet(GridComp, clock=clock, rc=rc)
    else
      clock = ESMF_ClockCreate(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompSet(GridComp, clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock, name=trim(name)//' clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Check for config presence
    if (configIsPresent) then
      call ESMF_GridCompGet(Gridcomp, config=config, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      !!> @todo: what todo with this information?
    endif
    
    !! Check for vm presence
    if (vmIsPresent) then
      call ESMF_GridCompGet(GridComp, vm=vm, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_VmGet(vm, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      !!> @todo: what todo with this information?
    endif

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(clock, currTime=currTime, advanceCount=advanceCount, &
      timeStep=timeStep, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)
    
    if (method == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' running'
    elseif (method == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initializing'
    elseif (method == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalizing'
    endif

		write(message,'(A,I1)') trim(message)//' phase ',phase
    
    if (method == ESMF_METHOD_RUN) then
      write(message,'(A,I8.8,A)') trim(message)//' step ',advanceCount,' ...'
    else
      write(message,'(A)') trim(message)//' ...'
    endif    
    
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)  
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_GridCompEntry


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
