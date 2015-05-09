!> @brief Implementation of ESMF Component utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Knut Klingbeil <knut.klingbeil@io-warnemuende.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "mossco_component.F90"

#define MOSSCO_MAXLEN_COMPNAME 15

module mossco_component

use esmf
use mossco_strings
use mossco_state
use mossco_field

implicit none

interface MOSSCO_CompExit
    module procedure MOSSCO_CplCompExit
    module procedure MOSSCO_GridCompExit
end interface

interface MOSSCO_CompLog
    module procedure MOSSCO_GridCompLog
end interface

interface MOSSCO_CompEntry
    module procedure MOSSCO_CplCompEntry
    module procedure MOSSCO_GridCompEntry
end interface

public MOSSCO_CompExit
public MOSSCO_CompEntry
public MOSSCO_CompLog
public MOSSCO_GridCompExitLog
public MOSSCO_GridCompEntryLog

private

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CplCompEntry"
  subroutine MOSSCO_CplCompEntry(cplComp, parentClock, name, currTime, rc)

    type(ESMF_CplComp), intent(inout)              :: cplComp
    type(ESMF_Clock), intent(in)                   :: parentClock
    character(ESMF_MAXSTR), intent(out), optional  :: name
    type(ESMF_Time), intent(out), optional         :: currTime
    integer, intent(out), optional                 :: rc

    character(ESMF_MAXSTR)  :: name_
    type(ESMF_Time)         :: currTime_
    integer                 :: rc_

    integer(ESMF_KIND_I4)   :: petCount, localPet, phase, phaseCount, localrc
    character(ESMF_MAXSTR)  :: message, timeString
    logical                 :: clockIsPresent, configIsPresent, vmIsPresent
    logical                 :: phaseZeroFlag
    type(ESMF_Clock)        :: clock
    type(ESMF_Vm)           :: vm
    type(ESMF_Method_Flag)  :: method
    type(ESMF_Context_Flag) :: context
    type(ESMF_Config)       :: config
    type(ESMF_Time)         :: startTime, stopTime

    rc_=ESMF_SUCCESS

    call ESMF_CplCompGet(cplComp, name=name_, clockIsPresent=clockIsPresent, &
      configIsPresent=configIsPresent, vmIsPresent=vmIsPresent, localPet=localPet, &
      petCount=petCount, currentMethod=method, currentPhase=phase, contextFlag=context, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Check for clock presence and add if necessary
    if (clockIsPresent) then
      call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    else
      clock = ESMF_ClockCreate(parentClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_CplCompSet(cplComp, clock=clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockSet(clock, name=trim(name_), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Check for config presence
    if (configIsPresent) then
      call ESMF_CplCompGet(cplcomp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!> @todo: what todo with this information?
    endif

    !! Check for vm presence
    if (vmIsPresent) then
      call ESMF_CplCompGet(cplComp, vm=vm, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call ESMF_VmGet(vm, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!> @todo: what todo with this information?
    endif

    !! Synchronize clock with parent clock if local clock is present
    call ESMF_ClockGet(parentClock, currTime=currTime_, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (clockIsPresent) then
      call ESMF_ClockGet(clock, startTime=startTime, stopTime=stopTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (currTime_>stopTime) currTime_=stopTime

      call ESMF_ClockSet(clock, currTime=currTime_, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
      !  call ESMF_TimePrint(startTime, options='string')
      !  call ESMF_TimePrint(stopTime, options='string')
      !  call ESMF_TimePrint(currTime_, options='string')
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !endif

      call ESMF_ClockGet(clock, currTime=currTime_, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_TimeGet(currTime_,timeStringISOFrac=timestring)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A)') name_(:MOSSCO_MAXLEN_COMPNAME)//' '//trim(timestring)

    if (method == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' running'
    elseif (method == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initializing'
    elseif (method == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalizing'
    elseif (method == ESMF_METHOD_READRESTART) then
      write(message,'(A)') trim(message)//' readrestarting'
    else
      write(message,'(A)') trim(message)//' doing'
    endif

    !call ESMF_CplCompGetEPPhaseCount(cplComp, method, phaseCount, &
    !  phaseZeroFlag, rc)
    phaseCount=1 !>@todo for now we assume all couplers have only 1 phase
    write(message,'(A,I1,A,I1)') trim(message)//' phase ',phase,' of ',phaseCount

    if (present(rc)) rc=rc_
    if (present(currTime)) currTime=currTime_
    if (present(name)) name=trim(name_)

    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)
    call ESMF_LogFlush()

    return

  end subroutine MOSSCO_CplCompEntry

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompEntry"
  subroutine MOSSCO_GridCompEntry(GridComp, parentClock, keywordEnforcer, name, &
    currTime, importState, exportState, rc)

    type(ESMF_GridComp), intent(inout)             :: gridComp
    type(ESMF_Clock), intent(in)                   :: parentClock
    type(ESMF_KeywordEnforcer), optional           :: keywordEnforcer ! must use keywords below
    character(ESMF_MAXSTR), intent(out), optional  :: name
    type(ESMF_Time), intent(out), optional         :: currTime
    integer, intent(out), optional                 :: rc
    type(ESMF_State), intent(in), optional         :: importState, exportState

    character(ESMF_MAXSTR)  :: name_
    type(ESMF_Time)         :: currTime_
    integer                 :: rc_

    integer(ESMF_KIND_I4)   :: petCount, localPet, phase, localrc
    logical                 :: clockIsPresent, configIsPresent, vmIsPresent, isPresent
    type(ESMF_Clock)        :: clock
    type(ESMF_Vm)           :: vm
    type(ESMF_Method_Flag)  :: method
    type(ESMF_Context_Flag) :: context
    type(ESMF_Config)       :: config
    character(len=ESMF_MAXSTR) :: message
    type(ESMF_State)        :: state

    rc_=ESMF_SUCCESS

    call ESMF_GridCompGet(GridComp, name=name_, &
      configIsPresent=configIsPresent, vmIsPresent=vmIsPresent, localPet=localPet, &
      petCount=petCount, contextFlag=context, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> if importState and exportState are provided as arguments, check whether they agree with
    !> the gridComp's import- and exportState
    if (present(importState)) then
      call ESMF_GridCompGet(GridComp, importStateIsPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (isPresent) then
        call ESMF_GridCompGet(GridComp, importState=state, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (state /= importState) then
          write(message,'(A)')  trim(name_)//' importState differs from state given as argument'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          call MOSSCO_StateLog(state)
        endif
      endif
    endif

    !! Check for clock presence and add if necessary
    call ESMF_GridCompGet(gridComp, clockIsPresent=clockIsPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (.not. clockIsPresent) then
      clock = ESMF_ClockCreate(parentClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockSet(clock, name=trim(name_), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridCompSet(GridComp, clock=clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_GridCompEntryLog(gridComp,name=name_,currentMethod=method,currentPhase=phase, &
      clockIsPresent=clockIsPresent, clock=clock, currTime=currTime_)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Check for config presence
    if (configIsPresent) then
      call ESMF_GridCompGet(Gridcomp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!> @todo: what todo with this information?
    endif

    !! Check for vm presence
    if (vmIsPresent) then
      call ESMF_GridCompGet(GridComp, vm=vm, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_VmGet(vm, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!> @todo: what todo with this information?
    endif

    if (present(rc)) rc=rc_
    if (present(currTime)) currTime=currTime_
    if (present(name)) name=trim(trim(name_))

    call ESMF_LogFlush()
    return

  end subroutine MOSSCO_GridCompEntry


#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CplCompExit"
  subroutine MOSSCO_CplCompExit(cplComp, rc)

    type(ESMF_CplComp), intent(in)    :: cplComp
    integer, intent(out), optional    :: rc

    integer(ESMF_KIND_I4)   :: phase, phaseCount, localrc, rc_
    character(ESMF_MAXSTR)  :: message, timeString
    logical                 :: clockIsPresent, phaseZeroFlag
    type(ESMF_Clock)        :: clock
    type(ESMF_Method_Flag)  :: method
    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime

    rc_=ESMF_SUCCESS

    call ESMF_CplCompGet(cplComp, name=name, clockIsPresent=clockIsPresent, &
      currentMethod=method, currentPhase=phase, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Check for clock presence
    if (clockIsPresent) then
      call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockGet(clock, currTime=currTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      write(message,'(A)') name(:MOSSCO_MAXLEN_COMPNAME)//' '//trim(timestring)
    else
      write(message,'(A)') name(:MOSSCO_MAXLEN_COMPNAME)//' -------------------'
    endif

    if (method == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' ran'
    elseif (method == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initialized'
    elseif (method == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalized'
    elseif (method == ESMF_METHOD_READRESTART) then
      write(message,'(A)') trim(message)//' readrestarted'
    else
      write(message,'(A)') trim(message)//' did'
    endif

    !call ESMF_CplCompGetEPPhaseCount(cplComp, method, phaseCount, &
    !  phaseZeroFlag, rc)
    phaseCount=1 !>@todo for now we assume all couplers have only 1 phase

    write(message,'(A,I1,A,I1)') trim(message)//' phase ',phase,' of ',phaseCount
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_CplCompExit

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompExit"
  subroutine MOSSCO_GridCompExit(GridComp, rc)

    type(ESMF_GridComp), intent(in)    :: GridComp
    integer, intent(out), optional     :: rc

    integer  :: rc_

    rc_ = ESMF_SUCCESS

    call MOSSCO_GridCompExitLog(gridComp)

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_GridCompExit

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompEntryLog"
  subroutine MOSSCO_GridCompEntryLog(gridComp,kwe,name,currentMethod,currentPhase, &
                                     clockIsPresent,clock,currTime)
    type(ESMF_GridComp)   ,intent(in )          :: gridComp
    logical               ,intent(in ),optional :: kwe !keyword-enforcer
    character(ESMF_MAXSTR),intent(out),optional :: name
    type(ESMF_Method_Flag),intent(out),optional :: currentMethod
    integer               ,intent(out),optional :: currentPhase
    logical               ,intent(out),optional :: clockIsPresent
    type(ESMF_Clock)      ,intent(out),optional :: clock
    type(ESMF_Time)       ,intent(out),optional :: currTime

    character(ESMF_MAXSTR) :: myName,timestring,message, formatstring
    type(ESMF_Method_Flag) :: cMethod
    integer                :: cPhase,phaseCount,petCount
    logical                :: have_clock, phaseZeroFlag
    type(ESMF_Clock)       :: myClock
    type(ESMF_Time)        :: cTime
    integer(ESMF_KIND_I4)  :: rc, localrc, rc_, days, hours, minutes, seconds
    integer(ESMF_KIND_I8)  :: advanceCount
    type(ESMF_TimeInterval) :: timeStep

    rc=ESMF_SUCCESS

    call ESMF_GridCompGet(gridComp,name=myName,currentMethod=cMethod,currentPhase=cPhase, &
                          clockIsPresent=have_clock)
    if (present(name          )) name           = myName
    if (present(currentMethod )) currentMethod  = cMethod
    if (present(currentPhase  )) currentPhase   = cPhase
    if (present(clockIsPresent)) clockIsPresent = have_clock

    call ESMF_GridCompGetEPPhaseCount(gridComp, cMethod, phaseCount=phaseCount, &
      phaseZeroFlag=phaseZeroFlag, rc=localrc)

    if (have_clock) then
      call ESMF_GridCompGet(gridComp,clock=myClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (present(clock)) clock = myClock
      call ESMF_ClockGet(myClock,currTime=cTime, timeStep=timeStep, advanceCount=advanceCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (present(currTime)) currTime = cTime
      call ESMF_TimeGet(cTime,timeStringISOFrac=timestring, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_TimeIntervalGet(timeStep, h=hours, m=minutes, s=seconds, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      write(timestring,'(A)') '-------------------'
    end if

    write(message,'(A)') myName(:MOSSCO_MAXLEN_COMPNAME)//' '//trim(timestring)
    if (cMethod == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initializing'
    else if (cMethod == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' running'
    else if (cMethod == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalizing'
    elseif (cMethod == ESMF_METHOD_READRESTART) then
      write(message,'(A)') trim(message)//' readrestarting'
    else
      write(message,'(A)') trim(message)//' doing'
    end if

    write(message,'(A,I1,A,I1)') trim(message)//' phase ',cPhase,' of ',phaseCount

    if (cMethod.eq.ESMF_METHOD_INITIALIZE .and. cPhase.eq.1) then
      call ESMF_GridCompGet(gridComp,petCount=petCount)
      write(formatstring,'(A)') '(A,'//intformat(petCount)//',A)'
      write(message,formatstring) trim(message)//' on ',petCount,' PETs'
    elseif (cMethod.eq.ESMF_METHOD_RUN .and. have_clock) then
      !! The code below is broken, as the timeSteop in the internal Clock is not anymore the
      !! timestep that the component is run for (with the new controlClock), thus we write out
      !! only the advanceCount and not the timeStep information
      ! write(formatstring,'(A)') '(A,'//intformat(advanceCount)//',A,'//intformat(hours)//',A,I2.2,A,I2.2,A)'
      ! write(message, formatstring) trim(message)//' step ',advanceCount,' dt=',hours,':',minutes,':',seconds,' hours'
      write(formatstring,'(A)') '(A,'//intformat(advanceCount)//')'
      write(message, formatstring) trim(message)//' step ',advanceCount
    end if
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

  end subroutine MOSSCO_GridCompEntryLog


#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompExitLog"
  subroutine MOSSCO_GridCompExitLog(gridComp,kwe,name,currentMethod,currentPhase, &
                                     clockIsPresent,clock,currTime)
    type(ESMF_GridComp)   ,intent(in )          :: gridComp
    logical               ,intent(in ),optional :: kwe !keyword-enforcer
    character(ESMF_MAXSTR),intent(out),optional :: name
    type(ESMF_Method_Flag),intent(out),optional :: currentMethod
    integer               ,intent(out),optional :: currentPhase
    logical               ,intent(out),optional :: clockIsPresent
    type(ESMF_Clock)      ,intent(out),optional :: clock
    type(ESMF_Time)       ,intent(out),optional :: currTime

    character(ESMF_MAXSTR) :: myName,timestring,message
    type(ESMF_Method_Flag) :: cMethod
    integer                :: cPhase,phaseCount
    logical                :: have_clock, phaseZeroFlag
    type(ESMF_Clock)       :: myClock
    type(ESMF_Time)        :: cTime
    integer                :: rc, localrc, rc_

    rc=ESMF_SUCCESS

    call ESMF_GridCompGet(gridComp,name=myName,currentMethod=cMethod,currentPhase=cPhase, &
                          clockIsPresent=have_clock)
    if (present(name          )) name           = myName
    if (present(currentMethod )) currentMethod  = cMethod
    if (present(currentPhase  )) currentPhase   = cPhase
    if (present(clockIsPresent)) clockIsPresent = have_clock

    call ESMF_GridCompGetEPPhaseCount(gridComp, cMethod, phaseCount=phaseCount, &
      phaseZeroFlag=phaseZeroFlag, rc=localrc)

    if (have_clock) then
      call ESMF_GridCompGet(gridComp,clock=myClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (present(clock)) clock = myClock
      call ESMF_ClockGet(myClock,currTime=cTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (present(currTime)) currTime = cTime
      call ESMF_TimeGet(cTime,timeStringISOFrac=timestring, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      write(timestring,'(A)') '-------------------'
    end if

    write(message,'(A)') myName(:MOSSCO_MAXLEN_COMPNAME)//' '//trim(timestring)
    if (cMethod == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initialized'
    else if (cMethod == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' ran'
    else if (cMethod == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalized'
    elseif (cMethod == ESMF_METHOD_READRESTART) then
      write(message,'(A)') trim(message)//' readrestarted'
    else
      write(message,'(A)') trim(message)//' did'
    end if

    write(message,'(A,I1,A,I1)') trim(message)//' phase ',cPhase,' of ',phaseCount
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

  end subroutine MOSSCO_GridCompExitLog


  subroutine MOSSCO_GridCompFieldsTable(gridComp, importState, exportState, rc)

    type(ESMF_GridComp), intent(in)              :: gridComp
    type(ESMF_State), intent(in), optional       :: importState
    type(ESMF_State), intent(in), optional       :: exportState
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                    :: rc_, localrc, itemCount, phase, i, j
    integer(ESMF_KIND_I4)                    :: fieldCount, k
    type(ESMF_State)                         :: state
    logical                                  :: isPresent
    type(ESMF_FieldStatus_Flag)              :: fieldStatus
    character(len=4047)                      :: row
    character(len=ESMF_MAXSTR)               :: message, name, itemName
    type(ESMF_Method_Flag)                   :: method
    type(ESMF_StateItem_Flag), allocatable   :: itemTypeList(:)
    character(len=ESMF_MAXSTR), allocatable  :: itemNameList(:)
    type(ESMF_Field)                         :: field
    type(ESMF_FieldBundle)                   :: fieldBundle
    type(ESMF_Field), allocatable            :: fieldList(:)

    call ESMF_GridCompGet(gridComp, name=name, rc=localrc)
    write(row,'(A)') '| '//trim(name)//' |'

    call ESMF_GridCompGet(gridComp, currentPhase=phase, currentMethod=method, rc=localrc)
    if (method==ESMF_METHOD_INITIALIZE) then
      write(row,'(A)') trim(row)//' I'
    elseif (method==ESMF_METHOD_RUN) then
      write(row,'(A)') trim(row)//' R'
    elseif (method==ESMF_METHOD_FINALIZE) then
      write(row,'(A)') trim(row)//' F'
    else
      write(row,'(A)') trim(row)//' O'
    endif
    write(row,'(A,I1,A)') trim(row)//' ',phase,' |'

    do i=1,2
      if (i==1) then
        if (present(importState)) then
          state=importState
        else
          call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
          if (.not.isPresent) then
            write(row,'(A)') trim(row)//' (none) | (empty) |'
            cycle
          endif
          call ESMF_GridCompGet(gridComp, importState=state, rc=localrc)
        endif
      endif
      if (i==2) then
        if (present(exportState)) then
          state=exportState
        else
          call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
          if (.not.isPresent) then
            write(row,'(A)') trim(row)//' (none) | (empty) |'
            cycle
          endif
          call ESMF_GridCompGet(gridComp, exportState=state, rc=localrc)
        endif
      endif

      call ESMF_StateGet(state, name=name, rc=localrc)
      write(row,'(A)') trim(row)//' '//trim(name)//' |'

      call ESMF_StateGet(state, itemCount=itemCount, rc=localrc)

      if (itemCount < 1) then
        write(row,'(A)') trim(row)//' (empty) |'
        cycle
      endif

      allocate(itemTypeList(itemCount))
      allocate(itemNameList(itemCount))
      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localrc)

      do j=1, itemCount
        itemName=itemNameList(j)
        if (itemTypeList(j)==ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, trim(itemName), field, rc=localrc)
          write(row,'(A)') trim(row)//' '//trim(itemName(1:20))
          call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
          if (fieldStatus==ESMF_FIELDSTATUS_EMPTY)  write(row,'(A)') trim(row)//' E'
          if (fieldStatus==ESMF_FIELDSTATUS_GRIDSET)  write(row,'(A)') trim(row)//' G'
          if (fieldStatus==ESMF_FIELDSTATUS_COMPLETE)  write(row,'(A)') trim(row)//' C'
        endif
        if (itemTypeList(j)==ESMF_STATEITEM_FIELDBUNDLE) then
          call ESMF_StateGet(state, trim(itemName), fieldBundle, rc=localrc)
          call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=localrc)
          if (fieldCount==0) cycle
          allocate(fieldList(fieldcount))
          call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, rc=localrc)
          do k=1, fieldCount
            call ESMF_FieldGet(fieldList(k), name=name, rc=localrc)
            write(row,'(A)') trim(row)//' '//trim(name(1:20))
            call ESMF_FieldGet(fieldList(k), status=fieldStatus, rc=localrc)
            if (fieldStatus==ESMF_FIELDSTATUS_EMPTY)  write(row,'(A)') trim(row)//' E'
            if (fieldStatus==ESMF_FIELDSTATUS_GRIDSET)  write(row,'(A)') trim(row)//' G'
            if (fieldStatus==ESMF_FIELDSTATUS_COMPLETE)  write(row,'(A)') trim(row)//' C'
          enddo
          deallocate(fieldList)
        endif
      enddo

      deallocate(itemTypeList)
      deallocate(itemNameList)
    enddo
    write(row,'(A)') trim(row)//' |'

    write(0,'(A)') trim(row)

    rc_=localrc
    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_GridCompFieldsTable

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompLog"
  subroutine MOSSCO_GridCompLog(comp, rc)
    type(ESMF_GridComp)             :: comp
    integer(ESMF_KIND_I4), optional :: rc

    integer(ESMF_KIND_I4)           :: localRc, itemCount, i, j, rank
    integer(ESMF_KIND_I4)           :: attCount, packCount, linkedCount
    character(len=ESMF_MAXSTR)      :: name, message, attributeName
    type(ESMF_Grid)                 :: grid
    logical, allocatable            :: logicalValueList(:)
    real(kind=ESMF_KIND_R4), allocatable    :: real4ValueList(:)
    real(kind=ESMF_KIND_R8), allocatable    :: real8ValueList(:)
    integer(kind=ESMF_KIND_I4), allocatable :: integer4ValueList(:)
    integer(kind=ESMF_KIND_I8), allocatable :: integer8ValueList(:)
    character(len=ESMF_MAXSTR), allocatable :: characterValueList(:)
    type(ESMF_Context_Flag)         :: contextFlag
    type(ESMF_Method_Flag)          :: methodFlag
    integer(ESMF_KIND_I4)           :: phase
    logical                         :: isPresent
    type(ESMF_Clock)                :: clock
    type(ESMF_Config)               :: config
    type(ESMF_Vm)                   :: vm
    type(ESMF_State)                :: state
    type(ESMF_TypeKind_Flag)        :: typeKind
    type(ESMF_AttPack)              :: attPack

    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_GridCompGet(comp, name=name, contextFlag=contextFlag, currentMethod=methodFlag, &
      currentPhase=phase, vmIsPresent=isPresent, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (methodFlag == ESMF_METHOD_INITIALIZE) then
      write(message,'(A,I1)') trim(name)//' in method INITIALIZE phase ',phase
    elseif (methodFlag == ESMF_METHOD_READRESTART) then
      write(message,'(A,I1)') trim(name)//' in method READRESTART phase ',phase
    elseif (methodFlag == ESMF_METHOD_RUN) then
      write(message,'(A,I1)') trim(name)//' in method RUN phase ',phase
    elseif (methodFlag == ESMF_METHOD_FINALIZE) then
      write(message,'(A,I1)') trim(name)//' in FINALIZE phase ',phase
    else
      write(message,'(A,I1)') trim(name)//' in unknown method ',phase
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    if (isPresent) then
      call ESMF_GridCompGet(comp, vm=vm, rc=localRc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!@ todo utilize VM info
      !! call MOSSCO_VmLog(vm)
    endif

    !!@ todo utilize context/method/phase/name

    call ESMF_GridCompGet(comp, configFileIsPresent=isPresent, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_GridCompGet(comp, config=config, rc=localRc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!@ todo utilize Config info
      !! call MOSSCO_ConfigLog(config)
    endif

    call ESMF_GridCompGet(comp, importStateIsPresent=isPresent, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_GridCompGet(comp, importState=state, rc=localRc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !! call MOSSCO_StateLog(state)
    endif

    call ESMF_GridCompGet(comp, exportStateIsPresent=isPresent, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_GridCompGet(comp, exportState=state, rc=localRc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !! call MOSSCO_StateLog(state)
    endif

    call ESMF_GridCompGet(comp, gridIsPresent=isPresent, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_GridCompGet(comp, grid=grid, rc=localRc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !! call MOSSCO_GridLog(grid)
    endif

    call ESMF_GridCompGet(comp, clockIsPresent=isPresent, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_GridCompGet(comp, clock=clock, rc=localRc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !! call MOSSCO_ClockLog(clock)
    endif

    call ESMF_AttributeGet(comp, count=linkedCount, attCountFlag=ESMF_ATTGETCOUNT_ATTLINK, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message, '(A,I2,A)') trim(name)//' contains ', linkedCount, ' linked, '

    call ESMF_AttributeGet(comp, count=packCount, attCountFlag=ESMF_ATTGETCOUNT_ATTPACK, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message, '(A,I2,A)') trim(message), packCount, ' packed, and '

    call ESMF_AttributeGet(comp, count=attCount, attCountFlag=ESMF_ATTGETCOUNT_ATTRIBUTE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message, '(A,I2,A)') trim(message), attCount, ' single attributes'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    do i=1, attCount
      message=''
      call MOSSCO_CompIndexedAttributeString(comp, i, message, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(name)//' single attribute '//trim(message)

      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

!     call ESMF_AttGetAttPack(comp, convention='CIM 1.5' , purpose='ModelComp', isPresent=isPresent, &
!       rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     if (isPresent) then
!       call ESMF_AttGetAttPack(comp, convention='CIM 1.5' , purpose='ModelComp', attPack=attPack, &
!         rc=localrc)
!       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!         call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     endif

!     do i=1, attCount
!     message=''
!         call MOSSCO_CompIndexedAttributeString(comp, i, message, localrc)
!         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!         write(message,'(A)')  trim(name)//' pack attribute '//trim(message)
!
!         call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
!     enddo

  end subroutine MOSSCO_GridCompLog

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CompIndexedAttributeString"
  subroutine MOSSCO_CompIndexedAttributeString(comp, index, message , rc)

    use mossco_strings
    implicit none

    type(ESMF_GridComp), intent(in)                :: comp
    character(len=ESMF_MAXSTR), intent(inout)      :: message
    integer(ESMF_KIND_I4), intent(in)              :: index
    integer(ESMF_KIND_I4), intent(out), optional   :: rc

    type(ESMF_TypeKind_Flag)        :: typeKind
    logical, allocatable            :: logicalValueList(:)
    real(kind=ESMF_KIND_R4), allocatable    :: real4ValueList(:)
    real(kind=ESMF_KIND_R8), allocatable    :: real8ValueList(:)
    integer(kind=ESMF_KIND_I4), allocatable :: integer4ValueList(:)
    integer(kind=ESMF_KIND_I8), allocatable :: integer8ValueList(:)
    character(len=ESMF_MAXSTR), allocatable :: characterValueList(:)

    integer(ESMF_KIND_I4)   :: rank, localrc, count, i, j, itemCount, rc_
    character(ESMF_MAXSTR)  :: attributeName

    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_AttributeGet(comp, attributeIndex=index , name=attributeName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_MessageAdd(message, ' '//trim(attributeName)//': ')

    call ESMF_AttributeGet(comp, name=attributeName, typekind=typekind,  itemCount=itemCount, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (typekind==ESMF_TYPEKIND_Logical) then
      allocate(logicalValueList(itemCount))
      call ESMF_AttributeGet(comp, name=attributeName, valueList=logicalValueList, rc=localrc)
      if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write(message,'(A,L)') trim(message)//' ',logicalValueList(1)
      do j=2, itemCount-1
        write(message,'(A,L)') trim(message)//', ',logicalValueList(j)
      enddo
      deallocate(logicalValueList)
    elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
       allocate(characterValueList(itemCount))
       call ESMF_AttributeGet(comp, name=attributeName, valueList=characterValueList, rc=localrc)
       if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       if (len_trim(message) + len_trim(characterValueList(1)) + 1 <= len(message)) then
         write(message,'(A,A)') trim(message)//' ',trim(characterValueList(1))
         do j=2, itemCount-1
           write(message,'(A,A)') trim(message)//', ',trim(characterValueList(j))
         enddo
         deallocate(characterValueList)
       endif
    elseif (typekind==ESMF_TYPEKIND_I4) then
       allocate(integer4ValueList(itemCount))
       call ESMF_AttributeGet(comp, name=attributeName, valueList=integer4ValueList, rc=localrc)
       if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       write(message,'(A,I3.3)') trim(message)//' ',integer4ValueList(1)
       do j=2, itemCount-1
         write(message,'(A,I3.3)') trim(message)//', ',integer4ValueList(j)
       enddo
       deallocate(integer4ValueList)
    elseif (typekind==ESMF_TYPEKIND_I8) then
       allocate(integer8ValueList(itemCount))
       call ESMF_AttributeGet(comp, name=attributeName, valueList=integer8ValueList, rc=localrc)
       if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       write(message,'(A,I3.3)') trim(message)//' ',integer8ValueList(1)
       do j=2, itemCount-1
         write(message,'(A,I3.3)') trim(message)//', ',integer8ValueList(j)
       enddo
       deallocate(integer8ValueList)
    elseif (typekind==ESMF_TYPEKIND_R4) then
       allocate(real4ValueList(itemCount))
       call ESMF_AttributeGet(comp, name=attributeName, valueList=real4ValueList, rc=localrc)
       if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       write(message,'(A,G8.2)') trim(message)//' ',real4ValueList(1)
       do j=2, itemCount-1
         write(message,'(A,G8.2)') trim(message)//', ',real4ValueList(j)
       enddo
       deallocate(real4ValueList)
    elseif (typekind==ESMF_TYPEKIND_R8) then
       allocate(real8ValueList(itemCount))
       call ESMF_AttributeGet(comp, name=attributeName, valueList=real8ValueList, rc=localrc)
       if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       write(message,'(A,G8.2)') trim(message)//' ',real8ValueList(1)
       do j=2, itemCount-1
         write(message,'(A,G8.2)') trim(message)//', ',real8ValueList(j)
       enddo
       deallocate(real8ValueList)
    endif

   end subroutine MOSSCO_CompIndexedAttributeString

end module mossco_component
