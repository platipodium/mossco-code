!> @brief Implementation of ESMF Component utilities
!
!  This computer program is part of MOSSCO.
!> @copyright 2021-2022 Helmholtz-Zentrum Hereon
!> @copyright 2014-2021 Helmholtz-Zentrum Geesthacht
!> @copyright 2014-2021 Institut für Ostseeforschung Warnemünde
!> @author Carsten Lemmen <carsten.lemmen@hereon.de>
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

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define MOSSCO_MAXLEN_COMPNAME 15

module mossco_component

use esmf
use mossco_strings
use mossco_state
use mossco_field
use mossco_info

implicit none

interface MOSSCO_CompExit
    module procedure MOSSCO_CplCompExit
    module procedure MOSSCO_GridCompExit
end interface

interface MOSSCO_CompLog
  module procedure MOSSCO_GridCompLog
  module procedure MOSSCO_CplCompLog
end interface

interface MOSSCO_CompEntry
    module procedure MOSSCO_CplCompEntry
    module procedure MOSSCO_GridCompEntry
end interface

!> An interface is not allowed for these ESMF routines
public MOSSCO_GridCompFinalize
public MOSSCO_GridCompReadRestart
public MOSSCO_CplCompFinalize

public MOSSCO_CompExit
public MOSSCO_CompEntry
public MOSSCO_CompLog
public MOSSCO_GridCompExitLog
public MOSSCO_GridCompEntryLog

private

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CplCompEntry"
  subroutine MOSSCO_CplCompEntry(cplComp, parentClock, kwe, name, currTime, rc)

    type(ESMF_CplComp), intent(inout)              :: cplComp
    type(ESMF_Clock), intent(in)                   :: parentClock
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    character(len=*), intent(out), optional        :: name
    type(ESMF_Time), intent(out), optional         :: currTime
    integer(ESMF_KIND_I4), intent(out), optional   :: rc

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

    integer(ESMF_KIND_I8)   :: systemClockStart, systemClockRate, systemClockMax
    real(ESMF_KIND_R8)      :: cpuTimeStart
    character(len=ESMF_MAXSTR) :: phaseString

    type(ESMF_Info)         :: info

    rc_=ESMF_SUCCESS

    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc))  rc = rc_

    call ESMF_CplCompGet(cplComp, name=name_, clockIsPresent=clockIsPresent, &
      configIsPresent=configIsPresent, vmIsPresent=vmIsPresent, localPet=localPet, &
      petCount=petCount, currentMethod=method, currentPhase=phase, contextFlag=context, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call system_clock(systemClockStart, count_rate=systemClockRate, count_max=systemClockMax)
    call cpu_time(cpuTimeStart)

    !! Check for clock presence and add if necessary
    if (clockIsPresent) then
      call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    else
      clock = ESMF_ClockCreate(parentClock, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_CplCompSet(cplComp, clock=clock, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_ClockSet(clock, name=trim(name_), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    !! Check for config presence
    if (configIsPresent) then
      call ESMF_CplCompGet(cplcomp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      !!> @todo: what todo with this information?
    endif

    !! Check for vm presence
    if (vmIsPresent) then
      call ESMF_CplCompGet(cplComp, vm=vm, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      !call ESMF_VmGet(vm, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      !!> @todo: what todo with this information?
    endif

    !! Synchronize clock with parent clock if local clock is present
    call ESMF_ClockGet(parentClock, currTime=currTime_, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (clockIsPresent) then
      call ESMF_ClockGet(clock, startTime=startTime, stopTime=stopTime, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (currTime_>stopTime) currTime_=stopTime

      call ESMF_ClockSet(clock, currTime=currTime_, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
      !  call ESMF_TimePrint(startTime, options='string')
      !  call ESMF_TimePrint(stopTime, options='string')
      !  call ESMF_TimePrint(currTime_, options='string')
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !endif
    endif

    call ESMF_TimeGet(currTime_,timeStringISOFrac=timestring)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    write(message,'(A)') name_(1:MOSSCO_MAXLEN_COMPNAME)//' '//trim(timestring)
    if (method == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' running'
      write(phaseString, '(A,I1)') 'run_p', phase
    elseif (method == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initializing'
      write(phaseString, '(A,I1)') 'initialize_p', phase
    elseif (method == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalizing'
      write(phaseString, '(A,I1)') 'finalize_p', phase
    elseif (method == ESMF_METHOD_READRESTART) then
      write(message,'(A)') trim(message)//' readrestarting'
      write(phaseString, '(A,I1)') 'readrestart_p', phase
    else
      write(message,'(A)') trim(message)//' doing'
      write(phaseString, '(A,I1)') 'other_p', phase
    endif

    !call ESMF_CplCompGetEPPhaseCount(cplComp, method, phaseCount, &
    !  phaseZeroFlag, rc)
    phaseCount=1 !>@todo for now we assume all couplers have only 1 phase
    write(message,'(A,I1,A,I1)') trim(message)//' phase ',phase,' of ',phaseCount

    if (present(currTime)) currTime=currTime_
    if (present(name)) name=trim(name_)

    call ESMF_InfoGetFromHost(cplComp, info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(info, 'cpu_time_start_'//trim(phaseString), &
      cpuTimeStart, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(info, 'system_clock_start_'//trim(phaseString), &
      systemClockStart, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

  end subroutine MOSSCO_CplCompEntry

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompEntry"
  subroutine MOSSCO_GridCompEntry(GridComp, parentClock, kwe, name, &
    currTime, importState, exportState, rc)

    type(ESMF_GridComp), intent(inout)             :: gridComp
    type(ESMF_Clock), intent(in)                   :: parentClock
    type(ESMF_KeywordEnforcer), optional, intent(in)  :: kwe ! must use keywords below
    character(len=*), intent(out), optional  :: name
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

    integer(ESMF_KIND_I8)   :: systemClockStart, systemClockRate, systemClockMax
    real(ESMF_KIND_R8)      :: cpuTimeStart
    character(len=ESMF_MAXSTR) :: phaseString
    type(ESMF_Info)         :: info

    rc_=ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS

    call system_clock(systemClockStart, count_rate=systemClockRate, count_max=systemClockMax)
    call cpu_time(cpuTimeStart)

    call ESMF_GridCompGet(GridComp, name=name_, &
      configIsPresent=configIsPresent, vmIsPresent=vmIsPresent, localPet=localPet, &
      petCount=petCount, contextFlag=context, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> if importState and exportState are provided as arguments, check whether they agree with
    !> the gridComp's import- and exportState
    if (present(importState)) then
      call ESMF_GridCompGet(GridComp, importStateIsPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      if (isPresent) then
        call ESMF_GridCompGet(GridComp, importState=state, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        if (state /= importState) then
          write(message,'(A)')  trim(name_)//' importState differs from state given as argument'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          call MOSSCO_StateLog(importState, rc=localrc)
          call MOSSCO_StateLog(state, rc=localrc)
        endif
      endif
    endif

    !! Check for clock presence and add if necessary
    call ESMF_GridCompGet(gridComp, clockIsPresent=clockIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (.not. clockIsPresent) then
      clock = ESMF_ClockCreate(parentClock, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_ClockSet(clock, name=trim(name_), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_GridCompSet(GridComp, clock=clock, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_GridCompEntryLog(gridComp, name=name_, currentMethod=method,&
      currentPhase=phase, &
      clockIsPresent=clockIsPresent, clock=clock, currTime=currTime_)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !! Check for config presence
    if (configIsPresent) then
      call ESMF_GridCompGet(Gridcomp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      !!> @todo: what todo with this information?
    endif

    !! Check for vm presence
    if (vmIsPresent) then
      call ESMF_GridCompGet(GridComp, vm=vm, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      call ESMF_VmGet(vm, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      !!> @todo: what todo with this information?
    endif

    if (method == ESMF_METHOD_RUN) then
      write(phaseString, '(A,I1)') 'run_p', phase
    elseif (method == ESMF_METHOD_INITIALIZE) then
      write(phaseString, '(A,I1)') 'initialize_p', phase
    elseif (method == ESMF_METHOD_FINALIZE) then
      write(phaseString, '(A,I1)') 'finalize_p', phase
    elseif (method == ESMF_METHOD_READRESTART) then
      write(phaseString, '(A,I1)') 'readrestart_p', phase
    else
      write(phaseString, '(A,I1)') 'other_p', phase
    endif

    call ESMF_InfoGetFromHost(gridComp, info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(info, 'cpu_time_start_'//trim(phaseString), &
      cpuTimeStart, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(info, 'system_clock_start_'//trim(phaseString), &
      systemClockStart, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (present(rc)) rc=rc_
    if (present(currTime)) currTime=currTime_
    if (present(name)) name=trim(trim(name_))

  end subroutine MOSSCO_GridCompEntry

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CplCompExit"
  subroutine MOSSCO_CplCompExit(cplComp, kwe, rc)

    type(ESMF_CplComp), intent(inout) :: cplComp
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    integer, intent(out), optional    :: rc

    integer(ESMF_KIND_I4)   :: phase, phaseCount, localrc, rc_, i
    character(ESMF_MAXSTR)  :: message, timeString
    logical                 :: clockIsPresent, phaseZeroFlag
    type(ESMF_Clock)        :: clock
    type(ESMF_Method_Flag)  :: method
    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime

    integer(ESMF_KIND_I8)   :: systemClockStart, systemClockStop, systemClockRate
    integer(ESMF_KIND_I8)   :: systemClockMax
    real(ESMF_KIND_R8)      :: systemClockDuration, systemClockTotalDuration
    real(ESMF_KIND_R8)      :: cpuTimeStart, cpuTimeStop, cpuTimeTotalDuration, cpuTimeDuration
    logical                 :: isPresent
    character(len=ESMF_MAXSTR) :: phaseString
    character(len=11), parameter :: methodsToEvaluate(4) = &
      (/'initialize ','readrestart','run        ','finalize   '/)

    type(ESMF_Info)         :: info

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS

    call ESMF_CplCompGet(cplComp, name=name, clockIsPresent=clockIsPresent, &
      currentMethod=method, currentPhase=phase, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !! Check for clock presence
    if (clockIsPresent) then
      call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_ClockGet(clock, currTime=currTime, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message,'(A)') name(:MOSSCO_MAXLEN_COMPNAME)//' '//trim(timestring)
    else
      write(message,'(A)') name(:MOSSCO_MAXLEN_COMPNAME)//' -------------------'
    endif

    if (method == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' ran'
      write(phaseString, '(A,I1)') 'run_p', phase
    elseif (method == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initialized'
      write(phaseString, '(A,I1)') 'initialize_p', phase
    elseif (method == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalized'
      write(phaseString, '(A,I1)') 'finalize_p', phase
    elseif (method == ESMF_METHOD_READRESTART) then
      write(message,'(A)') trim(message)//' readrestarted'
      write(phaseString, '(A,I1)') 'readrestart_p', phase
    else
      write(message,'(A)') trim(message)//' did'
      write(phaseString, '(A,I1)') 'other_p', phase
    endif

    !call ESMF_CplCompGetEPPhaseCount(cplComp, method, phaseCount, &
    !  phaseZeroFlag, rc)
    phaseCount=1 !>@todo for now we assume all couplers have only 1 phase

    call system_clock(systemClockStop, count_rate=systemClockRate, count_max=systemClockMax)
    call cpu_time(cpuTimeStop)

    call ESMF_InfoGetFromHost(cplComp, info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='system_clock_start_'//trim(phaseString), 
      value=systemClockStart, default=systemClockStop, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='cpu_time_start_'//trim(phaseString), 
      value=cpuTimeStart, default=cpuTimeStop, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='system_clock_duration'//trim(phaseString), 
      value=systemClockTotalDuration, default=0.0d0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='cpu_time_duration_'//trim(phaseString), 
      value=cpuTimeTotalDuration, default=0.0d0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    systemClockDuration = dble(systemClockStop - systemClockStart) / systemClockRate
    systemClockTotalDuration = systemClockTotalDuration + systemClockDuration
    cpuTimeDuration = cpuTimeStop - cpuTimeStart
    cpuTimeTotalDuration = cpuTimeTotalDuration + cpuTimeDuration

    call ESMF_InfoSet(info, key='cpu_time_duration_'//trim(phaseString), &
      value=cpuTimeTotalDuration, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(cplComp, key='system_clock_duration_'//trim(phaseString), &
      value=systemClockTotalDuration, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (method == ESMF_METHOD_FINALIZE) then
      do i = lbound(methodsToEvaluate,1), ubound(methodsToEvaluate,1)
        write(phaseString,'(A,I1)') trim(methodsToEvaluate(i))//'_p', phase
        call ESMF_InfoGet(info, key='system_clock_duration_'//trim(phaseString), &
          isPresent=isPresent, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        if (.not.isPresent) cycle

        call ESMF_InfoGet(info, key='cpu_time_duration_'//trim(phaseString), &
          isPresent=isPresent, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        if (.not.isPresent) cycle

        call ESMF_InfoGet(info, key='system_clock_duration_'//trim(phaseString), &
          value=systemClockTotalDuration, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        call ESMF_AttributeGet(info, key='cpu_time_duration_'//trim(phaseString), &
          value=cpuTimeTotalDuration, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        write(message,'(A,ES9.2,A,ES9.2,A)') trim(name)//' spent ', cpuTimeTotalDuration, '/', &
          systemClockTotalDuration,' cpu/wall seconds in '//trim(phaseString)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo
    endif

    write(message,'(A)') name(:MOSSCO_MAXLEN_COMPNAME)//' '//trim(timestring)
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

    write(message,'(A,I1,A,I1,A,ES9.2,A,ES9.2,A)') trim(message)//' phase ',phase,' of ', &
      phaseCount,' in ',cpuTimeDuration,'/',systemClockDuration,' cpu/wall seconds'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_CplCompExit

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompExit"
  subroutine MOSSCO_GridCompExit(gridComp, kwe, rc)

    type(ESMF_GridComp), intent(inout) :: gridComp
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    integer, intent(out), optional     :: rc

    integer  :: rc_

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS

    call MOSSCO_GridCompExitLog(gridComp)

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_GridCompExit

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompEntryLog"
  subroutine MOSSCO_GridCompEntryLog(gridComp, kwe, name, currentMethod, &
    currentPhase, clockIsPresent, clock, currTime)

    type(ESMF_GridComp)   ,intent(inout)        :: gridComp
    logical               ,intent(in ),optional :: kwe !keyword-enforcer
    character(len=*),intent(out),optional :: name
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
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
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
    type(ESMF_GridComp)   ,intent(inout)        :: gridComp
    logical               ,intent(in ),optional :: kwe !keyword-enforcer
    character(len=*),intent(out),optional       :: name
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
    integer                :: rc, localrc, rc_, i

    integer(ESMF_KIND_I8)   :: systemClockRate, systemClockStart, systemClockStop
    integer(ESMF_KIND_I8)   :: systemClockMax
    real(ESMF_KIND_R8)      :: systemClockDuration, systemClockTotalDuration
    real(ESMF_KIND_R8)      :: cpuTimeStart, cpuTimeStop, cpuTimeTotalDuration, cpuTimeDuration
    logical                 :: isPresent
    character(len=ESMF_MAXSTR) :: phaseString
    character(len=11), parameter :: methodsToEvaluate(4) = &
      (/'initialize ','readrestart','run        ','finalize   '/)

    type(ESMF_Info)         :: info

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

    if (cMethod == ESMF_METHOD_INITIALIZE) then
      write(phaseString, '(A,I1)') 'initialize_p', cPhase
    else if (cMethod == ESMF_METHOD_RUN) then
      write(phaseString, '(A,I1)') 'run_p', cPhase
    else if (cMethod == ESMF_METHOD_FINALIZE) then
      write(phaseString, '(A,I1)') 'finalize_p', cPhase
    elseif (cMethod == ESMF_METHOD_READRESTART) then
      write(phaseString, '(A,I1)') 'readrestart_p', cPhase
    else
      write(phaseString, '(A,I1)') 'other_p', cPhase
    end if

    call system_clock(systemClockStop, count_rate=systemClockRate, count_max=systemClockMax)
    call cpu_time(cpuTimeStop)

    call ESMF_InfoGetFromHost(gridcomp, info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='system_clock_start_'//trim(phaseString), 
      value=systemClockStart, default=systemClockStop, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='cpu_time_start_'//trim(phaseString), 
      value=cpuTimeStart, default=cpuTimeStop, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='system_clock_duration'//trim(phaseString), 
      value=systemClockTotalDuration, default=0.0d0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='cpu_time_duration_'//trim(phaseString), 
      value=cpuTimeTotalDuration, default=0.0d0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    systemClockDuration = dble(systemClockStop - systemClockStart) / systemClockRate
    systemClockTotalDuration = systemClockTotalDuration + systemClockDuration
    cpuTimeDuration = cpuTimeStop - cpuTimeStart
    cpuTimeTotalDuration = cpuTimeTotalDuration + cpuTimeDuration

    call ESMF_InfoSet(info, key='cpu_time_duration_'//trim(phaseString), &
      value=cpuTimeTotalDuration, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(info, key='system_clock_duration_'//trim(phaseString), &
      value=systemClockTotalDuration, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (cMethod == ESMF_METHOD_FINALIZE) then
      do i = lbound(methodsToEvaluate,1), ubound(methodsToEvaluate,1)
        write(phaseString,'(A,I1)') trim(methodsToEvaluate(i))//'_p', cPhase
        call ESMF_InfoGet(info, key='system_clock_duration_'//trim(phaseString), &
          isPresent=isPresent, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        if (.not.isPresent) cycle

        call ESMF_InfoGet(info, key='cpu_time_duration_'//trim(phaseString), &
          isPresent=isPresent, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        if (.not.isPresent) cycle

        call ESMF_InfoGet(info, key='system_clock_duration_'//trim(phaseString), &
          value=systemClockTotalDuration, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        call ESMF_InfoGet(info, key='cpu_time_duration_'//trim(phaseString), &
          value=cpuTimeTotalDuration, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        write(message,'(A,ES9.2,A,ES9.2,A)') trim(myName)//' spent ', cpuTimeTotalDuration, '/', &
          systemClockTotalDuration,' cpu/wall seconds in '//trim(phaseString)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo
    endif

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

    write(message,'(A,I1,A,I1,A,ES9.2,A,ES9.2,A)') trim(message)//' phase ',cPhase,' of ', &
      phaseCount,' in ',cpuTimeDuration,'/',systemClockDuration,' cpu/wall seconds'
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
    character(ESMF_MAXSTR)  :: attributeName, string

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

      write(string, '(L)') logicalValueList(1)
      call MOSSCO_MessageAdd(message,' '//trim(string))
      do j=2, itemCount-1
        write(string, '(L)') logicalValueList(j)
        call MOSSCO_MessageAdd(message,', '//trim(string))
      enddo
      deallocate(logicalValueList)

    elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
       allocate(characterValueList(itemCount))
       call ESMF_AttributeGet(comp, name=attributeName, valueList=characterValueList, rc=localrc)
       if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       call MOSSCO_MessageAdd(message,' '//trim(characterValueList(1)))
        do j=2, itemCount-1
           call MOSSCO_MessageAdd(message,', '//trim(characterValueList(j)))
        enddo
        deallocate(characterValueList)

    elseif (typekind==ESMF_TYPEKIND_I4) then
       allocate(integer4ValueList(itemCount))
       call ESMF_AttributeGet(comp, name=attributeName, valueList=integer4ValueList, rc=localrc)
       if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       write(string, '(I6.6)') integer4ValueList(1)
       call MOSSCO_MessageAdd(message,' '//trim(string))
       do j=2, itemCount-1
         write(string, '(I6.6)') integer4ValueList(j)
         call MOSSCO_MessageAdd(message,', '//trim(string))
       enddo
       deallocate(integer4ValueList)

    elseif (typekind==ESMF_TYPEKIND_I8) then
       allocate(integer8ValueList(itemCount))
       call ESMF_AttributeGet(comp, name=attributeName, valueList=integer8ValueList, rc=localrc)
       if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       write(string, *) integer8ValueList(1)
       call MOSSCO_MessageAdd(message,' '//trim(string))
       do j=2, itemCount-1
         write(string, *) integer8ValueList(j)
         call MOSSCO_MessageAdd(message,', '//trim(string))
       enddo
       deallocate(integer8ValueList)

    elseif (typekind==ESMF_TYPEKIND_R4) then
       allocate(real4ValueList(itemCount))
       call ESMF_AttributeGet(comp, name=attributeName, valueList=real4ValueList, rc=localrc)
       if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       write(string, '(ES9.2)') real4ValueList(1)
       call MOSSCO_MessageAdd(message,' '//trim(string))
       do j=2, itemCount-1
         write(string, '(ES9.2)') real4ValueList(j)
         call MOSSCO_MessageAdd(message,', '//trim(string))
       enddo
       deallocate(real4ValueList)

    elseif (typekind==ESMF_TYPEKIND_R8) then
       allocate(real8ValueList(itemCount))
       call ESMF_AttributeGet(comp, name=attributeName, valueList=real8ValueList, rc=localrc)
       if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       write(string, '(ES9.2)') real8ValueList(1)
       call MOSSCO_MessageAdd(message,' '//trim(string))
       do j=2, itemCount-1
         write(string, '(ES9.2)') real8ValueList(j)
         call MOSSCO_MessageAdd(message,', '//trim(string))
       enddo
       deallocate(real8ValueList)
    endif

   end subroutine MOSSCO_CompIndexedAttributeString

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompLog"
  subroutine MOSSCO_GridCompLog(gridComp, kwe, log, rc)

    type(ESMF_GridComp)             :: gridComp
    logical,intent(in ),optional    :: kwe !keyword-enforcer
    type(ESMF_Log), optional        :: log
     integer(ESMF_KIND_I4), optional :: rc

     integer(ESMF_KIND_I4)           :: localrc, itemCount, i, j, rc_
     character(len=ESMF_MAXPATHLEN)  :: string, message
     character(len=ESMF_MAXSTR)      :: name, attributeName
     logical                         :: isPresent
     type(ESMF_TypeKind_Flag)        :: typeKind
     logical, allocatable            :: logicalValueList(:)
     real(kind=ESMF_KIND_R4), allocatable    :: real4ValueList(:)
     real(kind=ESMF_KIND_R8), allocatable    :: real8ValueList(:)
     integer(kind=ESMF_KIND_I4), allocatable :: integer4ValueList(:)
     integer(kind=ESMF_KIND_I8), allocatable :: integer8ValueList(:)
     character(len=4096)       , allocatable :: characterValueList(:)

     type(ESMF_State)                :: state
     type(ESMF_Grid)                 :: grid
     type(ESMF_Config)               :: config
     type(ESMF_Vm)                   :: vm
     type(ESMF_Clock)                :: clock

    integer(ESMF_KIND_I4)           :: petCount, count
    type(ESMF_Context_Flag)         :: contextFlag
    type(ESMF_Method_Flag)          :: methodFlag
    integer(ESMF_KIND_I4)           :: phase
    type(ESMF_AttPack)              :: attPack

    type(ESMF_Info)                 :: info

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    if (present(kwe)) rc = rc_

    call ESMF_GridCompGet(gridComp, name=name, contextFlag=contextFlag, &
      currentMethod=methodFlag, currentPhase=phase, vmIsPresent=isPresent, rc=localRc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (methodFlag == ESMF_METHOD_INITIALIZE) then
      write(message,'(A,I1)') trim(name)//' in method INITIALIZE phase ',phase
    elseif (methodFlag == ESMF_METHOD_READRESTART) then
      write(message,'(A,I1)') trim(name)//' in method READRESTART phase ',phase
    elseif (methodFlag == ESMF_METHOD_RUN) then
      write(message,'(A,I1)') trim(name)//' in method RUN phase ',phase
    elseif (methodFlag == ESMF_METHOD_FINALIZE) then
      write(message,'(A,I1)') trim(name)//' in FINALIZE phase ',phase
    else
      !> @todo find out why this occurs and re-enable a warning when cleared
      write(message,'(A,I1)') trim(name)//' in unknown method phase ',phase
    endif
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    if (isPresent) then
      call ESMF_GridCompGet(gridComp, vm=vm, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      !!@ todo utilize VM info
      !call ESMF_GridCompGet(gridComp, localPet, petCount, contextflag, &
      !comptype)
    endif

    call ESMF_GridCompGet(gridComp, gridIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent) then
      call ESMF_GridCompGet(gridComp, grid=grid, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_GridGet(grid, name=string, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message, '(A)') trim(name)//' has grid '//trim(string)
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent) then
      call ESMF_GridCompGet(gridComp, importState=state, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_StateGet(state, name=string, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message, '(A)') trim(name)//' has importState '//trim(string)
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent) then
      call ESMF_GridCompGet(gridComp, exportState=state, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_StateGet(state, name=string, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message, '(A)') trim(name)//' has exportState '//trim(string)
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    call ESMF_GridCompGet(gridComp, configIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent) then
      call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message, '(A)') trim(name)//' has config'
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    call ESMF_GridCompGet(gridComp, configFileIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent) then
      call ESMF_GridCompGet(gridComp, configFile=string, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message, '(A)') trim(name)//' has config file '//trim(string)
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    call ESMF_GridCompGet(gridComp, clockIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent) then
      call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_ClockGet(clock, name=string, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message, '(A)') trim(name)//' has clock '//trim(string)
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

#if ESMF_VERSION_MAJOR < 8
    call ESMF_AttributeGet(gridComp, count=count , attCountFlag=ESMF_ATTGETCOUNT_ATTLINK, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message, '(A,I2,A)') trim(name)//' contains ', count, ' linked, '

    !call ESMF_AttributeGet(gridComp, count=count , attCountFlag=ESMF_ATTGETCOUNT_TOTAL, rc=localrc)
  !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
  !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  !  write(message, '(A,I2,A)') trim(name)//' contains ', count, ' total, '

    call ESMF_AttributeGet(gridComp, count=count, attCountFlag=ESMF_ATTGETCOUNT_ATTPACK, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message, '(A,I2,A)') trim(message), count, ' packed, and '

    call ESMF_AttributeGet(gridComp, count=count, attCountFlag=ESMF_ATTGETCOUNT_ATTRIBUTE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message, '(A,I2,A)') trim(message), count, ' single attributes'

    if (present(log)) then
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
    else
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_AttributeGet(gridComp, count=count, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, count
       call ESMF_AttributeGet(gridComp, attributeIndex=i , name=attributeName, rc=localrc)
       if (localrc /= ESMF_SUCCESS) cycle

       write(message,'(A)')  trim(name)//':'
       call MOSSCO_MessageAdd(message,trim(attributeName)//' =')

       call ESMF_AttributeGet(gridComp, name=attributeName, typekind=typekind,  itemCount=itemCount, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
         call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

       if (typekind==ESMF_TYPEKIND_Logical) then
         call MOSSCO_MessageAdd(message,' (L)')
         allocate(logicalValueList(itemCount))
         call ESMF_AttributeGet(gridComp, name=attributeName, valueList=logicalValueList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         write(string, '(L)') logicalValueList(1)
         call MOSSCO_MessageAdd(message,' '//trim(string))
         do j=2, itemCount-1
           write(string, '(L)') logicalValueList(j)
           call MOSSCO_MessageAdd(message,', '//trim(string))
         enddo
         deallocate(logicalValueList)

       elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
         if (allocated(characterValueList)) deallocate(characterValueList)
         allocate(characterValueList(itemCount))
         call ESMF_AttributeGet(gridComp, name=attributeName, valueList=characterValueList, rc=localrc)
         if (localrc /= ESMF_SUCCESS) then
           !(ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
           !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           !>@ todo: how to deal with very long attributes that don't fit into valueList?
           rc=ESMF_SUCCESS
         endif

         call MOSSCO_MessageAdd(message,' "'//trim(characterValueList(1))//'"')
         do j=2, itemCount-1
            call MOSSCO_MessageAdd(message,', "'//trim(characterValueList(j))//'"')
         enddo
         if (allocated(characterValueList)) deallocate(characterValueList)

       elseif (typekind==ESMF_TYPEKIND_I4) then
         call MOSSCO_MessageAdd(message,' (I4)')
         allocate(integer4ValueList(itemCount))
         call ESMF_AttributeGet(gridComp, name=attributeName, valueList=integer4ValueList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         write(string, '(I6.6)') integer4ValueList(1)
         call MOSSCO_MessageAdd(message,' '//trim(string))
         do j=2, itemCount-1
           write(string, '(I6.6)') integer4ValueList(j)
           call MOSSCO_MessageAdd(message,', '//trim(string))
         enddo
         deallocate(integer4ValueList)
       elseif (typekind==ESMF_TYPEKIND_I8) then
         call MOSSCO_MessageAdd(message,' (I8)')
         allocate(integer8ValueList(itemCount))
         call ESMF_AttributeGet(gridComp, name=attributeName, valueList=integer8ValueList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         write(string, *) integer8ValueList(1)
         call MOSSCO_MessageAdd(message,' '//trim(string))
         do j=2, itemCount-1
             write(string, *) integer8ValueList(j)
             call MOSSCO_MessageAdd(message,', '//trim(string))
         enddo
         deallocate(integer8ValueList)
       elseif (typekind==ESMF_TYPEKIND_R4) then
         call MOSSCO_MessageAdd(message,' (R4)')
         allocate(real4ValueList(itemCount))
         call ESMF_AttributeGet(gridComp, name=attributeName, valueList=real4ValueList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         write(string, '(ES9.2)') real4ValueList(1)
         call MOSSCO_MessageAdd(message,' '//trim(string))
         do j=2, itemCount-1
           write(string, '(ES9.2)') real4ValueList(j)
           call MOSSCO_MessageAdd(message,', '//trim(string))
         enddo
         deallocate(real4ValueList)
       elseif (typekind==ESMF_TYPEKIND_R8) then
         call MOSSCO_MessageAdd(message,' (R8)')
         allocate(real8ValueList(itemCount))
         call ESMF_AttributeGet(gridComp, name=attributeName, valueList=real8ValueList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         write(string, '(ES9.2)') real8ValueList(1)
         call MOSSCO_MessageAdd(message,' '//trim(string))
         do j=2, itemCount-1
           write(string, '(ES9.2)') real8ValueList(j)
           call MOSSCO_MessageAdd(message,', '//trim(string))
         enddo
         deallocate(real8ValueList)
       endif
       if (present(log)) then
         call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
       else
         call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
       endif
     enddo
#else ! ESMF version >= 8

  if (present(log)) then
    call MOSSCO_InfoLogObject(gridComp, log=log, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  else
    call MOSSCO_InfoLogObject(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  endif

  call ESMF_InfoGetFromHost(gridComp, info, rc=localrc)
  call ESMF_InfoPrint(info, unit=message, rc=localrc )
  if (present(log)) then
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
  else
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
  endif
#endif

   end subroutine MOSSCO_GridCompLog

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CplCompLog"
  subroutine MOSSCO_CplCompLog(cplComp, kwe, log, rc)

    type(ESMF_CplComp)             :: cplComp
    type(ESMF_KeywordEnforcer), intent(in ), optional :: kwe
    type(ESMF_Log), optional        :: log
    integer(ESMF_KIND_I4), optional :: rc

    integer(ESMF_KIND_I4)           :: localrc, itemCount, i, j, rc_
    character(len=ESMF_MAXPATHLEN)  :: string, message
    character(len=ESMF_MAXSTR)      :: name, attributeName
    logical                         :: isPresent
    type(ESMF_TypeKind_Flag)        :: typeKind
    logical, allocatable            :: logicalValueList(:)
    real(kind=ESMF_KIND_R4), allocatable    :: real4ValueList(:)
    real(kind=ESMF_KIND_R8), allocatable    :: real8ValueList(:)
    integer(kind=ESMF_KIND_I4), allocatable :: integer4ValueList(:)
    integer(kind=ESMF_KIND_I8), allocatable :: integer8ValueList(:)
    character(len=4096)       , allocatable :: characterValueList(:)

    type(ESMF_Config)               :: config
    type(ESMF_Vm)                   :: vm
    type(ESMF_Clock)                :: clock

    integer(ESMF_KIND_I4)           :: petCount, count
    type(ESMF_Context_Flag)         :: contextFlag
    type(ESMF_Method_Flag)          :: methodFlag
    integer(ESMF_KIND_I4)           :: phase
    type(ESMF_AttPack)              :: attPack

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    if (present(kwe)) rc = rc_

    call ESMF_CplCompGet(cplComp, name=name, contextFlag=contextFlag, &
      currentMethod=methodFlag, currentPhase=phase, vmIsPresent=isPresent, rc=localRc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (methodFlag == ESMF_METHOD_INITIALIZE) then
      write(message,'(A,I1)') trim(name)//' in method INITIALIZE phase ',phase
    elseif (methodFlag == ESMF_METHOD_READRESTART) then
      write(message,'(A,I1)') trim(name)//' in method READRESTART phase ',phase
    elseif (methodFlag == ESMF_METHOD_RUN) then
      write(message,'(A,I1)') trim(name)//' in method RUN phase ',phase
    elseif (methodFlag == ESMF_METHOD_FINALIZE) then
      write(message,'(A,I1)') trim(name)//' in FINALIZE phase ',phase
    else
      !> @todo clarify what happens here and re-enable WARNING
      write(message,'(A,I1)') trim(name)//' in unknown method phase ',phase
    endif
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    if (isPresent) then
      call ESMF_CplCompGet(cplComp, vm=vm, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      !!@ todo utilize VM info
      !call ESMF_CplCompGet(cplComp, localPet, petCount, contextflag, &
      !comptype)
    endif

    call ESMF_CplCompGet(cplComp, configIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent) then
      call ESMF_CplCompGet(cplComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message, '(A)') trim(name)//' has config'
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    call ESMF_CplCompGet(cplComp, configFileIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent) then
      call ESMF_CplCompGet(cplComp, configFile=string, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message, '(A)') trim(name)//' has config file '//trim(string)
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    call ESMF_CplCompGet(cplComp, clockIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent) then
      call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_ClockGet(clock, name=string, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message, '(A)') trim(name)//' has clock '//trim(string)
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

#if ESMF_VERSION_MAJOR < 8
    call ESMF_AttributeGet(cplComp, count=count , attCountFlag=ESMF_ATTGETCOUNT_ATTLINK, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    write(message, '(A,I2,A)') trim(name)//' contains ', count, ' linked, '
  !call ESMF_AttributeGet(cplComp, count=count , attCountFlag=ESMF_ATTGETCOUNT_TOTAL, rc=localrc)
  !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  ! write(message, '(A,I2,A)') trim(name)//' contains ', count, ' total, '

    call ESMF_AttributeGet(cplComp, count=count, attCountFlag=ESMF_ATTGETCOUNT_ATTPACK, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message, '(A,I2,A)') trim(message), count, ' packed, and '

    call ESMF_AttributeGet(cplComp, count=count, attCountFlag=ESMF_ATTGETCOUNT_ATTRIBUTE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message, '(A,I2,A)') trim(message), count, ' single attributes'

    if (present(log)) then
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
    else
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
    endif

    call ESMF_AttributeGet(cplComp, count=count, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, count
       call ESMF_AttributeGet(cplComp, attributeIndex=i , name=attributeName, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
         call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

       write(message,'(A)')  trim(name)//':'
       call MOSSCO_MessageAdd(message,trim(attributeName)//' =')

       call ESMF_AttributeGet(cplComp, name=attributeName, typekind=typekind,  itemCount=itemCount, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
         call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

       if (typekind==ESMF_TYPEKIND_Logical) then
         call MOSSCO_MessageAdd(message,' (L)')
         allocate(logicalValueList(itemCount))
         call ESMF_AttributeGet(cplComp, name=attributeName, valueList=logicalValueList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         write(string, '(L)') logicalValueList(1)
         call MOSSCO_MessageAdd(message,' '//trim(string))
         do j=2, itemCount-1
           write(string, '(L)') logicalValueList(j)
           call MOSSCO_MessageAdd(message,', '//trim(string))
         enddo
         deallocate(logicalValueList)

       elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
         if (allocated(characterValueList)) deallocate(characterValueList)
         allocate(characterValueList(itemCount))
         call ESMF_AttributeGet(cplComp, name=attributeName, valueList=characterValueList, rc=localrc)
         if (localrc /= ESMF_SUCCESS) then
           !(ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
           !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           !>@ todo: how to deal with very long attributes that don't fit into valueList?
           rc=ESMF_SUCCESS
         endif

         call MOSSCO_MessageAdd(message, ' "'//trim(characterValueList(1))//'"')
         do j=2, itemCount-1
           call MOSSCO_MessageAdd(message, ', "'//trim(characterValueList(j))//'"')
          enddo
         if (allocated(characterValueList)) deallocate(characterValueList)

       elseif (typekind==ESMF_TYPEKIND_I4) then
         call MOSSCO_MessageAdd(message,' (I4)')
         allocate(integer4ValueList(itemCount))
         call ESMF_AttributeGet(cplComp, name=attributeName, valueList=integer4ValueList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         write(string, '(I6.6)') integer4ValueList(1)
         call MOSSCO_MessageAdd(message,' '//trim(string))
         do j=2, itemCount-1
           write(string, '(I6.6)') integer4ValueList(j)
           call MOSSCO_MessageAdd(message,', '//trim(string))
         enddo
         if (allocated(integer4ValueList)) deallocate(integer4ValueList)

       elseif (typekind==ESMF_TYPEKIND_I8) then
         call MOSSCO_MessageAdd(message,' (I8)')
         allocate(integer8ValueList(itemCount))
         call ESMF_AttributeGet(cplComp, name=attributeName, valueList=integer8ValueList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         write(string, *) integer8ValueList(1)
         call MOSSCO_MessageAdd(message,' '//trim(string))
         do j=2, itemCount-1
           write(string, *) integer8ValueList(j)
           call MOSSCO_MessageAdd(message,', '//trim(string))
         enddo
         deallocate(integer8ValueList)

       elseif (typekind==ESMF_TYPEKIND_R4) then
         call MOSSCO_MessageAdd(message,' R4)')
         allocate(real4ValueList(itemCount))
         call ESMF_AttributeGet(cplComp, name=attributeName, valueList=real4ValueList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         write(string, '(ES9.2)') real4ValueList(1)
         call MOSSCO_MessageAdd(message,' '//trim(string))
         do j=2, itemCount-1
           write(string, '(ES9.2)') real4ValueList(j)
           call MOSSCO_MessageAdd(message,', '//trim(string))
         enddo
         deallocate(real4ValueList)

       elseif (typekind==ESMF_TYPEKIND_R8) then
         call MOSSCO_MessageAdd(message,' (R8)')
         allocate(real8ValueList(itemCount))
         call ESMF_AttributeGet(cplComp, name=attributeName, valueList=real8ValueList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         write(string, '(ES9.2)') real8ValueList(1)
         call MOSSCO_MessageAdd(message,' '//trim(string))
         do j=2, itemCount-1
           write(string, '(ES9.2)') real8ValueList(j)
           call MOSSCO_MessageAdd(message,', '//trim(string))
         enddo
         deallocate(real8ValueList)

       endif
       if (present(log)) then
         call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
       else
         call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
       endif
     enddo
#else
  if (present(log)) then
    call MOSSCO_InfoLogObject(cplComp, log=log, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  else
    call MOSSCO_InfoLogObject(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  endif
#endif

   end subroutine MOSSCO_CplCompLog

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompRemoveCreated"
subroutine MOSSCO_GridCompRemoveCreated(gridComp, importState, exportState, rc)

  type(ESMF_GridComp), intent(inout)           :: gridComp
  type(ESMF_State), intent(inout), optional    :: importState, exportState
  integer(ESMF_KIND_I4), intent(out), optional :: rc

  integer(ESMF_KIND_I4)           :: rc_, localrc

  rc_ = ESMF_SUCCESS

  if (present(importState)) call MOSSCO_GridCompStateRemoveCreated(gridComp, importState, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (present(exportState)) call MOSSCO_GridCompStateRemoveCreated(gridComp, exportState, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (present(rc)) rc = rc_
  return

end subroutine MOSSCO_GridCompRemoveCreated

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompStateRemoveCreated"
subroutine MOSSCO_GridCompStateRemoveCreated(gridComp, state, rc)

  type(ESMF_GridComp), intent(inout)           :: gridComp
  type(ESMF_State), intent(inout)              :: state
  integer(ESMF_KIND_I4), intent(out), optional :: rc

  integer(ESMF_KIND_I4)                    :: rc_, localrc, itemCount
  integer(ESMF_KIND_I4)                    :: fieldCount, i, j
  type(ESMF_StateItem_Flag), allocatable   :: itemTypeList(:)
  character(len=ESMF_MAXSTR), allocatable  :: itemNameList(:)
  type(ESMF_Field), allocatable            :: fieldList(:)
  type(ESMF_Field)                         :: field
  type(ESMF_FieldBundle)                   :: fieldBundle
  logical                                  :: isPresent
  character(ESMF_MAXSTR)                   :: name, fieldName, message, creator

  rc_ = ESMF_SUCCESS

  call ESMF_GridCompGet(gridComp, name=name, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  call ESMF_StateGet(state, itemCount=itemCount, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  if (itemCount > 0) then

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  endif

   do i = 1, itemCount

     if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then

       call ESMF_StateGet(state, trim(itemNameList(i)), field, rc=localrc)
       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

       call ESMF_StateRemove(state, trim(itemNameList(i)) , rc=localrc)
       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

       call ESMF_AttributeGet(field, 'creator', isPresent=isPresent, rc=localrc)
       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

       if (.not.isPresent) cycle

       call ESMF_AttributeGet(field, 'creator', creator, rc=localrc)
       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

       if (trim(creator) /= trim(name)) cycle

       write(message, '(A)') trim(name)//' destroys '
       call MOSSCO_FieldString(fieldList(j), message)
       call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

       call ESMF_FieldDestroy(field, rc=localrc)
       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then

       call ESMF_StateGet(state, trim(itemNameList(i)), fieldBundle, rc=localrc)
       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

       call ESMF_StateRemove(state, trim(itemNameList(i)), rc=localrc)
       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

       call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=localrc)
       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call MOSSCO_Reallocate(fieldList, fieldCount, keep=.false., rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

       call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, rc=localrc)
       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

       do j = lbound(fieldList,1), ubound(fieldList,1)

         call ESMF_FieldGet(fieldList(j), name=fieldName, rc=localrc)
         _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

         call ESMF_FieldBundleRemove(fieldBundle, (/trim(fieldName)/), rc=localrc)
         _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

         call ESMF_AttributeGet(fieldList(j), 'creator', isPresent=isPresent, rc=localrc)
         _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

         if (.not.isPresent) cycle

         write(message, '(A)') trim(name)//' destroys '
         call MOSSCO_FieldString(fieldList(j), message)
         call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

         call ESMF_FieldDestroy(fieldList(j), rc=localrc)
         _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

       enddo

      call ESMF_AttributeGet(fieldBundle, 'creator', isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.isPresent) cycle

      call ESMF_AttributeGet(field, 'creator', creator, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (trim(creator) /= trim(name)) cycle

      write(message, '(A)') trim(name)//' destroys fieldBundle '//trim(itemNameList(i))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_FieldBundleDestroy(fieldBundle, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    else

      call ESMF_StateRemove(state, trim(itemNameList(i)), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    endif

  enddo

  call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  if (present(rc)) rc = rc_

end subroutine MOSSCO_GridCompStateRemoveCreated

!> Provide a generic Restart routine for all MOSSCO components,
!> For all fields in the exportState of a component, check if information
!> exists in import and overwrite the information in export.
#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompReadRestart"
subroutine MOSSCO_GridCompReadRestart(gridComp, importState, exportState, parentClock, rc)

  type(ESMF_GridComp)   :: gridComp
  type(ESMF_State)      :: importState, exportState
  type(ESMF_Clock)      :: parentClock
  integer, intent(out)  :: rc

  character(ESMF_MAXSTR)  :: name, message
  integer(ESMF_KIND_I4)   :: localrc, itemCount, i
  integer(ESMF_KIND_I4)   :: exportfieldCount, importFieldCount
  type(ESMF_Field), allocatable          :: exportfieldList(:), importFieldList(:)
  character(ESMF_MAXSTR), allocatable    :: itemNameList(:)
  type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
  character(ESMF_MAXSTR), pointer        :: includeList(:)

  rc = ESMF_SUCCESS

  call MOSSCO_CompEntry(gridComp, parentClock, name=name, &
    importState=importState, exportState=exportState, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_StateGet(exportState, itemCount=itemCount, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  if (itemCount < 1) then
    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    return
  endif

  allocate(itemNameList(itemCount))
  allocate(itemTypeList(itemCount))

  call ESMF_StateGet(exportState, itemNameList=itemNameList, &
    itemTypeList=itemTypeList, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  do i=1, itemCount

    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(1))
    includeList(1) = itemNameList(i)

    if (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then

      call MOSSCO_StateGet(exportState, fieldList=exportFieldList, &
        fieldCount=exportFieldCount, include=includeList, &
        fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (exportFieldCount < 1) cycle

      call MOSSCO_StateGet(importState, fieldList=importFieldList, &
        fieldCount=importFieldCount, include=includeList,  &
        fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (importFieldCount /= exportfieldCount) cycle

      !> @todo  call MOSSCO_StateCopyMatchingFields
      localrc = ESMF_RC_NOT_IMPL
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then

      call MOSSCO_StateGet(exportState, fieldList=exportFieldList, &
        fieldCount=exportFieldCount, itemSearch= includeList(1), &
        fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (exportFieldCount < 1) cycle

      call MOSSCO_StateGet(importState, fieldList=importFieldList, &
        fieldCount=importFieldCount, itemSearch=includeList(1), &
        fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (exportFieldCount /= importFieldCount) cycle

      call MOSSCO_FieldCopy(exportFieldList(1), importFieldList(1), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    else
      cycle
    endif
  enddo

  call MOSSCO_CompExit(gridComp, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

end subroutine MOSSCO_GridCompReadRestart

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompFinalize"
subroutine MOSSCO_GridCompFinalize(gridComp, importState, exportState, parentClock, rc)

  type(ESMF_GridComp)   :: gridComp
  type(ESMF_State)      :: importState, exportState
  type(ESMF_Clock)      :: parentClock
  integer, intent(out)  :: rc

  character(ESMF_MAXSTR)  :: name
  type(ESMF_Time)         :: currTime
  integer(ESMF_KIND_I4)   :: localrc
  logical                 :: isPresent
  type(ESMF_Config)       :: config

  rc = ESMF_SUCCESS

  call MOSSCO_CompEntry(gridComp, parentClock, name=name, &
    importState=importState, exportState=exportState, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_GridCompGet(gridComp, configIsPresent=isPresent, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  if (isPresent) then

    call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ConfigDestroy(config, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end if

  call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  if (isPresent) then
    call ESMF_StateValidate(importState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_DestroyOwn(importState, trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  endif

  call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  if (isPresent) then
    call ESMF_StateValidate(exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_DestroyOwn(exportState, trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  endif

  !> @todo deal with everything that is not in states, i.e. geometries and clock

  call MOSSCO_CompExit(gridComp, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

end subroutine MOSSCO_GridCompFinalize

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CplCompFinalize"
subroutine MOSSCO_CplCompFinalize(CplComp, importState, exportState, parentClock, rc)

  type(ESMF_CplComp)    :: CplComp
  type(ESMF_State)      :: importState, exportState
  type(ESMF_Clock)      :: parentClock
  integer, intent(out)  :: rc

  character(ESMF_MAXSTR)  :: name, itemName, message
  integer(ESMF_KIND_I4)   :: localrc
  logical                 :: isPresent
  type(ESMF_Config)       :: config
  type(ESMF_Clock)        :: clock

  rc = ESMF_SUCCESS

  call MOSSCO_CompEntry(CplComp, parentClock, name=name, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_CplCompGet(CplComp, configIsPresent=isPresent, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  if (isPresent) then

    call ESMF_CplCompGet(CplComp, config=config, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ConfigDestroy(config, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end if

  call ESMF_CplCompGet(CplComp, clockIsPresent=isPresent, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  if (isPresent) then

    call ESMF_CplCompGet(CplComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(clock, name=itemName, rc=localrc)

    if (trim(name) == itemName(1:len_trim(name))) then
      call ESMF_ClockDestroy(clock, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

  end if

  call MOSSCO_CompExit(CplComp, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

end subroutine MOSSCO_CplCompFinalize

end module mossco_component
