!> @brief Implementation of ESMF State utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen
!> @author Knut Klingbeil
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
      call ESMF_VmGet(vm, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!> @todo: what todo with this information?
    endif

    !! Synchronize clock with parent clock if local clock is present
    call ESMF_ClockGet(parentClock, currTime=currTime_, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (clockIsPresent) then
      call ESMF_ClockSet(clock, currTime=currTime_, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockGet(clock, currTime=currTime_, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
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
    endif

    !call ESMF_CplCompGetEPPhaseCount(cplComp, method, phaseCount, &
    !  phaseZeroFlag, rc)
    phaseCount=1 !>@todo for now we assume all couplers have only 1 phase

    if (phaseCount>1 .or. phase==0) then
      write(message,'(A,I1,A,I1)') trim(message)//' phase ',phase,' of ',phaseCount
    endif
    write(message,'(A)') trim(message)//' ...'

    if (present(rc)) rc=rc_
    if (present(currTime)) currTime=currTime_
    if (present(name)) name=trim(name_)

    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)
    call ESMF_LogFlush()

    return

  end subroutine MOSSCO_CplCompEntry

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCompEntry"
  subroutine MOSSCO_GridCompEntry(GridComp, parentClock, name, currTime, rc)

    type(ESMF_GridComp), intent(inout)             :: gridComp
    type(ESMF_Clock), intent(in)                   :: parentClock
    character(ESMF_MAXSTR), intent(out), optional  :: name
    type(ESMF_Time), intent(out), optional         :: currTime
    integer, intent(out), optional                 :: rc

    character(ESMF_MAXSTR)  :: name_
    type(ESMF_Time)         :: currTime_
    integer                 :: rc_

    integer(ESMF_KIND_I4)   :: petCount, localPet, phase, localrc
    logical                 :: clockIsPresent, configIsPresent, vmIsPresent
    type(ESMF_Clock)        :: clock
    type(ESMF_Vm)           :: vm
    type(ESMF_Method_Flag)  :: method
    type(ESMF_Context_Flag) :: context
    type(ESMF_Config)       :: config
    character(len=ESMF_MAXSTR) :: message

    rc_=ESMF_SUCCESS

    call ESMF_GridCompGet(GridComp, &
      configIsPresent=configIsPresent, vmIsPresent=vmIsPresent, localPet=localPet, &
      petCount=petCount, contextFlag=context, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    endif

    !call ESMF_CplCompGetEPPhaseCount(cplComp, method, phaseCount, &
    !  phaseZeroFlag, rc)
    phaseCount=1 !>@todo for now we assume all couplers have only 1 phase

    if (phaseCount>1 .or. phase==0) then
      write(message,'(A,I1,A,I1)') trim(message)//' phase ',phase,' of ',phaseCount
    endif
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
    end if

    if (phaseCount>1 .or. cphase==0) then
      write(message,'(A,I1,A,I1)') trim(message)//' phase ',cPhase,' of ',phaseCount
    endif

    if (cMethod.eq.ESMF_METHOD_INITIALIZE .and. cPhase.eq.1) then
      call ESMF_GridCompGet(gridComp,petCount=petCount)
      write(formatstring,'(A)') '(A,'//intformat(petCount)//',A)'
      write(message,formatstring) trim(message)//' on ',petCount,' PETs'
    elseif (cMethod.eq.ESMF_METHOD_RUN .and. have_clock) then
      write(formatstring,'(A)') '(A,'//intformat(advanceCount)//',A,'//intformat(hours)//',A,I2.2,A,I2.2,A)'
      write(message, formatstring) trim(message)//' step ',advanceCount,' dt=',hours,':',minutes,':',seconds,' hours'
    end if
    write(message,'(A)') trim(message)//' ...'
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
    end if

    if (phaseCount>1 .or. cphase==0) then
      write(message,'(A,I1,A,I1)') trim(message)//' phase ',cPhase,' of ',phaseCount
    endif
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

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_GridCompFieldsTable

end module mossco_component
