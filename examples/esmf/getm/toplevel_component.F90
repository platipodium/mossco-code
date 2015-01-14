!> @brief Implementation of an ESMF toplevel coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
module toplevel_component

  use esmf
  IMPLICIT NONE

  private

  public SetServices

  type(ESMF_Clock)    :: topClock
  type(ESMF_GridComp) :: getmCmp
  type(ESMF_State)    :: getmExportState

  contains

  subroutine SetServices(topCmp,rc)

    IMPLICIT NONE

    type(ESMF_GridComp) :: topCmp
    integer,intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(topCmp,ESMF_METHOD_INITIALIZE,     &
                                    userRoutine=topCmp_init,rc=rc)
    call ESMF_GridCompSetEntryPoint(topCmp,ESMF_METHOD_RUN,            &
                                    userRoutine=topCmp_run,rc=rc)
    call ESMF_GridCompSetEntryPoint(topCmp,ESMF_METHOD_FINALIZE,       &
                                    userRoutine=topCmp_finalize,rc=rc)

  end subroutine SetServices

  subroutine topCmp_init(topCmp,iState,exportState,pClock,rc)

    use getm_component, only : SetServices
    IMPLICIT NONE

    type(ESMF_GridComp) :: topCmp
    type(ESMF_State)    :: iState,exportState
    type(ESMF_Clock)    :: pClock
    integer,intent(out) :: rc

    type(ESMF_Clock)        :: getmClock
    type(ESMF_Time)         :: startTime,stopTime
    type(ESMF_TimeInterval) :: runDuration
    logical                 :: ClockIsPresent

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_TRACE)

!   Check whether application driver called ESMF_GridCompCreate() with clock.
    call ESMF_GridCompGet(topCmp,clockIsPresent=ClockIsPresent)

!   Create child components
    if (ClockIsPresent) then
      call ESMF_GridCompGet(topCmp,clock=topClock)
      getmClock = ESMF_ClockCreate(topClock)
      call ESMF_ClockSet(getmClock,name="getmClock")
      getmCmp = ESMF_GridCompCreate(name="getmCmp",clock=getmClock)
    else
      topClock = ESMF_ClockCreate(pClock)
      call ESMF_ClockSet(topClock,name="topClock")
      call ESMF_GridCompSet(topCmp,clock=topClock)
      getmCmp = ESMF_GridCompCreate(name="getmCmp")
    end if
    call ESMF_GridCompSetServices(getmCmp,SetServices)
    getmExportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="getmExportState")
    call ESMF_GridCompInitialize(getmCmp,clock=topClock,exportState=getmExportState)

    if (.not. ClockIsPresent) then
      call ESMF_GridCompGet(getmCmp,clockIsPresent=ClockIsPresent)
      if (ClockIsPresent) then
!       adapt clock
        call ESMF_GridCompGet(getmCmp,clock=getmClock)
        call ESMF_ClockGet(getmClock,startTime=startTime, &
                           stopTime=stopTime,runDuration=runDuration)
        call ESMF_ClockSet(topClock,startTime=startTime, &
                           stopTime=stopTime,timeStep=runDuration, &
                           currTime=startTime)
      end if
    end if

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_TRACE)

    rc = ESMF_SUCCESS

  end subroutine topCmp_init

  subroutine topCmp_run(topCmp,iState,exportState,pClock,rc)

    IMPLICIT NONE

    type(ESMF_GridComp) :: topCmp
    type(ESMF_State)    :: iState,exportState
    type(ESMF_Clock)    :: pClock
    integer,intent(out) :: rc

    type(ESMF_Time)         :: topTime,NextTime
    type(ESMF_TimeInterval) :: topTimeStep
    integer                 :: localrc

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_TRACE)

    call ESMF_ClockGet(topClock,timeStep=topTimeStep,currtime=topTime)

!   use pClock to do determine time of calling routine
    call ESMF_ClockGetNextTime(pClock,NextTime,rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) then
      call ESMF_LogWrite('will continue until own stopTime',ESMF_LOGMSG_WARNING, &
                         line=__LINE__,file=__FILE__,method='topCmp_run()')
      call ESMF_ClockGet(topClock,stopTime=NextTime)
   end if

   do while (topTime + 0.5d0*topTimeStep <= NextTime)

      if (ESMF_ClockIsStopTime(topClock)) then
         call ESMF_LogWrite('already exceeded stopTime',ESMF_LOGMSG_ERROR, &
                            line=__LINE__,file=__FILE__,method='topCmp_run()')
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if

!     Run of child components
      call ESMF_GridCompRun(getmCmp,clock=topClock)

      call ESMF_ClockAdvance(topClock)
      call ESMF_ClockGet(topClock,currtime=topTime)

    end do

    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_TRACE)

  end subroutine topCmp_run

  subroutine topCmp_finalize(topCmp,iState,exportState,pClock,rc)

    IMPLICIT NONE

    type(ESMF_GridComp) :: topCmp
    type(ESMF_State)    :: iState,exportState
    type(ESMF_Clock)    :: pClock
    integer,intent(out) :: rc

    call ESMF_LogWrite("Toplevel component finalizing ... ",ESMF_LOGMSG_TRACE)

!   Finalize of child components
    call ESMF_GridCompFinalize(getmCmp,clock=topClock)

!   Destruction of child components
    call ESMF_GridCompDestroy(getmCmp)

    call ESMF_StateDestroy(getmExportState)

    call ESMF_ClockDestroy(topClock)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_TRACE)

    rc=ESMF_SUCCESS

  end subroutine topCmp_finalize

end module toplevel_component
