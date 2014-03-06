module toplevel_component

  use esmf
  IMPLICIT NONE

  private

  public SetServices

  type(ESMF_Clock)    :: topClock
  type(ESMF_GridComp) :: getmCmp

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

  subroutine topCmp_init(topCmp,iState,eState,pClock,rc)

    use getm_component, only : getmCmp_SetServices
    IMPLICIT NONE

    type(ESMF_GridComp) :: topCmp
    type(ESMF_State)    :: iState,eState
    type(ESMF_Clock)    :: pClock
    integer,intent(out) :: rc

    type(ESMF_Clock)        :: getmClock
    type(ESMF_Time)         :: startTime,stopTime
    type(ESMF_TimeInterval) :: runDuration
    logical                 :: ClockIsPresent

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
    call ESMF_GridCompSetServices(getmCmp,getmCmp_SetServices)
    call ESMF_GridCompInitialize(getmCmp,clock=topClock)

    if (.not. ClockIsPresent) then
      call ESMF_GridCompGet(getmCmp,clockIsPresent=ClockIsPresent)
      if (ClockIsPresent) then
!       adapt clock
        call ESMF_GridCompGet(getmCmp,clock=getmClock)
        call ESMF_ClockGet(getmClock,startTime=startTime, &
                           stopTime=stopTime,runDuration=runDuration)
        call ESMF_ClockSet(topClock,startTime=startTime, &
                           stopTime=stopTime,timeStep=runDuration)
      end if
    end if

    rc = ESMF_SUCCESS

  end subroutine topCmp_init

  subroutine topCmp_run(topCmp,iState,eState,pClock,rc)

    IMPLICIT NONE
    
    type(ESMF_GridComp) :: topCmp
    type(ESMF_State)    :: iState,eState
    type(ESMF_Clock)    :: pClock
    integer,intent(out) :: rc

    type(ESMF_Time)         :: topTime,NextTime
    type(ESMF_TimeInterval) :: topTimeStep
    integer                 :: localrc

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
 
  end subroutine topCmp_run

  subroutine topCmp_finalize(topCmp,iState,eState,pClock,rc)

    IMPLICIT NONE
    
    type(ESMF_GridComp) :: topCmp
    type(ESMF_State)    :: iState,eState
    type(ESMF_Clock)    :: pClock
    integer,intent(out) :: rc

!   Finalize of child components
    call ESMF_GridCompFinalize(getmCmp,clock=topClock)

!   Destruction of child components
    call ESMF_GridCompDestroy(getmCmp)

    call ESMF_ClockDestroy(topClock)
   
    rc=ESMF_SUCCESS

  end subroutine topCmp_finalize

end module toplevel_component
