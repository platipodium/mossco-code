program main
   
  use esmf 
  use toplevel_component, only: topCmp_SetServices

  IMPLICIT NONE

  type(ESMF_GridComp)     :: topCmp
  type(ESMF_Clock)        :: mainClock,topClock
  type(ESMF_Time)         :: time1,time2
  type(ESMF_TimeInterval) :: runDuration
  character(len=40)       :: timestring


! Initialize
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN)
  call ESMF_LogSet(flush=.true.,logmsgList=(/ESMF_LOGMSG_WARNING,ESMF_LOGMSG_ERROR/))

! Get the wall clock starting time
  call ESMF_TimeSet(time1)
  call ESMF_TimeSyncToRealTime(time1)
  call ESMF_TimeGet(time1,timeStringISOFrac=timestring)
  call ESMF_LogWrite("ESMF/GETM started on "//timestring,ESMF_LOGMSG_INFO)

! Create toplevel component and call its setservices routines
  topCmp = ESMF_GridCompCreate(contextflag=ESMF_CONTEXT_PARENT_VM,name='topCmp')
! KK-TODO: contextflag=ESMF_CONTEXT_OWN_VM (default)
  call ESMF_GridCompSetServices(topCmp,topCmp_SetServices)

  call ESMF_GridCompInitialize(topCmp)
  call ESMF_GridCompGet(topCmp,clock=topClock)
  mainClock = ESMF_ClockCreate(topClock)
  call ESMF_ClockGet(mainClock,runDuration=runDuration)
  call ESMF_ClockSet(mainClock,timeStep=runDuration)

  call ESMF_GridCompRun(topCmp,clock=mainClock)

! Destroy toplevel component and clean up
  call ESMF_GridCompFinalize(topCmp,clock=mainClock)
  call ESMF_GridCompDestroy(topCmp)
  call ESMF_ClockDestroy(mainClock)

  call ESMF_TimeSet(time2)
  call ESMF_TimeSyncToRealTime(time2)

  call ESMF_TimeGet(time2,timeStringISOFrac=timestring)

  call ESMF_LogWrite("ESMF/GETM finished on "//timestring,ESMF_LOGMSG_INFO)

  call ESMF_Finalize()
   
  end

!-----------------------------------------------------------------------
! Copyright by the MOSSCO-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------
