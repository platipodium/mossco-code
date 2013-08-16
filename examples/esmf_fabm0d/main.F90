program main
   
   use esmf 
   use esmf_toplevel_component, only: SetServices

   implicit none

   character(LEN=8)          :: datestr
   type(ESMF_Time)           :: time1, time2, startTime, stopTime
   type(ESMF_TimeInterval)   :: timeStep
   integer                   :: localrc, rc, petCount
   double precision          :: seconds
   character(len=40)         :: timestring
   type(ESMF_GridComp)       :: topComp
   type(ESMF_State)          :: topState ! for import and export, empty
   type(ESMF_Clock)          :: clock
   type(ESMF_VM)             :: vm
   

! Initialize
   call ESMF_Initialize(defaultLogFileName="ESMF_fabm0d_driver",rc=localrc,&
     logkindflag=ESMF_LOGKIND_MULTI,defaultCalKind=ESMF_CALKIND_GREGORIAN,&
     vm=vm)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 
   call ESMF_LogWrite("ESMF/FABM0d driver start", ESMF_LOGMSG_INFO)

! Get the wall clock starting time
   call ESMF_TimeSet(time1,rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 
   call ESMF_TimeSyncToRealTime(time1,rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 
   call ESMF_TimeGet(time1,timeStringISOFrac=timestring)
   call ESMF_LogWrite("Program starts at wall clock "//timestring, ESMF_LOGMSG_INFO)
  
! Create and initialize a clock, with 60 second timestep, and 1 month running time
! This can be overwritten by the run.nml namelist parameters
   call ESMF_TimeIntervalSet(timeStep, s=60, rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

   call ESMF_TimeSet(startTime, yy=2002, mm=1, dd=1, rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

   call ESMF_TimeSet(stopTime, yy=2002, mm=2, dd=1, rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

   clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, & 
     name="Parent clock", rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

! start and stop times will be overwritten by the esmf fabm0d component

! Create toplevel component and call its setservices routines
   topComp = ESMF_GridCompCreate(name="ESMF Toplevel Component", rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 
   
   call ESMF_GridCompSetServices(topComp,SetServices,rc=localrc)

   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 
   
   topState = ESMF_StateCreate(name="topState",rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

   call ESMF_GridCompInitialize(topComp,importState=topState,exportState=topState,clock=clock,rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

   call ESMF_GridCompRun(topComp,importState=topState,exportState=topState,clock=clock,rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

! Destroy toplevel component and clean up
   call ESMF_GridCompFinalize(topComp,importState=topState,exportState=topState,clock=clock,rc=localrc)
   call ESMF_GridCompDestroy(topComp,rc=localrc)
   call ESMF_LogWrite("All ESMF components destroyed", ESMF_LOGMSG_INFO)

   call ESMF_TimeSet(time2,rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 
   call ESMF_TimeSyncToRealTime(time2,rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

   call ESMF_TimeGet(time2,timeStringISOFrac=timestring)
   call ESMF_TimeIntervalGet(time2-time1,s_r8=seconds) 

   call ESMF_LogWrite('ESMF/FABM0d finished on '//timestring,ESMF_LOGMSG_INFO)   
   call ESMF_Finalize(rc=localrc,endflag=ESMF_END_NORMAL)
   
   end
!EOC

!-----------------------------------------------------------------------
! Copyright by the MOSSCO-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------
