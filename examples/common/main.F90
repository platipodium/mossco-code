program main
   
   use esmf 
   use toplevel_component, only: SetServices
   use mossco_time
   use mossco_strings

   implicit none

   character(LEN=8)          :: datestr
   type(ESMF_Time)           :: time1, time2, startTime, stopTime
   type(ESMF_TimeInterval)   :: timeStepIntv
   integer                   :: localrc, rc, petCount,nmlunit=2013
   double precision          :: seconds,timestep=360.0
   character(len=40)         :: timestring
   character(len=40)         :: start='2000-01-01 00:00:00'
   character(len=40)         :: stop='2000-01-02 00:00:00'
   character(len=40)         :: title='main'
   type(ESMF_GridComp)       :: topComp
   type(ESMF_State)          :: topState ! for import and export, empty
   type(ESMF_Clock)          :: clock
   type(ESMF_VM)             :: vm
   integer                   :: iostat

   namelist /mossco_run/ title,start,stop
   

! Read mossco_run.nml
   open(nmlunit,file='mossco_run.nml',status='old',action='read',iostat=iostat)
   if (iostat .eq. 0) then
      read(nmlunit,nml=mossco_run)
      close(nmlunit)
   end if
! substitute characters in title string
   call replace_character(title,'/','-')
   call replace_character(title,' ','_')

! Initialize
   call ESMF_Initialize(defaultLogFileName=trim(title),rc=localrc,&
     logkindflag=ESMF_LOGKIND_MULTI,defaultCalKind=ESMF_CALKIND_GREGORIAN,&
     vm=vm)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 
   call ESMF_LogWrite(trim(title)//" start", ESMF_LOGMSG_INFO)

! Get the wall clock starting time
   call ESMF_TimeSet(time1,rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 
   call ESMF_TimeSyncToRealTime(time1,rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 
   call ESMF_TimeGet(time1,timeStringISOFrac=timestring)
   call ESMF_LogWrite("Program starts at wall clock "//timestring, ESMF_LOGMSG_INFO)
  
! Create and initialize a clock from mossco_run.nml

   call ESMF_TimeIntervalSet(timeStepIntv, s_r8=real(timestep,kind=ESMF_KIND_R8), rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

   call timeString2ESMF_Time(start,startTime)
   call timeString2ESMF_Time(stop,stopTime)

   clock = ESMF_ClockCreate(timeStep=timeStepIntv, startTime=startTime, stopTime=stopTime, & 
     name="Parent clock", rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

   call ESMF_TimeGet(startTime,timeStringISOFrac=timestring)
   call ESMF_LogWrite("Simulation starts at "//timestring, ESMF_LOGMSG_INFO)
   call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring)
   call ESMF_LogWrite("Simulation ends at "//timestring, ESMF_LOGMSG_INFO)

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

   call ESMF_LogWrite(trim(title)//' finished on '//timestring,ESMF_LOGMSG_INFO)

   call ESMF_StateDestroy(topState,rc=rc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_ClockDestroy(clock,rc=rc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   call ESMF_Finalize(rc=localrc,endflag=ESMF_END_NORMAL)
   
end program main
