module esmf_toplevel_component

  use esmf
  use empty_component, only: empty_SetServices => SetServices

  implicit none

  type(ESMF_GridComp),dimension(:),save, allocatable  :: childComponents
  type(ESMF_State), dimension(:), save, allocatable   :: exportStates, importStates
  type(ESMF_Alarm), dimension(:), save, allocatable   :: couplingAlarms
  
  public SetServices

  contains

  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)    :: gridComp
    type(ESMF_State)       :: importState
    type(ESMF_State)       :: exportState
    type(ESMF_Clock)       :: parentClock
    integer, intent(out)   :: rc

    integer                :: i,n=3
    character(ESMF_MAXSTR) :: name
    type(ESMF_Time)        :: time,currentTime,ringTime
    type(ESMF_TimeInterval) :: alarmInterval, timeInterval
    type(ESMF_Alarm),dimension(:),allocatable :: alarmList

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)

    call ESMF_GridCompGet(gridComp,name=name,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
    !! @todo read some kind of input file that describes the coupling
    !! 1. find out how many different components, allocate n
    !! 2. find out the pointer to the correct SetServices routine
    !! 3. for each pair, create the coupling alarm
 
 
    ! Create n child components, call their setservices, and create states
    allocate(childComponents(n))
    allocate(exportStates(n))
    allocate(importStates(n))
    
    
    do i=1,n
      write(name,'(A,I1)') 'child_component_',i
      childComponents(i)= ESMF_GridCompCreate(name=trim(name), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_GridCompSetServices(childComponents(i),empty_SetServices, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      write(name,'(A,I1,A)') 'child_component_',i,'_import_state'
      importStates(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name=trim(name))
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      write(name,'(A,I1,A)') 'child_component_',i,'_export_state'
      exportStates(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name=trim(name))
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_GridCompInitialize(childComponents(i), importState=importStates(i), &
        exportState=exportStates(i), clock=parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    enddo
    
    n=6
    allocate(couplingAlarms(n))
    
    !! Set the coupling alarm starting from current time of parent clock
    call ESMF_ClockGet(parentClock,startTime=time,rc=rc)
    
    call ESMF_TimeIntervalSet(alarmInterval,h=60,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)    
    couplingAlarms(1)=ESMF_AlarmCreate(clock=parentClock,ringTime=time+alarmInterval, &
      ringInterval=alarmInterval,name="1-2",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_TimeIntervalSet(alarmInterval,h=90,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)    
    couplingAlarms(2)=ESMF_AlarmCreate(clock=parentClock,ringTime=time+alarmInterval, &
      ringInterval=alarmInterval,name="1-3",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_TimeIntervalSet(alarmInterval,h=40,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)    
    couplingAlarms(3)=ESMF_AlarmCreate(clock=parentClock,ringTime=time+alarmInterval, &
      ringInterval=alarmInterval,name="2-1",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_TimeIntervalSet(alarmInterval,h=40,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)    
    couplingAlarms(4)=ESMF_AlarmCreate(clock=parentClock,ringTime=time+alarmInterval, &
      ringInterval=alarmInterval,name="3-2",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    !! Search the clock for next ringing Alarm
        
    call ESMF_ClockGetAlarmList(parentClock,ESMF_ALARMLIST_ALL,alarmCount=n,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    allocate(alarmList(n))
    
    call ESMF_ClockGetAlarmList(parentClock,ESMF_ALARMLIST_ALL,alarmList=alarmList,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    if (size(alarmList).gt.0) then
      call ESMF_AlarmGet(alarmList(1),ringTime=time,rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
      
    do i=2,size(alarmList)
      call ESMF_AlarmGet(alarmList(i),ringTime=ringTime,rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if (ringtime<time) time=ringTime
    enddo
    if (allocated(alarmList)) deallocate(alarmList)
    
    call ESMF_ClockGet(parentClock,currTime=currentTime,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(parentClock,timeStep=time-currentTime,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO) 

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc
    
    type(ESMF_GridComp), dimension(:,:), pointer :: ringingComp
    integer(ESMF_KIND_I4) :: count,i,k,j
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: startTime, currentTime
    type(ESMF_TimeInterval) :: timeInterval

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !! Run *all* component the first time, each until their next coupling alarm is going off
    clock = ESMF_ClockCreate(clock=parentClock, rc=rc)    
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

	  call ESMF_ClockGet(parentClock,currTime=currentTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (.not. ESMF_ClockIsStopTime(parentClock, rc=rc)) then
      count=size(childComponents)
      do i=1,count
        !! Determine for each child the clock and run the component with this clock   
        !call MOSSCO_TimeIntervalGetFromAlarms(parentClock, comp=childComponents(i), timeInterval=timeInterval, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call ESMF_ClockSet(clock, startTime=currentTime, stopTime=currentTime+timeInterval, timeStep=timeInterval, rc=rc) 
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
      	call ESMF_GridCompRun(childComponents(i),importState=importStates(i),exportState=exportStates(i), &
      	  clock=clock, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      enddo  
    endif

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))

	    !! Advance parentClock by next minimum time step of coupling
      call ESMF_ClockAdvance(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
     
      ! call MOSSCO_ClockGetRingingComponents(parentClock, ringingComp=ringingComp, count=count, rc=rc)
      do k=1,count
         !call MOSSCO_GridCompGetIndex(ringingComp(k,1), index=i, rc=rc)
         !call MOSSCO_GridCompGetIndex(ringingComp(k,2), index=j, rc=rc)
         
         
         call ESMF_GridCompRun(ringingComp(k,1),importState=importStates(i), exportState=exportStates(i), &
           clock=parentClock, rc=rc)
         
         ! call MOSSCO_GridCompCoupling(ringingComp(k,:), importState=exportStates(i), exportState=importStates(j), parentClock, rc=rc) 
          
         call ESMF_GridCompRun(ringingComp(k,2),importState=importStates(j), exportState=exportStates(j), &
           clock=parentClock, rc=rc) 
      enddo
      ! call ESMF_ClockSetRinginAlarmsOff
      ! call MOSSCO_ClockSetTimeStepByAlarms
      
    enddo 

    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_INFO)
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc
    
    integer              :: i

    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)

    !! @todo destroy alarms        


    if (allocated(couplingAlarms)) deallocate(couplingAlarms)

    do i=1,size(childComponents)
      call ESMF_StateDestroy(exportStates(i),rc=rc)
      call ESMF_StateDestroy(importStates(i),rc=rc)
      call ESMF_GridCompDestroy(childComponents(i),rc=rc)
    enddo

    if (allocated(childComponents)) deallocate(childComponents)
    if (allocated(exportStates)) deallocate(exportStates)
    if (allocated(importStates)) deallocate(importStates)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

subroutine MOSSCO_ClockSetTimeStepByAlarms(clock, rc)
  type(ESMF_Clock), intent(inout) :: clock
  integer(ESMF_KIND_I4), intent(out), optional :: rc

  type(ESMF_TimeInterval) :: timeInterval

	call MOSSCO_ClockGetTimeStepToNextAlarm(clock, timeInterval, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
  call ESMF_ClockSet(clock, timeStep=timeInterval, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
end subroutine MOSSCO_ClockSetTimeStepByAlarms    

!> This subroutine searches a clock's alarms and returns the time interval to the next 
!! ringing alarm
subroutine MOSSCO_ClockGetTimeStepToNextAlarm(clock, timeInterval, rc)
  type (ESMF_Clock), intent(in) :: clock
  type (ESMF_TimeInterval), intent(out) :: timeInterval
  integer(ESMF_KIND_I4), intent(out), optional :: rc

  type(ESMF_Time)         :: ringTime, time, currentTime
  type(ESMF_Alarm), dimension(:), allocatable :: alarmList
  integer(ESMF_KIND_I4) :: n,i

  call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmCount=n,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  allocate(alarmList(n))
  
  call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmList=alarmList,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
  n=size(alarmList)
  if (n>0) then
    call ESMF_AlarmGet(alarmList(1),ringTime=time,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  endif
      
  do i=2,n
    call ESMF_AlarmGet(alarmList(i),ringTime=ringTime,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (ringtime<time) time=ringTime
  enddo
  
  if (allocated(alarmList)) deallocate(alarmList)
  
  call ESMF_ClockGet(clock,currTime=currentTime,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  timeInterval=time - currentTime

end subroutine  MOSSCO_ClockGetTimeStepToNextAlarm 


end module esmf_toplevel_component
