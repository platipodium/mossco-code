module esmf_toplevel_component

  use esmf
  use empty_component, only: empty_SetServices => SetServices
  use constant_component, only: constant_SetServices => SetServices
  use mossco_time

  implicit none

  character(ESMF_MAXSTR), dimension(:), allocatable :: componentNames, couplingNames
  type(ESMF_GridComp),dimension(:),save, allocatable  :: childComponents
  type(ESMF_State), dimension(:), save, allocatable   :: exportStates, importStates
  type(ESMF_Alarm), dimension(:), save, allocatable   :: couplingAlarms
  type(ESMF_Clock)      :: clock
  
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
    integer(ESMF_KIND_I4), dimension(:), allocatable :: couplingHours

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)

    !call ESMF_GridCompGet(gridComp,name=name,rc=rc)
    !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
    !! @todo read some kind of input file that describes the coupling
    !! 1. find out how many different components, allocate n
    !! 2. find out the pointer to the correct SetServices routine
    !! 3. for each pair, create the coupling alarm
 
 
    ! Create n child components, call their setservices, and create states
    allocate(childComponents(n))
    allocate(componentNames(n))
    allocate(exportStates(n))
    allocate(importStates(n))   
 
    do i=1,n
      write(componentnames(i),'(A,I1)') 'child_component_',i
      childComponents(i)= ESMF_GridCompCreate(name=trim(componentnames(i)), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      if (i==1) then
        call ESMF_GridCompSetServices(childComponents(i),empty_SetServices, rc=rc)
      else
        call ESMF_GridCompSetServices(childComponents(i),constant_SetServices, rc=rc)
      endif
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      write(name,'(A,A)') trim(componentnames(i)),'_import_state'
      importStates(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name=trim(name))
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      write(name,'(A,A)') trim(componentnames(i)),'_export_state'
      exportStates(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name=trim(name))
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
      call ESMF_AttributeSet(childComponents(i),'component_index',i,rc=rc)      
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_GridCompInitialize(childComponents(i), importState=importStates(i), &
        exportState=exportStates(i), clock=parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    enddo
    
    n=4
    allocate(couplingAlarms(n))
    allocate(couplingnames(n))
    allocate(couplinghours(n))
    
    !! @todo the following information should come from external config/yaml  
    write(couplingnames(1),'(A,A,A)') trim(componentnames(1)),MOSSCO_CPL_SEPARATOR, &
     trim(componentnames(2))
    write(couplingnames(2),'(A,A,A)') trim(componentnames(1)),MOSSCO_CPL_SEPARATOR, &
     trim(componentnames(3))
    write(couplingnames(3),'(A,A,A)') trim(componentnames(2)),MOSSCO_CPL_SEPARATOR, &
     trim(componentnames(1))
    write(couplingnames(4),'(A,A,A)') trim(componentnames(3)),MOSSCO_CPL_SEPARATOR, &
     trim(componentnames(2))

    couplinghours=(/60,90,40,40/) 
 
    call ESMF_AttributeSet(childComponents(1),couplingnames(1),couplinghours(1),rc=rc)
    call ESMF_AttributeSet(childComponents(1),couplingnames(2),couplinghours(2),rc=rc)
    call ESMF_AttributeSet(childComponents(1),couplingnames(3),couplinghours(3),rc=rc)
    call ESMF_AttributeSet(childComponents(2),couplingnames(1),couplinghours(1),rc=rc)
    call ESMF_AttributeSet(childComponents(2),couplingnames(3),couplinghours(3),rc=rc)
    call ESMF_AttributeSet(childComponents(3),couplingnames(4),couplinghours(4),rc=rc)
 
   !! Set the coupling alarm starting from current time of parent clock
    call ESMF_ClockGet(parentClock,startTime=time,rc=rc)
   
    do i=1,n 
      call ESMF_TimeIntervalSet(alarmInterval,h=couplinghours(i),rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)    
      
      couplingAlarms(i)=ESMF_AlarmCreate(clock=parentClock,ringTime=time+alarmInterval, &
        ringInterval=alarmInterval,name=trim(couplingnames(i)),rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
    enddo

    !! Search the clock for next ringing Alarm
    call ESMF_ClockGetAlarmList(parentClock,ESMF_ALARMLIST_ALL,alarmCount=n,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    allocate(alarmList(n))
    
    call ESMF_ClockGetAlarmList(parentClock,ESMF_ALARMLIST_ALL,alarmList=alarmList,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    if (size(alarmList).gt.0) then
      call ESMF_AlarmGet(alarmList(1),ringTime=time,rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      !call ESMF_AlarmPrint(alarmList(1))
    endif
      
    do i=2,size(alarmList)
      call ESMF_AlarmGet(alarmList(i),ringTime=ringTime,rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if (ringtime<time) time=ringTime
      !call ESMF_AlarmPrint(alarmList(i))
    enddo
    if (allocated(alarmList)) deallocate(alarmList)
    
    call ESMF_ClockGet(parentClock,currTime=currentTime,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(parentClock,timeStep=time-currentTime,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    clock=ESMF_ClockCreate(parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO) 

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc
    
    type(ESMF_GridComp), dimension(:), allocatable :: componentList
    integer(ESMF_KIND_I4) :: count,i,k,j, n
    type(ESMF_Time)       :: startTime, currentTime
    type(ESMF_TimeInterval) :: timeInterval
    character(ESMF_MAXSTR) :: name, message
    real(ESMF_KIND_R8) :: timestep
    integer(ESMF_KIND_I4) :: hours
    type(ESMF_Alarm), dimension(:), allocatable :: alarmList
    logical :: isPresent

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !! Run *all* component the first time, each until their next coupling alarm is going off
    call ESMF_ClockGet(parentClock,currTime=currentTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
    if (.not. ESMF_ClockIsStopTime(parentClock, rc=rc)) then
      count=size(childComponents)
      do i=1,count
        !! Determine for each child the clock and run the component with this clock   
        call ESMF_GridCompGet(childComponents(i),name=name, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        call MOSSCO_ClockGetTimeStepToNextAlarm(parentClock, name, timeInterval, rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call ESMF_ClockSet(clock, startTime=currentTime, stopTime=currentTime+timeInterval, timeStep=timeInterval, rc=rc) 
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
        call ESMF_TimeIntervalGet(timeInterval, h=hours, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
        write(message,'(A,A,A,G8.2,A)') 'Calling ',trim(name),' to run for ', hours, ' hours'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO, rc=rc);
        
        call ESMF_GridCompRun(childComponents(i),importState=importStates(i),exportState=exportStates(i), &
          clock=clock, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      enddo  
    endif

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))

    !! Advance parentClock by next minimum time step of coupling
      call ESMF_ClockAdvance(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
     
      call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_RINGING,alarmCount=n,rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
      if (n<1) cycle
      allocate(alarmList(n))
  
      call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_RINGING,alarmList=alarmList,rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
      ! Go through the list of ringing alarms 
      allocate(componentList(2))
      do k=1,count
      
        write(*,'(A,I1,A)') 'Cycling ',k,' alarms.'
        call ESMF_AlarmGet(alarmList(k), name=name, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
        ! Find the 2 components that have as attribute this alarm name
        ! then do their coupling
        do i=1,size(childComponents)
          call ESMF_AttributeGet(childComponents(i),trim(name),isPresent=isPresent)
          if (.not.isPresent) cycle 
          componentList(1)=childComponents(i)
          do j=i,size(childComponents)
            call ESMF_AttributeGet(childComponents(i),trim(name),isPresent=isPresent)
            if (.not.isPresent) cycle 
            componentList(2)=childComponents(i)
          enddo
        enddo
            
        ! Do the copuling
        ! call MOSSCO_do_copuling(componentList)
      enddo
      if (allocated(componentList)) deallocate(componentList)
   
      do i=1,size(childComponents)
        do k=1,count
          call ESMF_AlarmGet(alarmList(k), name=name, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
          if (index(componentNames(i), name)==0) cycle
          
          call MOSSCO_ClockGetTimeStepToNextAlarm(parentClock, trim(componentNames(i)), timeInterval, rc)
          call ESMF_ClockSet(clock, startTime=currentTime, stopTime=currentTime+timeInterval, &
            timeStep=timeInterval, rc=rc) 
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
          call ESMF_TimeIntervalGet(timeInterval, s_r8=timestep, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
          write(message,'(A,A,A,G8.2,A)') 'Calling ',trim(name),' to run for ', timestep, ' seconds'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO, rc=rc);
        
          call ESMF_GridCompRun(childComponents(i),importState=importStates(i),exportState=exportStates(i), &
            clock=clock, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        enddo
      ! call ESMF_ClockSetRinginAlarmsOff
      ! call MOSSCO_ClockSetTimeStepByAlarms
      enddo  
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


end module esmf_toplevel_component
