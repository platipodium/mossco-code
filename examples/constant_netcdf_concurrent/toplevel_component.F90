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
  use mossco_variable_types
  use mossco_state

  use constant_component, only : constant_SetServices => SetServices 
  use netcdf_component, only : netcdf_SetServices => SetServices 
  use redist_coupler, only : redist_coupler_SetServices => SetServices 

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),dimension(:),save, allocatable :: gridCompList
  type(ESMF_CplComp),dimension(:), save, allocatable :: cplCompList
  type(ESMF_State), dimension(:),  save, allocatable :: exportStates, importStates
  type(ESMF_Alarm), dimension(:),  save, allocatable :: cplAlarmList
  type(ESMF_Clock), dimension(:),  save, allocatable :: gridCompClockList 
  character(len=ESMF_MAXSTR), dimension(:), save, allocatable :: gridCompNames
  character(len=ESMF_MAXSTR), dimension(:), save, allocatable :: cplCompNames
  character(len=ESMF_MAXSTR), dimension(:), save, allocatable :: cplNames
  type(ESMF_CplComp), save  :: redist_couplerComp
  type(ESMF_GridComp), save :: constantComp
  type(ESMF_GridComp), save :: netcdfComp
  type(ESMF_State), save    :: constantExportState, constantImportState
  type(ESMF_State), save    :: netcdfExportState, netcdfImportState


  contains

  !> Provide an ESMF compliant SetServices routine, which defines
  !! entry points for Init/Run/Finalize
  subroutine SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine SetServices

  !> Initialize the coupling
  !!
  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: clockTime, startTime, stopTime, currTime
    type(ESMF_Time)         :: ringTime, time
    type(ESMF_TimeInterval) :: timeInterval, timeStep, alarmInterval
    real(ESMF_KIND_R8)      :: dt
     
    integer(ESMF_KIND_I4)  :: numGridComp, numCplComp
    integer(ESMF_KIND_I4)  :: alarmCount, numCplAlarm, i
    type(ESMF_Alarm), dimension(:), allocatable :: alarmList !> @todo shoudl this be a pointer?
    character(ESMF_MAXSTR) :: name, message
    type(ESMF_Alarm)       :: childAlarm
    type(ESMF_Clock)       :: childClock
    type(ESMF_Clock)       :: clock !> This component's internal clock
    logical                :: clockIsPresent, vmIsPresent

    integer(ESMF_KIND_I4)  :: localPet, petCount
    integer(ESMF_KIND_I4), allocatable  :: petList(:)
     
    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero

    rc = ESMF_SUCCESS
     
    !! Check whether there is already a clock (it might have been set 
    !! with a prior ESMF_gridCompCreate() call.  If not, then create 
    !! a local clock as a clone of the parent clock, and associate it
    !! with this component.  Finally, set the name of the local clock
    call ESMF_GridCompGet(gridComp, name=name, clockIsPresent=clockIsPresent, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (clockIsPresent) then
      call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)     
    else
      clock = ESMF_ClockCreate(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompSet(gridComp, clock=clock, rc=rc)    
    endif
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock, name=trim(name)//' clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    !! Log the call to this function
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    !! Set up the distribution for concurrent simulations
    call ESMF_GridCompGet(gridComp, vmIsPresent=vmIsPresent, localPet=localPet, &
      petCount=petCount, rc=rc)

    !! Allocate the fields for all gridded components and their names
    numGridComp = 2
    allocate(gridCompList(numGridComp))
    allocate(gridCompClockList(numGridComp))
    allocate(gridCompNames(numGridComp))
    allocate(importStates(numGridComp))
    allocate(exportStates(numGridComp))
    
    gridCompNames(1) = 'constant'
    gridCompNames(2) = 'netcdf'

    !! Create all gridded components, and create import and export states for these
    do i = 1, numGridComp
      gridCompClockList(i) = ESMF_ClockCreate(clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo

    if (petCount>1) then 
      gridCompList(1) = ESMF_GridCompCreate(name=trim(gridCompNames(1))//'Comp',  &
        clock=gridCompClockList(1), petList=(/0,1/), rc=rc)
    else 
      gridCompList(1) = ESMF_GridCompCreate(name=trim(gridCompNames(1))//'Comp',  &
        clock=gridCompClockList(1), petList=(/0/), rc=rc)
    endif
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (petCount>1) then 
      gridCompList(2) = ESMF_GridCompCreate(name=trim(gridCompNames(2))//'Comp',  &
        clock=gridCompClockList(2), petList=(/1/), rc=rc)
    else 
      gridCompList(2) = ESMF_GridCompCreate(name=trim(gridCompNames(2))//'Comp',  &
        clock=gridCompClockList(2), petList=(/0/), rc=rc)
    endif
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


    do i = 1, numGridComp
      exportStates(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
        name=trim(gridCompNames(i))//'ExportState')
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      importStates(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
        name=trim(gridCompNames(i))//'ImportState')
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
   
    !! Now register all setServices routines for the gridded components
    call ESMF_GridCompSetServices(gridCompList(1), constant_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gridCompList(2), netcdf_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !! Allocate the fields for all coupler components and their names
    numCplComp = 1
    allocate(cplCompList(numCplComp))
    allocate(cplCompNames(numCplComp))
    cplCompNames(1) = 'redist_coupler'
    
    do i = 1, numCplComp
      cplCompList(i) = ESMF_CplCompCreate(name=trim(cplCompNames(i))//'Comp', rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
    
    call ESMF_CplCompSetServices(cplCompList(1), redist_coupler_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !! Initialize all components, both cpl and grid components, do this
    !! in the order specified by dependencies/couplings
    !! Also, try to find coupling/dependency specific export/import states in
    !! the initialization
    !! Initializing 0: redist_coupler
    call ESMF_CplCompInitialize(cplCompList(1), importState=exportStates(1), &
      exportState=importStates(2), clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !! Initializing 1: constant
    call ESMF_GridCompInitialize(gridCompList(1), importState=importStates(1), &
      exportState=exportStates(1), clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !! Initializing 2: netcdf
    !! which couples from constant
    call ESMF_CplCompRun(cplCompList(1), importState=exportStates(1), &
      exportState=importStates(2), clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(gridCompList(2), importState=importStates(2), &
      exportState=exportStates(2), clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    numCplAlarm = 1
    if (.not.allocated(cplAlarmList)) allocate(cplAlarmList(numCplAlarm))
    if (.not.allocated(cplNames)) allocate(cplNames(numCplAlarm))
    cplNames(:) = 'redist'

    !! Set the coupling alarm starting from start time of local clock
    call ESMF_ClockGet(clock,startTime=startTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
    call ESMF_TimeIntervalSet(alarmInterval, startTime, h=48 ,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    cplAlarmList(1)=ESMF_AlarmCreate(clock=clock,ringTime=startTime+alarmInterval, &
      ringInterval=alarmInterval, name='constant--netcdf--cplAlarm', rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Copy this alarm to all children as well
    do i=1,numGridComp
      call ESMF_GridCompGet(gridCompList(i),name=name, rc=rc) 
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
      if (trim(name)=='constantComp' .or. trim(name)=='netcdfComp') then
        call ESMF_GridCompGet(gridCompList(i), clockIsPresent=clockIsPresent, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
        if (clockIsPresent) then
          call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=rc) 
        else
          call ESMF_LOGWRITE('Creating clock for '//trim(name)//', this should have been done by the component.', &
            ESMF_LOGMSG_WARNING)        
        
          childClock=ESMF_ClockCreate(clock=clock, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          
          call ESMF_GridCompSet(gridCompList(i),clock=childClock, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        endif      
        childAlarm=ESMF_AlarmCreate(cplAlarmList(1), rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        call ESMF_AlarmSet(childAlarm, clock=childClock)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
      endif
    enddo
    
    !! Set the default ringTime to the stopTime of local clock, then get all Alarms
    !! from local clock into alarmList, find those that contain the string "cplAlarm"
    !! and look for the earliest ringtime in all coupling alarms.  Save that in the
    !! ringTime
    call ESMF_ClockGet(clock, stopTime=ringTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc) 
       
    call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmCount=alarmCount,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (.not.allocated(alarmList)) allocate(alarmList(alarmCount))
    call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmList=alarmList,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do i=1,ubound(alarmList,1)
      call ESMF_AlarmGet(alarmList(i), ringTime=time, name=name, rc=rc)
      
      call ESMF_TimeGet(time,timeStringISOFrac=timestring)
      write(message,'(A)') trim(name)//' rings at '//trim(timestring)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      
      if (index(trim(name),'cplAlarm') < 1) cycle 
      if (time<ringTime) ringTime=time    
    enddo
    if (allocated(alarmList)) deallocate(alarmList)
    
    !! Set the timestep such that it corresponds to the time until the 
    !! first ringing alarm, log that time
    call ESMF_ClockGet(clock,currTime=currTime,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock,timeStep=ringTime-currTime,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
    call ESMF_GridCompGet(gridComp, name=name, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    write(message,'(A)') trim(name)//' alarms ring next at '//trim(timestring)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    
    !! Log the successful completion of this function
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initialized'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: timestring, cplName, myName
    type(ESMF_Time)            :: stopTime, currTime, ringTime, time
    type(ESMF_TimeInterval)    :: timeInterval, ringInterval
    integer(ESMF_KIND_I8)      :: advanceCount,  i, j, k, l
    integer(ESMF_KIND_I4)      :: alarmCount, petCount, localPet
    integer(ESMF_KIND_I4)      :: numGridComp, numCplComp, hours
    
    type(ESMF_Alarm), dimension(:), allocatable :: alarmList
    type(ESMF_Alarm)        :: childAlarm
    type(ESMF_Clock)        :: childClock, clock
    logical                 :: clockIsPresent
    type(ESMF_State)        :: impState, expState
    type(ESMF_Field)        :: field
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_Array)        :: array
    type(ESMF_ArrayBundle)  :: arrayBundle
    type(ESMF_StateItem_Flag), dimension(:), allocatable :: itemTypeList
    character(len=ESMF_MAXSTR), dimension(:), allocatable:: itemNameList
    integer                  :: itemCount
    
    character(len=ESMF_MAXSTR) :: message, compName, name, alarmName, otherName   
    
    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero

    if (.not.allocated(alarmList)) allocate(alarmList(20))

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (.not.clockIsPresent) then
      call ESMF_LogWrite('Required clock not found in '//trim(name), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    numGridComp=ubound(gridCompList,1)-lbound(gridCompList,1)+1
    
    call ESMF_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
      alarmCount=alarmCount, rc=rc)
    
    if (allocated(alarmList)) then
      if (size(alarmList)<alarmCount) then
        deallocate(alarmList)
        allocate(alarmList(alarmCount))
      endif
    else 
      allocate(alarmList(alarmCount))
    endif
   
    !! Run until the clock's stoptime is reached
    do 

      call ESMF_ClockGet(clock,currTime=currTime, stopTime=stopTime, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      if (currTime>stopTime) then
        call ESMF_LogWrite('Clock out of scope in '//trim(compName), ESMF_LOGMSG_ERROR)
        call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=rc)
      endif

      !! Loop through all components and check whether their clock is currently at the 
      !! same time as my own clock's currTime, if yes, then run the respective couplers
      do i=1,numGridComp
        !! Determine for each child the clock    
        call ESMF_GridCompGet(gridCompList(i),name=compName, clockIsPresent=clockIsPresent, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        if (.not.clockIsPresent) then
          call ESMF_LogWrite('Required clock not found in '//trim(compName), ESMF_LOGMSG_ERROR)
          call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=rc)
        endif

        call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
        call ESMF_ClockGet(childClock,currTime=time, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        if (time>currTime) cycle

        !! Find all the alarms in this child and call all the couplers that
        !! have ringing alarms at this stage
   
        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmCount=alarmCount, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        if (alarmCount>ubound(alarmList,1)) then
          deallocate(alarmList)
          allocate(alarmList(alarmCount))
        endif
                
        if (alarmCount==0) then
          timeInterval=stopTime-currTime
        else                 
          call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
             alarmList=alarmList, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        endif
        
        do j=1,alarmCount
          call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=ringTime, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
          !! Skip this alarm if it is not a cplAlarm
          if (index(trim(alarmName),'cplAlarm') < 1) cycle
  
          !! Skip this alarm if it is inbound of this component
          if (trim(alarmName(1:index(alarmName,'--')-1))/=trim(compName(1:index(compName,'Comp')-1))) cycle
            
          !! Skip this alarm if it is not ringing now
          !if (ringTime > currTime) cycle
            
          call ESMF_TimeGet(ringTime,timeStringISOFrac=timeString)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
          
          !write(0,*) trim(compName)//' ', i,'/',alarmCount,' '//trim(alarmName)//' rings at '//trim(timeString)
          write(message,'(A)') trim(compName)//' '//trim(alarmName)//' rings at '//trim(timeString)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)
            
          myName=trim(alarmName(1:index(alarmName,'--')-1))
          otherName=trim(alarmName(index(alarmName,'--')+2:index(alarmName,'--cplAlarm')-1))
          
          do k=1,ubound(cplAlarmList,1) 
            if (cplAlarmList(k) == alarmList(j)) then
              cplName = trim(cplNames(k))
              exit
            endif
          enddo
            
          write(message,'(A)') trim(timeString)//' '//trim(myName)//' ->'
          if (trim(cplName) /= 'redist') then
            write(message,'(A)') trim(message)//' '//trim(cplName)//' ->'
          else
            write(message,'(A)') trim(message)//' ('//trim(cplName)//') ->'
          endif
          write(message,'(A)') trim(message)//' '//trim(otherName)  
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)            
            
          call ESMF_GridCompGet(gridCompList(i), exportState=impState, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

          !! Search the gridCompList for other's name
          do k=1, ubound(gridCompList,1)
              call ESMF_GridCompGet(gridCompList(k), name=name, rc=rc)
              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)              
              if (trim(name)==trim(otherName)//'Comp') exit
          enddo

          if (trim(name) /= trim(otherName)//'Comp') then
            write(message,'(A)') 'Did not find component '//trim(otherName)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)  
            call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)          
          endif

          !! Search the cplCompList for cplName
          do l=1, ubound(cplCompNames,1)
              !write(0,*) l,trim(cplCompNames(l))//' ?= '//trim(cplName)//'_coupler' 
              if (trim(cplCompNames(l))==trim(cplName)//'_coupler') exit
          enddo

          call ESMF_GridCompGet(gridCompList(k), importState=expState, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          
          call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
          write(message,'(A)') trim(timeString)//' Calling '//trim(cplCompNames(l))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)  
          
          call ESMF_CplCompRun(cplCompList(l), importState=impState, &
            exportState=expState, clock=clock, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          
        enddo 
      enddo

      !! Loop through all components and check whether their clock is currently at the 
      !! same time as my own clock's currTime, if yes, then run the component
      do i=1,numGridComp
        !! Determine for each child the clock    
        call ESMF_GridCompGet(gridCompList(i),name=compName, clockIsPresent=clockIsPresent, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        if (.not.clockIsPresent) then
          call ESMF_LogWrite('Required clock not found in '//trim(compName), ESMF_LOGMSG_ERROR)
          call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=rc)
        endif

        call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
        call ESMF_ClockGet(childClock,currTime=time, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        if (time>currTime) cycle
        
        !! Find the child's alarm list, get the interval to the next ringing alarm
        !! and run the component for the interval until that alarm
   
        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmCount=alarmCount, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        if (alarmCount==0) then
          !call ESMF_LogWrite('No alarm found in '//trim(compName), ESMF_LOGMSG_WARNING)
          timeInterval=stopTime-currTime
        else                 
          call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
             alarmList=alarmList, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        endif

        !! Set the default ringTime to the stopTime of local clock, then get all Alarms
        !! from local clock into alarmList, find those that contain the string "cplAlarm"
        !! and look for the earliest ringtime in all coupling alarms.  Save that in the
        !! ringTime
        call ESMF_ClockGet(clock, stopTime=ringTime, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc) 
                
        do j=1,alarmCount
          call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=time, &
            ringInterval=ringInterval, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          if (index(trim(alarmName),'cplAlarm')<1) cycle
        
          if (time==currTime) ringTime=currTime+ringInterval
          if (time<ringTime) ringTime=time
        enddo 

        timeInterval=ringTime-currTime
        
        call ESMF_ClockSet(childClock, stopTime=ringTime, rc=rc) 
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call ESMF_TimeIntervalGet(timeInterval, h=hours, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
        write(message,'(A,A,G6.2,A)') trim(timeString)//' calling '//trim(compName), &
          ' to run for ', hours, ' h'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=rc);
        
        call ESMF_GridCompRun(gridCompList(i),importState=importStates(i),&
          exportState=exportStates(i), clock=clock, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
        call ESMF_ClockGet(childClock, currTime=time, rc=rc) 
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
        if (time == currTime) then
          !! This child component did not advance its clock in its Run() routine
          !! We do that here
          call ESMF_ClockAdvance(childClock, timeStep=timeInterval, rc=rc) 
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
      enddo

      !! Now that all child components have been started, find out the minimum time
      !! to the next coupling and use this as a time step for my own clock Advance
      call ESMF_GridCompGet(gridComp, name=name, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)      
      
      call ESMF_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
        alarmCount=alarmCount, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      if (alarmCount==0) then
        !call ESMF_LogWrite('No alarm found in '//trim(name), ESMF_LOGMSG_WARNING)
        timeInterval=stopTime-currTime
      else                 
        call ESMF_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmList=alarmList, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
        call ESMF_AlarmGet(alarmList(1), ringTime=ringTime, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
        do j=2,alarmCount        
          call ESMF_AlarmGet(alarmList(j), ringTime=time, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
     
          if (time<ringTime) ringTime=time
        enddo 

        timeInterval=ringTime-currTime
      endif

      !> Log current and next ring time 
      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      write(message,'(A)') trim(timeString)//' '//trim(name)//' stepping to'
      call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      write(message,'(A)') trim(message)//' '//trim(timeString)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=rc);

      !> Set new time interval and advance clock, stop if end of 
      !! simulation reached
      call ESMF_ClockSet(clock, timeStep=timeInterval, rc=rc) 
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ClockAdvance(clock, rc=rc) 
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_ClockIsStopTime(clock, rc=rc)) exit
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)           
    enddo

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

!#ifdef DEBUG 
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finished running.'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO, rc=rc);
!#endif

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I8)   :: i
    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero

    !> Obtain information on the component, especially whether there is a local
    !! clock to obtain the time from and to later destroy
    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (.not.clockIsPresent) then
      clock=parentClock
    else 
      call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    
    !> Get the time and log it
    call ESMF_ClockGet(clock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' finalizing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    do i=1,ubound(cplCompList,1)
      call ESMF_CplCompFinalize(cplCompList(i), clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
    do i=1,ubound(gridCompList,1)
      call ESMF_GridCompFinalize(gridCompList(i), clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
    do i=1,ubound(gridCompList,1)
      !!@todo destroy any remaining fields/arrays in states   
      call ESMF_StateDestroy(exportStates(i), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_StateDestroy(importStates(i), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
    
    call ESMF_GridCompDestroy(gridCompList(1), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gridCompList(2), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompDestroy(cplCompList(1), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (allocated(gridCompClockList)) deallocate(gridCompClockList) 
    if (allocated(gridCompList)) deallocate(gridCompList) 
    if (allocated(cplCompList))  deallocate(cplCompList) 
    if (allocated(exportStates)) deallocate(exportStates) 
    if (allocated(importStates)) deallocate(importStates) 
    if (allocated(cplAlarmList)) deallocate(cplAlarmList)
  
    if (clockIsPresent) call ESMF_ClockDestroy(clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finalized'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE) 
  
  end subroutine Finalize

end module toplevel_component
