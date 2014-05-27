!> @brief Implementation of an ESMF intermediate coupling
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
module gotmfabm_component

  use esmf
  use mossco_variable_types
  use mossco_state

  use gotm_component, only : gotm_SetServices => SetServices 
  use fabm_gotm_component, only : fabm_gotm_SetServices => SetServices 

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),dimension(:),save, allocatable :: gridCompList
  type(ESMF_Clock), dimension(:),  save, allocatable :: gridCompClockList 
  character(len=ESMF_MAXSTR), dimension(:), save, allocatable :: gridCompNames
  type(ESMF_GridComp), save :: gotmComp
  type(ESMF_GridComp), save :: fabm_gotmComp

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
    type(ESMF_Time)         :: time
    type(ESMF_TimeInterval) :: timeInterval, timeStep
    real(ESMF_KIND_R8)      :: dt
     
    integer(ESMF_KIND_I4)  :: numGridComp, petCount
    integer(ESMF_KIND_I4)  :: i
    character(ESMF_MAXSTR) :: name, message
    type(ESMF_Clock)       :: childClock
    type(ESMF_Clock)       :: clock !> This component's internal clock
    logical                :: clockIsPresent
    integer(ESMF_KIND_I4), allocatable :: petList(:)
    type(ESMF_VM)          :: vm
     
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

    !! Allocate the fields for all gridded components and their names
    numGridComp = 2
    allocate(gridCompList(numGridComp))
    allocate(gridCompClockList(numGridComp))
    allocate(gridCompNames(numGridComp))
    
    gridCompNames(1) = 'gotm'
    gridCompNames(2) = 'fabm_gotm'

    !! Create all gridded components, and create import and export states for these
    call ESMF_GridCompGet(gridComp, vm=vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_VmGet(vm, petCount=petCount, rc=rc)
    allocate(petList(petCount))
    do i=1,petCount
      petList(i)=i-1
    enddo

    do i = 1, numGridComp
      gridCompClockList(i) = ESMF_ClockCreate(clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      gridCompList(i) = ESMF_GridCompCreate(name=trim(gridCompNames(i))//'Comp',  &
        petList=petList, clock=gridCompClockList(i), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
   
    !! Now register all setServices routines for the gridded components
    call ESMF_GridCompSetServices(gridCompList(1), gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gridCompList(2), fabm_gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !! Initializing gotm
    call ESMF_GridCompInitialize(gridCompList(1), importState=importState, &
      exportState=exportState, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !! Initializing fabm_gotm
    call ESMF_GridCompInitialize(gridCompList(2), importState=importState, &
      exportState=exportState, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
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

    character(len=ESMF_MAXSTR) :: timestring
    type(ESMF_Time)            :: stopTime, currTime, time
    type(ESMF_TimeInterval)    :: timeInterval
    integer(ESMF_KIND_I8)      :: advanceCount,  i, j, k, l
    integer(ESMF_KIND_I4)      :: petCount, localPet
    integer(ESMF_KIND_I4)      :: numGridComp, hours
    
    type(ESMF_Clock)        :: childClock, clock
    logical                 :: clockIsPresent
    type(ESMF_Field)        :: field
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_Array)        :: array
    type(ESMF_ArrayBundle)  :: arrayBundle
    type(ESMF_StateItem_Flag), dimension(:), allocatable :: itemTypeList
    character(len=ESMF_MAXSTR), dimension(:), allocatable:: itemNameList
    integer                  :: itemCount
    
    character(len=ESMF_MAXSTR) :: message, compName, name, otherName   
    
    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (.not.clockIsPresent) then
      call ESMF_LogWrite('Required clock not found in '//trim(name), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,currTime=currTime,  timeStep=timeInterval, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    numGridComp=ubound(gridCompList,1)-lbound(gridCompList,1)+1
       
    !! Run until the clock's stoptime is reached
    do 

      call ESMF_ClockGet(clock,currTime=currTime, stopTime=stopTime, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      if (currTime>stopTime) then
        call ESMF_LogWrite('Clock out of scope in '//trim(compName), ESMF_LOGMSG_ERROR)
        call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=rc)
      endif

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
        call ESMF_TimeGet(time,timeStringISOFrac=timeString)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
        if (time>currTime) then
          call ESMF_TimeGet(time,timeStringISOFrac=timeString)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
          write(message,'(A)') trim(compName)//' now at '//trim(timestring)//', but'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
          
          call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
          write(message,'(A)') trim(name)//' now at '//trim(timestring)//', cycling ...'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)

          cycle
        endif
 
        call ESMF_ClockSet(childClock, stopTime=currTime + timeInterval, rc=rc) 
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
        call ESMF_ClockGet(childClock, timeStep=timeInterval, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc) 
        !if (timeInterval>ringTime-currTime) then
        !  call ESMF_ClockSet(childClock, timeStep=ringTime-currTime, rc=rc)
        !  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc) 
        !endif

        call ESMF_TimeIntervalGet(timeInterval, h=hours, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
        write(message,'(A,A,G6.2,A)') trim(timeString)//' calling '//trim(compName), &
          ' to run for ', hours, ' h'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=rc);
        
        call ESMF_GridCompRun(gridCompList(i),importState=importState,&
          exportState=exportState, clock=clock, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
        call ESMF_ClockGet(childClock, currTime=time, rc=rc) 
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
        if (time == currTime) then
          !! This child component did not advance its clock in its Run() routine
          !! We do that here
          call ESMF_LogWrite(trim(compName)//' did not advance its clock',ESMF_LOGMSG_WARNING)

          call ESMF_ClockAdvance(childClock, timeStep=timeInterval, rc=rc) 
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
      enddo

      !! Now that all child components have been started, find out the minimum time
      !! to the next coupling and use this as a time step for my own clock Advance
      call ESMF_GridCompGet(gridComp, name=name, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)      
      
      
      !> Log current and next ring time 
      call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeInterval, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      write(message,'(A)') trim(timeString)//' '//trim(name)//' stepping to'
      call ESMF_TimeGet(currTime+timeInterval,timeStringISOFrac=timestring, rc=rc)
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

    do i=1,ubound(gridCompList,1)
      call ESMF_GridCompFinalize(gridCompList(i), clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
    
    call ESMF_GridCompDestroy(gridCompList(1), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gridCompList(2), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (allocated(gridCompClockList)) deallocate(gridCompClockList) 
    if (allocated(gridCompList)) deallocate(gridCompList) 
  
    !if (clockIsPresent) call ESMF_ClockDestroy(clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finalized'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE) 
  
  end subroutine Finalize

end module gotmfabm_component
