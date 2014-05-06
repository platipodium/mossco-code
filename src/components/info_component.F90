!> @brief Implementation of an information ESMF gridded component
!> @file info_component.F90
!!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2014 Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module info_component

  use esmf

  implicit none

  private
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
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR):: name, message, timeString
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: currTime
    logical               :: clockIsPresent
    
    integer(ESMF_KIND_I4) :: petCount, localPet
    type(ESMF_VM)         :: vm
    
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


    !> Here comes your own initialization code
    !! In particular, this should contain
    !! 1. Setting your internal timestep and adding it to your clock, this could
    !!    be a timestep read from an external file
    !!    ESMF_TimeIntervalSet(timeStep)
    !!    ESMF_ClockSet(Clock, timeStep=timeStep)
    !!
    !! 2. Creating your own fields, these could be Fields that store a pointer to
    !!    your model's internal fields, or could be a new allocated storage space
    !!    ESMF_FieldCreate() 
    !!
    !! 3. Adding fields to your exportState, so that they are accessible to other
    !!    components in the system.
    !!
    !! 4. Adding fieldname:required attributes to your import State, so that other
    !!    components know what you expect
    
    
    !! Finally, log the successful completion of this function
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initialized'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)
  
  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)     :: gridComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock
    integer, intent(out)    :: rc

    integer(ESMF_KIND_I8)   :: advanceCount
    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent, gridIsPresent, vmIsPresent
    logical                 :: importStateIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    type(ESMF_TimeInterval) :: timeInterval
    
    integer(ESMF_KIND_I4), allocatable, dimension(:) :: exclusiveCount, exclusiveLBound, exclusiveUBound
    integer(ESMF_KIND_I4), allocatable, dimension(:) :: computationalCount, computationalLBound, computationalUBound
    integer(ESMF_KIND_I4), allocatable, dimension(:) :: totalCount, totalLBound, totalUBound
    type(ESMF_Grid)         :: grid
    type(ESMF_Field)        :: field
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    integer(ESMF_KIND_I4)   :: dimCount, rank, localDeCount, i, j, itemCount
    type(ESMF_ArraySpec)    :: arraySpec
    character(len=ESMF_MAXSTR) :: stateName
    type(ESMF_VM)           :: vm
     
    call ESMF_GridCompGet(gridComp,name=name, clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (.not.clockIsPresent) then
      clock = parentClock
    else
      call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    
    call ESMF_ClockGet(clock,currTime=currTime, advanceCount=advanceCount, &
      timeStep=timeInterval, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,I8)') trim(timestring)//' '//trim(name)//' running step ',advanceCount
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    !! Introspect yourself for the parallel environment
    call ESMF_GridCompGet(gridComp, petCount=petCount,localPet=localPet, &
      vmIsPresent=vmIsPresent, rc=rc)
    if (vmIsPresent) call ESMF_GridCompGet(gridComp, vm=vm, rc=rc)
    
    call ESMF_GridCompGet(gridComp, importStateIsPresent=importStateIsPresent, rc=rc)
    if (importStateIsPresent) then 
      !call ESMF_GridCompGet(importState=iState, rc=rc)
    endif

    !! Print info about import State
    call ESMF_StateGet(importState, itemCount=itemCount, name=stateName, rc=rc)
    write(message,'(A,I2,A,I3)') '{name: '//trim(stateName)//', pet: ',localPet,', itemCount: ', itemCount
    write(*,*) trim(message)
    allocate(itemTypeList(itemCount))
    allocate(itemNameList(itemCount))
    if (itemCount>0) write(message,'(A)') 'itemNameList: ['//trim(itemNameList(1))
    do i=2, itemCount
      write(message,'(A)') trim(message)//', '//trim(itemNameList(i))
    enddo
    if (itemCount>0) write(message,'(A)') trim(message)//']'
    write(*,*) trim(message)
    
    do i=1,itemCount
      write(message,'(A)') 'itemName:'//trim(itemNameList(i))
      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(importState, itemNameList(i), field, rc=rc)
        write(message,'(A)') trim(message)//', type: field'
        call ESMF_FieldGet(field, dimCount=dimCount, localDeCount=localDeCount, rank=rank, &
           grid=grid, arraySpec=arraySpec, vm=vm, rc=rc)
        write(message,'(A,I1,A,I1,A,I2)') trim(message)//', dimCount: ',dimCount,' , rank: ',rank,&
          ' , localDeCount: ', localDeCount
          
        allocate(exclusiveLBound(rank))
        allocate(exclusiveUBound(rank))
        allocate(exclusiveCount(rank))
        allocate(computationalLBound(rank))
        allocate(computationalUBound(rank))
        allocate(computationalCount(rank))
        allocate(totalUBound(rank))
        allocate(totalLBound(rank))
        allocate(totalCount(rank))  
        do j=0,localDeCount-1
          write(message,'(A,I2)') 'localDe: ', j
          call ESMF_FieldGetBounds(field,localDe=j, exclusiveLBound=exclusiveLBound, &
            exclusiveUBound=exclusiveUBound, exclusiveCount=exclusiveCount, &
            computationalLBound=computationalLBound, computationalUBound=computationalUbound, &
            computationalCount=computationalCount, totalLBound=totalLBound, &
            totalUBound=totalUbound, totalCount=totalCount, rc=rc)
          write(message,'(A,3I2)') trim(message)//' , exclusiveCount: ',exclusiveCount  
          write(message,'(A,3I2)') trim(message)//' , computationalCount: ',computationalCount  
          write(message,'(A,3I2)') trim(message)//' , totalCount: ',totalCount  
        enddo
        deallocate(exclusiveLbound,exclusiveUBound,exclusiveCount)
        deallocate(computationalLBound,computationalUBound,computationalCount)
        deallocate(totalUBound,totalLBound,totalCount)
      endif
    enddo
    
    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)

    if (clockIsPresent) then 
      do while (.not. ESMF_ClockIsStopTime(clock, rc=rc))

      !! Your own code continued:
      !! 2. Calling a single (or even multiple) internal of your model
       
        call ESMF_ClockAdvance(clock, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      enddo       
    endif
    
    !! 3. You should not have to do anything with the export state, because the mapping
    !!    between your internal model's data and the exported fields has already been
    !!    done in the Initialize() routine.  In MOSSCO, this is recommended practices, but
    !!    don't rely on this.

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finished running.'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=rc)

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

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
   
    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory    
    !! 3. Destroy your clock

    if (clockIsPresent) call ESMF_ClockDestroy(clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finalized'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE)

  end subroutine Finalize

end module info_component
