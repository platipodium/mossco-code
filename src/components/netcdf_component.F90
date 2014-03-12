!> @brief Implementation of an ESMF netcdf output component
!>
!> This computer program is part of MOSSCO. 
!> @copyright Copyright 2014, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
module netcdf_component

  use esmf
  use mossco_variable_types
  use mossco_netcdf
  use mossco_strings

  implicit none
  private

  type(type_mossco_netcdf)   :: nc !> @todo should this be an array?

  public :: SetServices

  contains

  !> Provide an ESMF compliant SetServices routine, which defines
  !! entry points for Init/Run/Finalize
  subroutine SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    rc=ESMF_SUCCESS
    
  end subroutine SetServices

  !> Initialize the component
  !!
  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: timestring, message, name
    type(ESMF_Time)            :: currTime, stopTime, startTime
    type(ESMF_TimeInterval)    :: timeInterval, timeStep
    integer(ESMF_KIND_I4)      :: petCount, localPet
    integer(ESMF_KIND_I8)      :: advanceCount
    type(ESMF_Clock)           :: clock

    clock = ESMF_ClockCreate(parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_GridCompSet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
 
    call ESMF_ClockGet(clock,startTime=startTime, currTime=currTime, &
      stopTime=stopTime, advanceCount=advanceCount, timeStep=timeStep, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    !message = trim(nf90_inq_libvers())
    !call ESMF_LogWrite(trim(name)//' uses NetCDF '//trim(message), ESMF_LOGMSG_INFO)

    write(message,'(A)') trim(timestring)//' '//trim(name)//' initialized.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    rc=ESMF_SUCCESS

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
 
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: currTime, currentTime, ringTime, time, refTime
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I8)   :: advanceCount,  i, j
    real(ESMF_KIND_R8)      :: seconds
    integer(ESMF_KIND_I4)   :: itemCount, timeSlice, localPet, fieldCount, ii
    type(ESMF_StateItem_Flag), allocatable, dimension(:) :: itemTypeList
    type(ESMF_Field)        :: field
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    type(ESMF_Array)        :: array
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_ArrayBundle)  :: arrayBundle
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: itemNameList
       
    character(len=ESMF_MAXSTR) :: message, fileName, name, numString, timeUnit
    type(ESMF_FileStatus_Flag) :: fileStatus=ESMF_FILESTATUS_REPLACE
    type(ESMF_IOFmt_Flag)      :: ioFmt

    call ESMF_GridCompGet(gridComp, localPet=localPET, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! This output routine only works on PET0
    if (localPET>0) return

    call ESMF_ClockGet(parentClock,currTime=currTime, timestep=timeInterval, &
                       advanceCount=advanceCount, refTime=refTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(timestring)//' netcdf_component running...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_AttributeGet(importState, name='filename', value=fileName, &
      defaultValue='netcdf_component.nc', rc=rc)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !write(numstring,'(I3)') itemCount
    !write(message,'(A)') 'Found '//trim(numstring)//' items in '//trim(name)
    !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    if (advanceCount<huge(timeSlice)) then
      timeSlice=int(advanceCount, ESMF_KIND_I4)
    else
      write(message,'(A)') 'Cannot use this advanceCount for a netcdf timeSlice, failed to convert long int to int'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    endif

    if (itemcount>0) then
      if (.not.allocated(itemTypeList)) allocate(itemTypeList(itemCount))
      if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))
      
      call ESMF_StateGet(importState, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_TimeGet(refTime, timeStringISOFrac=timeString, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write(timeUnit,'(A)') 'seconds since '//timeString(1:10)//' '//timestring(12:len_trim(timestring))

      call ESMF_TimeIntervalGet(currTime-refTime, s_r8=seconds, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      nc = mossco_netcdfOpen(fileName, timeUnit=timeUnit, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call nc%add_timestep(seconds, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
      do i=1,itemCount
        
        if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(importState, trim(itemNameList(i)), field, rc=rc) 
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

          write(*,*) i, itemCount, trim(itemNameList(i))
          
          call nc%put_variable(field)
          !if (.not.nc%variable_present(trim(itemNameList(i))))  then 
          !  call nc%create_variable(field)
          !endif

        else 
          write(message,'(A)') 'Item with name '//trim(itemNameList(i))//' not saved to file ' 
        endif
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      enddo

      if (allocated(itemTypeList)) deallocate(itemTypeList)
      if (allocated(itemNameList)) deallocate(itemNameList)

      call nc%close()
    endif 
    
    write(message,'(A)') trim(timestring)//' netcdf_component finished running.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_Time)      :: currTime
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I8)   :: advanceCount
    character(len=ESMF_MAXSTR) :: message, timeString


    call ESMF_ClockGet(parentClock,currTime=currTime, timestep=timeInterval, &
                       advanceCount=advanceCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(timestring)//' netcdf_component finalized'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  end subroutine Finalize

!  subroutine MOSSCO_Reallocate(variable,dimensions, keepLarge)
!    integer, intent(inout), allocatable :: variable(:)
!    integer :: dimensions
!    logical, optional :: keepLarge
!
!    if (.not.present(keepLarge)) keepLarge=.false.
!
!    if (.not.allocated(variable)) then
!      allocate(variable(dimensions))
!    else
!      if (.not.((size(variable)>=dimensions).and.keepLarge)) then
!        deallocate(variable)
!        allocate(variable(dimensions))
!      endif
!    endif
!
!   end subroutine MOSSCO_Reallocate 

end module netcdf_component
