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

  implicit none
  private

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

    character(len=ESMF_MAXSTR) :: timestring, message
    type(ESMF_Time)   :: currTime
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I8)       :: advanceCount
    
    type(ESMF_Field), dimension(:), allocatable  :: exportFields, importFields
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr  

    call ESMF_ClockGet(parentClock,currTime=currTime, timestep=timeInterval, &
                       advanceCount=advanceCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !! @todo create a netcdf file, check whether it is existing


    write(message,'(A)') trim(timestring)//' netcdf_component initialized.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
 
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: currTime, currentTime, ringTime, time
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I8)   :: advanceCount,  i, j
    integer                 :: itemCount
    type(ESMF_StateItem_Flag), allocatable, dimension(:) :: itemTypeList
    type(ESMF_Field)        :: field
    type(ESMF_Array)        :: array
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_ArrayBundle)  :: arrayBundle
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: itemNameList
       
    character(len=ESMF_MAXSTR) :: message, fileName, name, numString

    call ESMF_ClockGet(parentClock,currTime=currTime, timestep=timeInterval, &
                       advanceCount=advanceCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(timestring)//' netcdf_component running...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      
!  ESMF_StateWrite(state, fileName, rc)
! Currently limited to write out all Arrays of a State object to a netCDF file.
! Future releases will enable more item types of a State to be written to files of various formats.

    call ESMF_StateGet(importState, name=name, itemCount=itemCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
    fileName='netcdf_component.nc'

    write(numstring,'(I10)') itemCount
    write(message,'(A)') 'Found '//trim(numstring)//' items in '//trim(name)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    !call ESMF_StateWrite(importState, fileName, rc=rc)
    !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      if (.not.allocated(itemTypeList)) allocate(itemTypeList(itemCount))
      if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))
      
      call ESMF_StateGet(importState, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      do i=1,itemCount
        
        if (itemTypeList(i) == ESMF_STATEITEM_ARRAY) then
          call ESMF_StateGet(importState, trim(itemNameList(i)), array, rc=rc) 
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

          !call ESMF_ArrayWrite(array, fileName, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        elseif (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(importState, trim(itemNameList(i)), field, rc=rc) 
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

          !call ESMF_FieldWrite(field, fileName, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        else 
          write(message,'(A)') 'Item with name '!//trim(itemNameList(i)!//' not saved to file ' 
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        endif

      enddo

      if (allocated(itemTypeList)) deallocate(itemTypeList)
      if (allocated(itemNameList)) deallocate(itemNameList)
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

    write(message,'(A)') trim(timestring)//' netcdf_component running...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  end subroutine Finalize

end module netcdf_component
