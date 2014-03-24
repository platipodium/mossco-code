!> @brief Implementation of an ESMF coupling between different grids
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
module grid_coupler
    
  use esmf
  use mossco_state

  implicit none

  private

  public SetServices

  type type_mossco_fields_handle
    type(ESMF_RouteHandle) :: routehandle
    type(ESMF_Field) :: srcField, dstField
    type(ESMF_State) :: srcState, dstState
    type(type_mossco_fields_handle), pointer :: next=>null()
  end type

  class(type_mossco_fields_handle), allocatable, target :: fieldsHandle

  contains

  subroutine SetServices(cplcomp, rc)

    type(ESMF_CplComp)   :: cplcomp
    integer, intent(out) :: rc

    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, Initialize  &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_RUN,    Run   &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_FINALIZE, Finalize &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine SetServices

  subroutine Initialize(cplcomp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentclock
    integer, intent(out) :: rc

    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: petCount, localPet, i, itemCount
    character(len=ESMF_MAXSTR)  :: message, name, timeString, exportName, importName
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMf_StateItem_Flag)   :: itemType
    type(ESMF_RouteHandle)      :: routeHandle
    class(type_mossco_fields_handle), pointer :: currHandle=>null() 
    type(ESMF_Field)            :: importField, exportField

    call ESMF_CplCompGet(cplComp, name=name, petCount=petCount, localPet=localPet, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    !> Search for all fields that are present in both import and export state, 
    !! for each combination of fields
    !! - if they are defined on different grids, create a route handle and
    !!   name it with the name of the two grids for identification (todo)

    call ESMF_StateGet(exportState, name=exportName, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateGet(importState, itemCount=itemCount, name=importName, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      allocate(itemNameList(itemCount))
      allocate(itemTypeList(itemCount))
      
      do i=1,itemCount
        if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) then 
          write(message,'(A)') trim(name)//' skipped non-field item '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif
     
        call ESMF_StateGet(exportState, itemName=itemNameList(i), itemType=itemType, rc=rc)   
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
        if (itemType==ESMF_STATEITEM_NOTFOUND)   then
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (not in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        elseif (itemType/=ESMF_STATEITEM_FIELD) then
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (not a field in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif
        
        call ESMF_StateGet(importState, itemNameList(i), importField, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        call ESMF_StateGet(exportState, itemNameList(i), exportField, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
        if (importField==exportField) then
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (already the same in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif

        call ESMF_FieldRegridStore(srcField=importField, dstField=exportField,&
          routeHandle=routehandle,regridmethod=ESMF_REGRIDMETHOD_BILINEAR,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        !! ESMF_FieldRegrid.F90:2018 ESMF_FieldRegridGetIwts Invalid argument 
        !! - - can't currently regrid a grid       that contains a DE of width less than 2

        if (.not.associated(currHandle)) then
          allocate(fieldsHandle)
          currHandle=>fieldsHandle
        endif
        
        do while(associated(currHandle%next)) 
          currHandle=>currHandle%next
        enddo
        allocate (currHandle%next)
        currHandle=>currHandle%next

        currHandle%srcField=importField
        currHandle%dstField=exportField
        currHandle%srcState=importState
        currHandle%dstState=exportState
        currHandle%routeHandle=routeHandle

      enddo
      
    else
      write(message,'(A)') trim(name)//' no couplable fields in '//trim(importName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
    endif  
            
    if (allocated(itemNameList)) deallocate(itemNameList)
    if (allocated(itemTypeList)) deallocate(itemTypeList)
    
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initialized.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

  end subroutine Initialize


  subroutine Run(cplcomp, importState, exportState, parentclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentclock
    integer, intent(out) :: rc

    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: petCount, localPet, i, itemCount
    character(len=ESMF_MAXSTR)  :: message, name, timeString, exportName, importName
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMf_StateItem_Flag)   :: itemType
    class(type_mossco_fields_handle), pointer :: currHandle=>null() 
    type(ESMF_Field)            :: importField, exportField
    type(ESMF_RouteHandle)      :: routeHandle

    call ESMF_CplCompGet(cplComp, name=name, petCount=petCount, localPet=localPet, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    call ESMF_StateGet(exportState, name=exportName, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateGet(importState, itemCount=itemCount, name=importName, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      allocate(itemNameList(itemCount))
      allocate(itemTypeList(itemCount))
      
      do i=1,itemCount
        if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) then 
          write(message,'(A)') trim(name)//' skipped non-field item '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif
     
        call ESMF_StateGet(exportState, itemName=itemNameList(i), itemType=itemType, rc=rc)   
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
        if (itemType==ESMF_STATEITEM_NOTFOUND) then 
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (not in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        elseif (itemType/=ESMF_STATEITEM_FIELD) then
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (not a field in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif
        
        call ESMF_StateGet(importState, itemNameList(i), importField, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        call ESMF_StateGet(exportState, itemNameList(i), exportField, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
        if (importField==exportField) then
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (already the same in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif

        !! search for the correct routeHandle
        currHandle=>fieldsHandle
        do while (associated(currHandle%next))
          if (.not.((currHandle%srcField==importField).and.(currHandle%dstField==exportField))) &
            currHandle=>currHandle%next
        enddo
        routeHandle=currHandle%routeHandle
        
        call ESMF_FieldRegrid(srcField=importField, dstField=exportField,&
          routeHandle=routehandle, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        !! ESMF_FieldRegrid.F90:2018 ESMF_FieldRegridGetIwts Invalid argument 
        !! - - can't currently regrid a grid       that contains a DE of width less than 2

      enddo
      
    else
      write(message,'(A)') trim(name)//' no couplable fields in '//trim(importName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
    endif  

    write(message,'(A)') trim(timestring)//' '//trim(name)//' finished running.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

  end subroutine Run

  !> The Finalize() routine of this coupler is empty
  subroutine Finalize(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR)  :: message, name, timeString
    class(type_mossco_fields_handle), pointer :: currHandle=>null() 

    rc = ESMF_SUCCESS

    if (allocated(fieldsHandle)) then
      currHandle=>fieldsHandle
      do while (associated(currHandle%next))
        currHandle=>currHandle%next
        deallocate(fieldsHandle)
      enddo
    endif
    
    !! Return with logging 
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' finalized.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)
    
  end subroutine Finalize

end module grid_coupler
