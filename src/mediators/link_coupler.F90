!> @brief Implementation of an ESMF link coupling
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
module link_coupler
    
  use esmf
  use mossco_state

  implicit none

  private

  public SetServices

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


  !> The Initialize() routine of this coupler is empty
  subroutine Initialize(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I4)       :: petCount, localPet
    integer(ESMF_KIND_I4)       :: i, itemCount, count
    character (len=ESMF_MAXSTR) :: timeString, message, name
    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList
    type(ESMF_StateItem_Flag),  dimension(:), allocatable, save :: itemTypeList
    type(ESMF_Field)            :: field

    !! Set default SUCCESS return value and log the call to this 
    !! function into the log
    rc = ESMF_SUCCESS
    
    call ESMF_CplCompGet(cplComp, name=name, petCount=petCount, localPet=localPet, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!#ifdef DEBUG
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)
!#endif

    call ESMF_CplCompGet(cplComp,petCount=petCount,localPet=localPet,name=name, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (.not.allocated(itemTypeList)) allocate(itemTypeList(itemCount))
    if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))

    call ESMF_StateGet(importState, itemTypeList=itemTypeList, &
      itemNameList=itemNameList, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    do i=1, itemCount
      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(importState, trim(itemNameList(i)), field, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
        call ESMF_StateGet(exportState, itemSearch=trim(itemNameList(i)), &
          itemCount=count, rc=rc)
        if (count>0) then
          write(message,'(A)') 'Did not link existing field '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)            
          
          !call ESMF_StateAddReplace(exportState,(/field/), rc=rc)        
        else        
          call ESMF_StateAddReplace(exportState,(/field/), rc=rc)  
        endif      
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)      
        
      else
        write(message,'(A)') 'Did not link non-field item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)            
      endif   
    enddo

    !! Return with logging 
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initialized.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

  end subroutine Initialize

  !> the Run() routine of this coupler copies all fields that are found
  !! in the importState into the exportState.  If the field exists in the
  !! exportState, then it will be replaced. 
  subroutine Run(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I4)       :: petCount, localPet
    integer(ESMF_KIND_I4)       :: i, itemCount, count
    character (len=ESMF_MAXSTR) :: timeString, message, name
    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList
    type(ESMF_StateItem_Flag),  dimension(:), allocatable, save :: itemTypeList
    type(ESMF_Field)            :: field
    type(ESMF_FieldBundle)      :: fieldBundle

    !! Set default SUCCESS return value and log the call to this 
    !! function into the log
    rc = ESMF_SUCCESS
    
    call ESMF_CplCompGet(cplComp, name=name, petCount=petCount, localPet=localPet, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

   call ESMF_CplCompGet(cplComp,petCount=petCount,localPet=localPet,name=name, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (itemCount > 0) then
      if (.not.allocated(itemTypeList)) then
        allocate(itemTypeList(itemCount))
        if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))
      elseif (ubound(itemTypeList,1)<itemCount) then
        deallocate(itemTypeList)
        allocate(itemTypeList(itemCount))
        deallocate(itemNameList)
        allocate(itemNameList(itemCount))
      endif
    
      call ESMF_StateGet(importState, itemTypeList=itemTypeList, &
        itemNameList=itemNameList, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    
    do i=1, itemCount
      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(importState, trim(itemNameList(i)), field, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
        call ESMF_StateGet(exportState, itemSearch=trim(itemNameList(i)), &
          itemCount=count, rc=rc)
        if (count>0) then
          write(message,'(A)') trim(name)//' replaced existing field '//trim(itemNameList(i))
          !write(message,'(A)') trim(name)//' did not link existing field '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)            
          
          call ESMF_StateAddReplace(exportState,(/field/), rc=rc)        
        else        
          call ESMF_StateAdd(exportState,(/field/), rc=rc)  
        endif      
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)      
        
      elseif (itemTypeList(i)==ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(importState, trim(itemNameList(i)), fieldBundle, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
        call ESMF_StateGet(exportState, itemSearch=trim(itemNameList(i)), &
          itemCount=count, rc=rc)
        if (count>0) then
          write(message,'(A)') trim(name)//' replaced existing fieldBundle '//trim(itemNameList(i))
          !write(message,'(A)') trim(name)//' did not link existing field '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)            
          
          call ESMF_StateAddReplace(exportState,(/fieldBundle/), rc=rc)        
        else        
          call ESMF_StateAdd(exportState,(/fieldBundle/), rc=rc)  
        endif      
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)      
       
      else
        write(message,'(A)') trim(name)//' did not link non-field item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)            
      endif   



    enddo
    
    !! Return with logging 
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
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

    type(ESMF_Time)            :: currTime
    character(len=ESMF_MAXSTR) :: message, timeString, name 

    rc = ESMF_SUCCESS

    !! Return with logging 
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' finalized.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    
  end subroutine Finalize

end module link_coupler

