!> @brief Implementation of an ESMF coupler that redistributes between PETs 
!>
!> This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2014, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, <carsten.lemmen@hereon.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
module redist_coupler
    
  use esmf

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

    integer(ESMF_KIND_I4)       :: petCount, localPet, dimCount, localDeCount
    integer(ESMF_KIND_I4)       :: rank
    integer(ESMF_KIND_I4), allocatable :: totalUBound(:), totalLBound(:)
    integer(ESMF_KIND_I4)       :: i, itemCount, count, j, k
    character (len=ESMF_MAXSTR) :: timeString, message, name
    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList
    type(ESMF_StateItem_Flag),  dimension(:), allocatable, save :: itemTypeList
    type(ESMF_Field)            :: srcField, dstField
    type(ESMF_RouteHandle)      :: routeHandle
    
    

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
        call ESMF_StateGet(importState, trim(itemNameList(i)), srcField, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
        call ESMF_StateGet(exportState, itemSearch=trim(itemNameList(i)), &
          itemCount=count, rc=rc)
        if (count>0) then
          write(message,'(A)') 'Redist existing field '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          
          call ESMF_FieldGet(srcField, localDeCount=localDeCount, rank=rank, rc=rc)
          if (allocated(totalLBound)) deallocate(totalLBound)
          if (allocated(totalUBound)) deallocate(totalUbound)
          allocate(totalLBound(rank),totalUBound(rank))
          do k=0,localDeCount-1
            call ESMF_FieldGetBounds(srcField, localDe=k, totalLBound=totalLBound, totalUBound=totalUbound, rc=rc)
            write(message,'(A,I3,A,I3)') 'Source field DE=',k,' lbounds (',totalLBound(1)
            do j=2,rank
              write(message,'(A,I3)') trim(message)//',', totalLBound(j)     
            enddo
            write(message,'(A,I3)') trim(message)//') ubounds (', totalUBound(1)     
            do j=2,rank
              write(message,'(A,I3)') trim(message)//',', totalUBound(j)     
            enddo
            write(message,'(A)') trim(message)//')'     
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          enddo          
          deallocate(totalLBound,totalUBound)
            
                      
          call ESMF_StateGet(exportState, trim(itemNameList(i)), field=dstField, &
            rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

          call ESMF_FieldGet(dstField, localDeCount=localDeCount, rank=rank, rc=rc)
          if (allocated(totalLBound)) deallocate(totalLBound)
          if (allocated(totalUBound)) deallocate(totalUbound)
          allocate(totalLBound(rank),totalUBound(rank))
          do k=0,localDeCount-1
            call ESMF_FieldGetBounds(dstField, localDe=k, totalLBound=totalLBound, totalUBound=totalUbound, rc=rc)
            write(message,'(A,I3,A,I3)') 'Source field DE=',k,' lbounds (',totalLBound(1)
            do j=2,rank
              write(message,'(A,I3)') trim(message)//',', totalLBound(j)     
            enddo
            write(message,'(A,I3)') trim(message)//') ubounds (', totalUBound(1)     
            do j=2,rank
              write(message,'(A,I3)') trim(message)//',', totalUBound(j)     
            enddo
            write(message,'(A)') trim(message)//')'     
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          enddo          
          deallocate(totalLBound,totalUBound)



!#if 0
          call ESMF_FieldRedistStore(srcField, dstField, routeHandle=routeHandle, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_FieldRedist(srcField,dstField, routeHandle, rc=rc)                  
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_FieldRedistRelease(routeHandle, rc=rc)                  
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!#endif
 
        else        
          write(message,'(A)') 'Did not redist non-matching field '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)            
        endif      
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)      
        
      else
        write(message,'(A)') 'Did not redist non-field item '//trim(itemNameList(i))
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
    integer(ESMF_KIND_I4)       :: i, itemCount, count, otherCount
    character (len=ESMF_MAXSTR) :: timeString, message, name
    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList
    type(ESMF_StateItem_Flag),  dimension(:), allocatable, save :: itemTypeList
    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_Field)            :: field, otherField
    type(ESMF_FieldBundle)      :: fieldBundle

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
#if 0
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
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
        if (count>0) then
          call ESMF_StateGet(exportState, itemName=trim(itemNameList(i)), &
          itemType=itemType, rc=rc)
          if (itemType == itemTypeList(i)) then
            call ESMF_StateGet(exportState, trim(itemNameList(i)), otherField, rc=rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

            if (otherField /= field) then
              write(message,'(A)') trim(name)//' replaced existing field '//trim(itemNameList(i))
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)     
              call ESMF_StateAddReplace(exportState,(/field/), rc=rc)        
       
            else
              write(message,'(A)') trim(name)//' skipped existing field '//trim(itemNameList(i))
              !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)     
            endif            
          endif          
        else        
          write(message,'(A)') trim(name)//' added field '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)            
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
          !write(message,'(A)') trim(name)//' did not redist existing field '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)            
          
          call ESMF_StateAddReplace(exportState,(/fieldBundle/), rc=rc)        
        else        
          call ESMF_StateAdd(exportState,(/fieldBundle/), rc=rc)  
        endif      
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)      
       
      else
        write(message,'(A)') trim(name)//' did not redist non-field item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)            
      endif   



    enddo
#endif 
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
    call ESMF_CplCompGet(cplComp,petCount=petCount,localPet=localPet,name=name, &
      clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (.not.clockIsPresent) then
      clock=parentClock
    else 
      call ESMF_CplCompGet(cplComp, clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    
    !> Get the time and log it
    call ESMF_ClockGet(clock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' finalizing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)
   
    if (clockIsPresent) call ESMF_ClockDestroy(clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finalized'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE)

  end subroutine Finalize

end module redist_coupler

