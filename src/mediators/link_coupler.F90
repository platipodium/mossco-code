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

  subroutine SetServices(cplComp, rc)

    type(ESMF_CplComp)  :: cplComp
    integer, intent(out) :: rc

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=rc)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=rc)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  subroutine InitializeP0(cplComp, importState, exportState, parentClock, rc)
  
    type(ESMF_cplComp)    :: cplComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=rc)

    call ESMF_CplCompGet(cplComp, name=name, rc=rc)
    write(message,'(A)') trim(name)//' initialized phase 0'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

  end subroutine InitializeP0

  !> The Initialize() routine of this coupler is empty
  subroutine InitializeP1(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I4)       :: petCount, localPet
    character (len=ESMF_MAXSTR) :: timeString, message, name
    type(ESMF_Time)             :: currTime

    call ESMF_CplCompGet(cplComp, name=name, petCount=petCount, localPet=localPet, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    call ESMF_CplCompGet(cplComp,petCount=petCount,localPet=localPet,name=name, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call  link_fields_and_fieldbundles_in_states(importState, exportState, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)    

    !! Return with logging 
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initialized.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

  end subroutine InitializeP1

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
    character (len=ESMF_MAXSTR) :: timeString, message, name
    type(ESMF_Time)             :: currTime

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

    call  link_fields_and_fieldbundles_in_states(importState, exportState, rc)
    
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

    call MOSSCO_CplCompEntry(cplComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    if (clockIsPresent) call ESMF_ClockDestroy(clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finalized'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE)

  end subroutine Finalize


  subroutine  link_fields_and_fieldbundles_in_states(importState, exportState, rc)

    type(ESMF_State), intent(in)    :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out)            :: rc   
    
    integer(ESMF_KIND_I4)       :: i, itemCount, exportItemCount
    character (len=ESMF_MAXSTR) :: message
    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList
    type(ESMF_StateItem_Flag),  dimension(:), allocatable, save :: itemTypeList
    type(ESMF_Field)            :: importField, exportField
    type(ESMF_FieldBundle)      :: importFieldBundle, exportFieldBundle
    type(ESMF_StateItem_Flag)   :: itemType
  
    call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Allocate/reallocate list do hold item information
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
    
    !! Loop over items 
    do i=1, itemCount
      
      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(importState, trim(itemNameList(i)), importField, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
        call ESMF_StateGet(exportState, itemSearch=trim(itemNameList(i)), &
          itemCount=exportItemCount, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
        if (exportItemCount>0) then
          call ESMF_StateGet(exportState, itemName=trim(itemNameList(i)), &
          itemType=itemType, rc=rc)
          if (itemType == itemTypeList(i)) then
            call ESMF_StateGet(exportState, trim(itemNameList(i)), exportField, rc=rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

            if (exportField /= importField) then
              write(message,'(A)') 'Replaced existing field '//trim(itemNameList(i))
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)     
              call ESMF_StateAddReplace(exportState,(/importField/), rc=rc)        
       
            else
              write(message,'(A)') 'Skipped existing field '//trim(itemNameList(i))
              !! call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)     
            endif            
          endif          
        else        
          write(message,'(A)') 'Added field '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)            
          call ESMF_StateAdd(exportState,(/importField/), rc=rc)  
        endif      
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)      
        
      elseif (itemTypeList(i)==ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(importState, trim(itemNameList(i)), importFieldBundle, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
        call ESMF_StateGet(exportState, itemSearch=trim(itemNameList(i)), &
          itemCount=exportItemCount, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          
        if (exportItemCount==0) then
          write(message,'(A)') 'Added fieldbundle '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)            
          call ESMF_StateAdd(exportState,(/importFieldBundle/), rc=rc)  
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)      
        else
          call ESMF_StateGet(exportState, itemName=trim(itemNameList(i)), &
          itemType=itemType, rc=rc)
          if (itemType == itemTypeList(i)) then
            call ESMF_StateGet(exportState, trim(itemNameList(i)), exportFieldBundle, rc=rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

            if (exportFieldBundle /= importFieldBundle) then
              write(message,'(A)') 'Replaced existing fieldbundle '//trim(itemNameList(i))
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)     
              call ESMF_StateAddReplace(exportState,(/importFieldBundle/), rc=rc)               
            else
              write(message,'(A)') 'Skipped existing fieldbundle '//trim(itemNameList(i))
              !! call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)     
            endif            
          endif          
          if (exportItemCount>1) then
            write(message,'(A)') 'Found multiple fieldbundles with name '//trim(itemNameList(i))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)     
              call ESMF_StateAddReplace(exportState,(/importFieldBundle/), rc=rc)     
          endif                      
        endif       
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)      
       
      else
        write(message,'(A)') 'Did not link non-field item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)            
      endif   
    enddo
  end subroutine link_fields_and_fieldbundles_in_states

  subroutine MOSSCO_CplCompEntry(cplComp, parentClock, name, currTime, rc)
  
    type(ESMF_CplComp), intent(inout)    :: cplComp
    type(ESMF_Clock), intent(in)         :: parentClock
    character(ESMF_MAXSTR), intent(out)  :: name
    type(ESMF_Time), intent(out)         :: currTime
    integer, intent(out)                 :: rc

    integer(ESMF_KIND_I4)   :: petCount, localPet, phase
    character(ESMF_MAXSTR)  :: message, timeString
    logical                 :: clockIsPresent, configIsPresent, vmIsPresent
    type(ESMF_Clock)        :: clock
    type(ESMF_Vm)           :: vm
    type(ESMF_Method_Flag)  :: method
    type(ESMF_Context_Flag) :: context
    type(ESMF_Config)       :: config
    
    call ESMF_CplCompGet(cplComp, name=name, clockIsPresent=clockIsPresent, &
      configIsPresent=configIsPresent, vmIsPresent=vmIsPresent, localPet=localPet, &
      petCount=petCount, currentMethod=method, currentPhase=phase, contextFlag=context, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Check for clock presence and add if necessary
    if (clockIsPresent) then
      call ESMF_CplCompGet(cplComp, clock=clock, rc=rc)
    else
      clock = ESMF_ClockCreate(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_CplCompSet(cplComp, clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock, name=trim(name)//' clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Check for config presence
    if (configIsPresent) then
      call ESMF_CplCompGet(cplcomp, config=config, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      !!> @todo: what todo with this information?
    endif
    
    !! Check for vm presence
    if (vmIsPresent) then
      call ESMF_CplCompGet(cplComp, vm=vm, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_VmGet(vm, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      !!> @todo: what todo with this information?
    endif

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)
    
    if (method == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' running'
    elseif (method == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initializing'
    elseif (method == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalizing'
    endif

		write(message,'(A,I1,A)') trim(message)//' phase ',phase,' ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)  

  end subroutine MOSSCO_CplCompEntry

  subroutine MOSSCO_CplCompExit(cplComp, rc)
  
    type(ESMF_CplComp), intent(inout)    :: cplComp
    integer, intent(out)                 :: rc

    integer(ESMF_KIND_I4)   :: phase
    character(ESMF_MAXSTR)  :: message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Clock)        :: clock
    type(ESMF_Method_Flag)  :: method
    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    
    call ESMF_CplCompGet(cplComp, name=name, clockIsPresent=clockIsPresent, &
      currentMethod=method, currentPhase=phase, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Check for clock presence
    if (clockIsPresent) then
      call ESMF_CplCompGet(cplComp, clock=clock, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write(message,'(A)') trim(timestring)
    endif
    
    write(message,'(A)') trim(message)//' '//trim(name)
    
    if (method == ESMF_METHOD_RUN) then
      write(message,'(A)') trim(message)//' ran'
    elseif (method == ESMF_METHOD_INITIALIZE) then
      write(message,'(A)') trim(message)//' initialized'
    elseif (method == ESMF_METHOD_FINALIZE) then
      write(message,'(A)') trim(message)//' finalized'
    endif

		write(message,'(A,I1)') trim(message)//' phase ',phase
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)  
  
  end subroutine MOSSCO_CplCompExit


end module link_coupler

