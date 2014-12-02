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

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "link_coupler.F90"

module link_coupler
    
  use esmf
  use mossco_state
  use mossco_component

  implicit none

  private

  public SetServices

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(cplComp, rc)

    implicit none

    type(ESMF_CplComp)   :: cplComp
    integer, intent(out) :: rc

    integer              :: localrc
    
    rc = ESMF_SUCCESS

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(cplComp, importState, exportState, parentClock, rc)
  
    implicit none
  
    type(ESMF_cplComp)    :: cplComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer              :: localrc
    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)       :: currTime

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer              :: localrc
    character (len=ESMF_MAXSTR) :: name
    type(ESMF_Time)             :: currTime

    rc = ESMF_SUCCESS

    call MOSSCO_CplCompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   
    call  link_fields_and_fieldbundles_in_states(importState, exportState, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CplCompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

  !> the Run() routine of this coupler copies all fields that are found
  !! in the importState into the exportState.  If the field exists in the
  !! exportState, then it will be replaced. 

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
subroutine Run(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer              :: localrc
    character (len=ESMF_MAXSTR) :: name
    type(ESMF_Time)             :: currTime
    type(ESMF_Clock)            :: clock

    rc = ESMF_SUCCESS

    call MOSSCO_CplCompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   
    call link_fields_and_fieldbundles_in_states(importState, exportState, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !!> @todo the following call creates a problem:
    !!call ESMF_ClockAdvance(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    
    call MOSSCO_CplCompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
 subroutine Finalize(cplComp, importState, exportState, parentClock, rc)
    
    type(ESMF_CplComp)    :: cplComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer              :: localrc
    character (len=ESMF_MAXSTR) :: name
    type(ESMF_Time)             :: currTime
    type(ESMF_Clock)            :: clock

    rc = ESMF_SUCCESS

    call MOSSCO_CplCompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   
    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CplCompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "link_fields_and_fieldbundles_in_states"

  subroutine  link_fields_and_fieldbundles_in_states(importState, exportState, rc)

    type(ESMF_State), intent(in)    :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out)            :: rc   
    
    integer              :: localrc
    integer(ESMF_KIND_I4)       :: i, itemCount, exportItemCount
    character (len=ESMF_MAXSTR) :: message, creatorName
    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList
    type(ESMF_StateItem_Flag),  dimension(:), allocatable, save :: itemTypeList
    type(ESMF_Field)            :: importField, exportField
    type(ESMF_FieldBundle)      :: importFieldBundle, exportFieldBundle
    type(ESMF_StateItem_Flag)   :: itemType
    logical                     :: isPresent

    rc = ESMF_SUCCESS
  
    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
        itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
    
    !! Loop over items 
    do i=1, itemCount
      
      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(importState, trim(itemNameList(i)), importField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      
        call ESMF_StateGet(exportState, itemSearch=trim(itemNameList(i)), &
          itemCount=exportItemCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        
        if (exportItemCount>0) then
          call ESMF_StateGet(exportState, itemName=trim(itemNameList(i)), &
          itemType=itemType, rc=localrc)
          if (itemType == itemTypeList(i)) then
            call ESMF_StateGet(exportState, trim(itemNameList(i)), exportField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            if (exportField /= importField) then
              write(message,'(A)') '    replaced existing field '//trim(itemNameList(i))
              call ESMF_AttributeGet(importField, 'creator', value=creatorName, defaultvalue='none', isPresent=isPresent, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
              if (isPresent) write(message,'(A)') trim(message)//' ['//trim(creatorName)//']'
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)     
              call ESMF_StateAddReplace(exportState,(/importField/), rc=localrc)        
       
            else
              write(message,'(A)') '    skipped existing field '//trim(itemNameList(i))
              !! call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)     
            endif            
          endif          
        else     
          write(message,'(A)') '    added field '
          call MOSSCO_FieldString(importField,message)  
          !call ESMF_AttributeGet(importField, 'creator', value=creatorName, defaultvalue='none', isPresent=isPresent, rc=localrc)
          !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          !if (isPresent) write(message,'(A)') trim(message)//' ['//trim(creatorName)//']'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)            
          call ESMF_StateAdd(exportState,(/importField/), rc=localrc)  
        endif      
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)      
        
      elseif (itemTypeList(i)==ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(importState, trim(itemNameList(i)), importFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      
        call ESMF_StateGet(exportState, itemSearch=trim(itemNameList(i)), &
          itemCount=exportItemCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          
        if (exportItemCount==0) then
          write(message,'(A)') '    added fieldbundle '//trim(itemNameList(i))
          call ESMF_AttributeGet(importFieldBundle, 'creator', value=creatorName, defaultvalue='none', isPresent=isPresent, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (isPresent) write(message,'(A)') trim(message)//' ['//trim(creatorName)//']'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)            
          call ESMF_StateAdd(exportState,(/importFieldBundle/), rc=localrc)  
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)  
        else
          call ESMF_StateGet(exportState, itemName=trim(itemNameList(i)), &
          itemType=itemType, rc=localrc)
          if (itemType == itemTypeList(i)) then
            call ESMF_StateGet(exportState, trim(itemNameList(i)), exportFieldBundle, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            if (exportFieldBundle /= importFieldBundle) then
              write(message,'(A)') '    replaced existing fieldbundle '//trim(itemNameList(i))
              call ESMF_AttributeGet(importFieldBundle, 'creator', value=creatorName, defaultvalue='none', isPresent=isPresent, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
              if (isPresent) write(message,'(A)') trim(message)//' ['//trim(creatorName)//']'
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)     
              call ESMF_StateAddReplace(exportState,(/importFieldBundle/), rc=localrc)               
            else
              write(message,'(A)') '    skipped existing fieldbundle '//trim(itemNameList(i))
              !! call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)     
            endif            
          endif          
          if (exportItemCount>1) then
            write(message,'(A)') '    found multiple fieldbundles with name '//trim(itemNameList(i))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)     
              call ESMF_StateAddReplace(exportState,(/importFieldBundle/), rc=localrc)     
          endif                      
        endif       
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)    
       
      else
        write(message,'(A)') '    did not link non-field item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)            
      endif   
    enddo
  end subroutine link_fields_and_fieldbundles_in_states

end module link_coupler

