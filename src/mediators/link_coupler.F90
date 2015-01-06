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

    call link_empty_and_111_fields_and_fieldbundles_in_states(importState, exportState, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call link_empty_and_111_fields_and_fieldbundles_in_states(exportState, importState, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call link_foreign_grid_or_needed_field_in_states(importState, exportState, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call link_foreign_grid_or_needed_field_in_states(exportState, importState, rc)
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
    type(ESMF_Time)             :: currTime, stopTime
    type(ESMF_Clock)            :: clock

    rc = ESMF_SUCCESS

    call MOSSCO_CplCompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call link_fields_and_fieldbundles_in_states(importState, exportState, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (stopTime>currTime) call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
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

#undef  ESMF_METHOD
#define ESMF_METHOD "link_empty_and_111_fields_and_fieldbundles_in_states"
  subroutine  link_empty_and_111_fields_and_fieldbundles_in_states(importState, exportState, rc)

    type(ESMF_State), intent(in)    :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out)            :: rc

    integer              :: localrc
    integer(ESMF_KIND_I4)       :: i, j, itemCount, exportItemCount, importItemCount, fieldCount
    integer(ESMF_KIND_I4)       :: rank
    integer(ESMF_KIND_I4),allocatable :: totalcount(:)
    character (len=ESMF_MAXSTR) :: message, creatorName, name
    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList, fieldNameList
    type(ESMF_StateItem_Flag),  dimension(:), allocatable, save :: itemTypeList
    type(ESMF_Field),  allocatable :: fieldList(:)
    type(ESMF_Field)            :: importField, exportField
    type(ESMF_FieldBundle)      :: importFieldBundle, exportFieldBundle
    type(ESMF_StateItem_Flag)   :: itemType
    logical                     :: isPresent
    type(ESMF_FieldStatus_Flag) :: fieldStatus, exportFieldStatus

    rc = ESMF_SUCCESS
    name='link_coupler'

    call ESMF_StateGet(exportState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Allocate/reallocate list to hold item information
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

      call ESMF_StateGet(exportState, itemTypeList=itemTypeList, &
        itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    !! Loop over items
    do i=1, itemCount

      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(exportState, trim(itemNameList(i)), exportField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGet(exportField, status=exportFieldStatus, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !! don't deal with gridset fields here
        if (exportFieldStatus == ESMF_FIELDSTATUS_GRIDSET) cycle

        !! only deal with 1x1x1 COMPLETE fields or empty fields
        !! if complete and 1x1x1 then set values in export state
        if (exportFieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
          call ESMF_FieldGet(exportField, rank=rank, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (.not.allocated(totalCount)) allocate(totalCount(rank))
          call ESMF_FieldGetBounds(exportField, totalCount=totalCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          !! don't deal with fields that have all degenerate dimensions
          if (any(totalCount>1)) then
            if (allocated(totalCount)) deallocate(totalCount)
            cycle
          endif
          if (allocated(totalCount)) deallocate(totalCount)
        endif

        call ESMF_StateGet(importState, itemSearch=trim(itemNameList(i)), &
          itemCount=importItemCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      	if (importItemCount<=0) cycle

        call ESMF_StateGet(importState, itemName=trim(itemNameList(i)), &
          itemType=itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemType == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(importState, trim(itemNameList(i)), importField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call ESMF_FieldGet(importField, status=fieldStatus, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

						if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
              write(message,'(A)') trim(name)//' did not replace empty field with empty field '//trim(itemNameList(i))
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
              cycle
            endif

            !! Introduce here ifelse COPMLETE
            if (exportFieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
              call copy_1x1x1_field_to_field(exportField, importField, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

              write(message,'(A)') trim(name)//' replaced 1x1x1 field with field '
            else
              write(message,'(A)') trim(name)//' replaced empty field with field '
            endif

            call ESMF_StateAddReplace(exportState, (/importField/), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call MOSSCO_FieldString(importField, message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        elseif (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateRemove(exportState,(/trim(itemNameList(i))/), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            call ESMF_StateGet(importState, trim(itemNameList(i)), importFieldBundle, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
 			 	    call ESMF_StateAddReplace(exportState, (/importFieldBundle/), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            write(message,'(A)') trim(name)//' replaced empty field with fieldBundle '
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        else
          write(message,'(A)') trim(name)//' cannot replace this empty field.'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

      elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(exportState, trim(itemNameList(i)), exportFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateGet(importState, itemSearch=trim(itemNameList(i)), &
          itemCount=exportItemCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (exportItemCount==0) cycle

        call ESMF_StateGet(importState, itemName=trim(itemNameList(i)), &
          itemType=itemType, rc=localrc)

        if (itemType == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(importState, trim(itemNameList(i)), importField, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldBundleAdd(exportFieldBundle, (/importField/), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
	      elseif (itemType /= ESMF_STATEITEM_FIELDBUNDLE) then
	        cycle
	      endif

        call ESMF_StateGet(importState, trim(itemNameList(i)), importFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

	      call ESMF_FieldBundleGet(exportFieldBundle, fieldCount=fieldCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

				if (fieldCount==0) then
				  call ESMF_StateAddReplace(exportState, (/importFieldBundle/), rc=localrc)
          call ESMF_FieldBundleDestroy(exportFieldBundle, rc=localrc)
				  cycle
				endif

				allocate(fieldList(fieldCount), fieldNameList(fieldCount))
				call ESMF_FieldBundleGet(exportFieldBundle, fieldList=fieldList, fieldNameList=fieldNameList, rc=localrc)

				do j=1, fieldCount
				  call ESMF_FieldGet(fieldList(j), status=fieldStatus, rc=localrc)
				  if (fieldStatus /= ESMF_FIELDSTATUS_EMPTY) cycle

				  call ESMF_FieldBundleGet(importFieldBundle, fieldNameList(j), isPresent=isPresent, rc=localrc)
				  if (isPresent) then
  				  call ESMF_FieldBundleGet(importFieldBundle, fieldNameList(j), field=importfield, rc=localrc)
				    call ESMF_FieldBundleAddReplace(exportFieldBundle, (/importField/), rc=localrc)
	          cycle
	        endif

	        call ESMF_StateGet(importState, fieldNameList(j), itemType=itemType, rc=localrc)
	        if (itemType == ESMF_STATEITEM_FIELD) then
	          call ESMF_StateGet(importState, trim(fieldNameList(j)), importField, rc=localrc)
				    call ESMF_FieldBundleAddReplace(exportFieldBundle, (/importField/), rc=localrc)
	        endif
				enddo
				deallocate(fieldList, fieldNameList)
      endif
    enddo
  end subroutine link_empty_and_111_fields_and_fieldbundles_in_states

#undef  ESMF_METHOD
#define ESMF_METHOD "link_foreign_grid_field_in_states"
  subroutine  link_foreign_grid_or_needed_field_in_states(importState, exportState, rc)

    type(ESMF_State), intent(in)    :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out)            :: rc

    integer              :: localrc
    integer(ESMF_KIND_I4)       :: i, j, itemCount, exportItemCount, importItemCount, fieldCount, count, len
    character (len=ESMF_MAXSTR) :: message, creatorName, name, fieldName, attributeName, stateName
    type(ESMF_Time)             :: currTime
    type(ESMF_Field)            :: importField, exportField
    type(ESMF_FieldBundle)      :: importFieldBundle, exportFieldBundle
    type(ESMF_StateItem_Flag)   :: itemType
    logical                     :: isPresent, isNeeded
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_TypeKind_Flag)    :: typekind

    rc = ESMF_SUCCESS
    name='link_coupler'

    call ESMF_AttributeGet(exportState, count=count, attcountflag=ESMF_ATTGETCOUNT_ATTRIBUTE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, count

      call ESMF_AttributeGet(exportState, attributeIndex=i, name=attributeName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Our interest is the foreign_grid_field_name attribute as character
      if (trim(attributeName) == 'foreign_grid_field_name') then
        call ESMF_AttributeGet(exportState, trim(attributeName), value=fieldName, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        !! Otherwise look at the name and see whether it ends in ':needed', then expect a logical value
        len=len_trim(attributeName)
        if (.not.attributeName(len-6:len) == ':needed') cycle

        fieldName = trim(attributeName(1:len-7))

        call ESMF_AttributeGet(exportState, trim(attributeName), value=isNeeded, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (.not.isNeeded) cycle
      endif

      !call ESMF_LogWrite('Looking for '//trim(attributeName)//' '//trim(fieldName), ESMF_LOGMSG_INFO)

      ! Check whether it is already there
      call ESMF_StateGet(exportState, itemSearch=trim(fieldName), itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemCount>0) then
        ! If it does exist, check for GRIDSET status and return silently, otherwise continue

        call ESMF_StateGet(exportState, trim(fieldName), itemType=itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemType==ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(exportState, trim(fieldName), exportField, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldGet(exportField, status=fieldStatus, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !! If this contains grid information, then return silently
          if (.not. (fieldStatus == ESMF_FIELDSTATUS_EMPTY)) cycle

        elseif (itemType==ESMF_STATEITEM_FIELDBUNDLE) then
          call ESMF_StateGet(exportState, trim(fieldName), exportFieldBundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

 	        call ESMF_FieldBundleGet(exportFieldBundle, fieldCount=fieldCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !! If this bundle contains fields, then return with warning that this case is not fully checked
				  if (fieldCount>0) then
            write(message,'(A)') trim(name)//' requested fieldbundle '//trim(fieldname)//' exists (not fully checked)'
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
            cycle
          endif
				endif
      endif

      ! At this point, the field is not already present or at least GRIDSET in export state, thus we need to find it in
      ! import state and link it

      call ESMF_StateGet(importState, itemSearch=trim(fieldName), itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateGet(importState, name=stateName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemCount<=0) then
        write(message,'(A)') trim(name)//' requested field(bundle) '//trim(fieldname)//' not found in '//trim(stateName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      call ESMF_StateGet(importState, trim(fieldName), itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemType==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(importState, trim(fieldName), importField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGet(importField, status=fieldStatus, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
          write(message,'(A)') trim(name)//' requested field '//trim(fieldname)//' does not contain grid information'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          cycle
        endif

        call ESMF_StateAddReplace(exportState, (/importField/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_FieldDestroy(exportField, rc=localrc)
        !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' replaced empty/added field '
        call MOSSCO_FieldString(importField, message)
      elseif (itemType==ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(importState, trim(fieldName), importFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateAddReplace(exportState, (/importFieldBundle/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        write(message,'(A)') trim(name)//' replaced/added fieldbundle '//trim(fieldName)
      else
        cycle
      endif

      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

    call ESMF_LogFlush()

  end subroutine link_foreign_grid_or_needed_field_in_states

#undef  ESMF_METHOD
#define ESMF_METHOD "copy_1x1x1_field_to_field"
  subroutine  copy_1x1x1_field_to_field(importField, exportField, rc)

    type(ESMF_Field), intent(in)    :: importField
    type(ESMF_Field), intent(inout) :: exportField
    integer, intent(out), optional  :: rc

    integer              :: localrc, rc_
    integer(ESMF_KIND_I4)       :: rank
    integer(ESMF_KIND_I4),allocatable :: totalcount(:), ubnd(:), lbnd(:)
    character (len=ESMF_MAXSTR) :: message, name

    real(ESMF_KIND_R8), pointer :: farrayPtr1(:), farrayPtr2(:,:), farrayPtr3(:,:,:)
    real(ESMF_KIND_R8)          :: defaultValue

    rc_ = ESMF_SUCCESS
    name='link_coupler'

 		call ESMF_FieldGet(importField, rank=rank, rc=localrc)
 		if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

 		if (rank<1 .or. rank > 3) then
 		  write(message,'(A,I1)') trim(name)//' cannot handle field with rank ',rank
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (.not.allocated(totalCount)) allocate(totalCount(rank))
 		call ESMF_FieldGetBounds(importField, totalCount=totalCount, rc=localrc)
 		if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

 		if (any(totalcount>1)) then
 		  write(message,'(A,I1)') trim(name)//' cannot handle non-degenerate field'
 		  call MOSSCO_FieldString(importField,message)
 		  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (rank==1) then
      call ESMF_FieldGet(importField, localDE=0, farrayPtr=farrayPtr1, rc=localrc)
      defaultvalue=farrayPtr1(1)
    elseif (rank==2) then
      call ESMF_FieldGet(importField, localDE=0, farrayPtr=farrayPtr2, rc=localrc)
      defaultvalue=farrayPtr2(1,1)
    elseif (rank==3) then
      call ESMF_FieldGet(importField, localDE=0, farrayPtr=farrayPtr3, rc=localrc)
      defaultvalue=farrayPtr3(1,1,1)
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if(allocated(totalCount)) deallocate(totalCount)

 		call ESMF_FieldGet(exportField, rank=rank, rc=localrc)
 		if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

 		if (rank<1 .or. rank > 3) then
 		  write(message,'(A,I1)') trim(name)//' cannot handle field with rank ',rank
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (.not.allocated(totalCount)) allocate(totalCount(rank))
    if (.not.allocated(ubnd)) allocate(ubnd(rank))
    if (.not.allocated(lbnd)) allocate(lbnd(rank))
      call ESMF_FieldGetBounds(exportField, totalLBound=lbnd,totalUBound=ubnd, rc=localrc)
 		if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rank==1) then
      call ESMF_FieldGet(exportField, localDE=0, farrayPtr=farrayPtr1, rc=localrc)
      farrayPtr1(lbnd(1):ubnd(1)) = defaultValue
    elseif (rank==2) then
      call ESMF_FieldGet(exportField, localDE=0, farrayPtr=farrayPtr2, rc=localrc)
      farrayPtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) = defaultValue
    elseif (rank==3) then
      call ESMF_FieldGet(exportField, localDE=0, farrayPtr=farrayPtr3, rc=localrc)
      farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) = defaultValue
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

	  if (present(rc)) rc=rc_

  end subroutine  copy_1x1x1_field_to_field

end module link_coupler

