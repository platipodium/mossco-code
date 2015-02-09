!> @brief Implementation of an ESMF connector coupling
!> This coupler selectively looks at variables in its import state
!> that satisfy a condition layed out in a "filter_suffix" attribute,
!> (by default "z_velocity_in_water"), looks at matching fields without this
!> suffix.
!>
!> In Init phase 1, empty corresponding fields for these fields are created
!> in the coupler's export state
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2015, Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "transport_connector.F90"

module transport_connector

  use esmf
  use mossco_state
  use mossco_field
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
    character(len=ESMF_MAXSTR)  :: name
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

    call link_fields_in_transport_fieldbundle(importState, exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    call MOSSCO_CplCompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

  !> the Run() routine of this coupler copies is empty

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

    rc = ESMF_SUCCESS

    call ESMF_CplCompGet(cplComp, name=name, rc=localrc)

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
#define ESMF_METHOD "create_empty_fields_for_filter_value"
  subroutine  create_empty_fields_for_filter_value(importState, exportState, rc)

    type(ESMF_State), intent(in)    :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out)            :: rc

    integer              :: localrc
    integer(ESMF_KIND_I4)       :: i, itemCount
    character (len=ESMF_MAXSTR) :: message, itemName, filter_suffix, name, replace_suffix
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList
    type(ESMF_Field)            :: field
    integer(ESMF_KIND_I4)       :: length, suffix_length
    type(ESMF_StateItem_Flag)   :: importItemState, exportItemState

    rc = ESMF_SUCCESS

    write(name,'(A)') 'transport_connector'

    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Allocate/reallocate list do hold item information
    if (itemCount > 0) then
      if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))

      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_AttributeGet(importState, 'filter_suffix', value=filter_suffix, &
      defaultValue='_z_velocity_in_water', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, 'replace_suffix', value=replace_suffix, &
      defaultValue='_in_water', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    suffix_length=len_trim(filter_suffix)

    !! Loop over items
    do i=1, itemCount

      itemName=trim(itemNameList(i))
      length=len_trim(itemName)
      if (length <= suffix_length) cycle
      if (itemName(length-suffix_length+1:length) /= trim(filter_suffix)) cycle

      !> Get the field name without the suffix, and make sure it is a field
      itemName=trim(itemName(1:length-suffix_length))//trim(replace_suffix)

      call ESMF_StateGet(importState, itemName=trim(itemName), &
          itemType=importItemState, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (importItemState == ESMF_STATEITEM_FIELDBUNDLE) then
        write(message,'(A)') trim(name)//' transport of field bundles not yet implemented'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      endif

      if (importItemState /= ESMF_STATEITEM_FIELD) cycle

      !> Make sure this is not present already in export state
      call ESMF_StateGet(exportState, itemName=trim(itemName),  &
        itemType=exportItemState, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (exportItemState /= ESMF_STATEITEM_NOTFOUND) cycle

      !> Get the field name *with* the suffix, and make sure it is not present in export
      !> state yet.
       call ESMF_StateGet(importState, itemName=trim(itemNameList(i)), &
          itemType=importItemState, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (importItemState /= ESMF_STATEITEM_FIELD) cycle

      call ESMF_StateGet(exportState, itemName=trim(itemNameList(i)), &
        itemType=exportItemState, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (exportItemState /= ESMF_STATEITEM_NOTFOUND) cycle

      field = ESMF_FieldEmptyCreate(name=trim(itemNameList(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateAdd(exportState,(/field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' added for transport field'
      call MOSSCO_FieldString(field, message)

      field = ESMF_FieldEmptyCreate(name=trim(itemName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateAdd(exportState,(/field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' added for transport field'
      call MOSSCO_FieldString(field, message)
    enddo

    return

  end subroutine create_empty_fields_for_filter_value


#undef  ESMF_METHOD
#define ESMF_METHOD "link_fields_in_transport_fieldbundle"
  subroutine  link_fields_in_transport_fieldbundle(importState, exportState, rc)

    type(ESMF_State), intent(in)    :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out)            :: rc

    integer              :: localrc
    integer(ESMF_KIND_I4)       :: i, itemCount
    character (len=ESMF_MAXSTR) :: itemName, filter_suffix, name, replace_suffix
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList
    type(ESMF_StateItem_Flag)   :: exportItemType
    integer(ESMF_KIND_I4)       :: length, suffix_length
    type(ESMF_StateItem_Flag)   :: importItemState, exportItemState
    type(ESMF_FieldBundle)      :: exportFieldBundle

    rc = ESMF_SUCCESS

    write(name,'(A)') 'transport_connector'

    !> Check whether there is a fieldbundle named "transport" in the exportState, only continue if there is.
    call ESMF_StateGet(exportState, 'transport', exportItemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (exportItemState /= ESMF_STATEITEM_FIELDBUNDLE) return

    call ESMF_StateGet(exportState, 'transport', exportFieldBundle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Find all complete fields in import state whose names correspond to the filter value
    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Allocate/reallocate list do hold item information
    if (itemCount > 0) then
      if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))

      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_AttributeGet(importState, 'filter_suffix', value=filter_suffix, &
      defaultValue='_z_velocity_in_water', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, 'replace_suffix', value=replace_suffix, &
      defaultValue='_in_water', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    suffix_length=len_trim(filter_suffix)

    !! Loop over items
    do i=1, itemCount

      itemName=trim(itemNameList(i))
      length=len_trim(itemName)
      if (length <= suffix_length) cycle
      if (itemName(length-suffix_length+1:length) /= trim(filter_suffix)) cycle

      !> Get the field name without the suffix, and make sure it is a field
      itemName=trim(itemName(1:length-suffix_length))//trim(replace_suffix)


      !> At this stage, both the name of the field and of the velocity field have been found in the
      !> import state.

      call ESMF_StateGet(importState, itemName=trim(itemName), &
          itemType=importItemState, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (importItemState == ESMF_STATEITEM_FIELD) then
        call link_field_in_transport_fieldbundle(importState, trim(itemName), exportFieldBundle, rc=localrc)
        call link_field_in_transport_fieldbundle(importState, trim(itemNameList(i)), exportFieldBundle, rc=localrc)
      elseif (importItemState == ESMF_STATEITEM_FIELDBUNDLE) then
        call link_fieldbundle_in_transport_fieldbundle(importState, trim(itemName), exportFieldBundle, rc=localrc)
        call link_fieldbundle_in_transport_fieldbundle(importState, trim(itemNameList(i)), exportFieldBundle, rc=localrc)
      endif

    enddo

  return

  end subroutine link_fields_in_transport_fieldbundle

#undef  ESMF_METHOD
#define ESMF_METHOD "link_field_in_transport_fieldbundle"
  subroutine  link_field_in_transport_fieldbundle(importState, importName, exportFieldBundle, rc)

    type(ESMF_State), intent(in)          :: importState
    character(ESMF_MAXSTR), intent(in)    :: importName
    type(ESMF_FIELDBUNDLE), intent(inout) :: exportFieldBundle
    integer, intent(out)                  :: rc

    integer              :: localrc
    integer(ESMF_KIND_I4)       :: fieldCount
    character (len=ESMF_MAXSTR) :: message, itemName,  name
     type(ESMF_Field)            :: importField, field
    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_Grid)             :: importGrid, exportGrid
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_GeomType_Flag)    :: exportGeomType, importGeomType


    rc = ESMF_SUCCESS

    write(name,'(A)') 'transport_connector'

    call ESMF_StateGet(importState, trim(itemName), itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELD) return

    call ESMF_StateGet(importState, trim(itemName), field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) return

    call ESMF_FieldGet(importField, geomType=importGeomType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (importGeomType == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(importField, grid=importGrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldBundleGet(exportFieldBundle, trim(itemName), fieldCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (fieldCount > 0) return

    call ESMF_FieldBundleGet(exportFieldBundle, geomType=exportGeomType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (exportGeomType == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldBundleGet(exportFieldBundle, grid=exportGrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (exportGeomType == ESMF_GEOMTYPE_GRID .and. importGeomType == ESMF_GEOMTYPE_GRID .and. (importGrid == exportGrid)) then
      call ESMF_FieldBundleAdd(exportFieldBundle,(/importField/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' linked for transport field'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    else
      field = ESMF_FieldEmptyCreate(name=trim(itemName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldBundleAdd(exportfieldBundle,(/field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' created for transport empty field'
      call MOSSCO_FieldString(field, message)
    endif

    return

  end subroutine link_field_in_transport_fieldbundle

#undef  ESMF_METHOD
#define ESMF_METHOD "link_fieldbundle_in_transport_fieldbundle"
  subroutine  link_fieldbundle_in_transport_fieldbundle(importState, importName, exportFieldBundle, rc)

    type(ESMF_State), intent(in)          :: importState
    character(ESMF_MAXSTR), intent(in)    :: importName
    type(ESMF_FIELDBUNDLE), intent(inout) :: exportFieldBundle
    integer, intent(out)                  :: rc

    integer              :: localrc
    integer(ESMF_KIND_I4)       :: i, itemCount
    character (len=ESMF_MAXSTR) :: message, itemName, name
    type(ESMF_Field)            :: field
    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_Grid)             :: importGrid, exportGrid
    type(ESMF_Field), allocatable :: fieldList(:)
    type(ESMF_FieldBundle)      :: fieldBundle
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_GeomType_Flag)    :: exportGeomType, importGeomType

    rc = ESMF_SUCCESS

    write(name,'(A)') 'transport_connector'

    call ESMF_FieldBundleGet(exportFieldBundle, geomType=exportGeomType, rc=localrc)
    if (exportGeomType == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldBundleGet(exportFieldBundle, grid=exportGrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(importState, trim(itemName), itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELDBUNDLE) return

    call ESMF_StateGet(importState, trim(itemName), fieldBundle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldBundleGet(fieldBundle, fieldCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount <= 0) return

    allocate(fieldList(itemCount))
    call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, itemCount

      call ESMF_FieldGet(fieldList(i), status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) return

      call ESMF_FieldGet(fieldList(i), geomType=importGeomType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (importGeomType == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(fieldList(i), grid=importGrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      if (importGeomType == ESMF_GEOMTYPE_GRID .and. exportGeomType == ESMF_GEOMTYPE_GRID .and. (importGrid == exportGrid)) then
        call ESMF_FieldBundleAdd(exportFieldBundle,(/fieldList(i)/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' linked for transport field'
        call MOSSCO_FieldString(fieldList(i) , message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        field = ESMF_FieldEmptyCreate(name=trim(itemName), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleAdd(exportfieldBundle,(/field/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' created for transport empty field'
        call MOSSCO_FieldString(field, message)
      endif
    enddo

    return

  end subroutine link_fieldbundle_in_transport_fieldbundle


end module transport_connector

