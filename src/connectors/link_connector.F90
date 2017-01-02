!> @brief Implementation of an ESMF link coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015, 2016 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "link_connector.F90"
#define VERBOSE

!> @description The link connector is the default and most simple coupler component
!> in MOSSCO.  As a coupler component, it does not define its own import and export states
!> but it receives in these states the export and import states from two other components
!> it should link.
!> The link coupler behaves very differently in its initialization phase (which can be
!> called multiple times), and its run phase. In the initialization phase, it (1) links
!> fields that are specially marked for early linking (ie.e. are flagged as "needed" or
!> "required"), it (2) links empty fields, and (3) copies default values into empty fields.
!> In its run phase, it links all complete fields from its import to its export state.
module link_connector

  use esmf
  use mossco_state
  use mossco_field
  use mossco_component
  use mossco_strings
  use mossco_grid

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

    integer(ESMF_KIND_I4)  :: localrc, rc_

    rc_ = ESMF_SUCCESS

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    rc=rc_

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

    integer(ESMF_KIND_I4)       :: localrc, rc_
    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime

    rc_ = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    rc = rc_

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I4)       :: localrc, rc_
    character (len=ESMF_MAXSTR) :: name
    type(ESMF_Time)             :: currTime

    rc_ = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call link_foreign_grid_or_needed_field_in_states(cplComp, importState, exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !call link_foreign_grid_or_needed_field_in_states(cplComp, exportState, importState, rc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call link_empty_fields_and_fieldbundles_in_states(cplComp, importState, exportState, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !call link_empty_fields_and_fieldbundles_in_states(exportState, importState, rc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_state_copy_default_values(importState, exportState, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    rc = rc_

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

    integer(ESMF_KIND_I4)       :: localrc, rc_
    character (len=ESMF_MAXSTR) :: name
    type(ESMF_Time)             :: currTime, stopTime
    type(ESMF_Clock)            :: clock

    rc_ = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call link_fields_and_fieldbundles_in_states(cplComp, importState, exportState, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (stopTime > currTime) then
      call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
    endif

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    rc = rc_

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
 subroutine Finalize(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)    :: cplComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4)       :: localrc, rc_
    character (len=ESMF_MAXSTR) :: name
    type(ESMF_Time)             :: currTime
    type(ESMF_Clock)            :: clock

    rc_ = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    rc = rc_

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "link_fields_and_fieldbundles_in_states"
  subroutine  link_fields_and_fieldbundles_in_states(cplComp, importState, exportState, rc)

    type(ESMF_CplComp), intent(in)  :: cplComp
    type(ESMF_State), intent(in)    :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out), optional  :: rc

    integer(ESMF_KIND_I4)       :: localrc, rc_, j
    integer(ESMF_KIND_I4)       :: i, itemCount, exportItemCount, importItemCount, differCount
    integer(ESMF_KIND_I4)       :: exportfieldCount, importfieldCount
    character (len=ESMF_MAXSTR) :: message, creatorName
    type(ESMF_Time)             :: currTime, startTime
    type(ESMF_Clock)            :: clock
    character(len=ESMF_MAXSTR), dimension(:), allocatable :: itemNameList, differList
    type(ESMF_StateItem_Flag),  dimension(:), allocatable :: itemTypeList
    type(ESMF_Field)            :: importField, exportField
    type(ESMF_Field), allocatable :: importFieldList(:), exportFieldList(:)
    type(ESMF_FieldBundle)      :: importFieldBundle, exportFieldBundle
    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_FieldStatus_Flag) :: fieldstatus, exportFieldStatus
    logical                     :: isPresent, isConformable = .false.
    type(ESMF_Grid)             :: importGrid, exportGrid
    type(ESMF_GeomType_Flag)    :: importgeomType, exportGeomType
    character(len=ESMF_MAXSTR)  :: name

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_CplCompGet(cplComp, name=name, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Don't do anything if no items in importState
    if (itemCount < 1) return

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(importState, itemTypeList=itemTypeList, &
      itemNameList=itemNameList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, itemCount

      if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then

        call ESMF_StateGet(importState, itemSearch=trim(itemNameList(i)), &
          itemCount=importItemCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (importItemCount /= 1) then
          write(message,'(A)') trim(name)//' skipped multiple item '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          cycle
        endif

        call ESMF_StateGet(importState, trim(itemNameList(i)), importField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGet(importField, status=fieldStatus, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (fieldStatus .eq. ESMF_FIELDSTATUS_EMPTY)  then
          write(message,'(A)') trim(name)//' skipped empty item '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          cycle
        endif

        !> At this point, we have a gridset or complete importField and
        !> search for a corresponding item in the exportState

        call ESMF_StateGet(exportState, itemSearch=trim(itemNameList(i)), &
          itemCount=exportItemCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (exportItemCount == 0) then
          write(message,'(A)') trim(name)//' added new field '
          call MOSSCO_FieldString(importField, message)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          call ESMF_StateAdd(exportState,(/importField/), rc=localrc)
          cycle
        endif

        call ESMF_StateGet(exportState, itemName=trim(itemNameList(i)), itemType=itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !> The item in exportState can be a field or a fieldBundle (which can be filled or empty)
        !> First see whether it is a field and thus matches the importField

        if (itemType == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(exportState, trim(itemNameList(i)), exportField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            !> Only log the message at start time of simulation
            if (currTime == startTime) then
              differCount = MOSSCO_FieldAttributesIdentical(importField, exportField, &
                differList=differList, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
              if (differCount > 0) then
                write(message,'(A)') '  some field attributes not identical for item '//trim(itemNameList(i))
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
                do j = lbound(differList,1), ubound(differList,1)
                  write(message,'(A)') '    '//trim(differList(j))
                  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
                enddo
                deallocate(differList)
              endif
            endif
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            if (exportField /= importField) then
              call ESMF_FieldGet(importField, status=fieldstatus, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

              if (fieldstatus /= ESMF_FIELDSTATUS_COMPLETE)  then
                write(message,'(A)') trim(name)//' skipped incomplete '
                call MOSSCO_FieldString(importField, message)
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
                cycle
              endif

              call ESMF_FieldGet(importField, geomType=importGeomType, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

              if (importGeomType /= ESMF_GEOMTYPE_GRID) then
                write(message,'(A)') trim(name)//' not implemented non-grid geometry in field'
                call MOSSCO_FieldString(importField, message)
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
                cycle
              endif

              call ESMF_FieldGet(importField, grid=importGrid,rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

              call ESMF_FieldGet(exportField, status=exportFieldStatus, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

              if (exportFieldStatus /= ESMF_FIELDSTATUS_EMPTY) then
                call ESMF_FieldGet(exportField, geomType=exportGeomType, rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

                if (exportGeomType /= ESMF_GEOMTYPE_GRID) then
                  write(message,'(A)') trim(name)//' not implemented: non-grid geometry in '
                  call MOSSCO_FieldString(importField, message)
                  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
                  cycle
                endif

                call ESMF_FieldGet(exportField, grid=exportGrid, rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

                call MOSSCO_GridIsConformable(importGrid, exportGrid, isConformable, rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

                if (.not.isConformable) then
                  write(message,'(A)') trim(name)//' might contain non-conforming grids'
                  call MOSSCO_GridString(importGrid, message)
                  call MOSSCO_GridString(exportGrid, message)
                  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
                  !cycle
                endif
              endif

              !> @todo make sure that the attributes of the old field are retained
              call MOSSCO_FieldCopyAttributes(importField, exportField, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

              call ESMF_StateAddReplace(exportState,(/importField/), rc=localrc)
!#ifdef VERBOSE
              write(message,'(A)') trim(name)//' replaced existing '
              call MOSSCO_FieldString(exportField, message)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
              write(message,'(A)') trim(name)//' with '
              call MOSSCO_FieldString(importField, message)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
!#endif

            else
              !write(message,'(A)') '    skipped existing field '//trim(itemNameList(i))
              !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
            endif
          endif
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        cycle
      endif ! ESMF_STATEITEM_FIELD

      if (itemTypeList(i) /= ESMF_STATEITEM_FIELDBUNDLE) then
        write(message,'(A)') trim(name)//' did not link non-field item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        cycle
      endif

      call ESMF_StateGet(importState, trim(itemNameList(i)), importFieldBundle, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateGet(exportState, itemSearch=trim(itemNameList(i)), &
        itemCount=exportItemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (exportItemCount==0) then
          write(message,'(A)') trim(name)//' added fieldBundle '//trim(itemNameList(i))
          call ESMF_AttributeGet(importFieldBundle, 'creator', value=creatorName, &
            defaultvalue='none', isPresent=isPresent, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (isPresent) write(message,'(A)') trim(message)//' ['//trim(creatorName)//']'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          call ESMF_StateAdd(exportState,(/importFieldBundle/), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          cycle
      endif

      call ESMF_StateGet(exportState, itemName=trim(itemNameList(i)), itemType=itemType, rc=localrc)

      if (itemType /= ESMF_STATEITEM_FIELDBUNDLE) then
        write(message,'(A)') trim(name)//' skipped unexpected item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      call ESMF_FieldBundleGet(importFieldBundle, fieldCount=importFieldCount, rc=localrc)
      if (importFieldCount == 0) then
        write(message,'(A)') trim(name)//' skipped empty fieldBundle '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        cycle
      endif

      call ESMF_StateGet(exportState, trim(itemNameList(i)), exportFieldBundle, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! Nothing todo if both items are identical
      if (exportFieldBundle == importFieldBundle) cycle

      allocate(importFieldList(importFieldCount), stat=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldBundleGet(importFieldBundle, fieldList=importFieldList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldBundleGet(exportFieldBundle, fieldCount=exportFieldCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (exportFieldCount == 0) then

        call ESMF_FieldBundleAdd(exportFieldBundle, importFieldList, multiflag=.true., rc=localrc)
        write(message,'(A)') trim(name)//' added to empty fieldBundle '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        cycle
      endif

      if (exportFieldCount /= importFieldCount) then
        write(message,'(A)') trim(name)//' skipped nonmatching size fieldBundle '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      ! allocate(exportFieldList(exportFieldCount), stat=localrc)
      ! call ESMF_FieldBundleGet(exportFieldBundle, fieldList=importFieldList, rc=localrc)
      !
      ! do j = 1, importFieldCount
      !
      !   call MOSSCO_FieldMatchFields(importFieldList(j), exportFieldList, index=matchIndex, rc=localrc)
      !
      !   if (matchIndex < 1) then
      !     write(message,'(A)') '    skipped ambiguous match in '//trim(itemNameList(i))
      !     call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      !     cycle
      !   endif
      ! enddo

      write(message,'(A)') trim(name)//' replaced existing fieldbundle '//trim(itemNameList(i))
      call ESMF_AttributeGet(importFieldBundle, 'creator', value=creatorName, &
        defaultvalue='none', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isPresent) write(message,'(A)') trim(message)//' ['//trim(creatorName)//']'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAddReplace(exportState,(/importFieldBundle/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(importfieldList)) deallocate(importFieldList)

    enddo

    call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine link_fields_and_fieldbundles_in_states


#undef  ESMF_METHOD
#define ESMF_METHOD "link_empty_fields_and_fieldbundles_in_states"
!> This routine looks at all incomplete fields and fieldBundles in exportState,
!> If a corresponding field is available in importState, replace the one in export.

  subroutine  link_empty_fields_and_fieldbundles_in_states(cplComp, importState, exportState, rc)

    type(ESMF_CplComp), intent(in)   :: cplComp
    type(ESMF_State), intent(in)     :: importState
    type(ESMF_State), intent(inout)  :: exportState
    integer, intent(out), optional   :: rc

    integer(ESMF_KIND_I4)       :: localrc, rc_
    integer(ESMF_KIND_I4)       :: i, j, itemCount, exportItemCount, importItemCount, fieldCount, differCount
    integer(ESMF_KIND_I4)       :: rank
    integer(ESMF_KIND_I4),allocatable :: totalcount(:)
    character (len=ESMF_MAXSTR) :: message, creatorName, name
    type(ESMF_Time)             :: currTime, startTime
    character(len=ESMF_MAXSTR), dimension(:), allocatable :: itemNameList, fieldNameList, differList
    type(ESMF_StateItem_Flag),  dimension(:), allocatable :: itemTypeList
    type(ESMF_Field),  allocatable :: fieldList(:)
    type(ESMF_Field)            :: importField, exportField
    type(ESMF_FieldBundle)      :: importFieldBundle, exportFieldBundle
    type(ESMF_GeomType_Flag)    :: importGeomType
    type(ESMF_Grid)             :: importGrid,exportGrid
    type(ESMF_StateItem_Flag)   :: itemType
    logical                     :: isPresent, isConformable
    type(ESMF_FieldStatus_Flag) :: fieldStatus, exportFieldStatus
    type(ESMF_Clock)            :: clock

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_CplCompGet(cplComp, name=name, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount < 1) return

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, itemTypeList=itemTypeList, &
      itemNameList=itemNameList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Loop over items
    do i=1, itemCount

      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then

        call ESMF_StateGet(exportState, trim(itemNameList(i)), exportField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) Then
          write(message, '(A)') trim(name)//' hint: check your item name '//trim(itemNameList(i))//' for invalid characters.'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          cycle
        endif

        call ESMF_FieldGet(exportField, status=exportFieldStatus, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !! don't deal with complete fields here
        if (exportFieldStatus .eq. ESMF_FIELDSTATUS_COMPLETE) then
          write(message, '(A)') trim(name)//' skipped complete field'
          call MOSSCO_FieldString(exportField, message)
          !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif

        call ESMF_StateGet(importState, itemSearch=trim(itemNameList(i)), &
          itemCount=importItemCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (importItemCount < 1) then
          write(message, '(A)') trim(name)//' skipped non-matched field'
          call MOSSCO_FieldString(exportField, message)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif

        call ESMF_StateGet(importState, itemName=trim(itemNameList(i)), &
          itemType=itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemType == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(importState, trim(itemNameList(i)), importField, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldGet(importField, status=fieldStatus, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
            write(message,'(A)') trim(name)//' did not replace empty field with '
            call MOSSCO_FieldString(exportField, message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            cycle
          endif

          !> Only log the message at start time of simulation
          if (currTime == startTime) then
            differCount =MOSSCO_FieldAttributesIdentical(importField, exportField, &
              differList=differList, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            if (differCount > 0) then
              write(message,'(A)') trim(name)//' some field attributes not identical for item '//trim(itemNameList(i))
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
              do j = lbound(differList,1), ubound(differList,1)
                write(message,'(A)') '  '//trim(differList(j))
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
              enddo
              deallocate(differList)
            endif
          endif

          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (exportFieldStatus .eq. ESMF_FIELDSTATUS_GRIDSET) then
            call ESMF_FieldGet(exportField,grid=exportGrid,rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call ESMF_FieldGet(importField, geomType=importGeomType, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            if (importGeomType .ne. ESMF_GEOMTYPE_GRID) cycle
            call ESMF_FieldGet(importField, grid=importGrid, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            if (importGrid /= exportGrid) then
              call MOSSCO_GridIsConformable(importGrid, exportGrid, isConformable, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

              if (.not.isConformable) then
                write(message,'(A)') trim(name)//' skipped non-conforming grids '
                call MOSSCO_GridString(importGrid, message)
                call MOSSCO_GridString(exportGrid, message)
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
                cycle
              endif
            end if

          end if

          call MOSSCO_FieldCopyAttributes(importField, exportField, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_StateAddReplace(exportState, (/importField/), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          write(message,'(A)') trim(name)//' replaced empty field with field '
          call MOSSCO_FieldString(importField, message)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        elseif (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
          ! exportState is an empty field, and importState is a fieldBundle

            call ESMF_StateRemove(exportState,(/trim(itemNameList(i))/), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call ESMF_StateGet(importState, trim(itemNameList(i)), importFieldBundle, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call ESMF_StateAddReplace(exportState, (/importFieldBundle/), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            write(message,'(A)') trim(name)//' replaced empty field with bundle '//trim(itemNameList(i))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        else
          write(message,'(A)') trim(name)//' cannot replace '
          if (present(rc)) rc = ESMF_RC_ARG_BAD
          call MOSSCO_FieldString(exportField, message)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          return
        endif

      elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then

        call ESMF_StateGet(exportState, trim(itemNameList(i)), exportFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateGet(importState, itemSearch=trim(itemNameList(i)), &
          itemCount=importItemCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (importItemCount < 1) then
          write(message, '(A)') '  skipped non-matched fieldBundle '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif

        call ESMF_StateGet(importState, itemName=trim(itemNameList(i)), &
          itemType=itemType, rc=localrc)

        if (itemType == ESMF_STATEITEM_FIELD) then
          !> we have a fieldBundle in exportState and field in importState, then
          !> we add this field to the fieldBundle

          call ESMF_StateGet(importState, trim(itemNameList(i)), importField, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldBundleAdd(exportFieldBundle, (/importField/),  rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          write(message,'(A)') trim(name)//' add homonymous field to fieldBundle '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        elseif (itemType /= ESMF_STATEITEM_FIELDBUNDLE) then
          cycle
        endif

        !> Fieldbundle in import and export states
        call ESMF_StateGet(importState, trim(itemNameList(i)), importFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleGet(exportFieldBundle, fieldCount=fieldCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (fieldCount < 0) then
          write(message,'(A)') trim(name)//' obtained < 0 fields for '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

        if (fieldCount == 0) then

          !> @todo retain attripubes of exportfieldBundle
          call ESMF_StateAddReplace(exportState, (/importFieldBundle/), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldBundleDestroy(exportFieldBundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          write(message,'(A)') trim(name)//' replaced empty fieldBundle '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif

        call MOSSCO_Reallocate(fieldList, fieldCount, keep=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_Reallocate(fieldNameList, fieldCount, keep=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleGet(exportFieldBundle, fieldList=fieldList, fieldNameList=fieldNameList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do j = 1, fieldCount
          call ESMF_FieldGet(fieldList(j), status=fieldStatus, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (fieldStatus /= ESMF_FIELDSTATUS_EMPTY) cycle

          call ESMF_FieldBundleGet(importFieldBundle, fieldNameList(j), isPresent=isPresent, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (isPresent) then
            call ESMF_FieldBundleGet(importFieldBundle, fieldNameList(j), field=importfield, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call ESMF_FieldBundleAddReplace(exportFieldBundle, (/importField/), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            write(message,'(A)') trim(name)//' replaced empty field '//trim(fieldNameList(j))//' in fieldBundle '//trim(itemNameList(i))//' by field from fieldBundle'
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            cycle
          endif

          call ESMF_StateGet(importState, fieldNameList(j), itemType=itemType, rc=localrc)
          if (itemType == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(importState, trim(fieldNameList(j)), importField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call ESMF_FieldBundleAddReplace(exportFieldBundle, (/importField/), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            write(message,'(A)') trim(name)//' replaced empty field '//trim(fieldNameList(j))//' in fieldBundle '//trim(itemNameList(i))//' by field from state'
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          endif
        enddo

        call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_Reallocate(fieldNameList, 0, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif
    enddo

    call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine link_empty_fields_and_fieldbundles_in_states

#undef  ESMF_METHOD
#define ESMF_METHOD "link_foreign_grid_field_in_states"
  subroutine  link_foreign_grid_or_needed_field_in_states(cplComp, importState, exportState, rc)

    type(ESMF_CplComp), intent(in)  :: cplComp
    type(ESMF_State), intent(in)    :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out), optional  :: rc

    integer(ESMF_KIND_I4)       :: localrc, rc_
    integer(ESMF_KIND_I4)       :: i, j, itemCount, exportItemCount, importItemCount, fieldCount, count, len
    character (len=ESMF_MAXSTR) :: message, creatorName, name, fieldName, attributeName, stateName
    type(ESMF_Field)            :: importField, exportField
    type(ESMF_FieldBundle)      :: importFieldBundle, exportFieldBundle
    type(ESMF_StateItem_Flag)   :: itemType
    logical                     :: isPresent, isNeeded
    type(ESMF_FieldStatus_Flag) :: fieldStatus, exportFieldStatus
    type(ESMF_TypeKind_Flag)    :: typekind

    rc_ = ESMF_SUCCESS

    call ESMF_CplCompGet(cplComp, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(exportState, count=count, attcountflag=ESMF_ATTGETCOUNT_ATTRIBUTE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, count

      call ESMF_AttributeGet(exportState, attributeIndex=i, name=attributeName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Our interest is the foreign_grid_field_name attribute as character
      if (trim(attributeName) == 'foreign_grid_field_name') then
        call ESMF_AttributeGet(exportState, trim(attributeName), value=fieldName, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        !! Otherwise look at the name and see whether it ends in ':needed', then expect a logical value
        len=len_trim(attributeName)
        if (len<7) cycle
        if (.not.attributeName(len-6:len) == ':needed') cycle

        fieldName = trim(attributeName(1:len-7))

        call ESMF_AttributeGet(exportState, trim(attributeName), value=isNeeded, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (.not.isNeeded) cycle
      endif

      !write(message,'(A)') trim(name)//' is looking for'
      !call MOSSCO_MessageAdd(message,trim(attributeName)//' '//trim(fieldName))
      !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      ! Check whether it is already there
      call ESMF_StateGet(exportState, itemSearch=trim(fieldName), itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemCount>0) then
        ! If it does exist, check for GRIDSET status and return silently, otherwise continue

        call ESMF_StateGet(exportState, trim(fieldName), itemType=itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemType==ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(exportState, trim(fieldName), exportField, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldGet(exportField, status=exportfieldStatus, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !! If this contains grid information, then return silently
          if (.not. (exportfieldStatus == ESMF_FIELDSTATUS_EMPTY)) cycle

        elseif (itemType==ESMF_STATEITEM_FIELDBUNDLE) then
          call ESMF_StateGet(exportState, trim(fieldName), exportFieldBundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldBundleGet(exportFieldBundle, fieldCount=fieldCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !! If this bundle contains fields, then cycle with warning that this case is not fully checked
          if (fieldCount>0) then
            write(message,'(A)') trim(name)//' requested fieldbundle '//trim(fieldname)//' exists'
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            write(message,'(A)') trim(name)//' this is not implemented, thus skipped'
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
            cycle
          endif
        endif
      endif

      ! At this point, there is either an empty field or fieldBundle, or the item is not found

      call ESMF_StateGet(importState, itemSearch=trim(fieldName), itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateGet(importState, name=stateName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemCount /= 1) then
        write(message,'(A)') trim(name)//' needs exactly one field(bundle) '//trim(fieldname)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        write(message,'(A,I1,A)') trim(name)//' found ',itemCount, ' in '//trim(stateName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_StateGet(importState, trim(fieldName), itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemType==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(importState, trim(fieldName), importField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGet(importField, status=fieldStatus, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
#ifdef VERBOSE
          write(message,'(A)') trim(name)//' did not replace with empty field '
          call MOSSCO_FieldString(importField,message)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
#endif
          cycle
        endif

        !call ESMF_StateGet(exportState, trim(fieldName), exportItemType, rc=localrc)

        !> @todo Retain attributes of old field, but somehow this crashes, as all other
        ! calls to exportField around here ...
        !call MOSSCO_FieldCopyAttributes(importField, exportField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateAddReplace(exportState, (/importField/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_FieldDestroy(exportField, rc=localrc)
        !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifdef VERBOSE
        write(message,'(A)') trim(name)//' replaced empty field '
        call MOSSCO_FieldString(importField, message, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#endif
      elseif (itemType==ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(importState, trim(fieldName), importFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !> @todo retain attributes of exportfieldBundle?
        call ESMF_StateAddReplace(exportState, (/importFieldBundle/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        write(message,'(A)') trim(name)//' replaced/added fieldbundle '//trim(fieldName)
      else
        cycle
      endif

      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

  end subroutine link_foreign_grid_or_needed_field_in_states


!> This subroutine sets a default real8 value for a non-empty field.
!> The default value is 0.0 if not provided
!> GRIDSET fields are promoted to complete with this subroutine
#undef  ESMF_METHOD
#define ESMF_METHOD "FieldSetValue"
  subroutine MOSSCO_FieldSetValue(field, value, rc)

    implicit none

    type(ESMF_Field), intent(inout)              :: field
    real(ESMF_KIND_R8), intent(in), optional     :: value
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    real(ESMF_KIND_R8)                   :: value_
    integer(ESMF_KIND_I4)                :: rc_, localrc, rank
    integer(ESMF_KIND_I4), allocatable   :: ubnd(:), lbnd(:)
    type(ESMF_FieldStatus_Flag)          :: fieldStatus
    real(ESMF_KIND_R8), pointer          :: farrayPtr1(:), farrayPtr2(:,:), farrayPtr3(:,:,:)
    real(ESMF_KIND_R8), pointer          :: farrayPtr4(:,:,:,:), farrayPtr5(:,:,:,:,:)
    real(ESMF_KIND_R8), pointer          :: farrayPtr6(:,:,:,:,:,:), farrayPtr7(:,:,:,:,:,:,:)
    character(len=ESMF_MAXSTR)           :: message, name

    rc_ = ESMF_SUCCESS
    if (present(value)) then
      value_ = value
    else
      value_ = 0.0_ESMF_KIND_R8
    endif

    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldStatus .ne. ESMF_FIELDSTATUS_COMPLETE) then
      if (present(rc)) rc=rc_
      return
    endif

    if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
      write(message,'(A)') 'cannot assign a value to empty field'
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (present(rc)) rc=rc_
      return
    endif

    !> If the field status is gridset, then complete the field with real8 typekind
    if (fieldStatus == ESMF_FIELDSTATUS_GRIDSET) then
      call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rank<1 .or. rank > 7) then
      write(message,'(A,I1,A)') trim(name)//' cannot handle rank ',rank,' in field'
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (.not.allocated(ubnd)) allocate(ubnd(rank), stat=localrc)
    if (.not.allocated(lbnd)) allocate(lbnd(rank), stat=localrc)

    call ESMF_FieldGetBounds(field, totalLBound=lbnd, totalUBound=ubnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rank==1) then
      call ESMF_FieldGet(field, localDE=0, farrayPtr=farrayPtr1, rc=localrc)
      farrayPtr1(lbnd(1):ubnd(1)) = value_
    elseif (rank==2) then
      call ESMF_FieldGet(field, localDE=0, farrayPtr=farrayPtr2, rc=localrc)
      farrayPtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) = value_
    elseif (rank==3) then
      call ESMF_FieldGet(field, localDE=0, farrayPtr=farrayPtr3, rc=localrc)
      farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) = value_
    elseif (rank==4) then
      call ESMF_FieldGet(field, localDE=0, farrayPtr=farrayPtr4, rc=localrc)
      farrayPtr4(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3),lbnd(4):ubnd(4)) = value_
    elseif (rank==5) then
      call ESMF_FieldGet(field, localDE=0, farrayPtr=farrayPtr5, rc=localrc)
      farrayPtr5(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3),lbnd(4):ubnd(4),lbnd(5):ubnd(5)) = value_
    elseif (rank==6) then
      call ESMF_FieldGet(field, localDE=0, farrayPtr=farrayPtr6, rc=localrc)
      farrayPtr6(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3),lbnd(4):ubnd(4),lbnd(5):ubnd(5),lbnd(6):ubnd(6)) = value_
    elseif (rank==7) then
      call ESMF_FieldGet(field, localDE=0, farrayPtr=farrayPtr7, rc=localrc)
      farrayPtr7(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3),lbnd(4):ubnd(4), &
        lbnd(5):ubnd(5),lbnd(6):ubnd(6),lbnd(7):ubnd(7)) = value_
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A)') 'assigned value to field'
    call MOSSCO_FieldString(field, message, rc=localrc)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(lbnd)) deallocate(lbnd)

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_FieldSetValue

#undef  ESMF_METHOD
#define ESMF_METHOD "state_copy_default_values"
  subroutine MOSSCO_state_copy_default_values(importState, exportState, rc)

    implicit none

    type(ESMF_State), intent(in)              :: importState
    type(ESMF_State), intent(inout)           :: exportState
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                     :: rc_, localrc, i
    character(len=ESMF_MAXSTR)                :: message, name
    character(len=ESMF_MAXSTR), allocatable   :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable    :: itemTypeList(:)
    integer(ESMF_KIND_I4)                     :: itemCount
    type(ESMF_StateItem_Flag)                 :: itemType
    type(ESMF_FieldBundle)                    :: importFieldBundle, exportFieldBundle
    type(ESMF_Field)                          :: importField, exportField

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc=rc_

    call ESMF_StateGet(exportState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount < 1) return

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, itemTypeList=itemTypeList, &
      itemNameList=itemNameList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Loop over items
    do i=1, itemCount

      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then

        call ESMF_StateGet(importState, trim(itemNameList(i)), itemType=itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemType /= ESMF_STATEITEM_FIELD) cycle

        call ESMF_StateGet(importState, trim(itemNameList(i)), importField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateGet(exportState, trim(itemNameList(i)), exportField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_field_copy_default_values(importField, exportField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

     elseif (itemTypeList(i)==ESMF_STATEITEM_FIELDBUNDLE) then

        call ESMF_StateGet(importState, trim(itemNameList(i)), itemType=itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemType /= ESMF_STATEITEM_FIELDBUNDLE) cycle

        call ESMF_StateGet(importState, trim(itemNameList(i)), importFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_StateGet(exportState, trim(itemNameList(i)), exportFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_fieldbundle_copy_default_values(importFieldBundle, exportFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    enddo

    call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_state_copy_default_values

 subroutine MOSSCO_fieldbundle_copy_default_values(importFieldBundle, exportFieldBundle, rc)

    implicit none

    type(ESMF_FieldBundle), intent(in)              :: importFieldBundle
    type(ESMF_FieldBundle), intent(inout)           :: exportFieldBundle
    integer(ESMF_KIND_I4), intent(out), optional    :: rc

    integer(ESMF_KIND_I4)                     :: rc_, localrc, i,j
    character(len=ESMF_MAXSTR)                :: message, name
    character(len=ESMF_MAXSTR), allocatable   :: fieldNameList(:)
    type(ESMF_Field), allocatable             :: fieldList(:), itemList(:)
    integer(ESMF_KIND_I4)                     :: fieldCount, itemCount
    type(ESMF_Field)                          :: importField
    logical                                   :: isPresent

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc=rc_

    call ESMF_FieldBundleGet(exportFieldBundle, fieldCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount < 0) return

    call MOSSCO_Reallocate(fieldList, fieldCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(fieldNameList, fieldCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldBundleGet(exportFieldBundle, fieldList=fieldList, &
      fieldNameList=fieldNameList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Loop over items
    do i=1, fieldCount

      !! @todo: find a working way to do this, at the moment, the implementation fails
      write(message,'(A)') 'Not implemented: copying default values in fieldBundles'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      cycle

      call ESMF_FieldBundleGet(importFieldBundle, trim(fieldNameList(i)), fieldCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemCount==0) cycle

      call MOSSCO_Reallocate(itemList, itemCount, keep=.false., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldBundleGet(importFieldBundle, fieldName=trim(fieldNameList(i)), fieldList=itemlist, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      do j=1, itemCount
        call MOSSCO_field_copy_default_values(importField, itemList(j), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo

      call MOSSCO_Reallocate(itemList, 0, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(fieldNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_fieldbundle_copy_default_values

 subroutine MOSSCO_field_copy_default_values(importField, exportField, rc)

    implicit none

    type(ESMF_Field), intent(in)              :: importField
    type(ESMF_Field), intent(inout)           :: exportField
    integer(ESMF_KIND_I4), intent(out), optional    :: rc

    integer(ESMF_KIND_I4)                     :: rc_, localrc
    character(len=ESMF_MAXSTR)                :: message, attributeName
    logical                                   :: isPresent
    real(ESMF_KIND_R8)                        :: real8
    real(ESMF_KIND_R4)                        :: real4
    integer(ESMF_KIND_I8)                     :: int8
    integer(ESMF_KIND_I4)                     :: int4
    type(ESMF_TypeKind_Flag)                  :: typeKind

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc=rc_

    attributeName='default_value'

    if (importField == exportField) return

    call ESMF_AttributeGet(importField, trim(attributeName), isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) return

    call ESMF_AttributeGet(importField, trim(attributeName), typeKind=typeKind, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (typeKind == ESMF_TYPEKIND_R8) then
      call ESMF_AttributeGet(importField, trim(attributeName), value=real8, rc=localrc)
    elseif (typeKind == ESMF_TYPEKIND_R4) then
      call ESMF_AttributeGet(importField, trim(attributeName), value=real4, rc=localrc)
      real8=real(real4, ESMF_KIND_R8)
    elseif (typeKind == ESMF_TYPEKIND_I8) then
      call ESMF_AttributeGet(importField, trim(attributeName), value=int8, rc=localrc)
      real8=real(int8, ESMF_KIND_R8)
    elseif (typeKind == ESMF_TYPEKIND_I4) then
      call ESMF_AttributeGet(importField, trim(attributeName), value=int4, rc=localrc)
      real8=real(int4, ESMF_KIND_R8)
    else
      if (present(rc)) rc=ESMF_RC_ARG_BAD
      return
    endif

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(exportField, trim(attributeName), isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      call ESMF_AttributeSet(exportField, trim(attributeName), value=real8, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call MOSSCO_FieldSetValue(exportField, real8, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_field_copy_default_values

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_link_fields_in_states"
!> @brief Links all fields from import state into export stateName
!>
!> Complete fields that don't exist in exportState are linked there
!> Fields that identically exist in exporState are skipped
!> Complete fields that exist in exportState with identical attributes are overwritten
!> Complete fields whose name matches a FieldBundles ...
!> FieldBundles that don't exist in exportState are linked there
!> Fields in fieldBundles that don't exist in exportState are linked there
!> ...
  subroutine  MOSSCO_link_fields_in_states(importState, exportState, rc)

    type(ESMF_State), intent(in)    :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out), optional  :: rc

    integer                     :: localrc, rc_
    integer(ESMF_KIND_I4)       :: i, itemCount
    integer(ESMF_KIND_I4)       :: j, k, fieldCount, differCount
    character (len=ESMF_MAXSTR) :: message
    character(len=ESMF_MAXSTR), dimension(:), allocatable :: itemNameList
    type(ESMF_StateItem_Flag),  dimension(:), allocatable :: itemTypeList
    type(ESMF_Field), allocatable, dimension(:)           :: importFieldList, exportFieldList
    type(ESMF_Field)            :: importField, exportField
    type(ESMF_FieldBundle)      :: importFieldBundle, exportFieldBundle
    type(ESMF_StateItem_Flag)   :: exportItemType, importItemType
    type(ESMF_FieldStatus_Flag) :: exportFieldStatus, importFieldStatus
    logical                     :: found

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount < 1) return

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(importState, itemTypeList=itemTypeList, &
      itemNameList=itemNameList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Loop over items
    do i=1, itemCount

      call MOSSCO_StateGetFieldList(importState, itemSearch=trim(itemNameList(i)), &
        fieldList=importFieldList, fieldStatus=importFieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateGet(exportState, trim(itemNameList(i)), &
        itemType=exportItemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_StateGetFieldList(exportState, itemSearch=trim(itemNameList(i)), &
        fieldList=exportFieldList, fieldStatus=exportFieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! If the item does not exist in export state, then simply link it into the
      ! export state
      if (exportItemType == ESMF_STATEITEM_NOTFOUND) then

        if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then

          ! Do not link an empty field
          if (importfieldStatus /= ESMF_FIELDSTATUS_COMPLETE) cycle

          call ESMF_StateAdd(exportState, importFieldList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then

          ! Do not link an empty fieldBundle
          call ESMF_StateGet(importState, trim(itemNameList(i)), importFieldBundle, rc=localrc)
          call ESMF_FieldBundleGet(importFieldBundle, fieldCount=fieldCount, rc=localrc)
          if (fieldCount < 1) cycle

          call ESMF_StateAdd(exportState, (/importFieldBundle/), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        else
          write(message,'(A)') '  cannot link to non-field or non-fieldBundle '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        endif

      elseif (exportItemType == ESMF_STATEITEM_FIELD) then

        if (importItemType == ESMF_STATEITEM_FIELD) then

          ! Don't do anything if the fields are identical
          if (importFieldList(1) == exportFieldList(1)) cycle

          ! Don't do anything if the import field is not complete
          if (importFieldStatus .ne. ESMF_FIELDSTATUS_COMPLETE) cycle

          differCount = MOSSCO_FieldAttributesIdentical(importFieldList(1), exportFieldList(1), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (differCount > 0) then
            write(message,'(A)') '  field attributes are not identical for '
            call MOSSCO_FieldString(importFieldList(1), message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          ! if (exportFieldStatus /= ESMF_FIELDSTATUS_EMPTY) then
          !   if (MOSSCO_FieldGeometryConformal(importFieldList(1), exportFieldList(1), rc=localrc) > 0) then
          !     write(message,'(A)') '  field geometry not conformal for '
          !     call MOSSCO_FieldString(importFieldList(1), message)
          !     call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          !     cycle
          !   endif
          ! endif
          call MOSSCO_FieldCopyAttributes(importField, exportField, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_StateAddReplace(exportState,(/importField/), rc=localrc)
          write(message,'(A)') '    replaced '
          call MOSSCO_FieldString(exportField, message)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          write(message,'(A)') '    with '
          call MOSSCO_FieldString(importField, message)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

        elseif (itemTypeList(i)==ESMF_STATEITEM_FIELDBUNDLE) then
          write(message,'(A)') '  not implemented: link fieldBundle to field '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

        else
          write(message,'(A)') '  cannot link non-field or non-fieldBundle '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        endif

      elseif (exportItemType == ESMF_STATEITEM_FIELDBUNDLE) then

        if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then

        elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then

          if (size(importFieldList) < 1) cycle

          call ESMF_StateGet(exportState, trim(itemNameList(i)), exportFieldBundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (size(exportFieldList) < 1) then
            call ESMF_FieldBundleAdd(exportFieldBundle, importFieldList, multiflag=.true., rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            cycle
          endif

          do j=lbound(importFieldList,1), ubound(importFieldList,1)

            ! Test for finding an identical field, if found, then skip
            ! this field, as it is already there
            found=.false.
            do k=lbound(exportFieldList,1), ubound(exportFieldList,1)
              if (importFieldList(j) == exportFieldList(k)) then
                found = .true.
                exit
              endif
            enddo
            if (found) cycle

            ! Test for finding a field with identical properties.
            found=.false.
            differCount = 0

            do k=lbound(exportFieldList,1), ubound(exportFieldList,1)
              differCount = MOSSCO_FieldAttributesIdentical(importFieldList(j), exportFieldList(k), rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

              if (differCount == 0) then
                found = .true.
                exit
              endif
            enddo
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            if (found) then
              ! If found then replace
              write(message,'(A)') '  replaced '//trim(itemNameList(i))//' in bundle'
              exportFieldList(k)=importFieldList(j)
            else
              ! else add this field
              call MOSSCO_Reallocate(exportFieldList, size(exportFieldList) + 1, keep=.true., rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

              exportFieldList(size(exportFieldList)) = importFieldList(j)

              write(message,'(A)') '  added '//trim(itemNameList(i))//' to bundle'

            endif

            call ESMF_FieldBundleReplace(exportFieldBundle, exportFieldList, multiflag=.true., rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          enddo ! loop over importFieldList
        else ! importItemType
          write(message,'(A)') '  cannot link  non-field, non-fieldBundle import item '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        endif ! importItemType
      else
        write(message,'(A)') '  cannot link  non-field, non-fieldBundle export item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      endif ! exportItemType
    enddo ! importItemCount

    call MOSSCO_Reallocate(importFieldList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(exportFieldList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_link_fields_in_states

end module link_connector
