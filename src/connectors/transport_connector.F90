!> @brief Implementation of an ESMF connector coupling
!>
!> This coupler selectively looks at variables in its import state
!> that satisfy a condition layed out in a "include" attribute,
!> (by default "z_velocity_in_water"), looks at matching fields without this
!> suffix.
!>
!> In Init phase 1, empty corresponding fields for these fields are created
!> in the coupler's export state
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
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
  use mossco_attribute
  use mossco_config
  use mossco_strings

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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

    character(len=ESMF_MAXSTR), pointer     :: filterIncludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer     :: filterExcludeList(:) => null()
    character(len=ESMF_MAXSTR)              :: configFileName, message
    logical                                 :: isPresent, createVelocity
    type(ESMF_Config)                       :: config

    createVelocity = .false.
    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    configfilename=trim(name)//'.cfg'
    inquire(file=trim(configfilename), exist=isPresent)

    if (isPresent) then

      write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      config = ESMF_ConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_ConfigGet(config, label='create_velocity', value=createVelocity, &
        defaultValue=.false., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_ConfigGet(config, 'exclude', filterExcludeList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (associated(filterExcludeList)) then
        call MOSSCO_AttributeSet(cplComp, 'filter_pattern_exclude', filterExcludeList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        deallocate(filterExcludeList)

        call MOSSCO_AttributeGet(cplComp, 'filter_pattern_exclude', filterExcludeList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' uses exclude patterns:'
        call MOSSCO_MessageAddListPtr(message, filterExcludeList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call MOSSCO_ConfigGet(config, 'include', filterIncludeList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    endif

    if (.not.associated(filterIncludeList)) then
      allocate(filterIncludeList(1))
      filterIncludeList(1) ='*_z_velocity_in_water'
    endif

    call MOSSCO_AttributeSet(cplComp, 'filter_pattern_include', filterIncludeList, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    deallocate(filterIncludeList)

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_include', filterIncludeList, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(name)//' uses include patterns:'
    call MOSSCO_MessageAddListPtr(message, filterIncludeList, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_AttributeSet(cplComp, 'create_velocity', createVelocity, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call link_fields_in_transport_fieldbundle(cplComp, importState, exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

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

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "link_fields_in_transport_fieldbundle"
  subroutine  link_fields_in_transport_fieldbundle(cplComp, importState, exportState, rc)

    type(ESMF_CplComp), intent(in)  :: cplComp
    type(ESMF_State), intent(inout) :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out), optional  :: rc

    integer              :: localrc
    integer(ESMF_KIND_I4)       :: i, itemCount, rc_
    character (len=ESMF_MAXSTR) :: itemName, filter_suffix, name, replace_suffix
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList
    type(ESMF_StateItem_Flag)   :: exportItemType
    integer(ESMF_KIND_I4)       :: length, suffix_length
    type(ESMF_StateItem_Flag)   :: wsItemType, itemType
    type(ESMF_FieldBundle)      :: concFieldBundle,wsFieldBundle

    logical                                  :: isMatch, createVelocity
    character (len=ESMF_MAXSTR), allocatable :: filterIncludeList(:), filterExcludeList(:)
    integer(ESMF_KIND_I4)                    :: j, k
    character (len=ESMF_MAXSTR)              :: message, wsItemName, stateName
    type(ESMF_Field), allocatable            :: fieldList(:)
    type(ESMF_Field)                         :: field

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_CplCompGet(cplComp, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, name=stateName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Check whether there is a fieldbundle named "concentrations_in_water" in the exportState,
    !> only continue if there is.
    call ESMF_StateGet(exportState, itemName='concentrations_in_water', &
      itemType=exportItemType, rc=localrc)

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (exportItemType /= ESMF_STATEITEM_FIELDBUNDLE) then
      write(message,'(A)') trim(name)//' did not find fieldBundle "concentrations_in_water"'
      call MOSSCO_MessageAdd(message,' in its export state')
      call MOSSCO_MessageAdd(message,' '//trim(stateName))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (present(rc)) rc=ESMF_SUCCESS
    endif

    call ESMF_StateGet(exportState, 'concentrations_in_water', concFieldBundle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Check whether there is a fieldbundle named "concentrations_z_velocity_in_water" in
    !> the exportState, only continue if there is.
    call ESMF_StateGet(exportState, 'concentrations_z_velocity_in_water', exportItemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (exportItemType /= ESMF_STATEITEM_FIELDBUNDLE) then
        write(message,'(A)') trim(name)//' did not find fieldBundle "concentrations_in_water_z_velocity"'
        call MOSSCO_MessageAdd(message,' in its export state')
        call MOSSCO_MessageAdd(message,' '//trim(stateName))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        if (present(rc)) rc=ESMF_SUCCESS
    endif

    call ESMF_StateGet(exportState, 'concentrations_z_velocity_in_water', wsFieldBundle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Find all complete fields in import state whose names correspond to the filter value
    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Allocate/reallocate list do hold item information
    if (itemCount > 0) then
      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_include', filterIncludeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_exclude', filterExcludeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(cplComp, 'create_velocity', value=createVelocity, &
      defaultValue=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Loop over items
    do i=1, itemCount

      itemName=trim(itemNameList(i))
      call ESMF_StateGet(importState, itemName=trim(itemName), &
        itemType=itemType, rc=localrc)

      ! Look for an exclusion pattern on this field name
      if (allocated(filterExcludeList)) then
        do j = lbound(filterExcludeList,1), ubound(filterExcludeList,1)
          call MOSSCO_StringMatch(trim(itemName), trim(filterExcludeList(j)), isMatch, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (ismatch) exit
        enddo
        if (ismatch) then
          write(message,'(A)')  trim(name)//' excluded item'
          call MOSSCO_MessageAdd(message, ' '//trim(itemName))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif
      endif

      !! Look for an inclusion pattern on this field name
      if (allocated(filterIncludeList)) then
        do j = lbound(filterIncludeList,1), ubound(filterIncludeList,1)
          call MOSSCO_StringMatch(trim(itemName), trim(filterIncludeList(j)), isMatch, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (ismatch) then

            ! Check for a corresponding velocity field
            if (.not.createVelocity) then
              k = index(itemName, '_z_velocity_in_water')
              if (k < 1) then
                k = index(itemName, '_in_water')
                call ESMF_StateGet(importState, itemName(1:k)//'z_velocity_in_water', &
                  itemType=itemType, rc=localrc)
                if (itemType == ESMF_STATEITEM_NOTFOUND .and. .not.createVelocity) then
                  write(message,'(A)')  trim(name)//' did not include'
                  call MOSSCO_MessageAdd(message, ' '//trim(itemName)//' without corresponding z-velocity')
                  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
                endif
                isMatch = .false.
                exit
              endif
            endif
            write(message,'(A)')  trim(name)//' includes from filter '//trim(filterIncludeList(j))
            call MOSSCO_MessageAdd(message, ' '//trim(itemName))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            exit
          endif

          ! If the filter contains z_velocity, and there is a corresponding z-velocity
          ! to a base variable then also include the base variable
          k = index(filterIncludeList(j),'_z_velocity_in_water')
          if (k > 0) k = index(itemName, '_in_water')
          if (k > 0) then
            call ESMF_StateGet(importState, itemName(1:k)//'z_velocity_in_water', &
              itemType=itemType, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            ! Unless the createVelocity is set to .true. cycle on a non-existing corresponding
            ! field (which is needed for getm)
            if (itemType == ESMF_STATEITEM_NOTFOUND .and. .not.createVelocity) then
              write(message,'(A)')  trim(name)//' did not include'
              call MOSSCO_MessageAdd(message, ' '//trim(itemName)//' without corresponding z-velocity')
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
              cycle
            endif

            call MOSSCO_StringMatch(itemName(1:k)//'z_velocity_in_water', trim(filterIncludeList(j)), isMatch, localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            if (ismatch) then
              write(message,'(A)')  trim(name)//' includes from expanded filter '//trim(filterIncludeList(j))
              call MOSSCO_MessageAdd(message, ' '//trim(itemName))
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
              exit
            endif
          endif

          ! If the variable contains z_velocity, check whether the base name is
          ! included in filter
          k = index(itemName,'_z_velocity_in_water')
          if (k > 0) then
            call MOSSCO_StringMatch(itemName(1:k)//'in_water', trim(filterIncludeList(j)), isMatch, localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            if (ismatch) then
              write(message,'(A)')  trim(name)//' includes from expanded filter '//trim(filterIncludeList(j))
              call MOSSCO_MessageAdd(message, ' '//trim(itemName))
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
              exit
            endif
          endif
        enddo
        if (.not.ismatch) then
          write(message,'(A)')  trim(name)//' did not include'
          call MOSSCO_MessageAdd(message, ' '//trim(itemName))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif
      endif

      call ESMF_StateGet(importState, itemName=trim(itemName), &
        itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      k = index(trim(itemName), '_z_velocity_')
      if (k < 1 .and. createVelocity) then
        k = index(itemNameList(i),'_in_water')
        if (k > 0) then
          wsItemName = trim(itemName(1:k)//'z_velocity_in_water')
          call ESMF_StateGet(importState, trim(wsItemName), itemType=wsItemType, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (wsItemType == ESMF_STATEITEM_NOTFOUND .and. itemType == ESMF_STATEITEM_FIELD) then
            call MOSSCO_StateGetFieldList(importState, fieldList,  &
              itemSearch=trim(itemName), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            field = ESMF_FieldEmptyCreate(name=trim(wsItemName), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call MOSSCO_FieldCopy(field, fieldList(1), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call MOSSCO_FieldInitialize(field, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
              write(message,'(A)')  trim(name)//' will transport auto-generated'
              call MOSSCO_MessageAdd(message, ' '//trim(wsItemName))

            call ESMF_FieldSet(field, name=trim(wsItemName), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

              write(message,'(A)')  trim(name)//' will transport auto-generated'
              call MOSSCO_MessageAdd(message, ' '//trim(wsItemName))

            call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

              write(message,'(A)')  trim(name)//' will transport auto-generated'
              call MOSSCO_MessageAdd(message, ' '//trim(wsItemName))

            call ESMF_StateAddReplace(importState, (/field/), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            write(message,'(A)')  trim(name)//' will transport auto-generated'
            call MOSSCO_MessageAdd(message, ' '//trim(wsItemName))

            call link_field_in_transport_fieldbundle(importState, trim(wsItemName), &
              wsFieldBundle, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          elseif (wsItemType == ESMF_STATEITEM_NOTFOUND &
            .and. itemType == ESMF_STATEITEM_FIELDBUNDLE) then
            write(message,'(A)') '  not implemented, velocity creation for fieldBundle'
            call MOSSCO_MessageAdd(message,' '//trim(itemName))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
            cycle
          endif
        endif
      endif

      if (itemType /= ESMF_STATEITEM_FIELD .and. itemType /= ESMF_STATEITEM_FIELDBUNDLE) cycle

      write(message,'(A)')  trim(name)//' will transport '
      call MOSSCO_MessageAdd(message, ' '//trim(itemName))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      k = index(trim(itemName), '_z_velocity_')
      if (k > 0) then
        if (itemType == ESMF_STATEITEM_FIELD) then
          call link_field_in_transport_fieldbundle(importState, trim(itemName), &
            wsFieldBundle, rc=localrc)
        else
          call link_fieldBundle_in_transport_fieldbundle(importState, trim(itemName), &
            wsFieldBundle, rc=localrc)
        endif
      else
        if (itemType == ESMF_STATEITEM_FIELD) then
          call link_field_in_transport_fieldbundle(importState, trim(itemName), &
            concFieldBundle, rc=localrc)
        else
          call link_fieldBundle_in_transport_fieldbundle(importState, trim(itemName), &
            concFieldBundle, rc=localrc)
        endif
      endif
    enddo

  end subroutine link_fields_in_transport_fieldbundle

#undef  ESMF_METHOD
#define ESMF_METHOD "link_field_in_transport_fieldbundle"
  subroutine  link_field_in_transport_fieldbundle(importState, itemName, exportFieldBundle, rc)

    type(ESMF_State), intent(in)          :: importState
    character(len=*), intent(in)          :: itemName
    type(ESMF_FIELDBUNDLE), intent(inout) :: exportFieldBundle
    integer, intent(out), optional        :: rc

    integer                     :: localrc, rc_
    integer(ESMF_KIND_I4)       :: fieldCount
    character (len=ESMF_MAXSTR) :: message, name
     type(ESMF_Field)           :: importField, field
    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_Grid)             :: importGrid, exportGrid
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_GeomType_Flag)    :: exportGeomType, importGeomType

    if (present(rc)) rc = ESMF_SUCCESS

    write(name,'(A)') 'transport_connector'

    call ESMF_StateGet(importState, trim(itemName), itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELD) return

    call ESMF_StateGet(importState, trim(itemName), importField, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(importField, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) return

    call ESMF_FieldGet(importField, geomType=importGeomType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (importGeomType == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(importField, grid=importGrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldBundleGet(exportFieldBundle, trim(itemName), fieldCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (fieldCount > 0) return

    call ESMF_FieldBundleGet(exportFieldBundle, geomType=exportGeomType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (exportGeomType == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldBundleGet(exportFieldBundle, grid=exportGrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (exportGeomType == ESMF_GEOMTYPE_GRID .and. importGeomType == ESMF_GEOMTYPE_GRID &
      .and. (importGrid == exportGrid)) then
      call ESMF_FieldBundleAdd(exportFieldBundle, (/importField/), multiflag=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' linked for transport field'
      call MOSSCO_FieldString(importField, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    else
      field = ESMF_FieldEmptyCreate(name=trim(itemName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldBundleAdd(exportfieldBundle,(/field/), multiflag=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' created for transport empty field'
      call MOSSCO_FieldString(field, message, rc=localrc)
    endif

  end subroutine link_field_in_transport_fieldbundle

#undef  ESMF_METHOD
#define ESMF_METHOD "link_fieldbundle_in_transport_fieldbundle"
  subroutine  link_fieldbundle_in_transport_fieldbundle(importState, itemName, exportFieldBundle, rc)

    type(ESMF_State), intent(in)          :: importState
    character(len=*), intent(in)          :: itemName
    type(ESMF_FIELDBUNDLE), intent(inout) :: exportFieldBundle
    integer, intent(out), optional        :: rc

    integer                     :: localrc, rc_
    integer(ESMF_KIND_I4)       :: i, itemCount
    character (len=ESMF_MAXSTR) :: message, name
    type(ESMF_Field)            :: field
    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_Grid)             :: importGrid, exportGrid
    type(ESMF_Field), allocatable :: fieldList(:)
    type(ESMF_FieldBundle)      :: fieldBundle
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_GeomType_Flag)    :: exportGeomType, importGeomType

    if (present(rc)) rc = ESMF_SUCCESS

    write(name,'(A)') 'transport_connector'

    call ESMF_FieldBundleGet(exportFieldBundle, geomType=exportGeomType, rc=localrc)
    if (exportGeomType == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldBundleGet(exportFieldBundle, grid=exportGrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(importState, trim(itemName), itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELDBUNDLE) return

    call ESMF_StateGet(importState, trim(itemName), fieldBundle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldBundleGet(fieldBundle, fieldCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount <= 0) return

    allocate(fieldList(itemCount))

    call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, itemCount

      call ESMF_FieldGet(fieldList(i), status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) return

      call ESMF_FieldGet(fieldList(i), geomType=importGeomType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (importGeomType == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(fieldList(i), grid=importGrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      if (importGeomType == ESMF_GEOMTYPE_GRID .and. exportGeomType == ESMF_GEOMTYPE_GRID &
        .and. (importGrid == exportGrid)) then
        call ESMF_FieldBundleAdd(exportFieldBundle,(/fieldList(i)/),multiflag=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' linked for transport field'
        call MOSSCO_FieldString(fieldList(i) , message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        field = ESMF_FieldEmptyCreate(name=trim(itemName), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleAdd(exportfieldBundle,(/field/),multiflag=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' created for transport empty field'
        call MOSSCO_FieldString(field, message, rc=localrc)
      endif
    enddo

  end subroutine link_fieldbundle_in_transport_fieldbundle


end module transport_connector
