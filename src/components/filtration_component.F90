!> @brief Implementation of an ESMF/MOSSCO component for filtration
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2015, 2016 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, HZG

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "filtration_component.F90"

#define RANGE2D lbnd(1):ubnd(1),lbnd(2):ubnd(2)
#define RANGE3D lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)

module filtration_component

  use esmf
  use mossco_variable_types
  use mossco_state
  use mossco_strings
  use mossco_component
  use mossco_grid
  use mossco_attribute
  use mossco_config

  implicit none

  private
  public :: SetServices

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  !> Provide an ESMF compliant SetServices routine, which defines
  !! the entry points for Init/Run/Finalize
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer              :: localrc

    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
    subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: localrc

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR)  :: name, message, configfilename
    type(ESMF_Time)         :: currTime
    integer(ESMF_KIND_I4)   :: localrc, rank, otherCount, i
    type(ESMF_Field)        :: field
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_Config)       :: config
    logical                 :: configIsPresent, fileIsPresent, labelIsPresent
    real(ESMF_KIND_R8)      :: mussel_mass, minimumFoodFlux

    character(len=ESMF_MAXSTR)  :: filterSpecies, xVelocity, yVelocity
    character(len=ESMF_MAXSTR), allocatable  :: filterSpeciesList(:), itemNameList(:)

    rc = ESMF_SUCCESS

    ! Provide default values for all parameters that could be set in the
    ! component's configuration file
    rank = 3 ! Default provide flux_in_water
    ! Taken from Rijsgaard 2001
    minimumFoodFlux  = 0.6166697552  ! mmol C s-1 m-3, equiv to 20 mg C
    filterSpecies = 'phytoplankton' ! Main variable to filter
    xVelocity = 'x_velocity'
    yVelocity = 'y_velocity'
    mussel_mass = 0.6 ! g DW / individual

    !! Make sure that a local clock exists, and that the call to this procedure
    !! is written to the log file
    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState,  exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Check whether there is a config in memory and load it
    call ESMF_GridCompGet(gridComp, configIsPresent=configIsPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (configIsPresent) then
      call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      config=ESMF_ConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    !! Check whether there is a local config file load it
    configfilename=trim(name)//'.cfg'
    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (fileIsPresent) then

      write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigFindLabel(config, label='mass:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (labelIsPresent) then
        call ESMF_ConfigGetAttribute(config, mussel_mass, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,ES10.3)') trim(name)//' found mass:', mussel_mass
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call ESMF_ConfigFindLabel(config, label='minimum_food_flux:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (labelIsPresent) then
        call ESMF_ConfigGetAttribute(config, minimumFoodFlux, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' found minimum_food_flux:'
        write(message,'(A,ES10.3)') trim(message), minimumFoodFlux
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call ESMF_ConfigFindLabel(config, label='filter:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (labelIsPresent) then
        call ESMF_ConfigGetAttribute(config, filterSpecies, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' found filter:'
        write(message,'(A)') trim(message)//' '//trim(filterSpecies)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call MOSSCO_ConfigGetList(config, 'other:', filterSpeciesList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(filterSpeciesList)) then
        call MOSSCO_AttributeSetList(gridComp, 'filter_other_species', filterSpeciesList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        otherCount = size(filterSpeciesList)

        write(message,'(A)') trim(name)//' found other:'
        call MOSSCO_MessageAdd(message, filterSpeciesList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        otherCount = 0
      endif

    endif

    call ESMF_AttributeSet(gridComp, 'filter_species', trim(filterSpecies), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'mussel_mass', mussel_mass, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'minimum_food_flux', minimumFoodFlux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! Create a list to hold the names of the item to filter, the names of the
    ! velocity fields, and the names of other items to co-filter
    call MOSSCO_Reallocate(itemNameList, 4 + otherCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    itemNameList(1) = trim(xVelocity)
    itemNameList(2) = trim(yVelocity)
    itemNameList(3) = 'mussel_abundance'
    itemNameList(4) = trim(filterSpecies)
    do i = 1, othercount
      itemNameList(4 + i) = trim(filterSpeciesList(i))
    enddo

    do i = 1, ubound(itemNameList,1)
      ! Create import state for co-filtration fields
      field = ESMF_FieldEmptyCreate(name=trim(itemNameList(i))//'_in_water', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (i == 1)  call ESMF_AttributeSet(field, 'units', 'm-3', rc=localrc)
      if (i == 4)  call ESMF_AttributeSet(field, 'units', 'mmol m-3', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message, '(A)') trim(name)//' created empty field'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAddReplace(importState, (/field/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    !> Create export states, add diagnostic variables
    !itemNameList(3) = 'layer_height_in_water'
    do i = 4, ubound(itemNameList,1)
      !> Create export states for filter and co-filter items
      if (i < 4) then
        field = ESMF_FieldEmptyCreate(name=trim(itemNameList(i)), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateAddReplace(exportState, (/field/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message, '(A)') trim(name)//' created empty field'

      else
        fieldBundle = ESMF_FieldBundleCreate(name=trim(itemNameList(i))//'_flux_in_water', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        field = ESMF_FieldEmptyCreate(name=trim(itemNameList(i))//'_flux_in_water', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleAdd(fieldBundle, (/field/), multiflag=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AttributeSet(fieldBundle, 'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateAddReplace(exportState, (/fieldBundle/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message, '(A)') trim(name)//' created bundled empty field'
      endif
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (i == 4)  call ESMF_AttributeSet(field, 'units', 'mmol m-3 s-1', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    enddo

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR)  :: name, message, filterSpecies
    character(ESMF_MAXSTR), allocatable  :: filterSpeciesList(:)
    character(ESMF_MAXSTR)  :: foreignGridFieldName
    type(ESMF_Time)         :: currTime

    type(ESMF_Grid)             :: grid
    type(ESMF_Field)            :: field
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    integer(ESMF_KIND_I4)       :: localrc, i, rank, gridRank
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    integer(ESMF_KIND_I4)                   :: itemCount
    real(ESMF_KIND_R8)                      :: mussel_mass, minimumFoodFlux

    rc = ESMF_SUCCESS
    rank = 3

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState,  exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
      value=foreignGridFieldName, defaultValue='none',rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (trim(foreignGridFieldName) == 'none') then
      !! Create grids on your own, for now this template code just exits with
      !! an error message
      write(message,'(A)') trim(name)//' needs a grid to operate. Specify this in your coupling configuration.'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(importState, trim(foreignGridFieldName), itemType=itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType == ESMF_STATEITEM_NOTFOUND) then
      write(message,'(A)') trim(name)//' could not find item '//trim(foreignGridFieldName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (itemType /= ESMF_STATEITEM_FIELD) then
      write(message,'(A)') trim(name)//' obtained item '//trim(foreignGridFieldName)//', which is not a field'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(importState, trim(foreignGridFieldName), field=field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
      write(message,'(A)') trim(name)//' cannot use empty field '//trim(foreignGridFieldName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridGet(grid, rank=gridRank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (gridRank /= 3) then
      write(message,'(A)') trim(name)//' cannot deal with grid rank other than 3 from field '//trim(foreignGridFieldName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    write(message,'(A)') trim(name)//' obtained grid from '
    call MOSSCO_FieldString(field, message)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    ! Get all fields that are empty and provide them with a grid
    call MOSSCO_StateGetFieldList(exportState, fieldList, &
      fieldStatus=ESMF_FIELDSTATUS_EMPTY, fieldCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, itemCount
      call ESMF_FieldEmptySet(fieldList(i), grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      write(message,'(A)') trim(name)//' added grid to '
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

    ! Get all fields that are gridset and complete them with a typekind
    call MOSSCO_StateGetFieldList(exportState, fieldList, &
      fieldStatus=ESMF_FIELDSTATUS_GRIDSET, fieldCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, itemCount
      call ESMF_FieldEmptySet(fieldList(i), grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_FieldInitialize(field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' completed and initialized with zero '
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

    ! get parameters from the gridComp, we can safely assume that they are present

    call ESMF_AttributeGet(gridComp, name='minimum_food_flux', &
      value=minimumFoodFlux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A,ES9.3)') trim(name)//' minimum food flux is ', minimumFoodFlux
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_AttributeGet(gridComp, name='mussel_mass', &
      value=mussel_mass, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A,ES9.3)') trim(name)//' mussel mass is ', mussel_mass
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_AttributeGet(gridComp, name='filter_species', &
      value=filterSpecies, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(name)//' species to filtrate is '//trim(filterSpecies)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call MOSSCO_AttributeGetList(gridComp, 'filter_other_species', filterSpeciesList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(filterSpeciesList)) then
      do i = lbound(filterSpeciesList,1), ubound(filterSpeciesList,1)
        write(message,'(A)') trim(name)//' filter also '//trim(filterSpeciesList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo
    endif

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP2

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4)      :: localrc
    type(ESMF_Time)            :: currTime
    character(len=ESMF_MAXSTR) :: name

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState,  exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Here omes your restart code, which in the simplest case copies
    !> values from all fields in importState to those in exportState

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine ReadRestart

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR):: name, message
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: currTime, stopTime
    type(ESMF_TimeInterval) :: timeStep

    real(ESMF_KIND_R8),allocatable               :: fractionalLossRate(:,:,:)
    real(ESMF_KIND_R8),allocatable               :: filtrationRate(:,:,:)
    real(ESMF_KIND_R8),allocatable               :: maximumFiltrationRate(:,:,:)
    real(ESMF_KIND_R8),allocatable               :: foodFlux(:,:,:)
    real(ESMF_KIND_R8),allocatable               :: foodFluxFactor(:,:,:)
    real(ESMF_KIND_R8),pointer,dimension(:,:,:)  :: abundance, lossRate
    real(ESMF_KIND_R8),pointer,dimension(:,:,:)  :: concentration, velocity
    !real(ESMF_KIND_R8),pointer,dimension(:,:,:)  :: layer_height, water_depth_at_interface
    logical, allocatable, dimension(:,:,:)       :: mask
    type(ESMF_Field)        :: field
    integer(ESMF_KIND_I4)   :: localrc, i, rank, otherCount, fieldCount, j
    real(ESMF_KIND_R8)      :: minimumFoodFlux, mussel_mass
    real(ESMF_KIND_R8)      :: missingValue, mmolPermg, mgPermmol
    integer(ESMF_KIND_I4), allocatable   :: ubnd(:), lbnd(:)
    type(ESMF_Field), allocatable        :: fieldList(:)

    character(len=ESMF_MAXSTR)  :: filterSpecies, fluxName, creatorName
    character(len=ESMF_MAXSTR), allocatable  :: filterSpeciesList(:)
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_StateItem_Flag)   :: itemType

    mmolPermg = 0.03083348776
    mgPermmol = 1./mmolPermg
    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState,  exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! get parameters from the importState, we can safely assume that they are present

    call ESMF_AttributeGet(gridComp, name='minimum_food_flux', &
      value=minimumFoodFlux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (minimumFoodFlux <= 0.0) then
      write(message,'(A)') trim(name)//' invalid minimum food flux'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_AttributeGet(gridComp, name='mussel_mass', &
      value=mussel_mass, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(gridComp, name='filter_species', &
      value=filterSpecies, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch=trim(filterSpecies)//'_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount /= 1) then
      write(message,'(A)') trim(name)//' did not find complete field with name '//trim(filterSpecies)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call MOSSCO_StateLog(importState)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    allocate(ubnd(3), stat=localrc)
    allocate(lbnd(3), stat=localrc)

    call ESMF_FieldGet(fieldList(1), farrayPtr=concentration, exclusiveUbound=ubnd, exclusiveLbound=lbnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! Get mussel abundance
    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='mussel_abundance_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount /= 1) then
      write(message,'(A)') trim(name)//' did not find complete field with name mussel_abundance_in_water'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call MOSSCO_StateLog(importState)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rank /= 3) then
      write(message,'(A)') trim(name)//' received non-rank 3 field'
      call MOSSCO_FieldString(fieldList(1), message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    allocate(ubnd(rank), stat=localrc)
    allocate(lbnd(rank), stat=localrc)

    call ESMF_FieldGet(fieldList(1), farrayPtr=abundance, exclusiveUbound=ubnd, exclusiveLbound=lbnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_FieldGetMissingValue(fieldList(1), missingValue, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(mask)) deallocate(mask)
    allocate(mask(ubnd(1)-lbnd(1)+1, ubnd(2)-lbnd(2)+1, ubnd(3)-lbnd(3)+1), stat=localrc)
    mask(RANGE3D) = (abundance(RANGE3D) /= missingValue)
    mask(RANGE3D) = (abundance(RANGE3D) > 0 .and. mask(RANGE3D))

    if (.not.any(mask)) then
      write(message,'(A)') trim(name)//' found no abundance'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (allocated(mask)) deallocate(mask)
      return
    endif

    mask(RANGE3D) = (concentration(RANGE3D) > 0 .and. mask(RANGE3D))
    if (.not.any(mask)) then
      write(message,'(A)') trim(name)//' found no concentration at abundance'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (allocated(mask)) deallocate(mask)
      return
    endif

    write(fluxName,'(A)') trim(filterSpecies)//'_flux_in_water'

    ! Get flux species, be careful to look at the creator attribute to choose
    ! the right one
    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch=trim(fluxName), fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, fieldCount
      call ESMF_AttributeGet(fieldList(i), 'creator', creatorName, defaultValue='none', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (trim(creatorName) /= trim(name)) then
        fieldCount=fieldCount - 1
        cycle
      endif
      field=fieldList(i)
    enddo

    if (fieldCount /= 1) then
      write(message,'(A)') trim(name)//' did not find complete field with name '//trim(fluxName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call MOSSCO_StateLog(importState)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGetBounds(field, exclusiveUbound=ubnd, exclusiveLbound=lbnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, farrayPtr=lossRate,  rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! Obtain the water transport in m3 s-1 from velocity and grid properties
    call MOSSCO_StateGetVelocity(importState, velocity, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! New core of the model (9 March 2016)
    if (.not.allocated(maximumFiltrationRate)) allocate(maximumFiltrationRate(RANGE3D), stat=localrc)
    if (.not.allocated(filtrationRate)) allocate(filtrationRate(RANGE3D), stat=localrc)
    if (.not.allocated(foodFlux)) allocate(foodFlux(RANGE3D), stat=localrc)
    if (.not.allocated(foodFluxFactor)) allocate(foodFluxFactor(RANGE3D), stat=localrc)
    if (.not.allocated(fractionalLossRate)) allocate(fractionalLossRate(RANGE3D), stat=localrc)

    ! The maximum filtration rate is taken from Bayne et al. 1993, who found
    ! an empirical relationship between TPM and filtration rate, given in
    ! units of mg PhyC h-1 (300 mg)-1 Mytilus; we convert mmol of concentration to
    ! mg for this relationship
    maximumFiltrationRate(RANGE3D) = .83 * (concentration(RANGE3D) * mgPermmol) ** .983

    ! Convert unit of maximumFiltrationRate to mg phyC mg-1 Mytilus s-1
    maximumFiltrationRate(RANGE3D) = maximumFiltrationRate(RANGE3D) / 300.0 / 3600.0

    ! The food flux in mmol s-1 is the product of clearance rate
    ! in m3 s-1 and concentration mmol phyC m-3
    ! clearance rate is identical to transport and thus numerically identical to
    ! velocity (@todo: let Richard check this)
    foodFlux(RANGE3D) = velocity(RANGE3D) * concentration(RANGE3D)

    ! The food flux factor is a threshold function that limits
    ! the supply at low rates
    foodFluxFactor(RANGE3D) = 1
    where(foodFluxFactor(RANGE3D) < minimumFoodFlux)
      foodFluxFactor(RANGE3D) = foodFlux(RANGE3D) / minimumFoodFlux
    endwhere

    ! The filtration rate is in mg PhyC mg-1 Mytilus s-1 and
    ! is composed of a concentration-dependent maximum rate and
    ! a supply-dependent food flux factor.
    filtrationRate(RANGE3D) = &
      maximumFiltrationRate(RANGE3D) * foodFluxFactor(RANGE3D)
    ! The loss rate is in mg PhyC m-3 s-1
    lossRate = &
    ! and is proportinal to mussel dry mass concentration, calculated
    ! from abundance m-3 times mussel_mass mg (600 mg default for Mytilus edulis)
      - abundance(RANGE3D) * mussel_mass &
    ! as well as filtration rate in
      * filtrationRate(RANGE3D) &
    ! and convert from mg to mmol
      * mmolPermg

    ! For co-filtration of other species, calculate the fractional loss rate
    where (concentration(RANGE3D) > 0)
      fractionalLossRate(RANGE3D) = lossRate(RANGE3D) / concentration(RANGE3D)
    endwhere

    write(message,'(A,ES10.3,A)') trim(name)//' is filtering up to ', &
        maxval(-lossRate(RANGE3D),mask=mask(RANGE3D)),' mmol m-3 s-1'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call MOSSCO_AttributeGetList(gridComp, 'filter_other_species', filterSpeciesList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(filterSpeciesList)) then
      otherCount = size(filterSpeciesList)
    else
      otherCount = 0
    endif

    do i = 1, otherCount
      write(fluxName,'(A)') trim(filterSpeciesList(i))//'_flux_in_water'

      ! Get cofiltered flux species, be careful to look at the creator attribute to choose
      ! the right one
      call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
        itemSearch=trim(fluxName), fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      do j=1, fieldCount
        call ESMF_AttributeGet(fieldList(j), 'creator', creatorName, defaultValue='none', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (trim(creatorName) /= trim(name)) then
          fieldCount=fieldCount - 1
          cycle
        endif
        field=fieldList(j)
      enddo

      if (fieldCount /= 1) then
        write(message,'(A)') trim(name)//' did not find complete field with name '//trim(fluxName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call MOSSCO_StateLog(importState)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_FieldGet(field, farrayPtr=lossRate,  rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Get concentrations from import state
      call ESMF_StateGet(importState, trim(filterSpeciesList(i))//'_in_water', &
        itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemType /= ESMF_STATEITEM_FIELD) then
        write(message,'(A)') trim(name)//' did not find field '//trim(filterSpeciesList(i))//'_in_water'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_StateGet(importState, trim(filterSpeciesList(i))//'_in_water', field=field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
        write(message,'(A)') trim(name)//' received incomplete field'
        call MOSSCO_FieldString(field, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_FieldGet(field, farrayPtr=concentration,  rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      where(mask(RANGE3D))
        lossRate(RANGE3D) = fractionalLossRate(RANGE3D) * concentration(RANGE3D) ! mmol s-1 m-3
      endwhere

      write(message,'(A,ES10.3,A)') trim(name)//' is filtering up to ', &
          maxval(-lossRate(RANGE3D),mask=mask(RANGE3D)),' XXX s-1'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    enddo

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(filterSpeciesList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(mask)) deallocate(mask)
    if (allocated(lbnd)) deallocate(lbnd)
    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(foodFlux)) deallocate(foodFlux)
    if (allocated(foodFluxFactor)) deallocate(foodFluxFactor)
    if (allocated(maximumFiltrationRate)) deallocate(maximumFiltrationRate)
    if (allocated(filtrationRate)) deallocate(filtrationRate)
    if (allocated(fractionalLossRate)) deallocate(fractionalLossRate)

    !! This component has no do loop over an internal timestep, it is advanced with the
    !! timestep written into its local clock from a parent component
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, currTime=currTime, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    timeStep=stopTime-currTime
    if (stopTime>currTime) then
      call ESMF_ClockAdvance(clock, timeStep=timeStep, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR):: name
    type(ESMF_Time)       :: currTime
    integer(ESMF_KIND_I4) :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState,  exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> @todo
    ! Insert here your finalization code
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize
end module filtration_component
