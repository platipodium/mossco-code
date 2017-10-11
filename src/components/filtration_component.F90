!> @brief Implementation of an ESMF/MOSSCO component for filtration, both
!> from organisms that are suspended in the water column as well as from
!> organisms that reside on the sea floor
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, HZG

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

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module filtration_component

  use esmf
  use mossco_variable_types
  use mossco_state
  use mossco_strings
  use mossco_component
  use mossco_grid
  use mossco_attribute
  use mossco_config
  use mossco_parameter

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
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR)  :: name, message, configfilename, itemName
    type(ESMF_Time)         :: currTime
    integer(ESMF_KIND_I4)   :: localrc, rank, otherCount, i, j, diagCount
    type(ESMF_Field)        :: field
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_Config)       :: config
    logical                 :: configIsPresent, fileIsPresent, labelIsPresent
    real(ESMF_KIND_R8)      :: musselMass, minimumFoodFlux, formFactor
    real(ESMF_KIND_R8)      :: maximumRelativeChange
    real(ESMF_KIND_R8)      :: musselLengthScale, roughnessLength

    character(len=ESMF_MAXSTR)  :: xVelocity, yVelocity
    character(len=ESMF_MAXSTR), allocatable  :: filterSpeciesList(:,:), itemNameList(:)
    character(len=ESMF_MAXSTR), allocatable  :: diagNameList(:), filterSpecies(:)

    type(MOSSCO_ParameterType), dimension(10) :: parameters

    rc = ESMF_SUCCESS

    ! Provide default values for all parameters that could be set in the
    ! component's configuration file
    parameters(1)%name='mussel_mass'
    parameters(1)%label='mussel_mass'
    parameters(1)%typeKind=ESMF_TYPEKIND_R8
    otherCount = 0
    !parameters(1)%unit='g'

    musselMass = 0.6 ! g DW / individual

    ! Numeric limits
    maximumRelativeChange = 0.1

    !! Make sure that a local clock exists, and that the call to this procedure
    !! is written to the log file
    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState,  exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !! Check whether there is a config in memory and load it
    call ESMF_GridCompGet(gridComp, configIsPresent=configIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (configIsPresent) then
      call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    else
      config=ESMF_ConfigCreate(rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    !! Check whether there is a local config file load it
    configfilename=trim(name)//'.cfg'
    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (fileIsPresent) then

      write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, label='maximum_relative_change', &
        value=maximumRelativeChange, isPresent=labelIsPresent, &
        defaultValue=maximumRelativeChange, rc = localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)
      if (.not. labelIsPresent) write(message,'(A)') trim(message)//' (default) '
      write(message,'(A,F4.1,A)') trim(message)//' maximum relative change is ',100*maximumrelativeChange, '%'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call MOSSCO_ConfigGet(config, label='filter', value=filterSpecies, &
        isPresent=labelIsPresent, rc = localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (.not.allocated(filterSpecies)) then
        allocate(filterSpecies(2))
        filterSpecies(1) = 'phytoplankton' ! Main variable to filter
        filterSpecies(2) = 'detritus'      ! What the main variable is converted to
      endif

      write(message,'(A)') trim(name)//' filters '//trim(filterSpecies(1))
      if (ubound(filterSpecies,1) > 1) then
        write(message,'(A)') trim(message)//' => '//trim(filterSpecies(2))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call MOSSCO_ConfigGet(config, 'other', filterSpeciesList, &
        isPresent=labelIsPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (allocated(filterSpeciesList)) then
        call MOSSCO_AttributeSet(gridComp, 'filter_other_species', filterSpeciesList, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        otherCount = ubound(filterSpeciesList,1)

        do j=1,otherCount
          write(message,'(A)') trim(name)//' co-filters '
          call MOSSCO_MessageAdd(message, ' '//trim(filterSpeciesList(j,1)), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (filterSpeciesList(j,2) /= '') then
            call MOSSCO_MessageAdd(message,' => '//trim(filterSpeciesList(j,2)), rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          endif

          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        enddo
      else
        otherCount = 0
      endif

      call MOSSCO_ConfigGet(config, 'diag', diagNameList, &
        isPresent=labelIsPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (allocated(diagNameList)) then
        write(message,'(A)') trim(name)//' found diagnostic:'
        call MOSSCO_MessageAdd(message, diagNameList, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      ! else ! provide default diagnostic names
      !   call MOSSCO_Reallocate(diagNameList, 7, keep=.false., rc=localrc)
      !   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      !     call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !
      !   diagNameList(1) = 'mussel_abundance_in_water'
      !   diagNameList(2) = 'layer_height_in_water'
      !   diagNameList(5) = 'maximum_filtration_rate'
      !   diagNameList(6) = 'fractional_filtration_rate'

      endif

      if (allocated(diagNameList)) then
        call MOSSCO_AttributeSet(gridComp, 'diagnostic_variables', diagNameList, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    else
      if (.not.allocated(filterSpecies)) then
        allocate(filterSpecies(2))
        filterSpecies(1) = 'phytoplankton' ! Main variable to filter
        filterSpecies(2) = 'detritus'      ! What the main variable is converted to
      endif
    endif

    call MOSSCO_AttributeSet(gridComp, 'filter_species', filterspecies, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(gridComp, 'maximum_relative_change', maximumRelativeChange, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(gridComp, 'mussel_mass', musselMass, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    ! Create a list to hold the names of the item to filter, the names of the
    ! velocity fields and shear stress, and the names of other items to co-filter
    call MOSSCO_Reallocate(itemNameList, 3 + otherCount, keep=.false., rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    itemNameList(1) = 'mussel_abundance'  ! '_at_soil_surface'
    itemNameList(2) = 'mussel_abundance'  ! '_at_water_surface'

    itemNameList(3) = trim(filterSpecies(1))
    do i = 1, othercount
      itemNameList(3 + i) = trim(filterSpeciesList(i,1))
    enddo

    do i = 1, ubound(itemNameList,1)

      itemName = trim(itemNameList(i))
      if (i > 2) then
        ! items can be specified with or without the '_in_water' postfix
        ! remove this if present
        j = index(itemName,'_in_water')
        if (j>1) itemName = itemName(1:j-1)
        field = ESMF_FieldEmptyCreate(name=trim(itemName)//'_in_water', rc=localrc)
      elseif (i == 1) then
        field = ESMF_FieldEmptyCreate(name=trim(itemName)//'_at_water_surface', rc=localrc)
      elseif (i == 2) then
        field = ESMF_FieldEmptyCreate(name=trim(itemName)//'_at_soil_surface', rc=localrc)
      endif
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (i == 1 .or. i == 2)  call ESMF_AttributeSet(field, 'units', 'm-2', rc=localrc)
      if (i > 2)  call ESMF_AttributeSet(field, 'units', 'mmol m-3', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' created for import empty field '
      call MOSSCO_FieldString(field, message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAddReplace(importState, (/field/),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    enddo

    diagCount = 0
    if (allocated(diagNameList)) diagCount = size(diagNameList)

    call MOSSCO_Reallocate(diagNameList, ubound(itemNameList,1) + diagCount, &
      keep=.true., rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    diagNameList(ubound(diagNameList,1)-size(itemNameList)+1:ubound(diagNameList,1)) &
        = itemNameList(:)

    !> Add the optional products
    if (filterSpecies(2) /= '') then
      call MOSSCO_Reallocate(diagNameList, ubound(diagNameList,1) + 1, keep=.true., rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      diagNameList(ubound(diagNameList,1))=trim(filterSpecies(2))
    endif
    do i=lbound(filterSpeciesList,1), ubound(filterSpeciesList,1)
      if (filterSpeciesList(i,2) == '') cycle
      call MOSSCO_Reallocate(diagNameList, ubound(diagNameList,1) + 1, keep=.true., rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      diagNameList(ubound(diagNameList,1))=trim(filterSpeciesList(i,2))
    enddo

    !> Create export states for all variables above index 5,  add diagnostic variables
    do i = 3, ubound(diagNameList,1)
      !> Create export states for diagnostic, filter and co-filter items
      if (i < ubound(diagNameList,1)-size(itemNameList) - 1) then
        field = ESMF_FieldEmptyCreate(name=trim(diagNameList(i)), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_StateAddReplace(exportState, (/field/),rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message, '(A)') trim(name)//' created for export empty field'

      else
        fieldBundle = ESMF_FieldBundleCreate(name=trim(diagNameList(i))//'_flux_in_water', rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        field = ESMF_FieldEmptyCreate(name=trim(diagNameList(i))//'_flux_in_water', rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        !> @todo check units for co-filtered and filter species
        call ESMF_AttributeSet(field, 'units', 'mmol m-3 s-1', rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldBundleAdd(fieldBundle, (/field/), multiflag=.true., rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AttributeSet(fieldBundle, 'creator', trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_StateAddReplace(exportState, (/fieldBundle/),rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message, '(A)') trim(name)//' created bundled empty field'
      endif
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_FieldString(field, message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    enddo

    call MOSSCO_Reallocate(diagNameList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR)  :: name, message
    character(ESMF_MAXSTR), allocatable  :: filterSpeciesList(:,:), filterSpecies(:)
    character(ESMF_MAXSTR)  :: foreignGridFieldName
    type(ESMF_Time)         :: currTime

    type(ESMF_Grid)             :: grid, grid2
    type(ESMF_Field)            :: field
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    integer(ESMF_KIND_I4)       :: localrc, i, rank, gridRank
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:), diagNameList(:)
    character(len=ESMF_MAXSTR)              :: itemName
    integer(ESMF_KIND_I4)                   :: itemCount
    real(ESMF_KIND_R8)                      :: musselMass, minimumFoodFlux

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState,  exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
      value=foreignGridFieldName, defaultValue='none', rc=localrc)
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
    call MOSSCO_FieldString(field, message, rc=localrc)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    ! Get all fields that are empty and provide them with a grid
    call MOSSCO_StateGetFieldList(exportState, fieldList, &
      fieldStatus=ESMF_FIELDSTATUS_EMPTY, fieldCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, itemCount
      call ESMF_FieldGet(fieldList(i), name=itemName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldEmptySet(fieldList(i), grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' added grid to '
      !> @todo
      call MOSSCO_FieldString(fieldList(i), message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

    ! Get all fields that are gridset and complete them with a typekind
    call MOSSCO_StateGetFieldList(exportState, fieldList, &
      fieldStatus=ESMF_FIELDSTATUS_GRIDSET, fieldCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, itemCount
      call ESMF_FieldEmptyComplete(fieldList(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_FieldInitialize(fieldList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' completed and initialized with zero '
      call MOSSCO_FieldString(fieldList(i), message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

    ! get parameters from the gridComp, we can safely assume that they are present

    call ESMF_AttributeGet(gridComp, name='mussel_mass', &
      value=musselMass, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A,ES10.3)') trim(name)//' mussel mass is ', musselMass
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call MOSSCO_AttributeGet(gridComp, 'filter_species', filterSpecies, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(name)//' filtrates '//trim(filterSpecies(1))
    if (ubound(filterSpecies,1) == 2) then
      write(message,'(A)') trim(message)//' => '//trim(filterSpecies(2))
    endif
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call MOSSCO_AttributeGet(gridComp, 'filter_other_species', filterSpeciesList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(filterSpeciesList)) then
      do i = lbound(filterSpeciesList,1), ubound(filterSpeciesList,1)
        write(message,'(A)') trim(name)//' filtrates also '//trim(filterSpeciesList(i,1))
        if ((ubound(filterSpeciesList,2) == 2) .and. (filterSpeciesList(i,2) /= '')) then
          write(message,'(A)') trim(message)//' => '//trim(filterSpeciesList(i,2))
        endif
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo
    endif

    call MOSSCO_AttributeGet(gridComp, 'diagnostic_variables', diagNameList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(diagNameList)) then
      do i = lbound(diagNameList,1), ubound(diagNameList,1)
        write(message,'(A)') trim(name)//' calculates diagnostic '//trim(diagNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo
    endif

    call MOSSCO_Reallocate(diagNameList, 0, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(filterSpeciesList, 0, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, rc=localrc)
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

    call MOSSCO_CompExit(gridComp, rc=localrc)
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

    real(ESMF_KIND_R8), pointer, dimension(:,:)    :: abundanceAtSurface => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:)    :: abundanceAtSoil => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: abundance => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: lossRate => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: layerHeight => null()
    real(ESMF_KIND_R8), allocatable, dimension(:,:,:)  :: layerWeight
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: maximumFiltrationRate=> null(), fractionalLossRate=> null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: concentration=> null()
    real(ESMF_KIND_R8), allocatable, dimension(:,:):: depthAtSoil

    logical, allocatable, dimension(:,:,:)       :: mask
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: grid
    integer(ESMF_KIND_I4)   :: localrc, i, rank, otherCount, fieldCount, j, k
    real(ESMF_KIND_R8)      :: musselMass, scaleFactor
    real(ESMF_KIND_R8)      :: maximumRelativeChange
    real(ESMF_KIND_R8)      :: integration_timestep, threshold
    real(ESMF_KIND_R8)      :: missingValue, mmolCPermgC, mgCPermmolC
    real(ESMF_KIND_R8)      :: mmolCPermgDW, mgDWPermmolC
    integer(ESMF_KIND_I4), allocatable   :: ubnd(:), lbnd(:)
    integer(ESMF_KIND_I4)                :: ubndZ(3), lbndZ(3)
    type(ESMF_Field), allocatable        :: fieldList(:)

    character(len=ESMF_MAXSTR)  :: fluxName, creatorName, string
    character(len=ESMF_MAXSTR), allocatable  :: filterSpeciesList(:,:), filterSpecies(:)
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    logical                     :: isSoil, isSurface
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8),parameter:: pi=3.141592653589793d0
    integer(ESMF_KIND_I8)       :: advanceCount
    real(ESMF_KIND_R8), parameter :: respiration_fraction=0.2

    rc = ESMF_SUCCESS

    !> Define all constants used in this routine
    !> the ratios mmolPermg and mgPermmol indicate the conversion from
    !> mole Carbon to total phytoplankton dry biomass; this is obtained by
    !> multiplying assuming that for each mole C there is a Redfield
    !> equivalent of N and P and that most of biomass is represented by sugar,
    !> i.e. <CH_2O>, such that O and H are added in these ratios, as well.
    !> Lastly, the unit mass of all these elements is weighted and summed.
    !> \textrm{POM DW} \approx M_{CNP} + 2M_{H} + M_{O}
    !>   = \frac{1}{106} (106 M_{C} + 16 M_{N} + M_{P}) + 2M_{H} + M_{O}
    !>   = (106*12,011 + 16*14,007 + 30,973)/106 + 2*1,008 + 15,999
    !>   = 32,4324622642 mg-DW mmol-C-1

    mgCpermmolC = 12.011
    mmolCpermgC = 1/mgCPermmolC
    mgDWPermmolC = 32.4324622642
    mmolCPermgDW = 1/mgDWPermmolC

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState,  exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, currTime=currTime, stopTime=stopTime, &
      advanceCount=advanceCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! get parameters from the importState, we can safely assume that they are present
    call ESMF_AttributeGet(gridComp, name='maximum_relative_change', &
      value=maximumRelativeChange, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (maximumRelativeChange <= 0.0 .or. maximumRelativeChange>=1.0) then
      write(message,'(A)') trim(name)//' invalid maximum relative change'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_AttributeGet(gridComp, name='mussel_mass', &
      value=musselMass, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (musselMass <= 0.0) then
      write(message,'(A)') trim(name)//' invalid mussel mass'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call MOSSCO_AttributeGet(gridComp, 'filter_species', filterSpecies, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch=trim(filterSpecies(1))//'_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount /= 1) then
      write(message,'(A)') trim(name)//' did not find complete field with name '//trim(filterSpecies(1))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call MOSSCO_StateLog(importState)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (allocated(ubnd)) deallocate(ubnd)
    allocate(ubnd(3), stat=localrc)
    if (allocated(lbnd)) deallocate(lbnd)
    allocate(lbnd(3), stat=localrc)

    call ESMF_FieldGet(fieldList(1), farrayPtr=concentration, exclusiveUbound=ubnd, &
      exclusiveLbound=lbnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! The default value for maximum integration timestep is 1 hour (3600 s)
    call ESMF_AttributeGet(fieldList(1), 'integration_timestep', integration_timestep, &
      defaultValue=3600.0D0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @todo get grid from specified grid variable
    call ESMF_FieldGet(fieldList(1), grid=grid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! Get layer height  to export
    call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
      itemSearch='layer_height_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), farrayPtr=layerHeight, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      allocate(layerHeight(RANGE3D), stat=localrc)
      layerHeight(RANGE3D)=1.0
    endif

    call MOSSCO_GridGetDepth(grid,  height=layerHeight,  rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.allocated(depthAtSoil)) allocate(depthAtSoil(RANGE2D), stat=localrc)
    depthAtSoil(RANGE2D) = sum(layerHeight, dim=3)

    ! Get mussel abundance to export
    call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
      itemSearch='mussel_abundance_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount > 1) then
      call ESMF_FieldGet(fieldList(1), farrayPtr=abundance, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_FieldGetMissingValue(fieldList(1), missingValue, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      allocate(abundance(RANGE3D), stat=localrc)
      abundance(RANGE3D) = 0.0
    endif

    ! Get mussel abundance to import
    nullify(abundanceAtSoil)
    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='mussel_abundance_at_soil_surface', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    isSoil = .false.
    if (fieldCount == 1) then
      isSoil = .true.
      call ESMF_FieldGet(fieldList(1), farrayPtr=abundanceAtSoil, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_FieldGetMissingValue(fieldList(1), missingValue, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      write(message,'(A)') trim(name)//' found no abundance at soil'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    endif

    ! Get mussel abundance to import
    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='mussel_abundance_at_water_surface', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)

    isSurface = .false.
    if (fieldCount == 1) then
      call ESMF_FieldGet(fieldList(1), farrayPtr=abundanceAtSurface, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      isSurface = .true.
    else
        write(message,'(A)') trim(name)//' found no abundance at surface'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    endif

    !> Create a loop here to enable exit/cycle statements (instead of goto) to cleanly
    !> deallocate on failure
    do k=1,1

      ! Return from this component if nothing can be done as there is no abundance
      if (.not.(isSurface .or. isSoil)) then
        write(message,'(A)') trim(name)//' found no abundance at all'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      if (isSurface) then
        !> @todo according to Krone, mussels are distributed up to a critical
        !> depth of 5 m from the surface, and 90% down to a depth of 2.5 m
        !> this is not implemented yet, but
        !> all mussels are soley applied to the surface layer

        threshold = 2.5
        call MOSSCO_WeightsFromSurface(height=layerHeight(RANGE3D), threshold=2.5D0, &
          weight=layerWeight, rc=localrc)
        !allocate(layerWeight(RANGE3D))
        !layerWeight = 0.0
        !layerWeight(RANGE2D,ubnd(3)) = threshold
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do i=lbnd(3), ubnd(3)
          where (layerHeight(RANGE2D,i) > 0 .and. layerWeight(RANGE2D,i) > 0)
            abundance(RANGE2D, i) = abundanceAtSurface(RANGE2D) &
            / layerWeight(RANGE2D,i) / threshold
          endwhere
        enddo
        if (allocated(layerWeight)) deallocate(layerWeight)

        !where (layerHeight(RANGE2D,ubnd(3)) > 0)
        !  abundance(RANGE2D,ubnd(3)) = abundanceAtSurface(RANGE2D) &
        !    / layerHeight(RANGE2D,ubnd(3))
        !endwhere
        write(message,'(A,ES10.3,A)') trim(name)//' max surface abundance is ', &
          maxval(abundanceAtSurface(RANGE2D)),' m-2'
          !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        write(message,'(A,ES10.3,A)') trim(name)//' min upper layer height is ', &
          minval(layerHeight(RANGE2D,ubnd(3))),' m'
          !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        write(message,'(A,ES10.3,A)') trim(name)//' max upper layer abundance is ', &
          maxval(abundance(RANGE2D,ubnd(3))),' m-3'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      if (isSoil) then
        where (layerHeight(RANGE2D,lbnd(3)) > 0)
          abundance(RANGE2D,lbnd(3)) = abundanceAtSoil(RANGE2D) &
            / layerHeight(RANGE2D,lbnd(3))
        endwhere
        write(message,'(A,ES10.3,A)') trim(name)//' max bottom abundance is ', &
            maxval(abundanceAtSoil(RANGE2D)),' m-2'
        !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        write(message,'(A,ES10.3,A)') trim(name)//' min lowest layer height is ', &
            minval(layerHeight(RANGE2D,lbnd(3))),' m'
        !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        write(message,'(A,ES10.3,A)') trim(name)//' max lowest layer abundance is ', &
            maxval(abundance(RANGE2D,lbnd(3))),' m-3'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      !> The typical range for abundance is 0 to 1E5 m-3 in mussel beds
      if (allocated(mask)) deallocate(mask)
      allocate(mask(ubnd(1)-lbnd(1)+1, ubnd(2)-lbnd(2)+1, ubnd(3)-lbnd(3)+1), stat=localrc)
      mask(RANGE3D) = (abundance(RANGE3D) /= missingValue)
      mask(RANGE3D) = (abundance(RANGE3D) > 0 .and. mask(RANGE3D))

      if (.not.any(mask)) then
        write(message,'(A)') trim(name)//' found no unmasked abundance'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      mask(RANGE3D) = (concentration(RANGE3D) > 0 .and. mask(RANGE3D))
      if (.not.any(mask)) then
        write(message,'(A)') trim(name)//' found no concentration at abundance'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      !> @todo read unit from field, add this unit in respective export flux
      write(message,'(A,ES10.3,A)') trim(name)//' max food concentration is ', &
        maxval(concentration(RANGE3D), mask=mask),' mmol m-3'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      write(fluxName,'(A)') trim(filterSpecies(1))//'_flux_in_water'

      ! Get flux species, be careful to look at the creator attribute to choose
      ! the right one, i.e. those created as export states from this component
      call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
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
        write(message,'(A)') trim(name)//' did not find unique complete field with name '//trim(fluxName)
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

      call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
        itemSearch='maximum_filtration_rate', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldCount > 0) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=maximumFiltrationRate, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        allocate(maximumFiltrationRate(RANGE3D), stat=localrc)
      endif

      call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
        itemSearch='fractional_loss_rate', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldCount > 0) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=fractionalLossRate, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        allocate(fractionalLossRate(RANGE3D), stat=localrc)
      endif

      ! The maximum filtration rate is taken from Bayne et al. 1993, who found
      ! an empirical relationship between food TPM and TPM filtration rate, givin
      ! in units of mg TPM DW h-1 per (300 mg) mussel mass.  The organic
      ! matter fraction POM/TPM in their experiments was consistently
      ! 0.56, and thus we convert the original formulation to one for food
      ! C amount
      ! FR_300 = 0.83*TPM^0.983
      !        = 0.83*(POM/0.56)^0.983
      !        = 0.83*(amountC*mgDWPermmolC/1000)^0.983
      !        = 0.05 amountC^0.983
      ! with amountC in mmol m-3 and FR_300 in mg DW TPM h-1 300 mg mussel DW-1

      ! Metabolic scaling for filtration rate and mass was used by Bayne et al
      ! with the factor 0.67, to arrive at our mussel_mass, we have to scale all
      ! rates with this factor
      scaleFactor = (musselMass / 0.6) ** 0.67

      ! Further scaling is required for going from TPM to amountC.  The above
      ! realtions with 56% POM/TPM and mgDWPermmolC then add to the scale factor
      scaleFactor = scaleFactor * 0.56 * mmolCPermgDW ! unit mmolC / mg TPM DW

      ! Finally, convert the hourly rate to a per-second rate
      scaleFactor = scaleFactor / 3600.0 ! unit mmolC mg TPM DW-1 h-1

      ! There is a lower filtration threshold at around 0.7 mmol C m-3, below
      ! which there is no filtration and mussels close their valves to conserve
      ! energy (Riisgard2003)
      maximumFiltrationRate(RANGE3D) = 0.0
      where(concentration(RANGE3D) > 0.7)
        maximumFiltrationRate(RANGE3D) = 0.05 * (concentration(RANGE3D))**.983
      endwhere

      maximumFiltrationRate(RANGE3D) = scaleFactor * maximumFiltrationRate(RANGE3D)

      write(message,'(A,ES10.3,A)') trim(name)//' max ind filtration rate is ', &
          maxval(maximumFiltrationRate(RANGE3D), mask=mask),' mmol C s-1 Ind-1'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      ! The loss rate is in mmol PhyC m-3 s-1 and is obtained
      ! by multiplying the individual filtration rate with the abundance
      ! volume concentration
      lossRate(RANGE3D) = &
        - maximumFiltrationRate(RANGE3D) * abundance(RANGE3D)
      write(message,'(A,ES10.3,A)') trim(name)//' max loss rate is ', &
          -minval(lossRate(RANGE3D), mask=mask),' mmol C m-3 s-1'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      ! For diagnostics and co-filtration of other species, calculate the fractional loss rate
      fractionalLossrate = 0.0
      where (concentration(RANGE3D) > 0)
        fractionalLossRate(RANGE3D) = lossRate(RANGE3D) / concentration(RANGE3D)
      endwhere

      ! Cap the fractional loss rate at 30% of integration_timestep. Then correct
      ! also the absolute loss rate

      write(message,'(A)') trim(name)
      if (any(-fractionalLossRate(RANGE3D) * integration_timestep > maximumRelativeChange)) then

        where (-fractionalLossRate(RANGE3D) * integration_timestep > maximumRelativeChange)
          fractionalLossRate(RANGE3D) = - maximumRelativeChange / integration_timestep
          lossRate(RANGE3D) = fractionalLossRate(RANGE3D) * concentration(RANGE3D)
        endwhere

        call MOSSCO_MessageAdd(message,' is filtering (capped) up to ')
      else
        call MOSSCO_MessageAdd(message,' is filtering up to ')
      endif

      write(string,'(ES10.3)') maxval(-lossRate(RANGE3D),mask=mask(RANGE3D))
      call MOSSCO_MessageAdd(message,trim(string))
      !write(string,'(F6.3)') maxval(-fractionalLossRate(RANGE3D),mask=mask(RANGE3D))*100.0*3600.0
      !call MOSSCO_MessageAdd(message,' mmol m-3 s-1 or '//trim(string)//'% h-1')
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      
      !> Add the produkt of the main filter species
      if (filterSpecies(2) /= '') then
        write(fluxName,'(A)') trim(filterSpecies(2))//'_flux_in_water'

        ! Get flux species, be careful to look at the creator attribute to choose
        ! the right one, i.e. those created as export states from this component
        call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
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
          write(message,'(A)') trim(name)//' did not find unique complete field with name '//trim(fluxName)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          call MOSSCO_StateLog(importState)
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        
        call ESMF_FieldGet(fieldList(1), farrayPtr=concentration, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        where(mask(RANGE3D))
          concentration(RANGE3D) =  -lossRate(RANGE3D) * (1-respiration_fraction)
        endwhere
        
      endif
      
      ! Now add co-filtered items
      call MOSSCO_AttributeGet(gridComp, 'filter_other_species', filterSpeciesList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(filterSpeciesList)) then
        otherCount = ubound(filterSpeciesList,1)
      else
        otherCount = 0
      endif

      do i = 1, otherCount
        write(fluxName,'(A)') trim(filterSpeciesList(i,1))//'_flux_in_water'

        ! Get cofiltered flux species, be careful to look at the creator attribute to choose
        ! the right one
        call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
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
          call MOSSCO_StateLog(exportState)
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

        call ESMF_FieldGet(field, farrayPtr=lossRate,  rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !> Get concentrations from import state
        call ESMF_StateGet(importState, trim(filterSpeciesList(i,1))//'_in_water', &
          itemType=itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemType /= ESMF_STATEITEM_FIELD) then
          write(message,'(A)') trim(name)//' did not find field '//trim(filterSpeciesList(i,1))//'_in_water'
          rc = ESMF_RC_NOT_FOUND
          return
        endif

        call ESMF_StateGet(importState, trim(filterSpeciesList(i,1))//'_in_water', field=field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
          if (advanceCount < 2) then
            write(message,'(A)') trim(name)//' received incomplete field'
            call MOSSCO_FieldString(field, message, rc=localrc)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          endif
          cycle
        endif

        call ESMF_FieldGet(field, farrayPtr=concentration,  rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        where(mask(RANGE3D))
          lossRate(RANGE3D) = fractionalLossRate(RANGE3D) * concentration(RANGE3D) ! mmol s-1 m-3
        endwhere

        write(message,'(A,ES10.3,A)') trim(name)//' is co-filtering up to ', &
            maxval(-lossRate(RANGE3D),mask=mask(RANGE3D)),' XXX s-1'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        !> If there is a product from the educt, then add the lossRate previously calculated
        !> to the product flux
        if (trim(filterSpeciesList(i,2)) == '') cycle

        write(fluxName,'(A)') trim(filterSpeciesList(i,2))//'_flux_in_water'

        call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
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
          write(message,'(A)') trim(name)//' did not find matching field with name '//trim(fluxName)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          cycle
        endif

        call ESMF_FieldGet(field, farrayPtr=concentration,  rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !> This assumes that the educt and product share the same unit
        where(mask(RANGE3D))
          concentration(RANGE3D) = -lossRate(RANGE3D)
        endwhere

      enddo
    enddo

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_Reallocate(filterSpeciesList, 0, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(filterSpecies, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(mask)) deallocate(mask)
    if (allocated(lbnd)) deallocate(lbnd)
    if (allocated(ubnd)) deallocate(ubnd)

    ! Deallocate the diagnostics only if they are not in the export state, i.e.
    ! if they were allocated locally

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='maximum_filtration_rate', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if ((fieldCount == 0) .and. associated(maximumFiltrationRate)) deallocate(maximumFiltrationRate)

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='fractional_loss_rate', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if ((fieldCount == 0) .and. associated(fractionalLossRate)) deallocate(fractionalLossRate)

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='layer_height_in_water', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if ((fieldCount == 0) .and. associated(layerheight)) deallocate(layerheight)

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='mussel_abundance_in_water', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if ((fieldCount == 0) .and. associated(abundance)) deallocate(abundance)

    if (allocated(depthAtSoil)) deallocate(depthAtSoil)

    !! This component has no do loop over an internal timestep, it is advanced with the
    !! timestep written into its local clock from a parent component
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    timeStep=stopTime-currTime
    if (stopTime>currTime) then
      call ESMF_ClockAdvance(clock, timeStep=timeStep, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    !> Deallocate those fields that might have been allocated locally
    call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
      itemSearch='layer_height_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if ((fieldCount == 0) .and. associated(layerHeight)) deallocate(layerHeight)

    call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
      itemSearch='mussel_abundance_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if ((fieldCount == 0) .and. associated(abundance)) deallocate(abundance)

    call MOSSCO_CompExit(gridComp, rc=localrc)
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

    call MOSSCO_CompExit(gridComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

  subroutine MOSSCO_WeightsFromSurface(height, weight, kwe, threshold, rc)

    real(ESMF_KIND_R8), intent(in)               :: height(:,:,:)
    real(ESMF_KIND_R8), intent(out), allocatable :: weight(:,:,:)
    logical, intent(in), optional                :: kwe
    real(ESMF_KIND_R8), intent(in), optional     :: threshold
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)            :: rc_, localrc, i, ubnd(3), lbnd(3)
    real(ESMF_KIND_R8)               :: threshold_
    logical, allocatable             :: mask(:,:,:)
    real(ESMF_KIND_R8) , allocatable :: uppersum(:,:), lowersum(:,:)

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(threshold)) then
      threshold_ = threshold
    else
      threshold_ = 0.0
    endif

    lbnd=lbound(height)
    ubnd=ubound(height)

    if (.not.allocated(weight)) allocate(weight(RANGE3D))

    if (threshold_ == 0.0) then
      weight = 1.0
      return
    endif

    !> By default, assign a weight of one to the surface, normalize later
    weight(:,:,:) = 0.0
    weight(:,:,ubound(height,3)) = 1.0
    allocate(mask(RANGE3D), stat=localrc)
    allocate(uppersum(RANGE2D), stat=localrc)
    allocate(lowersum(RANGE2D), stat=localrc)

    !> In all pelagic layers, distribute the weight, assign 1 if completely
    !> above threshold, otherwise fraction
    do i=ubound(height,3)-1,lbound(height,3),-1
      uppersum=sum(height(:,:,i+1:ubound(height,3)), dim=3)
      lowersum=sum(height(:,:,i:ubound(height,3)), dim=3)
      where (uppersum <= threshold)
        weight(:,:,i) = 1.0
      endwhere
      where (uppersum > threshold .and. lowersum < threshold)
        weight(:,:,i) = (threshold - uppersum)/(lowersum - uppersum)
      endwhere
    enddo

    if (allocated(mask)) deallocate(mask)
    if (allocated(uppersum)) deallocate(uppersum)
    if (allocated(lowersum)) deallocate(lowersum)

  end subroutine MOSSCO_WeightsFromSurface

end module filtration_component
