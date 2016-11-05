!> @brief Implementation of an ESMF/MOSSCO component for filtration, both
!> from organisms that are suspended in the water column as well as from
!> organisms that reside on the sea floor
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
    integer(ESMF_KIND_I4)   :: localrc, rank, otherCount, i, diagCount
    type(ESMF_Field)        :: field
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_Config)       :: config
    logical                 :: configIsPresent, fileIsPresent, labelIsPresent
    real(ESMF_KIND_R8)      :: musselMass, minimumFoodFlux, formFactor
    real(ESMF_KIND_R8)      :: musselLengthScale, roughnessLength

    character(len=ESMF_MAXSTR)  :: filterSpecies, xVelocity, yVelocity
    character(len=ESMF_MAXSTR), allocatable  :: filterSpeciesList(:), itemNameList(:)
    character(len=ESMF_MAXSTR), allocatable  :: diagNameList(:)

    type(MOSSCO_ParameterType), dimension(10) :: parameters

    rc = ESMF_SUCCESS

    ! Provide default values for all parameters that could be set in the
    ! component's configuration file
    parameters(1)%name='mussel_mass'
    parameters(1)%label='mussel_mass'
    parameters(1)%typeKind=ESMF_TYPEKIND_R8
    !parameters(1)%unit='g'

    ! Mussel parameters from Rijsgaard 2001
    minimumFoodFlux  = 0.6166697552  ! mmol C s-1 m-3, equiv to 20 mg C
    filterSpecies = 'phytoplankton' ! Main variable to filter
    xVelocity = 'x_velocity'
    yVelocity = 'y_velocity'
    musselMass = 0.6 ! g DW / individual

    ! Mussel parameters from van Duren (for mussel bed)
    roughnessLength = .004 ! typical roughness length
    musselLengthScale = .05 ! m typical size of mussel

    ! Geometric parameter for organisms in water (reduction of flow)
    formFactor = 0.35

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

      call MOSSCO_ConfigGet(config, label='roughness_length', value=roughnessLength, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_ConfigGet(config, label='form_factor', value=formFactor, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_ConfigGet(config, label='mussel_length_scale', value=musselLengthScale, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_ConfigGet(config, label='minimum_food_flux', value=minimumFoodFlux, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_ConfigGet(config, label='filter', value=filterSpecies, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_ConfigGet(config, 'other', filterSpeciesList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(filterSpeciesList)) then
        call MOSSCO_AttributeSet(gridComp, 'filter_other_species', filterSpeciesList, localrc)
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

      call MOSSCO_ConfigGet(config, 'diag', diagNameList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(diagNameList)) then
        write(message,'(A)') trim(name)//' found diagnostic:'
        call MOSSCO_MessageAdd(message, diagNameList, rc=localrc)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      ! else ! provide default diagnostic names
      !   call MOSSCO_Reallocate(diagNameList, 7, keep=.false., rc=localrc)
      !   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      !     call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !
      !   diagNameList(1) = 'mussel_abundance_in_water'
      !   diagNameList(2) = 'layer_height_in_water'
      !   diagNameList(3) = 'potential_clearance_rate'
      !   diagNameList(4) = 'boundary_layer_speed'
      !   diagNameList(5) = 'maximum_filtration_rate'
      !   diagNameList(6) = 'fractional_filtration_rate'
      !   diagNameList(7) = 'shear_speed'

      endif

      if (allocated(diagNameList)) then
        call MOSSCO_AttributeSet(gridComp, 'diagnostic_variables', diagNameList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

    endif

    call ESMF_AttributeSet(gridComp, 'filter_species', trim(filterSpecies), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'mussel_length_scale', musselLengthScale, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'form_factor', formFactor, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'mussel_mass', musselMass, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'roughness_length', roughnessLength, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'minimum_food_flux', minimumFoodFlux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! Create a list to hold the names of the item to filter, the names of the
    ! velocity fields and shear stress, and the names of other items to co-filter
    call MOSSCO_Reallocate(itemNameList, 6 + otherCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    itemNameList(1) = trim(xVelocity)
    itemNameList(2) = trim(yVelocity)
    itemNameList(3) = 'maximum_shear_stress'
    itemNameList(4) = 'mussel_abundance'  ! '_at_soil_surface'
    itemNameList(5) = 'mussel_abundance'  ! '_at_water_surface'

    itemNameList(6) = trim(filterSpecies)
    do i = 1, othercount
      itemNameList(6 + i) = trim(filterSpeciesList(i))
    enddo

    do i = 1, ubound(itemNameList,1)

      if (i < 3 .or. i > 5) then
        field = ESMF_FieldEmptyCreate(name=trim(itemNameList(i))//'_in_water', rc=localrc)
      elseif (i == 3) then
        field = ESMF_FieldEmptyCreate(name=trim(itemNameList(i)), rc=localrc)
      elseif (i == 4) then
        field = ESMF_FieldEmptyCreate(name=trim(itemNameList(i))//'_at_water_surface', rc=localrc)
      elseif (i == 5) then
        field = ESMF_FieldEmptyCreate(name=trim(itemNameList(i))//'_at_soil_surface', rc=localrc)
      endif

      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (i < 3) call ESMF_AttributeSet(field, 'units', 'm s-1', rc=localrc)
      if (i == 3)  call ESMF_AttributeSet(field, 'units', 'Pa', rc=localrc)
      if (i == 4 .or. i == 5)  call ESMF_AttributeSet(field, 'units', 'm-2', rc=localrc)
      if (i > 5)  call ESMF_AttributeSet(field, 'units', 'mmol m-3', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message, '(A)') trim(name)//' created empty field'
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAddReplace(importState, (/field/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    diagCount = 0
    if (allocated(diagNameList)) diagCount = size(diagNameList)

    call MOSSCO_Reallocate(diagNameList, ubound(itemNameList,1) + diagCount, &
      keep=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    diagNameList(ubound(diagNameList,1)-size(itemNameList)+1:ubound(diagNameList,1)) &
        = itemNameList(:)

    !> Create export states for all variables above index 5,  add diagnostic variables
    do i = 6, ubound(diagNameList,1)
      !> Create export states for diagnostic, filter and co-filter items
      if (i < ubound(diagNameList,1)-size(itemNameList)) then
        field = ESMF_FieldEmptyCreate(name=trim(diagNameList(i)), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateAddReplace(exportState, (/field/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message, '(A)') trim(name)//' created empty field'

      else
        fieldBundle = ESMF_FieldBundleCreate(name=trim(diagNameList(i))//'_flux_in_water', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        field = ESMF_FieldEmptyCreate(name=trim(diagNameList(i))//'_flux_in_water', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !> @todo check units for co-filtered and filter species
        call ESMF_AttributeSet(field, 'units', 'mmol m-3 s-1', rc=localrc)
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

      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    enddo

    call MOSSCO_Reallocate(diagNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    real(ESMF_KIND_R8)                      :: mussel_mass, minimumFoodFlux

    rc = ESMF_SUCCESS

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

    call ESMF_AttributeGet(gridComp, name='minimum_food_flux', &
      value=minimumFoodFlux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A,ES10.3)') trim(name)//' minimum food flux is ', minimumFoodFlux
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_AttributeGet(gridComp, name='mussel_mass', &
      value=mussel_mass, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A,ES10.3)') trim(name)//' mussel mass is ', mussel_mass
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_AttributeGet(gridComp, name='filter_species', &
      value=filterSpecies, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(name)//' species to filtrate is '//trim(filterSpecies)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call MOSSCO_AttributeGet(gridComp, 'filter_other_species', filterSpeciesList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(filterSpeciesList)) then
      do i = lbound(filterSpeciesList,1), ubound(filterSpeciesList,1)
        write(message,'(A)') trim(name)//' filter also '//trim(filterSpeciesList(i))
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

    real(ESMF_KIND_R8), allocatable, dimension(:,:)   :: xWidth, yWidth
    real(ESMF_KIND_R8), allocatable, dimension(:,:,:) :: foodFluxFactor, filtrationRate

    real(ESMF_KIND_R8), pointer, dimension(:,:)    :: abundanceAtSurface => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:)    :: abundanceAtSoil => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:)    :: bottomShearStress => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: abundance => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: lossRate => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: layerHeight => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: potentialClearanceRate, ustar, speed
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: maximumFiltrationRate, fractionalLossRate
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: interfaceDepth, xVelocity, yVelocity
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)  :: concentration
    real(ESMF_KIND_R8), allocatable, dimension(:,:):: depthAtSoil, frictionCoefficient
    real(ESMF_KIND_R8), allocatable, dimension(:,:):: hydraulicRadius

    logical, allocatable, dimension(:,:,:)       :: mask
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: grid
    integer(ESMF_KIND_I4)   :: localrc, i, rank, otherCount, fieldCount, j
    real(ESMF_KIND_R8)      :: surfaceRoughness, diameter, distance
    real(ESMF_KIND_R8)      :: minimumFoodFlux, musselMass, crossSection, sandRoughness
    real(ESMF_KIND_R8)      :: roughnessLength, musselLengthScale
    real(ESMF_KIND_R8)      :: karman
    real(ESMF_KIND_R8)      :: missingValue, mmolPermg, mgPermmol
    integer(ESMF_KIND_I4), allocatable   :: ubnd(:), lbnd(:)
    integer(ESMF_KIND_I4)                :: ubndZ(3), lbndZ(3)
    type(ESMF_Field), allocatable        :: fieldList(:)

    character(len=ESMF_MAXSTR)  :: filterSpecies, fluxName, creatorName
    character(len=ESMF_MAXSTR), allocatable  :: filterSpeciesList(:)
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    logical                     :: isSoil, isSurface
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8),parameter:: pi=3.141592653589793d0

    mmolPermg = 0.03083348776
    mgPermmol = 1./mmolPermg
    karman = 0.4
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
      value=musselMass, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(gridComp, name='mussel_length_scale', &
      value=musselLengthScale, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(gridComp, name='roughness_length', &
      value=roughnessLength, rc=localrc)
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

    ! Return from this component if nothing can be done as there is no abundance
    if (.not.(isSurface .or. isSoil)) then
      write(message,'(A)') trim(name)//' found no abundance at all'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

      call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
        itemSearch='layer_height_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      if ((fieldCount == 0) .and. associated(layerHeight)) deallocate(layerHeight)

      call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
        itemSearch='mussel_abundance_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      if ((fieldCount == 0) .and. associated(abundance)) deallocate(abundance)

      call MOSSCO_CompExit(gridComp, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      return
    endif

    if (isSurface) then
      !> @todo according to Krone, mussels are distributed up to a critical
      !> depth of 5 m from the surface, this is not implemented yet, but
      !> all mussels are soley applied to the surface layer

      where (layerHeight(RANGE2D,ubnd(3)) > 0)
        abundance(RANGE2D,ubnd(3)) = abundanceAtSurface(RANGE2D) &
          / layerHeight(RANGE2D,ubnd(3))
      endwhere
      write(message,'(A,ES10.3,A)') trim(name)//' max upper layer abundance is ', &
          maxval(abundance(RANGE2D,ubnd(3))),' m-3'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    if (isSoil) then
      where (layerHeight(RANGE2D,lbnd(3)) > 0)
        abundance(RANGE2D,lbnd(3)) = abundanceAtSoil(RANGE2D) &
          / layerHeight(RANGE2D,lbnd(3))
      endwhere
      write(message,'(A,ES10.3,A)') trim(name)//' max lowest layer abundance is ', &
          maxval(abundance(RANGE2D,lbnd(3))),' m-3'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    if (allocated(mask)) deallocate(mask)
    allocate(mask(ubnd(1)-lbnd(1)+1, ubnd(2)-lbnd(2)+1, ubnd(3)-lbnd(3)+1), stat=localrc)
    mask(RANGE3D) = (abundance(RANGE3D) /= missingValue)
    mask(RANGE3D) = (abundance(RANGE3D) > 0 .and. mask(RANGE3D))

    if (.not.any(mask)) then
      write(message,'(A)') trim(name)//' found no unmasked abundance'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (allocated(mask)) deallocate(mask)
      call MOSSCO_CompExit(gridComp, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      return
    endif

    mask(RANGE3D) = (concentration(RANGE3D) > 0 .and. mask(RANGE3D))
    if (.not.any(mask)) then
      write(message,'(A)') trim(name)//' found no concentration at abundance'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (allocated(mask)) deallocate(mask)

      call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
        itemSearch='layer_height_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      if ((fieldCount == 0) .and. associated(layerHeight)) deallocate(layerHeight)

      call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
        itemSearch='mussel_abundance_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      if ((fieldCount == 0) .and. associated(abundance)) deallocate(abundance)

      call MOSSCO_CompExit(gridComp, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      return
    endif

    write(message,'(A,ES10.3,A)') trim(name)//' max food concentration is ', &
        maxval(concentration(RANGE3D), mask=mask),' mmol m-3'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    write(fluxName,'(A)') trim(filterSpecies)//'_flux_in_water'

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

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='x_velocity_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(fieldList(1), farrayPtr=xVelocity, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='y_velocity_in_water', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(fieldList(1), farrayPtr=yVelocity, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
      itemSearch='boundary_layer_speed', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), farrayPtr=speed, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      allocate(speed(RANGE3D), stat=localrc)
    endif

    !> Calculate the absolute velocity, i.e. speed (in m s-1)
    speed(RANGE3D) = sqrt( yVelocity(RANGE3D) ** 2 &
                         + xVelocity(RANGE3D) **2)

    !> Assume for all layers above the surface layer that the filter feeders grow
    !> on structures in the water column. Then, the friction stress
    !> is calculated as $tau = cw \cdot \rho \cdot u^2$, and the shear velocity is
    !> $u^* = \sqrt(tau/rho) = sqrt(cw) \cdot u.$
    !> We consider the cylinder to be a long plate and are interested not in the
    !> normal, but in the tangential (skin) friction stress
    !> To calculate the skin friction drag coefficient factor $cw$, we first calculate
    !> the friction coefficient $\lambda$ and then $c_w$ with  $c_w = \lambda/8 $.
    !> To obtain $\lambda$, we use the turbulent part of the Colebrook-White hypothesis:
    !> $$
    !> 1 / \sqrt\lambda = -2.03\cdot\log_10(k_s/(14.84\cdot r_{hy}))
    !> $$

    !> The hydraulic radius $r_{hy}$ is calculated as the ratio of the channel width
    !> (space between windpiles) times water depth divided by the wetted circumference
    !> interspace + 2 * water depth.  For typical values of diameter 3 m, distance 250 m,
    !> and depth 40 m, the hydraulic radius is 30 m
    if (.not.allocated(hydraulicRadius)) allocate(hydraulicRadius(RANGE2D))
    diameter = 3
    distance = 250
    hydraulicRadius = ((distance - diameter) * depthAtSoil(RANGE2D)) &
      / (distance - diameter + 2 * depthAtSoil(RANGE2D))

    !> The Colebrook-White resistance law
    !> $$
    !> 1 / \sqrt\lambda = -2.03\cdot\log_10(2.51/(Re\cdot\sqrt\lambda) + k_s/(14.84\cdot r_{hy}))
    !> $$
    !> is an iterative solver for the friction coefficient $\lambda$ in pipe (and open channel)
    !> flow.
    !> It can be simplified for high Reynolds numbers (i.e. turbulent flow)
    !> $$
    !> 1 / \sqrt\lambda = -2.03\cdot\log_10(k_s/(14.84\cdot r_{hy}))
    !> $$
    !> with the equivalent sand roughness 30 times the surface roughness
    !> $k_s$ = 30 z_0$.
    if (.not.allocated(frictionCoefficient)) allocate(frictionCoefficient(RANGE2D))
    sandRoughness = 30 * surfaceRoughness
    frictionCoefficient(RANGE2D) = - 2.03 * log10(sandRoughness &
      / 14.84 / hydraulicRadius(RANGE2D))
    frictionCoefficient(RANGE2D) = 1 / (frictionCoefficient(RANGE2D) &
      * frictionCoefficient(RANGE2D))

    !> For typical valus of z0 = 4.4 mm and the hydraulic radius of 25, the
    !> friction coefficient is 1 / 50 = 0.02
    !> The friction drag factor $c_w$ is then $\lambda / 8$, and has a typical
    !> value of 0.0025
    !> skinFrictionDragCoefficient = frictionCoefficient / 8

    call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
      itemSearch='shear_speed', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), farrayPtr=ustar, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      allocate(ustar(RANGE3D), stat=localrc)
    endif

    !> the friction stress $\tau$ is then  $\tau = c_w \cdot \rho \cdot v^2$,
    !> i.e. for typical velocities of .1 m s-1 it is 0.025 Pa
    !> The skin friction shear speed u* is then $u* = \sqrt(\tau/rho)$,
    !> and on the order of 0.005 m s-1
    do i = lbound(speed,3)+1, ubound(speed,3)
      ustar(RANGE2D,i) = speed(RANGE2D,i) &
        * sqrt(frictionCoefficient(RANGE2D)/8)
    enddo

    !> According to van Duren 2006, typical values for a high-velocity regime
    !> are z0=4.4 mm, shear velocity u* = 4E-2 m s-1 (we get roughness length from
    !> the configuration file and assume it is equal at bottom and pile)
    ustar(RANGE2D,lbound(speed,3))  = 4.0E-2

    ! Alternatively, we can also calculate shear velocity from the ocean model's &
    ! maximum_bottom_stress, if that is (optionally) provided
    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='maximum_bottom_stress', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), farrayPtr=bottomShearStress, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Bottom shear stress translates to shear velocity with the square root:
      !> $$ u^* = \sqrt\left(\frac{\tau}{\rho}\right)
      ustar(RANGE2D,lbound(speed,3)) = sqrt(bottomShearStress(RANGE2D)/1000)
    endif

    !> Finally, apply law of the wall to estimate the boundary speed at distance
    !> musselLengthScale, given $u*$ and roughness length $z0$.
    !> $$
    !> u = u^* / k \cdot \log_e \frac{z}{z0}
    !> $$
    speed(RANGE3D) = ustar(RANGE3D) &
      / karman * log (musselLengthScale / roughnessLength)

    call MOSSCO_StateGetFieldList(exportState, fieldList, fieldCount=fieldCount, &
      itemSearch='potential_clearance_rate', fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), farrayPtr=potentialClearanceRate, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! call ESMF_AttributeAdd(fieldList(1), 'units', 'mmol m-3 s-1', rc=localrc)
      ! if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      !   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !
      ! call ESMF_AttributeAdd(fieldList(1), 'creator', trim(name), rc=localrc)
      ! if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      !   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    else
      allocate(potentialClearanceRate(RANGE3D), stat=localrc)
    endif

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

    if (.not.allocated(filtrationRate)) allocate(filtrationRate(RANGE3D), stat=localrc)
    if (.not.allocated(foodFluxFactor)) allocate(foodFluxFactor(RANGE3D), stat=localrc)

    ! The maximum filtration rate is taken from Bayne et al. 1993, who found
    ! an empirical relationship between TPM and filtration rate, given in
    ! units of mg PhyC h-1 (300 mg)-1 Mytilus; we convert mmol of concentration to
    ! mg for this relationship.  Concentrations are around 8 mmol m-3, so this calculation
    ! yields 250 = .83 * (8 / 0.03083348776)** .983
    maximumFiltrationRate(RANGE3D) = .83 * (concentration(RANGE3D) * mgPermmol) ** .983

    ! Convert unit of maximumFiltrationRate to mg phyC mg-1 Mytilus s-1
    ! This typically yields 250E-6
    maximumFiltrationRate(RANGE3D) = maximumFiltrationRate(RANGE3D) / (300.0 * 3600.0)

    write(message,'(A,ES10.3,A)') trim(name)//' max filtration max rate is ', &
        maxval(maximumFiltrationRate(RANGE3D), mask=mask),' mg mg-1 s-1'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    ! The potential clearence rate in mmol m-3 s-1 is the product of boundary velocity
    ! in m s-1, concentration in mmol phyC m-3, and cross-sectional area of the
    !> individual mussels times abundance
    !> the cross sectional area is taken as the individual gill area ~20E-4 m2
    crossSection = 20E-4
    potentialClearanceRate(RANGE3D) = speed(RANGE3D) * concentration(RANGE3D) &
      * abundance(RANGE3D) * crossSection

    !> at 1E-2 m s-1 speed, concentration of 25 mmol m-3, cross section area of
    !> 2E-5 m2, and abundance 200 m-3, the potential clearance rate is
    !> 1E-2 mmol m-3 s-1
    write(message,'(A,ES10.3,A)') trim(name)//' max potential clearance rate is ', &
        maxval(potentialClearanceRate(RANGE3D), mask=mask),' mmol m-3 s-1'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    ! The food flux factor is a threshold function that limits
    ! the supply at low rates, it is typically
    ! minimumFoodFlux  = 0.6166697552  ! mmol C s-1 m-3, equiv to 20 mg C
    foodFluxFactor(RANGE3D) = 1
    where(foodFluxFactor(RANGE3D) < minimumFoodFlux)
      foodFluxFactor(RANGE3D) = potentialClearanceRate(RANGE3D) / minimumFoodFlux
    endwhere

    ! The filtration rate is in mg PhyC mg-1 Mytilus s-1 and
    ! is composed of a concentration-dependent maximum rate and
    ! a supply-dependent food flux factor.
    filtrationRate(RANGE3D) = &
      maximumFiltrationRate(RANGE3D) * foodFluxFactor(RANGE3D)
    write(message,'(A,ES10.3,A)') trim(name)//' max filtration rate is ', &
        maxval(filtrationRate(RANGE3D), mask=mask),' mg mg-1 s-1'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    ! The loss rate is in mg PhyC m-3 s-1
    lossRate = &
    ! and is proportinal to mussel dry mass concentration, calculated
    ! from abundance m-3 times mussel_mass mg (600 mg default for Mytilus edulis)
      - abundance(RANGE3D) * musselMass &
    ! as well as filtration rate in
      * filtrationRate(RANGE3D) &
    ! and convert from mg to mmol
      * mmolPermg

    write(message,'(A,ES10.3,A)') trim(name)//' is filtering up to ', &
      maxval(-lossRate(RANGE3D),mask=mask(RANGE3D)),' mmol m-3 s-1'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    ! For diagnostics and co-filtration of other species, calculate the fractional loss rate
    where (concentration(RANGE3D) > 0)
      fractionalLossRate(RANGE3D) = lossRate(RANGE3D) / concentration(RANGE3D)
    endwhere

    write(message,'(A,ES10.3,A)') trim(name)//' is filtering up to ', &
        maxval(-lossRate(RANGE3D),mask=mask(RANGE3D)),' mmol m-3 s-1'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call MOSSCO_AttributeGet(gridComp, 'filter_other_species', filterSpeciesList, rc=localrc)
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
        call MOSSCO_FieldString(field, message, rc=localrc)
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
    if (allocated(filtrationRate)) deallocate(filtrationRate)
    if (allocated(foodFluxFactor)) deallocate(foodFluxFactor)

    ! Deallocate the diagnostics only if they are not in the export state, i.e.
    ! if they were allocated locally
    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='potential_clearance_rate', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if ((fieldCount == 0) .and. associated(potentialClearanceRate)) &
      deallocate(potentialClearanceRate)

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='maximum_filtration_rate', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if ((fieldCount == 0) .and. associated(maximumFiltrationRate)) deallocate(maximumFiltrationRate)

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='shear_speed', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if ((fieldCount == 0) .and. associated(ustar)) deallocate(ustar)

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='boundary_layer_speed', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if ((fieldCount == 0) .and. associated(speed)) deallocate(speed)

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='fractional_loss_rate', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if ((fieldCount == 0) .and. associated(fractionalLossRate)) deallocate(fractionalLossRate)

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='potential_clearance_rate', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if ((fieldCount == 0) .and. associated(potentialClearanceRate)) deallocate(potentialClearanceRate)

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
    if (allocated(hydraulicRadius)) deallocate(hydraulicRadius)
    if (allocated(frictionCoefficient)) deallocate(frictionCoefficient)

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
