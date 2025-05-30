!> @brief Implementation of an ESMF mediator component that reduces
!> the vertical dimension by avg/sum/min/max operations
!> @file vertical_reduction.F90
!!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2021-2022 Helmholtz-Zentrum Hereon
!> @copyright Copyright (C) 2017-2021 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hereon.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "vertical_reduction.F90"

#define RANGE2D lbnd(1):ubnd(1),lbnd(2):ubnd(2)
#define RANGE3D lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module vertical_reduction

  use esmf
  use mossco_field
  use mossco_state
  use mossco_component
  use mossco_config
  use mossco_info
  use mossco_logging
  use mossco_grid

  implicit none

  public SetServices

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(cplComp, rc)

    type(ESMF_cplComp)  :: cplComp
    integer, intent(out) :: rc

    integer              :: localrc

    rc=ESMF_SUCCESS

    call ESMF_cplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_cplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_cplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_cplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: localrc
    type(ESMF_Info)             :: info 

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_InfoGetFromHost(cplComp, info=info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_InfoSet(info, key="NUOPC/General/InititalizePhaseMap", &
      values=InitializePhaseMap, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateReconcile(importState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateReconcile(exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_cplComp)    :: cplComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=ESMF_MAXSTR)      :: name, message, configFileName
    type(ESMF_Time)                 :: currTime

    integer(ESMF_KIND_I4)           :: localrc

    type(ESMF_Config)               :: config
    real(ESMF_KIND_R8)              :: offset, scale_factor, scale_depth
    character(len=ESMF_MAXSTR)      :: operator
    logical                         :: labelIsPresent, isPresent, fileIsPresent
    character(len=ESMF_MAXSTR), pointer :: filterExcludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer :: filterIncludeList(:) => null()
    type(ESMF_Info)                 :: info

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Default values for operations
    operator = 'average'
    scale_factor = 1.0
    offset = 0.0
    scale_depth = 0.0

    configfilename=trim(name)//'.cfg'
    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (fileIsPresent) then

      write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      !> @todo deal with already existing config
      config = ESMF_ConfigCreate(rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Look up labels, don't check for rc, as it will return ESMF_RC_NOT_FOUND if not found
      call MOSSCO_ConfigGet(config, label='operator', value=operator, defaultValue='average', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, label='offset', value=offset, defaultValue=0.0D0, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, label='scale', value=scale_factor, defaultValue=1.0D0, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, label='scale_depth', value=scale_depth, defaultValue=0.0D0, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, 'exclude', filterExcludeList)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, 'include', filterIncludeList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_CplCompSet(cplComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    endif

    !> Homogenize possible input values for operator
    select case (trim(operator))
    case ('mean','avg','average')
      operator = 'average'
    case ('ttl','sum','total')
      operator = 'total'
    case ('min', 'minimum')
      operator = 'minimum'
    case ('max', 'maximum')
      operator = 'maximum'
    case default
      write(message, '(A)') trim(name)//' obtained invalid operator '//trim(operator)
      call MOSSCO_CompExit(cplComp, rc=localrc)
      rc = ESMF_RC_ARG_BAD
      return
    end select

    call ESMF_InfoGetFromHost(cplComp, info=info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Add all configurable options as attributes
    if (associated(filterExcludeList)) then
      call ESMF_InfoSet(info, key='filter_pattern_exclude', values=filterExcludeList, rc=localrc)
      !call MOSSCO_AttributeSet(cplComp, 'filter_pattern_exclude', filterExcludeList, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    if (associated(filterIncludeList)) then
      call ESMF_InfoSet(info, key='filter_pattern_include', values=filterIncludeList, rc=localrc)
      !call MOSSCO_AttributeSet(cplComp, 'filter_pattern_include', filterIncludeList, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call ESMF_InfoSet(info, key='operator_type', value=trim(operator), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_InfoSet(info, key='scale_factor', value=scale_factor, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_InfoSet(info, key='scale_depth', value=scale_depth, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_InfoSet(info, key='add_offset', value=offset, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_cplComp)      :: cplComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock
    integer, intent(out)    :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    integer(ESMF_KIND_I4)   :: localrc

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CreateVerticallyReducedExportFields(cplComp, importState, &
      exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_ReduceFields(cplComp, importState, exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !! Finally, log the successful completion of this function
    call MOSSCO_CompExit(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
subroutine Finalize(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)    :: cplComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    integer(ESMF_KIND_I4)   :: localrc
    logical                 :: isPresent
    type(ESMF_Config)       :: config

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_CplCompGet(cplComp, configIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) then

      call ESMF_CplCompGet(cplComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ConfigDestroy(config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    end if

    call MOSSCO_CompExit(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CreateVerticallyReducedExportFields"
  subroutine MOSSCO_CreateVerticallyReducedExportFields(cplComp, importState, exportState, rc)

    type(ESMF_CplComp), intent(in)         :: cplComp
    type(ESMF_State)                       :: importState, exportState
    integer(ESMF_KIND_I4), optional        :: rc

    type(ESMF_Field), allocatable          :: importfieldList(:), exportFieldList(:), fieldList(:)
    character(ESMF_MAXSTR)                 :: message, itemName, name, operator
    integer(ESMF_KIND_I4)                  :: i, j, jj, rank
    integer(ESMF_KIND_I4)                  :: importFieldCount, exportFieldCount, fieldCount

    logical                                :: isPresent, tagOnly_, isArray
    character(len=ESMF_MAXSTR), pointer    :: filterExcludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer    :: filterIncludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer    :: checkExcludeList(:) => null()

    real(ESMF_KIND_R8)                     :: offset, scale_factor, scale_depth
    integer(ESMF_KIND_I4)                  :: localrc, rc_, matchIndex, matchScore, size
    integer(ESMF_KIND_I8)                  :: advanceCount
    type(ESMF_Clock)                       :: clock
    type(ESMF_Time)                        :: startTime, currTime
    type(ESMF_TimeInterval)                :: timeStep

    type(ESMF_FieldStatus_Flag)            :: fieldStatus
    type(ESMF_Grid)                        :: grid
    type(ESMF_Field)                       :: exportField
    type(ESMF_Info)                        :: info

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_CplCompGet(cplComp, name=name, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGetFromHost(cplComp, info=info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='add_offset', default=0.0D0, value=offset, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='operator_type', default='average', value=operator, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='scale_factor', default=1.0D0, value=scale_factor, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='scale_depth', default=0.0D0, value=scale_depth, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    ! It is better to compare startTime with currTime in a connector, as it could be
    ! called multiple times for a single timeStep
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, &
      timeStep=timeStep, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (currTime > startTime) then
      if (advanceCount < 1) advanceCount = nint((currTime - startTime) / timeStep)
      if (advanceCount < 1) advanceCount = 1
    else
      advanceCount = 0
    endif

    call ESMF_InfoGetArrayMeta(info, key='filter_pattern_include', isArray=isArray, size=size, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    allocate(filterIncludeList(size))
    call ESMF_InfoGet(info, key='filter_pattern_include', values=filterIncludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGetArrayMeta(info, key='filter_pattern_exclude', isArray=isArray, size=size, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    allocate(filterExcludeList(size))
    call ESMF_InfoGet(info, key='filter_pattern_exclude', values=filterExcludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_StateGet(importState, fieldList=importFieldList, fieldCount=importFieldCount, &
        fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), include=filterIncludeList, &
        exclude=filterExcludeList, verbose=.true., rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !if (importFieldCount < 1 .and. advanceCount < 2) then
    if (importFieldCount < 1) then
      write(message,'(A)') trim(name)//' found no fields to reduce'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    do i=1, importFieldCount

      !> Found out whether this field has a vertical dimension, if not, then cycle
      !> @todo, what about meshes?
      call ESMF_FieldGet(importFieldList(i), grid=grid, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_GridGet(grid, rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (rank /= 3) cycle

      ! The field is created from and complete fields, no error is thrown if
      ! the field exists
      call MOSSCO_StateGetFieldList(exportState, exportFieldList, fieldCount=exportFieldCount, &
        itemSearch='vred_'//operator(1:4)//'_'//trim(itemName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (exportFieldCount < 1) then

        call MOSSCO_CreateVerticallyReducedField(importFieldList(i), exportField, &
          operator=trim(operator), offset=offset,  scale_depth=scale_depth, &
          scale=scale_factor, name=trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        call ESMF_StateAddReplace(exportState, (/exportField/), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      endif

      call MOSSCO_Reallocate(exportFieldList, 0,  rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    enddo

    call MOSSCO_Reallocate(importFieldList, 0,  rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (associated(filterIncludeList)) deallocate(filterIncludeList)
    if (associated(filterExcludeList)) deallocate(filterExcludeList)

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_CreateVerticallyReducedExportFields

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ReduceFields"
  subroutine MOSSCO_ReduceFields(cplComp, importState, exportState, rc)

    type(ESMF_CplComp), intent(in)         :: cplComp
    type(ESMF_State)                       :: importState, exportState
    integer(ESMF_KIND_I4), optional        :: rc

    type(ESMF_Field), allocatable          :: importfieldList(:), exportFieldList(:)
    character(ESMF_MAXSTR)                 :: message, itemName, name, operator
    integer(ESMF_KIND_I4)                  :: i, j, importFieldCount, exportFieldCount

    logical                                :: isPresent
    character(len=ESMF_MAXSTR), pointer    :: filterExcludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer    :: filterIncludeList(:) => null()

    real(ESMF_KIND_R8)                     :: offset, scale_factor, scale_depth
    integer(ESMF_KIND_I4)                  :: localrc, rc_
    integer(ESMF_KIND_I8)                  :: advanceCount
    type(ESMF_Clock)                       :: clock
    type(ESMF_Time)                        :: startTime, currTime
    type(ESMF_TimeInterval)                :: timeStep

    integer(ESMF_KIND_I4), allocatable     :: exportUbnd(:), exportLbnd(:)
    integer(ESMF_KIND_I4), allocatable     :: lbnd(:), ubnd(:)
    integer(ESMF_KIND_I4)                  :: exportRank, exportGridRank, importRank
    integer(ESMF_KIND_I4)                  :: importGridRank
    type(ESMF_Grid)                        :: importGrid, exportGrid
    type(ESMF_FieldStatus_Flag)            :: exportFieldStatus, importFieldStatus

    integer(ESMF_KIND_I4),dimension(:,:,:), pointer :: mask3 => null()
    integer(ESMF_KIND_I4),dimension(:,:),   pointer :: mask2 => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:) :: farrayPtr3 => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:)   :: farrayPtr2 => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:) :: layer_height => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:) :: depth => null()
    real(ESMF_KIND_R8), allocatable, dimension(:,:)   :: sum_weight
    real(ESMF_KIND_R8), allocatable, dimension(:,:,:) :: weight
    character(len=ESMF_MAXSTR)             :: importItemName
    type(ESMF_TypeKind_Flag)               :: typeKind
    type(ESMF_Array)                       :: gridArray
    type(ESMF_DistGrid)                    :: distGrid
    type(ESMF_Index_Flag)                  :: indexFlag
    character(len=ESMF_MAXSTR), pointer    :: includeList(:) => null()
    type(ESMF_Info)                        :: info

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_CplCompGet(cplComp, name=name, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, 'add_offset', value=offset, default=0.0D0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='operator_type', default='average', value=operator, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='scale_factor', default=1.0D0, value=scale_factor, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGet(info, key='scale_depth', default=1.0D0, value=scale_depth, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    ! It is better to compare startTime with currTime in a connector, as it could be
    ! called multiple times for a single timeStep
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, &
      timeStep=timeStep, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (currTime > startTime) then
      if (advanceCount < 1) advanceCount = nint((currTime - startTime) / timeStep)
      if (advanceCount < 1) advanceCount = 1
    else
      advanceCount = 0
    endif

    allocate(includeList(1))
    includeList(1) = 'vred_'//operator(1:4)//'_*'
    call MOSSCO_StateGet(exportState, exportFieldList, &
      include=includeList, fieldCount=exportFieldCount, &
      fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (exportFieldCount < 1) then
      write(message,'(A)') trim(name)//' found no fields to reduce'
      if  (advanceCount < 2) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (present(rc)) rc=ESMF_SUCCESS
      return
    endif

    do i=1, exportFieldCount

      !> Get the matching field for a given export field
      call ESMF_FieldGet(exportFieldList(i), name=itemName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      importItemName = itemName(11:len_trim(itemName))
      includeList(1) = importItemName

      call MOSSCO_StateGet(importState, fieldList=importFieldList, fieldCount=importFieldCount, &
        include=includeList, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      !> if not found, or if multiple fields with the same name, then skip this
      !> @todo add later capability for field bundles
      !> This message occurs when multiple components are connect through this mediator,
      !> thus it is disabled for now
      if (importFieldCount /= 1) then
        write(message,'(A)') trim(name)//' did not find matching '//trim(importItemName)//' for '
        call MOSSCO_FieldString(exportFieldList(i), message)
        !if  (advanceCount < 2) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      !> Obtain grid information and see whether it matches

      call ESMF_FieldGet(exportFieldList(i), grid=exportGrid, rank=exportRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_FieldGet(importFieldList(1), grid=importGrid, rank=importRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.importRank == 3) cycle

      call ESMF_GridGet(exportGrid, rank=exportGridRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_GridGet(importGrid, rank=importGridRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      allocate(lbnd(3), stat=localrc)
      allocate(ubnd(3), stat=localrc)

      !> @todo this should ask for grid bounds not field bounds
      call ESMF_FieldGet(importFieldList(1),  localDe=0, farrayPtr=farrayPtr3, &
        exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_GridGetItem(importGrid, ESMF_GRIDITEM_MASK, &
        staggerloc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (isPresent) then
        call ESMF_GridGetItem(importGrid, ESMF_GRIDITEM_MASK, &
          staggerloc=ESMF_STAGGERLOC_CENTER,farrayPtr=mask3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      else
        if (associated(mask3)) deallocate(mask3)
        allocate(mask3(RANGE3D))
      endif

      call ESMF_GridGetItem(exportGrid, ESMF_GRIDITEM_MASK, &
        staggerloc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (isPresent) then
        call ESMF_GridGetItem(exportGrid, ESMF_GRIDITEM_MASK, &
          staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=mask2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      else

        if (associated(mask2)) deallocate(mask2)
        allocate(mask2(RANGE2D))
        mask2 = minval(mask3(RANGE3D), dim=3)

        call ESMF_GridGet(exportGrid, &
          distGrid=distGrid, indexFlag=indexFlag, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

              ! gridArray = ESMF_ArrayCreate(distGrid=distGrid, name='gridMask', &
              !   indexFlag=indexFlag, farray=mask2, rc=localrc)
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
              !
              ! call ESMF_GridSetItem(exportGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
              !   itemFlag=ESMF_GRIDITEM_MASK, array=gridArray, rc=localrc)
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      endif

      if (operator == 'average' .or. operator == 'total') then

        if (allocated(weight)) deallocate(weight)
        allocate(weight(RANGE3D), stat=localrc)
        weight(RANGE3D) = 0.0

        if (allocated(sum_weight)) deallocate(sum_weight)
        allocate(sum_weight(RANGE2D), stat=localrc)
        sum_weight(RANGE2D) = 0.0

      endif

      !> If we use vertical profile weighting with depth scale (exponential)
      !> decrease with depth, the weight is generated from depth.
      !> @todo make sure this is only available with 'average' operator, consider
      !> min/max
      if (scale_depth > 0.0 .and. operator == 'average') then

        call MOSSCO_GridGetDepth(importGrid, depth=depth, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        where (depth(RANGE3D) > 0)
          weight(RANGE3D) = exp (- depth(RANGE3D)/scale_depth)
        endwhere

        where (sum(weight(RANGE3D), dim=3) > 0)
          sum_weight(RANGE2D) = sum(weight(RANGE3D), dim=3)
        endwhere

        do j = lbnd(3), ubnd(3)
          where (sum_weight(RANGE2D) > 0)
            weight(RANGE2D,j) = weight(RANGE2D,j)/sum_weight(RANGE2D)
          endwhere
        enddo

      elseif (operator == 'average' .or. operator == 'total') then
        !> The weight is generated from layer height
        call MOSSCO_GridGetDepth(importGrid, height=layer_height, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        write(*,*) 'LH: ',lbound(layer_height),ubound(layer_height),shape(layer_height),__LINE__

        sum_weight(RANGE2D) = sum(layer_height(RANGE3D), dim=3, mask=(mask3(RANGE3D)>0))

        do j = lbnd(3), ubnd(3)
          where (layer_height(RANGE2D,j) > 0)
            weight(RANGE2D,j) = layer_height(RANGE2D,j)/sum_weight(RANGE2D)
          endwhere
        enddo

      endif ! 'average' or 'total'

      if (allocated(sum_weight)) then
        sum_weight(RANGE2D) = sum(weight(RANGE3D), dim=3, mask=(mask3(RANGE3D)>0))

        if (nint(maxval(sum_weight(RANGE2D), mask=(mask2(RANGE2D)>0))) /= 1) then
          write(*,*) 'SW:',sum_weight(RANGE2D)
          write(message,'(A)') trim(name)//' could not calculate weights, assumes equal weight everywhere'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING,ESMF_CONTEXT)
          if (operator == 'total') then
            weight(RANGE3D) = 1.0
          else
            weight(RANGE3D) = 1.0/(ubnd(3) - lbnd(3) + 1)
          endif
        endif
        deallocate(sum_weight)
      endif

      if (exportGridRank == 2 .and. exportRank == 2) then

        allocate(exportLbnd(2), stat=localrc)
        allocate(exportUbnd(2), stat=localrc)

        call ESMF_FieldGet(exportFieldList(i), localDe=0, farrayPtr=farrayPtr2, &
          exclusiveLbound=exportLbnd, exclusiveUbound=exportUbnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        if (any(ubnd(1:2) - lbnd(1:2) /= exportUbnd(1:2) - exportLbnd(1:2))) cycle

        select case(trim(operator))
        case ('total')
          farrayPtr2(RANGE2D) = &
            sum(farrayPtr3(RANGE3D) * layer_height(RANGE3D), dim=3, mask=(mask3(RANGE3D)>0))

        case ('average')
          farrayPtr2(RANGE2D) = &
            sum(farrayPtr3(RANGE3D) * weight(RANGE3D), dim=3, mask=(mask3(RANGE3D)>0))

        case ('minimum' )
          call ESMF_LogWrite('VR:'//trim(itemName),ESMF_LOGMSG_INFO,ESMF_CONTEXT)
          farrayPtr2(RANGE2D) = -maxval(-farrayPtr3(RANGE3D), dim=3, mask=(mask3(RANGE3D)>0))

          write(message,'(ES10.3,X,ES10.3)') maxval(farrayPtr3(RANGE3D),mask=(mask3(RANGE3D)>0)),minval(farrayPtr3(RANGE3D),mask=(mask3(RANGE3D)>0))
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO,ESMF_CONTEXT)
        case ('maximum' )
          farrayPtr2(RANGE2D) = maxval(farrayPtr3(RANGE3D), dim=3, mask=(mask3(RANGE3D)>0))
          write(message,'(ES10.3,X,ES10.3)') maxval(farrayPtr3(RANGE3D),mask=(mask3(RANGE3D)>0)),minval(farrayPtr3(RANGE3D),mask=(mask3(RANGE3D)>0))
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO,ESMF_CONTEXT)

        case default
          rc = ESMF_RC_NOT_IMPL
          call ESMF_LogWrite(trim(name)//' operator '//trim(operator)//' not implemented', ESMF_LOGMSG_ERROR)
          return
        end select
      else
        if (advanceCount < 2) then
          write(message, '(A)') trim(name)//' could not reduce non-rank 3 item '//trim(itemName)
        endif
      endif

      !if (advanceCount < 2) then
        write(message,'(A)') trim(name)//' reduced '
        call MOSSCO_FieldString(importFieldList(1), message)
        call MOSSCO_MessageAdd(message,' to ')
        call MOSSCO_FieldString(exportFieldList(i), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      !endif

      if (allocated(weight)) deallocate(weight)
      nullify(layer_height)
      nullify(mask3)
      nullify(mask2)
      nullify(farrayPtr3)
      nullify(farrayPtr2)

      call MOSSCO_Reallocate(importFieldList, 0,  rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      deallocate(exportLbnd, stat=localrc)
      deallocate(exportUbnd, stat=localrc)
      deallocate(lbnd, stat=localrc)
      deallocate(lbnd, stat=localrc)

    enddo

    if (associated(includeList)) deallocate(includeList)
    if (allocated(weight)) deallocate(weight)
    if (associated(mask2)) deallocate(mask2)
    if (associated(mask3)) deallocate(mask3)
    if (associated(depth)) deallocate(depth)
    if (associated(layer_height)) deallocate(layer_height)

    call MOSSCO_Reallocate(exportFieldList, 0,  rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_ReduceFields

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CreateVerticallyReducedField"
  subroutine MOSSCO_CreateVerticallyReducedField(importField, exportfield, kwe, &
    scale, offset, scale_depth, operator, name, rc)

    type(ESMF_Field), intent(in)           :: importField
    type(ESMF_Field), intent(out)          :: exportField
    type(ESMF_KeywordEnforcer), optional   :: kwe
    real(ESMF_KIND_R8), optional, intent(in) :: scale, offset, scale_depth
    character(len=*), optional, intent(in) :: operator
    character(len=*), optional, intent(in) :: name
    integer(ESMF_KIND_I4), optional        :: rc

    character(ESMF_MAXSTR)                 :: exportName, importName, unitString

    integer(ESMF_KIND_I4)                  :: localrc, rc_, rank, i
    real(ESMF_KIND_R8)                     :: scale_factor, offset_, scale_depth_
    character(len=ESMF_MAXSTR)             :: operator_, name_, message
    type(ESMF_FieldStatus_Flag)            :: fieldStatus
    type(ESMF_Grid)                        :: importGrid, exportGrid
    type(ESMF_TypeKind_Flag)               :: typeKind
    type(ESMF_Info)                        :: info 

    scale_factor = 1.0D0
    offset_ = 0.0D0
    scale_depth_ = 0.0D0
    rc_ = ESMF_SUCCESS
    if (present(rc))  rc = rc_
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(scale)) scale_factor = scale
    if (present(scale_depth)) scale_depth_ = scale_depth
    if (present(offset)) offset_ = offset
    if (present(operator)) operator_ = operator
    if (present(name)) then
      name_ = trim(name)
    else
      name_ = 'vertical_reduction'
    endif

    call ESMF_FieldGet(importField, name=importName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_FieldGet(importField, status=fieldStatus, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
      if (present(rc)) rc = ESMF_RC_ARG_BAD
      return
    endif

    call ESMF_FieldGet(importField, grid=importGrid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_GridGet(importGrid, rank=rank, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (rank /= 3) then
      if (present(rc)) rc = ESMF_RC_ARG_BAD
      return
    endif

    exportGrid = MOSSCO_GridCreateFromOtherGrid(importGrid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    exportName = 'vred_'//operator_(1:4)//'_'//trim(importName)

    exportField = ESMF_FieldEmptyCreate(name=trim(exportName), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_FieldEmptySet(exportField, exportGrid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_FieldGet(importField, typeKind=typeKind, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_FieldEmptyComplete(exportfield, typeKind=typeKind, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_FieldCopyInfo(exportField, importField, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_FieldInitialize(exportField, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoGetFromHost(exportField, info=info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(info, key='cell_methods', &
      value='vertical: '//trim(operator), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_InfoSet(info, key='creator', value=trim(name_), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    write(message,'(A)') trim(name)//' created '
    call MOSSCO_FieldString(exportField, message)

    !> For max, min, average, the unit stays the same, for total however, the unit
    !> must be multiplie by the unit of layer_height, which is here assumed to be 'm'
    if (trim(operator) == 'total') then

      call ESMF_InfoGetFromHost(importField, info=info, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_InfoGet(info, key='units', value=unitString, default='', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call MOSSCO_CleanUnit(unitString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      i = index(unitString,'m-3')
      if (i > 0) then
        unitString(i+2:i+2) = '2'
      else
        i = index(unitString,'m-1')
        if (i == 1) then
          unitString = unitString(3:len(unitString))
        elseif (i > 1) then
          unitString = unitString(1:i-1)//unitString(i+3:len(unitString))
        else
          unitString = trim(unitString)//' m'
        endif
      endif

      call ESMF_InfoGetFromHost(importField, info=info, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_InfoSet(info, key='units', value=trim(unitString),  rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call MOSSCO_MessageAdd(message,' with unit '//trim(unitString))
    endif
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  end subroutine MOSSCO_CreateVerticallyReducedField

end module vertical_reduction
