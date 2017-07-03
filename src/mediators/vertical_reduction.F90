!> @brief Implementation of an ESMF mediator component that reduces
!> the vertical dimension by avg/sum/min/max operations
!> @file vertical_reduction.F90
!!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2016, 2017 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
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
  use mossco_attribute
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

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateReconcile(importState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateReconcile(exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, localrc)
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
    real(ESMF_KIND_R8)              :: offset, scale
    character(len=ESMF_MAXSTR)      :: operator
    logical                         :: labelIsPresent, isPresent, fileIsPresent
    character(len=ESMF_MAXSTR), allocatable :: filterExcludeList(:), filterIncludeList(:)

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Default values for operations
    operator = 'average'
    scale = 1.0
    offset = 0.0

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

      call MOSSCO_ConfigGet(config, label='scale', value=scale, defaultValue=1.0D0, rc=localrc)
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
    case ('product')
    !case ('norm')
    case default
      write(message, '(A)') trim(name)//' obtained invalid operator '//trim(operator)
      call MOSSCO_CompExit(cplComp, rc)
      rc = ESMF_RC_ARG_BAD
      return
    end select

    !> Add all configurable options as attributes
    if (allocated(filterExcludeList)) then
      call MOSSCO_AttributeSet(cplComp, 'filter_pattern_exclude', filterExcludeList, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    if (allocated(filterIncludeList)) then
      call MOSSCO_AttributeSet(cplComp, 'filter_pattern_include', filterIncludeList, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call ESMF_AttributeSet(cplComp, 'operator_type', trim(operator), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(cplComp, 'scale_factor', offset, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(cplComp, 'add_offset', offset, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, rc)
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

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CreateVerticallyReducedExportFields(cplComp, importState, exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_ReduceFields(cplComp, importState, exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !! Finally, log the successful completion of this function
    call MOSSCO_CompExit(cplComp, localrc)
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

    call MOSSCO_CompExit(cplComp, localrc)
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

    logical                                 :: isPresent, tagOnly_, isMatch
    character(len=ESMF_MAXSTR), allocatable :: filterExcludeList(:), filterIncludeList(:)
    character(len=ESMF_MAXSTR), allocatable :: checkExcludeList(:)

    real(ESMF_KIND_R8)                     :: offset, scale
    integer(ESMF_KIND_I4)                  :: localrc, rc_, matchIndex, matchScore
    integer(ESMF_KIND_I8)                  :: advanceCount
    type(ESMF_Clock)                       :: clock
    type(ESMF_Time)                        :: startTime, currTime
    type(ESMF_TimeInterval)                :: timeStep

    type(ESMF_FieldStatus_Flag)            :: fieldStatus
    type(ESMF_Grid)                        :: grid
    type(ESMF_Field)                       :: exportField

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_CplCompGet(cplComp, name=name, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_AttributeGet(cplComp, name='add_offset', defaultValue=0.0D0, value=offset, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_AttributeGet(cplComp, name='operator_type', defaultValue='average', value=operator, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_AttributeGet(cplComp, name='scale_factor', defaultValue=1.0D0, value=scale, rc=localrc)
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

    call MOSSCO_StateGetFieldList(importState, importFieldList, fieldCount=importFieldCount, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (importFieldCount < 1 .and. advanceCount < 2) then
      write(message,'(A)') trim(name)//' found no fields to reduce'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_include', filterIncludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_exclude', filterExcludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    do i=1, importFieldCount

      call ESMF_FieldGet(importFieldList(i), name=itemName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      ! Look for an exclusion pattern on this itemName
      if (allocated(filterExcludeList)) then
        do j=1,ubound(filterExcludeList,1)
          call MOSSCO_StringMatch(itemName, filterExcludeList(j), isMatch, localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

          if (ismatch) exit
        enddo
        if (ismatch .and. advanceCount < 2) then
          write(message,'(A)')  trim(name)//' excluded item'
          call MOSSCO_MessageAdd(message, trim(itemName))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif
      endif

      !! Look for an inclusion pattern on this field/bundle name
      if (allocated(filterIncludeList)) then
        do j=1,ubound(filterIncludeList,1)
          call MOSSCO_StringMatch(itemName, filterIncludeList(j), isMatch, localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
          if (ismatch) exit
        enddo
        if (.not.ismatch .and. advanceCount < 2) then
          write(message,'(A)')  trim(name)//' did not include'
          call MOSSCO_MessageAdd(message, ' '//trim(itemName))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif
      endif

      !> @todo add later capability for field bundles, for now
      !> get a temporary fieldList with all items mathcing itemName
      call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
        itemSearch=trim(itemName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call MOSSCO_Reallocate(fieldList, 0,  rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (fieldCount /= 1) cycle

      call ESMF_FieldGet(importFieldList(i), status=fieldStatus, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) cycle

      !> Found out whether this field has a vertical dimension, if not, then cycle
      call ESMF_FieldGet(importFieldList(i), grid=grid, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_GridGet(grid, rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (rank /= 3) cycle

      ! The field is created from gridset and complete fields, no error is thrown if
      ! the field exists
      call MOSSCO_StateGetFieldList(exportState, exportFieldList, fieldCount=exportFieldCount, &
        itemSearch='vred_'//trim(itemName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (exportFieldCount < 1) then
        call MOSSCO_CreateVerticallyReducedField(importFieldList(i), exportField, operator=operator, &
          scale=scale, offset=offset, rc=localrc)

        if (localrc == ESMF_SUCCESS) then
          call ESMF_StateAddReplace(exportState, (/exportField/), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        endif
      endif

      call MOSSCO_Reallocate(exportFieldList, 0,  rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    enddo

    call MOSSCO_Reallocate(importFieldList, 0,  rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_Reallocate(filterIncludeList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_Reallocate(filterExcludeList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

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

    logical                                 :: isPresent
    character(len=ESMF_MAXSTR), allocatable :: filterExcludeList(:), filterIncludeList(:)

    real(ESMF_KIND_R8)                     :: offset, scale
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

    integer(ESMF_KIND_I4),dimension(:,:,:), pointer :: mask => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:) :: farrayPtr3 => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:)   :: farrayPtr2 => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)   :: layer_height => null()
    real(ESMF_KIND_R8), allocatable, dimension(:,:,:)   :: weight
    character(len=ESMF_MAXSTR)             :: importItemName
    type(ESMF_TypeKind_Flag)               :: typeKind

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_CplCompGet(cplComp, name=name, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_AttributeGet(cplComp, 'add_offset', value=offset, defaultValue=0.0D0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_AttributeGet(cplComp, name='operator_type', defaultValue='average', value=operator, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_AttributeGet(cplComp, name='scale_factor', defaultValue=1.0D0, value=scale, rc=localrc)
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

    call MOSSCO_StateGetFieldList(exportState, exportFieldList, fieldCount=exportFieldCount, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (exportFieldCount < 1 .and. advanceCount < 2) then
      write(message,'(A)') trim(name)//' found no fields to reduce'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (present(rc)) rc=ESMF_SUCCESS
      return
    endif

    do i=1, exportFieldCount

      call ESMF_FieldGet(exportFieldList(i), name=itemName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (itemName(1:5) /= 'vred_') cycle
      importItemName = itemName(6:len_trim(itemName))

      !> Find the name of the variable on which the reduction shall be applied
      !call ESMF_AttributeGet(exportFieldList(i), name='source', isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      !if (.not.isPresent()) cycle

      !call ESMF_AttributeGet(exportFieldList(i), name='source', value=importItemName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call MOSSCO_StateGetFieldList(importState, importFieldList, fieldCount=importFieldCount, &
        itemSearch=trim(importItemName), fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      !call ESMF_LogWrite(trim(name)//' in loop'//trim(importItemName), ESMF_LOGMSG_INFO)

      !> if not found, or if multiple fields with the same name, then skip this
      !> @todo add later capability for field bundles
      if (importFieldCount /= 1) cycle

      !call ESMF_LogWrite(trim(name)//' importfielCount', ESMF_LOGMSG_INFO)

      !> Complete the field if it is gridset
      call ESMF_FieldGet(exportFieldList(i), status=exportFieldStatus,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (exportfieldStatus /= ESMF_FIELDSTATUS_COMPLETE) cycle

      call ESMF_FieldGet(exportFieldList(i), grid=exportGrid, rank=exportRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_FieldGet(importFieldList(1), grid=importGrid, rank=importRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_GridGet(exportGrid, rank=exportGridRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_GridGet(importGrid, rank=importGridRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.importRank == 3) cycle
      allocate(lbnd(3), stat=localrc)
      allocate(ubnd(3), stat=localrc)

      if (.not.importGridRank == 3) cycle

      call ESMF_FieldGet(importFieldList(1),  localDe=0, farrayPtr=farrayPtr3, &
        exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call MOSSCO_GridGetDepth(importGrid, height=layer_height, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (allocated(weight)) deallocate(weight)
      allocate(weight(RANGE3D), stat=localrc)
      weight(RANGE3D) = 0.0

      do j = lbnd(3), ubnd(3)
        where (layer_height(RANGE2D,j) > 0)
          weight(RANGE2D,j) = layer_height(RANGE2D,j)/sum(layer_height(RANGE3D), dim=3)
        endwhere
      enddo

      call ESMF_GridGetItem(importGrid, ESMF_GRIDITEM_MASK, &
        staggerloc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (isPresent) then
        call ESMF_GridGetItem(importGrid, ESMF_GRIDITEM_MASK, &
          staggerloc=ESMF_STAGGERLOC_CENTER,farrayPtr=mask, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      else
        if (associated(mask)) deallocate(mask)
        allocate(mask(RANGE3D))
      endif

      if (exportGridRank == 2 .and. exportRank == 2) then

        allocate(exportLbnd(2), stat=localrc)
        allocate(exportUbnd(2), stat=localrc)

        call ESMF_FieldGet(exportFieldList(i), localDe=0, farrayPtr=farrayPtr2, exclusiveLbound=exportLbnd, &
          exclusiveUbound=exportUbnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        if (any(ubnd(1:2) - lbnd(1:2) /= exportUbnd(1:2) - exportLbnd(1:2))) cycle

        select case(trim(operator))
        case ('total')
          farrayPtr2(exportLbnd(1):exportUbnd(1),exportLbnd(2):exportUbnd(2)) = &
            sum(farrayPtr3(RANGE3D) * layer_height(RANGE3D), dim=3, mask=(mask(RANGE3D)>0))
        case ('average')
          farrayPtr2(exportLbnd(1):exportUbnd(1),exportLbnd(2):exportUbnd(2)) = &
            sum(farrayPtr3(RANGE3D) * weight(RANGE3D), dim=3, mask=(mask(RANGE3D)>0))
            write(0,*) 'AVG: ',maxval(layer_height),maxval(weight),maxval(farrayPtr3)
        case ('minimum' )
          farrayPtr2(exportLbnd(1):exportUbnd(1),exportLbnd(2):exportUbnd(2)) = &
            minval(farrayPtr3(RANGE3D) * weight(RANGE3D), dim=3, mask=(mask(RANGE3D)>0))
        case ('maximum' )
          farrayPtr2(exportLbnd(1):exportUbnd(1),exportLbnd(2):exportUbnd(2)) = &
            maxval(farrayPtr3(RANGE3D) * weight(RANGE3D), dim=3, mask=(mask(RANGE3D)>0))
        !case ('norm', 'product')
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

      if (advanceCount < 2) then
        write(message,'(A)') trim(name)//' reduced '
        call MOSSCO_FieldString(importFieldList(1), message)
        call MOSSCO_MessageAdd(message,' to ')
        call MOSSCO_FieldString(exportFieldList(i), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      if (allocated(weight)) deallocate(weight)
      nullify(layer_height)
      nullify(mask)
      nullify(farrayPtr3)
      nullify(farrayPtr2)

      call MOSSCO_Reallocate(importFieldList, 0,  rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      deallocate(exportLbnd, stat=localrc)
      deallocate(exportUbnd, stat=localrc)
      deallocate(lbnd, stat=localrc)
      deallocate(lbnd, stat=localrc)

    enddo

    call MOSSCO_Reallocate(exportFieldList, 0,  rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_ReduceFields

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CreateVerticallyReducedField"
  subroutine MOSSCO_CreateVerticallyReducedField(importField, exportfield, kwe, &
    scale, offset, operator, rc)

    type(ESMF_Field), intent(in)           :: importField
    type(ESMF_Field), intent(out)          :: exportField
    type(ESMF_KeywordEnforcer), optional   :: kwe
    real(ESMF_KIND_R8), optional, intent(in) :: scale, offset
    character(len=*), optional, intent(in) :: operator
    integer(ESMF_KIND_I4), optional        :: rc

    character(ESMF_MAXSTR)                 :: exportName, importName

    integer(ESMF_KIND_I4)                  :: localrc, rc_, rank
    real(ESMF_KIND_R8)                     :: scale_, offset_
    character(len=ESMF_MAXSTR)             :: operator_
    type(ESMF_FieldStatus_Flag)            :: fieldStatus
    type(ESMF_Grid)                        :: importGrid, exportGrid
    type(ESMF_TypeKind_Flag)               :: typeKind

    rc_ = ESMF_SUCCESS
    if (present(rc))  rc = rc_
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(scale)) scale_ = scale
    if (present(offset)) offset_ = offset
    if (present(operator)) operator_ = operator

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

    exportName = 'vred_' // trim(importName)

    exportField = ESMF_FieldEmptyCreate(name=trim(exportName), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_FieldEmptySet(exportField, exportGrid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_FieldGet(importField, typeKind=typeKind, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_FieldEmptyComplete(exportfield, typeKind=typeKind, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_FieldInitialize(exportField, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  end subroutine MOSSCO_CreateVerticallyReducedField

end module vertical_reduction
