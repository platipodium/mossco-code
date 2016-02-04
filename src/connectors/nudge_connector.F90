!> @brief Implementation of an ESMF nudging connector component
!> @file nudge_connector.F90
!!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2016 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "nudge_connector.F90"

module nudge_connector

  use esmf
  use mossco_field
  use mossco_state
  use mossco_component
  use mossco_config
  use mossco_attribute

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_cplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_cplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_cplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
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

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: localrc

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

    call ESMF_StateReconcile(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateReconcile(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    real(ESMF_KIND_R8)              :: weight
    logical                         :: labelIsPresent, isPresent, fileIsPresent
    character(len=ESMF_MAXSTR), allocatable :: filterExcludeList(:), filterIncludeList(:)

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    configfilename=trim(name)//'.cfg'
    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (fileIsPresent) then

      write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      !> @todo deal with already existing config
      config = ESMF_ConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigFindLabel(config, label='weight:', isPresent=labelIsPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (labelIsPresent) then
        call ESMF_ConfigGetAttribute(config, weight, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,ES9.2)')  trim(name)//' found in file '//trim(configFileName)//' weight: ',weight
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        weight = 0.0
      endif

      call ESMF_AttributeGet(cplComp, 'weight', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isPresent) then
        call ESMF_AttributeGet(cplComp, 'weight', value=weight, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,ES9.2)')  trim(name)//' found attribute weight: ',weight
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      if (weight < 0.0) then
        weight = 0.0
        write(message,'(A,ES9.2)')  trim(name)//' found invalid weight < 0, reset to ',weight
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      elseif (weight > 1.0) then
        weight = 1.0
        write(message,'(A,ES9.2)')  trim(name)//' found invalid weight > 1, reset to ',weight
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      endif

      call ESMF_AttributeSet(cplComp, 'weight', weight, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_ConfigGetList(config, 'exclude:', filterExcludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(filterExcludeList)) then
        call MOSSCO_AttributeSetList(cplComp, 'filter_pattern_exclude', filterExcludeList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call MOSSCO_ConfigGetList(config, 'include:', filterIncludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(filterIncludeList)) then
        call MOSSCO_AttributeSetList(cplComp, 'filter_pattern_include', filterIncludeList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_cplCompSet(cplComp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call MOSSCO_WeightImportIntoExportState(cplComp, importState, &
      exportState, tagOnly=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_cplComp)     :: cplComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock
    integer, intent(out)    :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    integer(ESMF_KIND_I4)   :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_WeightImportIntoExportState(cplComp, importState, exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Finally, log the successful completion of this function
    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_cplComp)   :: cplComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    integer(ESMF_KIND_I4)   :: localrc

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompGet(cplComp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockDestroy(clock, rc=rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_WeightImportIntoExportState"
  subroutine MOSSCO_WeightImportIntoExportState(cplComp, importState, exportState, kwe, tagOnly, rc)

    type(ESMF_CplComp), intent(in)         :: cplComp
    type(ESMF_State)                       :: importState, exportState
    logical, intent(in), optional          :: kwe, tagOnly
    integer(ESMF_KIND_I4), optional        :: rc

    type(ESMF_Field)                       :: exportField, importField
    character(ESMF_MAXSTR), allocatable    :: itemNameList(:)
    character(ESMF_MAXSTR)                 :: message, itemName, importCreator, name
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_StateItem_Flag)              :: itemType
    type(ESMF_FieldStatus_Flag)            :: fieldStatus
    integer(ESMF_KIND_I4)                  :: i, j, itemCount, rank, exportRank, int4
    integer(ESMF_KIND_I8)                  :: numChanged, advanceCount, int8
    integer(ESMF_KIND_I4), allocatable     :: ubnd(:), lbnd(:)
    integer(ESMF_KIND_I4), allocatable     :: exportUbnd(:), exportLbnd(:)

    real(ESMF_KIND_R8), pointer            :: importPtr3(:,:,:), exportPtr3(:,:,:)
    real(ESMF_KIND_R8), pointer            :: importPtr2(:,:), exportPtr2(:,:)
    logical, allocatable                   :: mask2(:,:), mask3(:,:,:)
    logical                                :: isMatch, isPresent, tagOnly_
    character(len=ESMF_MAXSTR), allocatable :: filterExcludeList(:), filterIncludeList(:)
    real(ESMF_KIND_R8)                     :: exportMissingValue, importMissingValue
    real(ESMF_KIND_R8)                     :: weight, real8
    real(ESMF_KIND_R4)                     :: real4
    type(ESMF_TypeKind_Flag)               :: typeKind

    integer(ESMF_KIND_I4)                  :: localrc, rc_
    type(ESMF_Clock)                       :: clock
    type(ESMF_Time)                        :: startTime, currTime
    type(ESMF_TimeInterval)                :: timeStep

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    if (present(kwe)) rc = rc_
    if (present(tagOnly)) then
      tagOnly_ = tagOnly
    else
      tagOnly_ = .false.
    endif

    call ESMF_CplCompGet(cplComp, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(cplComp, name='weight', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(cplComp, name='weight', value=weight, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      weight = 1.0
    endif

    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! It is better to compare startTime with currTime in a connecter, as it could be
    ! called multiple times for a single timeStep
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, &
      timeStep=timeStep, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (currTime > startTime) then
      if (advanceCount < 1) advanceCount = nint((currTime - startTime) / timeStep)
      if (advanceCount < 1) advanceCount = 1
    else
      advanceCount = 0
    endif

    if (.not. tagOnly_) then
      if (weight <= 0.0) then
        if (advanceCount < 1) then
          write(message,'(A,ES9.2)')  trim(name)//' enabled but weight is ', weight
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        endif
        if (present(rc)) rc=ESMF_SUCCESS
        return
      endif
    endif

    call MOSSCO_StateLog(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(importState, itemNameList=itemNameList, itemTypeList=itemTypeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_AttributeGetList(cplComp, 'filter_pattern_include', filterIncludeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_AttributeGetList(cplComp, 'filter_pattern_exclude', filterExcludeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, itemCount

      itemName=trim(itemNameList(i))

      ! Look for an exclusion pattern on this field name
      if (allocated(filterExcludeList)) then
        do j=1,ubound(filterExcludeList,1)
          call MOSSCO_StringMatch(itemName, filterExcludeList(j), isMatch, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (ismatch) exit
        enddo
        if (ismatch .and. advanceCount < 1) then
          write(message,'(A)')  trim(name)//' excluded item'
          call MOSSCO_MessageAdd(message, trim(itemName))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif
      endif

      !! Look for an inclusion pattern on this field name
      if (allocated(filterIncludeList)) then
        do j=1,ubound(filterIncludeList,1)
          call MOSSCO_StringMatch(itemName, filterIncludeList(j), isMatch, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (ismatch) exit
        enddo
        if (.not.ismatch .and. advanceCount < 1) then
          write(message,'(A)')  trim(name)//' did not include'
          call MOSSCO_MessageAdd(message, ' '//trim(itemName))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif
      endif

      call ESMF_StateGet(exportState, trim(itemName), itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Make sure that import state's item type (in list) and export state's agree. Also,
      !> now only fields are implemented
      if (itemType /= itemTypeList(i)) then
        write(message,*) 'itemType not matching for item ',trim(itemName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      end if
      if (itemType /= ESMF_STATEITEM_FIELD) then
        write(message,*) 'fieldBundle not implemented, skip item ',trim(itemName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      call ESMF_StateGet(exportState, trim(itemName), exportField, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(exportField, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) cycle

      call ESMF_FieldGet(exportField, rank=exportRank, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateGet(importState, trim(itemName), importField, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(importField, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) cycle

      call ESMF_FieldGet(importField, rank=rank, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (rank /= exportRank) then
        write(message,'(A)')  trim(name)//' rank mismatch in '
        call MOSSCO_FieldString(exportField, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize()
      endif

      if (rank > 0) then
        if (allocated(exportUbnd)) deallocate(exportUbnd)
        allocate(exportUbnd(rank), stat=localrc)
        if (allocated(exportLbnd)) deallocate(exportLbnd)
        allocate(exportLbnd(rank), stat=localrc)
        if (allocated(lbnd)) deallocate(lbnd)
        allocate(lbnd(rank), stat=localrc)
        lbnd(:) = 1
        if (allocated(ubnd)) deallocate(ubnd)
        allocate(ubnd(rank), stat=localrc)
        ubnd(:) = 0
      endif

      call ESMF_FieldGetBounds(importField, exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGetBounds(exportField, exclusiveUBound=exportUbnd, exclusiveLBound=exportLbnd, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (any(exportLbnd - lbnd > 0) .or. any(exportUbnd - ubnd > 0))  then
        write(message,'(A)')  trim(name)//' exclusive bounds mismatch in '
        call MOSSCO_FieldString(exportField, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize()
      endif

      if (allocated(exportUbnd)) deallocate(exportUbnd)
      if (allocated(exportLbnd)) deallocate(exportLbnd)

      call ESMF_AttributeGet(importField, 'missing_value', &
        isPresent=isPresent,  rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (isPresent) then
        call ESMF_AttributeGet(importField, 'missing_value', typeKind=typeKind, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (typeKind == ESMF_TYPEKIND_R8) then
          call ESMF_AttributeGet(importField, 'missing_value', real8, rc=localrc)
          importMissingValue = real8
        elseif (typeKind == ESMF_TYPEKIND_R4) then
          call ESMF_AttributeGet(importField, 'missing_value', real4, rc=localrc)
          importMissingValue = dble(real4)
        elseif (typeKind == ESMF_TYPEKIND_I8) then
          call ESMF_AttributeGet(importField, 'missing_value', int8, rc=localrc)
          importMissingValue = dble(int8)
        elseif (typeKind == ESMF_TYPEKIND_I4) then
          call ESMF_AttributeGet(importField, 'missing_value', int4, rc=localrc)
          importMissingValue = dble(int4)
        else
          write(message,'(A)')  '  missing value non-implemented type '
          call MOSSCO_FieldString(importField, message)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        importMissingValue=-1.0E30
      endif

      call ESMF_AttributeGet(exportField, name='missing_value', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isPresent) then
        call ESMF_AttributeGet(exportField, 'missing_value', typeKind=typeKind, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (typeKind == ESMF_TYPEKIND_R8) then
          call ESMF_AttributeGet(exportField, 'missing_value', real8, rc=localrc)
          exportMissingValue = real8
        elseif (typeKind == ESMF_TYPEKIND_R4) then
          call ESMF_AttributeGet(exportField, 'missing_value', real4, rc=localrc)
          exportMissingValue = dble(real4)
        elseif (typeKind == ESMF_TYPEKIND_I8) then
          call ESMF_AttributeGet(exportField, 'missing_value', int8, rc=localrc)
          exportMissingValue = dble(int8)
        elseif (typeKind == ESMF_TYPEKIND_I4) then
          call ESMF_AttributeGet(exportField, 'missing_value', int4, rc=localrc)
          exportMissingValue = dble(int4)
        else
          write(message,'(A)')  '  missing value non-implemented type '
          call MOSSCO_FieldString(exportField, message)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        exportMissingValue=-1.0E30
      endif

      call ESMF_AttributeSet(exportField, 'nudging_weight', weight, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeGet(importField, 'creator', importCreator, defaultValue='unknown', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(exportField, 'nudging_component', trim(importCreator), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> @todo The has_boundary_data attribute is set to .true. by default here,
      !> this should be changed by a configuration attribute and the attribute name MOSSCO_AttributeGetList
      !> be synchronized with the transporting component (e.g. getm_component)
      call ESMF_AttributeSet(exportField, 'has_boundary_data', .true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (.not. tagOnly_) then
        numChanged = 0
        select case (rank)
          case(2)
            call ESMF_FieldGet(importField, farrayPtr=importPtr2, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            call ESMF_FieldGet(exportField, farrayPtr=exportPtr2, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            if (allocated(mask2)) deallocate(mask2)
            allocate(mask2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)), stat=localrc)
            !> @todo add a mask (also for 3d) working on the missingValue
            !mask2 = (abs(exportPtr2 - exportMissingValue) > tiny(1.0))
            !mask2 = ((abs(importPtr2 - importMissingValue) >  tiny(1.0)) .and. mask2)
            mask2 = (exportPtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) .ge. 0.0 &
              .and. (importPtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) .ge. 0.0 ))
            numChanged = count(mask2)
            if (numChanged>0) then
              where (mask2)
                exportPtr2 = (1.0 - weight) * exportPtr2 + weight * importPtr2
              endwhere
            endif
            if (allocated(mask2)) deallocate(mask2)
          case(3)
            call ESMF_FieldGet(importField, farrayPtr=importPtr3, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            call ESMF_FieldGet(exportField, farrayPtr=exportPtr3, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            if (allocated(mask3)) deallocate(mask3)
            allocate(mask3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)), stat=localrc)
            mask3 = (exportPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) .ge. 0.0 &
              .and. (importPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) .ge. 0.0 ))

            numChanged = count(mask3)
            if (numChanged>0) then
              where (mask3)
                exportPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) = (1.0 - weight) &
                  * exportPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) &
                  + weight * importPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3))
              endwhere
            endif
            if (allocated(mask3)) deallocate(mask3)
          case default
            if (allocated(lbnd)) deallocate(lbnd)
            if (allocated(ubnd)) deallocate(ubnd)
            if (present(rc)) rc=ESMF_RC_NOT_IMPL
            return
        endselect

        if (numChanged>0) then
          write(message,'(A,ES9.2,A,I5.5,A)') trim(name)//' weight ', weight, ' ', numChanged, ' cells '
          call MOSSCO_FieldString(exportField, message)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif
      endif

      if (allocated(lbnd)) deallocate(lbnd)
      if (allocated(ubnd)) deallocate(ubnd)

    enddo

    call MOSSCO_Reallocate(filterIncludeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(filterExcludeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc=rc_

    return

  end subroutine MOSSCO_WeightImportIntoExportState

end module nudge_connector
