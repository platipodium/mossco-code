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
  use mossco_logging

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

      call MOSSCO_ConfigGet(config, 'exclude', filterExcludeList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(filterExcludeList)) then
        call MOSSCO_AttributeSet(cplComp, 'filter_pattern_exclude', filterExcludeList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call MOSSCO_ConfigGet(config, 'include', filterIncludeList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(filterIncludeList)) then
        call MOSSCO_AttributeSet(cplComp, 'filter_pattern_include', filterIncludeList, localrc)
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompGet(cplComp, configIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then

      call ESMF_CplCompGet(cplComp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigDestroy(config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    end if

    call MOSSCO_CompExit(cplComp, localrc)
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

    type(ESMF_Field), allocatable          :: importfieldList(:), exportFieldList(:)
    type(ESMF_Field), allocatable          :: fieldList(:)
    character(ESMF_MAXSTR), allocatable    :: differList(:)
    character(ESMF_MAXSTR)                 :: message, itemName, name
    integer(ESMF_KIND_I4)                  :: i, j, jj, fieldCount
    integer(ESMF_KIND_I4)                  :: importFieldCount, exportFieldCount

    logical                                 :: isPresent, tagOnly_, isMatch
    character(len=ESMF_MAXSTR), allocatable :: filterExcludeList(:), filterIncludeList(:)
    character(len=ESMF_MAXSTR), allocatable :: checkExcludeList(:)

    real(ESMF_KIND_R8)                     :: weight
    integer(ESMF_KIND_I4)                  :: localrc, rc_, matchIndex, matchScore
    integer(ESMF_KIND_I8)                  :: advanceCount
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

    ! It is better to compare startTime with currTime in a connector, as it could be
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

    call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
      fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount < 1) then
      write(message,'(A)') trim(name)//' found nothing to nudge from'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (present(rc)) rc=ESMF_SUCCESS
      return
    endif

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_include', filterIncludeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_exclude', filterExcludeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(checkExcludeList, 11, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @todo reconsider this list, especially the 'units' attribute should *not* be
    !> blacklisted
    checkExcludeList(1)='creator'
    checkExcludeList(2)='long_name'
    checkExcludeList(3)='coordinates'
    checkExcludeList(4)='units'
    checkExcludeList(5)='axis'
    checkExcludeList(6)='longname'
    checkExcludeList(7)='missing_value'
    checkExcludeList(8)='_FillValue'
    ! some MOSSCO-specific, internal attributes
    checkExcludeList(9)='nudging_weight'
    checkExcludeList(10)='nudging_component'
    checkExcludeList(11)='has_boundary_data'

    do i=1, fieldCount

      call ESMF_FieldGet(fieldList(i), name=itemName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! Look for an exclusion pattern on this itemName
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

      !! Look for an inclusion pattern on this field/bundle name
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

      call MOSSCO_StateGetFieldList(importState, importFieldList, fieldCount=importFieldCount, &
        itemSearch=trim(itemName), fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (importFieldCount < 1) cycle

      call MOSSCO_StateGetFieldList(exportState, exportFieldList, fieldCount=exportFieldCount, &
        itemSearch=trim(itemName), fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !call ESMF_LogWrite(trim(name)//' '//trim(itemName), ESMF_LOGMSG_INFO, ESMF_CONTEXT)

      !> if not item is found to nudge to, then skip this item
      if (exportFieldCount  < 1) cycle

      !call ESMF_LogWrite(trim(name)//' '//trim(itemName), ESMF_LOGMSG_INFO, ESMF_CONTEXT)

      !> Check same number of items
      if (importFieldCount /= exportFieldCount) then
        write(message,'(A)') trim(name)
        call MOSSCO_MessageAdd(message,' item numbers differ for skipped ')
        call MOSSCO_MessageAdd(message,' '//trim(itemName))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        call MOSSCO_Log(exportState)
        call MOSSCO_Log(importState)
        cycle
      end if

      !call ESMF_LogWrite(trim(name)//' '//trim(itemName), ESMF_LOGMSG_INFO, ESMF_CONTEXT)

      !> Find item in exportFieldList that best matches the fieldList(i) item
      call MOSSCO_FieldMatchFields(fieldlist(i), exportFieldList, index=matchIndex, &
        score=matchScore, owner=trim(name), rc=localrc)
      if (matchIndex > 0) then

        call MOSSCO_FieldWeightField(exportFieldList(matchIndex), fieldList(i), &
          weight, tagOnly=tagOnly_, owner=trim(name), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif

      call MOSSCO_Reallocate(importFieldList, 0,  rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_Reallocate(exportFieldList, 0,  rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    enddo

    call MOSSCO_Reallocate(checkExcludeList, 0,  rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(filterIncludeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(filterExcludeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_WeightImportIntoExportState

end module nudge_connector
