!> @brief Implementation of an ESMF time aggregation component
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2017, 2018 Helmholtz-Zentrum Geesthacht, Bundesanstalt für
!> Wasserbau
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Markus Kreus <markus.kreus@baw.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "time_aggregation_component.F90"

#define RANGE1D lbnd(1):ubnd(1)
#define RANGE2D RANGE1D,lbnd(2):ubnd(2)
#define RANGE3D RANGE2D,lbnd(3):ubnd(3)
#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module time_aggregation_component

  use esmf
  use mossco_strings
  use mossco_component
  use mossco_field
!  use mossco_fieldbundle
  use mossco_state
  use mossco_attribute
  use mossco_config
  use mossco_time

  implicit none
  private
  public :: SetServices

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer              :: localrc

    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

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
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: name, configFileName
    character(len=4096)        :: message
    type(ESMF_Time)            :: currTime
    integer(ESMF_KIND_I4)      :: localrc
    logical                    :: configIsPresent, labelIsPresent, fileIsPresent
    type(ESMF_Config)          :: config
    character(len=ESMF_MAXSTR), pointer :: filterExcludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer :: filterIncludeList(:) => null()

    rc  = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    !! Check whether there is a config file with the same name as this component
    !! If yes, load it, otherwise try to load one that is already present in the
    !! component.
    configIsPresent = .false.
    configfilename=trim(name)//'.cfg'
    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (fileIsPresent) then

      write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      config = ESMF_ConfigCreate(rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    else
      call ESMF_GridCompGet(gridComp, configIsPresent=configIsPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      if (configIsPresent) then
        call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

        write(message,'(A)')  trim(name)//' reads configuration from component'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    if (fileIsPresent .or. configIsPresent) then

      !> Default value for filter ExcludeList is a non-associated pointer
      call MOSSCO_ConfigGet(config, label='exclude', value=filterExcludeList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      !> Default value for filterIncludeList is a non-associated pointer
      call MOSSCO_ConfigGet(config, 'include', value=filterIncludeList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    endif

    if (associated(filterIncludeList)) then
      call MOSSCO_AttributeSet(importState, 'filter_pattern_include', filterIncludeList, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      call ESMF_AttributeGet(importState, 'filter_pattern_include', value=message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      write(message,'(A)') trim(name)//' include patterns: '//trim(message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      deallocate(filterIncludeList)
    endif

    if (associated(filterExcludeList)) then
      call MOSSCO_AttributeSet(importState, 'filter_pattern_exclude', &
        filterExcludeList, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      call ESMF_AttributeGet(importState, 'filter_pattern_exclude', value=message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      write(message,'(A)') trim(name)//' exclude patterns: '//trim(message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      deallocate(filterExcludeList)
    endif

    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: currTime, refTime
    type(ESMF_Time)         :: startTime, stopTime, maxTime, minTime, ringTime
    type(ESMF_TimeInterval) :: timeStep
    integer(ESMF_KIND_I8)   :: i, j, k, advanceCount
    real(ESMF_KIND_R8)      :: seconds
    integer(ESMF_KIND_I4)   :: itemCount, timeSlice, localPet,  petCount
    integer(ESMF_KIND_I4)   :: localrc, fieldCount, alarmCount, counter
    integer(ESMF_KIND_I4)   :: exportFieldCount, rank, matchIndex
    type(ESMF_StateItem_Flag), allocatable, dimension(:) :: itemTypeList
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: itemNameList
    type(ESMF_Clock)        :: clock
    logical                 :: isMatch
    logical ,save                :: needReset
    character(len=ESMF_MAXSTR) :: form
    type(ESMF_Alarm), dimension(:), allocatable :: alarmList
    integer(ESMF_KIND_I4), allocatable :: ubnd(:), lbnd(:), iubnd(:), ilbnd(:)

    character(len=ESMF_MAXSTR) :: message, name, timeUnit, alarmName
    character(len=ESMF_MAXSTR), allocatable :: filterIncludeList(:), filterExcludeList(:)
    character(len=ESMF_MAXSTR), allocatable :: options(:)
    type(ESMF_Field), allocatable :: fieldList(:), exportFieldList(:)
    type(ESMF_Field)              :: exportField, field
    type(ESMF_FieldBundle)        :: fieldBundle
    character(len=ESMF_MAXSTR), pointer :: include(:)=> null()
    type(ESMF_GeomType_Flag)      :: geomType
    type(ESMF_Grid)               :: grid
    type(ESMF_XGrid)              :: xgrid
    type(ESMF_LocStream)          :: locStream
    type(ESMF_Mesh)               :: mesh

    real(ESMF_KIND_R8), pointer :: farrayPtr2(:,:) => null()
    integer(ESMF_KIND_I4), pointer :: mask2(:,:) => null()

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    !> Synchronize this component's current time with the parent clock's
    call ESMF_ClockSet(clock, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, refTime=refTime, &
      startTime=startTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_GridCompGet(gridComp, petCount=petCount, localPet=localPet, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    ! Find in my alarmList couplings to any further calls to this component,
    ! if there are none, then
    ! remember that this is the last step before having to reset the fields
    call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_RINGING, &
      alarmCount=alarmCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_TimeGet(currTime, timeStringISOFrac=timestring, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    !> Skip if there are no alarms (that would be unusual at any time later
    !> than the first advanceCount, so warn about it)
    if (alarmCount == 0 .and. advanceCount > 0) then
      write(message,'(A)') trim(name)//' '//trim(timestring)//' did not find any ringing alarms'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      call MOSSCO_CompExit(gridComp, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)
      return
    endif

    needReset = .true.
    if (alarmCount > 0) then
      if (allocated(alarmList)) deallocate(alarmList)
      allocate(alarmList(alarmCount), stat=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_RINGING, &
        alarmList=alarmList,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      needReset = .false.

      do k=lbound(alarmList,1),ubound(alarmList,1)

        call ESMF_AlarmGet(alarmList(k), ringTime=ringTime, name=alarmName, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

        !! Skip this alarm if it is not a cplAlarm
        if (index(trim(alarmName),'cplAlarm') < 1) cycle

        call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

        write(message,'(A)') trim(name)//' '//trim(timeString)//' ringing alarm '//trim(alarmName)

        call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

        call MOSSCO_MessageAdd(message,' will ring next at '//trim(timestring))
        !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        if (trim(alarmName(1:index(alarmName,'--')-1)) == trim(name)) then
          ! 'getm--time_aggregation--cpAlarm'  => .false.
          ! 'time_aggregation--mossco_getm--cpAlarm' => .true.
          call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

          write(message,'(A)') trim(name)//' '//trim(timeString)//' resets aggregated variables now'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          needReset = .true.
          call ESMF_AlarmRingerOff(alarmList(k), rc=localrc)
          exit ! from k-loop over alarmList
        endif
      enddo
    endif

    call MOSSCO_AttributeGet(importState, 'filter_pattern_include', &
      filterIncludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call MOSSCO_AttributeGet(importState, 'filter_pattern_exclude', &
      filterExcludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)
    allocate(itemTypeList(itemCount), stat=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    allocate(itemNameList(itemCount), stat=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_StateGet(importState, itemTypeList=itemTypeList, &
      itemNameList=itemNameList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    do i=1, itemCount

      ! Only deal with fields and field bundles, otherwise skip
      if (itemTypeList(i) /= ESMF_STATEITEM_FIELD .and. &
        itemTypeList(i) /= ESMF_STATEITEM_FIELDBUNDLE) cycle

      ! Check whether the item is in any one of the exclude patterns, if matched,
      ! then skip this item
      isMatch = .false.
      call MOSSCO_StringMatch(itemNameList(i), filterExcludeList, &
        isMatch, localrc)
      if (isMatch) cycle

      if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(importState, itemNameList(i), field, rc=localrc)
        write(message,'(A)') trim(name)//' looks at '
        call MOSSCO_FieldString(field, message)
      else
        call ESMF_StateGet(importState, itemNameList(i), fieldBundle, rc=localrc)
        write(message,'(A)') trim(name)//' looks at bundle '//trim(itemNameList(i))
        !call MOSSCO_FieldBundleString(fieldBundle, message)
      endif
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      ! Check whether the item is in any one of the include patterns, if not
      ! matched in any, then skip this item
      !> @todo this is not correctly working for bundles, see implementation in
      !> netcdf_component
      isMatch = .true.
      call MOSSCO_StringMatch(itemNameList(i), filterIncludeList, &
        isMatch, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      if (.not.ismatch) cycle

      allocate(include(1), stat=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      include(1)=trim(itemNameList(i))
      call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
        include=include, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldCount < 1) then
        write(message,'(A)') trim(name)//' skipped non-field or incomplete item '
        call MOSSCO_MessageAdd(message,' '//itemNameList(i))
        if (advanceCount < 1) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        cycle
      endif

      if (advanceCount < 1) then
        do j=1, fieldCount
          write(message,'(A)') trim(name)//' will time aggregate '
          if (fieldCount > 1) write(message,'(A)') trim(message)//' bundled '
          call MOSSCO_MessageAdd(message,' '//itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        enddo
      endif

      include(1) = 'avg_'//trim(itemNameList(i))
      call MOSSCO_StateGetFieldList(exportState, exportFieldList, fieldCount=exportFieldCount, &
        include=include,  rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (exportFieldCount == 0) then

        if (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
          call ESMF_StateGet(importState, trim(itemNameList(i)), &
            fieldBundle, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldBundleGet(fieldBundle, geomType=geomType,rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (geomType == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldBundleGet(fieldBundle, grid=grid, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            fieldBundle = ESMF_FieldBundleCreate(name='avg_'//trim(itemNameList(i)),  rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
            call ESMF_FieldBundleSet(fieldBundle, grid=grid, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          elseif (geomType == ESMF_GEOMTYPE_MESH) then
            call ESMF_FieldBundleGet(fieldBundle, mesh=mesh, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            fieldBundle = ESMF_FieldBundleCreate(name='avg_'//trim(itemNameList(i)), rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
            call ESMF_FieldBundleSet(fieldBundle, mesh=mesh, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          elseif (geomType == ESMF_GEOMTYPE_XGRID) then
            call ESMF_FieldBundleGet(fieldBundle, xgrid=xgrid, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            fieldBundle = ESMF_FieldBundleCreate(name='avg_'//trim(itemNameList(i)), rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
            call ESMF_FieldBundleSet(fieldBundle, xgrid=xgrid, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          elseif (geomType == ESMF_GEOMTYPE_LOCSTREAM) then
            call ESMF_FieldBundleGet(fieldBundle, xgrid=xgrid, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            fieldBundle = ESMF_FieldBundleCreate(name='avg_'//trim(itemNameList(i)), rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
            call ESMF_FieldBundleSet(fieldBundle, locstream=locstream, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          endif

          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_AttributeSet(fieldBundle, 'creator', trim(name), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_StateAddReplace(exportState, (/fieldBundle/), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A)') trim(name)//' created fieldBundle avg_'//trim(itemNameList(i))
          if (advanceCount < 1) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif

        do j=1,fieldCount

          exportField = ESMF_FieldEmptyCreate(name='avg_'//trim(itemNameList(i)), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldGet(fieldList(j), geomType=geomType, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call MOSSCO_FieldCopy(exportField, fieldList(j), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_AttributeSet(exportField, 'creator', trim(name), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
 

          !call MOSSCO_FieldInitialize(exportField, value=0.0d0, &
          !  owner=trim(name), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A)') trim(name)//' created '

          if (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
            write(message,'(A)') trim(message)//' '//trim(itemNameList(i))//'/'
          endif

          allocate(options(2))
          options(:)=(/'creator','mean   '/)

          call MOSSCO_FieldString(exportField, message, options=options)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          deallocate(options)

          if (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_FieldBundleAdd(fieldBundle, (/exportField/), multiflag=.true., rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)
          endif

        enddo

        if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateAddReplace(exportState, (/exportField/), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)
        endif

      endif ! exportCount == 0, i.e. exportField not created yet

      include(1) = 'avg_'//trim(itemNameList(i))
      call MOSSCO_StateGetFieldList(exportState, exportFieldList, fieldCount=exportFieldCount, &
        include=include,  rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      if (needReset) then
        write(message,'(A)') trim(name)//' '//trim(timeString)//' resets  now '

        do j=1, exportFieldCount
          call MOSSCO_FieldInitialize(exportFieldList(j), value=0.0D0, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

          call ESMF_AttributeSet(exportFieldList(j), 'averaging_counter', 0, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)
        enddo
      else
        write(message,'(A)') trim(name)//' '//trim(timeString)//' does not need reset'
      endif
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      !> At this point, we are within the same itemName, but can have different
      !> order/index of fields in import and export State
      if (exportFieldCount /= fieldCount) then
        write(message,'(A,I1,A,I1)') trim(name)//' mismatch in field counts for item '//trim(itemNameList(i))//' ',fieldCount,'/=',exportFieldCount
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        localrc = ESMF_RC_ARG_BAD
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      do j=1, exportFieldCount

        write(message,'(A)') trim(name)//' looks at '
        allocate(options(4))
        options(:)=(/'creator','loc    ','geom   ','mean   '/)

        call MOSSCO_FieldString(exportFieldList(j), message, options=options)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        deallocate(options)

        call ESMF_FieldGet(exportfieldList(j), rank=rank, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

        if (allocated(ubnd)) deallocate(ubnd)
        if (allocated(lbnd)) deallocate(lbnd)
        allocate(ubnd(rank))
        allocate(lbnd(rank))

        call ESMF_FieldGetBounds(exportFieldList(j), exclusiveUbound=ubnd, &
          exclusiveLbound=lbnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

        call ESMF_AttributeGet(exportFieldList(j), 'averaging_counter', counter, &
          defaultValue=0, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

        counter = counter + 1
        call ESMF_AttributeSet(exportFieldList(j), 'averaging_counter', counter, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

        !> look for matching field in importState's selected item
        call MOSSCO_FieldMatchFields(exportFieldList(j), fieldList, &
          index=matchIndex, owner=trim(name), verbose=.true., rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (matchIndex < 1 .or. matchIndex > fieldCount) then
          write(message,'(A)') trim(name)//' skipped unmatched  '
          call MOSSCO_FieldString(exportFieldList(j), message)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
          return
        endif

        call MOSSCO_FieldAdd(exportFieldList(j), fieldList(matchIndex), verbose=.true., owner=trim(name), rc=localrc)

        write(message,'(A,I2.2)') trim(name)//' aggregated step ',counter
        if (fieldCount > 1) then
          write(message,'(A)') trim(message)//' bundled '
          call MOSSCO_MessageAdd(message,' '//itemNameList(i))
        endif
        call MOSSCO_FieldString(exportFieldList(j), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      enddo
    enddo

    if (associated(include)) deallocate(include)
    if (allocated(iubnd)) deallocate(iubnd)
    if (allocated(ilbnd)) deallocate(ilbnd)
    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(lbnd)) deallocate(lbnd)
    if (allocated(alarmList)) deallocate(alarmList)
    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)
    if (allocated(fieldList)) deallocate(fieldList)
    if (allocated(exportFieldList)) deallocate(exportFieldList)

    if (allocated(filterIncludeList)) deallocate(filterIncludeList)
    if (allocated(filterExcludeList)) deallocate(filterExcludeList)

    !! For this component, it does not make sense to advance its clock by a regular
    !! timestep.  Thus, it is advanced to the next alarm time.

    call MOSSCO_ClockGetTimeStepToNextAlarm(clock, timeStep, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_ClockAdvance(clock, timeStep=timeStep, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    integer(ESMF_KIND_I4)   :: localrc
    logical                 :: isPresent
    type(ESMF_Config)       :: config

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_GridCompGet(gridComp, configIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (isPresent) then

      call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      call ESMF_ConfigDestroy(config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    end if

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call MOSSCO_DestroyOwn(importState, trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    !> @temporarily disabled until fixed in mossco_state
    !> ESMCI_Container_F.C:321 ESMCI::Container::remove() Invalid argument  - key is not unique
    !> ESMF_FieldBundle.F90:3925 ESMF_FieldBundleRemove() Invalid argument
    !> mossco_state.F90:1454 MOSSCO_FieldBundleDestroyOwn Invalid argument
    !call MOSSCO_DestroyOwn(exportState, trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

  end subroutine Finalize

end module time_aggregation_component
