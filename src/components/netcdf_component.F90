!> @brief Implementation of an ESMF netcdf output component
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2021-2022 Helmholtz-Zentrum Hereon
!> @copyright Copyright 2014-2021 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "netcdf_component.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module netcdf_component

  use esmf
  use mossco_variable_types
  use mossco_netcdf
  use mossco_strings
  use mossco_component
  use mossco_field
  use mossco_state
  use mossco_attribute
  use mossco_config
  use mossco_time

  implicit none
  private

  type(type_mossco_netcdf)   :: nc

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
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
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
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
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

    character(len=ESMF_MAXSTR) :: name, configFileName, fileName
    character(len=4096)        :: message
    character(len=ESMF_MAXSTR) :: timeString, timeUnit, form
    type(ESMF_Time)            :: currTime, refTime
    type(ESMF_Clock)           :: clock
    integer(ESMF_KIND_I4)      :: localrc, localPet, petCount, j
    logical                    :: fileIsPresent, configIsPresent, labelIsPresent
    type(ESMF_Config)          :: config
    character(len=ESMF_MAXSTR), pointer :: filterExcludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer :: filterIncludeList(:) => null()
    logical                    :: checkNaN, checkInf

    rc  = ESMF_SUCCESS
    checkNaN = .true.
    checkInf = .true.

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !! The default filename to read is the name of the component with a .nc
    !! extension
    fileName=trim(name)//'.nc'

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
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    else
      call ESMF_GridCompGet(gridComp, configIsPresent=configIsPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (configIsPresent) then
        call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message,'(A)')  trim(name)//' reads configuration from component'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    if (fileIsPresent .or. configIsPresent) then

      !> Value of netcdf fileName defaults to name of component
      call MOSSCO_ConfigGet(config, label='filename', value=fileName, &
        defaultValue=trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, label='exclude', value=filterExcludeList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Value of checkNaN defaults to .true.
      call MOSSCO_ConfigGet(config, label='checkNaN', value=checkNaN, defaultValue=.true., rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Value of checkInf defaults to .true.
      call MOSSCO_ConfigGet(config, label='checkInf', value=checkInf, defaultValue=.true., rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Default value for filter ExcludeList is a non-allocated field
      call MOSSCO_ConfigGet(config, label='exclude', value=filterExcludeList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Default value for filterIncludeList is a non-allocated field
      call MOSSCO_ConfigGet(config, 'include', value=filterIncludeList, &
        isPresent=labelIsPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    endif

    if (associated(filterIncludeList)) then
      call MOSSCO_AttributeSet(importState, 'filter_pattern_include', filterIncludeList, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeGet(importState, 'filter_pattern_include', value=message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' include patterns: '//trim(message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      deallocate(filterIncludeList)
    endif

    if (associated(filterExcludeList)) then
      call MOSSCO_AttributeSet(importState, 'filter_pattern_exclude', filterExcludeList, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeGet(importState, 'filter_pattern_exclude', value=message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' exclude patterns: '//trim(message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      deallocate(filterExcludeList)
    endif

    call MOSSCO_AttributeSet(importState, 'check_inf', checkInf, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_AttributeSet(importState, 'check_nan', checkNaN, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_AttributeSet(gridComp, 'check_inf', checkInf, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_AttributeSet(gridComp, 'check_nan', checkNaN, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(importState, 'filename', trim(fileName), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(clock, refTime=refTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_TimeGet(refTime, timeStringISOFrac=timeString, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    write(timeUnit,'(A)') 'seconds since '//timeString(1:10)//' '//timestring(12:len_trim(timestring))

    call ESMF_GridCompGet(gridComp, petCount=petCount, localPet=localPet, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeGet(importState, name='filename', value=fileName, &
      defaultValue=trim(name)//'.nc', rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    j=index(fileName,'.nc',back=.true.)
    if (j < len_trim(fileName)-2) fileName=trim(fileName)//'.nc'

    if (petCount>1) then
      write(form,'(A)')  '(A,'//trim(intformat(int(petCount-1,kind=8)))//',A)'
      write(fileName,form) filename(1:index(filename,'.nc')-1)//'.',localPet,'.nc'
    endif

    nc = mossco_netcdfCreate(fileName, timeUnit=timeUnit, state=importState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(exportState, 'netcdf_id', nc%ncid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(exportState, 'netcdf_file_name', trim(fileName), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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
    type(ESMF_Time)         :: startTime, stopTime, maxTime, minTime
    type(ESMF_TimeInterval) :: timeStep
    integer(ESMF_KIND_I8)   :: i, j, advanceCount
    real(ESMF_KIND_R8)      :: seconds
    integer(ESMF_KIND_I4)   :: itemCount, timeSlice, localPet,  petCount
    integer(ESMF_KIND_I4)   :: localrc, fieldCount, itemFieldCount
    integer(ESMF_KIND_I4)   :: alarmCount
    type(ESMF_StateItem_Flag) :: itemType
    type(ESMF_StateItem_Flag), allocatable, dimension(:)  :: itemTypeList
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: itemNameList
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: fieldNameList
    type(ESMF_Clock)        :: clock
    logical                 :: isMatch, verbose
    character(len=ESMF_MAXSTR) :: form, fieldName

    character(len=ESMF_MAXSTR) :: message, fileName, name, timeUnit
    character(len=ESMF_MAXSTR), pointer :: filterIncludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer :: filterExcludeList(:) => null()
    logical                    :: checkNaN=.true., checkInf=.true.
    type(ESMF_Field), allocatable :: fieldList(:), itemFieldList(:)
    type(ESMF_FieldBundle)        :: fieldBundle
    type(ESMF_State)              :: state
    character(len=ESMF_MAXSTR), pointer :: itemSearch(:) => null()

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Synchronize this component's current time with the parent clock's
    call ESMF_ClockSet(clock, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, refTime=refTime, startTime=startTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, petCount=petCount, localPet=localPet, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeGet(importState, name='filename', value=fileName, &
      defaultValue=trim(name)//'.nc', rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    j=index(fileName,'.nc',back=.true.)
    if (j < len_trim(fileName)-2) fileName=trim(fileName)//'.nc'

    if (petCount>1) then
      write(form,'(A)')  '(A,'//trim(intformat(int(petCount-1,kind=8)))//',A)'
      write(fileName,form) filename(1:index(filename,'.nc')-1)//'.',localPet,'.nc'
    endif

    call MOSSCO_AttributeGet(importState, 'filter_pattern_include', filterIncludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_AttributeGet(importState, 'filter_pattern_exclude', filterExcludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_AttributeGet(gridComp, 'check_nan', checkNaN, defaultValue=.true., rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_AttributeGet(gridComp, 'check_inf', checkInf, defaultValue=.true., rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Deal with time first, independent of items to be written
    if (advanceCount<huge(timeSlice)) then
      timeSlice=int(advanceCount, ESMF_KIND_I4)
    else
      write(message,'(A)') trim(name)//' cannot use this advanceCount for a netcdf timeSlice, failed to convert long int to int'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      call MOSSCO_CompExit(gridcomp, rc=localrc)
      return
    endif

    if (.true.) then

      call ESMF_TimeGet(startTime, timeStringISOFrac=timeString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' start time is '//trim(timeString)
      !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

      call ESMF_TimeGet(currTime, timeStringISOFrac=timeString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' curr  time is '//trim(timeString)
      !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

      call ESMF_TimeGet(refTime, timeStringISOFrac=timeString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' ref   time is '//trim(timeString)
      !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

      write(timeUnit,'(A)') 'seconds since '//timeString(1:10)//' '//timestring(12:len_trim(timestring))

      call ESMF_TimeIntervalGet(currTime-refTime, s_r8=seconds, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (currTime == startTime) then
        !nc = mossco_netcdfCreate(fileName, timeUnit=timeUnit, state=importState, rc=localrc)
        !call ESMF_AttributeSet(exportState, 'netcdf_id', nc%ncid, rc=localrc)
        !call ESMF_AttributeSet(exportState, 'netcdf_file_name', trim(fileName), rc=localrc)
        verbose = .true.

      else
        verbose = .false.
      end if

      nc = mossco_netcdfOpen(fileName, timeUnit=timeUnit, state=importState, owner=trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call nc%update()
      call nc%update_variables()

      !! Check for monotonic time, if not, then set itemCount=0 to skip writing of
      !! variables.  Continue an error message

      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      call nc%timeGet(minTime, searchIndex=1, stopTime=maxTime, rc=localrc)
      if (localrc == ESMF_RC_NOT_FOUND .or. maxTime < currTime) then
        call nc%add_timestep(seconds, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      elseif (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      elseif (maxTime > currTime) then
        call ESMF_TimeGet(currTime, timeStringISOFrac=timeString, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message, '(A)')  trim(name)//' '//trim(timeString)//' cannot insert time before'

        call ESMF_TimeGet(maxTime, timeStringISOFrac=timeString, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call MOSSCO_MessageAdd(message,' '//trim(timeString))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)

        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        itemCount=0
      endif

      call nc%update()
      call nc%update_variables()

    endif

    !> Get all fields irrespective of bundle and that satisfy include/exclude
    call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
        fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), include=filterIncludeList, &
        exclude=filterExcludeList, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> This stores the itemSearch pointer field Name
    if (.not.associated(itemSearch)) allocate(itemSearch(1), stat=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    do i=1, fieldCount

      call ESMF_FieldGet(fieldList(i), name=fieldName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      ! Skip the face-node connectivity as this is provided by mesh
      if (trim(fieldName) == 'mesh_element_node_connectivity') return

      !> We need to know whether this field occurs multiple times
      itemSearch(1)=trim(fieldName)
      call MOSSCO_StateGet(importState, itemfieldList, &
        fieldCount=itemfieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), &
        include=itemSearch, verbose=verbose,  rc=localrc)

      !> Simple case: field occurs only once
      if (itemFieldCount == 1) then

        if (verbose) then
          write(message,'(A)') trim(name)//' writes '
          call MOSSCO_FieldString(itemFieldList(1), message)
          call MOSSCO_MessageAdd(message,' to '//fileName)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif

        call nc_state_field_write(importState, trim(fieldName), &
          checkNaN=checkNaN, checkInf=checkInf, owner=trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      elseif (itemFieldCount > 1) then

        write(message,'(A)') trim(name)//' will write multiple item  '
        call MOSSCO_FieldString(itemFieldList(1), message)
        call MOSSCO_MessageAdd(message,' to '//fileName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        !> this is the case when a multiple field is in one or more fieldbundles,
        !> either of the same or a different name. We assemble those fieldNames
        !> in a fieldNameList for later processing

        if (.not.allocated(fieldNameList)) then
          call MOSSCO_Reallocate(fieldNameList, 1, keep=.false., rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        else
          !> Avoid duplicate writing of fields from fieldBundle
          call MOSSCO_StringMatch(fieldName, fieldNameList, isMatch, localrc)
          if (isMatch) cycle

          call MOSSCO_Reallocate(fieldNameList, ubound(fieldNameList,1)+1, keep=.true., rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif

        fieldNameList(ubound(fieldNameList,1)) = trim(fieldName)

      else
        write(message,'(A)') trim(name)//' strangely received zero for item '//trim(fieldName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      endif
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    enddo

    !> Now take care of all existing fieldBundles with field named in fieldNameList
    if (allocated(fieldNameList)) then
      do i=1, ubound(fieldNameList,1)

        call ESMF_StateGet(importState, fieldNameList(i), itemType, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        itemSearch(1)=trim(fieldNameList(i))

        call MOSSCO_StateGet(importState, itemfieldList, &
          fieldCount=itemfieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), &
          include=itemSearch, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        !> Create a fieldBundle of the same name in the importState and add
        !> all fields with this name the fieldBundle
        if (itemType == ESMF_STATEITEM_NOTFOUND) then

          fieldBundle = ESMF_FieldBundleCreate(name=fieldNameList(i), &
            multiflag=.true., rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldBundleAdd(fieldBundle, itemFieldList, multiflag=.true., rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_StateAddReplace(importState, (/fieldBundle/), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif

        if (verbose) then
          write(message,'(A)') trim(name)//' writes bundle '
          call MOSSCO_MessageAdd(message,' '//trim(fieldNameList(i))//' to ')
          call MOSSCO_MessageAdd(message,' '//fileName)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif

        do j=1,itemFieldCount
          write(message,'(A)') trim(name)//' writes '
          call MOSSCO_FieldString(itemFieldList(j), message)
          call MOSSCO_MessageAdd(message,' to '//fileName)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        enddo

        call nc_state_fieldbundle_write(importState, trim(fieldNameList(i)), &
          checkNaN=checkNaN, checkInf=checkInf, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      enddo
    endif

    call MOSSCO_Reallocate(fieldNameList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_Reallocate(itemFieldList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    deallocate(itemSearch, stat=localrc)

      !> Remove from import state all fields, whether written or not, ensure that all processes have
      !> processed all states by using a barrier
      !> commented, as ESMF_VMBarrier produces a non-reproducible segfault on ocean
!       call ESMF_GridCompGet(gridComp, vmIsPresent=isPresent, rc=localrc)
!       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!         call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!       if (isPresent) then
!         call ESMF_GridCompGet(gridComp, vm=vm, rc=localrc)
!         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!         call ESMF_VMBarrier(vm, rc=localrc)
!         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!       endif

    !! Somehow this did not help, so we ask again for the items
    !call ESMF_StateReconcile(importState, rc=localrc) ! not working on ocean
    if (allocated(itemNameList)) deallocate(itemNameList)
    if (allocated(fieldNameList)) deallocate(fieldNameList)
   ! 7.0.0 b64
   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)


    !> Look for states in states and write them as a non-dimensional
    !> variable to record model metadata that is not stored in variables
    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)
    allocate(itemTypeList(itemCount))
    allocate(itemNameList(itemCount))

    call ESMF_StateGet(importState, itemTypeList=itemTypeList, &
      itemNameList=itemNameList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    do i=1, itemCount

      if (itemTypeList(i) /= ESMF_STATEITEM_STATE) cycle

      call ESMF_StateGet(importState, itemNameList(i), state, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call nc%put_state(state, name=trim(itemNameList(i)), &
        owner=trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    enddo

    ! The removal creates problesm, it is disabled here for further testing.  Is
    ! it needed at all?
    ! if (itemcount>0) then
    !
    !   allocate(itemNameList(itemCount))
    !
    !   call ESMF_StateGet(importState, itemNameList=itemNameList, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   !call ESMF_StateRemove(importState, itemNameList, rc=localrc)
    !   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
    !     write(message,'(A)') trim(name)//' ignores error above '
    !     call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    !     localrc = ESMF_SUCCESS
    !     !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !   endif
    !
    ! endif

    call nc%close()

    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)

    !call ESMF_StateReconcile(importState, rc=localrc) ! not working on ocean
    !call ESMF_LogOpen(log, "netcdf", rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !call MOSSCO_StateLog(importState, log=log, rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !call ESMF_LogClose(log, rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (associated(filterIncludeList)) deallocate(filterIncludeList)
    if (associated(filterExcludeList)) deallocate(filterExcludeList)

    ! For this component, it does not make sense to advance its clock by a regular
    ! timestep.  Thus, it is advanced to the next alarm time, if there is
    ! such an alarmList
    call ESMF_ClockGet(clock, alarmCount=alarmCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (alarmCount > 0) then
      call MOSSCO_ClockGetTimeStepToNextAlarm(clock, timeStep, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    else
      call ESMF_ClockGet(clock, timeStep=timeStep, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call ESMF_ClockAdvance(clock, timeStep=timeStep, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !call nc%close(rc=localrc)

    call ESMF_GridCompGet(gridComp, configIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) then

      call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ConfigDestroy(config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    end if

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_DestroyOwn(importState, trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_DestroyOwn(exportState, trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "nc_state_fieldbundle_write"
  subroutine nc_state_fieldbundle_write(state, bundleName, kwe, owner, checkNaN, checkInf, rc)

    type(ESMF_State), intent(in)           :: state
    character(len=*), intent(in)           :: bundleName
    character(len=*), intent(in), optional :: owner
    type(ESMF_KeywordEnforcer), optional   :: kwe
    logical, optional, intent(in)          :: checkNaN, checkInf
    integer(ESMF_KIND_I4), intent(out), optional   :: rc

    type(ESMF_FieldBundle)              :: fieldBundle
    type(ESMF_Field), allocatable       :: fieldList(:)
    character(ESMF_MAXSTR), allocatable :: fieldNameList(:)
    integer(ESMF_KIND_I4)               :: i, fieldCount, localrc, rc_, myFieldCount
    character(ESMF_MAXSTR)              :: numberString, owner_
    type(ESMF_StateItem_Flag)           :: itemType
    logical                             :: checkNaN_ = .true., checkInf_ = .true.

    rc_ = ESMF_SUCCESS
    owner_ = '--'
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc))  rc = rc_
    if (present(checkNaN)) checkNaN_ = checkNaN
    if (present(checkInf)) checkInf_ = checkInf
    if (present(owner)) call MOSSCO_StringCopy(owner_, owner)

    call ESMF_StateGet(state, trim(bundleName), itemType=itemType, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (itemType /= ESMF_STATEITEM_FIELDBUNDLE) then
      call ESMF_LogWrite(trim(owner_)//' requested fieldBundle '//trim(bundleName)//' was not found', ESMF_LOGMSG_WARNING)
      return
    endif

    call ESMF_StateGet(state, trim(bundleName), fieldBundle, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (fieldCount<1) return

    allocate(fieldList(fieldCount), stat=localrc)
    allocate(fieldNameList(fieldCount), stat=localrc)

    call ESMF_FieldBundleGet(fieldBundle, itemorderflag=ESMF_ITEMORDER_ADDORDER, &
      fieldList=fieldList, fieldNameList=fieldNameList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    do i=1, fieldCount

      call ESMF_FieldBundleGet(fieldBundle, fieldName=fieldNameList(i), &
        fieldCount=myFieldCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      !> If there are multiple fields with the same name then postFix
      !> field name in netcdf with a numeric index
      if (myFieldCount > 0) then
        write(numberstring,'(I0.3)') i

        call nc_field_write(fieldList(i), postFix=trim(numberString), &
          checkNaN=checkNaN_, checkInf=checkInf_, owner=owner_, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      else
        call nc_field_write(fieldList(i), &
          checkNaN=checkNaN_, checkInf=checkInf_, owner=owner_, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      endif

    end do
    if (allocated(fieldList))     deallocate(fieldList)
    if (allocated(fieldNameList)) deallocate(fieldNameList)

  end subroutine nc_state_fieldbundle_write

#undef  ESMF_METHOD
#define ESMF_METHOD "nc_state_field_write"
  subroutine nc_state_field_write(state, fieldName, kwe, owner, checkInf, checkNaN, rc)

    type(ESMF_State), intent(in)           :: state
    character(len=*), intent(in)           :: fieldName
    type(ESMF_KeywordEnforcer), optional   :: kwe
    logical, intent(in), optional          :: checkNaN, checkInf
    character(len=*), intent(in), optional :: owner
    integer(ESMF_KIND_I4), intent(out), optional   :: rc

    type(ESMF_Field)           :: field
    type(ESMF_StateItem_Flag)  :: itemType
    integer(ESMF_KIND_I4)      :: localrc, rc_
    logical                    :: checkNaN_ = .true., checkInf_ = .true.
    character(len=ESMF_MAXSTR) :: owner_

    rc_ = ESMF_SUCCESS
    owner_ = '--'
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc))  rc = rc_
    if (present(checkNaN)) checkNaN_ = checkNaN
    if (present(checkInf)) checkInf_ = checkInf
    if (present(owner)) call MOSSCO_StringCopy(owner_, owner)

    call ESMF_StateGet(state, trim(fieldName), itemType=itemType, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (itemType /= ESMF_STATEITEM_FIELD) then
      call ESMF_LogWrite(trim(owner_)//' field '//trim(fieldName)//' was not found', ESMF_LOGMSG_WARNING)
      return
    endif

    call ESMF_StateGet(state, trim(fieldName), field, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call nc_field_write(field, checkNaN=checkNaN_, checkInf=checkInf_, &
      owner=owner_, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (present(rc)) rc=localrc

  end subroutine nc_state_field_write

#undef  ESMF_METHOD
#define ESMF_METHOD "nc_field_write"
  subroutine nc_field_write(field, kwe, postFix, checkNaN, checkInf, owner, rc)

    type(ESMF_Field), intent(inout)                    :: field !> @todo check inout
    type(ESMF_KeywordEnforcer), optional, intent(in)   :: kwe
    logical, optional, intent(in)                      :: checkNaN, checkInf
    character(len=*), intent(in), optional             :: postFix, owner
    integer(ESMF_KIND_I4), intent(out), optional       :: rc

    integer(ESMF_KIND_I4)               :: localDeCount, localrc, rc_
    character(ESMF_MAXSTR)              :: fieldName, message, owner_
    logical                             :: checkNaN_ = .true. , checkInf_ = .true.

    rc_ = ESMF_SUCCESS
    owner_ = '--'
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc))  rc = rc_
    if (present(checkNaN)) checkNan_ = checkNaN
    if (present(checkInf)) checkInf_ = checkInf
    if (present(owner)) call MOSSCO_StringCopy(owner_, owner)

    call ESMF_FieldGet(field, name=fieldName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_FieldGet(field, localDeCount=localDeCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (localDeCount < 1) return

    if (present(postFix)) fieldName=trim(fieldName)//'_'//trim(postFix)

    call nc%put_variable(field, name=trim(fieldName), &
      checkNaN=checkNaN_, checkInf=checkInf_, owner=trim(owner_), rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      write(message,'(A)') trim(owner_)//' could not write'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
    endif
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  end subroutine nc_field_write

#undef  ESMF_METHOD
end module netcdf_component
