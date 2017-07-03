!> @brief Implementation of an ESMF netcdf output component
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2014, 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "netcdf_component.F90"

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
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
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
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

    character(len=ESMF_MAXSTR) :: name, configFileName, fileName
    character(len=4096)        :: message
    character(len=ESMF_MAXSTR) :: timeString, timeUnit
    type(ESMF_Time)            :: currTime, refTime
    type(ESMF_Clock)           :: clock
    integer(ESMF_KIND_I4)      :: localrc
    logical                    :: fileIsPresent, configIsPresent, labelIsPresent
    type(ESMF_Config)          :: config
    character(len=ESMF_MAXSTR), allocatable :: filterExcludeList(:)
    character(len=ESMF_MAXSTR), allocatable :: filterIncludeList(:)
    logical                    :: checkNaN, checkInf

    rc  = ESMF_SUCCESS
    checkNaN = .true.
    checkInf = .true.

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    else
      call ESMF_GridCompGet(gridComp, configIsPresent=configIsPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (configIsPresent) then
        call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)')  trim(name)//' reads configuration from component'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    if (fileIsPresent .or. configIsPresent) then

      !> Value of netcdf fileName defaults to name of component
      call MOSSCO_ConfigGet(config, label='filename', value=fileName, &
        defaultValue=trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_ConfigGet(config, label='exclude', value=filterExcludeList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Value of checkNaN defaults to .true.
      call MOSSCO_ConfigGet(config, label='checkNaN', value=checkNaN, defaultValue=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Value of checkInf defaults to .true.
      call MOSSCO_ConfigGet(config, label='checkInf', value=checkNaN, defaultValue=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Default value for filter ExcludeList is a non-associated pointer
      call MOSSCO_ConfigGet(config, label='exclude', value=filterExcludeList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Default value for filterIncludeList is a non-associated pointer
      call MOSSCO_ConfigGet(config, 'include', value=filterIncludeList, &
        isPresent=labelIsPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    endif

    if (allocated(filterIncludeList)) then
      call MOSSCO_AttributeSet(importState, 'filter_pattern_include', filterIncludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeGet(importState, 'filter_pattern_include', value=message, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' include patterns: '//trim(message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call MOSSCO_Reallocate(filterIncludeList, 0, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (allocated(filterExcludeList)) then
      call MOSSCO_AttributeSet(importState, 'filter_pattern_exclude', filterExcludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeGet(importState, 'filter_pattern_exclude', value=message, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' exclude patterns: '//trim(message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call MOSSCO_Reallocate(filterExcludeList, 0, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call MOSSCO_AttributeSet(importState, 'check_inf', checkInf, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_AttributeSet(importState, 'check_nan', checkNaN, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(importState, 'filename', trim(fileName), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, refTime=refTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(refTime, timeStringISOFrac=timeString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
     call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(timeUnit,'(A)') 'seconds since '//timeString(1:10)//' '//timestring(12:len_trim(timestring))

    nc = mossco_netcdfCreate(fileName, timeUnit=timeUnit, state=importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(exportState, 'netcdf_id', nc%ncid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(exportState, 'netcdf_file_name', trim(fileName), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4)      :: localrc
    character(len=ESMF_MAXSTR) :: name

    rc = ESMF_SUCCESS

    !> Here omes your restart code, which in the simplest case copies
    !> values from all fields in importState to those in exportState

    call ESMF_GridCompGet(gridComp, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(importState, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(parentClock, name=name, rc=localrc)
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

    character(len=19)       :: timestring
    type(ESMF_Time)         :: currTime, refTime
    type(ESMF_Time)         :: startTime, stopTime, maxTime, minTime
    type(ESMF_TimeInterval) :: timeStep
    integer(ESMF_KIND_I8)   :: i, j, advanceCount
    real(ESMF_KIND_R8)      :: seconds
    integer(ESMF_KIND_I4)   :: itemCount, timeSlice, localPet,  petCount
    integer(ESMF_KIND_I4)   :: localrc, fieldCount
    type(ESMF_StateItem_Flag), allocatable, dimension(:) :: itemTypeList
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: itemNameList
    type(ESMF_Clock)        :: clock
    logical                 :: isMatch
    character(len=ESMF_MAXSTR) :: form

    character(len=ESMF_MAXSTR) :: message, fileName, name, timeUnit
    character(len=ESMF_MAXSTR), allocatable :: filterIncludeList(:), filterExcludeList(:)
    logical                    :: checkNaN=.true., checkInf=.true.
    type(ESMF_Field), allocatable :: fieldList(:)

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Synchronize this component's current time with the parent clock's
    call ESMF_ClockSet(clock, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, refTime=refTime, startTime=startTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, petCount=petCount, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, name='filename', value=fileName, &
      defaultValue=trim(name)//'.nc', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    j=index(fileName,'.nc',back=.true.)
    if (j < len_trim(fileName)-2) fileName=trim(fileName)//'.nc'

    if (petCount>1) then
      write(form,'(A)')  '(A,'//trim(intformat(int(petCount-1,kind=8)))//',A)'
      write(fileName,form) filename(1:index(filename,'.nc')-1)//'.',localPet,'.nc'
    endif

    call MOSSCO_AttributeGet(importState, 'filter_pattern_include', filterIncludeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_AttributeGet(importState, 'filter_pattern_exclude', filterExcludeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_AttributeGet(importState, 'check_nan', checkNaN, defaultValue=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_AttributeGet(importState, 'check_inf', checkInf, defaultValue=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (advanceCount<huge(timeSlice)) then
      timeSlice=int(advanceCount, ESMF_KIND_I4)
    else
      write(message,'(A)') 'Cannot use this advanceCount for a netcdf timeSlice, failed to convert long int to int'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return
    endif

    if (itemcount>0) then
      if (.not.allocated(itemTypeList)) allocate(itemTypeList(itemCount))
      if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))

      call ESMF_StateGet(importState, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_TimeGet(refTime, timeStringISOFrac=timeString, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(timeUnit,'(A)') 'seconds since '//timeString(1:10)//' '//timestring(12:len_trim(timestring))

      call ESMF_TimeIntervalGet(currTime-refTime, s_r8=seconds, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (currTime == startTime) then
        nc = mossco_netcdfCreate(fileName, timeUnit=timeUnit, state=importState, rc=localrc)
        call ESMF_AttributeSet(exportState, 'netcdf_id', nc%ncid, rc=localrc)
        call ESMF_AttributeSet(exportState, 'netcdf_file_name', &
          trim(fileName), rc=localrc)
      else
        nc = mossco_netcdfOpen(fileName, timeUnit=timeUnit, state=importState, rc=localrc)
      end if
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call nc%update()
      call nc%update_variables()

      !! Check for monotonic time, if not, then set itemCount=0 to skip writing of
      !! variables.  Continue an error message

      call nc%timeGet(minTime, searchIndex=1, stopTime=maxTime, rc=localrc)
      if (localrc == ESMF_RC_NOT_FOUND .or. maxTime < currTime) then
        call nc%add_timestep(seconds, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      elseif (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      elseif (maxTime > currTime) then
        call ESMF_TimeGet(currTime, timeStringISOFrac=timeString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message, '(A)')  trim(name)//' '//trim(timeString)//' cannot insert time before'
        call ESMF_TimeGet(maxTime, timeStringISOFrac=timeString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_MessageAdd(message,' '//trim(timeString))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)

        itemCount=0
      endif

      call nc%update()
      call nc%update_variables()

    endif


    do i=1,itemCount

      !! Look for an exclusion pattern on this field name
      if (allocated(filterExcludeList)) then
        do j=1,ubound(filterExcludeList,1)
          call MOSSCO_StringMatch(itemNameList(i), filterExcludeList(j), isMatch, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (ismatch) exit
        enddo

        if (isMatch) then
          if (advanceCount < 1) then
            write(message,'(A)') trim(name)//' excluded'
            call MOSSCO_MessageAdd(message, ' '//trim(itemNameList(i))//' from pattern ')
            call MOSSCO_MessageAdd(message, ' '//trim(filterExcludeList(j)))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          endif
          cycle
        endif
      endif

      !! Look for an inclusion pattern on this field name
      if (allocated(filterIncludeList)) then
        do j=1,ubound(filterIncludeList,1)
          call MOSSCO_StringMatch(itemNameList(i), filterIncludeList(j), isMatch, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (ismatch) exit
        enddo
        if (.not.ismatch) then
          if (advanceCount < 1) then
            write(message,'(A)') trim(name)//' did not include '
            call MOSSCO_MessageAdd(message,' '//itemNameList(i))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          endif
          cycle
        endif
      endif

      call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
        itemSearch=trim(itemNameList(i)), fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldCount < 1) then
        if (advanceCount < 1) then
          write(message,'(A)') trim(name)//' skipped non-field or incomplete item '
          call MOSSCO_MessageAdd(message,' '//itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif
        cycle
      endif

        if (advanceCount < 1) then
          write(message,'(A)') trim(name)//' will write'
          call MOSSCO_MessageAdd(message,' '//itemNameList(i))
          call MOSSCO_MessageAdd(message,' to file ')
          call MOSSCO_MessageAdd(message,' '//fileName)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif

        if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
          call nc_state_field_write(importState, trim(itemNameList(i)), &
            checkNaN=checkNaN, checkInf=checkInf, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
          call nc_state_fieldbundle_write(importState, trim(itemNameList(i)), &
            checkNaN=checkNaN, checkInf=checkInf, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        elseif (advanceCount < 1) then
          write(message,'(A)') trim(name)//' not implemented saving item '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        endif
    enddo

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
    if (allocated(itemNameList)) deallocate(itemNameList)
   ! call ESMF_StateReconcile(importState, rc=localrc) ! not working on ocean
   ! 7.0.0 b64
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemcount>0) then

      allocate(itemNameList(itemCount))

      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateRemove(importState, itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
        write(message,'(A)') trim(name)//' ignores error above '
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call nc%close()
    endif

    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)

    !call ESMF_LogOpen(log, "netcdf", rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !call MOSSCO_StateLog(importState, log=log, rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !call ESMF_LogClose(log, rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(filterIncludeList)) deallocate(filterIncludeList)
    if (allocated(filterExcludeList)) deallocate(filterExcludeList)

    !! For this component, it does not make sense to advance its clock by a regular
    !! timestep.  Thus, it is advanced to the next alarm time.

    call MOSSCO_ClockGetTimeStepToNextAlarm(clock, timeStep, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !if (timeStep>0) then
      call ESMF_ClockAdvance(clock, timeStep=timeStep, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !endif

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !call nc%close(rc=localrc)

    call ESMF_GridCompGet(gridComp, configIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then

      call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigDestroy(config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    end if

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_DestroyOwn(importState, trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_DestroyOwn(exportState, trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "nc_state_fieldbundle_write"
  subroutine nc_state_fieldbundle_write(state, bundleName, kwe, checkNaN, checkInf, rc)

    type(ESMF_State), intent(in)           :: state
    character(len=*), intent(in)           :: bundleName
    type(ESMF_KeywordEnforcer), optional   :: kwe
    logical, optional, intent(in)          :: checkNaN, checkInf
    integer(ESMF_KIND_I4), intent(out), optional   :: rc

    type(ESMF_FieldBundle)              :: fieldBundle
    type(ESMF_Field), allocatable       :: fieldList(:)
    integer(ESMF_KIND_I4)               :: i, fieldCount, localrc, rc_
    character(ESMF_MAXSTR)              :: numberString
    type(ESMF_StateItem_Flag)           :: itemType
    logical                             :: checkNaN_ = .true., checkInf_ = .true.

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc))  rc = rc_
    if (present(checkNaN)) checkNaN_ = checkNaN
    if (present(checkInf)) checkInf_ = checkInf

    call ESMF_StateGet(state, trim(bundleName), itemType=itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELDBUNDLE) then
      call ESMF_LogWrite('  fieldBundle '//trim(bundleName)//' was not found', ESMF_LOGMSG_WARNING)
      return
    endif

    call ESMF_StateGet(state, trim(bundleName), fieldBundle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount<1) return

    allocate(fieldList(fieldCount))
    call ESMF_FieldBundleGet(fieldBundle,fieldList=fieldList,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! go through list of fields and put fields into netcdf using field name and number
    do i=1, fieldCount

      write(numberstring,'(I0.3)') i

      call nc_field_write(fieldList(i), postFix=trim(numberString), &
        checkNaN=checkNaN_, checkInf=checkInf_, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    end do
    if (allocated(fieldList)) deallocate(fieldList)

    if (present(rc)) rc=localrc

  end subroutine nc_state_fieldbundle_write

#undef  ESMF_METHOD
#define ESMF_METHOD "nc_state_field_write"
  subroutine nc_state_field_write(state, fieldName, kwe, checkInf, checkNaN, rc)

    type(ESMF_State), intent(in)           :: state
    character(len=*), intent(in)           :: fieldName
    type(ESMF_KeywordEnforcer), optional   :: kwe
    logical, intent(in), optional          :: checkNaN, checkInf
    integer(ESMF_KIND_I4), intent(out), optional   :: rc

    type(ESMF_Field)           :: field
    type(ESMF_StateItem_Flag)  :: itemType
    integer(ESMF_KIND_I4)      :: localrc, rc_
    logical                    :: checkNaN_ = .true., checkInf_ = .true.

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc))  rc = rc_
    if (present(checkNaN)) checkNaN_ = checkNaN
    if (present(checkInf)) checkInf_ = checkInf

    call ESMF_StateGet(state, trim(fieldName), itemType=itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELD) then
      call ESMF_LogWrite('  field '//trim(fieldName)//' was not found', ESMF_LOGMSG_WARNING)
      return
    endif

    call ESMF_StateGet(state, trim(fieldName), field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call nc_field_write(field, checkNaN=checkNaN_, checkInf=checkInf_, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc=localrc

  end subroutine nc_state_field_write

#undef  ESMF_METHOD
#define ESMF_METHOD "nc_field_write"
  subroutine nc_field_write(field, kwe, postFix, checkNaN, checkInf, rc)

    type(ESMF_Field), intent(inout)        :: field
    type(ESMF_KeywordEnforcer), optional   :: kwe
    logical, optional, intent(in)          :: checkNaN, checkInf
    character(len=*), intent(in), optional :: postFix
    integer(ESMF_KIND_I4), intent(out), optional   :: rc

    integer(ESMF_KIND_I4)               :: localDeCount, localrc, rc_
    character(ESMF_MAXSTR)              :: fieldName
    logical                             :: checkNaN_ = .true. , checkInf_ = .true.

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc))  rc = rc_
    if (present(checkNaN)) checkNan_ = checkNaN
    if (present(checkInf)) checkInf_ = checkInf

    call ESMF_FieldGet(field, name=fieldName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(postFix)) fieldName=trim(fieldName)//'_'//trim(postFix)
    if (localDeCount>0) then
      call nc%put_variable(field, name=trim(fieldName), &
        checkNaN=checkNaN_, checkInf=checkInf_, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (present(rc)) rc=localrc

  end subroutine nc_field_write

end module netcdf_component
