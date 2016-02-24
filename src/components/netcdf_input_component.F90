!> @brief Implementation of an ESMF netcdf output component
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2014, 2015, 2016 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "netcdf_input_component.F90"

module netcdf_input_component

  use esmf
  use mossco_variable_types
  use mossco_netcdf
  use mossco_field
  use mossco_strings
  use mossco_component
  use mossco_state
  use mossco_time
  use mossco_grid
  use mossco_attribute
  use mossco_config

  implicit none
  private

  type(type_mossco_netcdf)   :: nc !> @todo should this be an array?

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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

    character(len=10)           :: InitializePhaseMap(2)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"
    InitializePhaseMap(2) = "IPDv00p2=2"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

  !> Initialize the component
  !!
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: timeString, message, name, fileName
    character(len=ESMF_MAXSTR) :: foreignGridFieldName, form
    type(ESMF_Time)            :: currTime, ncTime
    type(ESMF_TimeInterval)    :: climatologyTimeStep
    integer(ESMF_KIND_I4)      :: petCount, localPet, localRc
    type(ESMF_Clock)           :: clock

    logical                    :: isPresent, fileIsPresent, labelIsPresent, hasGrid
    logical                    :: isBundle=.true.
    type(ESMF_Grid)            :: grid2, grid3, grid
    type(ESMF_Field)           :: field
    character(len=ESMF_MAXSTR) :: configFileName, timeUnit, itemName, petFileName, gridName
    character(len=ESMF_MAXSTR) :: gridFileName, interpolationMethod
    type(ESMF_Config)          :: config

    integer(ESMF_KIND_I4)      :: itemCount, i, j, timeid, itime, udimid
    integer(ESMF_KIND_I4)      :: fieldRank, gridRank
    type(ESMF_Time)            :: refTime, climatologyTime
    real(ESMF_KIND_R8)         :: seconds
    type(ESMF_Field), allocatable :: fieldList(:)
    integer(ESMF_KIND_I4), allocatable    :: ungriddedUbnd(:), ungriddedLbnd(:)
    character(len=ESMF_MAXSTR), allocatable :: aliasList(:,:), filterExcludeList(:), filterIncludeList(:)
    character(len=ESMF_MAXSTR), allocatable :: climatologyList(:)
    logical                    :: isMatch

    rc = ESMF_SUCCESS

    hasGrid = .false.

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      rc=localrc)
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

      call ESMF_ConfigFindLabel(config, label='filename:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (labelIsPresent) then
        call ESMF_ConfigGetAttribute(config, fileName, rc=localrc, default=trim(name))
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)')  trim(name)//' found in file'
        call MOSSCO_MessageAdd(message,' '//trim(configFileName)//' filename: '//trim(fileName))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call ESMF_AttributeGet(importState, 'filename', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isPresent) then
        call ESMF_AttributeGet(importState, 'filename', value=fileName, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      elseif (labelIsPresent) then
        call ESMF_AttributeSet(importState, 'filename', trim(fileName), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_ConfigFindLabel(config, label='interpolation:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (labelIsPresent) then
        call ESMF_ConfigGetAttribute(config, interpolationMethod, rc=localrc, default=trim(name))
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)')  trim(name)//' found in file'
        call MOSSCO_MessageAdd(message,' '//trim(configFileName)//' interpolation: '//trim(interpolationMethod))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        interpolationMethod='recent'
      endif

      call ESMF_AttributeGet(importState, 'interpolation_method', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isPresent) then
        call ESMF_AttributeGet(importState, 'interpolation_method', value=interpolationMethod, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      elseif (labelIsPresent) then
        call ESMF_AttributeSet(importState, 'interpolation_method', trim(interpolationMethod), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_ConfigFindLabel(config, label='grid:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (labelIsPresent) then
        call ESMF_ConfigGetAttribute(config, gridFileName, rc=localrc, default='none')
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' grid: '//trim(gridFileName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call ESMF_AttributeGet(gridComp, 'grid_file_name', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isPresent) then
        call ESMF_AttributeGet(gridComp, 'grid_file_name', value=gridFileName, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      elseif (labelIsPresent) then
        call ESMF_AttributeSet(gridComp, 'grid_file_name', trim(gridFileName), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif


      call MOSSCO_ConfigGetList(config, 'climatology:', climatologyList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(climatologyList)) then

        write(timeString, '(A)') trim(climatologyList(1))
        if (size(climatologyList) > 1) write(timeString, '(A)') trim(timeString)//' '//trim(climatologyList(2))

        write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' climatology: '//trim(timeString)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        call ESMF_AttributeSet(gridComp, 'climatology_period', trim(timeString), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_Reallocate(climatologyList, 0, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif

      call MOSSCO_ConfigGetList(config, 'exclude:', filterExcludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(filterExcludeList)) then
        call MOSSCO_AttributeSetList(importState, 'filter_pattern_exclude', filterExcludeList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_Reallocate(filterExcludeList, 0, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_AttributeGetList(importState, 'filter_pattern_exclude', filterExcludeList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' uses exclude patterns:'
        call MOSSCO_MessageAdd(message, filterExcludeList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      endif

      call MOSSCO_ConfigGetList(config, 'include:', filterIncludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(filterIncludeList)) then
        call MOSSCO_AttributeSetList(importState, 'filter_pattern_include', filterIncludeList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_Reallocate(filterIncludeList, 0, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_AttributeGetList(importState, 'filter_pattern_include', filterIncludeList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' uses include patterns:'
        call MOSSCO_MessageAdd(message, filterIncludeList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call MOSSCO_ConfigGetList(config, 'alias:', aliasList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(aliasList)) then
        call MOSSCO_AttributeSetList(importState, 'alias_definition', aliasList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_Reallocate(aliasList, 0, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_AttributeGetList(importState, 'alias_definition', aliasList, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do i = lbound(aliasList,1), ubound(aliasList,1)
          write(message,'(A)') trim(name)//' uses alias: '
          call MOSSCO_MessageAdd(message, trim(aliasList(i,1))//' = '//trim(aliasList(i,2)), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        enddo
      endif

      call ESMF_ConfigFindLabel(config, label='bundle:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (labelIsPresent) then
        call ESMF_ConfigGetAttribute(config, isBundle, rc=localrc, default=.true.)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      !call ESMF_AttributeSet(importState, 'bundle_fields', isBundle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_AttributeGet(importState, 'filename', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      write(message,'(A)') trim(name)//' received no filename to read from'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      call MOSSCO_CompExit(gridComp)
      return
    endif

    call ESMF_AttributeGet(importState, 'filename', value=fileName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! For multiprocessor applications, try to read cpu-specific input first, default
    ! back to overall file
    if (petCount>0) then
      write(form,'(A)')  '(A,'//trim(intformat(int(petCount-1,kind=8)))//',A)'
      write(petFileName,trim(form)) filename(1:len_trim(filename)-2),localPet,'.nc'
      inquire(file=trim(petFileName), exist=isPresent)
      if (isPresent) fileName=trim(petFileName)
    endif

    inquire(file=trim(fileName), exist=isPresent)

    if (.not.isPresent) then
      write(message,'(A)') trim(name)//' cannot read file '//trim(fileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      call MOSSCO_CompExit(gridComp)
      return
    endif

    write(message,'(A)')  trim(name)//' reading file '//trim(fileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_AttributeGet(gridComp, name='grid_file_name', &
      isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(gridComp, name='grid_file_name', &
        value=gridFileName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (trim(gridFileName) /= 'none') then

        grid = ESMF_GridCreate(filename=trim(gridFileName),fileFormat=ESMF_FILEFORMAT_SCRIP, &
          regDecomp=(/1,1/), isSphere=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message, '(A)') trim(name)//' obtains grid from file '//trim(gridFileName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        call ESMF_GridCompSet(gridComp, grid=grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)

        fieldRank = 2
        hasGrid=.true.
      endif
    endif

    call ESMF_GridCompGet(gridComp, gridIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      write(message,'(A)') trim(name)//' found grid in component'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)

      call ESMF_GridCompGet(gridComp, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)
      hasGrid=.true.
    endif

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
      isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.hasGrid .and. isPresent) then
      call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
        value=foreignGridFieldName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (trim(foreignGridFieldName) /= 'none') then
        call ESMF_StateGet(importState,  trim(foreignGridFieldName), field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message, '(A)') trim(name)//' obtains grid from field'
        call MOSSCO_FieldString(field,message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        call ESMF_FieldGet(field, grid=grid, rank=fieldRank, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        hasGrid=.true.
      endif
    endif

    if (.not.hasGrid) then
      write(message,'(A)') trim(name)//' not implemented without grid or foreign_grid'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      !rc = ESMF_RC_NOT_IMPL
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_GridGet(grid, rank=gridRank, name=gridName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Check for ungridded dimensions in the case of foreignGrid
    if (isPresent .and. fieldRank>gridRank) then
      allocate(ungriddedUbnd(fieldRank-gridRank))
      allocate(ungriddedLbnd(fieldRank-gridRank))
      call ESMF_FieldGet(field, ungriddedLBound=ungriddedLbnd, ungriddedUbound=ungriddedUbnd, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (gridRank == 2) then
      grid2 = grid
      grid3 = MOSSCO_GridCreateFromOtherGrid(grid2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    elseif (gridRank == 3) then
      grid3 = grid
      grid2 = MOSSCO_GridCreateFromOtherGrid(grid3, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      write(message,'(A)') trim(name)//' cannot use grid with rank<2 or >3 for foreign_grid'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    nc = MOSSCO_NetcdfOpen(trim(fileName), mode='r', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call nc%update_variables()
    call nc%update()
    itemCount = nc%nvars

    ! Get time information from time variable if present and log the time span available
    itime=1
    timeid=0
    do i=1, itemCount
      if (trim(nc%variables(i)%name) == 'time' .or. trim(nc%variables(i)%standard_name) == 'time') then
        timeid=i
        write(message,'(A)')  trim(name)//' found time variable'
        write(message,'(A,I3,A,I1,A)') trim(message)//' ', &
          nc%variables(i)%varid,' rank ',nc%variables(i)%rank,' units='//trim(nc%variables(i)%units)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        exit
      endif
    enddo

    if (timeid > 0) then
      call ESMF_ClockGet(clock, currTime=currTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      timeunit=trim(nc%variables(timeid)%units)
      udimid=nc%variables(timeid)%dimids(1)
      i=index(timeunit,'since ')
      if (i>0) then
        call MOSSCO_TimeSet(refTime, timeunit(i+6:len_trim(timeunit)), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        timeunit=timeunit(1:i-1)
      endif

      if (trim(timeUnit) == 'seconds') then
        call ESMF_TimeIntervalGet(currTime-refTime, s_r8=seconds, rc=rc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        write(message,'(A)')  trim(name)//' not implemented: unit for time is '//trim(timeUnit)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        seconds=0.0
      endif

      call ESMF_AttributeGet(gridComp, 'climatology_period', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isPresent) then
        call ESMF_AttributeGet(gridComp, 'climatology_period', timeString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_TimeIntervalSet(climatologyTimeStep, trim(timeString), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' uses climatology with period '//trim(timeString)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        call nc%timeGet(ncTime, searchIndex=1, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_TimeGet(ncTime, timeString=timeString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AttributeSet(gridComp, 'climatology_start', trim(timeString), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' climatology from '//trim(timeString)
        call nc%timeIndex(ncTime + climatologyTimeStep, itime, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call nc%timeGet(ncTime, searchIndex=itime, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_TimeGet(ncTime, timeString=timeString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(message)//' to '//trim(timeString)
        call ESMF_AttributeSet(gridComp, 'climatology_stop', trim(timeString), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        climatologyTime = currTime
        do while (climatologyTime > (ncTime + climatologyTimeStep))
          climatologyTime = climatologyTime - climatologyTimeStep
        enddo
        call nc%timeIndex(climatologyTime, itime, rc=localrc)
      else
        call nc%timeIndex(currTime, itime, rc=localrc)
      endif
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    else
      udimid=-1
    endif

    call MOSSCO_Reallocate(fieldList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, itemCount

      itemName=trim(nc%variables(i)%standard_name)
      if (len_trim(itemName)<1) itemName=trim(nc%variables(i)%name)
      if (len_trim(itemName)<1) cycle

      !! Convert aliases in file to proper item names
      if (allocated(aliasList)) then
        do j = lbound(aliasList,1), ubound(aliasList,1)
          if (trim(itemName) == trim(aliasList(j,1))) then
            itemName=trim(aliasList(j,2))
            exit
          endif
        enddo
      endif

      if (trim(itemName) == 'time') cycle
      if (trim(itemName) == 'latitude') cycle
      if (trim(itemName) == 'longitude') cycle

      ! Look for an exclusion pattern on this field name
      if (allocated(filterExcludeList)) then
        do j = lbound(filterExcludeList,1), ubound(filterExcludeList,1)
          write(0,*) 'filterExcludeList '//trim(itemName)//', '//trim(filterExcludeList(j))
          call MOSSCO_StringMatch(trim(itemName), trim(filterExcludeList(j)), isMatch, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (ismatch) exit
        enddo
        if (ismatch) then
          write(message,'(A)')  trim(name)//' excluded item'
          call MOSSCO_MessageAdd(message, ' '//trim(itemName))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif
      endif

      !! Look for an inclusion pattern on this field name
      if (allocated(filterIncludeList)) then
        do j = lbound(filterIncludeList,1), ubound(filterIncludeList,1)
          write(0,*) 'filterIncludeList '//trim(itemName)//', '//trim(filterIncludeList(j))
          call MOSSCO_StringMatch(trim(itemName), trim(filterIncludeList(j)), isMatch, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (ismatch) exit
        enddo
        if (.not.ismatch) then
          write(message,'(A)')  trim(name)//' did not include'
          call MOSSCO_MessageAdd(message, ' '//trim(itemName))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif
      endif

      if (nc%variables(i)%rank < 2) then
        write(message,'(A)') trim(name)//' does not implemented reading of rank < 2 item'
        call MOSSCO_MessageAdd(message, ' '//trim(itemName)//'"')
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        cycle
      endif

      write(message,'(A)') trim(name)//' found item "'
      call MOSSCO_MessageAdd(message, trim(itemName)//'"')
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      write(message,'(A,I3,A,I1,A)') trim(name)//' id = ', &
         nc%variables(i)%varid,', rank = ',nc%variables(i)%rank,' units = "'//trim(nc%variables(i)%units)//'"'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      fieldList(i) = ESMF_FieldEmptyCreate(name=trim(itemName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !call nc%gridget(varGrid, nc%variables(i), localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (hasGrid) then

        !! Make sure varRank>=fieldRank>=gridRank

        if (any(nc%variables(i)%dimids==udimid)) then
          if (fieldRank /= nc%variables(i)%rank-1) then
            write(message,'(A,I1)') trim(name)//' mismatch from'
            call MOSSCO_MessageAdd(message,' '//trim(nc%name)//'::'//trim(nc%variables(i)%name))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            write(message,'(A,I1,A)') '  rank ',nc%variables(i)%rank-1,' /= field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)

            cycle
            !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif
        else
          if (fieldRank /= nc%variables(i)%rank) then
            write(message,'(A,I1)') trim(name)//' mismatch from'
            call MOSSCO_MessageAdd(message,' '//trim(nc%name)//'::'//trim(nc%variables(i)%name))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            write(message,'(A,I1,A)') '  rank ',nc%variables(i)%rank-1,' /= field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)

            cycle
            !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif
        endif

        !> @todo: test if varGrid conforms to grid
        if (gridRank==2) then
          call ESMF_FieldEmptySet(fieldList(i), grid2, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
        elseif (gridRank==3) then
          call ESMF_FieldEmptySet(fieldList(i), grid3, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
        else
          write(message,'(A)') trim(name)//' not implemented with gridrank <2 or >3'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          rc = ESMF_RC_NOT_IMPL
        endif
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        write(message,'(A)') trim(name)//' not implemented without foreign grid'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_NOT_IMPL
        return
      endif

      if (fieldRank>gridRank) then
        call ESMF_FieldEmptyComplete(fieldList(i), typekind=ESMF_TYPEKIND_R8, ungriddedLBound= &
          ungriddedLbnd, ungriddedUbound=ungriddedUbnd, rc=localrc)
      else
        call ESMF_FieldEmptyComplete(fieldList(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      endif
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call nc%getvar(fieldList(i), nc%variables(i), itime=itime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeGet(fieldList(i), 'creator', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (.not.isPresent) then
        call ESMF_AttributeSet(fieldList(i), 'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_AttributeSet(fieldList(i), 'netcdf_filename', trim(nc%name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(fieldList(i), 'netcdf_varname', trim(nc%variables(i)%name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message, '(A)') trim(name)//' created field'
      call MOSSCO_FieldString(fieldList(i), message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAdd(exportState, (/fieldList(i)/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    enddo

    if (allocated(ungriddedUbnd)) deallocate(ungriddedUbnd)
    if (allocated(ungriddedLbnd)) deallocate(ungriddedLbnd)

    call MOSSCO_Reallocate(filterExcludeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(filterIncludeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(aliasList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call nc%close(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isBundle) then
      write(message, '(A)') trim(name)//' will bundle fields with same name'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call MOSSCO_StateMoveFieldsToBundle(exportState, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      write(message, '(A)') trim(name)//' will not bundle fields'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_CompExit(gridComp)
    return

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: name
    type(ESMF_Time)            :: currTime
    integer(ESMF_KIND_I4)      :: localrc

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp)
    return

  end subroutine InitializeP2

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4) :: localrc

    rc = ESMF_SUCCESS

    !> Here omes your restart code, which in the simplest case copies
    !> values from all fields in importState to those in exportState

    call ESMF_StateReconcile(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateReconcile(exportState, rc=localrc)
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
    type(ESMF_Time)         :: currTime, stopTime, recentTime, nextTime
    type(ESMF_Time)         :: climatologyStartTime, climatologyTime
    type(ESMF_TimeInterval) :: timeStep, climatologyTimeStep
    integer(ESMF_KIND_I8)   :: i, j, advanceCount
    integer(ESMF_KIND_I4)   :: itemCount, localDeCount
    real(ESMF_KIND_R8)      :: weight
    type(ESMF_StateItem_Flag), allocatable, dimension(:) :: itemTypeList
    type(ESMF_Field)        :: field, nextField
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: itemNameList
    character(len=ESMF_MAXSTR) :: fileName
    character(len=ESMF_MAXSTR) :: addString
    type(ESMF_Clock)        :: clock
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(type_mossco_netcdf_variable), pointer    :: var => null()

    character(len=ESMF_MAXSTR) :: message, name, interpolationMethod
    integer(ESMF_KIND_I4)      :: localrc, itime, jtime
    logical                    :: isPresent
    character(len=ESMF_MAXSTR), allocatable :: aliasList(:,:)
    type(ESMF_TypeKind_Flag)   :: typeKind
    type(ESMF_Grid)            :: grid

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, 'filename', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      call MOSSCO_CompExit(gridComp)
      return
    endif

    call ESMF_AttributeGet(importState, 'filename', value=fileName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    inquire(file=trim(fileName), exist=isPresent)

    if (.not.isPresent) then
      call MOSSCO_CompExit(gridComp)
      return
    endif

    nc = MOSSCO_NetcdfOpen(trim(fileName), mode='r', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call nc%update_variables()
    call nc%update()

    if (nc%nvars==0) then
      write(message,'(A)') trim(name)//' contains no variables to read.'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      call MOSSCO_CompExit(gridComp)
      return
    endif

    call ESMF_StateGet(exportState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      call ESMF_StateGet(exportState, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_AttributeGet(importState, 'interpolation_method', value=interpolationMethod, &
      defaultValue='recent', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(gridComp, 'climatology_period', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(gridComp, 'climatology_period', timeString, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_TimeIntervalSet(climatologyTimeStep, trim(timeString), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeGet(gridComp, 'climatology_start', timeString, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_TimeSet(climatologyStartTime, timeString, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      climatologyTime = currTime
      do while (climatologyTime > (climatologyStartTime + climatologyTimeStep))
        climatologyTime = climatologyTime - climatologyTimeStep
      enddo

      call nc%timeIndex(climatologyTime, itime, jtime=jtime, weight=weight, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! if itime and jtime are equal, we have the time exactly represented in
      ! the netcdf file, or only one available time. In other cases, jtime
      ! will be greater itime
      if (jtime > itime) then
        ! Try to move itime and jtime within climatological window
        call MOSSCO_TimeSet(recentTime, timeString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call nc%timeGet(recentTime, searchIndex=itime, rc=localrc)
        if (recentTime < climatologyStartTime) then
          call nc%timeIndex(climatologyTime + climatologyTimeStep, itime, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call nc%timeGet(recentTime, searchIndex=itime, rc=localrc)
        endif

        call nc%timeGet(nextTime, searchIndex=jtime, rc=localrc)
        if (nextTime > climatologyStartTime + climatologyTimeStep) then
          call nc%timeIndex(climatologyStartTime, itime, jtime=jtime, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call nc%timeGet(recentTime, searchIndex=jtime, rc=localrc)
        endif

        if (trim(interpolationMethod) == 'recent') climatologyTime = recentTime
        if (trim(interpolationMethod) == 'next')   climatologyTime = nextTime
        if (trim(interpolationMethod) == 'nearest' .or. &
          trim(interpolationMethod) == 'linear') then
          call nc%timeIndex(recentTime, itime, rc=localrc)
          call nc%timeIndex(nextTime, jtime, rc=localrc)

          if (jtime > itime) then
            weight = (currTime - recentTime) / (nextTime - recentTime)
          else
            weight = (currTime - nextTime) / (nextTime - recentTime)
          endif

          if (trim(interpolationMethod) == 'nearest') then
            climatologyTime = recentTime
            if (weight > 0.5) climatologyTime = nextTime
          endif
        endif

      endif

      call ESMF_TimeGet(currTime, timeString=timeString, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' '//trim(timestring)//' uses climatological value from '

      if (trim(interpolationMethod) /= 'linear') then
        call ESMF_TimeGet(currTime, timeString=timeString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_MessageAdd(message, ' '//trim(interpolationMethod)//' time '//trim(timeString))
      elseif (jtime == itime) then
        call ESMF_TimeGet(currTime, timeString=timeString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_MessageAdd(message, ' linear interpolation at '//trim(timeString))

      else
        call ESMF_TimeGet(recentTime, timeString=timeString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_MessageAdd(message, ' linear interpolation '//trim(timeString))

        call ESMF_TimeGet(nextTime, timeString=timeString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_MessageAdd(message, ' to '//trim(timeString))

        write(addString,'(A,F4.2)') ', w=',weight
        call MOSSCO_MessageAdd(message, trim(addString))
      endif
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    else
      call nc%timeIndex(currTime, itime, jtime=jtime, weight=weight, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      climatologyTime = currTime
      if (trim(interpolationMethod) == 'recent') then
        call nc%timeGet(climatologyTime, searchIndex=itime, rc=localrc)
      elseif (trim(interpolationMethod) == 'nearest' .and. weight <= 0.5) then
        call nc%timeGet(climatologyTime, searchIndex=itime, rc=localrc)
      elseif (trim(interpolationMethod) == 'nearest' .and. weight > 0.5) then
        call nc%timeGet(climatologyTime, searchIndex=jtime, rc=localrc)
      elseif (trim(interpolationMethod) == 'next') then
        call nc%timeGet(climatologyTime, searchIndex=jtime, rc=localrc)
      endif

      if (localrc == ESMF_RC_NOT_FOUND) then
        write(message,'(A)') trim(name)//' uses constant value'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_TimeGet(currTime, timeString=timeString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' '//trim(timeString)//' uses'

        if (trim(interpolationMethod) /= 'linear') then
          call ESMF_TimeGet(climatologyTime, timeString=timeString, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call MOSSCO_MessageAdd(message, ' '//trim(interpolationMethod)//' from '//trim(timeString))
        elseif (jtime == itime) then
            call ESMF_TimeGet(climatologyTime, timeString=timeString, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call MOSSCO_MessageAdd(message, ' linear interpolated value at '//trim(timeString))
        else
            call ESMF_TimeGet(recentTime, timeString=timeString, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call MOSSCO_MessageAdd(message, ' linear interpolated value from '//trim(timeString))

            call ESMF_TimeGet(nextTime, timeString=timeString, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call MOSSCO_MessageAdd(message, ' and '//trim(timeString))

            write(message,'(A,F4.2)') trim(message)//', w=',weight
        endif
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    call MOSSCO_AttributeGetList(importState, 'alias_definition', aliasList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Go through list of export variables and fill their pointers with values from the file
    do i=1, itemCount

      if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) cycle

      call ESMF_StateGet(exportState, trim(itemNameList(i)), field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) cycle

      call ESMF_FieldGet(field, localDeCount=localDeCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (localDeCount < 1) cycle

      !! Convert aliases in file to proper item names
      if (allocated(aliasList)) then
        do j = lbound(aliasList,1), ubound(aliasList,1)
          if (trim(itemNameList(i)) == trim(aliasList(j,1))) then
            itemNameList(i)=trim(aliasList(j,2))
            exit
          endif
        enddo
      endif

      var => nc%getvarvar(trim(itemNameList(i)))
      !> @todo this needs an error message?
      if (.not.associated(var)) cycle

      !! @todo check shape of variable agains shape of field

      call nc%getvar(field, var, itime=int(itime, kind=ESMF_KIND_I4), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (trim(interpolationMethod) == 'linear' .and. (jtime /= itime)) then

        call ESMF_FieldGet(field, typeKind=typeKind, grid=grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        nextField = ESMF_FieldCreate(grid=grid, typeKind=typeKind, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call nc%getvar(nextField, var, itime=int(jtime, kind=ESMF_KIND_I4), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_FieldWeightField(field, nextField, weight, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldDestroy(nextField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      elseif ((trim(interpolationMethod) == 'next') &
        .or. (trim(interpolationMethod) == 'nearest' .and. weight > 0.5)) then
        call nc%getvar(field, var, itime=int(itime, kind=ESMF_KIND_I4), rc=localrc)
      endif
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    enddo

    call MOSSCO_Reallocate(aliasList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call nc%close()

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

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    integer(ESMF_KIND_I4)   :: localrc

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module netcdf_input_component
