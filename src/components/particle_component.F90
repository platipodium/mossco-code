!> @brief Implementation of an ESMF particle component
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2019 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "particle_component.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module particle_component

  use esmf
  use mossco_netcdf
  use mossco_field
  use mossco_strings
  use mossco_component
  use mossco_state
  use mossco_attribute
  use mossco_config
  use mossco_time
  use mossco_geom
  !use particle_driver

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
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
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
    integer                     :: localrc

    rc=ESMF_SUCCESS

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

    character(len=ESMF_MAXSTR) :: timeString, message, name
    character(len=ESMF_MAXSTR) :: form
    type(ESMF_Time)            :: currTime
    integer(ESMF_KIND_I4)      :: petCount, localPet, localRc
    type(ESMF_Clock)           :: clock

    logical                    :: isPresent, fileIsPresent, labelIsPresent, hasGrid
    logical                    :: hasDimension
    type(ESMF_Grid)            :: grid, grid2, grid3
    type(ESMF_Field)           :: field
    character(len=ESMF_MAXSTR) :: configFileName, fileName, creator, dimensionName
    character(len=ESMF_MAXSTR) :: geomName, itemName, string
    type(ESMF_Config)          :: config

    integer(ESMF_KIND_I4)      :: itemCount, i, j, nlayer
    integer(ESMF_KIND_I4)      :: fieldRank, gridRank
    integer(ESMF_KIND_I4), allocatable    :: ungriddedUbnd(:), ungriddedLbnd(:)
    integer(ESMF_KIND_I4), allocatable    :: decompositionList(:), dimList(:)
    real(ESMF_KIND_R8), allocatable       :: cornerList(:)
    type(ESMF_Vm)              :: vm

    type(ESMF_Array)                   :: array

    real(ESMF_KIND_R8), allocatable, target  :: farray(:)
    integer(ESMF_KIND_I4), allocatable :: lbnd(:), ubnd(:), coordDimCount(:)
    integer(ESMF_KIND_I4), allocatable :: exclusiveCount(:), coordDimIds(:)
    integer(ESMF_KIND_I4)              :: rank, dimCount, dimensionId
    integer(ESMF_KIND_I4)              :: numLocationsOnThisPet
    type(ESMF_CoordSys_Flag)           :: coordSys


    logical                            :: checkFile
    type(ESMF_LocStream)               :: locStream
    type(type_mossco_netcdf_variable), pointer  :: var => null()

    character(len=ESMF_MAXSTR)         :: foreignGridFieldName
    type(ESMF_Field)                   :: newField
    type(ESMF_LocStream)               :: newLocstream
    type(ESMF_RouteHandle)             :: routeHandle
    type(ESMF_FILEFORMAT_Flag)         :: fileFormat

    character(len=ESMF_MAXSTR), allocatable :: keyNames(:), fieldNames(:)
    character(len=ESMF_MAXSTR)              :: keyUnits, keyLongName
    integer(ESMF_KIND_I4)                   :: keyCount
    type(ESMF_TYPEKIND_Flag)                :: typeKind
    real(ESMF_KIND_R8), pointer             :: farrayPtr1(:)

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, petCount=petCount, localPet=localPet, name=name, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    dimensionName = 'nparts'
    fileName = trim(name)//'.nc'
    checkFile = .true.

    configfilename=trim(name)//'.cfg'
    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (fileIsPresent) then

      write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_GridCompGet(gridComp, configIsPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (isPresent) then
        call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      else
        config = ESMF_ConfigCreate(rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, label='filename', value=fileName, &
        defaultValue=trim(name)//'.nc', isPresent=labelIsPresent, rc = localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      ! if the filename is present, also look at other paramters relevant to file,
      ! i.e. checkFile, fileFormat
      if (labelIsPresent) then

        call ESMF_AttributeSet(gridComp, 'file_name', trim(fileName), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call MOSSCO_ConfigGet(config, label='checkFile', value=checkFile, &
          defaultValue=checkFile, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call MOSSCO_AttributeSet(gridComp, 'check_file', checkFile, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        inquire(file=trim(fileName), exist=isPresent)
        if (.not.isPresent) then
          write(message, '(A)') trim(name)//' cannot find '//trim(fileName)
          if (checkFile) then
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            rc = ESMF_RC_NOT_FOUND
          else
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          endif
          return
        endif

        call MOSSCO_ConfigGet(config, label='format', value=string, &
          defaultValue='SCRIP', rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        select case(trim(adjustl(string)))
        case('scrip','SCRIP')
          fileFormat=ESMF_FILEFORMAT_SCRIP
        case ('ugrid','UGRID')
          fileFormat = ESMF_FILEFORMAT_UGRID
        case ('esmf','ESMF')
          fileFormat = ESMF_FILEFORMAT_ESMFMESH
        case default
          write(message, '(A)') trim(name)//' invalid format '//trim(string)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          rc = ESMF_RC_NOT_FOUND
          return
        end select

      else

        call MOSSCO_ConfigGet(config, label='dimension', value=dimensionName, &
          defaultValue='nparts', rc = localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AttributeSet(gridComp, 'dimension', dimensionName, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    inquire(file=trim(fileName), exist=isPresent)
    if (isPresent) then

      write(message,'(A)')  trim(name)//' reading file '//trim(fileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_AttributeSet(gridComp, 'file_name', trim(fileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_GeomCreate(fileName, fileFormat=fileFormat, geomType=ESMF_GEOMTYPE_LOCSTREAM, &
        locStream=locStream, owner=name, writeVtk=.true., rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' created locStream from file '//trim(fileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    !> Obtain a grid onto which to partition the locStream
    call ESMF_AttributeGet(importState, 'foreign_grid_field_name', &
      isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    ! if (isPresent) then
    !   call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
    !     value=foreignGridFieldName, rc=localrc)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   call MOSSCO_StateGetForeignGrid(importState, grid, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   call ESMF_GridGet(grid, rank=rank, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   !> @todo make sure the grid rank corresponds to locstream coord key number (currently 2)
    !
    !   !> @todo Fix this interface (seems unknown)
    !   !newLocStream = ESMF_LocStreamCreate(locStream, coordKeyNames="Lon:Lat", &
    !   ! background=grid, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    ! endif

    call ESMF_GridCompSet(gridComp, locstream=locstream, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(locStream, 'creator', trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_LocStreamGet(locStream, keyCount=keycount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (keyCount > 0) then
      allocate(keyNames(keycount), stat=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_LocStreamGet(locStream, keyNames=keyNames, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    do i=1,keyCount

      call ESMF_LocStreamGetKey(locStream, keyNames(i), keyUnits=keyUnits, &
        keyLongName=keyLongName, typeKind=typeKind, rc=localrc)

      write(message,'(A)') trim(name)//' has key '//trim(keyNames(i))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      field = ESMF_FieldEmptyCreate(name=trim(keyLongName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldEmptySet(field, locStream=locStream, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> If a grid was provided, then create a new field, create a redistribution,
      !> and move the data from field to the newField
      if (isPresent) then

        newField = ESMF_FieldEmptyCreate(name=trim(keyNames(i)), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldEmptySet(newField, locStream=newLocStream, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldRedistStore(field, newField, routeHandle, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldRedist(field, newField, routeHandle, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldRedistRelease(routeHandle, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldDestroy(field, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        field=newField

      endif

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'units', trim(keyUnits), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'long_name', trim(keyLongName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (typeKind == ESMF_TYPEKIND_R8) then
        call ESMF_LocStreamGetKey(locstream, keyNames(i), farray=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldEmptyComplete(field, farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      else
        write(message,'(A)') trim(name)//' skipped key '//trim(keynames(i))// &
          ', no implementation for this type'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      call ESMF_StateAddReplace(exportState, (/field/), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' created field '
      call MOSSCO_FieldString(field, message)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    enddo

    allocate(fieldNames(3), stat=localrc)
    fieldNames(1) = 'x_velocity_in_water'
    fieldNames(2) = 'y_velocity_in_water'
    fieldNames(3) = 'z_velocity_in_water'

    do i=1, 1
      field = ESMF_FieldEmptyCreate(name=trim(fieldNames(i)), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'units', 'm s-1', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldEmptySet(field, locStream=locStream, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_StateAddReplace(importState, (/field/), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    enddo

    call MOSSCO_Reallocate(fieldNames, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_Reallocate(keyNames, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP1

  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4)      :: localrc
    character(len=ESMF_MAXSTR) :: name

    rc = ESMF_SUCCESS

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

    !> @TODO read the new positiosn from a time field (or obtain from immport state)

  end subroutine ReadRestart

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    type(ESMF_Time)            :: currTime, stopTime
    character(len=ESMF_MAXSTR) :: name
    type(ESMF_TimeInterval)    :: timeStep
    integer(ESMF_KIND_I4)      :: localrc
    type(ESMF_Clock)           :: clock

    type(ESMF_Field), allocatable :: fieldList(:)
    type(ESMF_LocStream)          :: locStream
    type(ESMF_GeomType_Flag)      :: geomType
    type(ESMF_Typekind_Flag)      :: typeKind
    real(ESMF_KIND_R8), pointer   :: farrayPtr1(:)
    character(ESMF_MAXSTR), allocatable :: keyNames(:)
    integer(ESMF_KIND_I4)         :: fieldCount, i, j, keycount
    character(len=ESMF_MAXSTR)    :: keyUnits
    real(ESMF_KIND_R8)            :: seconds
    type(ESMF_CoordSys_Flag)      :: CoordSys
    character(len=ESMF_MAXSTR)    :: message

    real(ESMF_KIND_R8), parameter :: pi = 4.D0 * datan(1.D0)
    real(ESMF_KIND_R8),parameter  :: radius = 6371000.0d0

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !! For this component, it does not make sense to advance its clock by a regular
    !! timestep.  Thus, it is advanced to the next alarm time.

    call MOSSCO_ClockGetTimeStepToNextAlarm(clock, timeStep, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_TimeIntervalGet(timeStep, s_r8=seconds, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    do i=1, 1 !fieldCount

      !> @todo avoid that a locstream is advected several times !

      call ESMF_FieldGet(fieldList(i), locStream=locStream, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_LocStreamGet(locstream, keyCount=keyCount, coordSys=coordSys, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (keyCount < 1) cycle
      if (coordSys /= ESMF_COORDSYS_SPH_DEG) then
        write(message, '(A)') trim(name)//' has not implementation for non-spherical coordinates'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      allocate(keyNames(keyCount), stat=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_LocStreamGet(locstream, keyNames=keyNames, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      do j=1, keyCount
        call ESMF_LocStreamGetKey(locStream, keyNames(j), keyUnits=keyUnits, &
          typeKind=typeKind, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (typeKind /= ESMF_TYPEKIND_R8) cycle

        call ESMF_LocStreamGetKey(locStream, keyNames(j),  &
          farray=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        select case(trim(keyNames(j)))
        case ('ESMF:Lon')
          !> @todo get x-wind component from importState, for now assume constant
          !> current comes as m s-1, so needs to be converted to lat
          farrayPtr1 = farrayPtr1 + seconds * 0.1 * 360 / (2 * pi * radius)
        case ('ESMF:Lat')
          !> @todo get y-wind component from importState, for now assume constant
          !> current comes as m s-1, so needs to be converted to lat
          farrayPtr1 = farrayPtr1 + seconds * 0.1 * 360 / (2 * pi * radius)
        case ('ESMF:Radius')
          !> @todo get z-wind component from importState, for now assume constant
          farrayPtr1 = farrayPtr1 + seconds * 0.1
        endselect

      enddo ! j loop over keyCount

      deallocate(keyNames, stat=localrc)
    enddo ! i loop over fieldCount

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !if (timeStep>0) then
      call ESMF_ClockAdvance(clock, timeStep=timeStep, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !endif

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
      importState=importState, exportState=exportState, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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

end module particle_component
