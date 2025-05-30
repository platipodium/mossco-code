!> @brief Implementation of an ESMF grid input component
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2016, 2017, 2018 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "grid_component.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define RANGE3D lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)
#define RANGE2D lbnd(1):ubnd(1),lbnd(2):ubnd(2)
module grid_component

  use esmf
  use mossco_netcdf
  use mossco_field
  use mossco_strings
  use mossco_component
  use mossco_state
  use mossco_grid
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

    use mossco_variable_types
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
    type(ESMF_Grid)            :: grid, grid2, grid3
    type(ESMF_Field)           :: field
    character(len=ESMF_MAXSTR) :: configFileName, gridFileName, creator, fileFormat
    character(len=ESMF_MAXSTR) :: geomName, mask_variable
    type(ESMF_Config)          :: config

    integer(ESMF_KIND_I4)      :: itemCount, i, j, nlayer, halo
    integer(ESMF_KIND_I4)      :: fieldRank, gridRank
    integer(ESMF_KIND_I4), allocatable    :: ungriddedUbnd(:), ungriddedLbnd(:)
    integer(ESMF_KIND_I4), allocatable    :: decompositionList(:), dimList(:)
    real(ESMF_KIND_R8), allocatable       :: cornerList(:)
    type(ESMF_Vm)              :: vm

    type(ESMF_Array)                   :: array
    real(ESMF_KIND_R8), pointer        :: farrayPtr2(:,:)
    real(ESMF_KIND_R8), pointer        :: farrayPtr3(:,:,:)
    real(ESMF_KIND_R8), pointer        :: farrayPtr1(:)
    integer(ESMF_KIND_I4), allocatable :: lbnd(:), ubnd(:), coordDimCount(:)
    integer(ESMF_KIND_I4), allocatable :: exclusiveCount(:), coordDimIds(:)
    integer(ESMF_KIND_I4)              :: rank, dimCount
    type(ESMF_CoordSys_Flag)           :: coordSys
    logical                            :: hasMaskVariable

    rc = ESMF_SUCCESS
    hasGrid = .false.
    fileFormat = 'SCRIP'
    nlayer = 0
    halo = 0
    gridFileName = 'grid.nc'
    hasMaskVariable = .false.

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, petCount=petCount, localPet=localPet, &
      name=name, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    gridFileName = trim(name)//'.nc'
    configfilename=trim(name)//'.cfg'
    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (fileIsPresent) then

      write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      config = ESMF_ConfigCreate(rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, label='filename', value=gridFileName, &
        defaultValue='grid.nc', isPresent=labelIsPresent, rc = localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (labelIsPresent) then
        write(message,'(A)') trim(name)// ' found filename = '//trim(gridFileName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      !> Find out whether the label was specified.  If yes, then require
      !> the file to be present, and return if not found
      inquire(file=trim(gridFileName), exist=fileIsPresent)

      if (labelIsPresent .and. .not. fileIsPresent) then
        write(message, '(A)') trim(name)//' cannot find '//trim(gridFileName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        rc = ESMF_RC_NOT_FOUND
        return
      endif

      !> Alternatively read specifications from configuration without file
      !> This needs the dimensions and optionally corners specification
      !> and is @todo not completely implemented
      if (.not.fileIsPresent) then

        call MOSSCO_ConfigGet(config, label='dimensions', value=dimList, &
          isPresent=labelIsPresent, rc = localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (labelIsPresent) then
          write(message,'(A)') trim(name)//' found dimensions'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif

        call MOSSCO_ConfigGet(config, label='corners', value=cornerList, &
          isPresent=labelIsPresent, rc = localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (labelIsPresent) then
          write(message,'(A)') trim(name)//' found corners'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif

      !> Otherwise read the format specification
      else
        call MOSSCO_ConfigGet(config, label='format', value=fileFormat, &
          defaultValue='SCRIP', rc = localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (labelIsPresent) then
          write(message,'(A)') trim(name)//' found format = '//trim(fileFormat)
        else
          write(message,'(A)') trim(name)//' assumes format '//trim(fileFormat)
        endif
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      !> Convert strings to standard format strings
      call MOSSCO_CleanGeomFormatString(fileFormat, rc=localrc)

      call MOSSCO_ConfigGet(config, label='decomposition', value=decompositionList, isPresent=labelIsPresent, rc = localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (labelIsPresent) then
        write(message,'(A)') trim(name)//' found decomposition'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call MOSSCO_ConfigGet(config, label='layers', value=nlayer, defaultValue=nlayer, isPresent=labelIsPresent, rc = localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (labelIsPresent) then
        write(message,'(A,I3)') trim(name)//' found layers = ',nlayer
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call MOSSCO_ConfigGet(config, label='mask', value=mask_variable, &
        defaultValue='mask', isPresent=labelIsPresent, rc = localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (labelIsPresent) then
        write(message,'(A)') trim(name)//' found mask = '//trim(mask_variable)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        hasMaskVariable = .true.
      endif

      call MOSSCO_ConfigGet(config, label='halo', value=halo, &
        defaultValue=halo, isPresent=labelisPresent, rc = localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (labelIsPresent) then
        write(message,'(A,I1)') trim(name)//' found halo = ',halo
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    if (.not.allocated(decompositionList)) then
      allocate(decompositionList(2), stat=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      decompositionList(:) = (/petCount,1/)
    endif

    if (.not.fileIsPresent .and. allocated(cornerList)) then
      if (ubound(cornerList,1) /= 4 ) then
        write(message,'(A)') trim(name)//' needs four corners ll-lon ll-lat ur-lon ur-lat'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        rc = ESMF_RC_ARG_BAD
        return
      endif
      call MOSSCO_AttributeSet(gridComp, 'decomposition_list', decompositionList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    !> @todo handle cases where decompositionList exceeds number of processors
    if (ubound(decompositionList,1) > 1 ) then
      write(message,'(A)') trim(name)//' creates grid with decomposition '
      write(message,'(A,I3,A,I3)') trim(message)//' ',decompositionList(1),' x ',decompositionList(2)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_AttributeSet(gridComp, 'decomposition_list', decompositionList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(gridComp, 'number_of_vertical_layers', nlayer, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (hasMaskVariable) then
      call ESMF_AttributeSet(gridComp, 'mask_variable', trim(mask_variable), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call ESMF_AttributeSet(gridComp, 'width_of_halo', halo, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    inquire(file=trim(gridFileName), exist=isPresent)
    if (isPresent) then

      write(message,'(A)')  trim(name)//' reading '//trim(fileFormat)//' file '//trim(gridFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_AttributeSet(gridComp, 'grid_file', trim(gridFileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(gridComp, 'file_format', trim(fileFormat), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> @todo add halo in grid creation
      if (trim(fileFormat) == 'SCRIP') then
        grid = ESMF_GridCreate(filename=trim(gridFileName), fileFormat=ESMF_FILEFORMAT_SCRIP, &
          regDecomp=decompositionList, isSphere=.false., rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (trim(fileFormat) == 'GRIDSPEC') then
        if (hasMaskVariable) then
          grid = ESMF_GridCreate(filename=trim(gridFileName), &
            fileFormat=ESMF_FILEFORMAT_GRIDSPEC, regDecomp=decompositionList, &
            isSphere=.false., addMask=.true., varname=trim(mask_variable), &
            rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        else
          grid = ESMF_GridCreate(filename=trim(gridFileName), &
            fileFormat=ESMF_FILEFORMAT_GRIDSPEC, regDecomp=decompositionList, &
            isSphere=.false., rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
      else
        write(message, '(A)') trim(name)//' wrong file format '//trim(fileformat)//', valid options are SCRIP or GRIDSPEC'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        rc = ESMF_RC_NOT_IMPL
        return
      endif

      call ESMF_GridGet(grid, rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A,I1)') trim(name)//' read grid of rank ',rank
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      if (rank == 2) then
        grid2 = grid
        grid3 = MOSSCO_GridCreateFromOtherGrid(grid2, nlayer=nlayer, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 3) then
        grid3 = grid
        grid2 = MOSSCO_GridCreateFromOtherGrid(grid3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      else
        write(message, '(A,I1,A)') trim(name)//' wrong rank ',rank,' valid ranks are 2 or 3'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        rc = ESMF_RC_NOT_IMPL
        return
      endif

      ! !> Try to add a mask from a variable, hardcoded for now
      ! if (hasMaskVariable .and. trim(fileFormat) == 'GRIDSPEC' .and. rank==2) then
      !   call MOSSCO_GridAddMaskFromVariable(grid2, gridFileName, trim(mask_variable), &
      !     owner=trim(name), rc=localrc)
      !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      !   write(message, '(A)') trim(name)//' added grid mask from '//trim(mask_variable)
      !   call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      ! endif

      call ESMF_GridGet(grid3, name=geomName)

      write(message, '(A)') trim(name)//' created grid from file '//trim(gridFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      hasGrid=.true.

    else

      write(message,'(A)')  trim(name)//' creates grid from parameters'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      if (.not.allocated(dimList)) then
        allocate(dimList(2))
        dimList = (/40,40/)
      endif
      if (ubound(dimList,1) /= 2) then
        write(message, '(A)') trim(name)//' wrong number of dimensions provided '
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        rc = ESMF_RC_NOT_IMPL
        return
      endif

      if (.not.allocated(cornerList)) then
        !! This should be the lat-lon coordinates of the lower left and upper
        !! right corner as a list of four values (ll-lon, ll-lat, ur-lon, ur-lat)
        allocate(cornerList(4))
        cornerList = (/4,50,9,55/)
      endif

      if (ubound(cornerList,1) /= 4) then
        write(message, '(A)') trim(name)//' wrong number of corners provided'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        rc = ESMF_RC_NOT_IMPL
        return
      endif

      grid3 = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), &
        maxIndex=(/dimList(1), dimList(2), nlayer/), &
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        indexflag=ESMF_INDEX_GLOBAL,  &
        name=trim(name), coordTypeKind=ESMF_TYPEKIND_R8, coordDep1=(/1/), &
        coorddep2=(/1/),rc=rc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)

      call ESMF_GridAddCoord(grid3, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)

      call ESMF_GridAddCoord(grid3, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, rc=rc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)

      if (.not.allocated(lbnd)) allocate(lbnd(1))
      if (.not.allocated(ubnd)) allocate(ubnd(1))

      call ESMF_GridGetCoord(grid3, coordDim=1, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        exclusiveLBound=lbnd, exclusiveUBound=ubnd, farrayPtr=farrayPtr1, rc=rc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)

      do i=lbnd(1), ubnd(1)
        farrayPtr1(i) = cornerList(1) + (i - 0.5) * (cornerList(3)-cornerList(1))
      enddo

      call ESMF_GridGetCoord(grid3, coordDim=2, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        exclusiveLBound=lbnd, exclusiveUBound=ubnd, farrayPtr=farrayPtr1, rc=rc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)

      do i=lbnd(1), ubnd(1)
        farrayPtr1(i) = cornerList(2) + (i - 0.5) * (cornerList(4)-cornerList(2))
      enddo
      write(*,*) 'lats = ',farrayPtr1(:)

      call ESMF_GridGetCoord(grid3, coordDim=3, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        exclusiveLBound=lbnd, exclusiveUBound=ubnd, farrayPtr=farrayPtr1, rc=rc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)

      do i=lbnd(1), ubnd(1)
        farrayPtr1(i) = i - 0.5
      enddo

      call ESMF_GridGetCoord(grid3, coordDim=3, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
        exclusiveLBound=lbnd, exclusiveUBound=ubnd, farrayPtr=farrayPtr1, rc=rc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)

      do i=lbnd(1), ubnd(1)
        farrayPtr1(i) = i - 1
      enddo
      write(*,*) 'vfaces = ',farrayPtr1(:)

      if (allocated(lbnd)) deallocate(lbnd)
      if (allocated(ubnd)) deallocate(ubnd)

    endif

    if (nlayer > 0) then
      grid = grid3
    else
      grid = grid2
    endif

    call ESMF_GridCompSet(gridComp, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)

    call ESMF_GridGet(grid, coordSys=coordSys, dimCount=dimCount, &
      name=gridFileName, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (coordSys /= ESMF_COORDSYS_SPH_DEG) then
      write(message,'(A)') trim(name)//' has not implementation for non-spherical grid'
      rc = ESMF_RC_NOT_SET
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      return
    endif

    call ESMF_AttributeSet(grid, 'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    field = ESMF_FieldEmptyCreate(name=trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldEmptySet(field, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (nlayer > 0) then
      call ESMF_FieldEmptyComplete(field, typeKind=ESMF_TYPEKIND_I4, &
        totalUWidth=(/halo,halo,0/), totalLWidth=(/halo,halo,0/), rc=localrc)
    else
      call ESMF_FieldEmptyComplete(field, typeKind=ESMF_TYPEKIND_I4, &
        totalUWidth=(/halo,halo/), totalLWidth=(/halo,halo/), rc=localrc)
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(name)//' created field '
    call MOSSCO_FieldString(field, message)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_StateAddReplace(exportState, (/field/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.allocated(coordDimCount)) allocate(coordDimCount(dimCount))
    call ESMF_GridGet(grid, coordDimCount=coordDimCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i = lbound(coordDimCount,1), ubound(coordDimCount,1)

      if (.not.allocated(coordDimids)) allocate(coordDimids(coordDimCount(i)))
      if (.not.allocated(exclusiveCount)) allocate(exclusiveCount(coordDimCount(i)))
      if (.not.allocated(lbnd)) allocate(lbnd(coordDimCount(i)))
      if (.not.allocated(ubnd)) allocate(ubnd(coordDimCount(i)))

      select case (coordDimCount(i))
        case (1)
          call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr1, &
            exclusiveLBound=lbnd, exclusiveUBound=ubnd, rc=localrc)
          case (2)
            call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr2, &
              exclusiveLBound=lbnd, exclusiveUBound=ubnd, rc=localrc)
          case (3)
            call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr3, &
              exclusiveLBound=lbnd, exclusiveUBound=ubnd, rc=localrc)
        case default
          write(message,'(A)')  '  cannot deal with less than 1 or more than 3 coordinate dimensions'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      end select
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_GridGetCoord(grid, i, staggerloc=ESMF_STAGGERLOC_CENTER, array=array, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(coordDimids)) deallocate(coordDimids)
      if (allocated(exclusiveCount)) deallocate(exclusiveCount)
      if (allocated(lbnd)) deallocate(lbnd)
      if (allocated(ubnd)) deallocate(ubnd)

    enddo


    if (allocated(ungriddedUbnd)) deallocate(ungriddedUbnd)
    if (allocated(ungriddedLbnd)) deallocate(ungriddedLbnd)
    if (allocated(decompositionList)) deallocate(decompositionList)
    if (allocated(coordDimCount)) deallocate(coordDimCount)

    call MOSSCO_CompExit(gridComp, rc=localrc)

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

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
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

    call MOSSCO_CompExit(gridComp, rc=localrc)
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

    call MOSSCO_CompExit(gridComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module grid_component
