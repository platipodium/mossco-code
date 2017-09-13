!> @brief Implementation of an ESMF component for simplewave
!!
!! This computer program is part of MOSSCO.
!! @copyright Copyright 2014, 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
!! @author Knut Klingbeil <knut.klingbeil@uni-hamburg.de>
!! @author Carsten Lemmen <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "simplewave_component.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define RANGE2D lbnd(1):ubnd(1),lbnd(2):ubnd(2)
#define RANGE3D lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)

module simplewave_component

  use esmf
  use simplewave_driver
  use mossco_variable_types
  use mossco_component
  use mossco_state
  use mossco_field
  use mossco_config
  use mossco_grid

  implicit none
  private

  public :: SetServices

  type(MOSSCO_VariableFArray2d),dimension(:),allocatable :: importList,exportList
  integer(ESMF_KIND_I4),dimension(:,:),pointer :: mask=>NULL()

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer              :: localrc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
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
  subroutine InitializeP0(gridComp, importState, exportState, clock, rc)

    implicit none

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(2)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    call MOSSCO_CompEntry(gridComp, clock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    InitializePhaseMap(1) = "IPDv00p1=1"
    InitializePhaseMap(2) = "IPDv00p2=2"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, clock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR)       :: foreignGridFieldName, message
    real(ESMF_KIND_R8), pointer      :: coordX(:), coordY(:)
    logical                          :: isPresent, hasGrid, foreignGridIsPresent
    character(len=ESMF_MAXSTR)       :: configFileName, gridFileName
    type(ESMF_Config)                :: config
    integer(ESMF_KIND_I4)            :: lbnd(2), ubnd(2), rank, localrc, i
    integer(ESMF_KIND_I4), allocatable :: totalUWidth(:), totalLWidth(:)

    character(ESMF_MAXSTR) :: name
    type(ESMF_Time)        :: currTime

    type(ESMF_Grid)        :: grid, grid2
    type(ESMF_Field)       :: field
    type(ESMF_StateItem_Flag) :: itemType

    call MOSSCO_CompEntry(gridComp, clock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !! Obtain a grid by trying to find on in the component, as a foreignGrid, or
    !! from a separate file
    hasGrid = .false.

    call ESMF_GridCompGet(gridComp, gridIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) then
      write(message,'(A)') trim(name)//' found grid in component'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

      call ESMF_GridCompGet(gridComp, grid=grid, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      hasGrid=.true.
    endif

    call ESMF_AttributeGet(importState, 'foreign_grid_field_name', isPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent .and. .not.hasGrid) then

      call ESMF_AttributeGet(importState, 'foreign_grid_field_name', foreignGridFieldName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_StateGetForeignGrid(importState, grid=grid, owner=trim(name), &
        attributeName='foreign_grid_field_name', totalUWidth=totalUWidth, &
        totalLWidth=totalLWidth, rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      foreignGridIsPresent = .true.
      hasGrid = .true.

    endif

    !! Check whether there is a config file with the same name as this component
    !! If yes, load it.
    configFileName=trim(name)//'.cfg'
    inquire(file=trim(configFileName), exist=isPresent)

    if (isPresent) then

      config = ESMF_ConfigCreate(rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ConfigLoadFile(config, configfilename, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, label='grid', value=gridFileName, &
        defaultValue=trim(name)//'_grid.nc', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      inquire(file=trim(gridFileName), exist=isPresent)
      if (isPresent) then
        grid = ESMF_GridCreate(trim(gridFileName),ESMF_FILEFORMAT_SCRIP,(/1,1/), &
          addCornerStagger=.true., rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AttributeSet(grid, 'creator', trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message,'(A,I6,A)') trim(name)//' uses regular grid from '//trim(gridFileName)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

        hasGrid = .true.
      endif
    endif

    if (.not.hasGrid) then
        grid = ESMF_GridCreateNoPeriDim(maxIndex=(/1,1/),coordDep1=(/1/),coordDep2=(/2/), &
          name="simplewaveGrid2D_"//trim(name),rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AttributeSet(grid,'creator',trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
          computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        do i=lbnd(1),ubnd(1)
          coordX(i) = i
        enddo
        call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
          computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        do i=lbnd(1),ubnd(1)
          coordY(i) = i
        enddo
    end if

    call ESMF_GridGet(grid, rank=rank, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (rank < 2 .or. rank > 3) then
      write(message, '(A,I1)') trim(name)//' needs grid with rank 2 or 3, but has', rank
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    if (rank == 3) then
      grid2 = MOSSCO_GridCreateFromOtherGrid(grid, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      grid = grid2
    endif

    call ESMF_GridCompSet(gridComp, grid=grid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    allocate(exportList(4))
    allocate(importList(3))

!!! Advertise Import Fields
    importList(1)%name  = 'water_depth_at_soil_surface'
    importList(1)%units = 'm'
    importList(2)%name  = 'wind_x_velocity_at_10m'
    importList(2)%units = 'm s-1'
    importList(3)%name  = 'wind_y_velocity_at_10m'
    importList(3)%units = 'm s-1'

    if (.not.allocated(totalUWidth)) then
      allocate(totalUWidth(2))
      totalUWidth(:) = 0
    endif
    if (.not.allocated(totalLWidth)) then
      allocate(totalLWidth(2))
      totalLWidth(:) = 0
    endif

    do i=1,size(importList)

      if (foreignGridIsPresent) then
        if (trim(importList(i)%name) == foreignGridFieldName) cycle
      end if

      field=ESMF_FieldEmptyCreate(name=trim(importList(i)%name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldEmptySet(field, grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field,'creator',trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field,'units',trim(importList(i)%units), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_StateAdd(importState,(/field/), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    end do

!!! Advertise Export Fields
    exportList(1)%name  = 'wave_height'
    exportList(1)%units = 'm'
    exportList(2)%name  = 'wave_period'
    exportList(2)%units = 's'
    exportList(3)%name  = 'wave_number'
    exportList(3)%units = 'm-1'
    exportList(4)%name  = 'wave_direction'
    exportList(4)%units = 'rad'

    do i=1,size(exportList)

      !! Avoid duplication of fields (this should actually never occur)
      call ESMF_StateGet(exportState, trim(exportList(i)%name), itemType=itemType, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        write(message,'(A)')  trim(name)//' got other than field type for item '//trim(exportList(i)%name)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call MOSSCO_StateLog(exportState, rc=localrc)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
      endif

      field = ESMF_FieldEmptyCreate(name=trim(exportList(i)%name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldEmptySet(field, grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field,'creator',trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field,'units',trim(exportList(i)%units), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_StateAdd(exportState,(/field/),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    end do

    if (allocated(totalUWidth)) deallocate(totalUWidth)
    if (allocated(totalLWidth)) deallocate(totalLWidth)
    !> @todo add optional fields (see Run method)

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, clock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR)           :: name
    type(ESMF_Time)                  :: currTime
    type(ESMF_Field), target         :: field
    type(ESMF_Grid)                  :: grid
    type(ESMF_FieldStatus_Flag)      :: status
    integer                          :: localrc

    integer, target                  :: coordDimCount(2), coordDimMap(2,2)
    integer, dimension(2)            :: totalLBound, totalUBound
    integer, dimension(2)            :: exclusiveLBound, exclusiveUBound
    integer                          :: i, j
    character(len=ESMF_MAXSTR)       :: message
    logical                          :: isPresent

    type :: allocatable_integer_array
      integer,dimension(:),allocatable :: data
    end type

    type(allocatable_integer_array) :: coordTotalLBound(2), coordTotalUBound(2)

    call MOSSCO_CompEntry(gridComp, clock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

!   Get the total domain size from the coordinates associated with the Grid
    call ESMF_GridCompGet(gridComp, grid=grid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridGet(grid,ESMF_STAGGERLOC_CENTER, 0, rc=localrc, &
        exclusiveLBound=exclusiveLBound,exclusiveUBound=exclusiveUBound)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridGet(grid,coordDimCount=coordDimCount,coordDimMap=coordDimMap, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    do i=1,2
      allocate(coordTotalLBound(i)%data(coordDimCount(i)))
      allocate(coordTotalUBound(i)%data(coordDimCount(i)))
      call ESMF_GridGetCoordBounds(grid,coordDim=i,                      &
                                   totalLBound=coordTotalLBound(i)%data, &
                                   totalUBound=coordTotalUBound(i)%data)
      do j=1,coordDimCount(i)
        if (coordDimMap(i,j) .eq. i) then
          totalLBound(i) = coordTotalLBound(i)%data(j)
          totalUBound(i) = coordTotalUBound(i)%data(j)
          exit
        end if
      end do
    end do

   call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, isPresent=isPresent, rc=localrc)
   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

   if (isPresent) then
     call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=mask, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) then
       call ESMF_LogWrite('ignore ERROR messages above related to GridGetItem - waiting for new ESMF release', &
                         ESMF_LOGMSG_INFO,ESMF_CONTEXT)
      endif
   end if

   if (isPresent .and. localrc == ESMF_SUCCESS) then

      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=mask, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
   else
      allocate(mask(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2)))
      mask = 0
      mask(exclusiveLBound(1):exclusiveUBound(1),exclusiveLBound(2):exclusiveUBound(2)) = 1
   end if

!   Complete Import Fields
    do i = lbound(importList,1), ubound(importList,1)
      call ESMF_StateGet(importState,trim(importList(i)%name),field)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(field,status=status, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (status == ESMF_FIELDSTATUS_GRIDSET) then
        write(message, '(A)') trim(name)//' imports internal field '
        call MOSSCO_FieldString(field, message)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

        allocate(importList(i)%data(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2)))

        call ESMF_FieldEmptyComplete(field, importList(i)%data, &
                                     totalLWidth=int(exclusiveLBound-totalLBound),  &
                                     totalUWidth=int(totalUBound-exclusiveUBound),  &
                                     rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        importList(i)%data = 0.0d0

      else if (status == ESMF_FIELDSTATUS_COMPLETE) then
        write(message, '(A)') trim(name)//' imports external field '
        call MOSSCO_FieldString(field, message)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

        call ESMF_FieldGet(field, farrayPtr=importList(i)%data, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (.not. (      all(lbound(importList(i)%data) .eq. totalLBound)          &
                   .and. all(ubound(importList(i)%data) .eq. totalUBound) ) ) then
          call ESMF_LogWrite('invalid field bounds', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
      else
        write(message, '(A)') trim(message)//' encountered unexpected empty field '//trim(importList(i)%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
    end do

!   Complete Export Fields
    do i=1,size(exportList)
      call ESMF_StateGet(exportState, trim(exportList(i)%name), field, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(field, status=status, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (status == ESMF_FIELDSTATUS_GRIDSET) then
        write(message, '(A)') trim(name)//' exports to internal field '
        call MOSSCO_FieldString(field, message)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

        allocate(exportList(i)%data(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2)))

        call ESMF_FieldEmptyComplete(field, exportList(i)%data, &
                                     totalLWidth=int(exclusiveLBound-totalLBound),  &
                                     totalUWidth=int(totalUBound-exclusiveUBound),  &
                                     rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        exportList(i)%data = 0.0d0

      else if (status == ESMF_FIELDSTATUS_COMPLETE) then
        write(message, '(A)') trim(name)//' exports to external field '
        call MOSSCO_FieldString(field, message)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

        call ESMF_FieldGet(field, farrayPtr=exportList(i)%data, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (.not. (      all(lbound(exportList(i)%data) .eq. totalLBound)          &
                   .and. all(ubound(exportList(i)%data) .eq. totalUBound) ) ) then
          call ESMF_LogWrite('invalid field bounds', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
      else
        write(message, '(A)') trim(message)//' encountered unexpected empty field '//trim(importList(i)%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
    end do

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP2

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    rc=ESMF_SUCCESS

    !> Here omes your restart code, which in the simplest case copies
    !> values from all fields in importState to those in exportState

  end subroutine ReadRestart


  subroutine Run(gridComp, importState, exportState, clock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR) :: name
    type(ESMF_Time)        :: currTime
    integer                :: localrc

    type(ESMF_Clock)        :: myClock
    type(ESMF_Time)         :: nextTime
    real(ESMF_KIND_R8),dimension(:,:),pointer :: waveH,waveT,waveK,waveDir
    real(ESMF_KIND_R8),dimension(:,:),pointer :: depth,windx,windy
    real(ESMF_KIND_R8)           :: wdepth,wind,wwind
    real(ESMF_KIND_R8),parameter :: max_depth_windwaves=30.0
    real(ESMF_KIND_R8),parameter :: kD_deepthresh = 100.0d0
    integer,dimension(2)         :: totalLBound,totalUBound
    integer                      :: i,j

    call MOSSCO_CompEntry(gridComp, clock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridcomp, clock=myClock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGetNextTime(clock,nextTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    depth   => importList(1)%data
    windx   => importList(2)%data
    windy   => importList(3)%data

    waveH   => exportList(1)%data
    waveT   => exportList(2)%data
    waveK   => exportList(3)%data
    waveDir => exportList(4)%data

    totalLBound = lbound(waveH)
    totalUBound = ubound(waveH)

    do j=totalLBound(2),totalUBound(2)
      do i=totalLBound(1),totalUBound(1)
        if ( mask(i,j) .gt. 0 ) then
        wind = sqrt( windx(i,j)*windx(i,j) + windy(i,j)*windy(i,j) )
        if (wind .gt. 0.0d0) then
          wdepth = min( depth(i,j) , max_depth_windwaves )
          waveH(i,j) = wind2waveHeight(wind,wdepth)
          waveT(i,j) = wind2wavePeriod(wind,wdepth)
          waveK(i,j) = wavePeriod2waveNumber(waveT(i,j),depth(i,j))
          waveDir(i,j) = atan2(windy(i,j),windx(i,j)) ! cartesian convention and in radians
        else
          waveH  (i,j) = 0.0d0
          waveT  (i,j) = 0.0d0
          waveK  (i,j) = kD_deepthresh / depth(i,j)
          waveDir(i,j) = 0.0d0
        end if
        end if
      end do
    end do

    call ESMF_ClockAdvance(myClock, timeStep=nextTime-currTime, rc=localrc)

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, clock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: myClock
    integer                 :: localrc

    call MOSSCO_CompEntry(gridComp, clock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    deallocate(importList)
    deallocate(exportList)

    call ESMF_GridCompGet(gridComp, clock=myClock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Finalize

end module simplewave_component
