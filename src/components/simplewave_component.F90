!> @brief Implementation of an ESMF component for simplewave
!!
!! This computer program is part of MOSSCO.
!! @copyright Copyright 2014, 2015 Helmholtz-Zentrum Geesthacht
!! @author Knut Klingbeil, IOW
!! @author Carsten Lemmen, HZG

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

module simplewave_component

  use esmf
  use simplewave_driver
  use mossco_variable_types
  use mossco_component
  use mossco_state

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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

    call MOSSCO_CompEntry(gridComp, clock, name, currTime, localrc)
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


#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, clock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: foreignGridFieldName,message
    integer              :: rank
    real(ESMF_KIND_R8), pointer           :: coordX(:), coordY(:)
    logical                         :: isPresent,foreignGridIsPresent=.false.
    character(ESMF_MAXSTR)          :: configFileName, gridFileName
    type(ESMF_Config)               :: config
    integer(ESMF_KIND_I4)           :: lbnd(2), ubnd(2)

    character(ESMF_MAXSTR) :: name
    type(ESMF_Time)        :: currTime
    integer                :: localrc

    type(ESMF_Grid)        :: grid
    type(ESMF_Field)       :: field
    integer                :: i
    type(ESMF_StateItem_Flag) :: itemType

    call MOSSCO_CompEntry(gridComp, clock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!!! Create Grid
    call ESMF_GridCompGet(gridComp,gridIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_GridCompGet(gridComp,grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)
    else
      call ESMF_AttributeGet(importState, 'foreign_grid_field_name', isPresent=foreignGridIsPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (foreignGridIsPresent) then
        call ESMF_AttributeGet(importState, name='foreign_grid_field_name', value=foreignGridFieldName, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_StateGetForeignGrid(importState, grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_GridGet(grid, rank=rank, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (rank .ne. 2) then
          write(message,*) 'foreign grid must be of rank = 2'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
          call MOSSCO_StateLog(importState)
          call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        end if
      else
        !! Check whether there is a config file with the same name as this component
        !! If yes, load it.
        configFileName=trim(name)//'.cfg'
        inquire(FILE=trim(configFileName), exist=isPresent)
        if (isPresent) then
          config = ESMF_ConfigCreate(rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_ConfigLoadFile(config, configfilename, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_ConfigFindLabel(config, label='grid:', isPresent=isPresent, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_ConfigGetAttribute(config, gridFileName, rc=localrc, default=trim(name)//'_grid.nc')
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          grid = ESMF_GridCreate(trim(gridFileName),ESMF_FILEFORMAT_SCRIP,(/1,1/), &
                                 addCornerStagger=.true., rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_AttributeSet(grid,'creator',trim(name), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A,I6,A)') trim(name)//' uses regular grid from '//trim(gridFileName)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
        else
          grid = ESMF_GridCreateNoPeriDim(maxIndex=(/1,1/),coordDep1=(/1/),coordDep2=(/2/), &
                                          name="simplewaveGrid2D_"//trim(name),rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           call ESMF_AttributeSet(grid,'creator',trim(name), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
             computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          do i=lbnd(1),ubnd(1)
            coordX(i) = i
          enddo
          call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
             computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          do i=lbnd(1),ubnd(1)
            coordY(i) = i
          enddo
        end if
      end if
      call ESMF_GridCompSet(gridComp, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    allocate(exportList(4))
    allocate(importList(3))

!!! Advertise Import Fields
    importList(1)%name  = 'water_depth_at_soil_surface'
    importList(1)%units = 'm'
    importList(2)%name  = 'wind_x_velocity_at_10m'
    importList(2)%units = 'm/s'
    importList(3)%name  = 'wind_y_velocity_at_10m'
    importList(3)%units = 'm/s'

    do i=1,size(importList)

      if (foreignGridIsPresent) then
        if (trim(importList(i)%name) == foreignGridFieldName) cycle
      end if

      field=ESMF_FieldEmptyCreate(name=trim(importList(i)%name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldEmptySet(field, grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'creator',trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'units',trim(importList(i)%units), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(importState,(/field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

!!! Advertise Export Fields
    exportList(1)%name  = 'wave_height'
    exportList(1)%units = 'm'
    exportList(2)%name  = 'wave_period'
    exportList(2)%units = 's'
    exportList(3)%name  = 'wave_number'
    exportList(3)%units = '1/m'
    exportList(4)%name  = 'wave_direction'
    exportList(4)%units = 'rad'

    do i=1,size(exportList)

      !! Avoid duplication of fields (this should actually never occur)
      call ESMF_StateGet(exportState, trim(exportList(i)%name), itemType=itemType, rc=localrc)
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        write(message,'(A)')  trim(name)//' got other than field type for item '//trim(exportList(i)%name)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call MOSSCO_StateLog(exportState)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      endif

      field = ESMF_FieldEmptyCreate(name=trim(exportList(i)%name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldEmptySet(field,grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'creator',trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'units',trim(exportList(i)%units), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(exportState,(/field/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

    !> @todo add optional fields (see Run method)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1


#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, clock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime

    type(ESMF_Field), target     :: field
    type(ESMF_Grid)      :: grid
    type(ESMF_FieldStatus_Flag)     :: status
    integer              :: localrc

    integer,target :: coordDimCount(2),coordDimMap(2,2)
    integer,dimension(2)            :: totalLBound,totalUBound
    integer,dimension(2)            :: exclusiveLBound,exclusiveUBound
    integer                         :: i,j
    type :: allocatable_integer_array
      integer,dimension(:),allocatable :: data
    end type
    type(allocatable_integer_array) :: coordTotalLBound(2),coordTotalUBound(2)


    call MOSSCO_CompEntry(gridComp, clock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!   Get the total domain size from the coordinates associated with the Grid
    call ESMF_GridCompGet(gridComp,grid=grid)
    call ESMF_GridGet(grid,ESMF_STAGGERLOC_CENTER,0,                                   &
                      exclusiveLBound=exclusiveLBound,exclusiveUBound=exclusiveUBound)
    call ESMF_GridGet(grid,coordDimCount=coordDimCount,coordDimMap=coordDimMap)
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

   !> The preferred interface would be to use isPresent, but htis only works in ESMF from Nov 2014
   !> @todo replace if 0 by ESMF_VERSION macros
#if 0
   call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, isPresent=isPresent, rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   if (isPresent) then
#else
   call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=mask, rc=localrc)
   !! Do not check for success here as NOT_FOUND is expected behaviour, @todo: check for NOT_FOUND flag
   if (localrc == ESMF_SUCCESS) then
#endif
      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=mask)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   else
      allocate(mask(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2)))
      mask = 0
      mask(exclusiveLBound(1):exclusiveUBound(1),exclusiveLBound(2):exclusiveUBound(2)) = 1
   end if

!   Complete Import Fields
    do i=1,size(importList)
      call ESMF_StateGet(importState,trim(importList(i)%name),field)
      call ESMF_FieldGet(field,status=status)
      if (status.eq.ESMF_FIELDSTATUS_GRIDSET) then
        allocate(importList(i)%data(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2)))
        call ESMF_FieldEmptyComplete(field,importList(i)%data,                &
                                     ESMF_INDEX_DELOCAL,                      &
                                     totalLWidth=exclusiveLBound-totalLBound, &
                                     totalUWidth=totalUBound-exclusiveUBound)
      else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
        call ESMF_FieldGet(field,farrayPtr=importList(i)%data,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT,rc=rc)
        call ESMF_LogWrite(' import from external field '//trim(importList(i)%name),ESMF_LOGMSG_INFO)
        if (.not. (      all(lbound(importList(i)%data) .eq. totalLBound) &
                   .and. all(ubound(importList(i)%data) .eq. totalUBound) ) ) then
          call ESMF_LogWrite('invalid field bounds',ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
      else
        call ESMF_LogWrite('empty field: '//trim(importList(i)%name),ESMF_LOGMSG_ERROR, &
                           line=__LINE__,file=__FILE__,method='InitializeP2()')
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
    end do

!   Complete Export Fields
    do i=1,size(exportList)
      call ESMF_StateGet(exportState,trim(exportList(i)%name),field)
      call ESMF_FieldGet(field,status=status)
      if (status.eq.ESMF_FIELDSTATUS_GRIDSET) then
        allocate(exportList(i)%data(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2)))
        call ESMF_FieldEmptyComplete(field,exportList(i)%data,                &
                                     ESMF_INDEX_DELOCAL,                      &
                                     totalLWidth=exclusiveLBound-totalLBound, &
                                     totalUWidth=totalUBound-exclusiveUBound)
      else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
        call ESMF_FieldGet(field,farrayPtr=exportList(i)%data,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT,rc=rc)
        call ESMF_LogWrite(' export to external field '//trim(exportList(i)%name),ESMF_LOGMSG_INFO)
        if (.not. (      all(lbound(exportList(i)%data) .eq. totalLBound) &
                   .and. all(ubound(exportList(i)%data) .eq. totalUBound) ) ) then
          call ESMF_LogWrite('invalid field bounds',ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
      else
        call ESMF_LogWrite('empty field: '//trim(exportList(i)%name),ESMF_LOGMSG_ERROR, &
                           line=__LINE__,file=__FILE__,method='InitializeP2()')
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
    end do

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP2


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
    real(ESMF_KIND_R8),parameter :: max_depth_windwaves=99999.0
    real(ESMF_KIND_R8),parameter :: kD_deepthresh = 100.0d0
    integer,dimension(2)         :: totalLBound,totalUBound
    integer                      :: i,j

    call MOSSCO_CompEntry(gridComp, clock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridcomp, clock=myClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockGetNextTime(clock,nextTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
        if (mask(i,j) .ne. 0) then
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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

    call MOSSCO_CompEntry(gridComp, clock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    deallocate(importList)
    deallocate(exportList)

    call ESMF_GridCompGet(gridComp, clock=myClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory
    !! 3. Destroy your clock

    !! @todo The clockIsPresent statement does not detect if a clock has been destroyed
    !! previously, thus, we comment the clock destruction code while this has not
    !! been fixed by ESMF
    call ESMF_ClockDestroy(myClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module simplewave_component
