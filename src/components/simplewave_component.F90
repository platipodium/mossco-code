!> @brief Implementation of an ESMF component for simplewave
!!
!! This computer program is part of MOSSCO. 
!! @copyright Copyright 2014, Helmholtz-Zentrum Geesthacht
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

  implicit none
  private

  public :: SetServices

  type(MOSSCO_VariableFArray2d), allocatable :: variableItemList(:)

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

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
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
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR) :: name
    type(ESMF_Time)        :: currTime
    integer                :: localrc

    type(ESMF_Field)       :: field
    integer                :: i

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(variableItemList(4+3))

!!! Advertise Export Fields
    variableItemList(1)%name='wave_height'
    variableItemList(1)%unit='m'
    variableItemList(2)%name='wave_period'
    variableItemList(2)%unit='s'
    variableItemList(3)%name='wave_number'
    variableItemList(3)%unit='1/m'
    variableItemList(4)%name='wave_direction'
    variableItemList(4)%unit='rad'

    do i=1,4
      variableItemList(i)%standard_name=trim(variableItemList(i)%name)
      field = ESMF_FieldEmptyCreate(name=trim(variableItemList(i)%standard_name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'units',trim(variableItemList(i)%unit), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(exportState,(/field/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

!!! Advertise Import Fields
    variableItemList(4+1)%name='water_depth_at_soil_surface'
    variableItemList(4+1)%unit='m'
    variableItemList(4+2)%name='wind_x_velocity_at_10m'
    variableItemList(4+2)%unit='m/s'
    variableItemList(4+3)%name='wind_y_velocity_at_10m'
    variableItemList(4+3)%unit='m/s'

    do i=4+1,4+3
      variableItemList(i)%standard_name=trim(variableItemList(i)%name)
      field=ESMF_FieldEmptyCreate(name=trim(variableItemList(i)%standard_name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(importState,(/field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

    !> @todo add optional fields (see Run method)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1


#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR)  :: name,message
    character(len=ESMF_MAXSTR) :: foreignGridFieldName
    type(ESMF_Time)         :: currTime
    integer                     :: i
    type(ESMF_Field), target     :: field
    type(ESMF_Grid)      :: grid
    integer              :: localrc
    integer              :: rank,farray_shape(2)
    real(ESMF_KIND_R8), pointer           :: coordX(:), coordY(:)
    
    logical                         :: gridIsPresent,fileIsPresent,labelIsPresent
    character(ESMF_MAXSTR)          :: configFileName, gridFileName
    type(ESMF_Config)               :: config
    integer(ESMF_KIND_I4)           :: lbnd(2), ubnd(2)
    

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!!! Create Grid
    call ESMF_GridCompGet(gridComp,gridIsPresent=gridIsPresent)
    if (gridIsPresent) then
      call ESMF_GridCompGet(gridComp,grid=grid)
    else
      call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
                             value=foreignGridFieldName, defaultValue='none',rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      if (trim(foreignGridFieldName)/='none') then
        call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldGet(field, grid=grid, rank=rank, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        if (rank .ne. 2) then
          write(message,*) 'foreign grid must be of rank = 2'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
          call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        end if
      else
        !! Check whether there is a config file with the same name as this component
        !! If yes, load it. 
        configFileName=trim(name)//'.cfg'
        inquire(FILE=trim(configFileName), exist=fileIsPresent)   
        if (fileIsPresent) then 
          config = ESMF_ConfigCreate(rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_ConfigLoadFile(config, configfilename, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_ConfigFindLabel(config, label='grid:', isPresent=labelIsPresent, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_ConfigGetAttribute(config, gridFileName, rc=localrc, default=trim(name)//'_grid.nc')
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          grid = ESMF_GridCreate(trim(gridFileName),ESMF_FILEFORMAT_SCRIP,(/1,1/), &
                                 addCornerStagger=.true., rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A,I6,A)') trim(name)//' uses regular grid from '//trim(gridFileName)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
        else
          grid = ESMF_GridCreateNoPeriDim(maxIndex=(/1,1/),coordDep1=(/1/),coordDep2=(/2/),name=trim(name)//'Grid',rc=localrc)
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
      call ESMF_GridCompSet(gridComp,grid=grid)
    end if
    ! Get information to generate the fields that store the pointers to variables

    call ESMF_GridGetFieldBounds(grid=grid,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER,&
      totalCount=farray_shape,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(0,*) farray_shape
    !> Create export fields and add them to export state, allocate the space for these
    !> that will be filled later with data

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP2


  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR)  :: name,message
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    type(ESMF_Time)         :: stopTime
    type(ESMF_StateItem_Flag) :: itemType
    integer(ESMF_KIND_I4)   :: itemCount
    real(ESMF_KIND_R8),dimension(:,:),pointer :: depth=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: wind=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: windDir=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: windx=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: windy=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: z0=>null()
    type(ESMF_Field)        :: Field
    real(ESMF_KIND_R8)           :: wdepth,wwind
    real(ESMF_KIND_R8),parameter :: min_wind=0.1d0
    real(ESMF_KIND_R8),parameter :: max_depth_windwaves=99999.0
    logical                      :: calc_wind,calc_windDir,calc_taubw
    logical,save                 :: taubw_ready=.false.
    integer                      :: i,j

    integer                      :: farray_shape(2)
    type(ESMF_Grid)      :: grid
    type(ESMF_FieldStatus_Flag)  :: fieldStatus
    integer                     :: localrc

    real(ESMF_KIND_R8),dimension(:,:),allocatable,target :: taubw
    real(ESMF_KIND_R8), pointer, dimension(:,:) :: waveH,waveT,waveDir,waveK

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridcomp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! associate local pointers with import data, check that all fields are complete
    !! before using them

    waveH   => variableItemList(1)%data
    waveT   => variableItemList(2)%data
    waveK   => variableItemList(3)%data
    waveDir => variableItemList(4)%data

    call ESMF_StateGet(importState, itemSearch='water_depth_at_soil_surface', &
      itemCount=itemCount, rc = localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (itemCount == 0) then
       write(message,'(A)') trim(name)//' required import field water_depth_at_soil_surface not found.' 
       call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
       call ESMF_StatePrint(importState)
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
    call ESMF_StateGet(importState, "water_depth_at_soil_surface", field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        
    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
       write(message,'(A)') trim(name)//' required import field water_depth_at_soil_surface not complete.' 
       call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
       call ESMF_StatePrint(importState)
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
    call ESMF_FieldGet(field, farrayPtr=depth, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(importState, "wind_speed", itemType)
    if (itemType .eq. ESMF_STATEITEM_NOTFOUND) then
       calc_wind = .true.
    else
       calc_wind = .false.
       call ESMF_StateGet(importState, "wind_speed", Field)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
       call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
       if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
         calc_wind=.true.
       else
         call ESMF_FieldGet(field, farrayPtr=wind)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
       endif
    endif

    call ESMF_StateGet(importState, "wind_direction", itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (itemType .eq. ESMF_STATEITEM_NOTFOUND) then
       calc_windDir = .true.
    else
       calc_windDir = .false.
       call ESMF_StateGet(importState, "wind_direction", Field)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
       call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
       if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
         calc_windDir=.true.
       else
         call ESMF_FieldGet(field, farrayPtr=windDir)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    call ESMF_StateGet(importState, "bottom_roughness_length", itemType)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (itemType .eq. ESMF_STATEITEM_NOTFOUND) then
       calc_taubw = .false.
    else
       calc_taubw = .true.
       call ESMF_StateGet(importState, "bottom_roughness_length", Field)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
       call ESMF_FieldGet(field, farrayPtr=z0)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

       if (.not. taubw_ready) then
          if (.not.(allocated(taubw))) allocate(taubw(farray_shape(1),farray_shape(2)))
          taubw = 0.0d0
          Field = ESMF_FieldCreate(grid,taubw,                             &
                                   indexflag=ESMF_INDEX_GLOBAL,          &
                                   staggerloc=ESMF_STAGGERLOC_CENTER,    &
                                   name='wave_only_bottom_stress', rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_StateAddReplace(exportState,(/Field/),rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          taubw_ready = .true.
          call ESMF_LogWrite('simplewave component post-initialized for bottom stress.',ESMF_LOGMSG_TRACE)
       end if
    end if

    if (calc_wind .or. calc_windDir) then    
      call ESMF_StateGet(importState, itemSearch='wind_x_velocity_at_10m', &
        itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (itemCount == 0) then
        write(message,'(A)') trim(name)//' required import field wind_x_velocity_at_10m not found.' 
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_StatePrint(importState)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
      call ESMF_StateGet(importState, "wind_x_velocity_at_10m", field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
        write(message,'(A)') trim(name)//' required import field wind_x_velocity_at_10m not complete.' 
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
      call ESMF_FieldGet(field, farrayPtr=windx, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateGet(importState, itemSearch='wind_y_velocity_at_10m', &
        itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (itemCount == 0) then
        write(message,'(A)') trim(name)//' required import field wind_y_velocity_at_10m not found.' 
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_StatePrint(importState)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
      call ESMF_StateGet(importState, "wind_y_velocity_at_10m", field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
        write(message,'(A)') trim(name)//' required import field wind_y_velocity_at_10m not complete.' 
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
      call ESMF_FieldGet(field, farrayPtr=windy, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if

    if (calc_wind) then
      if (.not. associated(wind)) allocate(wind(farray_shape(1),farray_shape(2)))
      wind = sqrt(windx**2 + windy**2)
    end if
    if (calc_windDir) then
      if (.not. associated(windDir)) allocate(windDir(farray_shape(1),farray_shape(2)))
      windDir = atan2(windy,windx) ! cartesian convention and in radians
    end if
   
    waveDir = windDir
    do j=1,farray_shape(2)
      do i=1, farray_shape(1)
      wwind = max( min_wind , wind(i,j) )
      wdepth = min( depth(i,j) , max_depth_windwaves )
      waveH(i,j) = wind2waveHeight(wwind,wdepth)
      waveT(i,j) = wind2wavePeriod(wwind,wdepth)
      waveK(i,j) = wavePeriod2waveNumber(waveT(i,j),depth(i,j))

        if (calc_taubw) then

          taubw(i,j) = wbbl_tauw(waveT(i,j),waveH(i,j),waveK(i,j),depth(i,j),z0(i,j))

        end if

      enddo
    enddo
    
    call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
 
    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    integer                 :: localrc

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    deallocate(variableItemList)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   
    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory    
    !! 3. Destroy your clock

    !! @todo The clockIsPresent statement does not detect if a clock has been destroyed 
    !! previously, thus, we comment the clock destruction code while this has not
    !! been fixed by ESMF
    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module simplewave_component
