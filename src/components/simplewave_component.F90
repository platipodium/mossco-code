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
  use mossco_variable_types
  use mossco_state
  use mossco_component

  implicit none
  private

  public :: SetServices

  type,extends(MOSSCO_VariableInfo)    :: variable_item_type
    type(variable_item_type), pointer  :: next => null()
    type(ESMF_Field), pointer          :: field => null()
    integer                            :: rank
    real(ESMF_KIND_R8), pointer        :: value(:,:) => null()
  end type

  real(ESMF_KIND_R8),parameter :: gravity=9.81d0
  type(variable_item_type), allocatable :: variableItemList(:)

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

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

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

    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    type(ESMF_Time)   :: clockTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8) :: dt
    integer                     :: myrank,i,j
    integer                     :: nimport,nexport
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Field), target     :: field
    type(ESMF_Grid)      :: grid
    integer              :: localrc
    integer              :: farray_shape(2)
    
    real(ESMF_KIND_R8), pointer           :: farrayPtr(:,:,:)

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Create the grid and coordinates
    !> This example grid is a 1 x 1 x 1 grid, you need to adjust this 
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/1,1/), &
      regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL, &
      name='simplewave grid', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   
    ! Get information to generate the fields that store the pointers to variables
    call ESMF_GridGet(grid,distgrid=distgrid,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridGetFieldBounds(grid=grid,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER,&
      totalCount=farray_shape,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Create export fields and add them to export state, allocate the space for these
    !> that will be filled later with data
    allocate(variableItemList(7))
    variableItemList(1)%name='wave_direction'
    variableItemList(1)%unit='degree_east'
    variableItemList(2)%name='wave_height'
    variableItemList(2)%unit='m'
    variableItemList(3)%name='wave_period'
    variableItemList(3)%unit='s'
    variableItemList(4)%name='wave_number'
    variableItemList(4)%unit=''
	  do i=1,4
	    variableItemList(i)%standard_name=trim(variableItemList(i)%name)
	    allocate(variableItemList(i)%value(farray_shape(1),farray_shape(2)))
	    variableItemList(i)%value=0.0
      field = ESMF_FieldCreate(grid, variableItemList(i)%value, indexflag=ESMF_INDEX_GLOBAL, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name=trim(variableItemList(i)%standard_name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      variableItemList(i)%field=>field
      
      call ESMF_AttributeSet(field,'units',trim(variableItemList(i)%unit), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo    

    variableItemList(5)%name='water_depth_at_soil_surface'
    variableItemList(6)%name='wind_x_velocity_at_10m'
    variableItemList(7)%name='wind_y_velocity_at_10m'

	  do i=5,7
      variableItemList(i)%standard_name=trim(variableItemList(i)%name)
	    field=ESMF_FieldEmptyCreate(name=trim(variableItemList(i)%standard_name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      variableItemList(i)%field=>field
      call ESMF_StateAddReplace(importState,(/field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call set_item_flags(importState,trim(variableItemList(i)%standard_name),requiredFlag=.true.,requiredRank=2)
    enddo

    !> @todo add optional fields (see Run method)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I8)   :: advanceCount
    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    type(ESMF_TimeInterval) :: timeInterval

    type(ESMF_Time)         :: clockTime, stopTime
    type(ESMF_StateItem_Flag) :: itemType
    integer(ESMF_KIND_I4)   :: itemCount
    integer(ESMF_KIND_I8)   :: n
    integer                 :: nvar
    real(ESMF_KIND_R8),dimension(:,:),pointer :: depth=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: wind=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: windDir=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: windx=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: windy=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: z0=>null()
    type(ESMF_Field)        :: Field
    character(len=ESMF_MAXSTR) :: string,varname
    real(ESMF_KIND_R8)           :: wdepth,wwind
    real(ESMF_KIND_R8)           :: Hrms,omegam1,uorb,aorb,Rew,tauwr,tauws
    real(ESMF_KIND_R8),parameter :: avmmolm1 = 1.8d6
    real(ESMF_KIND_R8),parameter :: sqrthalf=sqrt(0.5d0)
    real(ESMF_KIND_R8),parameter :: pi=3.1415926535897932384626433832795029d0
    real(ESMF_KIND_R8),parameter :: oneovertwopi=0.5d0/pi
    real(ESMF_KIND_R8),parameter :: Rew_crit = 5.0d5 ! (Stanev et al., 2009)
!   real(ESMF_KIND_R8),parameter :: Rew_crit = 1.5d5 ! (Soulsby & Clarke, 2005)
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

		waveH=>variableItemList(2)%value
		waveK=>variableItemList(4)%value
		waveDir=>variableItemList(2)%value
		waveT=>variableItemList(3)%value

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! associate local pointers with import data, check that all fields are complete
    !! before using them
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

        Hrms = sqrthalf * waveH(i,j)
        omegam1 = oneovertwopi * waveT(i,j)
!     peak wave orbital velocity (orbital velocity amplitude) at bottom (ubot in SWAN)
        uorb = 0.5d0 * Hrms / ( omegam1*sinh(waveK(i,j)*depth(i,j)) )
!     wave orbital excursion
        aorb = omegam1 * uorb
!     wave Reynolds number
        Rew = aorb * uorb * avmmolm1

!     Note (KK): We do not calculate fw alone, because for small
!                uorb this can become infinite.

!     KK-TODO: For combined wave-current flow, the decision on
!              turbulent or laminar flow depends on Rew AND Rec!
!              (Soulsby & Clarke, 2005)
!              However, here we decide according to Lettmann et al. (2009).
!              (Or we always assume turbulent currents.)
        if ( Rew .gt. Rew_crit ) then
!       rough turbulent flow
          tauwr = 0.5d0 * 1.39d0 * (omegam1/z0(i,j))**(-0.52d0) * uorb**(2-0.52d0)
!       smooth turbulent flow
          tauws = 0.5d0 * (omegam1*avmmolm1)**(-0.187d0) * uorb**(2-2*0.187d0)
!       Note (KK): For combined wave-current flow, the decision on
!                  rough or smooth flow depends on the final taubmax.
!                  (Soulsby & Clarke, 2005)
!                  However, here we decide according to Stanev et al. (2009).
!                  (as for wave-only flow)
          taubw(i,j) = max( tauwr , tauws )
        else
!       laminar flow
          taubw(i,j) = (omegam1*avmmolm1)**(-0.5d0) * uorb
        end if
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

    integer(ESMF_KIND_I4)   :: petCount, localPet, i
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    integer                 :: localrc

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,size(variableItemList)
      if (associated(variableItemList(i)%field)) nullify(variableItemList(i)%field)
      if (associated(variableItemList(i)%value)) nullify(variableItemList(i)%value)
    enddo
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

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: wind2waveHeight - estimates significant wave height from wind
!
! !INTERFACE:
   real(ESMF_KIND_R8) function wind2waveHeight(wind,depth)

! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   real(ESMF_KIND_R8),intent(in) :: wind,depth
!
! !DESCRIPTION:
!  Calculates significant wave height (Hm0) under assumption of unlimited fetch.
!  See page 250 in Holthuijsen (2007).
!
! !REVISION HISTORY:
!  Original author(s): Ulf Graewe
!                      Knut Klingbeil
!
! !LOCAL VARIABLES
   real(ESMF_KIND_R8)           :: depthstar,waveHeightstar
   real(ESMF_KIND_R8),parameter :: waveHeightstar8 = 0.24d0
   real(ESMF_KIND_R8),parameter :: k3 = 0.343d0
   real(ESMF_KIND_R8),parameter :: m3 = 1.14d0
   real(ESMF_KIND_R8),parameter :: p  = 0.572d0
    integer                      :: farray_shape(2)
!
!EOP
!-----------------------------------------------------------------------
!BOC

!  dimensionless depth
   depthstar = gravity * depth / wind**2

!  dimensionless significant wave height
   waveHeightstar = waveHeightstar8 * tanh(k3*depthstar**m3)**p

!  significant wave height
   wind2waveHeight = wind**2 * waveHeightstar / gravity

   end function wind2waveHeight
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: wind2wavePeriod - estimates peak wave period from wind
!
! !INTERFACE:
   real(ESMF_KIND_R8) function wind2wavePeriod(wind,depth)

! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   real(ESMF_KIND_R8),intent(in) :: wind,depth
!
! !DESCRIPTION:
!  Calculates peak wave period under assumption of unlimited fetch.
!  See page 250 in Holthuijsen (2007).
!  The peak wave period can be empirically related to the significant
!  wave period (Holthuijsen Eqs. (4.2.7) and (4.2.9)).
!
! !REVISION HISTORY:
!  Original author(s): Ulf Graewe
!                      Knut Klingbeil
!
! !LOCAL VARIABLES
   real(ESMF_KIND_R8)           :: depthstar,wavePeriodstar
   real(ESMF_KIND_R8),parameter :: wavePeriodstar8 = 7.69d0
   real(ESMF_KIND_R8),parameter :: k4 = 0.10d0
   real(ESMF_KIND_R8),parameter :: m4 = 2.01d0
   real(ESMF_KIND_R8),parameter :: q  = 0.187d0
 !
!EOP
!-----------------------------------------------------------------------
!BOC

!  dimensionless depth
   depthstar = gravity * depth / wind**2

!  dimensionless peak wave period
   wavePeriodstar = wavePeriodstar8 * tanh(k4*depthstar**m4)**q

!  peak wave period
   wind2wavePeriod = wind * wavePeriodstar / gravity

   end function wind2wavePeriod
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: wavePeriod2waveNumber - approximates wave number from wave period
!
! !INTERFACE:
   real(ESMF_KIND_R8) function wavePeriod2waveNumber(period,depth)

! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   real(ESMF_KIND_R8),intent(in) :: period,depth
!
! !DESCRIPTION:
!  x=k*D=kD,y=omega/sqrt(g/D)=omegastar
!  y=sqrt(x*tanh(x)),y(1)=0.8727=omegastar1
!  x'=lg(x),(dx'|dx)=1/(x*ln(10))
!  y'=lg(y),(dy'|dy)=1/(y*ln(10))
!  m'(x)=(dy'|dx')=(dy'|dy)*(dy|dx)*(dx|dx')=x/y*m(x)
!  m(x)=(dy|dx)=0.5*[tanh(x)+x*(1-tanh(x)**2)]/sqrt(x*tanh(x))
!  m(1)=0.677,m'(1)=0.77572=slopestar1
!  y'=lg(y(1))+m'(1)*x' <=> y=y(1)*[x**m'(1)] <=> x=(y/y(1))**(1/m'(1))
!  shallow: y=x       => x<=y(1)**(1/(1  -m'(1)))=0.5449  => y<=0.5449
!  deep   : y=sqrt(x) => x>=y(1)**(1/(0.5-m'(1)))=1.63865 => y>=1.28
!
!  For alternatives see Holthuijsen (2007) page 124
!  (Eckart, 1952 and Fenton, 1988)
!
! !REVISION HISTORY:
!  Original author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   real(ESMF_KIND_R8)           :: omega,omegastar,kD
   real(ESMF_KIND_R8),parameter :: omegastar1_rec = 1.0d0/0.8727d0
   real(ESMF_KIND_R8),parameter :: slopestar1_rec = 1.0d0/0.77572d0
   real(ESMF_KIND_R8),parameter :: one5th = 1.0d0/5
   real(ESMF_KIND_R8),parameter :: pi=3.1415926535897932384626433832795029d0


!
!EOP
!-----------------------------------------------------------------------
!BOC
 
   omega = 2 * pi / period ! radian frequency
   omegastar = omega * sqrt(depth/gravity) ! non-dimensional radian frequency

!!   approximation by Knut
!!   (errors less than 5%)
!   if ( omegastar .gt. 1.28d0 ) then
!!     deep-water approximation
!      kD = omegastar**2
!   else if ( omegastar .lt. 0.5449d0 ) then
!!     shallow-water approximation
!      kD = omegastar
!   else
!!     tangential approximation in loglog-space for full dispersion relation
!      kD = (omegastar1_rec * omegastar) ** slopestar1_rec
!   end if

!  approximation by Soulsby (1997, page 71) (see (18) in Lettmann et al., 2009)
!  (errors less than 1%)
   if ( omegastar .gt. 1.0d0 ) then
      kD = omegastar**2 * ( 1.0d0 + one5th*exp(2*(1.0d0-omegastar**2)) )
   else
      kD = omegastar * ( 1.0d0 + one5th*omegastar**2 )
   end if

   wavePeriod2waveNumber = kD / depth

   end function wavePeriod2waveNumber
!EOC
!-----------------------------------------------------------------------
end module simplewave_component
