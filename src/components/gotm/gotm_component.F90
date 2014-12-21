!> @brief Implementation of a GOTM wrapper as ESMF component
!
!> This module serves as a wrapper for the General Ocean Turbulence Model
!> (GOTM). This model describes a 1D water column.
!> @import 
!> @export water_temperature, grid_height, (FABM variables)
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, 2014, Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "gotm_component.F90"

#ifndef GOTM_REALTYPE
#define GOTM_REALTYPE real(kind=selected_real_kind(13))
#endif

#ifndef _ZERO_
#define _ZERO_ 0.0d0
#endif

#ifndef _ONE_
#define _ONE_  1.0d0
#endif

module gotm_component

  use esmf
  use time, only: gotm_time_min_n => MinN, gotm_time_max_n => MaxN
  use time, only: gotm_time_timestep => timestep
  use time, only: gotm_time_start => start, gotm_time_stop => stop
  use time, only: gotm_time_timefmt => timefmt
  use time, only: gotm_time_init_time => init_time
  use time, only: timestepkind,update_time
  use gotm, only: init_gotm, gotm_time_loop => time_loop, clean_up
  use output, only: prepare_output,do_output,gotm_output_nsave => nsave

  use mossco_variable_types
  use mossco_component
  
  implicit none

  private
 
  real(ESMF_KIND_R8), allocatable, target :: variables_3d(:,:,:,:)
  real(ESMF_KIND_R8), allocatable, target :: variables_2d(:,:,:)
  type(MOSSCO_VariableFArray3d), dimension(:), allocatable :: export_variables_3d
  type(MOSSCO_VariableFArray2d), dimension(:), allocatable :: export_variables_2d
  real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: coordZ

   !> Declare an alarm to ring when output to file is requested
  type(ESMF_Alarm),save :: outputAlarm

  !> local variables for the setup control
  character(len=80)         :: title,name
  integer                   :: nlev
  GOTM_REALTYPE             :: cnpar,latitude,longitude,depth
  GOTM_REALTYPE             :: T0,S0,p0,dtr0,dsr0
  integer                   :: buoy_method,eq_state_mode,eq_state_method
    
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

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)         :: gridComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: parentClock
    integer, intent(out)        :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc=localrc)
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

    use meanflow, only : gotm_temperature => T
    use meanflow, only : gotm_salinity => S
    use meanflow, only : gotm_heights => h 
    use meanflow, only : gotm_radiation => rad
    use meanflow, only : gotm_u => u
    use meanflow, only : gotm_v => v
    use meanflow, only: h
    use turbulence, only : gotm_tknu => num

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_Clock)  :: clock 
    character(len=19) :: timestring
    type(ESMF_Time)   :: wallTime, currTime, stopTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8) :: dt
    integer                     :: lbnd(3), ubnd(3),farray_shape(3)
    integer                     :: myrank,i,j,k
    integer                     :: nimport,nexport_3d, nexport_2d
    real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2=>null()
    
    logical                    :: clockIsPresent
    integer(ESMF_KIND_I8)      :: advanceCount
    integer(ESMF_KIND_I4)      :: localPet, petCount, localrc
    integer(ESMF_KIND_I4)      :: hours, minutes, seconds
    character(len=ESMF_MAXSTR) :: message, name

    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Grid)      :: grid,grid2d
    type(ESMF_ArraySpec) :: arrayspec
    
    type(ESMF_Field), dimension(:), allocatable  :: exportFieldList
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr  
    type(ESMF_Field) :: field
      
    namelist /model_setup/ title,nlev,dt,cnpar,buoy_method
    namelist /station/ name,latitude,longitude,depth
    namelist /eqstate/ eq_state_mode,eq_state_method,T0,S0,p0,dtr0,dsr0
 
    rc = ESMF_SUCCESS
    
    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call init_gotm()

    ! read model_setup namelist
    open(921,file='gotmrun.nml',status='old',action='read')
    read(921,nml=model_setup)
    read(921,nml=station)
    close(921)

    ! copy start and stop time from clock to gotm's time parameters
    call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_time_timestep,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockSet(clock, timeStep=timeInterval, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    gotm_time_start=timestring(1:10)//" "//timestring(12:19)

    call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    gotm_time_stop=timestring(1:10)//" "//timestring(12:19)

    gotm_time_timefmt = 2 
    call gotm_time_init_time(gotm_time_min_n,gotm_time_max_n)      

    !! The output timestep is used to create an alarm in the  Clock
    !> @todo implement this also driven by the parent clock
    call ESMF_ClockGet(clock,startTime=currTime) 
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_output_nsave*gotm_time_timestep,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! Introduced dependency to FABM component, which use the same name for the alarm
    outputAlarm = ESMF_AlarmCreate(clock=parentclock,name="GOTM output Alarm", &
      ringTime=currTime+timeInterval,ringInterval=timeInterval,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    outputAlarm = ESMF_AlarmCreate(clock=clock,name="GOTM output Alarm", &
      ringTime=currTime+timeInterval,ringInterval=timeInterval,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Create the grid and coordinates
    !> This example grid is a 1 x 1 x nlev grid 
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/1,1,nlev/), &
      regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name=trim(name)//'3d',coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),gridAlign=(/1,1,1/),gridEdgeLWidth=(/1,1,1/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   
    grid2d = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/1,1/), &
      regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name=trim(name)//'2d',coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridAddCoord(grid2d,staggerloc=ESMF_STAGGERLOC_CENTER,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER_VFACE,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    do i=lbnd(1),ubnd(1) 
      coordX(i) = longitude
    enddo
    call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    do i=lbnd(1),ubnd(1) 
      coordY(i) = latitude
    enddo
    call ESMF_GridGetCoord(grid2d,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    do i=lbnd(1),ubnd(1) 
      coordX(i) = longitude
    enddo
    call ESMF_GridGetCoord(grid2d,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    do i=lbnd(1),ubnd(1) 
      coordY(i) = latitude
    enddo 
    call ESMF_GridGetCoord(grid,coordDim=3,localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
      farrayPtr=coordZ, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    coordZ(1,1,0) = -depth
    do i=1,nlev 
      coordZ(1,1,i) = coordZ(1,1,i-1) + h(i)
    enddo

    ! Get information to generate the fields that store the pointers to variables
    call ESMF_GridGet(grid,distgrid=distgrid,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridGetFieldBounds(grid=grid,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER,&
      totalCount=farray_shape,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    nexport_3d = 6
    nexport_2d = 8
    allocate(exportFieldList(nexport_3d + nexport_2d))

    !> Create 3d export fields and add them to export state, allocate the space for these
    !> that will be filled later with data, copying of data is necessary to provide 3d fields
    !> for ESMF
    
    allocate(export_variables_3d(nexport_3d))
    export_variables_3d(1)%standard_name="temperature"
    export_variables_3d(2)%standard_name="grid_height"
    export_variables_3d(3)%standard_name="salinity"
    export_variables_3d(4)%standard_name="radiation"
    export_variables_3d(5)%standard_name="x_velocity"
    export_variables_3d(6)%standard_name="y_velocity"
    allocate(variables_3d(farray_shape(1),farray_shape(2),0:farray_shape(3),nexport_3d))
    
    call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    
    do k=1,nexport_3d
      farrayPtr => variables_3d(:,:,:,k)
      exportFieldList(k) = ESMF_FieldCreate(grid, farrayPtr=farrayPtr, name=trim(export_variables_3d(k)%standard_name)//'_in_water', &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        totalLWidth=(/0,0,1/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateAddReplace(exportState,(/exportFieldList(k)/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo
    
    !> fill export fields
    do k=1,nlev
      variables_3d(:,:,k,1) = gotm_temperature(k)
      variables_3d(:,:,k,2) = gotm_heights(k)
      variables_3d(:,:,k,3) = gotm_salinity(k)
      variables_3d(:,:,k,4) = gotm_radiation(k)
      variables_3d(:,:,k,5) = gotm_u(k)
      variables_3d(:,:,k,6) = gotm_v(k)
    end do

    !> Create 2d export fields and add them to export state, allocate the space for these
    !> that will be filled later with data, copying of data is necessary to provide 2d fields
    !> for ESMF
    
    allocate(export_variables_2d(nexport_2d))
    export_variables_2d(1)%standard_name="water_depth_at_soil_surface"
    export_variables_2d(2)%standard_name="layer_height_at_soil_surface"
    export_variables_2d(3)%standard_name="depth_averaged_x_velocity_in_water"
    export_variables_2d(4)%standard_name="depth_averaged_y_velocity_in_water"
    export_variables_2d(5)%standard_name="x_velocity_at_soil_surface"
    export_variables_2d(6)%standard_name="y_velocity_at_soil_surface"
    export_variables_2d(7)%standard_name="temperature_at_soil_surface"
    export_variables_2d(8)%standard_name="turbulent_kinematic_viscosity_at_soil_surface"



    allocate(variables_2d(farray_shape(1),farray_shape(2),nexport_2d))
    
    !!> @todo bound checking and not restricting to 1 column in the following calls
    variables_2d(1,1,1) = sum(variables_3d(1,1,:,2))
    variables_2d(1,1,2) = variables_3d(1,1,1,2)
    variables_2d(1,1,3) = sum (variables_3d(1,1,:,2) * variables_3d(1,1,:,5)) / variables_2d(1,1,1)
    variables_2d(1,1,4) = sum (variables_3d(1,1,:,2) * variables_3d(1,1,:,6)) / variables_2d(1,1,1)
    variables_2d(1,1,5) = variables_3d(1,1,1,5)
    variables_2d(1,1,6) = variables_3d(1,1,1,6)
    variables_2d(1,1,7) = variables_3d(1,1,1,1)
    variables_2d(1,1,8) = gotm_tknu(1)

    
    call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    
    do k=1,nexport_2d
      ptr_f2 => variables_2d(:,:,k)
      exportFieldList(k) = ESMF_FieldCreate(grid2d, farrayPtr=ptr_f2, name=trim(export_variables_2d(k)%standard_name), &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        totalLWidth=(/0,0/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateAddReplace(exportState,(/exportFieldList(k)/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo
    
    deallocate(exportFieldList)
    
    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
subroutine Run(gridComp, importState, exportState, parentClock, rc)

    use meanflow, only : gotm_temperature => T
    use meanflow, only : gotm_salinity => S
    use meanflow, only : gotm_heights => h 
    use meanflow, only : gotm_radiation => rad
    use meanflow, only : gotm_u => u
    use meanflow, only : gotm_v => v
    use turbulence, only : gotm_tknu => num

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_Clock)  :: clock 
    character(len=19)       :: timestring
    type(ESMF_Time)         :: wallTime, currTime, stopTime
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I8)   :: n,k, advanceCount
    integer                 :: itemcount,nvar
    real(ESMF_KIND_R8),pointer,dimension(:,:)  :: ptr_f2
    real(ESMF_KIND_R8),pointer,dimension(:,:,:):: ptr_f3
    type(ESMF_Field)        :: Field
    character(len=ESMF_MAXSTR) :: string, varname, message, name
    
    logical                 :: clockIsPresent
    integer(ESMF_KIND_I4)   :: petCount, localPet
    integer(ESMF_KIND_I4)   :: seconds, hours, localrc

    rc = ESMF_SUCCESS
    
    call MOSSCO_CompEntry (gridComp, parentClock, name, currTime, localrc)
    if  (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock,currTime=currTime,  timeStep=timeInterval, &
      stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running with dt='
    call ESMF_TimeIntervalGet(timeInterval,s=seconds)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A,I5,A)') trim(message),seconds,' s to '
    call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(message)//' '//trim(timeString)//' ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    do while (.not.ESMF_ClockIsStopTime(clock))

       call ESMF_ClockGet(clock,currTime=currTime, advanceCount=n, &
         timeStep=timeInterval, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifdef DEBUG
       call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

       write(message,'(A,I5)') trim(timestring)//" GOTM iteration ", n
       call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
#endif

       call update_time(n)
       call gotm_time_step()

       !> Check if the output alarm is ringing, if so, quiet it and 
       !> call do_output from GOTM
       if (ESMF_AlarmIsRinging(outputAlarm)) then
         call ESMF_AlarmRingerOff(outputAlarm,rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call prepare_output(n)
         call do_output(n,nlev)
       endif

       
       if (timeInterval > stopTime-currTime) timeInterval = stopTime-currTime
       call ESMF_ClockAdvance(clock, timeStep=timeInterval, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

    !> update grid of export fields
    do k=1,nlev 
      coordZ(1,1,k) = coordZ(1,1,k-1) + gotm_heights(k)
    enddo

    !> update export fields
    do k=1,nlev
      variables_3d(:,:,k,1) = gotm_temperature(k)
      variables_3d(:,:,k,2) = gotm_heights(k)
      variables_3d(:,:,k,3) = gotm_salinity(k)
      variables_3d(:,:,k,4) = gotm_radiation(k)
      variables_3d(:,:,k,5) = gotm_u(k)
      variables_3d(:,:,k,6) = gotm_v(k)
    end do
 
    !!> @todo bound checking and not restricting to 1 column in the following calls
    variables_2d(1,1,1) = sum(variables_3d(1,1,:,2))
    variables_2d(1,1,2) = variables_3d(1,1,1,2)
    variables_2d(1,1,3) = sum (variables_3d(1,1,:,2) * variables_3d(1,1,:,5)) / variables_2d(1,1,1)
    variables_2d(1,1,4) = sum (variables_3d(1,1,:,2) * variables_3d(1,1,:,6)) / variables_2d(1,1,1)
    variables_2d(1,1,5) = variables_3d(1,1,1,5)
    variables_2d(1,1,6) = variables_3d(1,1,1,6)
    variables_2d(1,1,7) = variables_3d(1,1,1,1)
    variables_2d(1,1,8) = gotm_tknu(1)
 
    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
 
  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_Clock)  :: clock 
    integer                      :: lbnd(3), ubnd(3), k
    real(ESMF_KIND_R8),pointer   :: farrayPtr(:,:,:)
    type(ESMF_Field)             :: field
    type(ESMF_StateItem_Flag), allocatable    :: itemTypeList(:)
    integer(ESMF_KIND_I4)        :: itemCount, localrc, i
    character(len=ESMF_MAXSTR)   :: name, message
    character(len=ESMF_MAXSTR),allocatable   :: itemNameList(:)
    logical                      :: clockIsPresent
    type(ESMF_Time)              :: currTime

!!> @todo StateGet gets a destroyed state here in the generic coupling, this needs to be fixed
    rc = ESMF_SUCCESS
    
    call MOSSCO_CompEntry (gridComp, parentClock, name, currTime, localrc)
    if  (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, name=name, itemCount=itemCount, rc=localRc)
    if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    if (itemCount /= 0) then
      allocate(itemTypeList(itemCount))
      allocate(itemNameList(itemCount))
      call ESMF_StateGet(exportState, itemTypeList=itemTypeList, itemNameList=itemNameList, &
        rc=localRc)
      if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif
    do i=1,itemCount
      if (itemtypeList(i) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(exportState, trim(itemNameList(i)), field, rc=localrc)
        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)        
      
#if ESMF_VERSION_MAJOR > 5
        call ESMF_StateRemove(exportState,trim(itemNameList(i)),rc=localrc)
#else
        call ESMF_StateRemove(exportState,trim(itemNameList(i)),rc=localrc)
#endif
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    enddo

    if (allocated(itemNameList)) deallocate(itemNameList)
    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(variables_3d)) deallocate(variables_3d)
    if (allocated(variables_2d)) deallocate(variables_2d)

    !! @todo The clockIsPresent statement does not detect if a clock has been destroyed 
    !! previously, thus, we comment the clock destruction code while this has not
    !! been fixed by ESMF
    call ESMF_GridCompGet(gridcomp, clockIsPresent=clockIsPresent,rc=localrc)
    if (clockIsPresent) then
      call ESMF_GridCompGet(GridComp,clock=clock,rc=localrc)
      call ESMF_ClockDestroy(clock, rc=localrc)
    end if
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call clean_up()
    rc = ESMF_SUCCESS
    
    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

  !> Actually, this should be an extension of ESMF_TimeSet 
#undef  ESMF_METHOD
#define ESMF_METHOD "timeString2ESMF_Time"
  subroutine timeString2ESMF_Time(timestring,time)
    character(len=*), intent(in) :: timestring
    type(ESMF_Time), intent(out) :: time

    integer :: yy,mm,dd,h,m,s

    read(timestring(1:4),'(i4)') yy
    read(timestring(6:7),'(i2)') mm
    read(timestring(9:10),'(i2)') dd
    read(timestring(12:13),'(i2)') h
    read(timestring(15:16),'(i2)') m
    read(timestring(18:19),'(i2)') s

    call ESMF_TimeSet(time,yy=yy,mm=mm,dd=dd,h=h,m=m,s=s)

  end subroutine timeString2ESMF_Time

#undef  ESMF_METHOD
#define ESMF_METHOD "gotm_time_step"
  subroutine gotm_time_step()

  use time, only: julianday,secondsofday,timestep,timestepkind
  use meanflow
  use input
  use observations
  use airsea
  use turbulence
  use kpp

  GOTM_REALTYPE             :: tFlux,btFlux,sFlux,bsFlux
  GOTM_REALTYPE             :: tRad(0:nlev),bRad(0:nlev)


!     all observations/data
  call do_input(julianday,secondsofday,nlev,z)
  call get_all_obs(julianday,secondsofday,nlev,z)

!     external forcing
  if( calc_fluxes ) then
    call set_sst(T(nlev))
    call set_ssuv(u(nlev),v(nlev))
  end if
  call do_air_sea(julianday,secondsofday)

!     reset some quantities
  tx = tx/rho_0
  ty = ty/rho_0

!     meanflow integration starts
  call updategrid(nlev,timestep,zeta)
  call coriolis(nlev,timestep)

!     update velocity
  call uequation(nlev,timestep,cnpar,tx,num,gamu,ext_press_mode)
  call vequation(nlev,timestep,cnpar,ty,num,gamv,ext_press_mode)
  call extpressure(ext_press_mode,nlev)
  call intpressure(nlev)
  call friction(kappa,avmolu,tx,ty)

!     update temperature and salinity
  if (s_prof_method .ne. 0) then
    call salinity(nlev,timestep,cnpar,nus,gams)
  endif

  if (t_prof_method .ne. 0) then
    call temperature(nlev,timestep,cnpar,I_0,heat,nuh,gamh,rad)
  endif

!     update shear and stratification
  call shear(nlev,cnpar)
  call stratification(nlev,buoy_method,timestep,cnpar,nuh,gamh)

!    compute turbulent mixing
  select case (turb_method)
  case (0)
!        do convective adjustment
    call convectiveadjustment(nlev,num,nuh,const_num,const_nuh,    &
                                   buoy_method,gravity,rho_0)
  case (99)
!        update KPP model
    call convert_fluxes(nlev,gravity,cp,rho_0,heat,precip+evap,    &
                             rad,T,S,tFlux,sFlux,btFlux,bsFlux,tRad,bRad)

    call do_kpp(nlev,depth,h,rho,u,v,NN,NNT,NNS,SS,                &
                     u_taus,u_taub,tFlux,btFlux,sFlux,bsFlux,           &
                     tRad,bRad,cori)

  case default
!        update one-point models
    call do_turbulence(nlev,timestep,depth,u_taus,u_taub,z0s,z0b,h,      &
                            NN,SS)
  end select

  end subroutine gotm_time_step

end module gotm_component
