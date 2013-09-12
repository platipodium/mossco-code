!> @brief Implementation of a GOTM wrapper as ESMF component
!
!> This module serves as a wrapper for the General Ocean Turbulence Model
!> (GOTM). This model describes a 1D water column.
!> @import 
!> @export water_temperature, grid_height
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

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

  implicit none

  private
  
  real(ESMF_KIND_R8), allocatable :: grid_height(:,:,:)
  real(ESMF_KIND_R8),dimension(:,:,:),pointer :: grid_height_ptr
  real(ESMF_KIND_R8),dimension(:,:,:),pointer :: salinity_ptr
  real(ESMF_KIND_R8),dimension(:,:,:),pointer :: radiation_ptr
  type(ESMF_Field)            :: grid_height_Field
  type(ESMF_Array)            :: grid_height_Array
  real(ESMF_KIND_R8), allocatable :: water_temperature(:,:,:)
  real(ESMF_KIND_R8), pointer :: water_temperature_ptr(:,:,:)
  type(ESMF_Field)            :: water_temperature_Field
  type(ESMF_Array)            :: water_temperature_Array

   !> Declare an alarm to ring when output to file is requested
  type(ESMF_Alarm),save :: outputAlarm

#define GOTM_REALTYPE real(kind=selected_real_kind(13))
#define _ZERO_ 0.0d0
#define _ONE_  1.0d0

  !> local variables for the setup control
  character(len=80)         :: title,name
  integer                   :: nlev
  GOTM_REALTYPE             :: cnpar,latitude,longitude,depth
  GOTM_REALTYPE             :: T0,S0,p0,dtr0,dsr0
  integer                   :: buoy_method,eq_state_mode,eq_state_method
    
  public :: SetServices
  
  contains

  !> Provide an ESMF compliant SetServices routine, which defines
  !! the entry points for Init/Run/Finalize
  subroutine SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  !> Initialize the component
  !!
  !! Allocate memory for boundaries and fluxes, create ESMF fields
  !! and export them
  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19) :: timestring
    type(ESMF_Time)   :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8) :: dt
    integer                     :: lbnd(3), ubnd(3),farray_shape(3)
    integer                     :: myrank,i,j,k
    real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Grid)      :: grid
    type(ESMF_ArraySpec) :: arrayspec
      
    namelist /model_setup/ title,nlev,dt,cnpar,buoy_method
    namelist /station/ name,latitude,longitude,depth
    namelist /eqstate/ eq_state_mode,eq_state_method,T0,S0,p0,dtr0,dsr0
 
    logical :: input_from_namelist = .false.  !> @todo later to be replaced by switch passed from parent component

    call ESMF_LogWrite("GOTM ocean component initializing.",ESMF_LOGMSG_INFO)
    call init_gotm()

    ! read model_setup namelist
    open(921,file='gotmrun.nml',status='old',action='read')
    read(921,nml=model_setup)
    read(921,nml=station)
    close(921)

    ! Manipulate the time parameters from the gotm namelist
    ! dt    ! float time steop for integration in seconds
    ! start  ! string date in yyyy-mm-dd hh:mm:ss format for start date
    ! stop   ! string date in yyyy-mm-dd hh:mm:ss format for end date
  
    call ESMF_TimeSet(clockTime)
    if (input_from_namelist) then !> get parent clock and overwrite namelist parameters
      call ESMF_LogWrite('Get GOTM input from namelist',ESMF_LOGMSG_INFO)
      call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_time_timestep,rc=rc)
      call ESMF_ClockSet(parentClock,timeStep=timeInterval,rc=rc)

      timestring=gotm_time_start(1:10)//"T"//gotm_time_start(12:19)
      !call ESMF_TimeSet(clockTime,timeStringISOFrac=timestring)
      call timestring2ESMF_Time(timestring,clockTime)
      call ESMF_ClockSet(parentClock,startTime=clockTime)
      
      timestring=gotm_time_stop(1:10)//"T"//gotm_time_stop(12:19)
      !call ESMF_TimeSet(clockTime,timeStringISOFrac=timestring)
      call timestring2ESMF_Time(timestring,clockTime)
      call ESMF_ClockSet(parentClock,stopTime=clockTime)
    else !> overwrite the parent clock's settings with the namelist parameters
      call ESMF_LogWrite('Set GOTM input from ESMF parent',ESMF_LOGMSG_INFO)
      call ESMF_ClockGet(parentClock,startTime=clockTime)
      call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
      gotm_time_start=timestring(1:10)//" "//timestring(12:19)

      call ESMF_ClockGet(parentClock,timeStep=timeInterval,rc=rc)
      call ESMF_TimeIntervalGet(timeInterval,s_r8=gotm_time_timestep,rc=rc)
     
      call ESMF_ClockGet(parentClock,stopTime=clockTime)
      call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
      gotm_time_stop=timestring(1:10)//" "//timestring(12:19)

      gotm_time_timefmt = 2 
      call gotm_time_init_time(gotm_time_min_n,gotm_time_max_n)      
    endif
  
    !! The output timestep is used to create an alarm
    !> @todo implement this also driven by the parent clock
    call ESMF_ClockGet(parentClock,startTime=clockTime) 
    call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_output_nsave*gotm_time_timestep,rc=rc)
    outputAlarm = ESMF_AlarmCreate(clock=parentClock,ringTime=clockTime+timeInterval,ringInterval=timeInterval,rc=rc)

    !> Create the grid and coordinates
    !> This example grid is a 1 x 1 x nlev grid 
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/1,1,nlev/), &
      regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="ocean grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    do i=lbnd(1),ubnd(1) 
      coordX(i) = longitude
    enddo
    call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    do i=lbnd(1),ubnd(1) 
      coordY(i) = latitude
    enddo  


    !> Create a water temperature field with a 3D array specification, fill the temperature
    !> field with T0, add the field to the ocean's export state
    call ESMF_GridGet(grid,distgrid=distgrid,rc=rc)
    call ESMF_GridGetFieldBounds(grid=grid,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER,&
      totalCount=farray_shape,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    allocate(water_temperature(1,1,nlev))
    water_temperature_Array = ESMF_ArrayCreate(distgrid=distgrid,farray=water_temperature, &
      indexflag=ESMF_INDEX_GLOBAL, name="water_temperature", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    water_temperature_Field = ESMF_FieldCreate(grid=grid, array=water_temperature_Array,&
       name="water_temperature", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_FieldGet(water_temperature_Field, farrayPtr=water_temperature_ptr,totalLBound=lbnd,&
      totalUBound=ubnd,localDE=0,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    water_temperature_ptr = T0

    call ESMF_StateAddReplace(exportState,(/water_temperature_Field/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

 
    !> Create a grid_height field with a 3D array specification, fill the 
    !> field with equidistant values, and add to ocean export state
    
    allocate(grid_height(1,1,nlev))
    grid_height_Array = ESMF_ArrayCreate(distgrid=distgrid,farray=grid_height, &
      indexflag=ESMF_INDEX_GLOBAL, name="grid_height", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    grid_height_Field = ESMF_FieldCreate(grid=grid, array=grid_height_Array,&
       name="grid_height", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_FieldGet(grid_height_Field, farrayPtr=grid_height_ptr,totalLBound=lbnd,&
      totalUBound=ubnd,localDE=0,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_StateAddReplace(exportState,(/grid_height_Field/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do k=lbnd(3),ubnd(3)
      do j=lbnd(2),ubnd(2)
        do i=lbnd(1),ubnd(1) 
          grid_height_ptr(i,j,k) = depth/nlev
        enddo
      enddo
    enddo

    call ESMF_LogWrite("GOTM ocean component initialized.",ESMF_LOGMSG_INFO)
    
  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    use meanflow, only : gotm_temperature => T
    use meanflow, only : gotm_salinity => S
    use meanflow, only : gotm_heights => h 
    use meanflow, only : gotm_radiation => rad

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I8)   :: n,k

    ! get local clock with GOTM timesteop, get global clock with coupling timestep, set n to global/local, call GOTM, advance local clock n steps., 
    call ESMF_TimeSet(clockTime)
    call ESMF_ClockGet(parentClock,currTime=clockTime,AdvanceCount=n)
    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
#ifdef DEBUG
    call ESMF_LogWrite("GOTM run at "//timestring//")", ESMF_LOGMSG_INFO)
#endif

    call update_time(n)
    call gotm_time_step()
    
    do k=1,nlev
      grid_height_ptr(:,:,k)=gotm_heights(k)
      water_temperature_ptr(:,:,k) = gotm_temperature(k)
!      salinity_ptr(:,:,k) = gotm_salinity(k)
!      radiation_ptr(:,:,k) = gotm_radiation(k)
    end do
    

    !> Check if the output alarm is ringing, if so, quiet it and 
    !> call do_output from GOTM
    if (ESMF_AlarmIsRinging(outputAlarm)) then
      call ESMF_AlarmRingerOff(outputAlarm,rc=rc)
      call prepare_output(n)
      call do_output(n,nlev)
    endif
    

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    call clean_up()

  end subroutine Finalize

  !> Actually, this should be an extension of ESMF_TimeSet 
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

  subroutine gotm_time_step()

  use time, only: julianday,secondsofday,timestep,timestepkind
  use meanflow
  use input
  use observations
  use airsea
  use gotm_fabm,only:set_env_gotm_fabm,do_gotm_fabm
  use gotm_fabm_input,only:init_gotm_fabm_input
  use gotm_fabm_output,only:do_gotm_fabm_output
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

!     FABM
  call do_gotm_fabm(nlev)

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
