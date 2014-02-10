!> @brief Implementation of a GOTM wrapper as ESMF component
!
!> This module serves as a wrapper for the General Ocean Turbulence Model
!> (GOTM). This model describes a 1D water column.
!> @import 
!> @export water_temperature, grid_height, (FABM variables)
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht
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

  use mossco_variable_types
  
  implicit none

  private
 
  type(ESMF_Clock)  :: clock 
  real(ESMF_KIND_R8), allocatable, target :: variables(:,:,:,:)
  type(MOSSCO_VariableFArray3d), dimension(:), allocatable :: export_variables

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
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    rc=ESMF_SUCCESS
    
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
    integer                     :: nimport,nexport
    real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Grid)      :: grid
    type(ESMF_ArraySpec) :: arrayspec
    
    type(ESMF_Field), dimension(:), allocatable  :: exportField, importField
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr  
      
    namelist /model_setup/ title,nlev,dt,cnpar,buoy_method
    namelist /station/ name,latitude,longitude,depth
    namelist /eqstate/ eq_state_mode,eq_state_method,T0,S0,p0,dtr0,dsr0
 
    call ESMF_LogWrite("GOTM ocean component initializing.",ESMF_LOGMSG_INFO)
    call init_gotm()

    ! read model_setup namelist
    open(921,file='gotmrun.nml',status='old',action='read')
    read(921,nml=model_setup)
    read(921,nml=station)
    close(921)


    ! Create a local clock, set its parameters to those of the parent clock, then
    ! copy start and stop time from clock to gotm's time parameters
    clock = ESMF_ClockCreate(parentClock, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_time_timestep,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_ClockSet(clock, name='GOTM clock',timeStep=timeInterval, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
     
    call ESMF_TimeSet(clockTime,rc=rc) 
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,startTime=clockTime,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    gotm_time_start=timestring(1:10)//" "//timestring(12:19)

    call ESMF_ClockGet(parentClock,stopTime=clockTime)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    gotm_time_stop=timestring(1:10)//" "//timestring(12:19)

    gotm_time_timefmt = 2 
    call gotm_time_init_time(gotm_time_min_n,gotm_time_max_n)      

    !! The output timestep is used to create an alarm in the parent Clock
    !> @todo implement this also driven by the parent clock
    call ESMF_ClockGet(clock,startTime=clockTime) 
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_output_nsave*gotm_time_timestep,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    ! Introduced dependency to FABM component, which use the same name for the alarm
    outputAlarm = ESMF_AlarmCreate(clock=parentclock,name="GOTM output Alarm", &
      ringTime=clockTime+timeInterval,ringInterval=timeInterval,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    outputAlarm = ESMF_AlarmCreate(clock=clock,name="GOTM output Alarm", &
      ringTime=clockTime+timeInterval,ringInterval=timeInterval,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

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

    ! Get information to generate the fields that store the pointers to variables
    call ESMF_GridGet(grid,distgrid=distgrid,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_GridGetFieldBounds(grid=grid,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER,&
      totalCount=farray_shape,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !> Create export fields and add them to export state, allocate the space for these
    !> that will be filled later with data, copying of data is necessary to provide 3d fields
    !> for ESMF
    nexport = 6
    allocate(export_variables(nexport))
    export_variables(1)%standard_name="water_temperature"
    export_variables(2)%standard_name="grid_height"
    export_variables(3)%standard_name="salinity"
    export_variables(4)%standard_name="radiation"
    export_variables(5)%standard_name="water_x_velocity"
    export_variables(6)%standard_name="water_y_velocity"
    allocate(exportField(nexport))
    allocate(variables(farray_shape(1),farray_shape(2),farray_shape(3),nexport))
    
    call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    do k=1,nexport
      farrayPtr => variables(:,:,:,k)
      exportField(k) = ESMF_FieldCreate(grid, farrayPtr=farrayPtr, name=export_variables(k)%standard_name, &
        staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_StateAddReplace(exportState,(/exportField(k)/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    enddo
  
    !> Specify water temperature information from T0 field
    !variables(:,:,:,1) =  T(1:nlev)
    !> Specify a grid_height 
    !variables(:,:,:,2) = h(1:nlev)
    !> Specify salinity
    !variables(:,:,:,3) = S(1:nlev)

    call ESMF_LogWrite("GOTM ocean component initialized.",ESMF_LOGMSG_INFO)
    
  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    use meanflow, only : gotm_temperature => T
    use meanflow, only : gotm_salinity => S
    use meanflow, only : gotm_heights => h 
    use meanflow, only : gotm_radiation => rad
    use meanflow, only : gotm_u => u
    use meanflow, only : gotm_v => v
    use gotm_mossco_fabm, only: gotm_fabm_bottom_flux => bfl

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I8)   :: n,k
    integer                 :: itemcount,nvar
    real(ESMF_KIND_R8),pointer,dimension(:,:)  :: ptr_f2
    real(ESMF_KIND_R8),pointer,dimension(:,:,:):: ptr_f3
    type(ESMF_Field)        :: Field
    character(len=ESMF_MAXSTR) :: string,varname,message

    call ESMF_ClockGet(parentClock,currTime=clockTime, timestep=timeInterval, advanceCount=n, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
#ifdef DEBUG
    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(timestring)//" GOTM run called"
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
#endif

    ! From parent clock get current time and time interval, calculate new stop time for local clock as currTime+timeInterval
    call ESMF_ClockSet(clock,stopTime=clockTime + timeInterval, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
    ! @todo implement a solution for short outer timesteps or non-integer number of internal vs outer timesteps
    do while (.not.ESMF_ClockIsStopTime(clock))

       call ESMF_ClockGet(clock,currTime=clockTime, advanceCount=n, rc=rc)
       if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifdef DEBUG
       call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
       if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       write(message,'(A,I5)') trim(timestring)//" GOTM iteration ", n
       call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
#endif

       call update_time(n)
       call gotm_time_step()

       do k=1,nlev
         variables(:,:,k,1) = gotm_temperature(k)
         variables(:,:,k,2) = gotm_heights(k)
         variables(:,:,k,3) = gotm_salinity(k)
         variables(:,:,k,4) = gotm_radiation(k)
         variables(:,:,k,5) = gotm_u(k)
         variables(:,:,k,6) = gotm_v(k)
       end do

       !> Check if the output alarm is ringing, if so, quiet it and 
       !> call do_output from GOTM
       if (ESMF_AlarmIsRinging(outputAlarm)) then
         call ESMF_AlarmRingerOff(outputAlarm,rc=rc)
         if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         call prepare_output(n)
         call do_output(n,nlev)
       endif

       call ESMF_ClockAdvance(clock,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end do

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer                     :: lbnd(3), ubnd(3), k
    real(ESMF_KIND_R8),pointer :: farrayPtr(:,:,:)
    type(ESMF_Field)     :: field

    do k=1,size(export_variables)
      call ESMF_StateGet(exportState,trim(export_variables(k)%standard_name), field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

#if ESMF_VERSION_MAJOR > 5
      call ESMF_StateRemove(exportState,(/ trim(export_variables(k)%standard_name) /),rc=rc)
#else
      call ESMF_StateRemove(exportState,trim(export_variables(k)%standard_name),rc=rc)
#endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_FieldDestroy(field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    enddo


    if (allocated(variables)) deallocate(variables)

    call ESMF_ClockDestroy(clock,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

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
