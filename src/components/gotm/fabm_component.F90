!> @brief Implementation of a FABM (with GOTM) ESMF component
!
!> This module serves as a wrapper for the FABM model in a 1D GOTM context.
!> @import water_temperature
!> @export (FABM variables)
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

module fabm_gotm_component

  use esmf
  use output, only: prepare_output,do_output,gotm_output_nsave => nsave

  use turbulence,  only: nuh
  use airsea,      only: wind=>w,tx,ty,I_0,cloud,heat,precip,evap
  use airsea,      only: bio_albedo,bio_drag_scale
  use meanflow,    only: s,t,rho,z,h,w,bioshade,taub
  use observations,only: SRelaxTau,sProf
  use observations
  use time,        only: secondsofday,yearday,gotm_time_timestep=>timestep

  use gotm_mossco_fabm
  use mossco_variable_types
  use mossco_state
  
  implicit none

  private
 
  type(ESMF_Clock)  :: clock 
  real(ESMF_KIND_R8), allocatable, target :: variables(:,:,:,:)
  type(MOSSCO_VariableFArray3d), dimension(:), allocatable :: export_variables
  type(export_state_type),dimension(:), allocatable        :: fabm_export_states

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
    character(len=ESMF_MAXSTR) :: varname
    type(ESMF_Time)   :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8) :: dt
    integer                     :: lbnd(3), ubnd(3),farray_shape(3)
    integer                     :: myrank,i,j,k
    integer                     :: nimport,nexport,itemcount
    real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Grid)      :: grid
    type(ESMF_ArraySpec) :: arrayspec
    
    type(ESMF_Field), dimension(:), allocatable  :: exportField, importField
    type(ESMF_Field)     :: field
    
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr
    namelist /model_setup/ title,nlev,dt,cnpar,buoy_method
    namelist /station/ name,latitude,longitude,depth
      
    call ESMF_LogWrite("FABM/GOTM component initializing.",ESMF_LOGMSG_INFO)

    ! read model_setup namelist
    open(921,file='gotmrun.nml',status='old',action='read')
    read(921,nml=model_setup)
    read(921,nml=station)
    close(921)

    call init_gotm_mossco_fabm(nlev,'gotm_fabm.nml',dt)
    call init_gotm_mossco_fabm_output()
    call set_env_gotm_fabm(latitude,longitude,dt,w_adv_method,w_adv_discr, &
                          t(1:nlev),s(1:nlev),rho(1:nlev), &
                          nuh,h,w,bioshade(1:nlev),I_0,cloud,taub,wind,precip,evap,z(1:nlev), &
                          A,g1,g2,yearday,secondsofday,SRelaxTau(1:nlev),sProf(1:nlev), &
                          bio_albedo,bio_drag_scale)

    ! Create a local clock, set its parameters to those of the parent clock, then
    ! copy start and stop time from clock to gotm's time parameters
    clock = ESMF_ClockCreate(parentClock, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_time_timestep,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockSet(clock, name='FABM/GOTM clock',timeStep=timeInterval, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
     
    !> Create the grid from existing grid of water_temperature field
    varname="water_temperature"
    call ESMF_StateGet(importState, itemSearch=trim(varname), itemCount=itemcount,rc=rc)
    if (itemcount==0) then
      call ESMF_LogWrite(trim(varname)//' not found. Cannot initialize '// &
                    ' without this variable.',ESMF_LOGMSG_ERROR)
    else
      call ESMF_StateGet(importState,trim(varname),field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
      call ESMF_FieldGet(field,grid=grid, arrayspec=arrayspec,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    ! Get information to generate the fields that store the pointers to variables
    call ESMF_GridGet(grid,distgrid=distgrid,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_GridGetFieldBounds(grid=grid,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER,&
      totalCount=farray_shape,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call get_all_export_states(fabm_export_states)
    allocate(exportField(2*size(fabm_export_states)))

    do k=1,size(fabm_export_states)
      exportField(2*k-1) = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
        name=trim(fabm_export_states(k)%standard_name), &
        staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      exportField(2*k) = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
    
        name=trim(fabm_export_states(k)%standard_name)//'_z_velocity', &
        staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_FieldGet(field=exportField(2*k-1), localDe=0, farrayPtr=farrayPtr, &
                       totalLBound=lbnd,totalUBound=ubnd, rc=rc) 
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      farrayPtr = fabm_export_states(k)%conc

      call ESMF_FieldGet(field=exportField(2*k), localDe=0, farrayPtr=farrayPtr, &
      call ESMF_FieldGet(field=exportField(4+2*k), localDe=0, farrayPtr=farrayPtr, &
                       totalLBound=lbnd,totalUBound=ubnd, rc=rc) 
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      farrayPtr = fabm_export_states(k)%ws

      call ESMF_StateAddReplace(exportState,(/exportField(2*k),exportField(2*k-1)/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    enddo

    call ESMF_LogWrite("FABM/GOTM component initialized.",ESMF_LOGMSG_INFO)
    
  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    use meanflow, only : gotm_temperature => T
    use meanflow, only : gotm_salinity => S
    use meanflow, only : gotm_heights => h 
    use meanflow, only : gotm_radiation => rad
    use gotm_mossco_fabm, only: gotm_fabm_bottom_flux => bfl

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval, timeStep
    integer(ESMF_KIND_I8)   :: n,k
    integer                 :: itemcount,nvar
    real(ESMF_KIND_R8),pointer,dimension(:,:)  :: ptr_f2
    real(ESMF_KIND_R8),pointer,dimension(:,:,:):: ptr_f3
    real(ESMF_KIND_R8)      :: dt
    type(ESMF_Field)        :: Field
    character(len=ESMF_MAXSTR) :: string,varname,message

    call ESMF_ClockGet(parentClock,currTime=clockTime, timestep=timeInterval, advanceCount=n, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    write(message,'(A)') trim(timestring)//" FABM/GOTM run called"
!#ifdef DEBUG
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
!#endif

    ! From parent clock get current time and time interval, calculate new stop time for local clock as currTime+timeInterval
    call ESMF_ClockSet(clock,stopTime=clockTime + timeInterval, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
    ! @todo implement a solution for short outer timesteps or non-integer number of internal vs outer timesteps
     do while (.not.ESMF_ClockIsStopTime(clock))

       call ESMF_ClockGet(clock,currTime=clockTime, advanceCount=n, &
         timestep=timeStep, rc=rc)
       if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring, rc=rc)
       if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       call ESMF_TimeIntervalGet(timeStep,s_r8=dt, rc=rc)
       if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       write(message,'(A,I5)') trim(timestring)//" FABM/GOTM iteration ", n
       call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

       ! get upward fluxes for FABM's state variables and put into bfl arrays of
       ! diffusion routine (so far - later use integration by solver_library)
       do nvar=1,size(gotmfabm%model%info%state_variables)
         varname=trim(gotmfabm%model%info%state_variables(nvar)%long_name)//'_upward_flux'
         call ESMF_StateGet(importState, itemSearch=trim(varname), itemCount=itemcount,rc=rc)
         if (itemcount==0) then
!#ifdef DEBUG
           call ESMF_LogWrite(trim(varname)//' not found',ESMF_LOGMSG_INFO)
!#endif
         else
             call ESMF_StateGet(importState,trim(varname),field,rc=rc)
             if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
             call ESMF_FieldGet(field,farrayPtr=ptr_f2,rc=rc)
             if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
             gotm_fabm_bottom_flux(1,1,nvar) = ptr_f2(1,1)
         end if
       end do

       call do_gotm_mossco_fabm(dt)
 
       
       ! Introduced dependency from FABM component, which use the same name for the alarm
       call ESMF_ClockGetAlarm(parentClock, alarmname="GOTM output Alarm", alarm=outputAlarm, rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

       if (ESMF_AlarmIsRinging(outputAlarm)) then
         call ESMF_AlarmRingerOff(outputAlarm,rc=rc)
         if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         call prepare_output(n)
         call do_gotm_mossco_fabm_output()
        ! Do we need the next line
         call update_export_states(fabm_export_states)
       endif

    ! update Field data:
      do nvar=1,size(fabm_export_states)
        call mossco_state_get(exportState,trim(fabm_export_states(nvar)%standard_name), &
          ptr_f3)
        ptr_f3 = fabm_export_states(nvar)%conc
        
        call mossco_state_get(exportState,trim(fabm_export_states(nvar)%standard_name)//'_z_velocity', &
          ptr_f3)
        ptr_f3 = fabm_export_states(nvar)%ws
      end do
 
      call ESMF_ClockAdvance(clock,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end do ! end of time loop

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer                     :: lbnd(3), ubnd(3), k
    real(ESMF_KIND_R8),pointer :: farrayPtr(:,:,:)
    type(ESMF_Field)     :: field

    do k=1,size(fabm_export_states)
      call ESMF_StateGet(exportState,trim(fabm_export_states(k)%standard_name), field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

#if ESMF_VERSION_MAJOR > 5
      call ESMF_StateRemove(exportState,(/ trim(fabm_export_states(k)%standard_name) /),rc=rc)
#else
      call ESMF_StateRemove(exportState,trim(fabm_export_states(k)%standard_name),rc=rc)
#endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_FieldDestroy(field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_StateGet(exportState,trim(fabm_export_states(k)%standard_name)//'_z_velocity', field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

#if ESMF_VERSION_MAJOR > 5
      call ESMF_StateRemove(exportState,(/ trim(fabm_export_states(k)%standard_name)//'_z_velocity' /),rc=rc)
#else
      call ESMF_StateRemove(exportState,trim(fabm_export_states(k)%standard_name)//'_z_velocity',rc=rc)
#endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_FieldDestroy(field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    enddo

    call ESMF_ClockDestroy(clock,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Finalize


end module fabm_gotm_component
