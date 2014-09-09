!> @brief Implementation of a FABM (with GOTM) ESMF component
!
!> This module serves as a wrapper for the FABM model in a 1D GOTM context.
!> @import temperature_in_water
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
  use mossco_strings
  
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
    
    type(ESMF_Field), dimension(:), allocatable  :: importField
    type(ESMF_Field)          :: field,concfield,wsfield
    type(ESMF_FieldBundle)    :: fieldBundle
    type(ESMF_StateItem_Flag) :: itemType
    
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr,wsPtr
    real(ESMF_KIND_R8)   :: attribute_r8
    character(len=ESMF_MAXSTR) :: attribute_name
    namelist /model_setup/ title,nlev,dt,cnpar,buoy_method
    namelist /station/ name,latitude,longitude,depth
    
    logical                     :: clockIsPresent, fileIsPresent
    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR)  :: message
    character(len=ESMF_MAXSTR)  :: configFileName
      
    !! Check whether there is already a clock (it might have been set 
    !! with a prior ESMF_gridCompCreate() call.  If not, then create 
    !! a local clock as a clone of the parent clock, and associate it
    !! with this component.  Finally, set the name of the local clock
    call ESMF_GridCompGet(gridComp, name=name, clockIsPresent=clockIsPresent, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (clockIsPresent) then
      call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)     
    else
      clock = ESMF_ClockCreate(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompSet(gridComp, clock=clock, rc=rc)    
    endif
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock, name=trim(name)//' clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    !! Log the call to this function
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    !> Read the GOTM configuration file
    configFileName = 'gotmrun.nml'
    inquire(file=trim(configFileName), exist=fileIsPresent)
    if (.not.fileIsPresent) then
      write(message,'(A)') trim(name)//' could not read config file '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    ! read model_setup namelist
    open(921,file=trim(configFileName),status='old',action='read')
    read(921,nml=model_setup)
    read(921,nml=station)
    close(921)

    ! Update the time interval of the local clock from the gotmrun namelist timestep
    call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_time_timestep,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockSet(clock, timeStep=timeInterval, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    ! Read the GOTMFABM configuration file
    configFileName = 'gotm_fabm.nml'
    inquire(file=trim(configFileName), exist=fileIsPresent)
    if (.not.fileIsPresent) then
      write(message,'(A)') trim(name)//' could not read config file '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    call init_gotm_mossco_fabm(nlev,trim(configFileName),dt)
    call init_gotm_mossco_fabm_output()
    call set_env_gotm_fabm(latitude,longitude,dt,w_adv_method,w_adv_discr, &
                          t(1:nlev),s(1:nlev),rho(1:nlev), &
                          nuh,h,w,bioshade(1:nlev),I_0,cloud,taub,wind,precip,evap,z(1:nlev), &
                          A,g1,g2,yearday,secondsofday,SRelaxTau(1:nlev),sProf(1:nlev), &
                          bio_albedo,bio_drag_scale)


    !> set required, required_rank and optional flags in the importState
    call set_import_flags(importState)
     
    !> Create the grid from existing grid of temperature_in_water field
    !! or any other field specified in foreign_grid_field_name. In general, the importState
    !! should be used. In the present configuration of GOTM/FABM components, the exportState
    !! unusually serves the purpose.
    call ESMF_AttributeGet(exportState, name='foreign_grid_field_name', &
           value=varname, defaultValue='temperature_in_water', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_StateGet(exportState, itemSearch=trim(varname), itemCount=itemcount,rc=rc)
    if (itemcount==0) then
      call ESMF_LogWrite(trim(varname)//' not found. Cannot initialize '// &
                    ' without this variable.',ESMF_LOGMSG_ERROR)
      call ESMF_StatePrint(importState)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    else
      call ESMF_StateGet(exportState,trim(varname),field,rc=rc)
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

    do k=1,size(fabm_export_states)
      !> create field for state variable
      concfield = ESMF_FieldCreate(grid, farrayPtr=fabm_export_states(k)%conc, &
        name=trim(fabm_export_states(k)%standard_name), &
        staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
      !> add attributes relevant for MOSSCO
      !! mean_particle_diameter and particle density given only,
      !! if property persent
      attribute_name=trim('mean_particle_diameter')
      attribute_r8 = gotmfabm%model%info%state_variables(k)%properties%get_real('diameter',default=-99.d0)
      if (attribute_r8 > 0.0d0) &
        call ESMF_AttributeSet(concfield,attribute_name, attribute_r8)
      attribute_name=trim('particle_density')
      attribute_r8 = gotmfabm%model%info%state_variables(k)%properties%get_real('density',default=-99.d0)
      if (attribute_r8 > 0.0d0) &
        call ESMF_AttributeSet(concfield,attribute_name, attribute_r8)
      !! always set units
      call ESMF_AttributeSet(concfield,'units', trim(fabm_export_states(k)%units))
      !> add fabm index in concentration array as "external_index" to be used by other components
      call ESMF_AttributeSet(concfield,'external_index',fabm_export_states(k)%fabm_id)

      !> create field for sinking velocity of state variable
      wsPtr => fabm_export_states(k)%ws
      wsfield = ESMF_FieldCreate(grid, farrayPtr=fabm_export_states(k)%ws, &
        name=trim(fabm_export_states(k)%standard_name)//'_z_velocity', &
        staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      call ESMF_AttributeSet(wsfield,'external_index',fabm_export_states(k)%fabm_id)
      !! always set units to m/s (fabm convention)
      call ESMF_AttributeSet(wsfield,'units', 'm/s')
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_StateGet(exportState, &
              trim(fabm_export_states(k)%standard_name),itemType, rc=rc)

      if (itemType == ESMF_STATEITEM_NOTFOUND) then
        call ESMF_StateAddReplace(exportState,(/concfield,wsfield/),rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      else if (itemType ==ESMF_STATEITEM_FIELD) then
      !> if field present, remove from state, create bundle, add fields
        call ESMF_StateGet(exportState, &
                trim(fabm_export_states(k)%standard_name),field,rc=rc)
#if ESMF_VERSION_MAJOR > 5
        call ESMF_StateRemove(exportState, &
                (/ trim(fabm_export_states(k)%standard_name) /),rc=rc)
#else
        call ESMF_StateRemove(exportState, &
                trim(fabm_export_states(k)%standard_name),rc=rc)
#endif
        fieldBundle = ESMF_FieldBundleCreate(fieldlist=(/field,concfield/), &
                name=trim(fabm_export_states(k)%standard_name),   &
                multiflag=.true.,rc=rc)
        call ESMF_StateAddReplace(exportState,(/fieldBundle/),rc=rc)

        call ESMF_StateGet(exportState, &
                trim(fabm_export_states(k)%standard_name)//'_z_velocity',field,rc=rc)
#if ESMF_VERSION_MAJOR > 5
        call ESMF_StateRemove(exportState, &
                (/ trim(fabm_export_states(k)%standard_name)//'_z_velocity' /),rc=rc)
#else
        call ESMF_StateRemove(exportState, &
                trim(fabm_export_states(k)%standard_name)//'_z_velocity',rc=rc)
#endif
        fieldBundle = ESMF_FieldBundleCreate(fieldlist=(/field,wsfield/), &
                name=trim(fabm_export_states(k)%standard_name)//'_z_velocity',   &
                multiflag=.true.,rc=rc)
        call ESMF_StateAddReplace(exportState,(/fieldBundle/),rc=rc)

      else if(itemType == ESMF_STATEITEM_FIELDBUNDLE) then
      !> if fieldBundle, get the bundle and add field
        call ESMF_StateGet(exportState,trim(fabm_export_states(k)%standard_name),fieldBundle,rc=rc)
        call ESMF_FieldBundleAdd(fieldBundle,(/concfield/),multiflag=.true.,rc=rc)
        call ESMF_StateGet(exportState,trim(fabm_export_states(k)%standard_name)//'_z_velocity',fieldBundle,rc=rc)
        call ESMF_FieldBundleAdd(fieldBundle,(/wsfield/),multiflag=.true.,rc=rc)
      end if

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
    type(ESMF_Time)         :: wallTime, clockTime, stopTime, currTime
    type(ESMF_TimeInterval) :: timeInterval, timeStep
    integer(ESMF_KIND_I8)   :: n,k
    integer                 :: nvar,ii,fabm_idx
    real(ESMF_KIND_R8),pointer,dimension(:,:)  :: ptr_f2
    real(ESMF_KIND_R8),pointer,dimension(:,:,:):: ptr_f3
    real(ESMF_KIND_R8)      :: dt
    type(ESMF_Field)        :: Field
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_Field),dimension(:),allocatable ::fieldlist
    type(ESMF_StateItem_Flag)  :: itemType
    character(len=ESMF_MAXSTR) :: string,varname,message
    
    integer(ESMF_KIND_I4)    :: localPet, petCount, hours, seconds, minutes
    logical                  :: clockIsPresent

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (.not.clockIsPresent) then
      call ESMF_LogWrite('Required clock not found in '//trim(name), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,currTime=currTime,  timeStep=timeInterval, &
      stopTime=stopTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running with dt='
    call ESMF_TimeIntervalGet(timeInterval,s_r8=dt)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,F6.1,A)') trim(message),dt,' s to '
    call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(message)//' '//trim(timeString)//' ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    ! set environment from GOTM arrays
    call set_env_gotm_fabm(latitude,longitude,dt,w_adv_method,w_adv_discr, &
                          t(1:nlev),s(1:nlev),rho(1:nlev), &
                          nuh,h,w,bioshade(1:nlev),I_0,cloud,taub,wind,precip,evap,z(1:nlev), &
                          A,g1,g2,yearday,secondsofday,SRelaxTau(1:nlev),sProf(1:nlev), &
                          bio_albedo,bio_drag_scale)

       ! get upward fluxes for FABM's state variables and put into bfl arrays of
       ! diffusion routine (so far - later use integration by solver_library)
       do nvar=1,size(gotmfabm%model%info%state_variables)
         varname=trim(only_var_name( &
           gotmfabm%model%info%state_variables(nvar)%long_name))//'_upward_flux'
         !> if fieldBundle, then start counter on name and map nvar accordinlgy
         !> later use attributes to distribute fields
         call ESMF_StateGet(importState, trim(varname), itemType,rc=rc)
         if (itemType == ESMF_STATEITEM_NOTFOUND) then
#ifdef DEBUG
           call ESMF_LogWrite(trim(varname)//' not found',ESMF_LOGMSG_INFO)
#endif
         else if (itemType == ESMF_STATEITEM_FIELD) then
#ifdef DEBUG
           call ESMF_LogWrite(trim(varname)//' field found',ESMF_LOGMSG_INFO)
#endif
             call ESMF_StateGet(importState,trim(varname),field,rc=rc)
             if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
             call ESMF_FieldGet(field,farrayPtr=ptr_f2,rc=rc)
             if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
             gotm_fabm_bottom_flux(1,1,nvar) = ptr_f2(1,1)
         else if (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
#ifdef DEBUG
           call ESMF_LogWrite(trim(varname)//' fieldbundle found',ESMF_LOGMSG_INFO)
#endif
           call ESMF_StateGet(importState, trim(varname), fieldBundle,rc=rc)
           !> allocate fieldlist by fieldCount
           if (.not.allocated(fieldlist)) then
             call ESMF_FieldBundleGet(fieldBundle,fieldCount=ii,rc=rc)
             allocate(fieldlist(ii))
           end if
           call ESMF_FieldBundleGet(fieldBundle,fieldlist=fieldlist,rc=rc)
           do ii=1,size(fieldlist)
             call ESMF_AttributeGet(fieldlist(ii),'external_index',fabm_idx)
#ifdef DEBUG
             write(0,*) trim(varname),': field index:',ii,'fabm index:',fabm_idx
#endif
             if (fabm_idx == nvar) then
               call ESMF_FieldGet(fieldlist(ii),farrayPtr=ptr_f2,rc=rc)
#ifdef DEBUG
               write(0,*) 'use external_index',fabm_idx,'for variable',nvar
#endif
               gotm_fabm_bottom_flux(1,1,nvar) = ptr_f2(1,1)
#ifdef DEBUG
               write(0,*) 'flux:',ptr_f2(1,1)
#endif
               exit
             else
               cycle
             end if
           end do
         end if
       end do

    ! @todo implement a solution for short outer timesteps or non-integer number of internal vs outer timesteps
     do while (.not.ESMF_ClockIsStopTime(clock))

       call ESMF_ClockGet(clock,currTime=clockTime, advanceCount=n, rc=rc)
       if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifdef DEBUG
       call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring, rc=rc)
       if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       write(message,'(A,I5)') trim(timestring)//" FABM/GOTM iteration ", n
       call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
#endif


       call do_gotm_mossco_fabm(dt)

       ! Introduced dependency from FABM component, which use the same name for the alarm
       call ESMF_ClockGetAlarm(parentClock, alarmname="GOTM output Alarm", alarm=outputAlarm, rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

       if (ESMF_AlarmIsRinging(outputAlarm)) then
         call ESMF_AlarmRingerOff(outputAlarm,rc=rc)
         if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         call prepare_output(n)
         call do_gotm_mossco_fabm_output()
       endif

      call ESMF_ClockAdvance(clock,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end do ! end of time loop

    ! update Field data:
    call update_export_states(fabm_export_states)
!> this is not needed any more, since the fields are created with pointers to
!> fabm_export_states(:)%conc and fabm_export_states(:)%ws
#if 0
    do nvar=1,size(fabm_export_states)
      call mossco_state_get(exportState,(/trim(fabm_export_states(nvar)%standard_name)/), &
        ptr_f3,rc=rc)
      ptr_f3 = fabm_export_states(nvar)%conc

      call mossco_state_get(exportState,(/trim(fabm_export_states(nvar)%standard_name)//'_z_velocity'/), &
        ptr_f3,rc=rc)
      ptr_f3 = fabm_export_states(nvar)%ws
    end do
#endif

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer                    :: lbnd(3), ubnd(3), k
    real(ESMF_KIND_R8),pointer :: farrayPtr(:,:,:)
    type(ESMF_Field)           :: field
    type(ESMF_FieldBundle)     :: fieldBundle
    type(ESMF_StateItem_Flag)  :: itemType

    do k=1,size(fabm_export_states)
      call ESMF_StateGet(exportState, &
              trim(fabm_export_states(k)%standard_name),itemType, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      if (itemType == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(exportState,trim(fabm_export_states(k)%standard_name), field, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldDestroy(field, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_StateGet(exportState,trim(fabm_export_states(k)%standard_name)//'_z_velocity', field, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldDestroy(field, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      else if (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(exportState,trim(fabm_export_states(k)%standard_name), &
                fieldBundle, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldBundleDestroy(fieldbundle,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        call ESMF_StateGet(exportState, &
                trim(fabm_export_states(k)%standard_name)//'_z_velocity', &
                fieldbundle, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldBundleDestroy(fieldbundle,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      end if

#if ESMF_VERSION_MAJOR > 5
      call ESMF_StateRemove(exportState, &
              (/ trim(fabm_export_states(k)%standard_name) /),relaxedFlag=.true.,rc=rc)
      call ESMF_StateRemove(exportState, &
              (/ trim(fabm_export_states(k)%standard_name)//'_z_velocity' /), &
              relaxedFlag=.true.,rc=rc)
#else
      call ESMF_StateRemove(exportState, &
              trim(fabm_export_states(k)%standard_name),relaxedFlag=.true.,rc=rc)
      call ESMF_StateRemove(exportState, &
              trim(fabm_export_states(k)%standard_name)//'_z_velocity', &
              relaxedFlag=.true.,rc=rc)
#endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    enddo

    !! @todo The clockIsPresent statement does not detect if a clock has been destroyed 
    !! previously, thus, we comment the clock destruction code while this has not
    !! been fixed by ESMF
    !if (clockIsPresent) call ESMF_ClockDestroy(clock, rc=rc)
    !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine Finalize

  !> set boundary flags, mainly for optional forcing fields
  subroutine set_import_flags(importState)
    type(ESMF_State)           :: importState
    character(len=ESMF_MAXSTR) :: name,varname
    integer                    :: n

    varname="temperature_in_water"
    call set_item_flags(importState,varname,requiredFlag=.true.,requiredRank=3)

    do n=1,size(gotmfabm%model%info%state_variables)
      varname=trim(only_var_name( &
           gotmfabm%model%info%state_variables(n)%long_name))//'_upward_flux'
      call set_item_flags(importState,varname,optionalFlag=.true.,requiredRank=2)
    end do
  end subroutine set_import_flags

end module fabm_gotm_component
