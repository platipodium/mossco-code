!> @brief Implementation of a FABM0D component
!
!> The ESMF/FABM 0d driver component module provides infrastructure for the
!> MOSSCO 0d pelagic component.
!> The ESMF component contains the 0d driver module
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, 2014, Helmholtz-Zentrum Geesthacht 
!> @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module fabm0d_component

  use esmf
  use time, only: gotm_time_min_n => MinN, gotm_time_max_n => MaxN
  use time, only: gotm_time_timestep => timestep
  use time, only: gotm_time_start => start, gotm_time_stop => stop
  use time, only: gotm_time_timefmt => timefmt
  use time, only: gotm_time_init_time => init_time
  use mossco_fabm0d, only: init_0d => init_run
  use mossco_fabm0d, only: time_loop_0d => time_loop
  use mossco_fabm0d, only: finalize_0d => clean_up
  use mossco_fabm0d, only: zerod, export_state_type
  use mossco_fabm0d, only: get_export_state_from_variable_name, update_export_states

  implicit none

  private

  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: water_temperature,salinity,radiation
  type(ESMF_Field)            :: import_field
  type(ESMF_Field)            :: din_field
  type(ESMF_Field)            :: pon_field
  type(ESMF_Field)            :: pon_ws_field
  integer                     :: ubnd(3),lbnd(3)
  type(export_state_type)     :: din,pon
  logical                     :: forcing_from_coupler=.false.

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
    use time, only: start,stop
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Array)     :: ponarray,wsarray,dinarray
    type(ESMF_Clock)     :: clock

    type(ESMF_Time)   :: wallTime, currTime, startTime, stopTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8) :: dt
    integer            :: ode_method,namlst=234
    character(len=80)  :: title
    logical            :: input_from_namelist = .true.
    character(len=256) :: din_variable='gotm_npzd_nut',pon_variable='gotm_npzd_det'
    character(len=ESMF_MAXSTR) :: message, timestring, name

    namelist /model_setup/ title,start,stop,dt,ode_method, &
                           din_variable, pon_variable
    namelist /mossco_fabm0d/ forcing_from_coupler,din_variable,pon_variable
  
    call ESMF_GridCompGet(gridComp, name=name, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
    ! Create a local clock, set its parameters to those of the parent clock
    clock = ESMF_ClockCreate(parentClock, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_ClockSet(clock, name=trim(name)//' clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_GridCompSet(gridComp, clock=clock, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    ! initialize from namelists (called by driver with *wrong* time info)
    call init_0d(forcing_from_coupler=forcing_from_coupler)

    ! read 0d namelist also from here
    open(namlst,file='run.nml',status='old',action='read')
    read(namlst,nml=model_setup)
    read(namlst,nml=mossco_fabm0d)
    close(namlst)

    ! set the timestep from the namelist, but overwrite the namelist's 
    ! start and stop times with the ones from the local clock
    gotm_time_timestep=dt
    call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_time_timestep,rc=rc)
    call ESMF_ClockSet(clock,timeStep=timeInterval,rc=rc)
    
    write(message,'(A,F8.0,A)') trim(name)//' internal timestep is ',gotm_time_timestep,' seconds'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)    
    
    call ESMF_ClockGet(clock,startTime=startTime, stopTime=stopTime)
    call ESMF_TimeGet(startTime,timeStringISOFrac=timestring)
    gotm_time_start=timestring(1:10)//" "//timestring(12:19)
     
    call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring)
    gotm_time_stop=timestring(1:10)//" "//timestring(12:19)


    !> get export_states information
    din = get_export_state_from_variable_name(din_variable)
    pon = get_export_state_from_variable_name(pon_variable)

    !> create grid
    distgrid =  ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/1,1,1/), &
                                    indexflag=ESMF_INDEX_GLOBAL, rc=rc)

    !> create export fields
    dinarray = ESMF_ArrayCreate(distgrid=distgrid,farray=din%conc, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   name="dissolved_inorganic_nitrogen_in_water", rc=rc)
    ponarray = ESMF_ArrayCreate(distgrid,farray=pon%conc, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   name="particulare_organic_nitrogen_in_water", rc=rc)
    wsarray = ESMF_ArrayCreate(distgrid,farray=pon%ws, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   name="pon_sinking_velocity_in_water", rc=rc)
    !> set export state
    call ESMF_StateAddReplace(exportState,(/dinarray,ponarray,wsarray/),rc=rc)
 
    call ESMF_ClockGet(clock,currTime=currTime)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' initialized.'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO, rc=rc);
   
    rc = ESMF_SUCCESS
    
  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: timestring, name, message
    integer(ESMF_KIND_I4)      :: localPet, petCount, itemCount
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime, startTime, stopTime
    integer(ESMF_KIND_I8)      :: seconds, advanceCount, dt
    type(ESMF_TimeInterval)    :: timeStep
    
    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
 
    call ESMF_ClockGet(clock,startTime=startTime, currTime=currTime, &
      stopTime=stopTime, advanceCount=advanceCount, timeStep=timeStep, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running'
     
    call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeIntervalGet(stopTime-currTime, s_i8=seconds, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeIntervalGet(timeStep, s_i8=dt, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    gotm_time_min_n=advanceCount
    gotm_time_max_n=advanceCount + int(seconds/gotm_time_timestep, ESMF_KIND_I8)

    write(message,'(A,I4,A,I4,A)') trim(message)//' ', gotm_time_max_n - gotm_time_min_n, &
      ' steps of ',dt,' s to '//trim(timestring)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    
    write(*,*) gotm_time_start, gotm_time_stop, gotm_time_min_n, gotm_time_max_n

    ! get import state
    if (forcing_from_coupler) then
      call ESMF_StateGet(importState, itemSearch='water_temperature', &
        itemCount=itemCount, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if (itemCount==1) then
        call ESMF_StateGet(importState, 'water_temperature', import_field, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldGet(import_field, farrayPtr=water_temperature, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        zerod%temp = water_temperature(1,1,1)
      else
        write(message,'(A)') 'Required field water_temperature not found'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      endif

      call ESMF_StateGet(importState, itemSearch='salinity', &
        itemCount=itemCount, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if (itemCount==1) then 
        call ESMF_StateGet(importState, 'salinity', import_field, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldGet(import_field, farrayPtr=salinity, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        zerod%salt = salinity(1,1,1)
      else
        call ESMF_LogWrite('Salinity field not found, '// &
              'set to default value of 30.0',ESMF_LOGMSG_WARNING)
        zerod%salt = 30.
      endif
      
      
      call ESMF_StateGet(importState, itemSearch='photosynthetically_available_radiation', &
        itemCount=itemCount, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if (itemCount==1) then 
        call ESMF_StateGet(importState, 'salinity', import_field, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldGet(import_field, farrayPtr=salinity, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          zerod%par  = radiation(1,1,1)
      else
        call ESMF_LogWrite('PAR field not found, '// &
              'set to default value of 100.0',ESMF_LOGMSG_WARNING)
        zerod%par = 100.
      endif
            
#ifdef DEBUG
    write (message,'(A,F6.3,A)') "Obtained water-temp = ",zerod%temp," from import state"
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
#endif
    end if

    call time_loop_0d()

    ! set export states
    call update_export_states( (/din,pon/) ) 

    do while (.not.ESMF_ClockIsStopTime(clock))
      call ESMF_ClockAdvance(clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
    
    call ESMF_GridCompGet(gridComp, name=name, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(timestring)//' '//trim(name)//' finished running'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    call finalize_0d()

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


end module fabm0d_component


