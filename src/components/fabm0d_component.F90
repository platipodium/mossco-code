!> @brief Implementation of a FABM0D component
!
!> The ESMF/FABM 0d driver component module provides infrastructure for the
!> MOSSCO 0d pelagic component.
!> The ESMF component contains the 0d driver module
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "fabm0d_component.F90"

module fabm0d_component

  use esmf
  use mossco_component
  use mossco_field

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

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer(ESMF_KIND_I4) :: localrc

    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

  !> Initialize the component
  !!
  !! Allocate memory for boundaries and fluxes, create ESMF fields
  !! and export them
#undef  ESMF_METHOD
#define ESMF_METHOD "Initialize"
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
    integer(ESMF_KIND_I4) :: localrc

    namelist /model_setup/ title,start,stop,dt,ode_method, &
                           din_variable, pon_variable
    namelist /mossco_fabm0d/ forcing_from_coupler,din_variable,pon_variable

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_time_timestep,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockSet(clock,timeStep=timeInterval,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(A,F8.0,A)') trim(name)//' internal timestep is ',gotm_time_timestep,' seconds'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_ClockGet(clock,startTime=startTime, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(startTime,timeStringISOFrac=timestring, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    gotm_time_start=timestring(1:10)//" "//timestring(12:19)

    call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring, rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    gotm_time_stop=timestring(1:10)//" "//timestring(12:19)


    !> get export_states information
    din = get_export_state_from_variable_name(din_variable)
    pon = get_export_state_from_variable_name(pon_variable)

    !> create grid
    distgrid =  ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/1,1,1/), &
                                    indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(distgrid,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> create export fields
    dinarray = ESMF_ArrayCreate(distgrid=distgrid,farray=din%conc, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   name="dissolved_inorganic_nitrogen_in_water", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(dinarray,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ponarray = ESMF_ArrayCreate(distgrid,farray=pon%conc, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   name="particulare_organic_nitrogen_in_water", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(ponarray,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    wsarray = ESMF_ArrayCreate(distgrid,farray=pon%ws, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   name="pon_sinking_velocity_in_water", rc=localrc)
    call ESMF_AttributeSet(wsarray,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> set export state
    call ESMF_StateAddReplace(exportState,(/dinarray,ponarray,wsarray/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)

  end subroutine Initialize

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


#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
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
    integer(ESMF_KIND_I4)      :: localrc

    rc = ESMF_SUCCESS
    call MOSSCO_CompEntry (gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clock=clock, rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    call ESMF_ClockGet(clock,startTime=startTime, currTime=currTime, &
      stopTime=stopTime, advanceCount=advanceCount, timeStep=timeStep, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running'

    call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalGet(stopTime-currTime, s_i8=seconds, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalGet(timeStep, s_i8=dt, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    gotm_time_min_n=advanceCount
    gotm_time_max_n=advanceCount + int(seconds/gotm_time_timestep, ESMF_KIND_I8)

    write(message,'(A,I4,A,I4,A)') trim(message)//' ', gotm_time_max_n - gotm_time_min_n, &
      ' steps of ',dt,' s to '//trim(timestring)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    ! get import state
    if (forcing_from_coupler) then
      call ESMF_StateGet(importState, itemSearch='temperature_in_water', &
        itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (itemCount==1) then
        call ESMF_StateGet(importState, 'temperature_in_water', import_field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldGet(import_field, farrayPtr=water_temperature, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        zerod%temp = water_temperature(1,1,1)
      else
        write(message,'(A)') 'Required field temperature_in_water not found'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      endif

      call ESMF_StateGet(importState, itemSearch='salinity_in_water', &
        itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (itemCount==1) then
        call ESMF_StateGet(importState, 'salinity_in_water', import_field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldGet(import_field, farrayPtr=salinity, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        zerod%salt = salinity(1,1,1)
      else
        call ESMF_LogWrite('Salinity field not found, '// &
              'set to default value of 30.0',ESMF_LOGMSG_WARNING)
        zerod%salt = 30.
      endif


      call ESMF_StateGet(importState, itemSearch='radiation_in_water', &
        itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (itemCount==1) then
        call ESMF_StateGet(importState, 'radiation_in_water', import_field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldGet(import_field, farrayPtr=salinity, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
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
      call ESMF_ClockAdvance(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    call MOSSCO_CompExit(gridComp, localrc)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    integer(ESMF_KIND_I4)      :: localrc

    call MOSSCO_CompEntry (gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call finalize_0d()

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
    integer(ESMF_KIND_I4)      :: localrc

    read(timestring(1:4),'(i4)') yy
    read(timestring(6:7),'(i2)') mm
    read(timestring(9:10),'(i2)') dd
    read(timestring(12:13),'(i2)') h
    read(timestring(15:16),'(i2)') m
    read(timestring(18:19),'(i2)') s

    call ESMF_TimeSet(time,yy=yy,mm=mm,dd=dd,h=h,m=m,s=s)

  end subroutine timeString2ESMF_Time


end module fabm0d_component


