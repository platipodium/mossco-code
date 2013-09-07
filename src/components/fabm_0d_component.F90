!> @file esmf_fabm_0d_component.F90
!! @brief ESMF/FABM 0d driver component
!!
!! The ESMF component contains the 0d driver module
!! @author Richard Hofmeister
!! @author Carsten Lemmen

!> The ESMF/FABM 0d driver component module provides infrastructure for the
!! MOSSCO 0d pelagic component.

module fabm_0d_component

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
  use mossco_fabm0d, only: get_export_state_from_variable_name, update_export_state

  implicit none

  private

  real(ESMF_KIND_R8), pointer :: water_temperature(:,:,:)
  type(ESMF_Field)            :: water_temperature_field
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

    character(len=19) :: timestring
    type(ESMF_Time)   :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8) :: dt
    integer            :: ode_method,namlst=234
    character(len=80)  :: title
    logical            :: input_from_namelist = .true.
    character(len=256) :: din_variable='',pon_variable=''

    namelist /model_setup/ title,start,stop,dt,ode_method, &
                           din_variable, pon_variable, forcing_from_coupler

    ! read 0d namelist
    open(namlst,file='run.nml',status='old',action='read')
    read(namlst,nml=model_setup)
    close(namlst)

    call ESMF_LogWrite('Initialize 0d',ESMF_LOGMSG_INFO)
    call init_0d(forcing_from_coupler=forcing_from_coupler)

    !> get export_states information
    call get_export_state_from_variable_name(din,din_variable)
    call get_export_state_from_variable_name(pon,pon_variable)

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

    !> set clock for 0d component
    call ESMF_TimeSet(clockTime)
    if (input_from_namelist) then !> overwrite the parent clock's settings with the namelist parameters
      call ESMF_LogWrite('Get GOTM input from namelist',ESMF_LOGMSG_INFO)
      call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_time_timestep,rc=rc)
      call ESMF_ClockSet(parentClock,timeStep=timeInterval,rc=rc)

      timestring=gotm_time_start(1:10)//"T"//gotm_time_start(12:19)
      call timestring2ESMF_Time(timestring,clockTime)
      call ESMF_ClockSet(parentClock,startTime=clockTime)
      
      timestring=gotm_time_stop(1:10)//"T"//gotm_time_stop(12:19)
      call timestring2ESMF_Time(timestring,clockTime)
      call ESMF_ClockSet(parentClock,stopTime=clockTime)
    else !> get parent clock and overwrite namelist parameters
      call ESMF_LogWrite('Set GOTM input from ESMF parent',ESMF_LOGMSG_INFO)
      call ESMF_ClockGet(parentClock,startTime=clockTime)
      call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
      gotm_time_start=timestring(1:10)//" "//timestring(12:19)

      call ESMF_ClockGet(parentClock,timeStep=timeInterval,rc=rc)
      call ESMF_TimeIntervalGet(timeInterval,s_r8=gotm_time_timestep,rc=rc)
     
      call ESMF_ClockGet(parentClock,stopTime=clockTime)
      call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
      gotm_time_stop=timestring(1:10)//" "//timestring(12:19)
      
    endif
    
  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)    :: timestring
    character(len=255)   ::logstring
    type(ESMF_Time)      :: clockTime
    integer(ESMF_KIND_I8)   :: n

    ! get global clock , set n to global/local, call 0d, advance local clock n steps., 
    call ESMF_TimeSet(clockTime)
    call ESMF_ClockGet(parentClock,currTime=clockTime,AdvanceCount=n)
    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
#ifdef DEBUG
    write (logstring,'(A,I6,A,A)') "0d run(",n,") at ",timestring
    call ESMF_LogWrite(trim(logstring), ESMF_LOGMSG_INFO)
#endif

    ! get import state
    if (forcing_from_coupler) then
      call ESMF_StateGet(importState, "water_temperature", water_temperature_field, rc=rc)
      call ESMF_FieldGet(water_temperature_field, farrayPtr=water_temperature, rc=rc)
      zerod%temp = water_temperature(1,1,1)
    end if

    ! use AdvanceCount from parent clock
    gotm_time_min_n = n
    gotm_time_max_n = gotm_time_min_n
   
    call time_loop_0d()

    ! set export states
    call update_export_state(din)
    call update_export_state(pon)

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


end module fabm_0d_component
