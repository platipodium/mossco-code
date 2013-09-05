
module erosed_component

  use esmf
  use mossco_erosed, only:

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

  public :: empty_SetServices
  
  contains

  !> Provide an ESMF compliant SetServices routine, which defines
  !! the entry points for Init/Run/Finalize

  subroutine empty_SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine empty_SetServices

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

    type(ESMF_Grid)      :: grid
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array)     :: array
    real(ESMF_KIND_R8),dimension(:),pointer :: LonCoord,LatCoord,DepthCoord 

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
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/1,1,1/),name="FABM0d grid")

    !> create export fields
    array = ESMF_ArrayCreate(distgrid,farray=din%conc,indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    din_field = ESMF_FieldCreate(grid, array, name="dissolved_inorganic_nitrogen_in_water", rc=rc)
    array = ESMF_ArrayCreate(distgrid,farray=pon%conc,indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    pon_field = ESMF_FieldCreate(grid, array, name="particulare_organic_nitrogen_in_water", rc=rc)
    array = ESMF_ArrayCreate(distgrid,farray=pon%ws,indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    pon_ws_field = ESMF_FieldCreate(grid, arrayspec, name="pon_sinking_velocity_in_water", rc=rc)
    !> set export state
    call ESMF_StateAdd(exportState,(/din_field,pon_field,pon_ws_field/),rc=rc)

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
    write (logstring,'(A,I6,A,A)') "0d run(",n,") at ",timestring
    call ESMF_LogWrite(trim(logstring), ESMF_LOGMSG_INFO)

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


end module erosed_component
