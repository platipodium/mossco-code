!> @file esmf_fabm_0d_component.F90
!! @brief ESMF/FABM 0d driver component
!!
!! The ESMF component contains the 0d driver module
!! @author Richard Hofmeister

!> The ESMF/FABM 0d driver component module provides infrastructure for the
!! MOSSCO 0d pelagic component.

module esmf_fabm_0d_component

  use esmf
  use time, only: gotm_time_min_n => MinN, gotm_time_max_n => MaxN
  use time, only: gotm_time_timestep => timestep
  use time, only: gotm_time_start => start, gotm_time_stop => stop
  use time, only: gotm_time_timefmt => timefmt
  use time, only: gotm_time_init_time => init_time
  use fabm_0d, only: init_0d => init_run
  use fabm_0d, only: time_loop_0d => time_loop
  use fabm_0d, only: finalize_0d => clean_up

  implicit none

  private

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

    character(len=19) :: timestring
    type(ESMF_Time)   :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8) :: dt
    integer            :: nsave,ode_method,namlst=234
    character(len=80)  :: title
    character          :: c1,c2,c3,c4
    integer            :: yy,mm,dd,hh,minu,ss

    namelist /model_setup/ title,start,stop,dt,ode_method

    ! read 0d namelist
    open(namlst,file='run.nml',status='old',action='read')
    read(namlst,nml=model_setup)
    close(namlst)

    call ESMF_LogWrite('Initialize 0d',ESMF_LOGMSG_INFO)
    call init_0d()

    ! set parent clock like in model_setup namelist
    read(start,'(i4,a1,i2,a1,i2,1x,i2,a1,i2,a1,i2)')  &
                          yy,c1,mm,c2,dd,hh,c3,minu,c4,ss 
    call ESMF_TimeSet(clockTime,YY=yy,MM=mm,DD=dd,H=hh,M=minu,S=ss)
    call ESMF_ClockSet(parentClock,startTime=clockTime)

    call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_time_timestep,rc=rc)
    call ESMF_ClockSet(parentClock,timeStep=timeInterval,rc=rc)

    read(stop,'(i4,a1,i2,a1,i2,1x,i2,a1,i2,a1,i2)')  &
                          yy,c1,mm,c2,dd,hh,c3,minu,c4,ss
    call ESMF_TimeSet(clockTime,YY=yy,MM=mm,DD=dd,H=hh,M=minu,S=ss)
    call ESMF_ClockSet(parentClock,stopTime=clockTime)
    
  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19) :: timestring
    type(ESMF_Time)   :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8)      :: dt
    integer(ESMF_KIND_I8)   :: n

    ! get global clock , set n to global/local, call 0d, advance local clock n steps., 
    call ESMF_TimeSet(clockTime)
    call ESMF_ClockGet(parentClock,currTime=clockTime,AdvanceCount=n)
    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
    call ESMF_LogWrite("0d run at "//timestring//")", ESMF_LOGMSG_INFO)

    ! use AdvanceCount from parent clock
    gotm_time_min_n = n
    gotm_time_max_n = gotm_time_min_n
   
    call time_loop_0d()

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    call finalize_0d()

  end subroutine Finalize

end module esmf_fabm_0d_component
