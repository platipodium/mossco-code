!> @file esmf_gotm_component.F90
!! @brief ESMF/GOTM driver component
!!
!! The ESMF component contains the gotm driver module
!! @author Carsten Lemmen

!> The ESMF/GOTM driver component module provides infrastructure for the
!! MOSSCO GOTM component.

module esmf_gotm_component

  use esmf
  use time, only: gotm_time_min_n => MinN, gotm_time_max_n => MaxN
  use time, only: gotm_time_timestep => timestep
  use time, only: gotm_time_start => start, gotm_time_stop => stop
  use time, only: gotm_time_timefmt => timefmt
  use time, only: gotm_time_init_time => init_time
  use gotm, only: init_gotm, gotm_time_loop => time_loop, clean_up

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
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    call ESMF_LogWrite('Initialize GOTM',ESMF_LOGMSG_INFO)
    call init_gotm()

    ! Manipulate the time parameters from the gotm namelist
    ! dt    ! float time steop for integration in seconds, mapped to timestep
    ! start  ! string date in yyyy-mm-dd hh:mm:ss format for start date, mapped to jul1,secs1
    ! stop   ! string date in yyyy-mm-dd hh:mm:ss format for end date, mapped to jul2,secs2
   
    ! Reset all other variables for consistency and those set in init_time
    gotm_time_timefmt = 2 
    ! gotm_time_timestep = 
    ! gotm_time_start = 
    ! gotm_time_end = 

    call gotm_time_init_time(gotm_time_min_n,gotm_time_max_n)
    

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    ! get local clock with GOTM timesteop, get glboal clock with coupling timestep, set n to global/local, call GOTM, advance local clock n steps., 

    ! call ESMF_GET_TIMESTEP_N
    gotm_time_min_n = 1
    gotm_time_max_n = gotm_time_min_n  + 0
    write (*,*) gotm_time_min_n,gotm_time_max_n,gotm_time_timestep,gotm_time_start,gotm_time_stop
    call gotm_time_loop()

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    call clean_up()

  end subroutine Finalize

end module esmf_gotm_component
