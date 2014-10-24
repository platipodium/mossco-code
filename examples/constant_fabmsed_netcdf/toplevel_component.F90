!> @brief Implementation of an ESMF toplevel coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
module toplevel_component

  use esmf

  ! Registration routines for fabm
  use fabm_sediment_component, only : fabmsed_SetServices => SetServices
  use constant_component, only : constant_SetServices => SetServices
  use pelagic_benthic_coupler, only : pb_coupler_SetServices => SetServices
  use netcdf_component, only: netcdf_SetServices => SetServices

  use mossco_state

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),save     :: fabmsedComp, constantComp
  type(ESMF_GridComp), save    :: netcdfComp  
  type(ESMF_CplComp),save      :: pbCplComp
  type(ESMF_State),save,target :: state

  contains

  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: temperatureField, field, newfield
    type(ESMF_FieldBundle):: fieldBundle
    integer               :: petCount, localPet
    real(ESMF_KIND_R8),dimension(:,:),allocatable :: farray
    type(ESMF_TimeInterval) :: cplInterval

    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)

    call ESMF_TimeIntervalSet(cplInterval,s_r8=3600.0d0)
    call ESMF_ClockSet(parentClock,timeStep=cplInterval)

    ! Create component, call setservices, and create states
    fabmsedComp = ESMF_GridCompCreate(name="fabmsedComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(fabmsedComp,fabmsed_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    constantComp = ESMF_GridCompCreate(name="constantComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(constantComp,constant_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    netcdfComp = ESMF_GridCompCreate(name="netcdfComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(netcdfComp,netcdf_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!    pbCplComp = ESMF_CplCompCreate(name="pbCoupler",rc=rc)
!    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!    call ESMF_CplCompSetServices(pbCplComp,pb_coupler_SetServices, rc=rc)
!    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 

    !> State for exchange (only one generic)
    state = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="Exchange state")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !> use mosscosediment.nc as filename when coupling to netcdf_component
    call ESMF_AttributeSet(state,name='filename',value='mosscosediment.nc',rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(constantComp, importState=state, exportState=state,clock=parentClock,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_GridCompInitialize(fabmsedComp, importState=state, exportState=state, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    !> stop and re-initialize constantComp, since fabmsed replaced
    !! the boundary fields
    call ESMF_GridCompFinalize(constantComp)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(constantComp, importState=state, exportState=state,clock=parentClock,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_GridCompInitialize(netcdfComp, importState=state, exportState=state, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO)

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc
    type(ESMF_Field)      :: field
    type(ESMF_Clock)      :: childClock
    type(ESMF_TimeInterval)   :: cplInterval
    type(ESMF_Time)       :: currtime, ringTime
    type(ESMF_Alarm)      :: alarm
    logical :: clockIsPresent
    integer(ESMF_KIND_I8) :: advanceCount

    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO)

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))

      call ESMF_ClockGet(parentClock,currTime=currTime,timeStep=cplInterval, &
        advanceCount=advanceCount, rc=rc)

      !> run coupler components
!      call ESMF_CplCompRun(pbCplComp, importState=state, exportState=state, clock=parentClock, rc=rc)

      !> run grid components
      call ESMF_GridCompGet(fabmsedComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then 
        call ESMF_GridCompGet(fabmsedComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(fabmsedComp, importState=state, exportState=state, clock=parentClock, rc=rc)

      if (mod(advanceCount,24)==0) &
          call ESMF_GridCompRun(netcdfComp, & 
          importState=state, exportState=state, clock=parentClock, rc=rc)
      
      call ESMF_ClockAdvance(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    enddo 

    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_INFO)
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero

    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)

    call ESMF_GridCompFinalize(fabmsedComp, exportState=state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(fabmsedComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(constantComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(constantComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(netcdfComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(netcdfComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateDestroy(state,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module toplevel_component
