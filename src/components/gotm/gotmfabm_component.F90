!> @brief GOTM/FABM component
!
!> This component is a modular, ESMF-based alternative to GOTM using its
!! FABM driver. Internally, the component uses the generic fabm_pelagic
!! infrastructure together with the gotm_component and the
!! gotm_transport_component for transport of fabm_pelagic's state variables
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
module gotmfabm_component

  use esmf

  use gotm_component, only : gotm_SetServices => SetServices 
  use fabm_pelagic_component, only : fabm_SetServices => SetServices
  use gotm_transport_component, only : gotm_transp_SetServices => SetServices
  use airsea, only : I_0 ! surface radiation in GOTM
  use observations, only : A ! Albedo in GOTM
  use meanflow, only : taub, rho

  implicit none

  private

  public SetServices

  type(ESMF_GridComp), save :: gotmComp, fabmComp, gotmTranspComp
  type(ESMF_State), save :: state
  real(ESMF_KIND_R8), dimension(:,:), pointer :: surface_radiation, bottom_stress
  real(ESMF_KIND_R8), dimension(:,:,:), pointer :: layer_height, gotm_layer_height, density

  contains

  !> Provide an ESMF compliant SetServices routine, which defines
  !! entry points for Init/Run/Finalize
  subroutine SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine SetServices

  !> Initialize the coupling
  !!
  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)     :: gridComp
    type(ESMF_State)        :: importState
    type(ESMF_State)        :: exportState
    type(ESMF_Clock)        :: parentClock, childClock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Field)        :: field
    integer, intent(out)    :: rc

    call ESMF_LogWrite("GOTM/FABM component initializing ... ",ESMF_LOGMSG_INFO)

    ! Create component, call setservices, and create states
    gotmComp = ESMF_GridCompCreate(name="gotmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gotmComp,gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    fabmComp = ESMF_GridCompCreate(name="fabmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(fabmComp,fabm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    gotmTranspComp = ESMF_GridCompCreate(name="gotmTranspComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gotmTranspComp,gotm_transp_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
    state = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="Exchange state")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(gotmComp, importState=state, exportState=state, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(state, 'foreign_grid_field_name', 'temperature_in_water',rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! set parentClock time step to gotmComp timestep
    call ESMF_GridCompGet(gotmComp, clock=childClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ClockGet(childClock, timeStep=timeStep, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ClockSet(parentCLock, timeStep=timeStep, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(fabmComp, importState=state, exportState=state, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(gotmTranspComp, importState=state, exportState=state, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! get surface radiation and layer_height pointer
    call ESMF_StateGet(state, 'surface_downwelling_photosynthetic_radiative_flux', field=field, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldGet(field, farrayPtr=surface_radiation, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateGet(state, 'bottom_stress', field=field, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldGet(field, farrayPtr=bottom_stress, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateGet(state, 'density_in_water', field=field, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldGet(field, farrayPtr=density, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_LogWrite("GOTM/FABM component initialized",ESMF_LOGMSG_INFO)

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)     :: gridComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock, childClock
    type(ESMF_Time)         :: currTime
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Field)        :: field
    integer, intent(out)    :: rc

    call ESMF_LogWrite("GOTM/FABM component running ... ",ESMF_LOGMSG_INFO)

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))
      
      call ESMF_ClockGet(parentClock, currTime=currTime, timeStep=timeStep, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_GridCompGet(gotmComp,clock=childClock)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ClockSet(childClock,stopTime=currTime+timeStep)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_GridCompRun(gotmComp, importState=state, exportState=state, clock=parentClock, rc=rc)

      call ESMF_GridCompGet(gotmTranspComp,clock=childClock)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ClockSet(childClock,stopTime=currTime+timeStep)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_GridCompRun(gotmTranspComp, importState=state, exportState=state, clock=parentClock, rc=rc)

      ! copy GOTM's surface radiation and layer height into state
      surface_radiation = I_0*(1.0d0 - A)
      bottom_stress = taub
      density(1,1,:) = rho(:)

      call ESMF_GridCompGet(fabmComp,clock=childClock)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ClockSet(childClock,stopTime=currTime+timeStep)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_GridCompRun(fabmComp, importState=state, exportState=state, clock=parentClock, rc=rc)

      call ESMF_ClockAdvance(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
    enddo 

    call ESMF_LogWrite("GOTM/FABM component finished running. ",ESMF_LOGMSG_INFO)

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    call ESMF_LogWrite("GOTM/FABM component finalizing",ESMF_LOGMSG_INFO)

#if 0
    call ESMF_GridCompFinalize(gotmTranspComp, importState=state, exportState=state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gotmTranspComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(fabmComp, importState=state, exportState=state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(fabmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(gotmComp, importState=state, exportState=state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateDestroy(state,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

    call ESMF_LogWrite("GOTM/FABM component finalized",ESMF_LOGMSG_INFO)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
    rc=ESMF_SUCCESS
  
  end subroutine Finalize

end module gotmfabm_component
