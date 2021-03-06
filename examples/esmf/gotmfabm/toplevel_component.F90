!> @brief Implementation of an ESMF toplevel coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
module toplevel_component

  use esmf

  use gotm_component, only : gotm_SetServices => SetServices
  use fabm_gotm_component, only : fabm_gotm_SetServices => SetServices

  use mossco_state

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),save  :: gotmComp, fabmgotmComp
  type(ESMF_State),save     :: state

  contains

  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)     :: gridComp
    type(ESMF_State)        :: importState
    type(ESMF_State)        :: exportState
    type(ESMF_Clock)        :: parentClock, childClock
    type(ESMF_TimeInterval) :: timeStep
    integer, intent(out)    :: rc

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)

    ! Create component, call setservices, and create states
    gotmComp = ESMF_GridCompCreate(name="gotmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gotmComp,gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    fabmgotmComp = ESMF_GridCompCreate(name="fabmgotmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(fabmgotmComp,fabm_gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
    state = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="Exchange state")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(gotmComp, exportState=state, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! set parentClock time step to gotmComp timestep
    call ESMF_GridCompGet(gotmComp, clock=childClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ClockGet(childClock, timeStep=timeStep, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ClockSet(parentCLock, timeStep=timeStep, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(fabmgotmComp, importState=state, exportState=state, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO)

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)     :: gridComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock, childClock
    type(ESMF_Time)         :: currTime
    type(ESMF_TimeInterval) :: timeStep
    integer, intent(out)    :: rc

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO)

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))
      
      call ESMF_ClockGet(parentClock, currTime=currTime, timeStep=timeStep, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_GridCompGet(gotmComp,clock=childClock)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ClockSet(childClock,stopTime=currTime+timeStep)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_GridCompRun(gotmComp, exportState=state, clock=parentClock, rc=rc)

      call ESMF_GridCompGet(fabmgotmComp,clock=childClock)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ClockSet(childClock,stopTime=currTime+timeStep)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_GridCompRun(fabmgotmComp, importState=state, exportState=state, clock=parentClock, rc=rc)

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

    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)

    call ESMF_GridCompFinalize(fabmgotmComp, exportState=state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(fabmgotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(gotmComp, exportState=state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateDestroy(state,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module toplevel_component
