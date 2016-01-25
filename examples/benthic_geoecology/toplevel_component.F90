!> @brief Implementation of an ESMF toplevel coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015, 2016 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "toplevel_component.F90"

module toplevel_component

  use esmf
  use mossco_component

  ! Registration routines for fabm
  use benthos_component, only : benthos_SetServices => SetServices
  use constant_component, only : constant_SetServices => SetServices
  use gotm_component, only : gotm_SetServices => SetServices
  use fabm_gotm_component, only : fabm_gotm_SetServices => SetServices
  use erosed_component, only: erosed_SetServices => SetServices
  use netcdf_component, only: netcdf_SetServices => SetServices

  use mossco_state

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),save     :: benthosComp, constantComp, gotmComp, fabmgotmComp
  type(ESMF_GridComp), save    :: erosedComp, netcdfComp
  type(ESMF_State),save,target :: state

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "Initialize"
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
    type(ESMF_Time)       :: currTime
    type(ESMF_Clock)      :: clock


    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2, localrc
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero
    character(len=ESMF_MAXSTR) :: name, message

    rc = ESMF_SUCCESS
    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalSet(cplInterval,s_r8=360.0d0)
    call ESMF_ClockSet(clock,timeStep=cplInterval)

    ! Create component, call setservices, and create states
    gotmComp = ESMF_GridCompCreate(name="gotmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gotmComp,gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    fabmgotmComp = ESMF_GridCompCreate(name="fabmgotmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(fabmgotmComp,fabm_gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    constantComp = ESMF_GridCompCreate(name="constantComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(constantComp,constant_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    erosedComp = ESMF_GridCompCreate(name="erosedComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(erosedComp,erosed_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    benthosComp = ESMF_GridCompCreate(name="benthosComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(benthosComp,benthos_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    netcdfComp = ESMF_GridCompCreate(name="netcdfComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(netcdfComp,netcdf_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !> State for exchange (only one generic)
    state = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="Exchange state")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !> use fbp_exchange_state.nc as filename when coupling to netcdf_component
    call ESMF_AttributeSet(state,name='filename',value='benthic_geoecology.nc',rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(constantComp, importState=state, exportState=state,clock=clock,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(gotmComp, importState=state, exportState=state, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(fabmgotmComp, importState=state, exportState=state, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(benthosComp, importState=state, exportState=state, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(erosedComp, importState=state, exportState=state, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(netcdfComp, importState=state, exportState=state, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO)

  end subroutine Initialize

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc
    type(ESMF_Field)      :: field
    type(ESMF_Clock)      :: clock,childClock
    type(ESMF_TimeInterval)   :: cplInterval
    type(ESMF_Time)       :: currtime, ringTime
    type(ESMF_Alarm)      :: alarm
    logical :: clockIsPresent
    integer(ESMF_KIND_I8) :: advanceCount

    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2, localrc
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero
    character(len=ESMF_MAXSTR) :: name, message

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))

      call ESMF_ClockGet(parentClock,currTime=currTime,timeStep=cplInterval, &
        advanceCount=advanceCount, rc=rc)

      !> run grid components
      call ESMF_GridCompGet(gotmComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then
        call ESMF_GridCompGet(gotmComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(gotmComp, importState=state, exportState=state, clock=clock, rc=rc)

      call ESMF_GridCompGet(benthosComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then
        call ESMF_GridCompGet(benthosComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(benthosComp, importState=state, exportState=state, clock=clock, rc=rc)

      call ESMF_GridCompGet(erosedComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then
        call ESMF_GridCompGet(erosedComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(erosedComp, importState=state, exportState=state, clock=clock, rc=rc)

      call ESMF_GridCompGet(fabmgotmComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then
        call ESMF_GridCompGet(fabmgotmComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(fabmgotmComp, importState=state, exportState=state, clock=clock, rc=rc)

      call ESMF_GridCompGet(netcdfComp,clock=childClock)
      call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      if (mod(advanceCount,5)==0) then
        call ESMF_GridCompRun(netcdfComp, importState=state, exportState=state, clock=clock, rc=rc)
      else
        call ESMF_ClockSet(childClock,currTime=currTime+cplInterval)
      endif

      call ESMF_ClockAdvance(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    enddo

    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_INFO)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2, localrc
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock,  &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(fabmgotmComp, exportState=state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(fabmgotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(gotmComp, exportState=state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(benthosComp, exportState=state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(benthosComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(constantComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(constantComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(erosedComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(erosedComp, rc=rc)
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
