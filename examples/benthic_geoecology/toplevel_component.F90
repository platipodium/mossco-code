module toplevel_component

  use esmf

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

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)

    call ESMF_TimeIntervalSet(cplInterval,s_r8=360.0d0)
    call ESMF_ClockSet(parentClock,timeStep=cplInterval)

    ! Create component, call setservices, and create states
    gotmComp = ESMF_GridCompCreate(name="gotmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gotmComp,gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    fabmgotmComp = ESMF_GridCompCreate(name="fabmgotmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(fabmgotmComp,fabm_gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    constantComp = ESMF_GridCompCreate(name="constantComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(constantComp,constant_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    erosedComp = ESMF_GridCompCreate(name="erosedComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(erosedComp,erosed_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    benthosComp = ESMF_GridCompCreate(name="benthosComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(benthosComp,benthos_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    netcdfComp = ESMF_GridCompCreate(name="netcdfComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(netcdfComp,netcdf_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !> State for exchange (only one generic)
    state = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="Exchange state")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !> use fbp_exchange_state.nc as filename when coupling to netcdf_component
    call ESMF_AttributeSet(state,name='filename',value='benthic_geoecology.nc',rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(constantComp, importState=state, exportState=state,clock=parentClock,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(gotmComp, importState=state, exportState=state, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(fabmgotmComp, importState=state, exportState=state, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(benthosComp, importState=state, exportState=state, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(erosedComp, importState=state, exportState=state, clock=parentClock, rc=rc)
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

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO)

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
      call ESMF_GridCompRun(gotmComp, importState=state, exportState=state, clock=parentClock, rc=rc)

      call ESMF_GridCompGet(benthosComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then 
        call ESMF_GridCompGet(benthosComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(benthosComp, importState=state, exportState=state, clock=parentClock, rc=rc)

      call ESMF_GridCompGet(erosedComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then 
        call ESMF_GridCompGet(erosedComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(erosedComp, importState=state, exportState=state, clock=parentClock, rc=rc)

      call ESMF_GridCompGet(fabmgotmComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then 
        call ESMF_GridCompGet(fabmgotmComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(fabmgotmComp, importState=state, exportState=state, clock=parentClock, rc=rc)
      
      if (mod(advanceCount,20)==0) &
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

    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)

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
