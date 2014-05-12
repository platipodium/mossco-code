module toplevel_component

  use esmf

  ! Registration routines for fabm
  use fabm_sediment_component, only : fabmsed_SetServices => SetServices
  use constant_component, only : constant_SetServices => SetServices
  use gotm_component, only : gotm_SetServices => SetServices
  use fabm_gotm_component, only : fabm_gotm_SetServices => SetServices
  use pelagic_benthic_coupler, only : pb_coupler_SetServices => SetServices
  use benthic_pelagic_coupler, only : bp_coupler_SetServices => SetServices
#ifdef MOSSCO_EROSED
  use erosed_component, only: erosed_SetServices => SetServices
#endif
  use netcdf_component, only: netcdf_SetServices => SetServices

  use mossco_state

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),save     :: fabmsedComp, constantComp, gotmComp, fabmgotmComp
#ifdef MOSSCO_EROSED
  type(ESMF_GridComp), save    :: erosedComp
#endif
  type(ESMF_GridComp), save    :: netcdfComp  
  type(ESMF_CplComp),save      :: pbCplComp,bpCplComp
  type(ESMF_State),save,target :: state
  type(ESMF_State),pointer     :: pelagicstate,sedimentstate

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
    fabmsedComp = ESMF_GridCompCreate(name="fabmsedComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(fabmsedComp,fabmsed_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    constantComp = ESMF_GridCompCreate(name="constantComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(constantComp,constant_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#ifdef MOSSCO_EROSED
    erosedComp = ESMF_GridCompCreate(name="erosedComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(erosedComp,erosed_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif
    netcdfComp = ESMF_GridCompCreate(name="netcdfComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(netcdfComp,netcdf_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    bpCplComp = ESMF_CplCompCreate(name="bpCoupler",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetServices(bpCplComp,bp_coupler_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    pbCplComp = ESMF_CplCompCreate(name="pbCoupler",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetServices(pbCplComp,pb_coupler_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 

    !> State for exchange (only one generic)
    !! keep pelagicstate and sedimentstate as used before 2014-03-20
    state = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="Exchange state")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !> use fbp_exchange_state.nc as filename when coupling to netcdf_component
    call ESMF_AttributeSet(state,name='filename',value='fbp_exchange_state.nc',rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    pelagicstate => state
    sedimentstate => state

    call ESMF_GridCompInitialize(constantComp, importState=pelagicState, exportState=pelagicState,clock=parentClock,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(gotmComp, importState=pelagicstate, exportState=pelagicstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(fabmgotmComp, importState=pelagicstate, exportState=pelagicstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(fabmsedComp, importState=pelagicstate, exportState=sedimentstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#ifdef MOSSCO_EROSED
    call ESMF_GridCompInitialize(erosedComp, importState=pelagicstate, exportState=sedimentstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif
    call ESMF_GridCompInitialize(netcdfComp, importState=pelagicstate, exportState=sedimentstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_CplCompInitialize(pbCplComp, importState=pelagicstate, exportState=pelagicstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompInitialize(bpCplComp, importState=pelagicstate, exportState=sedimentstate, clock=parentClock, rc=rc)
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

      !> run coupler components
      call ESMF_CplCompRun(pbCplComp, importState=pelagicstate, exportState=pelagicstate, clock=parentClock, rc=rc)
      call ESMF_CplCompRun(bpCplComp, importState=sedimentstate, exportState=sedimentstate, clock=parentClock, rc=rc)

      !> run grid components
      call ESMF_GridCompGet(gotmComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then 
        call ESMF_GridCompGet(gotmComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(gotmComp, importState=pelagicstate, exportState=pelagicstate, clock=parentClock, rc=rc)

#ifdef MOSSCO_EROSED
      call ESMF_GridCompGet(erosedComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then 
        call ESMF_GridCompGet(erosedComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(erosedComp, importState=pelagicstate, exportState=sedimentstate, clock=parentClock, rc=rc)
#endif

      call ESMF_GridCompGet(fabmgotmComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then 
        call ESMF_GridCompGet(fabmgotmComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(fabmgotmComp, importState=sedimentstate, exportState=pelagicstate, clock=parentClock, rc=rc)

      call ESMF_GridCompGet(fabmsedComp,clockIsPresent=clockIsPresent)
      if (clockIsPresent) then 
        call ESMF_GridCompGet(fabmsedComp,clock=childClock)
        call ESMF_ClockSet(childClock,stopTime=currTime+cplInterval)
      else
        childClock = parentClock
      endif
      call ESMF_GridCompRun(fabmsedComp, importState=pelagicstate, exportState=sedimentstate, clock=parentClock, rc=rc)

      !call ESMF_GridCompGet(gotmComp,clockIsPresent=clockIsPresent)
      !  call ESMF_GridCompGet(gotmComp,clock=childClock)
      !  call ESMF_ClockGetAlarm(childClock,'outputAlarm', alarm, rc=rc)
      !  write(0,*) rc
      
      if (mod(advanceCount,240)==0) &
          call ESMF_GridCompRun(netcdfComp, & 
          importState=sedimentstate, exportState=sedimentstate, clock=parentClock, rc=rc)
      
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

    call ESMF_GridCompFinalize(fabmgotmComp, exportState=pelagicstate, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(fabmgotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(gotmComp, exportState=pelagicstate, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(fabmsedComp, exportState=sedimentstate, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(fabmsedComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(constantComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(constantComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifdef MOSSCO_EROSED
    call ESMF_GridCompFinalize(erosedComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(erosedComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

    call ESMF_GridCompFinalize(netcdfComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(netcdfComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateDestroy(state,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateDestroy(pelagicstate,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateDestroy(sedimentstate,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module toplevel_component
