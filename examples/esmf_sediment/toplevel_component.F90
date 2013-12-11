module esmf_toplevel_component

  use esmf

  ! Registration routines for fabm
  use fabm_sediment_component, only : fabmsed_SetServices => SetServices
  use fabm_sediment_component, only : bdys,fluxes,rk
  use constant_component, only : constant_SetServices => SetServices

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),save  :: fabmComp,constantComp
  type(ESMF_State),save     :: fabmExp, fabmImp

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
    type(ESMF_Field)      :: temperatureField
    type(ESMF_FieldBundle) :: fieldBundle
    integer               :: petCount, localPet
    real(ESMF_KIND_R8),dimension(:,:),allocatable :: farray

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)

    ! Create component, call setservices, and create states
    fabmComp = ESMF_GridCompCreate(name="fabmComp", rc=rc)
    call ESMF_GridCompSetServices(fabmComp,fabmsed_SetServices, rc=rc)
    constantComp = ESMF_GridCompCreate(name="constantComp",rc=rc)
    call ESMF_GridCompSetServices(constantComp,constant_SetServices, rc=rc)
    
    fabmImp = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name="fabmImp")
    fabmExp = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="fabmExp")

    call ESMF_GridCompInitialize(constantComp, importState=fabmExp, exportState=fabmImp,clock=parentClock,rc=rc)
    call ESMF_GridCompInitialize(fabmComp, importState=fabmImp, exportState=fabmExp, clock=parentClock, rc=rc)

    ! same boundary conditions as for the standalone omexdia_p example:
    bdys(1,1,1:9) = 0.0_rk
    bdys(1,1,1) = 10._rk   ! degC temperature
    bdys(1,1,5) = 1.0_rk   ! mmolP/m**3 po4
    bdys(1,1,6) = 10.0_rk  ! mmolN/m**3 no3
    bdys(1,1,7) = 0.0_rk   ! mmolN/m**3 nh3
    bdys(1,1,8) = 250.0_rk ! mmolO2/m**3 oxy
    bdys(1,1,9) = 0.0_rk   ! odu

    fluxes(1,1,1:8) = 0.0_rk
    fluxes(1,1,1) = 5.0_rk/86400.0_rk !fdet
    fluxes(1,1,2) = 5.0_rk/86400.0_rk !sdet
    fluxes(1,1,3) = 0.08/86400.0_rk !pdet

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO) 

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO)

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))

      call ESMF_ClockAdvance(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      call ESMF_GridCompRun(fabmComp, importState=fabmImp, exportState=fabmExp, clock=parentClock, rc=rc)

    enddo 

    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_INFO)
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)

    call ESMF_GridCompFinalize(fabmComp, importState=fabmImp, exportState=fabmExp, &
      clock=parentClock, rc=rc)
    call ESMF_GridCompDestroy(fabmComp, rc=rc)

    call ESMF_GridCompFinalize(constantComp, importState=fabmExp, exportState=fabmImp, &
      clock=parentClock, rc=rc)
    call ESMF_GridCompDestroy(constantComp, rc=rc)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module esmf_toplevel_component
