module esmf_toplevel_component

  use esmf

  ! Registration routines for fabm0d
  use fabm_0d_component, only : SetServices

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),save  :: fabm0dComp
  type(ESMF_State),save     :: fabm0dExp, fabm0dImp

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

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)
    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet)

    ! Create a grid and a field (still to clarify how to access the array
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), maxIndex=(/10,20,15/), &
      regDecomp=(/2,2,1/), name="fabm0dGrid", rc=rc)
    temperatureField = ESMF_FieldCreate(grid,typekind=ESMF_TYPEKIND_R8, &
      indexflag=ESMF_INDEX_DELOCAL, staggerloc=ESMF_STAGGERLOC_CENTER, name = "temperature", rc=rc)
    fieldBundle = ESMF_FieldBundleCreate(name="FABM field bundle", rc=rc)
    call ESMF_FieldBundleAdd(fieldBundle, (/temperatureField/),rc=rc)


    ! Create component, call setservices, and create states
    fabm0dComp = ESMF_GridCompCreate(name="fabm0dComp", grid=grid, rc=rc)
    call ESMF_GridCompSetServices(fabm0dComp,SetServices, rc=rc)
    
    fabm0dImp = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name="fabm0dImp")
    fabm0dExp = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="fabm0dExp")

    call ESMF_StateAdd(fabm0dImp, (/fieldBundle/), rc=rc)
    call ESMF_StateAdd(fabm0dExp, (/fieldBundle/), rc=rc)

    call ESMF_GridCompInitialize(fabm0dComp, importState=fabm0dImp, exportState=fabm0dExp, clock=parentClock, rc=rc)

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO) 

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO)

    write(0,*) '   time loop'
    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))

      call ESMF_ClockAdvance(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      call ESMF_GridCompRun(fabm0dComp, importState=fabm0dImp, exportState=fabm0dExp, clock=parentClock, rc=rc)

    enddo 

    write(0,*) '------------------------------------------------------------------------'
    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_INFO)
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)

    call ESMF_GridCompFinalize(fabm0dComp, importState=fabm0dImp, exportState=fabm0dExp, &
      clock=parentClock, rc=rc)

    call ESMF_GridCompDestroy(fabm0dComp, rc=rc)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module esmf_toplevel_component
