module toplevel_component

  use esmf

  ! Registration routines for gotm
  use gotm_component, only : gotm_SetServices => SetServices

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),save  :: gotmComp
  type(ESMF_State),save     :: gotmExp, gotmImp

  contains

  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    rc=ESMF_SUCCESS
    return

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
    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Create a grid and a field (still to clarify how to access the array
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), maxIndex=(/10,20,15/), &
      regDecomp=(/2,2,1/), name="gotmGrid", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    temperatureField = ESMF_FieldCreate(grid,typekind=ESMF_TYPEKIND_R8, &
      indexflag=ESMF_INDEX_DELOCAL, staggerloc=ESMF_STAGGERLOC_CENTER, name = "temperature", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    fieldBundle = ESMF_FieldBundleCreate(name="FABM field bundle", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleAdd(fieldBundle, (/temperatureField/),rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Create component, call setservices, and create states
    gotmComp = ESMF_GridCompCreate(name="gotmComp", grid=grid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
    call ESMF_GridCompSetServices(gotmComp,gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    gotmImp = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name="gotmImp")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    gotmExp = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="gotmExp")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(gotmImp, (/fieldBundle/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateAdd(gotmExp, (/fieldBundle/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(gotmComp, importState=gotmImp, exportState=gotmExp, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO) 
    rc = ESMF_SUCCESS

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc
  
    type(ESMF_Time)       :: clockTime
    character(ESMF_MAXSTR) :: timestring, message
    integer(ESMF_KIND_I8) :: count

    call ESMF_ClockGet(parentClock,currTime=clockTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(timestring)
    call ESMF_ClockGet(parentClock,stopTime=clockTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(message)//" toplevel comp runs until "//trim(timestring)
    call ESMF_LogWrite(message,ESMF_LOGMSG_INFO)

    do while (.not. ESMF_ClockIsStopTime(parentClock))

      call ESMF_ClockGet(parentClock,currTime=clockTime, advanceCount=count, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring,rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      write(message,'(A,I6)') trim(timestring)//" toplevel iteration ",count
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

      call ESMF_GridCompRun(gotmComp, importState=gotmImp, exportState=gotmExp, clock=parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_ClockAdvance(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
    enddo 

    call ESMF_ClockGet(parentClock,currTime=clockTime, advanceCount=count, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A,I6)') trim(timestring)//" toplevel finished running."
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    rc=ESMF_SUCCESS
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)

    call ESMF_GridCompFinalize(gotmComp, importState=gotmImp, exportState=gotmExp, &
      clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompDestroy(gotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module toplevel_component
