module toplevel_component

  use esmf
  use benthos_component, only: benthos_SetServices => SetServices

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),save    :: benthosComp
  character(len=ESMF_MAXSTR)  :: benthosCompName
  type(ESMF_State)            :: benthosImportState, benthosExportState

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

    integer               :: petCount, localPet

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)

    benthosCompName = "ESMF Benthos component"
    benthosComp     = ESMF_GridCompCreate(name=benthosCompName, contextflag=ESMF_CONTEXT_PARENT_VM,rc=rc)
    call ESMF_GridCompSetServices(benthoscomp, benthos_SetServices, rc=rc)
    benthosImportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name="benthos Import")
    benthosExportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="benthos Export")
    call ESMF_GridCompInitialize(benthosComp,importState=benthosImportState,exportState=benthosExportState,&
      clock=parentClock,rc=rc)

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO)

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer  :: myrank
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring,message

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO)
    call ESMF_GridCompGet(gridComp, localPet=myrank, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))
      call ESMF_ClockGet(parentClock, currtime=localtime, rc=rc)
      call ESMF_TimeGet(localtime, timeString=timestring, rc=rc)
      message = "Toplevel ticking at "//trim(timestring)
      call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)

      call ESMF_GridCompRun(benthosComp,importState=benthosImportState,&
        exportState=benthosExportState,clock=parentclock, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_ClockAdvance(parentClock, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   enddo

    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_INFO)

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    call ESMF_GridCompFinalize(benthosComp,importState=benthosImportState,exportState=benthosExportState, &
                            clock=parentclock, rc=rc)
    call ESMF_GridCompDestroy(benthosComp,rc=rc)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)

    rc=ESMF_SUCCESS

  end subroutine Finalize

end module toplevel_component
