module esmf_toplevel_component

  use esmf
  use remtc_ocean, only: ocean_SetServices => SetServices
  use ocean_sediment_coupler, only: oscpl_SetServices => SetServices
  use esmf_fabm_sediment_component, only: sediment_SetServices => empty_SetServices
  use esmf_fabm_0d_component, only: fabm0d_SetServices => SetServices

  implicit none
  
  private

  public SetServices

  type(ESMF_GridComp),save    :: fabm0dComp
  character(len=ESMF_MAXSTR)  :: fabm0dCompName 
  type(ESMF_State)            :: fabm0dImportState, fabm0dExportState

  type(ESMF_GridComp),save    :: sedimentComp
  character(len=ESMF_MAXSTR)  :: sedimentCompName 
  type(ESMF_State)            :: sedimentImportState, sedimentExportState

  type(ESMF_GridComp),save    :: oceanComp
  character(len=ESMF_MAXSTR)  :: oceanCompName 
  type(ESMF_State)            :: oceanImportState, oceanExportState

  type(ESMF_CplComp),save     :: oscplComp
  character(len=ESMF_MAXSTR)  :: oscplCompName 

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
    
    sedimentCompName = "ESMF/FABM sediment component"
    sedimentComp     = ESMF_GridCompCreate(name=sedimentCompName, contextflag=ESMF_CONTEXT_PARENT_VM,rc=rc)
    call ESMF_GridCompSetServices(sedimentcomp, sediment_SetServices, rc=rc)
    sedimentImportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name="Sediment Import")
    sedimentExportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="Sediment Export")
    call ESMF_GridCompInitialize(sedimentComp,importState=sedimentImportState,exportState=sedimentExportState,&
      clock=parentClock,rc=rc)

    oceanCompName = "ESMF Remtc Ocean component"
    oceanComp     = ESMF_GridCompCreate(name=oceanCompName, contextflag=ESMF_CONTEXT_PARENT_VM,rc=rc)
    call ESMF_GridCompSetServices(oceancomp, ocean_SetServices, rc=rc)
    oceanImportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name="Ocean Import")
    oceanExportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="Ocean Export")
    call ESMF_GridCompInitialize(oceanComp,importState=oceanImportState,exportState=oceanExportState,&
      clock=parentClock,rc=rc)

    oscplCompName = "ESMF/FABM ocean-sediment coupler component"
    oscplComp     = ESMF_CplCompCreate(name=oscplCompName, contextflag=ESMF_CONTEXT_PARENT_VM,rc=rc)
    call ESMF_CplCompSetServices(oscplcomp, oscpl_SetServices, rc=rc)
    call ESMF_CplCompInitialize(oscplComp,importState=sedimentExportState,exportState=oceanImportState,&
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
      call ESMF_ClockAdvance(parentClock, rc=rc)
      call ESMF_ClockGet(parentClock, currtime=localtime, rc=rc)
      call ESMF_TimeGet(localtime, timeString=timestring, rc=rc)
      message = "Toplevel ticking at "//trim(timestring)
      call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
      call ESMF_GridCompRun(sedimentComp,importState=sedimentImportState,&
        exportState=sedimentExportState,clock=parentclock, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_CplComprun(oscplComp,importState=sedimentExportState,& 
        exportState=oceanImportState,clock=parentclock,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
      call ESMF_GridCompRun(oceanComp,importState=oceanImportState,&
        exportState=oceanExportState,clock=parentclock, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   enddo 

    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_INFO)
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)
    call ESMF_GridCompFinalize(sedimentComp,importState=sedimentImportState,exportState=sedimentExportState, &
                            clock=parentclock, rc=rc)
    call ESMF_GridCompDestroy(sedimentComp,rc=rc)
    call ESMF_GridCompFinalize(oceanComp,importState=oceanImportState,exportState=oceanExportState, &
                            clock=parentclock, rc=rc)
    call ESMF_GridCompDestroy(oceanComp,rc=rc)
  
    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module esmf_toplevel_component
