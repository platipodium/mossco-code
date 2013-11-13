!> @brief Toplevel component for a n-way coupled system
!
!> This module implements a n-way coupled system configured by
!> the configuration given in coupling.yaml
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module toplevel_component

  use esmf
  use coupling_system

#ifdef _GOTMOCEAN_
  use gotm_component, only: ocean_SetServices => SetServices
#else
  use remtc_ocean, only: ocean_SetServices => SetServices
#endif
  use fabm_0d_component, only: fabm0d_SetServices => SetServices

  implicit none
  
  private

  public SetServices

  type(ESMF_GridComp),save    :: fabm0dComp
  type(ESMF_State)            :: fabm0dImportState, fabm0dExportState

  type(ESMF_GridComp),save    :: oceanComp
  type(ESMF_State)            :: oceanImportState, oceanExportState

  type(ESMF_CplComp),save     :: oscplComp

  contains

  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridComp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridComp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridComp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridComp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer               :: petCount, localPet
    
    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)
    
    fabm0dComp     = ESMF_GridCompCreate(name="ESMF/FABM 0d component", &          
                         contextflag=ESMF_CONTEXT_PARENT_VM,rc=rc)
    call ESMF_GridCompSetServices(fabm0dComp, fabm0d_SetServices, rc=rc)
    fabm0dImportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name="fabm0d Import")
    fabm0dExportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="fabm0d Export")
    call ESMF_GridCompInitialize(fabm0dComp,importState=fabm0dImportState,exportState=fabm0dExportState,&
      clock=parentClock,rc=rc)

    oceanComp     = ESMF_GridCompCreate(name="ESMF Ocean component", contextflag=ESMF_CONTEXT_PARENT_VM,rc=rc)
    call ESMF_GridCompSetServices(oceanComp, ocean_SetServices, rc=rc)
    oceanImportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name="Ocean Import")
    oceanExportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="Ocean Export")
    call ESMF_GridCompInitialize(oceanComp,importState=oceanImportState,exportState=oceanExportState,&
      clock=parentClock,rc=rc)

    !oscplComp     = ESMF_CplCompCreate(name="O-S coupler component", contextflag=ESMF_CONTEXT_PARENT_VM,rc=rc)
    !call ESMF_CplCompSetServices(oscplComp, oscpl_SetServices, rc=rc)
    !call ESMF_CplCompInitialize(oscplComp,importState=sedimentExportState,exportState=oceanImportState,&
    !  clock=parentClock,rc=rc)

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO) 

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer  :: myrank
    type(ESMF_Time)             :: localTime
    character (len=ESMF_MAXSTR) :: timeString,message

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO)
    call ESMF_GridCompGet(gridComp, localPet=myrank, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))
      call ESMF_ClockAdvance(parentClock, rc=rc)
      call ESMF_ClockGet(parentClock, currtime=localtime, rc=rc)
      call ESMF_TimeGet(localTime, timeString=timeString, rc=rc)
#ifdef DEBUG
      message = "Toplevel ticking at "//trim(timeString)
      call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
#endif

    !  call ESMF_CplComprun(oscplComp,importState=sedimentExportState,& 
    !    exportState=oceanImportState,clock=parentclock,rc=rc)
    !  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
      call ESMF_GridCompRun(oceanComp,importState=oceanImportState,&
        exportState=oceanExportState,clock=parentclock, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      !call ESMF_StatePrint(oceanImportState,rc=rc)

      call ESMF_GridCompRun(fabm0dComp,importState=oceanExportState,&
      !call ESMF_GridCompRun(fabm0dComp,importState=fabm0dImportState,&
        exportState=fabm0dExportState,clock=parentclock, rc=rc)
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
    
    call ESMF_GridCompFinalize(oceanComp,importState=oceanImportState,exportState=oceanExportState, &
                            clock=parentclock, rc=rc)
    call ESMF_GridCompDestroy(oceanComp,rc=rc)
  
    call ESMF_GridCompFinalize(fabm0dComp,importState=fabm0dImportState,exportState=fabm0dExportState, &
                            clock=parentclock, rc=rc)
    call ESMF_GridCompDestroy(fabm0dComp,rc=rc)
  
    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module toplevel_component
