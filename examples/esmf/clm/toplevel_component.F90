!> @brief Implementation of an ESMF toplevel coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
module toplevel_component

  use esmf
  use clm_netcdf_component, only: clm_SetServices => SetServices

  implicit none

  private

  type(ESMF_GridComp),save    :: atmosphereComp
  character(len=ESMF_MAXSTR)  :: atmosphereCompName 
  type(ESMF_State)            :: atmosphereImportState, atmosphereExportState

  public SetServices

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
    
    type(ESMF_VM)         :: vm
    type(ESMF_Field)      :: field
    real(ESMF_KIND_R8),pointer    :: farrayPtr(:,:,:)

    integer               :: petCount, localPet
    
    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)
    
    atmosphereCompName = "ESMF CLM/NetCDF Atmospheric component"
    atmosphereComp     = ESMF_GridCompCreate(name=atmosphereCompName, contextflag=ESMF_CONTEXT_PARENT_VM,rc=rc)
    call ESMF_GridCompSetServices(atmospherecomp, clm_SetServices, rc=rc)
    atmosphereImportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name="Atmosphere Import")
    atmosphereExportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="Atmosphere Export")
    call ESMF_GridCompInitialize(atmosphereComp,importState=atmosphereImportState, &
      exportState=atmosphereExportState, clock=parentClock,rc=rc)

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
    type(ESMF_Field)      :: field
    real(ESMF_KIND_R8),pointer    :: farrayPtr(:,:,:)
    
    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO)
    call ESMF_GridCompGet(gridComp, localPet=myrank, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))
      call ESMF_ClockGet(parentClock, currtime=localtime, rc=rc)
      call ESMF_TimeGet(localtime, timeString=timestring, rc=rc)
      message = "Toplevel ticking at "//trim(timestring)
      call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
      call ESMF_GridCompRun(atmosphereComp,importState=atmosphereImportState,&
        exportState=atmosphereExportState,clock=parentclock, rc=rc)
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

    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)
    call ESMF_GridCompFinalize(atmosphereComp,importState=atmosphereImportState,exportState=atmosphereExportState, &
                            clock=parentclock, rc=rc)
    call ESMF_GridCompDestroy(atmosphereComp,rc=rc)
  
    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module toplevel_component
