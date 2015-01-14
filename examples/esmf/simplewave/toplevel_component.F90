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
  use simplewave_component, only: simplewave_SetServices => SetServices

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),save    :: simplewave_Comp
  type(ESMF_State)            :: simplewave_ImportState, simplewave_ExportState

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

    integer               :: phaseCount,p
    logical               :: phaseZeroFlag
    
    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_TRACE)

!   create child component
    simplewave_Comp = ESMF_GridCompCreate(name="simplewave",                  &
                                          contextflag=ESMF_CONTEXT_PARENT_VM, &
                                          rc=rc)
    call ESMF_GridCompSetServices(simplewave_comp, simplewave_SetServices, rc=rc)

    simplewave_ImportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name="simplewave_importState")
    simplewave_ExportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="simplewave_exportState")

    call ESMF_GridCompGetEPPhaseCount(simplewave_Comp,ESMF_METHOD_INITIALIZE,      &
                                                      phaseCount=phaseCount,       &
                                                      phaseZeroFlag=phaseZeroFlag)

    if (phaseZeroFlag) then
      call ESMF_GridCompInitialize(simplewave_Comp,phase=0,            &
                                   importState=simplewave_ImportState, &
                                   exportState=simplewave_ExportState, &
                                   clock=parentClock)
    end if

    do p=1,phaseCount
      call ESMF_GridCompInitialize(simplewave_Comp,phase=p,            &
                                   importState=simplewave_ImportState, &
                                   exportState=simplewave_ExportState, &
                                   clock=parentClock)
    end do

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_TRACE) 

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    type(ESMF_Field)                          :: field
    real(ESMF_KIND_R8),dimension(:,:),pointer :: farrayPtr

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_TRACE)

    call ESMF_StateGet(simplewave_ImportState,"water_depth_at_soil_surface",field)
    call ESMF_FieldGet(field,farrayPtr=farrayPtr)
    farrayPtr = 50.0
    write(*,*) "depth:",farrayPtr

    call ESMF_StateGet(simplewave_ImportState,"wind_x_velocity_at_10m",field)
    call ESMF_FieldGet(field,farrayPtr=farrayPtr)
    farrayPtr = 10.0
    write(*,*) "windx:",farrayPtr

    call ESMF_StateGet(simplewave_ImportState,"wind_y_velocity_at_10m",field)
    call ESMF_FieldGet(field,farrayPtr=farrayPtr)
    farrayPtr = 20.0
    write(*,*) "windy:",farrayPtr

    call ESMF_GridCompRun(simplewave_Comp,                    &
                          importState=simplewave_ImportState, &
                          exportState=simplewave_ExportState, &
                          clock=parentclock, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_StateGet(simplewave_ExportState,"wave_height",field)
    call ESMF_FieldGet(field,farrayPtr=farrayPtr)
    write(*,*) "wave_height:",farrayPtr

    call ESMF_StateGet(simplewave_ExportState,"wave_period",field)
    call ESMF_FieldGet(field,farrayPtr=farrayPtr)
    write(*,*) "wave_period:",farrayPtr

    call ESMF_StateGet(simplewave_ExportState,"wave_number",field)
    call ESMF_FieldGet(field,farrayPtr=farrayPtr)
    write(*,*) "wave_number:",farrayPtr

    call ESMF_StateGet(simplewave_ExportState,"wave_direction",field)
    call ESMF_FieldGet(field,farrayPtr=farrayPtr)
    write(*,*) "wave_direction:",farrayPtr

    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_TRACE)
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    call ESMF_GridCompFinalize(simplewave_Comp,importState=simplewave_ImportState,exportState=simplewave_ExportState, &
                            clock=parentclock, rc=rc)
    call ESMF_GridCompDestroy(simplewave_Comp,rc=rc)
  
    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_TRACE)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module toplevel_component
