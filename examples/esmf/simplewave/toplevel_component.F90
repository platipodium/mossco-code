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
  character(len=ESMF_MAXSTR)  :: simplewave_CompName 
  type(ESMF_State)            :: simplewave_ImportState, simplewave_ExportState
  real(ESMF_KIND_R8),dimension(:,:),allocatable,target :: wind,windDir,depth,windx,windy

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
    type(ESMF_Field)      :: exportField
    type(ESMF_Grid)       :: grid
    
    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_TRACE)

    !> Create the grid and coordinates
    !> This example grid is a 1 x 1 x 1 grid, you need to adjust this 
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/1,1/), &
      regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL, &
      name='top grid', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   

    allocate(windx(1,1))
    windx = 10.0d0
    allocate(windy(1,1))
    windy = 2.0d0
    allocate(depth(1,1))
    depth = 100.0d0

!   create child component
    simplewave_CompName = "ESMF simplewave component"
    simplewave_Comp     = ESMF_GridCompCreate(name=simplewave_CompName, contextflag=ESMF_CONTEXT_PARENT_VM,rc=rc)
    call ESMF_GridCompSetServices(simplewave_comp, simplewave_SetServices, rc=rc)

!   create import state for child component
    simplewave_ImportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name="simplewave_Import")
    simplewave_ExportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name="simplewave_export")

    exportField = ESMF_FieldCreate(grid, windx,                       &
                                   indexflag=ESMF_INDEX_GLOBAL,      &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name='wind_x_velocity_at_10m', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_StateAddReplace(simplewave_ImportState,(/exportField/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    exportField = ESMF_FieldCreate(grid, windy,                       &
                                   indexflag=ESMF_INDEX_GLOBAL,      &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name='wind_y_velocity_at_10m', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_StateAddReplace(simplewave_ImportState,(/exportField/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    exportField = ESMF_FieldCreate(grid, depth,                       &
                                   indexflag=ESMF_INDEX_GLOBAL,      &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name='water_depth_at_soil_surface', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_StateAddReplace(simplewave_ImportState,(/exportField/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)


    call ESMF_GridCompInitialize(simplewave_Comp,importState=simplewave_ImportState,exportState=simplewave_ExportState,&
      clock=parentClock,rc=rc)

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_TRACE) 

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

   type(ESMF_Field) :: field
    integer  :: myrank
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring,message

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_TRACE)
    call ESMF_GridCompGet(gridComp, localPet=myrank, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))
      call ESMF_ClockAdvance(parentClock, rc=rc)
      call ESMF_ClockGet(parentClock, currtime=localtime, rc=rc)
      call ESMF_TimeGet(localtime, timeString=timestring, rc=rc)
      message = "Toplevel ticking at "//trim(timestring)
      call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
      
      call ESMF_GridCompRun(simplewave_Comp,importState=simplewave_ImportState,&
        exportState=simplewave_ExportState,clock=parentclock, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   enddo 

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
