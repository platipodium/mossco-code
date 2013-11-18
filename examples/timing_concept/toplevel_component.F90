module esmf_toplevel_component

  use esmf
  use empty_component, only: empty_SetServices => SetServices

  implicit none

	integer, parameter :: n = 3
  type(ESMF_GridComp),dimension(n),save  :: childComponents
  type(ESMF_State), dimension(n)         :: exportStates, importStates

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

    type(ESMF_Grid)       :: grid
    integer               :: petCount, localPet, i
	  character(ESMF_MAXSTR) :: name

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)
    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet)

    ! Create a grid
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), maxIndex=(/10,20,15/), &
      regDecomp=(/2,2,1/), name="emptyGrid", rc=rc)

    ! Create n child components, call their setservices, and create states
    do i=1,n
      write(name,'(A,I1)') 'child_component_',i
      childComponents(i)= ESMF_GridCompCreate(name=name, grid=grid, rc=rc)
      call ESMF_GridCompSetServices(childComponents(i),empty_SetServices, rc=rc)
      write(name,'(A,I1)') 'child_component_',i,'_import_state'
      importStates(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT,name=name)
      write(name,'(A,I1)') 'child_component_',i,'_export_state'
      exportStates(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT,name=name)
      call ESMF_GridCompInitialize(childComponents(i), importState=importStates(i), &
        exportState=exportStates(i), clock=parentClock, rc=rc)
    enddo
    
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
      
    enddo 

    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_INFO)
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module esmf_toplevel_component
