!> This component describes a very simple 2D ocean.  It was coded from an example AO coupling
!> @author Hartmut Kapitza
!> @author Carsten Lemmen

module remtc_ocean

  use esmf

  implicit none

  real(ESMF_KIND_R8), pointer :: water_temperature(:,:)
  type(ESMF_Field)            :: temperatureField

  public remtc_ocean_SetServices
 
  contains

  subroutine remtc_ocean_SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine remtc_ocean_SetServices

  !> Initialize the component
  !!
  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

  end subroutine Initialize

 
  !> Run the component
  !!
  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

  end subroutine Run

  !> Finalize the component
  !!
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

  end subroutine Finalize

end module remtc_ocean
