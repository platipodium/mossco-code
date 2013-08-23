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

    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Grid)      :: grid
    type(ESMF_ArraySpec) :: arrayspec

    integer(ESMF_KIND_I4),allocatable :: deBlockList(:,:,:)
    
    call ESMF_LogWrite("Remtc Ocean component Initialized",ESMF_LOGMSG_INFO)
 
    !> Create a distribution for compute elements
    allocate( deBlockList(2,2,4) )
    deBlockList(:,1,1) = (/ 1, 1/) ! minindex 1st de-block
    deBlockList(:,2,1) = (/15,20/) ! maxindex
    deBlockList(:,1,2) = (/16, 1/) ! minindex 2nd de-block
    deBlockList(:,2,2) = (/40,12/) ! maxindex
    deBlockList(:,1,3) = (/ 1,21/) ! minindex 3rd de-block
    deBlockList(:,2,3) = (/15,40/) ! maxindex
    deBlockList(:,1,4) = (/16,13/) ! minindex 4th de-block
    deBlockList(:,2,4) = (/40,40/) ! maxindex

    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/),                     &
                                   maxIndex=(/40,40/),                   &
                                   deBlockList=deBlockList,              &
                                   indexflag=ESMF_INDEX_GLOBAL,          &
                                   rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    grid = ESMF_GridCreateNoPeriDim(maxIndex=(/40, 40/), &
                            regDecomp=(/2,2/),            &
                            coordSys=ESMF_COORDSYS_SPH_DEG,  &
                            indexflag=ESMF_INDEX_GLOBAL,  &
                            name="Ocean grid", rc=rc)

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
