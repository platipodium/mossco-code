!> This component describes a very simple 2D atmosphere as a boundary condition.  A simple field
!> "air_temperature_at_surface" is provided.  It was coded from an example AO coupling
!> @author Hartmut Kapitza
!> @author Carsten Lemmen

module remtc_atmosphere

  use esmf

  implicit none

  real(ESMF_KIND_R8), pointer :: air_temperature_at_surface(:,:)
  type(ESMF_Field)            :: temperatureField

  public remtc_atmosphere_SetServices
 
  contains

  subroutine remtc_atmosphere_SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine remtc_atmosphere_SetServices

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
    integer                     :: lbnd(2), ubnd(2)
    integer                     :: myrank,i,j
    real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY
 
    call ESMF_LogWrite("Remtc Atmosphere component initializing ...",ESMF_LOGMSG_INFO)
 
    !> Create the grid and coordinates
    !> This example grid is a 40 x 40 grid at 0.1 degree resolution from 0..4 deg East
    !> to 50 .. 55 deg North
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/40, 50/), &
      regDecomp=(/2,2/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="atmosphere grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX, rc=rc)
    do i=lbnd(1),ubnd(1) 
      coordX(i) = 0 + 0.1 * i + 0.05
    enddo
    call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=rc)
    do i=lbnd(1),ubnd(1) 
      coordY(i) = 50 + 0.1 * i + 0.05
    enddo
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
    !> Create a air temperature field with a 2D array specification, fill the temperature
    !> field with some values, add the field to the atmosphere's import and export states
    call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    temperatureField = ESMF_FieldCreate(grid, arrayspec, name="air_temperature_at_surface", &
      staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(temperatureField, farrayPtr=air_temperature_at_surface,totalLBound=lbnd,&
      totalUBound=ubnd,localDE=0,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    do i=lbnd(1),ubnd(1) 
      do j=lbnd(2),ubnd(2)
        air_temperature_at_surface =  15.0D0
      enddo
    enddo
    call ESMF_StateAdd(exportState,(/temperatureField/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_LogWrite("Remtc Atmosphere component initialized.",ESMF_LOGMSG_INFO)
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

    call ESMF_FieldDestroy(temperatureField, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_LogWrite("Remtc Atmosphere component finalized", ESMF_LOGMSG_INFO)

  end subroutine Finalize

end module remtc_atmosphere
