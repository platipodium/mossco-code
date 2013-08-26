!> This component describes a very simple 2D ocean.  It was coded from an example AO coupling
!> @author Hartmut Kapitza
!> @author Carsten Lemmen

module remtc_ocean

  use esmf

  implicit none

  real(ESMF_KIND_R8), pointer :: water_temperature_at_surface(:,:)
  real(ESMF_KIND_R8), pointer :: air_temperature_at_surface(:,:)
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
    integer                     :: lbnd(2), ubnd(2)
    integer                     :: myrank
    real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)
 
    integer(ESMF_KIND_I4),allocatable :: deBlockList(:,:,:)
    
    call ESMF_LogWrite("Remtc Ocean component initializing ...",ESMF_LOGMSG_INFO)
 
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
    grid = ESMF_GridCreate(filename="ocean_grid.nc",fileformat=ESMF_FILEFORMAT_GRIDSPEC, &
                             regDecomp=(/2,2/),isSphere=.false., rc=rc,coordNames=(/"grid_center_lon","grid_center_lat"/))
    
    call ESMF_GridGetCoord(grid, localDE=0, coordDim=1, &
                           farrayPtr=coordX, computationalLBound=lbnd, &
                           computationalUBound=ubnd, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridGetCoord(grid, localDE=0, coordDim=2, &
                           farrayPtr=coordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Setup 2D array specification
    call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Create temperature field and have it create the array internally
    temperatureField = ESMF_FieldCreate(grid, arrayspec, name="water_temperature_at_surface", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    !call ESMF_FieldGet(temperatureField, farrayPtr=air_temperature_at_surface, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    ! Create ocean variables
    allocate(water_temperature_at_surface(lbnd(1):ubnd(1),lbnd(2):ubnd(2)))
    call ESMF_LogWrite("Remtc Ocean component initialized.",ESMF_LOGMSG_INFO)

    call ESMF_StateAdd(importState,(/temperatureField/),rc=rc)
    call ESMF_FieldWrite(temperatureField, file="air_temperature_at_surface.nc", timeslice=1, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Initialize
    
 
  !> Run the component
  !!
  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer                     :: myrank, ib, ie, jb, je
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    character (len=ESMF_MAXSTR) :: message
     
    ib = lbound(water_temperature_at_surface,1)
    ie = ubound(water_temperature_at_surface,1)
    jb = lbound(water_temperature_at_surface,2)
    je = ubound(water_temperature_at_surface,2)

    call ESMF_GridCompGet(gridComp, localPet=myrank, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(parentClock, currtime=localtime, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(localtime, timeString=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    message = "Remtc ocean called at "//trim(timestring)
    call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)

! Get import state and extract arrays
    call ESMF_StateGet(importState, "air_temperature_at_surface", temperatureField, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(temperatureField, farrayPtr=air_temperature_at_surface, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Do something
    water_temperature_at_surface = water_temperature_at_surface *0.009d0 + air_temperature_at_surface * 0.001d0

! Output to netCDF files
    !print_count = print_count + 1
    !call ESMF_FieldWrite(temperatureField, file="air_temperature_at_surface.nc", timeslice=print_count, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

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

    call ESMF_LogWrite("Remtc Ocean component finalized", ESMF_LOGMSG_INFO)

  end subroutine Finalize

end module remtc_ocean
