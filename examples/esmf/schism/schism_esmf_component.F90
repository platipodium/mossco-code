! SCHISM-ESMF component
!
! Licensed under the Apache License, Version 2.0
!  (http://www.apache.org/licenses/LICENSE-2.0)
! Author(s): Richard Hofmeister

module schism_esmf_component
use schism_driver_interfaces
use esmf
use mpi

implicit none
integer               :: iths=0,ntime=0

public SetServices

contains

  subroutine SetServices(comp,rc)
  type(ESMF_GridComp) :: comp
  integer,intent(out) :: rc

  call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
                            userRoutine=Init, rc=rc)
  call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, &
                            userRoutine=Run, rc=rc)
  call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
                            userRoutine=Finalize, rc=rc)

  rc = ESMF_SUCCESS
  end subroutine SetServices


  subroutine Init(comp, importState, exportState, clock, rc)
  use schism_glbl
  use schism_msgp, only: schism_mpi_comm=>comm
  use schism_msgp, only: parallel_init
  type(ESMF_GridComp)   :: comp
  type(ESMF_State)      :: importState
  type(ESMF_State)      :: exportState
  type(ESMF_Field)      :: field
  type(ESMF_Clock)      :: clock
  type(ESMF_VM)         :: vm
  type(ESMF_Mesh)       :: mesh2d,mesh3d
  type(ESMF_DistGrid)   :: distgrid
  integer, dimension(:), allocatable            :: nodeids,elementids,nv
  real(ESMF_KIND_R8), dimension(:), allocatable :: nodecoords2d, nodecoords3d
  real(ESMF_KIND_R8), dimension(:),pointer      :: schism_windx,schism_windy
  real(ESMF_KIND_R8), dimension(:),pointer      :: schism_ptr2d
  integer, dimension(:), allocatable            :: nodeowners, elementtypes
  integer, dimension(:), allocatable            :: nodemask, elementmask
  integer, intent(out)  :: rc
  integer               :: mpi_comm
  integer               :: i,n,nvcount

  ! get communicator
  call ESMF_GridCompGet(comp,vm=vm,rc=rc)
  call ESMF_VMGet(vm,mpiCommunicator=mpi_comm,rc=rc)

  ! initialize schism's MPI
  call parallel_init(communicator=mpi_comm)

  ! call initialize model
  call schism_init(iths,ntime)
  write(0,*) '  Initialized SCHISM'

  ! define mesh
  allocate(nodeids(npa))
  allocate(nodecoords2d(2*npa))
  allocate(nodecoords3d(3*npa))
  allocate(nodeowners(npa))
  allocate(nodemask(npa))
  allocate(elementids(nea))
  allocate(elementtypes(nea))
  allocate(elementmask(nea))
  allocate(nv(4*nea))
  do i=1,npa
    nodeids(i)=ipgl(i)%id
    nodecoords2d(2*i-1) = xlon(ipgl(i)%id) ! or lon
    nodecoords2d(2*i)   = ylat(ipgl(i)%id) ! or lat
    nodeowners(i)       = ipgl(i)%rank
    nodemask(i)         = idry(i)
  end do
  nvcount=1
  do i=1,nea
    elementids(i)=iegl(i)%id
    elementtypes(i)=i34(i)
    elementmask(i)=idry_e(i)
    nv(nvcount:nvcount+i34(i)-1) = elnode(1:i34(i),i)
    nvcount = nvcount+i34(i)
  end do
  mesh2d = ESMF_MeshCreate(parametricDim=2,spatialdim=2,nodeIds=nodeids, &
             nodeCoords=nodecoords2d,nodeOwners=nodeowners, &
             nodeMask=nodemask,elementMask=elementmask, &
             elementIds=elementids, elementTypes=elementtypes, &
             elementConn=nv(1:nvcount-1),rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! define fields for import and export
  schism_ptr2d => windx2
  field = ESMF_FieldCreate(mesh2d, name='wind_x-velocity_in_10m_height', &
                           farrayPtr=schism_ptr2d, &
                           meshloc=ESMF_MESHLOC_NODE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateAddReplace(importstate, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  schism_ptr2d => windy2
  field = ESMF_FieldCreate(mesh2d, name='wind_y-velocity_in_10m_height', &
                           farrayPtr=schism_ptr2d, &
                           meshloc=ESMF_MESHLOC_NODE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateAddReplace(importState, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  schism_ptr2d => pr2
  field = ESMF_FieldCreate(mesh2d, name='air_pressure_at_water_surface', &
                           farrayPtr=schism_ptr2d, &
                           meshloc=ESMF_MESHLOC_NODE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateAddReplace(importState, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  schism_ptr2d => airt2
  field = ESMF_FieldCreate(mesh2d, name='temperature_in_air', &
                           farrayPtr=schism_ptr2d, &
                           meshloc=ESMF_MESHLOC_NODE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateAddReplace(importState, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  schism_ptr2d => shum2
  field = ESMF_FieldCreate(mesh2d, name='specific_humidity_in_air', &
                           farrayPtr=schism_ptr2d, &
                           meshloc=ESMF_MESHLOC_NODE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateAddReplace(importState, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  schism_ptr2d => srad
  field = ESMF_FieldCreate(mesh2d, name='downwelling_short_wave_radiation_at_water_surface', &
                           farrayPtr=schism_ptr2d, &
                           meshloc=ESMF_MESHLOC_NODE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateAddReplace(importState, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  schism_ptr2d => fluxevp
  field = ESMF_FieldCreate(mesh2d, name='evaporation_flux_at_water_surface', &
                           farrayPtr=schism_ptr2d, &
                           meshloc=ESMF_MESHLOC_NODE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateAddReplace(importState, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  schism_ptr2d => fluxprc
  field = ESMF_FieldCreate(mesh2d, name='precipitation_flux_at_water_surface', &
                           farrayPtr=schism_ptr2d, &
                           meshloc=ESMF_MESHLOC_NODE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateAddReplace(importState, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! fill export state

  field = ESMF_FieldCreate(mesh2d, name='temperature_at_water_surface', &
                           typekind=ESMF_TYPEKIND_R8, &
                           meshloc=ESMF_MESHLOC_NODE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !   initialize
  call ESMF_FieldGet(field,farrayPtr=schism_ptr2d,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  schism_ptr2d(1:npa)=tr_nd(1,nvrt,1:npa)
  call ESMF_StateAddReplace(exportState, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  field = ESMF_FieldCreate(mesh2d, name='temperature_at_soil_surface', &
                           typekind=ESMF_TYPEKIND_R8, &
                           meshloc=ESMF_MESHLOC_NODE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !   initialize
  call ESMF_FieldGet(field,farrayPtr=schism_ptr2d,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  do i=1,npa
    schism_ptr2d(i) = tr_nd(1,max(1,kbp(i)),i)
  end do
  call ESMF_StateAddReplace(exportState, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! clean up
  deallocate(nodeids)
  deallocate(nodecoords2d)
  deallocate(nodeowners)
  deallocate(nodemask)
  deallocate(elementids)
  deallocate(elementmask)
  deallocate(elementtypes)
  deallocate(nv)

  rc = ESMF_SUCCESS
  end subroutine Init



  subroutine Run(comp, importState, exportState, clock, rc)
  use schism_glbl, only: dt,tr_nd,nvrt,npa,kbp,idry
  implicit none
  type(ESMF_GridComp)     :: comp
  type(ESMF_State)        :: importState
  type(ESMF_State)        :: exportState
  type(ESMF_Clock)        :: clock
  type(ESMF_Clock)        :: schism_clock
  type(ESMF_TimeInterval) :: schism_dt
  integer, intent(out)    :: rc
  integer                 :: i
  integer, save           :: it=1
  type(ESMF_Field)        :: field
  real(ESMF_KIND_R8), pointer :: ptr2d(:)

  ! get data from fields in import state
  !   here: nothing to be done for the wind velocities,
  !         since fields are created with ESMF_DATACOPY_REFERENCE

  ! run model to stopTime of clock
  schism_clock = ESMF_ClockCreate(clock)
  call ESMF_TimeIntervalSet(schism_dt,s_r8=dt,rc=rc)
  call ESMF_ClockSet(schism_clock,timeStep=schism_dt,rc=rc)

  do while (.not. ESMF_ClockIsStopTime(schism_clock, rc=rc))
    write(0,'(A5,I4,A,F0.2,A1)') 'it = ',it,', elapsed  ',it*dt,'s'
    call schism_step(it)
    call ESMF_ClockAdvance(schism_clock, rc=rc)
    it=it+1
  end do

  ! update 3d grid
  ! (so far not necessary for 2d variables to be exchanged)

  ! put data to fields in export state
  !   SST, copy uncontiguous data into field in ESMF
  call ESMF_StateGet(exportState, 'temperature_at_water_surface', field=field, rc=rc)
  call ESMF_FieldGet(field, farrayPtr=ptr2d,rc=rc)
  ptr2d(1:npa) = tr_nd(1,nvrt,1:npa)

  !   bottom temp, copy uncontiguous data into field in ESMF
  call ESMF_StateGet(exportState, 'temperature_at_soil_surface', field=field, rc=rc)
  call ESMF_FieldGet(field, farrayPtr=ptr2d,rc=rc)
  do i=1,npa
    ptr2d(i) = tr_nd(1,max(kbp(i),1),i)
  end do

  call ESMF_ClockDestroy(schism_clock)

  rc = ESMF_SUCCESS
  end subroutine Run



  subroutine Finalize(comp, importState, exportState, clock, rc)
  implicit none
  type(ESMF_GridComp)   :: comp
  type(ESMF_State)      :: importState
  type(ESMF_State)      :: exportState
  type(ESMF_Clock)      :: clock
  type(ESMF_Mesh)       :: mesh
  type(ESMF_Field)      :: field
  integer, intent(out)  :: rc

  call ESMF_StateGet(importState,'wind_x-velocity_in_10m_height',field=field,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field,mesh=mesh,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(field,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_MeshDestroy(mesh,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  rc = ESMF_SUCCESS
  end subroutine Finalize


end module schism_esmf_component
