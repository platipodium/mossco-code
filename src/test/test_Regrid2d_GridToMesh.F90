!> @file test_Regrid2d_GridToMesh.F90
!! @brief explores Regrid Grid to Mesh capability on one PET
!! @author Richard Hofmeister & Carsten Lemmen
!!

program test_Regrid2d_GridToMesh

use esmf

implicit none

type(ESMF_Grid)        :: srcGrid
type(ESMF_Mesh)        :: mesh
type(ESMF_Field)       :: srcField, dstField
type(ESMF_VM)          :: vm
type(ESMF_RouteHandle) :: routeHandle
type(ESMF_ArraySpec)   :: arraySpec

integer(ESMF_KIND_I4)  :: petCount, localPet
real(ESMF_KIND_R8), dimension(:,:), pointer :: farrayPtr2
real(ESMF_KIND_R8), dimension(:)  , pointer :: farrayPtr1
integer                :: i, j, rc,  counts(2), cLBound(2), cUBound(2)
real(ESMF_KIND_R8)     :: min(2), max(2), dx, dy
real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY

type(ESMF_Mesh)        :: dualMesh
type(ESMF_Field)       :: dualDstField
integer(ESMF_KIND_I4), pointer:: factorIndexList(:,:)
real(ESMF_KIND_R8), pointer :: factorList(:)

call ESMF_Initialize(vm=vm, defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=rc)
call ESMF_VmGet(vm, petCount=petCount, localPet=localPet, rc=rc)

call ESMF_LogFlush(rc=rc)
! Create the first grid, 50 x 40 indices between 0..60 deg E and 0..50 deg N
call ESMF_LogWrite('Creating first 50 x 40 grid', ESMF_LOGMSG_INFO, rc=rc)

counts(1) = 50
counts(2) = 40
min(1) = -5.0
max(1) = 20.0
min(2) = 40.0
max(2) = 60.0

dx = (max(1)-min(1))/(counts(1)-1)
dy = (max(2)-min(2))/(counts(2)-1)


srcGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=counts, rc=rc, &
    indexflag=ESMF_INDEX_GLOBAL, regDecomp=(/1,1/), name="source grid")
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridAddCoord(srcGrid, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(srcGrid, coordDim=1, farrayPtr=coordX, localDE=0,  &
  computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(srcGrid, localDE=0, coordDim=2, &
  farrayPtr=coordY, computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

do j = cLBound(2), cUBound(2)
  do i = cLBound(1), cUbound(1)
    coordX(i,j) = (i-1)*dx
    coordY(i,j) = (j-1)*dy
  enddo
enddo


! Create the Mesh, depending if UGRID available
#ifdef UGRID
call ESMF_LogWrite('Create Mesh from UGRID', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)
mesh = ESMF_MeshCreate(filename='ugrid_sediment.nc',filetypeflag=ESMF_FILEFORMAT_UGRID,rc=rc)
#else
call ESMF_LogWrite('Create Mesh from SCRIP', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)
mesh = ESMF_MeshCreate(filename='scrip_sediment.nc', &
       convertToDual=.false.,filetypeflag=ESMF_FILEFORMAT_SCRIP,rc=rc)
!           NOTE HERE   ^ SETTING TO FALSE, BECAUSE DEFAULT IS TRUE

#endif
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

! Create Array spec and fields for both grids
call ESMF_LogWrite('Create fields', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)

call ESMF_ArraySpecSet(arrayspec, rank=2,typekind=ESMF_TYPEKIND_R8)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
  totalLWidth=(/0,0/), totalUWidth=(/0,0/), name="srcField", rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_FieldGet(srcField, farrayPtr=farrayPtr2, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
farrayPtr2(:,:)=1.0

dstField = ESMF_FieldCreate(mesh, typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, name="dstField", rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_FieldGet(dstField, farrayPtr=farrayPtr1, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
farrayPtr1(:)=2.0

call ESMF_FieldPrint(srcField)
call ESMF_FieldPrint(dstField)


! Mesh Create from file doesn't set center coordinates right now, so can't do dual,
! thus can't do bilinear on MESHLOC_ELEMENT when using MeshCreate
! As a work around use MeshCreate from SCRIP file with convertToDual flag set.
! For historical reasons, this does things
! differently and thus doesn't require the center coordinates to be set in the Mesh.
!
! The work around is this:
! 1. Create a Dual Mesh (dualMesh) from the SCRIP file
! 2. Create a tmp Field (dualDstField) on the NODEs of the Dual Mesh (which correspond to the ELEMENTs of the non-dual Mesh)
! 3. Use tmp Field to create regridding weights
! 4. Create the Non-Dual Mesh
! 5. Create Field on the Non-Dual Mesh
! 6. Use those weights to build the RouteHandle
! 7. Clean up all the stuff from the workaround

! Create temporary dual mesh
dualMesh = ESMF_MeshCreate(filename='scrip_sediment.nc', &
       convertToDual=.true.,filetypeflag=ESMF_FILEFORMAT_SCRIP,rc=rc)

! Build a field on it
dualDstField= ESMF_FieldCreate(dualMesh, typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_NODE, &
                 name="dstField", rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

! Generate Weights
call ESMF_FieldRegridStore(srcField, dualDstField, &
     factorIndexList=factorIndexList, factorList=factorList, &
     regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

! Create routeHandle on original Mesh using weights
call ESMF_FieldSMMStore(srcField, dstField, &
     factorIndexList=factorIndexList, factorList=factorList, &
     routeHandle=routeHandle, &
     rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

! Cleanup from workaround
call ESMF_MeshDestroy(dualMesh, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_FieldDestroy(dualDstField, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

deallocate(factorIndexList)
deallocate(factorList)


call ESMF_LogWrite('Perform Regridding', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)
call ESMF_FieldRegrid(srcField, dstField, routeHandle=routeHandle, rc=rc)

call ESMF_FieldPrint(dstField)

call ESMF_LogWrite('Release handle', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)
call ESMF_FieldRegridRelease(routeHandle=routeHandle, rc=rc)

call ESMF_LogWrite('Cleanup', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)

call ESMF_FieldDestroy(srcField, rc=rc)
call ESMF_FieldDestroy(dstField, rc=rc)

call ESMF_GridDestroy(srcGrid, rc=rc)
call ESMF_MeshDestroy(mesh, rc=rc)

call ESMF_LogFlush(rc=rc)
call ESMF_Finalize()

end program
