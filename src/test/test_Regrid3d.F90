!> @file test_Regrid.F90
!! @brief explores Regrid capability on one PET
!! @author Carsten Lemmen
!!

program test_Regrid

use esmf

implicit none

type(ESMF_Grid)      :: srcGrid, dstGrid
type(ESMF_Field)     :: srcField, dstField
type(ESMF_VM)        :: vm
type(ESMF_RouteHandle) :: routeHandle
type(ESMF_ArraySpec) :: arraySpec

integer(ESMF_KIND_I4)  :: petCount, localPet
real(ESMF_KIND_R8), dimension(:,:,:) , pointer:: farrayPtr3
integer              :: i, j, k, rc,  counts(3), cLBound(3), cUBound(3)
real(ESMF_KIND_R8)   :: amin(3), amax(3), dx, dy, dz
real(ESMF_KIND_R8), dimension(:,:,:), pointer :: coordX, coordY, coordZ

call ESMF_Initialize(vm=vm, defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=rc)
call ESMF_VmGet(vm, petCount=petCount, localPet=localPet, rc=rc)

call ESMF_LogFlush(rc=rc)
! Create the first grid, 60 x 40 * 10 indices between 0..60 deg E and 0..50 deg N
! and for depths of 0..1500 m
call ESMF_LogWrite('Creating first 60 x 40 x 10 grid', ESMF_LOGMSG_INFO, rc=rc)

counts(1) = 60
counts(2) = 40
counts(3) = 10
amin(1) = 0.0
amax(1) = 60.0
amin(2) = 0.0
amax(2) = 50.0
amin(3) = 0.0
amax(3) = 1500.
  
dx = (amax(1)-amin(1))/(counts(1)-1)
dy = (amax(2)-amin(2))/(counts(2)-1)
dz = (amax(3)-amin(3))/(counts(3)-1)

    
srcGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), maxIndex=counts, &
    gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), rc=rc, &
    indexflag=ESMF_INDEX_GLOBAL, regDecomp=(/1,1,1/), name="source grid", coordsys=ESMF_COORDSYS_CART)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
call ESMF_GridAddCoord(srcGrid, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(srcGrid, coordDim=1, farrayPtr=coordX, localDE=0,  &
  computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(srcGrid, localDE=0, coordDim=2, &
  farrayPtr=coordY, computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(srcGrid, localDE=0, coordDim=3, &
  farrayPtr=coordZ, computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

do k = cLBound(3), cUBound(3)
  do j = cLBound(2), cUBound(2)
    do i = cLBound(1), cUbound(1)
      coordX(i,j,k) = amin(1)+(i-1)*dx
      coordY(i,j,k) = amin(2)+(j-1)*dy
      coordZ(i,j,k) = amin(3)+(k-1)*dz
    enddo
  enddo
enddo
write(0,*) 'src coordmin',coordX(1,1,1),coordY(1,1,1),coordZ(1,1,1)
write(0,*) 'src coordmax',coordX(60,40,10),coordY(60,40,10),coordZ(60,40,10)

! Create the second grid, 80 x 30 x 20 indices, same domain as srcGrid
call ESMF_LogWrite('Creating second 80 x 30 x 20 grid', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)

counts(1) = 80
counts(2) = 30
counts(3) = 20
dx = (amax(1)-amin(1))/(counts(1)+1)
dy = (amax(2)-amin(2))/(counts(2)+1)
dz = (amax(3)-amin(3))/(counts(3)+1)

dstGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), maxIndex=counts, &
    gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
    indexflag=ESMF_INDEX_GLOBAL, &
    regDecomp=(/1,1,1/), name="destination grid", rc=rc, coordsys=ESMF_COORDSYS_CART)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
call ESMF_GridAddCoord(dstGrid, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(dstGrid, localDE=0, coordDim=1, &
  farrayPtr=coordX, computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(dstGrid, localDE=0, coordDim=2, &
  farrayPtr=coordY, computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(dstGrid, localDE=0, coordDim=3, &
  farrayPtr=coordZ, computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

do k = cLBound(3), cUBound(3)
  do j = cLBound(2), cUBound(2)
    do i = cLBound(1), cUbound(1)
      coordX(i,j,k) = amin(1)+i*dx
      coordY(i,j,k) = amin(2)+j*dy
      coordZ(i,j,k) = amin(3)+k*dz
    end do
  enddo
enddo

write(0,*) 'dst coordmin',coordX(1,1,1),coordY(1,1,1),coordZ(1,1,1)
write(0,*) 'dst coordmax',coordX(80,30,20),coordY(80,30,20),coordZ(80,30,20)

! Create Array spec and fields for both grids
call ESMF_LogWrite('Create fields', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)

call ESMF_ArraySpecSet(arrayspec, rank=3,typekind=ESMF_TYPEKIND_R8)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
  totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), name="srcField", rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_FieldGet(srcField, farrayPtr=farrayPtr3, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
farrayPtr3(:,:,:)=1.0

dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
  totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), name="dstField", rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_FieldGet(dstField, farrayPtr=farrayPtr3, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
farrayPtr3(:,:,:)=2.0

call ESMF_FieldPrint(srcField)
call ESMF_FieldPrint(dstField)

call ESMF_LogWrite('Create regrid store', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)
call ESMF_FieldRegridStore(srcField, dstField, routeHandle=routeHandle, &
  regridmethod=ESMF_REGRIDMETHOD_BILINEAR, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_LogWrite('Perform Regridding', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)
call ESMF_FieldRegrid(srcField, dstField, routeHandle=routeHandle, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_FieldPrint(dstField)

call ESMF_LogWrite('Release handle', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)
call ESMF_FieldRegridRelease(routeHandle=routeHandle, rc=rc)

call ESMF_LogWrite('Cleanup', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)

call ESMF_FieldDestroy(srcField, rc=rc)
call ESMF_FieldDestroy(dstField, rc=rc)

call ESMF_GridDestroy(srcGrid, rc=rc)
call ESMF_GridDestroy(dstGrid, rc=rc)

call ESMF_LogFlush(rc=rc)
call ESMF_Finalize()

end program

