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
real(ESMF_KIND_R8), dimension(:,:) , pointer:: farrayPtr2
integer              :: i, j, rc,  counts(2), cLBound(2), cUBound(2)
real(ESMF_KIND_R8)   :: min(2), max(2), dx, dy
real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY

call ESMF_Initialize(vm=vm, defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=rc)
call ESMF_VmGet(vm, petCount=petCount, localPet=localPet, rc=rc)

call ESMF_LogFlush(rc=rc)
! Create the first grid, 60 x 40 indices between 0..60 deg E and 0..50 deg N
call ESMF_LogWrite('Creating first 60 x 40 grid', ESMF_LOGMSG_INFO, rc=rc)

counts(1) = 60
counts(2) = 40
min(1) = 0.0
max(1) = 60.0
min(2) = 0.0
max(2) = 50.0
  
dx = (max(1)-min(1))/(counts(1)-1)
dy = (max(2)-min(2))/(counts(2)-1)

    
srcGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=counts, &
    gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), rc=rc, &
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

! Create the second grid, 80 x 30 indices, same domain as srcGrid
call ESMF_LogWrite('Creating second 80 x 30 grid', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)

counts(1) = 80
counts(2) = 30
dx = (max(1)-min(1))/(counts(1)-1)
dy = (max(2)-min(2))/(counts(2)-1)

dstGrid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=counts, &
    gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
    indexflag=ESMF_INDEX_GLOBAL, &
    regDecomp=(/1,1/), name="destination grid", rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
call ESMF_GridAddCoord(dstGrid, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(dstGrid, localDE=0, coordDim=1, &
  farrayPtr=coordX, computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(dstGrid, localDE=0, coordDim=2, &
  farrayPtr=coordY, computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

do j = cLBound(2), cUBound(2)
  do i = cLBound(1), cUbound(1)
    coordX(i,j) = (i-1)*dx
    coordY(i,j) = (j-1)*dy
  enddo
enddo

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

dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
  totalLWidth=(/0,0/), totalUWidth=(/0,0/), name="dstField", rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_FieldGet(dstField, farrayPtr=farrayPtr2, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
farrayPtr2(:,:)=2.0

call ESMF_FieldPrint(srcField)
call ESMF_FieldPrint(dstField)

call ESMF_LogWrite('Create regrid store', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)
call ESMF_FieldRegridStore(srcField, dstField, routeHandle=routeHandle, &
  regridmethod=ESMF_REGRIDMETHOD_BILINEAR, rc=rc)

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
call ESMF_GridDestroy(dstGrid, rc=rc)

call ESMF_LogFlush(rc=rc)
call ESMF_Finalize()

end program

