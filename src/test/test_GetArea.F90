!> @file test_GetArea.F90
!! @brief checks function of FieldRegridGetArea
!! @author Richard Hofmeister
!!

program test_GetArea

use esmf

implicit none

type(ESMF_Grid)      :: grid
type(ESMF_Field)     :: dataField, areaField

real(ESMF_KIND_R8), dimension(:,:) , pointer:: farrayPtr2, areaPtr
integer              :: i, j, rc,  counts(2), cLBound(2), cUBound(2)
integer              :: min(2), max(2)
real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY, cornerX, cornerY
real(ESMF_KIND_R8)   :: dx, dy

call ESMF_Initialize(rc=rc)

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

    
grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=counts, &
    gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), rc=rc, &
    indexflag=ESMF_INDEX_GLOBAL, regDecomp=(/1,1/), name="source grid")
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
call ESMF_GridAddCoord(grid, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=coordX, localDE=0,  &
  computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(grid, localDE=0, coordDim=2, &
  farrayPtr=coordY, computationalLBound=cLBound, computationalUBound=cUBound, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

do j = cLBound(2), cUBound(2)
  do i = cLBound(1), cUbound(1)
    coordX(i,j) = (i-1)*dx
    coordY(i,j) = (j-1)*dy
  enddo
enddo

! Create fields for data an area
call ESMF_LogWrite('Create fields', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)

dataField = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
  totalLWidth=(/0,0/), totalUWidth=(/0,0/), name="dataField", rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_FieldGet(dataField, farrayPtr=farrayPtr2, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
farrayPtr2(:,:)=1.0

areaField = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
  totalLWidth=(/0,0/), totalUWidth=(/0,0/), name="areaField", rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_FieldRegridGetArea(areaField, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_FieldGet(areaField, farrayPtr=areaPtr, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_FieldPrint(dataField)
call ESMF_FieldPrint(areaField)

call ESMF_LogWrite('Cleanup', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)

call ESMF_FieldDestroy(areaField, rc=rc)
call ESMF_FieldDestroy(dataField, rc=rc)

call ESMF_GridDestroy(grid, rc=rc)

call ESMF_LogFlush(rc=rc)
call ESMF_Finalize()

end program

