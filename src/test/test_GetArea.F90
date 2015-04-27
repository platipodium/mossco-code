!> @file test_GetArea.F90
!! @brief checks function of FieldRegridGetArea
!! @author Richard Hofmeister
!!

program test_GetArea

use esmf

implicit none

type(ESMF_Grid)      :: grid
type(ESMF_Distgrid)  :: cornerDistgrid, centerDistgrid
type(ESMF_Field)     :: dataField, areaField
type(ESMF_ArraySpec) :: arrayspec
type(ESMF_Array)     :: cornerX, cornerY

real(ESMF_KIND_R8), dimension(:,:) , pointer:: farrayPtr2, areaPtr
integer              :: i, j, rc,  counts(2), cLBound(2), cUBound(2)
integer              :: min(2), max(2)
real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY, cornerXPtr, cornerYPtr, gridareaPtr
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

call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridAddItem(grid, itemflag=ESMF_GRIDITEM_AREA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, &
  staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=gridareaPtr, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
gridareaPtr=123456.d0

#if 0
call ESMF_GridGet(grid, distgridToGridMap=distgridToGridMap, rc=rc)
call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CORNER, &
  distgrid=cornerDistgrid)
call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)

cornerX = ESMF_ArrayCreate(cornerDistgrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
cornerY = ESMF_ArrayCreate(cornerDistgrid, typekind=ESMF_TYPEKIND_R8, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridSetCoord(grid, 1, array=cornerX, &
  staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridSetCoord(grid, 2, array=cornerY, &
  staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=cornerXPtr, localDE=0,  &
  computationalLBound=cLBound, computationalUBound=cUBound, &
  staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=cornerYPtr, &
  computationalLBound=cLBound, computationalUBound=cUBound, &
  staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

write(0,*) 'got coordinates pointers, lbnd,ubnd:',cLBound, cUBound

do j = cLBound(2), cUBound(2)
  do i = cLBound(1), cUbound(1)
    cornerXPtr(i,j) = (i-1)*dx
    cornerYPtr(i,j) = (j-1)*dy
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

!call ESMF_FieldPrint(areaField)
write(0,*) 'expect cell area(10,10)',dy*111.d0*dx*111.d0*cos(cornerYPtr(10,10)*3.14d0/360.d0)
write(0,*) 'calculated area(10,10)',areaPtr(10,10)

call ESMF_LogWrite('Cleanup', ESMF_LOGMSG_INFO, rc=rc)
call ESMF_LogFlush(rc=rc)

call ESMF_FieldDestroy(areaField, rc=rc)
call ESMF_FieldDestroy(dataField, rc=rc)

call ESMF_GridDestroy(grid, rc=rc)

call ESMF_LogFlush(rc=rc)
call ESMF_Finalize()

end program

