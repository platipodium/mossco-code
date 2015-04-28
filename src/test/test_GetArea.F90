!> @brief Test program for FieldRegridGetArea
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2015 Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "test_GetArea.F90"


#undef  ESMF_METHOD
#define ESMF_METHOD "main"
program test_GetArea

  use esmf

  implicit none

  type(ESMF_Grid)      :: grid
  type(ESMF_Distgrid)  :: cornerDistgrid, centerDistgrid
  type(ESMF_Field)     :: dataField, areaField
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_Array)     :: cornerX, cornerY

  real(ESMF_KIND_R8), dimension(:,:) , pointer:: farrayPtr2
  integer              :: i, j, rc, localrc, counts(2), cLBound(2), cUBound(2)
  integer              :: min(2), max(2)
  real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY, cornerXPtr, cornerYPtr
  real(ESMF_KIND_R8)   :: dx, dy
  integer(ESMF_KIND_I4) :: maxIndex(2), minIndex(2), localPet, petCount, gridDims(2)
  real(ESMF_KIND_R8), allocatable :: area(:,:)
  type(ESMF_Vm)        :: vm

  call ESMF_Initialize(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get information about parallel context
  call ESMF_VMGetCurrent(vm, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create grid, 60 x 40 indices between 0..60 deg E and 0..50 deg N
  call ESMF_LogWrite('Creating first 60 x 40 grid', ESMF_LOGMSG_INFO, rc=localrc)

  counts = (/60, 40/)
  min    = (/0, 0/)
  max    = (/60, 50/)

  dx = (max(1)-min(1))/(counts(1)-1)
  dy = (max(2)-min(2))/(counts(2)-1)

  grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=counts, &
    gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), rc=localrc, &
    indexflag=ESMF_INDEX_GLOBAL, regDecomp=(/1,1/), name="grid")
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Add area item to the grid and set a default value for the area

  call ESMF_GridAddItem(grid, itemflag=ESMF_GRIDITEM_AREA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, &
    staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=farrayPtr2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  farrayPtr2=123456.d0

! #if 0
! call ESMF_GridGet(grid, distgridToGridMap=distgridToGridMap, rc=localrc)
! call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CORNER, &
!   distgrid=cornerDistgrid)
!
! cornerX = ESMF_ArrayCreate(cornerDistgrid, typekind=ESMF_TYPEKIND_R8, rc=localrc)
! if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
! cornerY = ESMF_ArrayCreate(cornerDistgrid, typekind=ESMF_TYPEKIND_R8, rc=localrc)
! if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
! call ESMF_GridSetCoord(grid, 1, array=cornerX, &
!   staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
! if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
! call ESMF_GridSetCoord(grid, 2, array=cornerY, &
!   staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
! if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
! #endif

  ! Add coordinates
  call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=cornerXPtr, localDE=0,  &
    computationalLBound=cLBound, computationalUBound=cUBound, &
    staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=cornerYPtr, &
    computationalLBound=cLBound, computationalUBound=cUBound, &
    staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  write(0,*) 'got coordinates pointers, lbnd,ubnd:',cLBound, cUBound

  do j = cLBound(2), cUBound(2)
    do i = cLBound(1), cUbound(1)
      cornerXPtr(i,j) = (i-1)*dx
      cornerYPtr(i,j) = (j-1)*dy
    enddo
  enddo

  ! Create fields for data and area
  call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogWrite('Create fields', ESMF_LOGMSG_INFO, rc=localrc)
  call ESMF_LogFlush(rc=localrc)

  dataField = ESMF_FieldCreate(grid, arraySpec, staggerloc=ESMF_STAGGERLOC_CENTER, &
    name="data", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(dataField, farrayPtr=farrayPtr2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  farrayPtr2(:,:)=1.0
  call ESMF_LogWrite('Created and set data field', ESMF_LOGMSG_INFO, rc=localrc)
  call ESMF_LogFlush(rc=localrc)

  ! Get size of Grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
  minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  gridDims(1)=maxIndex(1)-minIndex(1)+1
  gridDims(2)=maxIndex(2)-minIndex(2)+1
  call ESMF_LogWrite('Obtained grid width', ESMF_LOGMSG_INFO, rc=localrc)
  call ESMF_LogFlush(rc=localrc)

  areaField = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, &
    name="area", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(areaField, farrayPtr=farrayPtr2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  farrayPtr2=987654.321

  call ESMF_LogWrite('Created area field', ESMF_LOGMSG_INFO, rc=localrc)
  call ESMF_LogFlush(rc=localrc)

  ! Get the local area, create a global farray and gather it there
  call ESMF_FieldRegridGetArea(areaField, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
 call ESMF_LogWrite('Called RegridGetArea', ESMF_LOGMSG_INFO, rc=localrc)
  call ESMF_LogFlush(rc=localrc)

  if (.not.allocated(area)) allocate(area(gridDims(1),gridDims(2)))

  call ESMF_FieldGather(areaField, farray=area, rootPet=0, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LogWrite('Called FieldGather', ESMF_LOGMSG_INFO, rc=localrc)
  call ESMF_LogFlush(rc=localrc)

  write(0,*) 'expect cell area(10,10)',dy*111.d0*dx*111.d0*cos(cornerYPtr(10,10)*3.14d0/360.d0)
  write(0,*) 'calculated area(10,10)',area(10,10)

  call ESMF_LogWrite('Cleanup', ESMF_LOGMSG_INFO, rc=localrc)
  call ESMF_LogFlush(rc=localrc)

  call ESMF_FieldDestroy(areaField, rc=localrc)
  call ESMF_FieldDestroy(dataField, rc=localrc)

  call ESMF_GridDestroy(grid, rc=localrc)

  call ESMF_LogFlush(rc=localrc)
  call ESMF_Finalize()

end program

