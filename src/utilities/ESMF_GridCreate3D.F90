!> @brief ESMF_GridCreate3D utility routines
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Ryan O'Kuinghttons
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!

#ifdef _ESMF_UNRELEASED_

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "ESMF_GridCreate3D.F90"

module esmfgridcreate3D

!INITIALIZATION
use esmf

implicit none

public ESMF_GridCreate

interface ESMF_GridCreate
  module procedure ESMF_GridCreateFrmGrid
  module procedure ESMF_GridCreateFrmField
end interface

contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateFrmGrid"
!> @subsubsection ESMF_GridCreateFrmGrid "Grid Create From Grid"
!> @brief Create a 3D Grid from a 2D Grid and an undistributed vertical dimension
!> @param equivalent: 
!> @param rulesets: 
!> @param nameout: 


!BOP
! !IROUTINE: ESMF_GridCreateFrmGrid - Create a 3D Grid from a 2D Grid by adding
! an undistributed vertical dimension

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate() ?
  function ESMF_GridCreateFrmGrid(grid, minIndex, maxIndex, rc)
!
! !RETURN VALUE:
    type(ESMF_Grid) :: ESMF_GridCreateFrmGrid
!
! !ARGUMENTS:
    type(ESMF_Grid), intent(in)            :: grid
    integer,         intent(in),  optional :: minIndex
    integer,         intent(in)            :: maxIndex
    integer,         intent(out), optional :: rc
!
! !DESCRIPTION:
!
! This function creates a new 3D {\tt ESMF\_Grid} object by adding an
! undistributed vertical dimension to an existing 2D {\tt ESMF\_Grid} object.
! A maximum and, optionally, a minimum value are provided by the user for
! the vertical dimension index. The coordinates from the input 2D grid are
! included in the final 3D grid, while values for the undistributed vertical
! coordinate must be set subsequently.
!
! The arguments are:
! \begin{description}
! \item[grid]
!     The original 2D {\tt ESMF\_Grid} object.
! \item[{[minIndex]}]
!     Minimum index value for the undistributed vertical dimension. If omitted,
!     defaults to 1.
! \item[maxIndex]
!     Maximum index value for the undistributed vertical dimension.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

    ! local variables
    integer :: localrc
    integer :: connectionCount, deCount, dimCount, itemCount, tileCount
    integer :: ldimCount, localDe, localDeCount, minIndx
    integer :: item
    integer :: tileIndexA, tileIndexB
    integer, dimension(:),       pointer :: positionVector,    orientationVector
    integer, dimension(:),       pointer :: newPositionVector, newOrientationVector
    integer, dimension(:),   allocatable :: coordDimCount,    distgridToGridMap
    integer, dimension(:),   allocatable :: newcoordDimCount, newdistgridToGridMap
    integer, dimension(:,:), allocatable :: coordDimMap,    minIndexPTile,    maxIndexPTile
    integer, dimension(:,:), allocatable :: newcoordDimMap, newminIndexPTile, newmaxIndexPTile
    real(ESMF_KIND_R8), dimension(:),     pointer :: fptrIn1d, fptrOut1d
    real(ESMF_KIND_R8), dimension(:,:),   pointer :: fptrIn2d, fptrOut2d
    type(ESMF_DistGridConnection), dimension(:), allocatable :: connectionList, newconnectionList
    type(ESMF_DistGrid)         :: distgrid, newdistgrid
    type(ESMF_Grid)             :: newgrid
    type(ESMF_Index_Flag)       :: indexflag
    type(ESMF_CoordSys_Flag)    :: coordSys

    ! begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! check additional dimension bounds
    minIndx = 1
    if (present(minIndex)) then
      minIndx = minIndex
    end if

    if (maxIndex <= minIndx) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="maxIndex must be > minIndex", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    end if

    ! get grid parameters and associated DistGrid object
    call ESMF_GridGet(grid, distgrid=distgrid, &
      dimCount=dimCount, coordSys=coordSys, indexflag=indexflag, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (dimCount /= 2) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="Grid object in field MUST have 2 dimensions", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    end if

    ! get 2D distribution information from Grid's DistGrid object
    allocate(coordDimCount(dimCount),  &
      distgridToGridMap(dimCount),     &
      coordDimMap(dimCount,dimCount), &
      stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridGet(grid, coordDimCount=coordDimCount, &
      distgridToGridMap=distgridToGridMap, &
      coordDimMap=coordDimMap, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! increment dimension count by one to build local 3D Grid
    ldimCount = dimCount + 1

    ! create mapping arrays for 3D Grid by extending original ones from 2D Grid
    allocate(newcoordDimCount(ldimCount),  &
      newdistgridToGridMap(ldimCount),     &
      newcoordDimMap(ldimCount,ldimCount), &
      stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    newcoordDimCount(1:dimCount)    = coordDimCount
    newcoordDimCount(ldimCount)     = 3

    newdistgridToGridMap(1:dimCount) = distgridToGridMap
    newdistgridToGridMap(ldimCount)  = 3

    newcoordDimMap(1:dimCount,1:dimCount) = coordDimMap
    newcoordDimMap(:, ldimCount) = 1
    newcoordDimMap(ldimCount, :) = (/ 1, 2, 3 /)

    deallocate(coordDimCount, distgridToGridMap, coordDimMap, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! in a similar fashion, extend index/tile arrays and connection settings
    ! for DistGrid object in new 3D Grid

    ! get original DistGrid information
    call ESMF_DistGridGet(distgrid, &
      tileCount=tileCount, connectionCount=connectionCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    allocate(minIndexPTile(dimCount, tileCount), &
             maxIndexPTile(dimCount, tileCount), &
             connectionList(connectionCount),    &
             stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! get original index arrays and connection list
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
      maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! create new index arrays
    allocate(newminIndexPTile(ldimCount, tileCount), &
             newmaxIndexPTile(ldimCount, tileCount), &
             stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    newminIndexPTile(1:dimCount,:) = minIndexPTile
    newmaxIndexPTile(1:dimCount,:) = maxIndexPTile
    newminIndexPTile(ldimCount, :) = minIndx
    newmaxIndexPTile(ldimCount, :) = maxIndex

    deallocate(minIndexPTile, maxIndexPTile, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! extend connection list for new Grid
    allocate(newConnectionList(connectionCount), &
             newPositionVector(ldimCount), newOrientationVector(ldimCount), &
             positionVector(dimCount), orientationVector(dimCount), &
             stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    do item = 1, connectionCount
      call ESMF_DistGridConnectionGet(connectionList(item), &
        tileIndexA=tileIndexA, tileIndexB=tileIndexB, &
        positionVector=positionVector, orientationVector=orientationVector, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      newPositionVector(1:dimCount) = positionVector
      newPositionVector( ldimCount) = 0
      newOrientationVector(1:dimCount) = orientationVector
      newOrientationVector( ldimCount) = 3

      call ESMF_DistGridConnectionSet(newConnectionList(item), &
        tileIndexA=tileIndexA, tileIndexB=tileIndexB, &
        positionVector=newPositionVector, orientationVector=newOrientationVector, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    end do

    deallocate(newPositionVector, newOrientationVector, &
      positionVector, orientationVector, connectionList, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! create 3D DistGrid object
    newdistgrid = ESMF_DistGridCreate(minIndexPTile=newminIndexPTile, &
      maxIndexPTile=newmaxIndexPTile, connectionList=newConnectionList, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    deallocate(newminIndexPTile, newmaxIndexPTile, newconnectionList, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! create 3D Grid object
    newgrid = ESMF_GridCreate(newdistgrid, coordDimCount=newcoordDimCount, &
      coordDimMap=newcoordDimMap, coordSys=coordSys, indexflag=indexflag, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    deallocate(newcoordDimMap, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! get localDeCount
    call ESMF_GridGet(newgrid, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! add coordinates to 3D Grid
    call ESMF_GridAddCoord(newgrid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! load 2D coordinates
    do item = 1, 2
      select case (newcoordDimCount(item))
        case (1)
          do localDe = 0, localDeCount - 1
            call ESMF_GridGetCoord(grid, coordDim=item, localDE=localDe, &
              farrayPtr=fptrOut1d, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_GridGetCoord(newgrid, coordDim=item, localDE=localDe, &
              farrayPtr=fptrIn1d, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            fptrIn1d = fptrOut1d
          end do
        case (2)
          do localDe = 0, localDeCount - 1
            call ESMF_GridGetCoord(grid, coordDim=item, localDE=localDe, &
              farrayPtr=fptrOut2d, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_GridGetCoord(newgrid, coordDim=item, localDE=localDe, &
              farrayPtr=fptrIn2d, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            fptrIn2d = fptrOut2d
          end do
        case default
          call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
            msg="Internal error - should never get here!", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
      end select
    end do

    deallocate(newcoordDimCount, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_GridCreateFrmGrid = newgrid

end function ESMF_GridCreateFrmGrid


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateFrmField"
!> @subsubsection ESMF_GridCreateFrmField "Grid Create From Field"
!> @brief Create a 3D Grid from a 2D Grid and a Field with 1 ungridded dimension
!> @param equivalent: 
!> @param rulesets: 
!> @param nameout: 


!BOPI
! !IROUTINE: ESMF_GridCreateFrmField - Create a 3D Grid from a Field defined on
! a 2D Grid with an ungridded dimension

! !INTERFACE:
  ! Private name; call using ESMF_GridCreate() ?
  ! NOTE: requires ESMF_GeomBaseMod, ESMF_FieldMod
  function ESMF_GridCreateFrmField(field, scale, offset, rc)
!
! !RETURN VALUE:
    type(ESMF_Grid) :: ESMF_GridCreateFrmField
!
! !ARGUMENTS:
    type(ESMF_Field),      intent(in)            :: field
    real(ESMF_KIND_R8),    intent(in),  optional :: scale
    real(ESMF_KIND_R8),    intent(in),  optional :: offset
    integer,               intent(out), optional :: rc
!
! !DESCRIPTION:
!
! This function creates a new 3D {\tt ESMF\_Grid} object from an
! {\tt ESMF\_Field} object defined on a 2D {\tt ESMF\_Grid}. The input
! {\tt field} must have an ungridded vertical dimension, which is added
! to the associated 2D grid to create a the new 3D grid with an undistributed
! vertical coordinate. Field values can be linearly transformed before
! being used as vertical coordinates in the 3D grid. To this purpose,
! the optional arguments {\tt scale} and {\tt offset} can be used,
! according the the formula:
! \begin{equation}
!   \vec f' = \texttt{(scale)}\,\vec f + \texttt{offset}.
! \end{equation}
! This is the most general method to create a 3D grid from an existing 2D grid,
! since each horizontal location can be assigned a given set of values for the
! vertical coordinate, which is specified by the values of the {\tt ESMF\_Field}
! object.
!
! The arguments are:
! \begin{description}
! \item[field]
!     {\tt ESMF\_Field} object defined on a 2D {\tt ESMF\_Grid} object with an
!     ungridded vertical dimension.
! \item[{[scale]}]
!     Scale factor to apply to the Field values.
! \item[{[offset]}]
!     Offset to apply to the Field values.
! \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI

    ! local variables
    integer :: localrc
    integer :: localDe, localDeCount
    integer :: item, itemCount
    integer :: ungriddedBound, vSize
    real(ESMF_KIND_R8)          :: scale_factor, add_offset
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: fptrIn3d, fptrOut3d
    type(ESMF_Grid)             :: grid, newgrid
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_GeomType_Flag)    :: geomtype

    character(len=*), dimension(2), parameter :: &
      AttributeList = (/ "UngriddedLBound", "UngriddedUBound" /)

    ! begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! check if field is completed
    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="Field has not been completely created.", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    end if

    ! check if field contains valid grid object
    call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (geomtype /= ESMF_GEOMTYPE_GRID) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="No Grid object found in field ", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    end if

    ! check if field has ungridded dimension
    vSize = 0
    do item = 1, 2
      call ESMF_AttributeGet(field, name=trim(AttributeList(item)), &
        convention="NUOPC", purpose="Instance", &
        itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (itemCount == 1) then
        call ESMF_AttributeGet(field, name=trim(AttributeList(item)), &
          convention="NUOPC", purpose="Instance", &
          value=ungriddedBound, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        vSize = ungriddedBound - vSize
      else
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="Field must have ONE ungridded dimension!", &
          ESMF_CONTEXT, rcToReturn=rc)
          return
       end if
     end do
     vSize = vSize + 1

    ! create 3D grid from field's 2D grid and vertical dimension
    ! get original 2D grid from field
    call ESMF_FieldGet(field, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! add vertical dimension
    newgrid = ESMF_GridCreateFrmGrid(grid, minIndex=1, maxIndex=vSize, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! get new grid's localDeCount
    call ESMF_GridGet(newgrid, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! -- load vertical coordinate
    scale_factor = 1._ESMF_KIND_R8
    add_offset   = 0._ESMF_KIND_R8
    if (present(scale)) scale_factor = scale
    if (present(offset)) add_offset  = offset

    do localDe = 0, localDeCount - 1

      call ESMF_FieldGet(field, localDE=localDe, farrayPtr=fptrOut3d, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_GridGetCoord(newgrid, coordDim=3, localDE=localDe, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        farrayPtr=fptrIn3d, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      fptrIn3d = scale_factor * fptrOut3d + add_offset

    end do

    ESMF_GridCreateFrmField = newgrid

  end function ESMF_GridCreateFrmField

end module esmfgridcreate3D
#endif
