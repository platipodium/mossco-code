!> @brief Implementation of grid utilities
!!
!! This computer program is part of MOSSCO.
!! @copyright Copyright 2014, Helmholtz-Zentrum Geesthacht
!! @author Carsten Lemmen, HZG
!! @author Hartmut Kapitza, HZG

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "mossco_grid.F90"

module mossco_grid

  use esmf

  implicit none

  public MOSSCO_GridCopyCoords

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCreateRegional3D"
function MOSSCO_GridCreateRegional3D(name, rc) result(grid)

	character(ESMF_MAXSTR), intent(in) :: name
	integer,  intent(out)              :: rc
	type(ESMF_Grid)                    :: grid

	integer(ESMF_KIND_I4)      :: minIndex(3), maxIndex(3), regDecomp(3)
	type(ESMF_Index_Flag)      :: indexFlag
	type(ESMF_CoordSys_Flag)   :: coordSys
	integer                    :: localrc, i, lbnd(3), ubnd(3)
  real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY

	rc = ESMF_SUCCESS

	minIndex=(/1,1,1/)
	maxIndex=(/40,50,10/)
	regDecomp=(/4,3,3/)
	coordSys=ESMF_COORDSYS_SPH_DEG
	indexFlag=ESMF_INDEX_GLOBAL

  grid = ESMF_GridCreateNoPeriDim(minIndex=minIndex, maxIndex=maxIndex, &
    regDecomp=regDecomp, coordSys=coordSys, indexFlag=indexFlag,  &
    name=trim(name)//' grid', coordTypeKind=ESMF_TYPEKIND_R8, coordDep1=(/1/), &
    coorddep2=(/2/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !> This example grid is a 40 x 50 grid at 0.1 degree resolution from 0..4 deg East
  !> to 50 .. 55 deg North
  call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
    computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX, rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  do i=lbnd(1),ubnd(1)
    coordX(i) = 0 + 0.1 * i + 0.05
  enddo
  call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
    computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  do i=lbnd(1),ubnd(1)
    coordY(i) = 50 + 0.1 * i + 0.05
  enddo
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  return

end function MOSSCO_GridCreateRegional3D

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCreateRegional2D"
function MOSSCO_GridCreateRegional2D(name, rc) result(grid)

	character(ESMF_MAXSTR), intent(in) :: name
	integer,  intent(out)              :: rc
	type(ESMF_Grid)                    :: grid

	integer(ESMF_KIND_I4)      :: minIndex(2), maxIndex(2), regDecomp(2)
	type(ESMF_Index_Flag)      :: indexFlag
	type(ESMF_CoordSys_Flag)   :: coordSys
	integer                    :: localrc, i, lbnd(2), ubnd(2)
  real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY

	rc = ESMF_SUCCESS

	minIndex=(/1,1/)
	maxIndex=(/40,50/)
	regDecomp=(/4,3/)
	coordSys=ESMF_COORDSYS_SPH_DEG
	indexFlag=ESMF_INDEX_GLOBAL

  grid = ESMF_GridCreateNoPeriDim(minIndex=minIndex, maxIndex=maxIndex, &
    regDecomp=regDecomp, coordSys=coordSys, indexFlag=indexFlag,  &
    name=trim(name)//' grid', coordTypeKind=ESMF_TYPEKIND_R8, coordDep1=(/1/), &
    coorddep2=(/2/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !> This example grid is a 40 x 50 grid at 0.1 degree resolution from 0..4 deg East
  !> to 50 .. 55 deg North
  call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
    computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX, rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  do i=lbnd(1),ubnd(1)
    coordX(i) = 0 + 0.1 * i + 0.05
  enddo
  call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
    computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  do i=lbnd(1),ubnd(1)
    coordY(i) = 50 + 0.1 * i + 0.05
  enddo
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  return

end function MOSSCO_GridCreateRegional2D



  subroutine MOSSCO_GridCopyFromForeignField(gridComp, state, gridfieldName, grida, gridb, rc)

    use mossco_strings

    implicit none

    type(ESMF_GridComp), intent(in)           :: gridComp
    type(ESMF_State), intent(in)              :: state
    character(len=*), intent(in)              :: gridFieldName
    type(ESMF_Grid), intent(out), optional    :: grida
    type(ESMF_Grid), intent(out), optional    :: gridb
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    type(ESMF_Grid)                           :: grida_, gridb_
    integer(ESMF_KIND_I4)                     :: rc_, localrc

    character(len=ESMF_MAXSTR)                :: message, name
    integer(ESMF_KIND_I4)                     :: ubnd3(3), lbnd3(3), ubnd2(2), lbnd2(2)
    type(ESMF_StateItem_Flag)                 :: itemType
    type(ESMF_FieldStatus_Flag)               :: status
    type(ESMF_Field)                          :: field
    integer(ESMF_KIND_I4)                     :: rank

    rc_=ESMF_SUCCESS

    call ESMF_GridCompGet(gridComp, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(state, trim(gridFieldName), itemType=itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELD) then
      call MOSSCO_MessageAdd(message, trim(name)//' cannot use non-field '//trim(gridFieldName)//' as grid')
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    call ESMF_StateGet(state, trim(gridFieldName), field=field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, status=status, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (status == ESMF_FIELDSTATUS_EMPTY) then
      call MOSSCO_MessageAdd(message, trim(name)//' cannot use empty field '//trim(gridFieldName)//' as grid')
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    ! At this point we're sure we have a field with at least GRIDSET status
    call MOSSCO_MessageAdd(message, trim(name)//' uses foreign grid '//trim(gridFieldName))
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    call ESMF_FieldGet(field, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rank<2 .or. rank>3) then
      call MOSSCO_MessageAdd(message, trim(name)//' cannot use field '//trim(gridFieldName)//' with rank /= 2 or 3')
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    end if

    call MOSSCO_MessageAdd(message, trim(name)//' uses foreign')
    write(message,'(A,I1)') trim(message)//' ', rank
    call MOSSCO_MessageAdd(message, 'D grid '//trim(gridFieldName))
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    call ESMF_FieldGet(field, grid=grida_, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    gridb_ =  MOSSCO_GridCreateFromOtherGrid(grida_, rc=localrc)

    if (present(grida)) grida=grida_
    if (present(gridb)) gridb=gridb_
    if (present(rc)) rc=rc_

  end subroutine MOSSCO_GridCopyFromForeignField


  function MOSSCO_GridCreateFromOtherGrid(grida, rc) result(gridb)

    implicit none

    type(ESMF_Grid), intent(in)                  :: grida
    type(ESMF_Grid)                              :: gridb
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                     :: rc_, localrc, rank
    integer(ESMF_KIND_I4)                     :: ubnd3(3), lbnd3(3)
    integer(ESMF_KIND_I4)                     :: ubnd2(2), lbnd2(2)

    rc_ = ESMF_SUCCESS

    call ESMF_GridGet(grida, rank=rank)

    if (rank == 3) then

      call ESMF_GridGetFieldBounds(grida, totalUBound=ubnd3, &
        totalLBound=lbnd3, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      gridb = ESMF_GridCreateNoPeriDim(minIndex=lbnd3(1:2), &
                   maxIndex=ubnd3(1:2), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      call ESMF_GridGetFieldBounds(grida, totalUBound=ubnd2, &
        totalLBound=lbnd2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      gridb = ESMF_GridCreateNoPeriDim(minIndex=(/lbnd2(1),lbnd2(2),1/), &
                   maxIndex=(/ubnd2(1),ubnd2(2),1/), &
                   regDecomp=(/1,1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    endif

    call ESMF_GridAddCoord(gridb, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_GridCopyCoords(grida, gridb, coordDims=(/1,2/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc=rc_
    return

  end function MOSSCO_GridCreateFromOtherGrid

  subroutine MOSSCO_GridCopyCoords(grida, gridb, coordDims, rc)

    implicit none

    type(ESMF_Grid), intent(in)                  :: grida
    type(ESMF_Grid), intent(inout)               :: gridb
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    integer(ESMF_KIND_I4), dimension(:)          :: coordDims

    integer(ESMF_KIND_I4)                     :: rc_, localrc
    integer(ESMF_KIND_I4)                     :: ubnd2(2), lbnd2(2), ubnd3(3), lbnd3(3)
    integer(ESMF_KIND_I4)                     :: coordDim, i, ranka, rankb
    real(ESMF_KIND_R8), pointer               :: coord2(:), coord3(:)
    character(len=ESMF_MAXSTR)                :: message

    rc_ = ESMF_SUCCESS

    call ESMF_GridGet(grida, rank=ranka, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridGet(grida, rank=ranka, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Make a link if both grids are of the same rank, otherwise create new grid
    !! of the missing (2/3) rank
    if (ranka == rankb) then
      gridb = grida
    elseif (ranka<2 .or.ranka>3) then
      call MOSSCO_MessageAdd(message, '  input grid rank must be 2 or 3')
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    elseif (rankb<2 .or.rankb>3) then
      call MOSSCO_MessageAdd(message, '  output grid rank must be 2 or 3')
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    elseif (ranka == 2) then
      do i=1, ubound(coordDims,1)
        coordDim=coordDims(i)

        call ESMF_GridGetCoord(gridb,coordDim=i,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
          computationalLBound=lbnd3, computationalUBound=ubnd3, farrayPtr=coord3,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_GridGetCoord(grida,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
             computationalLBound=lbnd2, computationalUBound=ubnd2, farrayPtr=coord2,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        coord3(lbnd3(i):ubnd3(i)) = coord2(lbnd2(i):ubnd2(i))
      enddo
    else ! rankb == 2
      do i=1, ubound(coordDims,1)
        coordDim=coordDims(i)

        call ESMF_GridGetCoord(grida,coordDim=i,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
          computationalLBound=lbnd3, computationalUBound=ubnd3, farrayPtr=coord3,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_GridGetCoord(gridb,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
             computationalLBound=lbnd2, computationalUBound=ubnd2, farrayPtr=coord2,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        coord2(lbnd2(i):ubnd2(i)) = coord3(lbnd3(i):ubnd3(i))
      enddo
    endif

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_GridCopyCoords

end module mossco_grid
