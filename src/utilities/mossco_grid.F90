!> @brief Implementation of grid utilities
!!
!! This computer program is part of MOSSCO.
!! @copyright Copyright 2014, 2015, 2016 Helmholtz-Zentrum Geesthacht
!! @author Carsten Lemmen <carsten.lemmen@hzg.de>
!! @author Hartmut Kapitza <hartmut.kapitza@hzg.de>
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
  use mossco_strings

  implicit none

  public MOSSCO_GridCopyCoords
  public MOSSCO_GridCreateFromOtherGrid

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCreateRegional3D"
function MOSSCO_GridCreateRegional3D(name, rc) result(grid)

  character(ESMF_MAXSTR), intent(in) :: name
  integer,  intent(out), optional    :: rc
  type(ESMF_Grid)                    :: grid

  integer(ESMF_KIND_I4)      :: minIndex(3), maxIndex(3), regDecomp(3)
  type(ESMF_Index_Flag)      :: indexFlag
  type(ESMF_CoordSys_Flag)   :: coordSys
  integer(ESMF_KIND_I4)      :: localrc, i, lbnd(3), ubnd(3), rc_
  real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY

  rc_ = ESMF_SUCCESS

  minIndex=(/1,1,1/)
  maxIndex=(/40,50,10/)
  regDecomp=(/4,3,3/)
  coordSys=ESMF_COORDSYS_SPH_DEG
  indexFlag=ESMF_INDEX_GLOBAL

  grid = ESMF_GridCreateNoPeriDim(minIndex=minIndex, maxIndex=maxIndex, &
    regDecomp=regDecomp, coordSys=coordSys, indexFlag=indexFlag,  &
    name=trim(name)//' grid', coordTypeKind=ESMF_TYPEKIND_R8, coordDep1=(/1/), &
    coorddep2=(/2/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !> This example grid is a 40 x 50 grid at 0.1 degree resolution from 0..4 deg East
  !> to 50 .. 55 deg North
  call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
    computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX, rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  do i=lbnd(1),ubnd(1)
    coordX(i) = 0 + 0.1 * i + 0.05
  enddo
  call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
    computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  do i=lbnd(1),ubnd(1)
    coordY(i) = 50 + 0.1 * i + 0.05
  enddo

  if (present(rc)) rc = rc_

end function MOSSCO_GridCreateRegional3D

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCreateRegional2D"
function MOSSCO_GridCreateRegional2D(name, rc) result(grid)

  character(ESMF_MAXSTR), intent(in) :: name
  integer,  intent(out), optional    :: rc
  type(ESMF_Grid)                    :: grid

  integer(ESMF_KIND_I4)      :: minIndex(2), maxIndex(2), regDecomp(2)
  type(ESMF_Index_Flag)      :: indexFlag
  type(ESMF_CoordSys_Flag)   :: coordSys
  integer(ESMF_KIND_I4)      :: localrc, i, lbnd(2), ubnd(2), rc_
  real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY

  rc_ = ESMF_SUCCESS

  minIndex=(/1,1/)
  maxIndex=(/40,50/)
  regDecomp=(/4,3/)
  coordSys=ESMF_COORDSYS_SPH_DEG
  indexFlag=ESMF_INDEX_GLOBAL

  grid = ESMF_GridCreateNoPeriDim(minIndex=minIndex, maxIndex=maxIndex, &
    regDecomp=regDecomp, coordSys=coordSys, indexFlag=indexFlag,  &
    name=trim(name)//' grid', coordTypeKind=ESMF_TYPEKIND_R8, coordDep1=(/1/), &
    coorddep2=(/2/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !> This example grid is a 40 x 50 grid at 0.1 degree resolution from 0..4 deg East
  !> to 50 .. 55 deg North
  call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
    computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX, rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  do i=lbnd(1),ubnd(1)
    coordX(i) = 0 + 0.1 * i + 0.05
  enddo
  call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
    computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  do i=lbnd(1),ubnd(1)
    coordY(i) = 50 + 0.1 * i + 0.05
  enddo

  if (present(rc)) rc = rc_

end function MOSSCO_GridCreateRegional2D

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCopyFromForeignField"
  subroutine MOSSCO_GridCopyFromForeignField(gridComp, state, gridfieldName, &
    kwe, nlayer, grida, gridb, rc)

    use mossco_strings

    implicit none

    type(ESMF_GridComp), intent(in)           :: gridComp
    type(ESMF_State), intent(in)              :: state
    character(len=*), intent(in)              :: gridFieldName
    logical, intent(in), optional             :: kwe
    integer(ESMF_KIND_I4), optional, intent(in) :: nlayer
    type(ESMF_Grid), intent(out), optional    :: grida
    type(ESMF_Grid), intent(out), optional    :: gridb
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    type(ESMF_Grid)                           :: grida_, gridb_
    integer(ESMF_KIND_I4)                     :: rc_, localrc, nlayer_

    character(len=ESMF_MAXSTR)                :: message, name
    integer(ESMF_KIND_I4)                     :: ubnd3(3), lbnd3(3), ubnd2(2), lbnd2(2)
    type(ESMF_StateItem_Flag)                 :: itemType
    type(ESMF_FieldStatus_Flag)               :: status
    type(ESMF_Field)                          :: field
    integer(ESMF_KIND_I4)                     :: rank

    rc_=ESMF_SUCCESS

    call ESMF_GridCompGet(gridComp, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(state, trim(gridFieldName), itemType=itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELD) then
      call MOSSCO_MessageAdd(message, trim(name)//' cannot use non-field '//trim(gridFieldName)//' as grid')
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    call ESMF_StateGet(state, trim(gridFieldName), field=field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, status=status, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (status == ESMF_FIELDSTATUS_EMPTY) then
      call MOSSCO_MessageAdd(message, trim(name)//' cannot use empty field '//trim(gridFieldName)//' as grid')
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    ! At this point we're sure we have a field with at least GRIDSET status
    call MOSSCO_MessageAdd(message, trim(name)//' uses foreign grid '//trim(gridFieldName))
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    call ESMF_FieldGet(field, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rank < 2 .or. rank > 3) then
      call MOSSCO_MessageAdd(message, trim(name)//' cannot use field '//trim(gridFieldName)//' with rank /= 2 or 3')
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    end if

    call MOSSCO_MessageAdd(message, trim(name)//' uses foreign')
    write(message,'(A,I1)') trim(message)//' ', rank
    call MOSSCO_MessageAdd(message, 'D grid '//trim(gridFieldName))
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    call ESMF_FieldGet(field, grid=grida_, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(nlayer)) then
      gridb_ =  MOSSCO_GridCreateFromOtherGrid(grida_, nlayer=nlayer, rc=localrc)
    else
      gridb_ =  MOSSCO_GridCreateFromOtherGrid(grida_, nlayer=nlayer, rc=localrc)
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(grida)) grida=grida_
    if (present(gridb)) gridb=gridb_
    if (present(rc)) rc=rc_

  end subroutine MOSSCO_GridCopyFromForeignField

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCreateFromOtherGrid"
  function MOSSCO_GridCreateFromOtherGrid(grida, kwe, nlayer, rc) result(gridb)

    implicit none

    type(ESMF_Grid), intent(in)                  :: gridA
    logical, intent(in), optional                :: kwe
    integer(ESMF_KIND_I4), intent(in), optional  :: nlayer
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    type(ESMF_Grid)                              :: gridB

    integer(ESMF_KIND_I4)                     :: rc_, localrc, rank, deCount, nlayer_
    type(ESMF_DistGrid)                       :: distGrid, distGridB
    type(ESMF_CoordSys_Flag)                  :: coordSys
    integer(ESMF_KIND_I4), allocatable        :: coordDimCount(:), coordDimMap(:,:)
    integer(ESMF_KIND_I4), allocatable        :: ubnd(:), lbnd(:)
    type(ESMF_DeLayout)                       :: deLayout
    integer(ESMF_KIND_I4)                     :: ubnd2(2), lbnd2(2), ubnd3(3), lbnd3(3)
    integer(ESMF_KIND_I4)                     :: distGridToArrrayMap(2)
    integer,dimension(:,:)  ,allocatable,target :: minIndexPDe,maxIndexPDe
    integer,dimension(:,:,:),allocatable,target :: deBlockList
    character(len=ESMF_MAXSTR)                :: message

    rc_ = ESMF_SUCCESS
    nlayer_ = 1

    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(nlayer)) nlayer_ = nlayer

    call ESMF_GridGet(grida, rank=rank, distGrid=distGrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if ((rank) == 3 .and. present(nlayer)) then
      write(message,'(A)') '  not allowed to provide nlayer argument with 3D grid'
      if (present(rc)) rc = ESMF_RC_ARG_BAD
      return
    endif

    if (rank>0) then
      allocate(coordDimCount(rank))
      allocate(coordDimMap(rank,3))
      allocate(ubnd(rank))
      allocate(lbnd(rank))
    endif

    call ESMF_GridGet(grida, coordSys=coordSys, coordDimCount=coordDimCount, &
      coordDimMap=coordDimMap, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_DistGridGet(distGrid, deLayout=deLayout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_DeLayoutGet(deLayout, deCount=deCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(minIndexPDe(rank,deCount))
    allocate(maxIndexPDe(rank,deCount))
    allocate(deBlockList(rank,2,deCount))

    call ESMF_DistGridGet(distGrid, minIndexPDe=minIndexPDe, &
                          maxIndexPDe=maxIndexPDe, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    deBlockList(:,1,:) = minIndexPDe
    deBlockList(:,2,:) = maxIndexPDe

    if (rank == 3) then

      distGridB = ESMF_DistGridCreate(minval(deBlockList(1:2,1,:),2), maxval(deBlockList(1:2,2,:),2), &
        int(deBlockList(1:2,:,:)), delayout=delayout, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      gridb = ESMF_GridCreate(distGridB, name="horizontal grid", gridAlign=(/1,1/), &
        coordSys=coordSys, coordDimCount=int(coordDimCount(1:2)),      &
        coordDimMap=int(coordDimMap(1:2,1:2)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    else

    !>l@ todo : adjust code below to use distgrid information, this is impolemented
    ! in fabm_pelagic_component.F90 and should be completley moved to here.

      call ESMF_GridGetFieldBounds(grida, totalUBound=ubnd2, &
        totalLBound=lbnd2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      gridb = ESMF_GridCreateNoPeriDim(minIndex=(/lbnd2(1),lbnd2(2),nlayer_/), &
                   maxIndex=(/ubnd2(1),ubnd2(2),1/), &
                   regDecomp=(/1,1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    endif
    call ESMF_GridAddCoord(gridb, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @todo: implement multidimensional grid coordinates in MOSSCO_GridCopyCoords
    !call MOSSCO_GridCopyCoords(grida, gridb, coordDims=(/1,2/), rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc = rc_

  end function MOSSCO_GridCreateFromOtherGrid

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCopyCoords"
  subroutine MOSSCO_GridCopyCoords(grida, gridb, coordDims, rc)

    implicit none

    type(ESMF_Grid), intent(in)                  :: grida
    type(ESMF_Grid), intent(inout)               :: gridb
    integer(ESMF_KIND_I4), dimension(:)          :: coordDims
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                     :: rc_, localrc, localDeCount
    integer(ESMF_KIND_I4)                     :: ubndA(3), lbndA(3), ubndB(3), lbndB(3)
    integer(ESMF_KIND_I4)                     :: coordDim, i, ranka, rankb
    real(ESMF_KIND_R8), pointer               :: coordA(:), coordB(:)
    character(len=ESMF_MAXSTR)                :: message

    integer(ESMF_KIND_I4), allocatable        :: coordDimCountA(:), coordDimCountB(:)

    rc_ = ESMF_SUCCESS
    call ESMF_GridGet(grida, rank=ranka, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridGet(gridb, rank=rankb, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Make a link if both grids are of the same rank, otherwise create new grid
    !! of the missing (2/3) rank
    if (ranka == rankb) then
      gridb = grida
      if (present(rc)) rc=rc_
      return
    endif

    if (ranka<2 .or.ranka>3) then
      write(message,'(A,I1)') '  input grid rank must be of rank 2 or 3, but is rank ',ranka
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    if (rankb<2 .or.rankb>3) then
      write(message,'(A,I1)') '  output grid rank must be of rank 2 or 3, but is rank ',rankb
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    call ESMF_GridGet(grida, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (localDeCount<1) return
    call ESMF_GridGet(gridb, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (localDeCount<1) return

    allocate(coordDimCountA(ranka))
    call ESMF_GridGet(grida, coordDimCount=coordDimCountA, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (any(coordDimCountA /= 1 )) then
      write(message,*) '  not implemented: copying grids with multidimensional coordinates', coordDimCountA
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
      !call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    allocate(coordDimCountB(rankb))
    call ESMF_GridGet(gridb, coordDimCount=coordDimCountB, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (any(coordDimCountB /= 1 )) then
      write(message,*) '  not implemented: copying grids with multidimensional coordinates', coordDimCountB
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
      !call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    do i=1, ubound(coordDims,1)
      coordDim=coordDims(i)

      call ESMF_GridGetCoordBounds(gridb,coordDim=i,localDE=0, &
        exclusiveLBound=lbndB, exclusiveUBound=ubndB, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (ubndB(1)-lbndB(1) <= 0) then
        write(message,'(A)') '  no coord data on this DE, skipped'
        continue
      endif

      call ESMF_GridGetCoord(gridb, coordDim=i, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        farrayPtr=coordB, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_GridGetCoordBounds(grida,coordDim=i,localDE=0, &
        exclusiveLBound=lbndA, exclusiveUBound=ubndA, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (ubndA(1)-lbndA(1) <= 0) then
        write(message,'(A)') '  no coord data on this DE, skipped'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        continue
      endif

      call ESMF_GridGetCoord(grida,coordDim=i,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=coordA,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      coordB(lbndB(1):ubndB(1)) = coordA(lbndB(1):ubndB(1))
    enddo

    deallocate(coordDimCountA)
    deallocate(coordDimCountB)

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_GridCopyCoords

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridString"
subroutine MOSSCO_GridString(grid, message, kwe, length, rc)

  type(ESMF_Grid), intent(in)                    :: grid
  character(len=ESMF_MAXSTR), intent(inout)      :: message
  logical, intent(in), optional                  :: kwe
  integer(ESMF_KIND_I4), intent(inout), optional :: length
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  integer(ESMF_KIND_I4)   :: rc_, length_, rank, localrc
  character(ESMF_MAXSTR)  :: stringValue, name

  logical                     :: isPresent
  integer(ESMF_KIND_I4), allocatable :: ubnd(:), lbnd(:)

  rc_ = ESMF_SUCCESS
  if (present(kwe)) rc_ = ESMF_SUCCESS

  call ESMF_AttributeGet(grid, name='creator', isPresent=isPresent, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (isPresent) then
    call ESMF_AttributeGet(grid, name='creator', value=stringValue, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call MOSSCO_MessageAdd(message, ' ['//stringValue)
    call MOSSCO_MessageAdd(message, ']'//name)
  else
    call MOSSCO_MessageAdd(message,' '//name)
  endif

  call ESMF_GridGet(grid, rank=rank, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (len_trim(message) + 7 <=len(message)) write(message,'(A,I1)') trim(message)//' rank',rank
  allocate(ubnd(rank))
  allocate(lbnd(rank))

  call ESMF_GridGetFieldBounds(grid, totalUBound=ubnd, &
        totalLBound=lbnd, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (rank>0 .and. (len_trim(message) + 5 <=len(message))) write(message,'(A,I3)') trim(message)//' (', ubnd(1)-lbnd(1)+1
  if (rank>1 .and. (len_trim(message) + 4 <=len(message))) write(message,'(A,X,I3)') trim(message), ubnd(2)-lbnd(2)+1
  if (rank>2 .and. (len_trim(message) + 4 <=len(message))) write(message,'(A,X,I3)') trim(message), ubnd(3)-lbnd(3)+1
  if (rank>3 .and. (len_trim(message) + 4 <=len(message))) write(message,'(A,X,I3)') trim(message), ubnd(4)-lbnd(4)+1
  if (len_trim(message) + 1 <=len(message)) write(message,'(A)') trim(message)//')'

  deallocate(ubnd)
  deallocate(lbnd)

  length_=len_trim(message)
  if (present(length)) length=length_
  if (present(rc)) rc=rc_

end subroutine MOSSCO_GridString

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridPrintBlockList"
subroutine MOSSCO_GridPrintBlockList(grid, rc)

  type(ESMF_Grid), intent(in)                    :: grid
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  integer(ESMF_KIND_I4)          :: localrc, rc_
  type(ESMF_DistGrid)            :: distGrid

  call ESMF_GridGet(grid, distGrid=distGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call MOSSCO_DistGridPrintBlockList(distGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (present(rc)) rc = rc_

end subroutine MOSSCO_GridPrintBlockList

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_DistGridPrintBlockList"
subroutine MOSSCO_DistGridPrintBlockList(distGrid, rc)

  type(ESMF_DistGrid), intent(in)                    :: distGrid
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  integer(ESMF_KIND_I4)          :: localrc, rc_
  type(ESMF_DeLayout)            :: deLayout

  call ESMF_DistGridGet(distGrid, deLayout=deLayout, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call MOSSCO_DeLayoutPrintBlockList(deLayout, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (present(rc)) rc = rc_

end subroutine MOSSCO_DistGridPrintBlockList


#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_DeLayoutPrintBlockList"
subroutine MOSSCO_DeLayoutPrintBlockList(deLayout, rc)

  type(ESMF_DeLayout), intent(in)                    :: deLayout
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  integer(ESMF_KIND_I4)              :: localrc, deCount, rank, localDeCount, rc_
  integer(ESMF_KIND_I4), allocatable :: deBlockList(:,:,:)
  character(len=ESMF_MAXSTR)         :: message

  call ESMF_DeLayoutGet(deLayout, deCount=deCount, localDeCount=localDeCount, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (localDeCount /= 1 ) then
    write(message, '(A,I3)') '  cannot handle localDeCount /= 1'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    call ESMF_Finalize()
  endif

  if (allocated(deBlockList)) deallocate(deBlockList)
  if (deCount<=0) then
    write(message, '(A,I3)') '  cannot handle deCount less than 1 (',deCount,')'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    call ESMF_Finalize()
  endif

  allocate(deBlockList(rank,2,deCount))
  !call MOSSCO_MatrixFilePrint(deBlocklist(:,1,:), filename, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  !call MOSSCO_MatrixFilePrint(deBlocklist(:,1,:), filename, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_DeLayoutPrint(deLayout, rc=localrc)

  if (allocated(deBlockList)) deallocate(deBlockList)
  if (present(rc)) rc = rc_

end subroutine MOSSCO_DeLayoutPrintBlockList

end module mossco_grid
