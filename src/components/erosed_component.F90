!> @brief Implementation of an ESMF component for erosion and sedimentation
!
!> @import
!> @export
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Hassan Nasermoaddeli, Bundesanstalt für Wasserbau
!> @author Carsten Lemmen
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "erosed_component.F90"

module erosed_component

  use esmf
  use mossco_component
  use mossco_state
  use mossco_field
  use mossco_variable_types

  use erosed_driver !, only : initerosed, erosed, getfrac_dummy
  use precision, only : fp
  use mossco_state
  use BioTypes , only :  BioturbationEffect
  implicit none

  public :: SetServices

  private

 ! These Parameters are defined in sedparam.inc seperately for delft-routine
 ! integer, parameter :: SEDTYP_NONCOHESIVE_TOTALLOAD = 0
 ! integer, parameter :: SEDTYP_NONCOHESIVE_SUSPENDED = 1
 ! integer, parameter :: SEDTYP_COHESIVE              = 2

  !! @todo hn: read CF documnetation for correct name of this
  !size_classes_of_upward_flux_of_pim_at_bottom

  type :: ptrarray2D
     real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr=>NULL()
  end type ptrarray2D
  type(ptrarray2D),dimension(:),allocatable :: size_classes_of_upward_flux_of_pim_at_bottom

  type(MOSSCO_VariableFArray2d),dimension(:),allocatable :: importList
  integer(ESMF_KIND_I4),dimension(:,:),pointer           :: mask=>NULL()
  ! Dimensions (x,y,depth layer, fraction index)
  type (BioturbationEffect)                     :: BioEffects
  integer,dimension(:),allocatable              :: external_idx_by_nfrac,nfrac_by_external_idx
  integer                                       :: ubnd(4),lbnd(4)
real(kind=ESMF_KIND_R8),dimension(:,:,:),pointer:: spm_concentration=>null()

   integer                                      :: nmlb           ! first cell number
   integer                                      :: nmub           ! last cell number
   integer                                      :: inum, jnum     ! number of elements in x and y directions , inum * jnum== nmub - nmlb + 1
   integer                                      :: flufflyr       ! switch for fluff layer concept
   integer                                      :: iunderlyr      ! Underlayer mechanism
   integer                                      :: nfrac          ! number of sediment fractions
   real(fp)    , dimension(:,:)    , pointer    :: mfluff=>null() ! composition of fluff layer: mass of mud fractions [kg/m2]
   real(fp)    , dimension(:,:)    , pointer    :: frac=>null()
    !
    ! Local variables
    !
    integer                                     :: i            ! diffusion layer counter
    integer                                     :: l            ! sediment counter
    integer                                     :: nm           ! cell counter
   !integer                                     :: istat        ! error flag
    integer     , dimension(:)  , allocatable   :: sedtyp       ! sediment type [-]
    real(fp)                                    :: g            ! gravitational acceleration [m/s2]
    real(fp)                                    :: morfac       ! morphological scale factor [-]
    real(fp)                                    :: rhow         ! density of water [kg/m3]
    real(fp)    , dimension(:)  , allocatable   :: cdryb        ! dry bed density [kg/m3]
    real(fp)    , dimension(:)  , allocatable   :: chezy        ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    real(fp)    , dimension(:)  , allocatable   :: h0           ! water depth old time level [m]
    real(fp)    , dimension(:)  , allocatable   :: h1           ! water depth new time level [m]
    real(fp)    , dimension(:)  , allocatable   :: rhosol       ! specific sediment density [kg/m3]
    real(fp)    , dimension(:)  , allocatable   :: sedd50       ! 50% diameter sediment fraction [m]
    real(fp)    , dimension(:)  , allocatable   :: sedd90       ! 90% diameter sediment fraction [m]
    real(fp)    , dimension(:)  , allocatable   :: taub         ! bottom shear stress [N/m2]
    real(fp)    , dimension(:)  , allocatable   :: umod         ! depth averaged flow magnitude [m/s]
    real(fp)    , dimension(:)  , allocatable   :: u_bot        ! velocity at the (center of the) bottom cell in u-direction
    real(fp)    , dimension(:)  , allocatable   :: v_bot        ! velocity at the (center of the) bottom cell in v-direction
    real(fp)    , dimension(:)  , allocatable   :: thick        ! thickness of the bottom cell layer
    real(fp)    , dimension(:,:), allocatable   :: mass         ! change in sediment composition of top layer, [kg/m2]
    real(fp)    , dimension(:,:), allocatable   :: massfluff    ! change in sediment composition of fluff layer [kg/m2]
!   real(fp)    , dimension(:,:), allocatable   :: r0           ! concentration old time level[kg/m3]
!   real(fp)    , dimension(:,:), allocatable   :: r1           ! concentration new time level[kg/m3]
!   real(fp)    , dimension(:,:), allocatable   :: rn           ! concentration [kg/m3]
    real(fp)    , dimension(:,:), allocatable   :: sink         ! sediment sink flux [m/s]
    real(fp)    , dimension(:,:), allocatable   :: sinkf        ! sediment sink flux fluff layer [m/s]
    real(fp)    , dimension(:,:), allocatable   :: sour         ! sediment source flux [kg/m2/s]
    real(fp)    , dimension(:,:), allocatable   :: sourf        ! sediment source flux fluff layer [kg/m2/s]
    real(fp)    , dimension(:,:), allocatable   :: ws           ! settling velocity [m/s]
    real(fp)    , dimension(:)  , allocatable   :: mudfrac
    logical                                     :: lexist, anymud, wave
    real(fp)    , dimension(:)  , allocatable   :: uorb, tper, teta ! Orbital velocity [m/s], Wave period, angle between current and wave

    integer :: unit707


contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer              :: localrc

    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)         :: gridComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: parentClock
    integer, intent(out)        :: rc

    character(len=10)           :: InitializePhaseMap(2)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"
    InitializePhaseMap(2) = "IPDv00p2=2"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)    :: gridComp
    type(ESMF_State)       :: importState, exportState
    type(ESMF_Clock)       :: parentClock
    integer, intent(out)   :: rc

    integer                :: localrc
    type(ESMF_Grid)        :: grid, foreign_grid
    type(ESMF_Field)       :: field
    type(ESMF_FieldBundle)                      :: fieldBundle
    character(len=ESMF_MAXSTR)                  :: foreignGridFieldName

    integer                   :: rank
    integer                   :: UnitNr, istat,j
    logical                   :: opnd, exst

    character(ESMF_MAXSTR)    :: name, message
    type(ESMF_Clock)          :: clock
    type(ESMF_Time)           :: currTime

    logical                   :: isPresent, foreignGridIsPresent=.false.

    integer(ESMF_KIND_I4)     :: lbnd2(2),ubnd2(2),lbnd3(3),ubnd3(3)
! local variables
    real(fp),dimension(:), allocatable :: eropartmp, tcrdeptmp,tcrerotmp,depefftmp,depfactmp, &
                             &   parfluff0tmp,parfluff1tmp,tcrflufftmp, fractmp, wstmp, spm_const
    real (fp)                 :: pmcrittmp

    namelist /globaldata/g, rhow
    namelist /benthic/   nmlb       ! = 1  ! first cell number
    namelist /benthic/   nmub       ! = 1  ! last cell number
    namelist /benthic/   morfac     ! = 1.0! morphological scale factor [-]
    !
    ! -----------------------------------------------------------
    !
    namelist /benthic/   nfrac      ! = 2  ! number of sediment fractions
    namelist /benthic/   iunderlyr  ! = 2  ! Underlayer mechanism (default = 1)
    namelist /benthic/   flufflyr   ! = 1  ! switch for fluff layer concept
                                    !  0: no fluff layer (default)
                                    !  1: all mud to fluff layer, burial to bed layers
                                    !  2: part mud to fluff layer, other part to bed layers (no burial)
    namelist /benthic/   anymud     != .true.

!#define DEBUG
    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

 !! get/set grid:
    !! rely on field with name foreignGridFieldName given as attribute and field
    !! in importState
    !! and just take the same grid&distgrid.

!!! Create Grid
    call ESMF_GridCompGet(gridComp,gridIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_GridCompGet(gridComp,grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)
    else

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
           value=foreignGridFieldName, defaultValue='none',rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (trim(foreignGridFieldName)=='none') then
      inum=1
      jnum = 1
      grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                   maxIndex=(/inum,jnum/), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_DELOCAL,  &
                   name="erosed", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridAddCoord(grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      foreignGridIsPresent=.true.
      write(message,*) trim(name)//' uses foreign grid '//trim(foreignGridFieldName)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

      call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_GridGet(grid, rank=rank, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (rank<2 .or. rank>3) then
        write(message,*) 'foreign grid must be of rank 2 or 3'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
      end if

      if (rank==2) then

        call ESMF_GridGet(grid,ESMF_STAGGERLOC_CENTER,0,                                   &
                          exclusiveLBound=lbnd2,exclusiveUBound=ubnd2, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        inum=ubnd2(1)-lbnd2(1)+1
        jnum=ubnd2(2)-lbnd2(2)+1
      endif

      if (rank==3) then
        write(message,*) 'foreign grid of rank 3 not yet implemented'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        call ESMF_FieldGet(field, grid=foreign_grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGetBounds(field, exclusiveLBound=lbnd3, exclusiveUBound=ubnd3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        inum=ubnd3(1)-lbnd3(1)+1
        jnum=ubnd3(2)-lbnd3(2)+1

        grid = ESMF_GridCreateNoPeriDim(minIndex=lbnd3(1:2), &
                   maxIndex=ubnd3(1:2), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="erosed", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_GridAddCoord(grid, rc=localrc)   !> ToDO we need to copy the coordiane from foreign Grid.
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif
    endif
      call ESMF_GridCompSet(gridComp, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

  inquire ( file = 'globaldata.nml', exist=exst , opened =opnd, Number = UnitNr )

  if (exst.and.(.not.opnd)) then
    call ESMF_UtilIOUnitGet(UnitNr, rc = localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    open (unit = UnitNr, file = 'globaldata.nml', action = 'read ', status = 'old', delim = 'APOSTROPHE')
    read (UnitNr, nml=globaldata, iostat = istat)
    close (UnitNr)
  end if

  inquire ( file = 'benthic.nml', exist=exst , opened =opnd, Number = UnitNr )

  if (exst.and.(.not.opnd)) then
    call ESMF_UtilIOUnitGet(UnitNr, rc = localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    open (unit = UnitNr, file = 'benthic.nml', action = 'read ', status = 'old', delim = 'APOSTROPHE')
    read (UnitNr, nml=benthic, iostat = istat)
    close (UnitNr)
  end if
!    nmlb    = 1                 ! first cell number
!    nmub    = 1                 ! last cell number
!    morfac  = 1.0               ! morphological scale factor [-]
!    !
!    ! -----------------------------------------------------------
!    !
!    nfrac       = 2             ! number of sediment fractions
!    iunderlyr   = 2             ! Underlayer mechanism (default = 1)
!    flufflyr    = 1             ! switch for fluff layer concept
!                                !  0: no fluff layer (default)
!                                !  1: all mud to fluff layer, burial to bed layers
!                                !  2: part mud to fluff layer, other part to bed layers (no burial)

    nmlb=1
    nmub = inum * jnum
    call initerosed( nmlb, nmub, nfrac)

    if (.not.associated(BioEffects%ErodibilityEffect)) allocate (BioEffects%ErodibilityEffect(inum, jnum))
    if (.not.associated(BioEffects%TauEffect))         allocate (BioEffects%TauEffect(inum,jnum))
    if (.not.associated(spm_concentration))            allocate(spm_concentration(inum,jnum,nfrac))
    allocate (cdryb     (nfrac))
    allocate (rhosol    (nfrac))
    allocate (sedd50    (nfrac))
    allocate (sedd90    (nfrac))
    allocate (sedtyp    (nfrac))
    !
    allocate (chezy     (nmlb:nmub))
    allocate (h0        (nmlb:nmub))
    allocate (h1        (nmlb:nmub))
    allocate (umod      (nmlb:nmub))
    allocate (u_bot     (nmlb:nmub))
    allocate (v_bot     (nmlb:nmub))
    allocate (thick     (nmlb:nmub))
    allocate (taub      (nmlb:nmub))
!    allocate (r0        (nfrac,nmlb:nmub))
!    allocate (r1        (nfrac,nmlb:nmub))
!    allocate (rn        (nfrac,nmlb:nmub))
    allocate (ws        (nfrac,nmlb:nmub))
    !
    allocate (mass      (nfrac,nmlb:nmub))
    allocate (massfluff (nfrac,nmlb:nmub))
    allocate (sink      (nfrac,nmlb:nmub))
    allocate (sinkf     (nfrac,nmlb:nmub))
    allocate (sour      (nfrac,nmlb:nmub))
    allocate (sourf     (nfrac,nmlb:nmub))

    allocate (frac(nfrac,nmlb:nmub))
    allocate (mfluff(nfrac,nmlb:nmub))
    allocate (mudfrac (nmlb:nmub))

    allocate (uorb      (nmlb:nmub))
    allocate (tper      (nmlb:nmub))
    allocate (teta      (nmlb:nmub))

    !allocation of temporal variables
    allocate ( eropartmp (nfrac),tcrdeptmp(nfrac),tcrerotmp(nfrac),fractmp(nfrac), &
             & depefftmp(nfrac), depfactmp(nfrac),parfluff0tmp(nfrac), &
             & parfluff1tmp(nfrac), tcrflufftmp(nfrac),wstmp(nfrac),spm_const(nfrac), stat =istat)
    if (istat /= 0) stop 'ERROR in allocation of temporal variables in InitializeP1'

    !Initialization
    sink = 0.0_fp
    sour = 0.0_fp
    sinkf=0.0_fp
    sourf=0.0_fp
    mass =0.0_fp
    massfluff=0.0_fp
    mudfrac = 0.0_fp
    mfluff =0.0_fp
    uorb = 0.0_fp
    tper = 1.0_fp
    teta = 0.0_fp
    wave = .false.
    BioEffects%TauEffect =1.0_fp
    BioEffects%ErodibilityEffect = 1.0_fp
!write (*,*)'in Init BioEffects%TauEffect ',BioEffects%TauEffect
    inquire ( file = 'sedparams.txt', exist=exst , opened =opnd, Number = UnitNr )

    if (exst.and.(.not.opnd)) then
      call ESMF_UtilIOUnitGet(UnitNr, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      open (unit = UnitNr, file = 'sedparams.txt', action = 'read ', status = 'old')

 ! non-cohesive sediment
      read (UnitNr,*, iostat = istat) (sedtyp(i),i=1,nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) ( cdryb(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (rhosol(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (sedd50(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (sedd90(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (fractmp(i), i=1, nfrac)
      !if (istat ==0 ) read (UnitNr,*, iostat = istat) ((frac(i,j), i=1, nfrac), j=nmlb,nmub)    ! fraction of each sedimt class from the whole
 ! cohesive sediment
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (eropartmp(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (tcrdeptmp(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (tcrerotmp(i), i=1, nfrac)
!      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((eropar(i,j), i=1, nfrac), j=nmlb,nmub)   ! erosion parameter for mud [kg/m2/s]
!      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((tcrdep(i,j), i=1, nfrac), j=nmlb,nmub)   ! critical bed shear stress for mud sedimentation [N/m2]
!      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((tcrero(i,j), i=1, nfrac), j=nmlb,nmub)   ! critical bed shear stress for mud erosion [N/m2]

 ! cohesive sediment
      if (istat ==0 ) read (UnitNr,*, iostat = istat) pmcrittmp
      !if (istat ==0 ) read (UnitNr,*, iostat = istat) (pmcrit (i), i = nmlb,nmub)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) betam                                      ! power factor for adaptation of critical bottom shear stress [-]
 ! sediment transport formulation
      if (istat ==0 ) read (UnitNr,*, iostat = istat) alf1                                       ! calibration coefficient van Rijn (1984) [-]
      if (istat ==0 ) read (UnitNr,*, iostat = istat) rksc
 ! fluff layer
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (depefftmp(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (depfactmp(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (parfluff0tmp(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (parfluff1tmp(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (tcrflufftmp(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (wstmp(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (spm_const(i), i=1, nfrac)
!      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((depeff(i,j), i=1, nfrac), j=nmlb,nmub)   ! deposition efficiency [-]
!      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((depfac(i,j), i=1, nfrac), j=nmlb,nmub)   ! deposition factor (flufflayer=2) [-]
!      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((parfluff0(i,j), i=1, nfrac), j=nmlb,nmub)! erosion parameter 1 [s/m]
!      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((parfluff1(i,j), i=1, nfrac), j=nmlb,nmub)! erosion parameter 2 [ms/kg]
!      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((tcrfluff(i,j), i=1, nfrac), j=nmlb,nmub) ! critical bed shear stress for fluff layer erosion [N/m2]
      if (istat ==0 ) read (UnitNr,*, iostat = istat) wave
      if (istat /=0) stop ' Error in reading sedparams !!!!'
      close (UnitNr)
      do i =nmlb, nmub
        eropar   (:,i) = eropartmp   (:)
        tcrdep   (:,i) = tcrdeptmp   (:)
        tcrero   (:,i) = tcrerotmp   (:)
        frac     (:,i) = fractmp     (:)
        pmcrit   (  i) = pmcrittmp
        depeff   (:,i) = depefftmp   (:)
        depfac   (:,i) = depfactmp   (:)
        parfluff0(:,i) = parfluff0tmp(:)
        parfluff1(:,i) = parfluff1tmp(:)
        tcrfluff (:,i) = tcrflufftmp (:)
        ws       (:,i) = wstmp       (:) ! initialization, for the case no sediment transport model is coupled with erosed
      end do
!write (*,*) 'wave', wave
      do i = 1, inum
        do j = 1, jnum
          spm_concentration (i,j,:) = spm_const (:)
        end do
      end do

    else
      Write (0,*) 'Error: sedparams.txt for use in erosed does not exit.!!'
      stop
    end if

    !   Initial bed composition
    !
    if (iunderlyr==2) then
        if (flufflyr>0) then
            mfluff  = 0.0_fp        ! composition of fluff layer: mass of mud fractions [kg/m2]
        endif
    endif
    !
    !   Initial flow conditions
    !
    chezy   = 50.0_fp       ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    h1      = 0.03_fp        ! water depth [m]
    h0      = h1            ! @ToDo : read h0 from input data
    umod    = 0.0_fp        ! depth averaged flow magnitude [m/s]
 !   ws      = 0.001_fp      ! Settling velocity [m/s]
!    r1(:,:) = 2.0e-1_fp    ! sediment concentration [kg/m3]
    u_bot   = 0.0_fp        ! flow velocity in u-direction at (center of the ) bottm cell
    v_bot   = 0.0_fp        ! flow velocity in v-direction at (center of the ) bottm cell
    thick   = 0.02_fp       ! height of the bottom cell
    taub    = 0.0_fp
#ifdef DEBUG
    ! Open file for producing output
    call ESMF_UtilIOUnitGet(unit707, rc = localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    inquire (file ='delft_sediment.out', exist = lexist)

!! This file output is not MPI compatible
    if (lexist) then

        open (unit = unit707, file = 'delft_sediment.out', status = 'REPLACE', action = 'WRITE')
    else
        open (unit = unit707, file = 'delft_sediment.out', status = 'NEW', action = 'WRITE')
    end if

    write (unit707, '(A4,2x,A8,2x, A5,7x,A13,3x,A14,4x,A5,6x,A7, 10x, A4, 8x, A8)') &
        'Step','Fractions','layer','Sink(g/m^2/s)','Source(g/m^2/s)', 'nfrac', 'mudfrac', 'taub', 'sink vel'
#endif

    allocate (size_classes_of_upward_flux_of_pim_at_bottom(nfrac))

!> not used fo export State, since sink,sour are used by bed module
!    allocate (size_classes_of_downward_flux_of_pim_at_bottom(1,1,nfrac))
!    size_classes_of_downward_flux_of_pim_at_bottom(1,1,:) = sour (:,1)

  allocate(external_idx_by_nfrac(nfrac))
  allocate(nfrac_by_external_idx(nfrac))


    !! Prepare import state for fields needed in run
!    if (wave) then
!      allocate(importList(15))
!    else
!      allocate(importList(11))
!    end if
    if (wave) then
      allocate(importList(11))
    else
      allocate(importList(7))
    end if
    importList(1)%name  = 'water_depth_at_soil_surface'
    importList(1)%units = 'm'
    importList(2)%name  = 'layer_height_at_soil_surface'
    importList(2)%units = 'm'
    importList(3)%name  = 'depth_averaged_x_velocity_in_water'
    importList(3)%units = 'm s**-1'
    importList(4)%name  = 'depth_averaged_y_velocity_in_water'
    importList(4)%units = 'm s**-1'
    importList(5)%name  = 'x_velocity_at_soil_surface'
    importList(5)%units = 'm s**-1'
    importList(6)%name  = 'y_velocity_at_soil_surface'
    importList(6)%units = 'm s**-1'
    importList(7)%name  = 'turbulent_diffusivity_of_momentum_at_soil_surface'
    importList(7)%units = 'm**2 s**-1'
!    importList( 8)%name  = 'Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface'
!    importList( 8)%units = '-'
!    importList( 9)%name  = 'Effect_of_MPB_on_sediment_erodibility_at_soil_surface'
!    importList( 9)%units = '-'
!    importList(10)%name  = 'Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface'
!    importList(10)%units = '-'
!    importList(11)%name  = 'Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface'
!    importList(11)%units = '-'

!  if (wave) then
!       importList(12)%name  = 'wave_height'
!       importList(12)%units = 'm'
!       importList(13)%name  = 'wave_period'
!       importList(13)%units = 's'
!       importList(14)%name  = 'wave_number'
!       importList(14)%units = 'm**-1'
!       importList(15)%name  = 'wave_direction'
!       importList(15)%units = 'rad'
!    end if
    if (wave) then
       importList( 8)%name  = 'wave_height'
       importList( 8)%units = 'm'
       importList( 9)%name  = 'wave_period'
       importList( 9)%units = 's'
       importList(10)%name  = 'wave_number'
       importList(10)%units = 'm**-1'
       importList(11)%name  = 'wave_direction'
       importList(11)%units = 'rad'
    end if

    do i=1,size(importList)

      if (foreignGridIsPresent) then
        if (trim(importList(i)%name) == foreignGridFieldName) cycle
      end if

      field = ESMF_FieldEmptyCreate(name=trim(importList(i)%name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldEmptySet(field, grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'units',trim(importList(i)%units), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'creator',trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(importState,(/field/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    end do

    fieldBundle = ESMF_FieldBundleCreate(name='concentration_of_SPM_in_water',multiflag=.true.,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleSet(fieldBundle,grid,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(fieldBundle,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateAdd(importState,(/fieldBundle/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    fieldBundle = ESMF_FieldBundleCreate(name='concentration_of_SPM_z_velocity_in_water',multiflag=.true.,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleSet(fieldBundle,grid,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(fieldBundle,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateAdd(importState,(/fieldBundle/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    fieldBundle = ESMF_FieldBundleCreate(name='concentration_of_SPM_upward_flux_at_soil_surface',multiflag=.true.,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleSet(fieldBundle,grid,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(fieldBundle,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateAdd(exportState,(/fieldBundle/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    fieldBundle = ESMF_FieldBundleCreate(name='concentration_of_SPM_downward_flux_at_soil_surface',multiflag=.true.,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleSet(fieldBundle,grid,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(fieldBundle,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateAdd(exportState,(/fieldBundle/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, clock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR)  :: name,message
    type(ESMF_Time)         :: currTime

    type(ESMF_Field), target     :: field
    type(ESMF_Grid)      :: grid
    type(ESMF_FieldStatus_Flag)     :: status
    integer              :: localrc

    integer,target :: coordDimCount(2),coordDimMap(2,2)
    integer,dimension(2)            :: totalLBound,totalUBound
    integer,dimension(2)            :: exclusiveLBound,exclusiveUBound
    integer                         :: i,j
    type :: allocatable_integer_array
      integer,dimension(:),allocatable :: data
    end type
    type(allocatable_integer_array) :: coordTotalLBound(2),coordTotalUBound(2)

    type(ESMF_Field)  ,dimension(:),allocatable :: fieldlist,spm_flux_fieldList
    type(ESMF_FieldBundle)                      :: fieldBundle
    integer(ESMF_KIND_I4)                       :: fieldCount

    real(ESMF_KIND_R8),dimension(:,:),pointer   :: ptr_f2=>null()

    integer :: n
    integer,dimension(:),allocatable :: spm_flux_id
    logical :: isPresent

    call MOSSCO_CompEntry(gridComp, clock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!   Get the total domain size from the coordinates associated with the Grid
    call ESMF_GridCompGet(gridComp,grid=grid)
    call ESMF_GridGet(grid,ESMF_STAGGERLOC_CENTER,0,                                   &
                      exclusiveLBound=exclusiveLBound,exclusiveUBound=exclusiveUBound)
    call ESMF_GridGet(grid,coordDimCount=coordDimCount,coordDimMap=coordDimMap)
    do i=1,2
      allocate(coordTotalLBound(i)%data(coordDimCount(i)))
      allocate(coordTotalUBound(i)%data(coordDimCount(i)))
      call ESMF_GridGetCoordBounds(grid,coordDim=i,                      &
                                   totalLBound=coordTotalLBound(i)%data, &
                                   totalUBound=coordTotalUBound(i)%data)
      do j=1,coordDimCount(i)
        if (coordDimMap(i,j) .eq. i) then
          totalLBound(i) = coordTotalLBound(i)%data(j)
          totalUBound(i) = coordTotalUBound(i)%data(j)
          exit
        end if
      end do
    end do
   !> The preferred interface would be to use isPresent, but htis only works in ESMF from Nov 2014
   !> @todo replace if 0 by ESMF_VERSION macros

#if 0
   call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, isPresent=isPresent, rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   if (isPresent) then
#else
   call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=mask, rc=localrc)
   !! Do not check for success here as NOT_FOUND is expected behaviour, @todo: check for NOT_FOUND flag
   if (localrc .ne. ESMF_SUCCESS) then
      call ESMF_LogWrite('ignore ERROR messages above related to GridGetItem - waiting for new ESMF release', &
                         ESMF_LOGMSG_INFO,ESMF_CONTEXT)
   end if
   if (localrc == ESMF_SUCCESS) then
#endif
      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=mask)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   else
      allocate(mask(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2)))
      mask = 0
      mask(exclusiveLBound(1):exclusiveUBound(1),exclusiveLBound(2):exclusiveUBound(2)) = 1
   end if

!   Complete Import Fields
    do i=1,size(importList)
      call ESMF_StateGet(importState,trim(importList(i)%name),field)
      call ESMF_FieldGet(field,status=status)
      if (status.eq.ESMF_FIELDSTATUS_GRIDSET) then
        call ESMF_LogWrite(' import from internal field '//trim(importList(i)%name),ESMF_LOGMSG_INFO)
        allocate(importList(i)%data(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2)))
        call ESMF_FieldEmptyComplete(field,importList(i)%data,                &
                                     ESMF_INDEX_DELOCAL,                      &
                                     totalLWidth=exclusiveLBound-totalLBound, &
                                     totalUWidth=totalUBound-exclusiveUBound)
      else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
        call ESMF_LogWrite(' import from external field '//trim(importList(i)%name),ESMF_LOGMSG_INFO)
        call ESMF_FieldGet(field,farrayPtr=importList(i)%data,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT,rc=rc)
        if (.not. (      all(lbound(importList(i)%data) .eq. totalLBound) &
                   .and. all(ubound(importList(i)%data) .eq. totalUBound) ) ) then
          call ESMF_LogWrite('invalid field bounds',ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
      else
        call ESMF_LogWrite('empty field: '//trim(importList(i)%name),ESMF_LOGMSG_ERROR, &
                           line=__LINE__,file=__FILE__,method='InitializeP2()')
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
    end do

  !> first try to get "external_index" from "concentration_of_SPM" fieldBundle in import State
    call ESMF_StateGet(importState,"concentration_of_SPM_in_water",fieldBundle,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldBundleGet(fieldBundle,fieldCount=fieldCount,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount==1 .and. nfrac>1) then
      write(message,'(A)') trim(name)//' mapped all fractions to one SPM fraction.'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      external_idx_by_nfrac(:)=1
    elseif (nfrac==1 .and. fieldCount>1) then
      write(message,'(A)') trim(name)//' cannot map 1 fraction to multiple SPM fractions, yet.'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    elseif (nfrac/= 0 .and. fieldCount ==0) then
      write(message,'(A)') trim(name)//'initial values from sedparams.txt will be used for sediment parameters.'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      do i = 1, nfrac
        external_idx_by_nfrac(i)=i
      end do
    elseif (nfrac /= fieldCount) then
      write(message,'(A)') trim(name)//' cannot map unequal size and SPM fractions'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else

      if (allocated(fieldlist) .and. size(fieldList)<fieldcount) deallocate(fieldlist)
      if (.not.allocated(fieldList)) allocate(fieldlist(fieldCount))

      call ESMF_FieldBundleGet(fieldBundle,fieldlist=fieldlist,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      do n=1,fieldCount
        call ESMF_AttributeGet(fieldlist(n),'external_index',external_idx_by_nfrac(n), &
          isPresent=isPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (isPresent) then
          call ESMF_AttributeGet(fieldlist(n),'external_index',external_idx_by_nfrac(n), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        else
          write(message,'(A,I1,A,I1)') trim(name)//' no external index attribute found for SPM fraction //', n
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          external_idx_by_nfrac(n)=n
        endif
      end do
    endif

  !> @todo change mapping from order of SPM fields in fieldbundle to trait-related
  !!       mapping by e.g. d50. It is unknown here, which SPM fraction in water is
  !!       related to SPM fractions in the bed module
  !! after having external_index defined by nfrac, create nfrac_by_external_idx:

  do n=1,ubound(external_idx_by_nfrac,1)
    nfrac_by_external_idx(external_idx_by_nfrac(n))=n
  end do

    call ESMF_StateGet(exportState,"concentration_of_SPM_upward_flux_at_soil_surface",fieldBundle,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldBundleGet(fieldBundle,fieldCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldcount .gt. 0) then
      allocate(spm_flux_fieldList(fieldCount))
      allocate(spm_flux_id(fieldCount))

      call ESMF_FieldBundleGet(fieldBundle, fieldList=spm_flux_fieldList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      do i=1,fieldCount
        call ESMF_AttributeGet(spm_flux_fieldList(i), 'external_index', value=spm_flux_id(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      end do
    end if

  do n=1,nfrac
      i = -1
      do j=1,fieldCount
         if (spm_flux_id(j) .eq. external_idx_by_nfrac(n) ) then
           i = j
           exit
         end if
      end do
      if (i .ne. -1) then
        call ESMF_LogWrite(' export to external field concentration_of_SPM_upward_flux_at_soil_surface',ESMF_LOGMSG_INFO)
        call ESMF_FieldGet(spm_flux_fieldList(i),status=status, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
          call ESMF_FieldGet(spm_flux_fieldList(i),farrayPtr=size_classes_of_upward_flux_of_pim_at_bottom(n)%ptr,rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT,rc=rc)
          if (.not. (      all(lbound(size_classes_of_upward_flux_of_pim_at_bottom(n)%ptr) .eq. (/   1,   1/) ) &
                     .and. all(ubound(size_classes_of_upward_flux_of_pim_at_bottom(n)%ptr) .eq. (/inum,jnum/) ) ) ) then
            call ESMF_LogWrite('invalid field bounds',ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
          end if
          if (.not. (      all(lbound(size_classes_of_upward_flux_of_pim_at_bottom(n)%ptr) .eq. totalLBound) &
                     .and. all(ubound(size_classes_of_upward_flux_of_pim_at_bottom(n)%ptr) .eq. totalUBound) ) ) then
            call ESMF_LogWrite(' field bounds do not match total domain',ESMF_LOGMSG_WARNING,ESMF_CONTEXT)
          end if
        else
          call ESMF_LogWrite('incomplete field',ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
      else
        call ESMF_LogWrite(' export to internal field concentration_of_SPM_upward_flux_at_soil_surface',ESMF_LOGMSG_INFO)
        allocate (size_classes_of_upward_flux_of_pim_at_bottom(n)%ptr(inum, jnum))
        do j=1,jnum
          do i= 1, inum
            size_classes_of_upward_flux_of_pim_at_bottom(n)%ptr(i,j) = sink(n,inum*(j -1)+i)-sour(n,inum*(j -1)+i)
          end do
        end do
        ptr_f2 => size_classes_of_upward_flux_of_pim_at_bottom(n)%ptr

    field = ESMF_FieldCreate(grid, farrayPtr=ptr_f2, &
            name='concentration_of_SPM_upward_flux_at_soil_surface', rc=localrc)
    call ESMF_AttributeSet(field,'external_index',external_idx_by_nfrac(n), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(name)//' creates field'
    call MOSSCO_FieldString(field, message)
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    call ESMF_FieldBundleAdd(fieldBundle,(/field/),multiflag=.true.,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      end if

  end do

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP2

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4) :: localrc

    rc=ESMF_SUCCESS

    !> It does not make sense to readrestart this component, thus, we
    !> 1. do not log calls to this function with CompEntry/CompExit
    !> 2. use dummy variables to avoid -Wunused

    call ESMF_GridCompValidate(gridComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockValidate(parentClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine ReadRestart

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)      :: gridComp
    type(ESMF_State)         :: importState, exportState
    type(ESMF_Clock)         :: parentClock
    integer, intent(out)     :: rc

    type(ESMF_Time)          :: stopTime
    type(ESMF_TimeInterval)  :: timestep
    integer(ESMF_KIND_I8)    :: advancecount
    real(ESMF_KIND_R8)       :: runtimestepcount,dt

    real(kind=ESMF_KIND_R8),dimension(:,:)  ,pointer :: depth=>null(),hbot=>null(),u2d=>null(),v2d=>null(),ubot=>null(),vbot=>null(),nybot=>null()
    real(kind=ESMF_KIND_R8),dimension(:,:)  ,pointer :: waveH=>null(),waveT=>null(),waveK=>null(),waveDir=>null()
    real(kind=ESMF_KIND_R8),dimension(:,:)  ,pointer :: ptr_f2=>null()
    real(kind=ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3=>null()
    type(ESMF_Field)         :: Microphytobenthos_erodibility,Microphytobenthos_critical_bed_shearstress, &
                              & Macrofauna_erodibility,Macrofauna_critical_bed_shearstress
    integer                  :: n, i, j, localrc
    type(ESMF_Field)         :: field
    type(ESMF_Field),dimension(:),allocatable :: fieldlist
    type(ESMF_FieldBundle)   :: fieldBundle
    logical                  :: forcing_from_coupler=.true.
    real(kind=ESMF_KIND_R8),parameter :: porosity=0.1 !> @todo make this an import field (e.g. by bed component)
    real(kind=ESMF_KIND_R8),parameter :: ws_convention_factor=-1.0 !upward positive

    integer                  :: petCount, localPet
    character(ESMF_MAXSTR)   :: name, message
    logical                  :: isPresent
    type(ESMF_Time)          :: currTime
    type(ESMF_Clock)         :: clock
    integer                  :: external_index
    logical                  :: First_entry = .true.
    type(ESMF_StateItem_Flag) :: itemType

!#define DEBUG
    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry (gridComp, parentClock, name, currTime, localrc)
    if  (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
  & call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, petCount=petCount,localPet=localPet,clock=clock, rc=localrc)
    if  (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
  & call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock,currTime=currTime, advanceCount=advanceCount, &
         runTimeStepCount=runTimeStepCount, timeStep=timeStep, rc=localrc)
    if  (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
  & call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalGet(timestep,s_r8=dt,rc=localrc)
    if  (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
  & call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

       !> get import state
    if (forcing_from_coupler) then

      depth => importList(1)%data
      hbot  => importList(2)%data
      u2d   => importList(3)%data
      v2d   => importList(4)%data
      ubot  => importList(5)%data
      vbot  => importList(6)%data
      nybot => importList(7)%data

      if (wave) then
        waveH   => importList( 8)%data
        waveT   => importList( 9)%data
        waveK   => importList(10)%data
        waveDir => importList(11)%data

      end if

!     if (wave) then
!        waveH   => importList(12)%data
!        waveT   => importList(13)%data
!        waveK   => importList(14)%data
!        waveDir => importList(15)%data
!
!      end if
      if (localrc == 0) then
         do j=1,jnum
          do i= 1, inum
           if (mask(i,j)/=0)then
             h1(inum*(j -1)+i) = depth(i,j)
 !          write (*,*) ' water depth ',depth(i,j), 'i,j', i,j, 'nm',inum*(j -1)+i
           endif   ! else use initial value in phase 1
          end do
         end do
      else
        h1=h0
      endif

      if (first_entry) then
         h0 = h1
         first_entry = .false.
      end if

!write (*,*) 'layer_height_at_soil_surface', hbot
!write (*,*)'depth_averaged_x_velocity_in_water', u2d

      if (localrc == 0) then

        do j=1,jnum
          do i= 1, inum
           ! filtering missing values (land)
           if (mask(i,j)/=0)then
            umod  (inum*(j -1)+i) = sqrt( u2d(i,j)*u2d(i,j) + v2d(i,j)*v2d(i,j) )
            thick (inum*(j -1)+i) = hbot (i,j)/depth(i,j)
            u_bot (inum*(j -1)+i) = ubot (i,j)
            v_bot (inum*(j -1)+i) = vbot (i,j)
!                umod (inum*(j -1)+i) = 1.0_fp
!                u_bot (inum*(j -1)+i)= 0.32_fp
!                v_bot (inum*(j -1)+i) = u_bot(inum*(j -1)+i)
!                u2d(i,j)=1.0_fp/1.414213_fp
!                v2d(i,j)=u2d(i,j)
!            write (*,*) 'ubot', ubot(i,j)
!            write (*,*) 'vbot', vbot(i,j)
!            write (*,*) 'u2d, v2d', u2d(i,j), v2d(i,j)
!            write (*,*) 'nm',inum*(j -1)+i, 'thick',thick(inum*(j -1)+i), 'i,j', i,j ,'depth', depth(i,j), 'hbot',hbot (i,j)
            if (wave) then
                tper (inum*(j -1)+i) = waveT (i,j)
                teta (inum*(j -1)+i) = WaveDir (i,j)
                uorb (inum*(j -1)+i) = CalcOrbitalVelocity (waveH(i,j), waveK(i,j), waveT(i,j), depth (i,j))
 !   write (*,*) 'nm', inum*(j -1)+i, 'i,j', i,j,'waveT', waveT(i,j), 'waveH', WaveH(i,j),'waveDir',waveDir(i,j), 'wavek', waveK(i,j)
            endif

           else
            u2d(i,j) = 0.0_fp
            v2d(i,j) = u2d(i,j)
           end if
          end do
        end do

      else
        umod = 0.2
      end if

       !> get spm concentrations, particle sizes and density
      call ESMF_StateGet(importState,'concentration_of_SPM_in_water',fieldBundle,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
   &    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldBundleGet(fieldBundle,fieldCount=n,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
   &    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if(n .eq. 0) then
        !> run without SPM forcing from pelagic component
!#ifdef DEBUG
        call ESMF_LogWrite( &
           'field Bundle concentration_of_SPM not found, run without pelagic forcing', &
           ESMF_LOGMSG_INFO)
!#endif
      else

        if (allocated(fieldlist)) deallocate(fieldlist)
        allocate(fieldlist(n))
        call ESMF_FieldBundleGet(fieldBundle,fieldlist=fieldlist,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do n=1,size(fieldlist)

          field = fieldlist(n)
          call ESMF_AttributeGet(field,'external_index', isPresent=isPresent, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (isPresent) then
            call ESMF_AttributeGet(field,'external_index',external_index, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          else
            write(message,'(A)')  trim(name)//' did not find "external_index" attribute in field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
            external_index=1
            write (*,*) 'external_index is not present, therefore set to 1)'
          endif

          call ESMF_FieldGet(field,farrayPtr=ptr_f3,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_AttributeGet(field,'mean_particle_diameter', isPresent=isPresent, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (isPresent) then
            call ESMF_AttributeGet(field,'mean_particle_diameter',sedd50(nfrac_by_external_idx(external_index)), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          else
            sedd50(nfrac_by_external_idx(external_index))=0.0
            write(message,'(A)')  trim(name)//' did not find "mean_particle_diameter" attribute in field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          endif

          call ESMF_AttributeGet(field,'particle_density', isPresent=isPresent, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (isPresent) then
            call ESMF_AttributeGet(field,'particle_density',rhosol(nfrac_by_external_idx(external_index)), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          else
            rhosol(nfrac_by_external_idx(external_index))=0.0
            write(message,'(A)')  trim(name)//' did not find "rhosol" attribute in field. It has bee set to zero'
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          endif

          !> @todo unclear which localrc is excpected here
          if (localrc == ESMF_SUCCESS) then
            spm_concentration(:,:,nfrac_by_external_idx(external_index)) = ptr_f3(1:inum,1:jnum,1)
          else
            write(0,*) 'cannot find SPM fraction',n
          end if
        end do

        !> get sinking velocities
        call ESMF_StateGet(importState,'concentration_of_SPM_z_velocity_in_water',fieldBundle,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldBundleGet(fieldBundle,fieldlist=fieldlist,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        do n=1,size(fieldlist)
          call ESMF_FieldGet(fieldlist(n), farrayPtr=ptr_f3,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_AttributeGet(fieldlist(n),'external_index',isPresent=isPresent, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (.not.isPresent) then
            write(message,'(A)') trim(name)//' external_index attribute is missing from field '
            call MOSSCO_FieldString(fieldList(n), message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
            external_index = n
          else
            call ESMF_AttributeGet(fieldlist(n),'external_index',external_index, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif

          do j=1,jnum
            do i= 1, inum
              ! filtering missing values (land)
              if (mask(i,j)/=0) then
               ws(nfrac_by_external_idx(external_index),inum*(j -1)+i) = ptr_f3(i,j,1)
               else
               ws(nfrac_by_external_idx(external_index),inum*(j -1)+i) = 0.0_fp
               endif
            end do
          end do
        end do
      end if

    else
      !> use initial values
      h0=h1
    end if

    !> get bio effects
    !> Find Effect_of_MPB_on_sediment_erodibility_at_soil_surface, if found, apply it, else
    !> in else-case the initial values equalt to 1.0 are used.
    call ESMF_StateGet(importState, 'Effect_of_MPB_on_sediment_erodibility_at_soil_surface', &
      itemType=itemType, rc=localrc)
    if  (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType == ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(importState,'Effect_of_MPB_on_sediment_erodibility_at_soil_surface', &
        Microphytobenthos_erodibility,rc=localrc)
      if  (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldGet (field = Microphytobenthos_erodibility, farrayPtr=ptr_f2, rc=localrc)
      if  (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      BioEffects%ErodibilityEffect = ptr_f2
#ifdef DEBUG
 !       write (*,*) 'in erosed component run:MPB BioEffects%ErodibilityEffect=', BioEffects%ErodibilityEffect
#endif
    end if

    !> Find Effect_of_MPB_on_sediment_erodibility_at_soil_surface, if found, apply it, else
    !> in else-case the initial values equalt to 1.0 are used.
    call ESMF_StateGet(importState, 'Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface', &
      itemType=itemType, rc=localrc)
    if  (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType == ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(importState,'Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface', &
        Macrofauna_erodibility,rc=localrc)
      if  (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldGet (field = Macrofauna_erodibility, farrayPtr=ptr_f2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      BioEffects%ErodibilityEffect = ptr_f2 * BioEffects%ErodibilityEffect

    endif

    call ESMF_StateGet(importState,'Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface', &
      itemType=itemType ,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType==ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(importState,'Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface', &
        Microphytobenthos_critical_bed_shearstress ,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet (field = Microphytobenthos_critical_bed_shearstress , farrayPtr=ptr_f2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      BioEffects%TauEffect = ptr_f2
    endif

    call ESMF_StateGet(importState,'Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface', &
      itemType=itemType ,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType==ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(importState,'Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface', &
        Macrofauna_critical_bed_shearstress ,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet (field = Macrofauna_critical_bed_shearstress , farrayPtr=ptr_f2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      BioEffects%TauEffect = ptr_f2 * BioEffects%TauEffect
    endif

   ! filtering missing values (land)
    do i = 1, inum
        do j = 1, jnum
            if (mask(i,j)== 0) then
                BioEffects%TauEffect = 1.0_fp
                BioEffects%ErodibilityEffect = 1.0_fp
                spm_concentration (i,j,:)    = 0.0_fp
            end if
         end do
     end do
#ifdef DEBUG
 !      write (*,*) 'in erosed component run:MPB and Mbalthica BioEffects%ErodibilityEffect=', BioEffects%ErodibilityEffect
 !      write (*,*) 'in erosed component run:MPB and Mbalthica BioEffects%TauEffect=', BioEffects%TauEffect
#endif
    call getfrac_dummy (anymud,sedtyp,nfrac,nmlb,nmub,frac,mudfrac)


    call erosed(  nmlb   , nmub   , flufflyr , mfluff , frac , mudfrac , ws_convention_factor*ws, &
                & umod   , h1     , chezy    , taub   , nfrac, rhosol  , sedd50                 , &
                & sedd90 , sedtyp , sink     , sinkf  , sour , sourf   , anymud   , wave ,  uorb, &
                & tper   , teta   , spm_concentration , BioEffects     , nybot, thick, u_bot, &
                & v_bot  , u2d    , v2d      , h0 , mask   )


    !   Updating sediment concentration in water column over cells
    do l = 1, nfrac
      do nm = nmlb, nmub
!                rn(l,nm) = r0(l,nm) ! explicit
!!                r1(l,nm) = r0(l,nm) + dt*(sour(l,nm) + sourf(l,nm))/h0(nm) - dt*(sink(l,nm) + sinkf(l,nm))*rn(l,nm)/h1(nm)

        i=  1+ mod((nm-1),inum)
        j=  1+int ((nm-1)/inum)
        if (mask(i,j) /=0) then
#ifdef DEBUG
          write (unit707, '(I4,4x,I4,4x,I5,6(4x,F11.4))' ) advancecount, l, nm,min(ws_convention_factor*ws(l,nm),sink(l,nm))*spm_concentration(i,j,l) , sour (l,nm)*1000.0,frac (l,nm), mudfrac(nm), taub(nm), sink(l,nm)
#endif
          size_classes_of_upward_flux_of_pim_at_bottom(l)%ptr(i,j) = &
          sour(l,nm) *1000.0_fp - min(ws_convention_factor*ws(l,nm),sink(l,nm))*spm_concentration(i,j,l)  ! spm_concentration is in [g m-3] and sour in [Kgm-3] (that is why the latter is multiplie dby 1000.
        !write (0, *) ' SOUR', sour(l,nm)*1000.0, 'SINK', sink(l,nm), 'SINKTERM',sink(l,nm) * spm_concentration(i,j,l)
 !       write (0,*) ' SPM_conc',spm_concentration(i,j,l), 'i,j,l',i,j,l, 'nm', nm
 !if (spm_concentration(i,j,l) >100.)  write (*,*) ' SPM_conc',spm_concentration(i,j,l), 'i,j,l',i,j,l, 'nm', nm


        end if
     enddo
      !> @todo check units and calculation of sediment upward flux, rethink ssus to be taken from FABM directly, not calculated by
      !! vanrjin84. So far, we add bed source due to sinking velocity and add material to water using constant bed porosity and
      !! sediment density.

    enddo

    !> save current water level to the old water level for the next time step
    h1 = h0

        !
        !   Compute change in sediment composition of top layer and fluff layer
        !
!    mass       = 0.0_fp    ! change in sediment composition of top layer, [kg/m2]
!    massfluff  = 0.0_fp    ! change in sediment composition of fluff layer [kg/m2]
!        !
!    do l = 1, nfrac
!            do nm = nmlb, nmub
!                !
!                ! Update dbodsd value at nm
!                !
!                mass(l, nm) = mass(l, nm) + dt*morfac*( sink(l,nm)*rn(l,nm) - sour(l,nm) )
!                !
!                ! Update dfluff value at nm
!                !
!                if (flufflyr>0) then
!                    massfluff(l, nm) = massfluff(l, nm) + dt*( sinkf(l,nm)*rn(l,nm) - sourf(l,nm) )
!                endif
!            enddo
!    enddo
        !

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (stopTime>currTime) then
      call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR) :: name
    logical                :: clockIsPresent
    type(ESMF_Time)        :: currTime
    type(ESMF_Clock)       :: clock
    integer                :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifdef DEBUG
    close (unit707)
#endif

    deallocate (cdryb)

    deallocate (rhosol)
    deallocate (sedd50)
    deallocate (sedd90)
    !
    deallocate (chezy)
    deallocate (h0)
    deallocate (h1)
    deallocate (umod)
    deallocate (u_bot)
    deallocate (v_bot)
    deallocate (thick)
    deallocate (taub)
!    deallocate (r0)
!    deallocate (r1)
!    deallocate (rn)
    deallocate (ws)
    !
    deallocate (mass)
    deallocate (massfluff)
    deallocate (sink)
    deallocate (sinkf)
    deallocate (sour)
    deallocate (sourf)
    !
    deallocate (mfluff, frac)
    deallocate (sedtyp)
    deallocate (mudfrac)

    deallocate (uorb, tper,teta)
    deallocate (BioEffects%TauEffect)
    deallocate (BioEffects%ErodibilityEffect)
    deallocate (size_classes_of_upward_flux_of_pim_at_bottom)
    deallocate (spm_concentration)

    call ESMF_GridCompGet(gridComp, clockIsPresent=clockIsPresent)

    if (clockIsPresent) then

      call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockDestroy(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    end if

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "d90_from_d50"
  function d90_from_d50(d50)
    real(ESMF_KIND_R8)            :: d90_from_d50
    real(ESMF_KIND_R8),intent(in) :: d50

    d90_from_d50 = 2.0_ESMF_KIND_R8 * d50

  end function d90_from_d50

  function CalcOrbitalVelocity (WaveHeight, WaveNumber, WavePeriod, WaterDepth)
   implicit none
   real (ESMF_KIND_R8) :: CalcOrbitalVelocity
   real (ESMF_KIND_R8) :: WaveHeight, WaveNumber, WavePeriod, WaterDepth
     CalcOrbitalVelocity = 3.14159265359 * WaveHeight / (WavePeriod * sinh (WaveNumber * WaterDepth))
  end function  CalcOrbitalVelocity

end module erosed_component
