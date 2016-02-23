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
  use mossco_grid
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
     real(ESMF_KIND_R8),dimension(:,:),pointer           :: ptr=>NULL()
  end type ptrarray2D
  type(ptrarray2D),dimension(:),allocatable              :: size_classes_of_upward_flux_of_pim_at_bottom
  type(ptrarray2D)                                       :: rms_orbital_velocity, bottom_shear_stress
  type(ptrarray2D)                                       :: bottom_shear_stress_noncoh, equilibrium_spm

  type(MOSSCO_VariableFArray2d),dimension(:),allocatable :: importList
  
  type (BioturbationEffect)                              :: BioEffects

  real   (kind=ESMF_KIND_R8),dimension(:,:,:)  ,pointer  :: layers_height=>null(),sigma_midlayer=>null(),sediment_mass=>null()
  real   (kind=ESMF_KIND_R8),dimension(:,:,:)  ,pointer  :: relative_thickness_of_layers=>null()
  real   (kind=ESMF_KIND_R8),dimension(:,:,:,:),pointer  :: spm_concentration=>null() ! Dimensions (x,y,depth layer, fraction index)
  
  integer(kind=ESMF_KIND_I4),dimension(:,:)    ,pointer  :: mask=>NULL()
  integer                   ,dimension(:),allocatable    :: external_idx_by_nfrac,nfrac_by_external_idx
  integer                                                :: ubnd(4),lbnd(4)
  
   integer                                      :: nmlb           ! first cell number
   integer                                      :: nmub           ! last cell number
   integer                                      :: inum, jnum     ! number of elements in y and x directions , inum * jnum== nmub - nmlb + 1
   integer                                      :: flufflyr       ! switch for fluff layer concept
   integer                                      :: iunderlyr      ! Underlayer mechanism
   integer                                      :: nfrac          ! number of sediment fractions
   real(fp)                                     :: init_thick     ! Initial thickness of the bed layer
   real(fp)                                     :: porosity       ! porosity of the bed layer (ideally mixed single bed layer)
   real(fp)    , dimension(:,:)    , pointer    :: mfluff=>null() ! composition of fluff layer: mass of mud fractions [kg/m2]
   real(fp)    , dimension(:,:)    , pointer    :: frac=>null()
    !
    ! Local variables
    !
    integer     , dimension(:)  , allocatable   :: sedtyp       ! sediment type [-]
    real(fp)                                    :: g            ! gravitational acceleration [m/s2]
    real(fp)                                    :: morfac       ! morphological scale factor [-]
    real(fp)                                    :: rhow         ! density of water [kg/m3]
    real(fp)    , dimension(:)  , allocatable   :: cdryb        ! dry bed density [kg/m3]
    real(fp)    , dimension(:)  , allocatable   :: chezy        ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    real(fp)    , dimension(:)  , allocatable, save   :: h0           ! water depth old time level [m]
    real(fp)    , dimension(:)  , allocatable   :: h1           ! water depth new time level [m]
    real(fp)    , dimension(:)  , allocatable   :: rhosol       ! specific sediment density [kg/m3]
    real(fp)    , dimension(:)  , allocatable   :: sedd50       ! 50% diameter sediment fraction [m]
    real(fp)    , dimension(:)  , allocatable   :: sedd90       ! 90% diameter sediment fraction [m]
    real(fp)    , dimension(:)  , allocatable   :: taub, taubn   ! bottom shear stress [N/m2]
    real(fp)    , dimension(:)  , allocatable   :: umod         ! depth averaged flow magnitude [m/s]
    real(fp)    , dimension(:)  , allocatable   :: u_bot        ! velocity at the (center of the) bottom cell in u-direction
    real(fp)    , dimension(:)  , allocatable   :: v_bot        ! velocity at the (center of the) bottom cell in v-direction

    real(fp)    , dimension(:,:), allocatable,save   :: mass         ! sediment mass in bottom layer as an ideally mixed single bed layer, [kg/m2]
    real(fp)    , dimension(:,:), allocatable   :: massfluff    ! change in sediment composition of fluff layer [kg/m2]
    real(fp)    , dimension(:,:), allocatable   :: sink         ! sediment sink flux [m/s]
    real(fp)    , dimension(:,:), allocatable   :: sinkf        ! sediment sink flux fluff layer [m/s]
    real(fp)    , dimension(:,:), allocatable   :: sour         ! sediment source flux [kg/m2/s]
    real(fp)    , dimension(:,:), allocatable   :: sourf        ! sediment source flux fluff layer [kg/m2/s]
    real(fp)    , dimension(:,:), allocatable   :: ws           ! settling velocity [m/s]
    real(fp)    , dimension(:)  , allocatable   :: mudfrac
    logical                                     :: lexist, anymud, wave, bedmodel
    real(fp)    , dimension(:)  , allocatable   :: uorb, tper, teta ! Orbital velocity [m/s], Wave period, angle between current and wave
    real(fp)    , dimension(:)  , allocatable   :: eq_conc    ! equilibrium sand fraction concentration [g.m**-3]    integer :: unit707


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
    logical                     :: isPresent


    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"
    InitializePhaseMap(2) = "IPDv00p2=2"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
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

    integer                :: localrc, knum
    type(ESMF_Grid)        :: grid, foreign_grid, grid3
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
    integer                   :: i

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
    namelist /benthic/   bedmodel   ! = .true. means a simple bed model for tracking mass balanace at sea bed is activated

!#define DEBUG
    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
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

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      inum=1
      jnum = 1
      knum = 30
      grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                   maxIndex=(/inum,jnum/), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_DELOCAL,  &
                   name="erosed", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridAddCoord(grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else

      call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
        value=foreignGridFieldName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      foreignGridIsPresent=.true.

      call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,*) trim(name)//' uses foreign grid from field'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

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
        knum=30 ! default value
      endif

      if (rank==3) then

        write(message,*) trim(name)//' uses foreign grid from field'
        call MOSSCO_FieldString(field, message)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

        call ESMF_FieldGet(field, grid=grid3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGetBounds(field, exclusiveLBound=lbnd3, exclusiveUBound=ubnd3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        inum=ubnd3(1)-lbnd3(1)+1
        jnum=ubnd3(2)-lbnd3(2)+1
        knum=ubnd3(3)-lbnd3(3)+1

        grid = MOSSCO_GridCreateFromOtherGrid(grid3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif
    endif
      call ESMF_GridCompSet(gridComp, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

  !initialization

  bedmodel = .false.

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

    nmlb=1
    nmub = inum * jnum
    call initerosed( nmlb, nmub, nfrac)

    if (.not.associated(BioEffects%ErodibilityEffect)) allocate (BioEffects%ErodibilityEffect(inum, jnum))
    if (.not.associated(BioEffects%TauEffect))         allocate (BioEffects%TauEffect(inum,jnum))
!   TODO: Replace static allocation to 30 layers !!!!!
    if (.not.associated(spm_concentration))            allocate(spm_concentration(inum,jnum,knum,nfrac))
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

    allocate (taub      (nmlb:nmub))
    allocate (taubn      (nmlb:nmub))
    allocate (eq_conc     (nmlb:nmub))
    allocate (ws        (nfrac,nmlb:nmub))
    !
    if (bedmodel) then
      allocate (mass (nfrac,nmlb:nmub))
      mass = 0.0d0
    end if
    
!    allocate (massfluff (nfrac,nmlb:nmub))
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
    if (istat /= 0) then
      call ESMF_LogWrite('Allocation of temporal variables in InitializeP1 failed', ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    !Initialization
    sink = 0.0_fp
    sour = 0.0_fp
    sinkf=0.0_fp
    sourf=0.0_fp
!    massfluff=0.0_fp
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
      if (istat ==0.and. bedmodel ) read (UnitNr,*, iostat = istat) init_thick
      if (istat ==0.and. bedmodel ) read (UnitNr,*, iostat = istat) porosity
      if (istat /= 0) then
        call ESMF_LogWrite('Error in reading sedparams !!!!', ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

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
     
      if (bedmodel) call init_mass(nfrac, frac,nmub, init_thick, porosity, rhosol,mass)    

!      do i = 1, inum
!        do j = 1, jnum
!          spm_concentration (i,j,:) = spm_const (:)
!        end do
!      end do

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
    u_bot   = 0.0_fp        ! flow velocity in u-direction at (center of the ) bottm cell
    v_bot   = 0.0_fp        ! flow velocity in v-direction at (center of the ) bottm cell

    taub    = 0.0_fp
    taubn    = 0.0_fp
    eq_conc =0.0_fp
#ifdef DEBUG
    ! Open file for producing output
!   call ESMF_UtilIOUnitGet(unit707, rc = localrc)
!   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!     call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!   inquire (file ='delft_sediment.out', exist = lexist)

!! This file output is not MPI compatible
!   if (lexist) then

!       open (unit = unit707, file = 'delft_sediment.out', status = 'REPLACE', action = 'WRITE')
!   else
!       open (unit = unit707, file = 'delft_sediment.out', status = 'NEW', action = 'WRITE')
!   end if

!   write (unit707, '(A4,2x,A8,2x, A5,7x,A13,3x,A14,4x,A5,6x,A7, 10x, A4, 8x, A8)') &
!       'Step','Fractions','layer','Sink(g/m^2/s)','Source(g/m^2/s)', 'nfrac', 'mudfrac', 'taub', 'sink vel'
#endif

    allocate (size_classes_of_upward_flux_of_pim_at_bottom(nfrac))

!> not used fo export State, since sink,sour are used by bed module
!    allocate (size_classes_of_downward_flux_of_pim_at_bottom(1,1,nfrac))
!    size_classes_of_downward_flux_of_pim_at_bottom(1,1,:) = sour (:,1)

  allocate(external_idx_by_nfrac(nfrac))


    !! Prepare import state for fields needed in run
    if (wave) then
      allocate(importList(17))
    else
      allocate(importList(13))
    end if

    importList( 1)%name  = 'water_depth_at_soil_surface'
    importList( 1)%units = 'm'
    importList( 2)%name  = 'layer_height_at_soil_surface'
    importList( 2)%units = 'm'
    importList( 3)%name  = 'depth_averaged_x_velocity_in_water'
    importList( 3)%units = 'm s**-1'
    importList( 4)%name  = 'depth_averaged_y_velocity_in_water'
    importList( 4)%units = 'm s**-1'
    importList( 5)%name  = 'x_velocity_at_soil_surface'
    importList( 5)%units = 'm s**-1'
    importList( 6)%name  = 'y_velocity_at_soil_surface'
    importList( 6)%units = 'm s**-1'
    importList( 7)%name  = 'turbulent_diffusivity_of_momentum_at_soil_surface'
    importList( 7)%units = 'm**2 s**-1'
    importList( 8)%name  = 'maximum_bottom_stress'
    importList( 8)%units = 'Pa'
    importList( 8)%optional = .true.
    importList( 9)%name  = 'Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface'
    importList( 9)%units = '-'
    importList( 9)%optional = .true.
    importList(10)%name  = 'Effect_of_MPB_on_sediment_erodibility_at_soil_surface'
    importList(10)%units = '-'
    importList(10)%optional = .true.
    importList(11)%name  = 'Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface'
    importList(11)%units = '-'
    importList(11)%optional = .true.
    importList(12)%name  = 'Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface'
    importList(12)%units = '-'
    importList(12)%optional = .true.
    importList(13)%name  = 'water_column_area'
    importList(13)%units = 'm2'

    if (wave) then
       importList(14)%name  = 'wave_height'
       importList(14)%units = 'm'
       importList(15)%name  = 'wave_period'
       importList(15)%units = 's'
       importList(16)%name  = 'wave_number'
       importList(16)%units = 'm**-1'
       importList(17)%name  = 'wave_direction'
       importList(17)%units = 'rad'
    end if

    do i=1,size(importList)

      if (foreignGridIsPresent) then
        if (trim(importList(i)%name) == foreignGridFieldName) cycle
      end if

      field = ESMF_FieldEmptyCreate(name=trim(importList(i)%name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldEmptySet(field, grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'units',trim(importList(i)%units), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'creator',trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message, '(A)') trim(name)//' created field'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAdd(importState,(/field/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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

    call MOSSCO_CompEntry(gridComp, clock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
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
      allocate(mask(exclusiveLBound(1):exclusiveUBound(1),exclusiveLBound(2):exclusiveUBound(2)))
      mask = 1
   end if

!   Complete Import Fields
    do i=1,size(importList)
      call ESMF_StateGet(importState,trim(importList(i)%name),field)
      call ESMF_FieldGet(field,status=status)
      if (status.eq.ESMF_FIELDSTATUS_GRIDSET) then
        if ( importList(i)%optional ) cycle
        call ESMF_LogWrite(' import from internal field '//trim(importList(i)%name),ESMF_LOGMSG_INFO)
        allocate(importList(i)%data(exclusiveLBound(1):exclusiveUBound(1),exclusiveLBound(2):exclusiveUBound(2)))
        call ESMF_FieldEmptyComplete(field,importList(i)%data,ESMF_INDEX_DELOCAL)
        importList(i)%data = 0.0d0
      else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
        call ESMF_LogWrite(' import from external field '//trim(importList(i)%name),ESMF_LOGMSG_INFO)
        call ESMF_FieldGet(field,farrayPtr=importList(i)%data,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT,rc=rc)
        if (.not. (    (      all(lbound(importList(i)%data) .eq. totalLBound    )           &
                        .and. all(ubound(importList(i)%data) .eq. totalUBound    ) )         &
                   .or.(      all(lbound(importList(i)%data) .eq. exclusiveLBound)           &
                        .and. all(ubound(importList(i)%data) .eq. exclusiveUBound) ) ) ) then
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

    allocate(nfrac_by_external_idx(1:maxval(external_idx_by_nfrac)))
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
        else
          call ESMF_LogWrite('incomplete field',ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
      else
        call ESMF_LogWrite(' export to internal field concentration_of_SPM_upward_flux_at_soil_surface',ESMF_LOGMSG_INFO)
        !> @todo This allocation might be critical if the field has totalwidth (halo zones)
        !>        We might have to allocate with these halo zones (not until we get into trouble)
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

    !> @todo This allocation might be critical if the field has totalwidth (halo zones)
    !>        We might have to allocate with these halo zones (not until we get into trouble)
    allocate (rms_orbital_velocity%ptr(inum, jnum))

    rms_orbital_velocity%ptr(:,:)= 0.0_fp

    field = ESMF_FieldCreate(grid, farrayPtr=rms_orbital_velocity%ptr, &
            name='rms_orbital_velocity_at_soil_surface', rc=localrc)
    call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(name)//' created field'
    call MOSSCO_FieldString(field, message)
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    call ESMF_StateAdd(exportState,(/field/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @todo This allocation might be critical if the field has totalwidth (halo zones)
    !>        We might have to allocate with these halo zones (not until we get into trouble)
    allocate (bottom_shear_stress%ptr(inum, jnum))

    bottom_shear_stress%ptr(:,:)= 0.0_fp

    field = ESMF_FieldCreate(grid, farrayPtr=bottom_shear_stress%ptr, &
            name='shear_stress_at_soil_surface', rc=localrc)
    call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(name)//' created field'
    call MOSSCO_FieldString(field, message)
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    call ESMF_StateAdd(exportState,(/field/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)



    allocate (bottom_shear_stress_noncoh%ptr(inum, jnum))

    bottom_shear_stress_noncoh%ptr(:,:)= 0.0_fp

    field = ESMF_FieldCreate(grid, farrayPtr=bottom_shear_stress_noncoh%ptr, &
            name='shear_stress_at_soil_surface_noncohesive', rc=localrc)
    call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(name)//' created field'
    call MOSSCO_FieldString(field, message)
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    call ESMF_StateAdd(exportState,(/field/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   allocate (equilibrium_spm%ptr(inum, jnum))

    equilibrium_spm%ptr(:,:)= 0.0_fp

    field = ESMF_FieldCreate(grid, farrayPtr=equilibrium_spm%ptr, &
            name='Equilibrium_SPM_concentration_at_soil_surface_noncohesive', rc=localrc)
    call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(name)//' created field'
    call MOSSCO_FieldString(field, message)
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    call ESMF_StateAdd(exportState,(/field/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!#ifdef DEBUG
   allocate (sediment_mass(inum, jnum,nfrac))

     field = ESMF_FieldCreate(grid, &
                         typekind=ESMF_TYPEKIND_R8, &
                         name='sediment_mass_in_bed',&
                         staggerloc=ESMF_STAGGERLOC_CENTER, &
                         ungriddedLBound=(/1/),ungriddedUBound=(/nfrac/), &
                         gridToFieldMap=(/1,2/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(field,'units',trim('Kg'),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!        call ESMF_AttributeSet(field,'missing_value',sed%missing_value,rc=localrc)
!        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,rcToReturn=rc)) &
!          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldGet(field=field, farrayPtr=sediment_mass,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        sediment_mass(:,:,:)= 0.0_fp
       
        call ESMF_StateAdd(exportState,(/field/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,ESMF_CONTEXT,rcToReturn=rc)) &
         call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)



    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    logical               :: isPresent

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

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
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
    type(ESMF_Grid)          :: grid
    integer(ESMF_KIND_I8)    :: advancecount
    real(ESMF_KIND_R8)       :: runtimestepcount,dt

    real(kind=ESMF_KIND_R8),dimension(:,:)  ,pointer :: depth=>null(),hbot=>null(),u2d=>null(),v2d=>null(),ubot=>null(),vbot=>null(),nybot=>null()
    real(kind=ESMF_KIND_R8),dimension(:,:)  ,pointer ::taubmax=>null(),area=>null()
    real(kind=ESMF_KIND_R8),dimension(:,:)  ,pointer :: waveH=>null(),waveT=>null(),waveK=>null(),waveDir=>null()
    real(kind=ESMF_KIND_R8),dimension(:,:)  ,pointer :: microEro=>null(),microTau=>null(),macroEro=>null(),macroTau=>null()
    real(kind=ESMF_KIND_R8),dimension(:,:)  ,pointer :: ptr_f2=>null()
    real(kind=ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3=>null()
    type(ESMF_Field)         :: Microphytobenthos_erodibility,Microphytobenthos_critical_bed_shearstress, &
                              & Macrofauna_erodibility,Macrofauna_critical_bed_shearstress
    integer                  :: n, i, j, k,l, localrc, istat,nm
    type(ESMF_Field)         :: field
    type(ESMF_Field),dimension(:),allocatable :: fieldlist
    type(ESMF_FieldBundle)   :: fieldBundle
    logical                  :: forcing_from_coupler=.true.
    real(kind=ESMF_KIND_R8),parameter :: ws_convention_factor=-1.0 !upward positive

    integer                  :: petCount, localPet
    character(ESMF_MAXSTR)   :: name, message
    logical                  :: isPresent
    type(ESMF_Time)          :: currTime
    type(ESMF_Clock)         :: clock
    integer                  :: external_index
    logical                  :: First_entry = .true.

    type(ESMF_StateItem_Flag) :: itemType
    type(ESMF_FieldStatus_Flag) :: status
    integer,dimension(2)     :: totalLBound,totalUBound
    integer,dimension(2)     :: exclusiveLBound,exclusiveUBound
    integer(ESMF_KIND_I4)    :: ubnd(3), lbnd(3), tubnd(3), tlbnd(3)
    integer                  :: kmx, kmaxsd !(kmaxsd: kmax-layer index for sand)
!#define DEBUG
    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, clock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
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

          call ESMF_FieldGet(field,farrayPtr=ptr_f3,exclusiveLBound=lbnd, &
          & exclusiveUBound=ubnd,TotalLBound=tlbnd,TotalUBound=tubnd,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          &   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           !> @todo proper bounds checking with eLBound required here
!          if (.not. ( all(lbound(ptr_f3)== lbnd).and. all(ubound(ptr_f3)==ubnd ) ) ) then
!            write(message, '(A)') trim(name)//' invalid field bounds in field'
!            call MOSSCO_FieldString(field, message)
!            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
!            call ESMF_Finalize(endflag=ESMF_END_ABORT)
!          end if
!
!          if (.not. (all(lbound(ptr_f3)==tlbnd).and. all(ubound(ptr_f3)==tubnd) ) ) then
!            write(message, '(A)') trim(name)//' bounds do not match total domain in field'
!            call MOSSCO_FieldString(field, message)
!            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
!          end if

          !> @todo unclear which localrc is excpected here
          !write (0,*) 'shape of spm_concentration original', shape (ptr_f3)
          if (localrc == ESMF_SUCCESS) then
            spm_concentration(:,:,:ubnd(3),nfrac_by_external_idx(external_index)) = ptr_f3(1:inum,1:jnum,1:)
          else
            write(0,*) 'cannot find SPM fraction',n
          end if


          call ESMF_FieldGet(field, grid=grid,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          & call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !> get z-positions of vertical layer interfaces
          call ESMF_GridGetCoord(grid, coordDim=3, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
           farrayPtr=layers_height, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          & call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
              if ( mask(i,j) .gt. 0 ) then
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

!-----

      depth    => importList( 1)%data
      hbot     => importList( 2)%data
      u2d      => importList( 3)%data
      v2d      => importList( 4)%data
      ubot     => importList( 5)%data
      vbot     => importList( 6)%data
      nybot    => importList( 7)%data
      taubmax  => importList( 8)%data

      microTau => importList( 9)%data
      microEro => importList(10)%data
      macroTau => importList(11)%data
      macroEro => importList(12)%data
      area     => importList(13)%data

      if (wave) then
        waveH   => importList(14)%data
        waveT   => importList(15)%data
        waveK   => importList(16)%data
        waveDir => importList(17)%data
      end if

      if (localrc == 0) then
         do j=1,jnum
          do i= 1, inum
           if ( mask(i,j) .gt. 0 ) then
             h1(inum*(j -1)+i) = depth(i,j)
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

      if (.not. associated (relative_thickness_of_layers)) then
        allocate (relative_thickness_of_layers(lbnd(1):ubnd(1),lbnd(2):ubnd(2) ,lbnd(3):ubnd(3) ), stat=istat)
        if (istat/=0) write (*,*) 'Warning/Error in allocation of relative_thickness_of_layers in erosed_component'
      end if

      if (.not. associated (sigma_midlayer)) then
        allocate (sigma_midlayer  (lbnd(1):ubnd(1),lbnd(2):ubnd(2) ,lbnd(3):ubnd(3) ), stat = istat)
        if (istat /= 0) Write (*,*) 'Error allocation of pointer sigma_midlayer in erosed'
      end if

      if (localrc == 0) then

        do k = 1,ubnd(3)

         relative_thickness_of_layers(lbnd(1):ubnd(1),lbnd(2):ubnd(2),k)= (layers_height(lbnd(1):ubnd(1),lbnd(2):ubnd(2),k) &
                                                           & -layers_height(lbnd(1):ubnd(1),lbnd(2):ubnd(2), k-1) )/ &
                                                           & (layers_height(lbnd(1):ubnd(1),lbnd(2):ubnd(2),ubnd(3)) &
                                                           & -layers_height(lbnd(1):ubnd(1),lbnd(2):ubnd(2),0) )
        end do


        do k = ubnd(3),1,-1
         if (k ==ubnd(3)) then
            sigma_midlayer (lbnd(1):ubnd(1),lbnd(2):ubnd(2),ubnd(3)) = -0.5_fp * relative_thickness_of_layers(lbnd(1):ubnd(1),lbnd(2):ubnd(2),ubnd(3))
         else
            sigma_midlayer (lbnd(1):ubnd(1),lbnd(2):ubnd(2),k) = sigma_midlayer (lbnd(1):ubnd(1),lbnd(2):ubnd(2),k+1) -0.5_fp * &
            &          ( relative_thickness_of_layers(lbnd(1):ubnd(1),lbnd(2):ubnd(2),k) +relative_thickness_of_layers(lbnd(1):ubnd(1),lbnd(2):ubnd(2),k+1) )
         endif
        end do

        do j=1,jnum
          do i= 1, inum
           ! filtering missing values (land)
           if ( mask(i,j) .gt. 0 ) then
            umod  (inum*(j -1)+i) = sqrt( u2d(i,j)*u2d(i,j) + v2d(i,j)*v2d(i,j) )

            u_bot (inum*(j -1)+i) = ubot (i,j)
            v_bot (inum*(j -1)+i) = vbot (i,j)
            if (wave) then
                tper (inum*(j -1)+i) = waveT (i,j)
                teta (inum*(j -1)+i) = WaveDir (i,j)
                uorb (inum*(j -1)+i) = CalcOrbitalVelocity (waveH(i,j), waveK(i,j), waveT(i,j), depth (i,j))
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

!-------

    !> get bio effects
    BioEffects%ErodibilityEffect = 1.0_fp
    if ( associated(microEro) ) BioEffects%ErodibilityEffect = BioEffects%ErodibilityEffect * microEro(1:inum,1:jnum)
    if ( associated(macroEro) ) BioEffects%ErodibilityEffect = BioEffects%ErodibilityEffect * macroEro(1:inum,1:jnum)

    BioEffects%TauEffect = 1.0_fp
    if ( associated(microTau) ) BioEffects%TauEffect = BioEffects%TauEffect * microTau(1:inum,1:jnum)
    if ( associated(macroTau) ) BioEffects%TauEffect = BioEffects%TauEffect * macroTau(1:inum,1:jnum)


   ! filtering missing values (land)
    do j = 1, jnum
         do i = 1, inum
            if (mask(i,j)== 0) then
                BioEffects%TauEffect (i,j) = 1.0_fp
                BioEffects%ErodibilityEffect(i,j) = 1.0_fp
                spm_concentration (i,j,:,:)    = 0.0_fp
            end if
         end do
     end do
#ifdef DEBUG
       write (*,*) 'in erosed component run:MPB and Mbalthica BioEffects%ErodibilityEffect=', BioEffects%ErodibilityEffect
       write (*,*) 'in erosed component run:MPB and Mbalthica BioEffects%TauEffect=', BioEffects%TauEffect
#endif
    call getfrac_dummy (anymud,sedtyp,nfrac,nmlb,nmub,frac,mudfrac)

    sedd90 = 1.50_fp *sedd50 ! according to manual of Delft3d page 356

    call erosed(  nmlb   , nmub   , flufflyr , mfluff , frac , mudfrac , ws_convention_factor*ws, &
                & umod   , h1     , chezy    , taub   , nfrac, rhosol  , sedd50                 , &
                & sedd90 , sedtyp , sink     , sinkf  , sour , sourf   , anymud      , wave ,  uorb, &
                & tper   , teta   , spm_concentration , BioEffects     , nybot       , sigma_midlayer, &
                & u_bot  , v_bot  , u2d      , v2d    , h0   , mask    , advancecount, taubn,eq_conc, &
                & relative_thickness_of_layers, kmaxsd, taubmax )

  n =0
    do l = 1, nfrac
      do nm = nmlb, nmub
!                rn(l,nm) = r0(l,nm) ! explicit
!!                r1(l,nm) = r0(l,nm) + dt*(sour(l,nm) + sourf(l,nm))/h0(nm) - dt*(sink(l,nm) + sinkf(l,nm))*rn(l,nm)/h1(nm)

        i=  1+ mod((nm-1),inum)
        j=  1+int ((nm-1)/inum)

        if (sedtyp (l) == 2) then !cohesive sediment
             kmx=1
        else
            kmx = kmaxsd

        end if
        if ( mask(i,j) .gt. 0 ) then
          if (bedmodel) &
            call update_sediment_mass (mass(l,nm), dt, &
              sink(l,nm) * spm_concentration(i,j,kmx,l)/1000._fp, &
              sour(l,nm), area(i,j))
          
          size_classes_of_upward_flux_of_pim_at_bottom(l)%ptr(i,j) = sour(l,nm) *1000.0_fp -  sink(l,nm) * spm_concentration(i,j,kmx,l)  ! spm_concentration is in [g m-3] and sour in [Kgm-3] (that is why the latter is multiplied by 1000.

        end if
     enddo
      !> @todo check units and calculation of sediment upward flux, rethink ssus to be taken from FABM directly, not calculated by
      !! vanrjin84. So far, we add bed source due to sinking velocity and add material to water using constant bed porosity and
      !! sediment density.

    enddo

    !> save current water level to the old water level for the next time step
    h0 = h1

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (stopTime>currTime) then
      call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(exportState, 'rms_orbital_velocity_at_soil_surface', itemType=itemType, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELD) then
      write(message, '(A)') trim(name)//' did not find field rms_orbital_velocity_at_soil_surface'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(exportState, 'rms_orbital_velocity_at_soil_surface', field=field, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, status=status, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
      write(message, '(A)') trim(name)//' received incomplete field'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, farrayPtr=rms_orbital_velocity%ptr, exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, totalLBound=totalLBound, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)



    !> @todo proper bounds checking with eLBound required here
    if (.not. (      all(lbound(rms_orbital_velocity%ptr) .eq. (/   1,   1/) ) &
                     .and. all(ubound(rms_orbital_velocity%ptr) .eq. (/inum,jnum/) ) ) ) then
      write(message, '(A)') trim(name)//' invalid field bounds in field'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if

    do j=1,jnum
      do i= 1, inum
        rms_orbital_velocity%ptr(i,j) = uorb(inum*(j -1)+i)
      end do
    end do

    call ESMF_StateGet(exportState, 'shear_stress_at_soil_surface', itemType=itemType, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELD) then
      write(message, '(A)') trim(name)//' did not find field shear_stress_at_soil_surface'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(exportState, 'shear_stress_at_soil_surface', field=field, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, status=status, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
      write(message, '(A)') trim(name)//' received incomplete field'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, farrayPtr=bottom_shear_stress%ptr, exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, totalLBound=totalLBound, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @todo proper bounds checking with eLBound required here

    do j=1,jnum
      do i= 1, inum
        bottom_shear_stress%ptr(i,j) = taub(inum*(j -1)+i)
      end do
    end do



      call ESMF_StateGet(exportState, 'shear_stress_at_soil_surface_noncohesive', itemType=itemType, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELD) then
      write(message, '(A)') trim(name)//' did not find field shear_stress_at_soil_surface_noncohesive'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(exportState, 'shear_stress_at_soil_surface_noncohesive', field=field, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, status=status, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
      write(message, '(A)') trim(name)//' received incomplete field'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, farrayPtr=bottom_shear_stress_noncoh%ptr, exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, totalLBound=totalLBound, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    do j=1,jnum
      do i= 1, inum
        bottom_shear_stress_noncoh%ptr(i,j) = taubn(inum*(j -1)+i)
      end do
    end do



      call ESMF_StateGet(exportState, 'Equilibrium_SPM_concentration_at_soil_surface_noncohesive', itemType=itemType, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELD) then
      write(message, '(A)') trim(name)//' did not find field Equilibrium_SPM_concentration_at_soil_surface_noncohesive'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(exportState, 'Equilibrium_SPM_concentration_at_soil_surface_noncohesive', field=field, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, status=status, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
      write(message, '(A)') trim(name)//' received incomplete field'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, farrayPtr=equilibrium_spm%ptr, exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, totalLBound=totalLBound, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @todo proper bounds checking with eLBound required here

    do j=1,jnum
      do i= 1, inum
        equilibrium_spm%ptr(i,j) = eq_conc(inum*(j -1)+i)
      end do
    end do

!#ifdef DEBUG

    call ESMF_StateGet(exportState,'sediment_mass_in_bed', itemType=itemType,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELD) then
      write(message, '(A)') trim(name)//' did not find field sediment_mass_in_bed'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(exportState, 'sediment_mass_in_bed', field=field,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, status=status, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
      write(message, '(A)') trim(name)//' received incomplete field'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, farrayPtr=sediment_mass,exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, totalLBound=totalLBound, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @todo proper bounds checking with eLBound required here

    do j=1,jnum
      do i= 1, inum
        sediment_mass(i,j,:) = mass(:,inum*(j -1)+i)
      end do
    end do
!#endif
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
    logical                :: clockIsPresent, isPresent
    type(ESMF_Time)        :: currTime
    type(ESMF_Clock)       :: clock
    integer                :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
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

    deallocate (taub, taubn, eq_conc)
!    deallocate (r0)
!    deallocate (r1)
!    deallocate (rn)
    deallocate (ws)
    !
    if (allocated (mass) ) deallocate (mass)
!    deallocate (massfluff)
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
    deallocate (relative_thickness_of_layers, sigma_midlayer)
    if ( associated( sediment_mass)) nullify (sediment_mass)

    call ESMF_GridCompGet(gridComp, clockIsPresent=clockIsPresent)

    if (clockIsPresent) then

      call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockDestroy(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    end if

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
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

  function CalcOrbitalVelocity (SigWaveHeight, WaveNumber, WavePeriod, WaterDepth)
   ! RMS orbital velocity (uorb) to be used later in bedbc1993 (van Rijn, 1993) according to Eq. 11.144 Delft manual
!  According to Delft3d Manual p. 366 (definition of notations) RMS orbital velocity is
! taken from wave module and is multiplied by aquare root of 2.0 to get the desired peak orbital velocity
! at the bed in bedbc1993 (vanRijn(1993)).
! routine within Delft3d.
! It should be noted that for cohesive sediment transport the orbital velocity is used for Soulsby (2004).
   implicit none
   real (ESMF_KIND_R8) :: CalcOrbitalVelocity, Hrms
   real (ESMF_KIND_R8) :: SigWaveHeight, WaveNumber, WavePeriod, WaterDepth
   real (ESMF_KIND_R8), parameter :: pi = 3.14159265358979323846_fp, g = 9.81
!   logical             :: wave
   real (ESMF_KIND_R8) :: gammax  ! ratio of wave height to the water depth
   real (ESMF_KIND_R8) :: omega, k0, k0h


     gammax = 0.2
     waterdepth = max(0.01_fp, waterdepth)
     WavePeriod = max(0.01_fp,WavePeriod )

     Hrms = SigWaveHeight/ sqrt (2.0_fp)
     Hrms = min (Hrms, gammax * WaterDepth)

     omega      = 2.0_fp*pi/WavePeriod
     k0         = omega*omega/g
     k0h        = k0*WaterDepth
     if (k0h>pi) then
         WaveNumber = k0
     elseif (k0h<0.005_fp) then
         WaveNumber = omega/sqrt(g*WaterDepth)
     else
         WaveNumber = wavenr(WaterDepth,WavePeriod,g)
     endif

     if (WaveNumber*WaterDepth<80.0_fp) then

         CalcOrbitalVelocity  = 0.5*Hrms*omega/sinh(WaveNumber*WaterDepth)
         CalcOrbitalVelocity = CalcOrbitalVelocity*sqrt(pi)/2.0

     else
         CalcOrbitalVelocity =0.0_fp
     endif

  end function  CalcOrbitalVelocity

function wavenr(h,t,ag)
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2013.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, Thsubroutine wavenr(h         ,t         ,k         ,ag        )
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2013.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, the Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!  $Id: wavenr.f90 2392 2013-03-28 14:27:50Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/2514/engines_gpl/flow2d3d/packages/io/src/output/wavenr.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Approximation of the dispersion, original sub-
!              routine Disp10 (L, T, h, g)
! Method used: relation according to linear wave theory:
!
!  = k TANH k
!
! h k = 2 pi h / L and w = (2 pi / T) sqrt (h/g), and L as
! nown.
! ational function approximation is made of the form :
!
!     1 + a1 w^2 + a2 w^4 + a3 w^6 + a4 w^8 + a5 w^10 + a6 w^12
! w^2 ---------------------------------------------------------
!     1 + b1 w^2 + b2 w^4 + b3 w^6 + b4 w^8           + a6 w^10
!
! ing the exact values for L for:
!  0.4, 0.7, 1.0, 1.3, 1.6, 1.95, 2.35, 2.9, 3.8 and 6.5,
!  a relative error less than 1.7E-6 for all w.
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
    !
!
! Local parameters
!
    real(fp)            :: wavenr
    real(hp), parameter :: a1 = 5.060219360721177D-01, a2 = 2.663457535068147D-01,&
                         & a3 = 1.108728659243231D-01, a4 = 4.197392043833136D-02,&
                         & a5 = 8.670877524768146D-03, a6 = 4.890806291366061D-03,&
                         & b1 = 1.727544632667079D-01, b2 = 1.191224998569728D-01,&
                         & b3 = 4.165097693766726D-02, b4 = 8.674993032204639D-03

! Global variables
!
    real(fp), intent(in)               :: h  !!  Waterheight

    real(fp), intent(in)               :: t  !!  Period
    real(fp), intent(in)               :: ag !!  Gravitational acceleration
!
!
! Local variables
!
    real(hp)               :: den                  ! Denominator
    real(hp)               :: kd                   ! Double value for K
    real(hp)               :: num                  ! Numerator
    real(hp)               :: ome2                 ! Omega
!
!
!! executable statements -------------------------------------------------------
!
    call init_mathconsts()
    ome2 = (2.0D0*pi_hp/real(t, hp))**2*real(h, hp)/real(ag, hp)
    !
    num = 1.0D0 +                                                               &
        & ome2*(a1 + ome2*(a2 + ome2*(a3 + ome2*(a4 + ome2*(a5 + ome2*a6)))))
    den = 1.0D0 + ome2*(b1 + ome2*(b2 + ome2*(b3 + ome2*(b4 + ome2*a6))))
    kd = sqrt(ome2*num/den)/real(h, hp)
    wavenr = real(kd, fp)
end function wavenr

end module erosed_component
