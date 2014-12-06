!> @brief Implementation of an ESMF component for erosion and sedimentation
!
!> @import
!> @export
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, Helmholtz-Zentrum Geesthacht
!> @author Hassan Nasermoaddeli, Bundesanstalt für Wasserbau
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
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
  use mossco_variable_types
  
  use erosed_driver !, only : initerosed, erosed, getfrac_dummy
  use precision, only : fp
  use mossco_state

  implicit none

  public :: SetServices

  private

 ! These Parameters are defined in sedparam.inc seperately for delft-routine
 ! integer, parameter :: SEDTYP_NONCOHESIVE_TOTALLOAD = 0
 ! integer, parameter :: SEDTYP_NONCOHESIVE_SUSPENDED = 1
 ! integer, parameter :: SEDTYP_COHESIVE              = 2

  !! @todo hn: read CF documnetation for correct name of this
  !size_classes_of_upward_flux_of_pim_at_bottom

  type(MOSSCO_VariableFArray2d),dimension(:),allocatable :: importList

  ! Dimensions (x,y,depth layer, fraction index)
  real(ESMF_KIND_R8), dimension(:,:,:), pointer :: size_classes_of_upward_flux_of_pim_at_bottom
  real(ESMF_KIND_R8), dimension(:,:,:), pointer :: size_classes_of_downward_flux_of_pim_at_bottom
  type(ESMF_Field)                              :: upward_flux_Field, downward_flux_Field
  integer,dimension(:),allocatable              :: external_idx_by_nfrac,nfrac_by_external_idx
  integer                     :: ubnd(4),lbnd(4)


   integer                                      :: nmlb           ! first cell number
   integer                                      :: nmub           ! last cell number
   integer                                      :: inum, jnum     ! number of elements in x and y directions , inum * jnum== nmub - nmlb + 1
   integer                                      :: flufflyr       ! switch for fluff layer concept
   integer                                      :: iunderlyr      ! Underlayer mechanism
   integer                                      :: nfrac          ! number of sediment fractions
   real(fp)    , dimension(:,:)    , pointer    :: mfluff         ! composition of fluff layer: mass of mud fractions [kg/m2]
   real(fp)    , dimension(:,:)    , pointer    :: frac
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
    logical                                     ::lexist, anymud, wave
    real(fp)    , dimension(:)  , allocatable   :: uorb, tper, teta ! Orbital velocity [m/s], Wave period, angle between current and wave


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

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)
 
    implicit none
  
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      
    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    type(ESMF_DistGrid)    :: distgrid
    type(ESMF_ArraySpec)   :: arrayspec
    type(ESMF_Array)       :: array
    type(ESMF_Field)       :: field
    real(ESMF_KIND_R8),dimension(:)  ,pointer :: LonCoord,LatCoord,DepthCoord
    real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2, ptr2_f2
    type(ESMF_FieldBundle)                    :: upward_flux_bundle,downward_flux_bundle,fieldBundle
    type(ESMF_Field),dimension(:),allocatable :: fieldlist
    character(len=ESMF_MAXSTR)                :: foreignGridFieldName

    integer , allocatable  :: maxIndex(:)
    integer                :: rank

    type(ESMF_Time)        :: wallTime, clockTime
    type(ESMF_TimeInterval):: timeInterval
    real(ESMF_KIND_R8)     :: dt
    character(len=80)      :: title
    character(len=256)     :: din_variable='',pon_variable=''
    integer(ESMF_KIND_I8)  :: nlev,n

    integer                :: UnitNr, istat,ii,j
    logical                :: opnd, exst

    character(ESMF_MAXSTR) :: name, message, timeString
    type(ESMF_Clock)       :: clock
    type(ESMF_Time)        :: currTime
    logical                :: clockIsPresent



    namelist /globaldata/g, rhow
    namelist /benthic/   nmlb   ! = 1                 ! first cell number
    namelist /benthic/   nmub   ! = 1                 ! last cell number
    namelist /benthic/   morfac ! = 1.0               ! morphological scale factor [-]
    !
    ! -----------------------------------------------------------
    !
    namelist /benthic/   nfrac      ! = 2             ! number of sediment fractions
    namelist /benthic/   iunderlyr  ! = 2             ! Underlayer mechanism (default = 1)
    namelist /benthic/   flufflyr   ! = 1             ! switch for fluff layer concept
                                !  0: no fluff layer (default)
                                !  1: all mud to fluff layer, burial to bed layers
                                !  2: part mud to fluff layer, other part to bed layers (no burial)
   namelist /benthic/    anymud       != .true.






!    namelist /sedparams/ sedtyp(1)   !1= SEDTYP_NONCOHESIVE_SUSPENDED  ! non-cohesive suspended sediment (sand)
!    namelist /sedparams/ sedtyp(2)   !2= SEDTYP_COHESIVE               ! cohesive sediment (mud)
!    namelist /sedparams/ cdryb       != 1650.0_fp                     ! dry bed density [kg/m3]
!    namelist /sedparams/ rhosol      != 2650.0_fp                     ! specific density [kg/m3]
!    namelist /sedparams/ sedd50      != 0.0001_fp                     ! 50% diameter sediment fraction [m]
!    namelist /sedparams/ sedd90      != 0.0002_fp                     ! 90% diameter sediment fraction [m]

!    namelist /sedparams/ frac        != 0.5_fp


    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)

 !! get/set grid:
    !! rely on field with name foreignGridFieldName given as attribute and field
    !! in importState
    !! and just take the same grid&distgrid.

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
           value=foreignGridFieldName, defaultValue='none',rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (trim(foreignGridFieldName)=='none') then
     inum=1
     jnum = 1
     ! call ESMF_ArraySpecSet(array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                   maxIndex=(/inum,jnum/), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="erosed grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridAddCoord(grid, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    else
      call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=rc)
      if(rc /= ESMF_SUCCESS) then
       call ESMF_StatePrint (importstate)
       call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      end if
      call ESMF_FieldGet(field, grid=foreign_grid, rank=rank, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      if (rank<2) then
        write(message,*) 'foreign grid must be of at least rank >= 2'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      end if

      allocate(maxIndex(rank))
      call ESMF_GridGet(foreign_grid,staggerloc=ESMF_STAGGERLOC_CENTER,localDE=0, &
               computationalCount=maxIndex,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      inum=maxIndex(1)
      jnum=maxIndex(2)
      if (rank ==2) then
        !grid = foreign_Grid    !> ToDO discuss copy or link for grid
        grid = ESMF_GridCreate(foreign_grid,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      elseif (rank == 3) then
         grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                   maxIndex=maxIndex(1:2), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="erosed grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=rc)
      !  numlayers=maxIndex(3)
        call ESMF_GridAddCoord(grid, rc=rc)   !> ToDO we need to copy the coordiane from foreign Grid.
      else
        write(message,*) 'foreign grid must be of rank = 3'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      end if
      deallocate(maxIndex)
    end if

    !> create grid

!     grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/1,1/), &
!           coordSys= ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,&
!            name="Erosed grid",  coordTypeKind=ESMF_TYPEKIND_R8, rc=rc)
 !    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!
  !   if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)





  inquire ( file = 'globaldata.nml', exist=exst , opened =opnd, Number = UnitNr )
    !write (*,*) 'exist ', exst, 'opened ', opnd, ' file unit', UnitNr

  if (exst.and.(.not.opnd)) then
    UnitNr = 567
    open (unit = UnitNr, file = 'globaldata.nml', action = 'read ', status = 'old', delim = 'APOSTROPHE')
   !write (*,*) ' in erosed-ESMF-component ', UnitNr, ' was just opened'
   read (UnitNr, nml=globaldata, iostat = istat)
   close (UnitNr)
  end if
!    g       = 9.81_fp   ! gravitational acceleration [m/s2]
!    rhow    = 1000.0_fp ! density of water [kg/m3]
    !

    !
  inquire ( file = 'benthic.nml', exist=exst , opened =opnd, Number = UnitNr )

  if (exst.and.(.not.opnd)) then
    UnitNr = 568
    open (unit = UnitNr, file = 'benthic.nml', action = 'read ', status = 'old', delim = 'APOSTROPHE')
    !write (*,*) ' in erosed-ESMF-component ', UnitNr, ' was just opened'
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
 !   nlev=nmub-nmlb+1

    nmlb=1
    nmub = inum * jnum
    call initerosed( nmlb, nmub, nfrac)


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
    tper = 0.0_fp
    teta = 0.0_fp
    wave = .false.

    inquire ( file = 'sedparams.txt', exist=exst , opened =opnd, Number = UnitNr )
  !  write (*,*) 'exist ', exst, 'opened ', opnd, ' file unit', UnitNr

    if (exst.and.(.not.opnd)) then
      UnitNr = 569
      open (unit = UnitNr, file = 'sedparams.txt', action = 'read ', status = 'old')
 ! non-cohesive sediment
      read (UnitNr,*, iostat = istat) (sedtyp(i),i=1,nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) ( cdryb(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (rhosol(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (sedd50(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (sedd90(i), i=1, nfrac)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((frac(i,j), i=1, nfrac), j=nmlb,nmub)
 ! cohesive sediment
      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((eropar(i,j), i=1, nfrac), j=nmlb,nmub)   ! erosion parameter for mud [kg/m2/s]
      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((tcrdep(i,j), i=1, nfrac), j=nmlb,nmub)   ! critical bed shear stress for mud sedimentation [N/m2]
      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((tcrero(i,j), i=1, nfrac), j=nmlb,nmub)   ! critical bed shear stress for mud erosion [N/m2]
 ! cohesive sediment
      if (istat ==0 ) read (UnitNr,*, iostat = istat) (pmcrit (i), i = nmlb,nmub)
      if (istat ==0 ) read (UnitNr,*, iostat = istat) betam                                      ! power factor for adaptation of critical bottom shear stress [-]
 ! sediment transport formulation
      if (istat ==0 ) read (UnitNr,*, iostat = istat) alf1                                       ! calibration coefficient van Rijn (1984) [-]
      if (istat ==0 ) read (UnitNr,*, iostat = istat) rksc
 ! fluff layer
      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((depeff(i,j), i=1, nfrac), j=nmlb,nmub)   ! deposition efficiency [-]
      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((depfac(i,j), i=1, nfrac), j=nmlb,nmub)   ! deposition factor (flufflayer=2) [-]
      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((parfluff0(i,j), i=1, nfrac), j=nmlb,nmub)! erosion parameter 1 [s/m]
      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((parfluff1(i,j), i=1, nfrac), j=nmlb,nmub)! erosion parameter 2 [ms/kg]
      if (istat ==0 ) read (UnitNr,*, iostat = istat) ((tcrfluff(i,j), i=1, nfrac), j=nmlb,nmub) ! critical bed shear stress for fluff layer erosion [N/m2]
      if (istat /=0) write (*,*) ' Error in reading sedparams !!!!'
      close (UnitNr)
    end if
    ! ================================================================================
    !   USER INPUT
    ! ================================================================================
    !
    !   Sediment properties (see also 'sedparams.inc')
    !
!    sedtyp(1)   = SEDTYP_NONCOHESIVE_SUSPENDED  ! non-cohesive suspended sediment (sand)
!    sedtyp(2)   = SEDTYP_COHESIVE               ! cohesive sediment (mud)
!    cdryb       = 1650.0_fp                     ! dry bed density [kg/m3]
!    rhosol      = 2650.0_fp                     ! specific density [kg/m3]
!    sedd50      = 0.0001_fp                     ! 50% diameter sediment fraction [m]
!    sedd90      = 0.0002_fp                     ! 90% diameter sediment fraction [m]
!
!    frac = 0.5_fp
    !
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
    h1      = 3.0_fp        ! water depth [m]
    umod    = 0.1_fp        ! depth averaged flow magnitude [m/s]
    ws      = 0.001_fp      ! Settling velocity [m/s]
!    r1(:,:) = 2.0e-1_fp     ! sediment concentration [kg/m3]


    do nm = nmlb, nmub
        taub(nm) = umod(nm)*umod(nm)*rhow*g/(chezy(nm)*chezy(nm)) ! bottom shear stress [N/m2]
    enddo

    ! Open file for producing output
    inquire (file ='delft_sediment.out', exist = lexist)

    if (lexist) then
  !      write (*,*) ' The output file "delft_sediment_test.out" already exits. It will be overwritten!!!'
        open (unit = 707, file = 'delft_sediment.out', status = 'REPLACE', action = 'WRITE')
    else
        open (unit = 707, file = 'delft_sediment.out', status = 'NEW', action = 'WRITE')
    end if

    write (707, '(A4,2x,A8,2x, A5,7x,A13,3x,A14,4x,A5,6x,A7, 10x, A4, 8x, A8)') &
        'Step','Fractions','layer','Sink(g/m^2/s)','Source(g/m^2/s)', 'nfrac', 'mudfrac', 'taub', 'sink vel'

    allocate (size_classes_of_upward_flux_of_pim_at_bottom(inum, jnum,nfrac))

     do j=1,jnum
       do i= 1, inum
          size_classes_of_upward_flux_of_pim_at_bottom(i,j,:) = sink(:,inum*(j -1)+i)-sour(:,inum*(j -1)+i)
       end do
     end do

!> not used fo export State, since sink,sour are used by bed module
!    allocate (size_classes_of_downward_flux_of_pim_at_bottom(1,1,nfrac))
!    size_classes_of_downward_flux_of_pim_at_bottom(1,1,:) = sour (:,1)

! Advertise Import Fields
  if (wave) then

    allocate(importList(4))

    importList(1)%name  = 'wave_height'
    importList(1)%units = 'm'
    importList(2)%name  = 'wave_period'
    importList(2)%units = 's'
    importList(3)%name  = 'wave_number'
    importList(3)%units = '1/m'
    importList(4)%name  = 'wave_direction'
    importList(4)%units = 'rad'

    do i=1,size(importList)
      field = ESMF_FieldEmptyCreate(name=trim(importList(i)%name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'units',trim(importList(i)%units), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(importState,(/field/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

  end if

! Complete Import Fields
! TODO: should be moved to InitializeP2()
  !if (wave) then
  !  do i=1,size(importList)
  !    call ESMF_StateGet(importState,trim(importList(i)%name),field)
  !    allocate(importList(i)%data(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2)))
  !    call ESMF_FieldEmptyComplete(field,grid,importList(i)%data,     &
  !                                 ESMF_INDEX_DELOCAL,                      &
  !                                 totalLWidth=exclusiveLBound-totalLBound, &
  !                                 totalUWidth=totalUBound-exclusiveUBound)
  !  end do
  !end if


    !> create export fields

  allocate(external_idx_by_nfrac(nfrac))
  allocate(nfrac_by_external_idx(nfrac))

  external_idx_by_nfrac(:)=-1
  nfrac_by_external_idx(:)=-1

 !> first try to get "external_index" from "concentration_of_SPM" fieldBundle in import State
  call ESMF_StateGet(importState,"concentration_of_SPM_in_water",fieldBundle,rc=rc)
  if(rc /= ESMF_SUCCESS) then
    write(0,*) 'erosed_component: cannot find field bundle "concentration_of_SPM_in_water". Possibly &
            & number of SPM fractions do not match with fabm_pelagic component.'
  else
    call ESMF_FieldBundleGet(fieldBundle,fieldCount=j,rc=rc)
    if (allocated(fieldlist)) deallocate(fieldlist)
    allocate(fieldlist(j))
    call ESMF_FieldBundleGet(fieldBundle,fieldlist=fieldlist,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (ubound(fieldlist,1) /= nfrac) then
          write(0,*) 'Warning: number of boundary SPM concentrations doesnt match number of erosed fractions',ubound(fieldlist,1),rc
    !call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end if
    do n=1,size(fieldlist)
      !> if attribute not present, set external idx to -1
      call ESMF_AttributeGet(fieldlist(n),'external_index',external_idx_by_nfrac(n), &
      defaultValue=-1,rc=rc)
    end do
  end if

  !> @todo change mapping from order of SPM fields in fieldbundle to trait-related
  !!       mapping by e.g. d50. It is unknown here, which SPM fraction in water is
  !!       related to SPM fractions in the bed module
  !! after having external_index defined by nfrac, create nfrac_by_external_idx:

  do n=1,ubound(external_idx_by_nfrac,1)
    nfrac_by_external_idx(external_idx_by_nfrac(n))=n
  end do

  upward_flux_bundle = ESMF_FieldBundleCreate(name='concentration_of_SPM_upward_flux_at_soil_surface',rc=rc)
  downward_flux_bundle = ESMF_FieldBundleCreate(name='concentration_of_SPM_downward_flux_at_soil_surface',rc=rc)

  do n=1,nfrac
    ptr_f2 => size_classes_of_upward_flux_of_pim_at_bottom(:,:,n)
    upward_flux_field = ESMF_FieldCreate(grid, farrayPtr=ptr_f2, &
            name='concentration_of_SPM_upward_flux_at_soil_surface', rc=rc)
    call ESMF_AttributeSet(upward_flux_field,'external_index',external_idx_by_nfrac(n))
    call ESMF_FieldBundleAdd(upward_flux_bundle,(/upward_flux_field/),multiflag=.true.,rc=rc)
  end do

  call ESMF_StateAddReplace(exportState,(/upward_flux_bundle,downward_flux_bundle/),rc=rc)

    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
subroutine Run(gridComp, importState, exportState, parentClock, rc)
    use BioTypes , only :  BioturbationEffect

    type(ESMF_GridComp)      :: gridComp
    type(ESMF_State)         :: importState, exportState
    type(ESMF_Clock)         :: parentClock
    integer, intent(out)     :: rc

    character(len=255)       :: logstring
    type(ESMF_Time)          :: clockTime, stopTime
    type(ESMF_TimeInterval)  :: timestep
    integer(ESMF_KIND_I8)    :: advancecount
    real(ESMF_KIND_R8)       :: runtimestepcount,dt
    real(kind=ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2, u_mean
    real(kind=ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3,u,v,spm_concentration,grid_height
    real(kind=ESMF_KIND_R8)  :: diameter
    type(ESMF_Field)         :: Microphytobenthos_erodibility,Microphytobenthos_critical_bed_shearstress, &
    &                            Macrofauna_erodibility,Macrofauna_critical_bed_shearstress
    integer                  :: n, i, j
    type(ESMF_Field)         :: field
    type(ESMF_Field),dimension(:),allocatable :: fieldlist
    type(ESMF_FieldBundle)   :: fieldBundle
    logical                  :: forcing_from_coupler=.true.
    real(kind=ESMF_KIND_R8),parameter :: porosity=0.1 !> @todo make this an import field (e.g. by bed component)
    real(kind=ESMF_KIND_R8),parameter :: ws_convention_factor=-1.0
    type (BioturbationEffect):: BioEffects

    integer                  :: petCount, localPet
    character(ESMF_MAXSTR)   :: name, message, timeString
    logical                  :: clockIsPresent, isPresent
    type(ESMF_Time)          :: currTime
    type(ESMF_Clock)         :: clock
    integer                  :: external_index
    real(kind=ESMF_KIND_R8)  :: vonkar, ustar, z0cur, cdr, cds, summ, rhowat,vicmol, reynold
    integer    :: ubnd(3),lbnd(3)

!#define DEBUG
    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)


    allocate (u_mean(inum,jnum))
    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)


    call ESMF_ClockGet(clock,currTime=currTime, advanceCount=advanceCount, &
      runTimeStepCount=runTimeStepCount, timeStep=timeStep, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalGet(timestep,s_r8=dt,rc=rc)

    if (.not.associated(spm_concentration)) allocate(spm_concentration(inum,jnum,nfrac))

    !> get import state
    if (forcing_from_coupler) then

      !> get water depth
      call mossco_state_get(importState,(/'water_depth_at_soil_surface'/),ptr_f2,rc)
      if (rc == 0) then
        h0 = ptr_f2(1,1)
      else
        h0=h1
      endif

     ! call ESMF_StatePrint(importState)

      !> get u,v and use bottom layer value
      call mossco_state_get(importState,(/'x_velocity_in_water'/),u,lbnd,ubnd,rc)
      call mossco_state_get(importState,(/'y_velocity_in_water'/),v,lbnd,ubnd,rc)
      call mossco_state_get(importState,(/'grid_height_in_water'/),grid_height,lbnd,ubnd,rc)

      if (rc == 0) then

      u_mean(:,:) = sum (grid_height*sqrt(u**2+v**2),3)/sum(grid_height,3)
      !umod = sqrt( u(1,1,:)**2 + v(1,1,:)**2)

     do j=1,jnum
       do i= 1, inum
           umod (inum*(j -1)+i) = u_mean(i,j)
       end do
     end do


      else
        umod = 0.2
      end if

      !> get bio effects

      call ESMF_StateGet(importState,'Effect_of_MPB_on_sediment_erodibility_at_soil_surface', &
                                                    Microphytobenthos_erodibility,rc=rc)
      if (rc==0) then

       if (.not.associated(BioEffects%ErodibilityEffect)) allocate (BioEffects%ErodibilityEffect(inum, jnum))

       call ESMF_FieldGet (field = Microphytobenthos_erodibility, farrayPtr=ptr_f2, rc=rc)

        BioEffects%ErodibilityEffect = ptr_f2
#ifdef DEBUG
        write (*,*) 'in erosed component run:MPB BioEffects%ErodibilityEffect=', BioEffects%ErodibilityEffect
#endif
      end if

      call ESMF_StateGet(importState,'Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface', &
                                                           Macrofauna_erodibility,rc=rc)
      if (rc==0) then

        if (.not. associated (BioEffects%ErodibilityEffect)) then

            allocate (BioEffects%ErodibilityEffect(inum, jnum))

            BioEffects%ErodibilityEffect = 1.0

        end if

        call ESMF_FieldGet (field = Macrofauna_erodibility, farrayPtr=ptr_f2, rc=rc)

        BioEffects%ErodibilityEffect = ptr_f2 * BioEffects%ErodibilityEffect
#ifdef DEBUG
        write (*,*) 'in erosed component run:MPB and Mbalthica BioEffects%ErodibilityEffect=', BioEffects%ErodibilityEffect
#endif

      end if

      call ESMF_StateGet(importState,'Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface', &
                                      Microphytobenthos_critical_bed_shearstress ,rc=rc)
      if (rc==0) then

         if (.not.associated(BioEffects%TauEffect)) allocate (BioEffects%TauEffect(inum,jnum))

         call ESMF_FieldGet (field = Microphytobenthos_critical_bed_shearstress , farrayPtr=ptr_f2, rc=rc)

         BioEffects%TauEffect = ptr_f2

      endif

      call ESMF_StateGet(importState,'Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface', &
                           Macrofauna_critical_bed_shearstress ,rc=rc)
      if (rc==0) then
         if (.not.associated(BioEffects%TauEffect)) then

            allocate (BioEffects%TauEffect(inum,jnum))

            BioEffects%TauEffect =1.0

          end if

         call ESMF_FieldGet (field = Macrofauna_critical_bed_shearstress , farrayPtr=ptr_f2, rc=rc)

         BioEffects%TauEffect = ptr_f2 * BioEffects%TauEffect

      endif

      !> get spm concentrations
      call ESMF_StateGet(importState,'concentration_of_SPM_in_water',fieldBundle,rc=rc)
      if(rc /= ESMF_SUCCESS) then
        !> run without SPM forcing from pelagic component
#ifdef DEBUG
        call ESMF_LogWrite( &
           'field Bundle concentration_of_SPM not found, run without pelagic forcing', &
           ESMF_LOGMSG_INFO)
#endif
      else
        call ESMF_FieldBundleGet(fieldBundle,fieldCount=n,rc=rc)
        if (allocated(fieldlist)) deallocate(fieldlist)
        allocate(fieldlist(n))
        call ESMF_FieldBundleGet(fieldBundle,fieldlist=fieldlist,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        do n=1,size(fieldlist)
          field = fieldlist(n)
          call ESMF_AttributeGet(field,'external_index',external_index,defaultvalue=-1)
          call ESMF_FieldGet(field,farrayPtr=ptr_f3,rc=rc)
          call ESMF_AttributeGet(field,'mean_particle_diameter', isPresent=isPresent, rc=rc)
          if (isPresent) then
            call ESMF_AttributeGet(field,'mean_particle_diameter',sedd50(nfrac_by_external_idx(external_index)), rc=rc)
          else
            sedd50(nfrac_by_external_idx(external_index))=0.0
          endif
          call ESMF_AttributeGet(field,'particle_density', isPresent=isPresent, rc=rc)
          if (isPresent) then
            call ESMF_AttributeGet(field,'particle_density',rhosol(nfrac_by_external_idx(external_index)), rc=rc)
          else
            rhosol(nfrac_by_external_idx(external_index))=0.0
          endif
          
          if (rc == ESMF_SUCCESS) then
            spm_concentration(:,:,nfrac_by_external_idx(external_index)) = ptr_f3(:,:,1)
          else
            write(0,*) 'cannot find SPM fraction',n
          end if
        end do

        !> this is not good, but should work:
!        r0(:,nmub) = spm_concentration(1,1,:)

        !> get sinking velocities
        call ESMF_StateGet(importState,'concentration_of_SPM_z_velocity_in_water',fieldBundle,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldBundleGet(fieldBundle,fieldlist=fieldlist,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        do n=1,size(fieldlist)
          call ESMF_FieldGet(fieldlist(n),farrayPtr=ptr_f3,rc=rc)
          call ESMF_AttributeGet(fieldlist(n),'external_index',external_index,defaultvalue=-1)
         do j=1,jnum
          do i= 1, inum
           ws(nfrac_by_external_idx(external_index),inum*(j -1)+i) = ptr_f3(i,j,1)
          end do
         end do

        end do
      end if

    else
      !> use initial values
      h0=h1
!      r0=r1
    end if

! Soulsby

    summ = 0.0_fp
    rhowat = 1000.0_fp
    vicmol     = 1e-6_fp

    call getfrac_dummy (anymud,sedtyp,nfrac,nmlb,nmub,frac,mudfrac)


    call erosed( nmlb     , nmub    , flufflyr , mfluff ,frac , mudfrac, &
                & ws_convention_factor*ws        , umod    , h0        , chezy  , taub          , &
                & nfrac     , rhosol  , sedd50   , sedd90 , sedtyp        , &
                & sink      , sinkf   , sour     , sourf  , anymud,  wave, uorb, tper, teta,BioEffects )


    !   Updating sediment concentration in water column over cells
    do l = 1, nfrac
     do nm = nmlb, nmub
!                rn(l,nm) = r0(l,nm) ! explicit
!!                r1(l,nm) = r0(l,nm) + dt*(sour(l,nm) + sourf(l,nm))/h0(nm) - dt*(sink(l,nm) + sinkf(l,nm))*rn(l,nm)/h1(nm)

             j= 1+ mod(nm,inum)
             i= nm - inum*(j -1)
             write (707, '(I4,4x,I4,4x,I5,6(4x,F11.4))' ) advancecount, l, nm,min(-ws(l,nm),sink(l,nm))*spm_concentration(i,j,l) , sour (l,nm)*1000.0,frac (l,nm), mudfrac(nm), taub(nm), sink(l,nm)

       size_classes_of_upward_flux_of_pim_at_bottom(i,j,l) = &
          sour(l,nm) *1000.0_fp - min(-ws(l,nm),sink(l,nm))*spm_concentration(i,j,l)
          !sour(l,nm) *1000.0_fp - sink(l,nm)*spm_concentration(i,j,l)
    enddo
      !> @todo check units and calculation of sediment upward flux, rethink ssus to be taken from FABM directly, not calculated by
      !! vanrjin84. So far, we add bed source due to sinking velocity and add material to water using constant bed porosity and
      !! sediment density.

   enddo

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
        
    call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)    
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=rc)    
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer               :: petCount, localPet
    character(ESMF_MAXSTR)     :: name, message, timeString
    logical               :: clockIsPresent
    type(ESMF_Time)       :: currTime
    type(ESMF_Clock)      :: clock

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)

    close (707)

    deallocate (cdryb)

    deallocate (rhosol)
    deallocate (sedd50)
    deallocate (sedd90)
    !
    deallocate (chezy)
    deallocate (h0)
    deallocate (h1)
    deallocate (umod)
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

    !! @todo uncomment next line
    !deallocate (pmcrit , depeff,  depfac, eropar, parfluff0,  parfluff1, &
    !             & tcrdep,  tcrero, tcrfluff)


    call ESMF_ClockDestroy(clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "d90_from_d50"
  function d90_from_d50(d50)
    real(ESMF_KIND_R8)            :: d90_from_d50
    real(ESMF_KIND_R8),intent(in) :: d50
    
    d90_from_d50 = 2.0_ESMF_KIND_R8 * d50
    
  end function d90_from_d50

end module erosed_component
