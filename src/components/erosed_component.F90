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

module erosed_component

  use esmf
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

  ! Dimensions (x,y,depth layer, fraction index)
  real(ESMF_KIND_R8), dimension(:,:,:), pointer :: size_classes_of_upward_flux_of_pim_at_bottom
  real(ESMF_KIND_R8), dimension(:,:,:), pointer :: size_classes_of_downward_flux_of_pim_at_bottom
  type(ESMF_Field)            :: upward_flux_Field, downward_flux_Field
  integer,dimension(:),allocatable              :: external_idx_by_nfrac,nfrac_by_external_idx
  integer                     :: ubnd(4),lbnd(4)


   integer                                     :: nmlb           ! first cell number
   integer                                     :: nmub           ! last cell number
   integer                                     :: flufflyr       ! switch for fluff layer concept
   integer                                     :: iunderlyr      ! Underlayer mechanism
   integer                                     :: nfrac          ! number of sediment fractions
   real(fp)    , dimension(:,:)    , pointer   :: mfluff         ! composition of fluff layer: mass of mud fractions [kg/m2]
   real(fp)    , dimension(:,:)    , pointer   :: frac
    !
    ! Local variables
    !
    integer                                     :: i            ! diffusion layer counter
    integer                                     :: l            ! sediment counter
    integer                                     :: nm           ! cell counter
   ! integer                                     :: istat        ! error flag
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
!    real(fp)    , dimension(:,:), allocatable   :: r0           ! concentration old time level[kg/m3]
!    real(fp)    , dimension(:,:), allocatable   :: r1           ! concentration new time level[kg/m3]
!    real(fp)    , dimension(:,:), allocatable   :: rn           ! concentration [kg/m3]
    real(fp)    , dimension(:,:), allocatable   :: sink         ! sediment sink flux [m/s]
    real(fp)    , dimension(:,:), allocatable   :: sinkf        ! sediment sink flux fluff layer [m/s]
    real(fp)    , dimension(:,:), allocatable   :: sour         ! sediment source flux [kg/m2/s]
    real(fp)    , dimension(:,:), allocatable   :: sourf        ! sediment source flux fluff layer [kg/m2/s]
    real(fp)    , dimension(:,:), allocatable   :: ws           ! settling velocity [m/s]
    real(fp)    , dimension(:)  , allocatable   :: mudfrac
    logical                                     ::lexist, anymud



contains

  !> Provide an ESMF compliant SetServices routine, which defines
  !! the entry points for Init/Run/Finalize
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  !> Initialize the component
  !!
  !! Allocate memory for boundaries and fluxes, create ESMF fields
  !! and export them
  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: importState, exportState
    type(ESMF_Clock)    :: parentClock
    integer, intent(out)     :: rc

    type(ESMF_Grid)      :: grid
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array)     :: array
    real(ESMF_KIND_R8),dimension(:),pointer :: LonCoord,LatCoord,DepthCoord
    real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2
    type(ESMF_FieldBundle) :: upward_flux_bundle,downward_flux_bundle,fieldBundle
    type(ESMF_Field),dimension(:),allocatable :: fieldlist

    type(ESMF_Time)   :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8) :: dt
    character(len=80)  :: title
    character(len=256) :: din_variable='',pon_variable=''
    integer(ESMF_KIND_I8) :: nlev,n

    integer                    :: UnitNr, istat,ii,j
    logical                    :: opnd, exst

    character(ESMF_MAXSTR):: name, message, timeString
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: currTime
    logical               :: clockIsPresent



    namelist /globaldata/ g, rhow
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
   namelist /benthic/   anymud       != .true.






!    namelist /sedparams/ sedtyp(1)   !1= SEDTYP_NONCOHESIVE_SUSPENDED  ! non-cohesive suspended sediment (sand)
!    namelist /sedparams/ sedtyp(2)   !2= SEDTYP_COHESIVE               ! cohesive sediment (mud)
!    namelist /sedparams/ cdryb       != 1650.0_fp                     ! dry bed density [kg/m3]
!    namelist /sedparams/ rhosol      != 2650.0_fp                     ! specific density [kg/m3]
!    namelist /sedparams/ sedd50      != 0.0001_fp                     ! 50% diameter sediment fraction [m]
!    namelist /sedparams/ sedd90      != 0.0002_fp                     ! 90% diameter sediment fraction [m]

!    namelist /sedparams/ frac        != 0.5_fp


    rc = ESMF_SUCCESS

    !! Check whether there is already a clock (it might have been set
    !! with a prior ESMF_gridCompCreate() call.  If not, then create
    !! a local clock as a clone of the parent clock, and associate it
    !! with this component.  Finally, set the name of the local clock
    call ESMF_GridCompGet(gridComp, name=name, clockIsPresent=clockIsPresent, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (clockIsPresent) then
      call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    else
      clock = ESMF_ClockCreate(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompSet(gridComp, clock=clock, rc=rc)
    endif
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock, name=trim(name)//' clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Log the call to this function
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

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
    nlev=nmub-nmlb+1


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


    !Initialization
    sink = 0.0_fp
    sour = 0.0_fp
    sinkf=0.0_fp
    sourf=0.0_fp
    mass =0.0_fp
    massfluff=0.0_fp
    mudfrac = 0.0_fp
    mfluff =0.0_fp
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
    inquire (file ='delft_sediment_test.out', exist = lexist)

    if (lexist) then
  !      write (*,*) ' The output file "delft_sediment_test.out" already exits. It will be overwritten!!!'
        open (unit = 707, file = 'delft_sediment_test.out', status = 'REPLACE', action = 'WRITE')
    else
        open (unit = 707, file = 'delft_sediment_test.out', status = 'NEW', action = 'WRITE')
    end if

    write (707, '(A4,2x,A8,2x, A5,7x,A13,3x,A14,4x,A5,6x,A7, 10x, A4, 8x, A8)') &
        'Step','Fractions','layer','Sink(g/m^2/s)','Source(g/m^2/s)', 'nfrac', 'mudfrac', 'taub', 'sink vel'


    !> create grid

     grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/1,1/), &
           coordSys= ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,&
            name="Erosed grid",  coordTypeKind=ESMF_TYPEKIND_R8, rc=rc)
     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
     call ESMF_GridAddCoord(grid, rc=rc)
     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    allocate (size_classes_of_upward_flux_of_pim_at_bottom(1,1,nfrac))
    size_classes_of_upward_flux_of_pim_at_bottom(1,1,:) = sink(:,1)-sour(:,1)

!> not used fo export State, since sink,sour are used by bed module
!    allocate (size_classes_of_downward_flux_of_pim_at_bottom(1,1,nfrac))
!    size_classes_of_downward_flux_of_pim_at_bottom(1,1,:) = sour (:,1)

    !> create export fields

  allocate(external_idx_by_nfrac(nfrac))
  allocate(nfrac_by_external_idx(nfrac))
  external_idx_by_nfrac(:)=-1
  nfrac_by_external_idx(:)=-1
  !> first try to get "external_index" from "concentration_of_SPM" fieldBundle in import State
  call ESMF_StateGet(importState,"concentration_of_SPM",fieldBundle,rc=rc)
  if(rc /= ESMF_SUCCESS) then
    write(0,*) 'erosed_component: cannot find field bundle "concentration_of_SPM". Possibly &
            & number of SPM fractions do not match with fabm_component.'
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
      call ESMF_AttributeGet(fieldlist(n),'external_index',external_idx_by_nfrac(n),rc=rc)
      !> if attribute not present, set external idx to -1
      if(rc /= ESMF_SUCCESS) external_idx_by_nfrac(n)=-1
    end do
  end if

  !> @todo change mapping from order of SPM fields in fieldbundle to trait-related
  !!       mapping by e.g. d50. It is unknown here, which SPM fraction in water is
  !!       related to SPM fractions in the bed module
  !! after having external_index defined by nfrac, create nfrac_by_external_idx:
  do n=1,ubound(external_idx_by_nfrac,1)
    nfrac_by_external_idx(external_idx_by_nfrac(n))=n
  end do

  upward_flux_bundle = ESMF_FieldBundleCreate(name='concentration_of_SPM_upward_flux',rc=rc)
  downward_flux_bundle = ESMF_FieldBundleCreate(name='concentration_of_SPM_downward_flux',rc=rc)
  do n=1,nfrac
    ptr_f2 => size_classes_of_upward_flux_of_pim_at_bottom(:,:,n)
    upward_flux_field = ESMF_FieldCreate(grid, farrayPtr=ptr_f2, &
            name='concentration_of_SPM_upward_flux', rc=rc)
    call ESMF_AttributeSet(upward_flux_field,'external_index',external_idx_by_nfrac(n))
    call ESMF_FieldBundleAdd(upward_flux_bundle,(/upward_flux_field/),multiflag=.true.,rc=rc)
  end do
  call ESMF_StateAddReplace(exportState,(/upward_flux_bundle,downward_flux_bundle/),rc=rc)

  call ESMF_LogWrite('Initialized Delft erosed component',ESMF_LOGMSG_INFO)

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    use BioTypes , only :  BioturbationEffect

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=255)       :: logstring
    type(ESMF_Time)          :: clockTime
    type(ESMF_TimeInterval)  :: timestep
    integer(ESMF_KIND_I8)    :: advancecount
    real(ESMF_KIND_R8)       :: runtimestepcount,dt
    real(kind=ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2
    real(kind=ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3,u,v,spm_concentration
    real(kind=ESMF_KIND_R8)  :: diameter
    type(ESMF_Field)         :: Microphytobenthos_erodibility,Microphytobenthos_critical_bed_shearstress, &
    &                            Macrofauna_erodibility,Macrofauna_critical_bed_shearstress
    integer                  :: n
    type(ESMF_Field)         :: field
    type(ESMF_Field),dimension(:),allocatable :: fieldlist
    type(ESMF_FieldBundle)   :: fieldBundle
    logical                  :: forcing_from_coupler=.true.
    real(kind=ESMF_KIND_R8),parameter :: porosity=0.1 !> @todo make this an import field (e.g. by bed component)
    real(kind=ESMF_KIND_R8),parameter :: ws_convention_factor=-1.0
    type (BioturbationEffect):: BioEffects

    integer               :: petCount, localPet
    character(ESMF_MAXSTR):: name, message, timeString
    logical               :: clockIsPresent
    type(ESMF_Time)       :: currTime
    type(ESMF_Clock)      :: clock
    integer               :: external_index
    real(kind=ESMF_KIND_R8):: vonkar, ustar, z0cur

!#define DEBUG
    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (.not.clockIsPresent) then
      call ESMF_LogWrite('Required clock not found in '//trim(name), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,currTime=currTime, advanceCount=advanceCount, &
      runTimeStepCount=runTimeStepCount, timeStep=timeStep, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,I8)') trim(timestring)//' '//trim(name)//' running step ',advanceCount
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    call ESMF_TimeIntervalGet(timestep,s_r8=dt,rc=rc)

    if (.not.associated(spm_concentration)) allocate(spm_concentration(1,1,nfrac))

    !> get import state
    if (forcing_from_coupler) then

      !> get water depth
      call mossco_state_get(importState,(/'water_depth_at_soil_surface'/),ptr_f2,rc)
      if (rc == 0) then
        h0 = ptr_f2(1,1)
      else
        h0=h1
      endif

      !> get u,v and use bottom layer value
      call mossco_state_get(importState,(/'x_velocity_in_water'/),u,rc)
      call mossco_state_get(importState,(/'y_velocity_in_water'/),v,rc)
      if (rc == 0) then
        umod = sqrt( u(1,1,1)**2 + v(1,1,1)**2 )
      else
        umod = 0.2
      end if

      !> get bio effects

      call ESMF_StateGet(importState,'Effect_of_MPB_on_sediment_erodibility_at_bottom', &
                                                    Microphytobenthos_erodibility,rc=rc)
      if (rc==0) then

       if (.not.associated(BioEffects%ErodibilityEffect)) allocate (BioEffects%ErodibilityEffect(1,1,1))

       call ESMF_FieldGet (field = Microphytobenthos_erodibility, farrayPtr=ptr_f3, rc=rc)

        BioEffects%ErodibilityEffect = ptr_f3
#ifdef DEBUG
        write (*,*) 'in erosed component run:MPB BioEffects%ErodibilityEffect=', BioEffects%ErodibilityEffect
#endif
      end if

      call ESMF_StateGet(importState,'Effect_of_Mbalthica_on_sediment_erodibility_at_bottom', &
                                                           Macrofauna_erodibility,rc=rc)
      if (rc==0) then

        if (.not. associated (BioEffects%ErodibilityEffect)) then

            allocate (BioEffects%ErodibilityEffect(1,1,1))

            BioEffects%ErodibilityEffect = 1.0

        end if

        call ESMF_FieldGet (field = Macrofauna_erodibility, farrayPtr=ptr_f3, rc=rc)

        BioEffects%ErodibilityEffect = ptr_f3 * BioEffects%ErodibilityEffect
#ifdef DEBUG
        write (*,*) 'in erosed component run:MPB and Mbalthica BioEffects%ErodibilityEffect=', BioEffects%ErodibilityEffect
#endif

      end if

      call ESMF_StateGet(importState,'Effect_of_MPB_on_critical_bed_shearstress', &
                                      Microphytobenthos_critical_bed_shearstress ,rc=rc)
      if (rc==0) then

         if (.not.associated(BioEffects%TauEffect)) allocate (BioEffects%TauEffect(1,1,1))

         call ESMF_FieldGet (field = Microphytobenthos_critical_bed_shearstress , farrayPtr=ptr_f3, rc=rc)

         BioEffects%TauEffect = ptr_f3

      endif

      call ESMF_StateGet(importState,'Effect_of_Mbalthica_on_critical_bed_shearstress', &
                           Macrofauna_critical_bed_shearstress ,rc=rc)
      if (rc==0) then
         if (.not.associated(BioEffects%TauEffect)) then

            allocate (BioEffects%TauEffect(1,1,1))

            BioEffects%TauEffect =1.0

          end if

         call ESMF_FieldGet (field = Macrofauna_critical_bed_shearstress , farrayPtr=ptr_f3, rc=rc)

         BioEffects%TauEffect = ptr_f3 * BioEffects%TauEffect

      endif

      !> get spm concentrations
      call ESMF_StateGet(importState,'concentration_of_SPM',fieldBundle,rc=rc)
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
          call ESMF_AttributeGet(field,'mean_particle_diameter',sedd50(nfrac_by_external_idx(external_index)))
          call ESMF_AttributeGet(field,'particle_density',rhosol(nfrac_by_external_idx(external_index)))
          sedd90(n) = d90_from_d50(sedd50(nfrac_by_external_idx(external_index)))

          if (rc == ESMF_SUCCESS) then
            spm_concentration(1,1,nfrac_by_external_idx(external_index)) = ptr_f3(1,1,1)
          else
            write(0,*) 'cannot find SPM fraction',n
          end if
        end do

        !> this is not good, but should work:
!        r0(:,nmub) = spm_concentration(1,1,:)

        !> get sinking velocities
        call ESMF_StateGet(importState,'concentration_of_SPM_z_velocity',fieldBundle,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldBundleGet(fieldBundle,fieldlist=fieldlist,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        do n=1,size(fieldlist)
          call ESMF_FieldGet(fieldlist(n),farrayPtr=ptr_f3,rc=rc)
          call ESMF_AttributeGet(fieldlist(n),'external_index',external_index,defaultvalue=-1)
          ws(nfrac_by_external_idx(external_index),nmub) = ptr_f3(1,1,1)
        end do
      end if

    else
      !> use initial values
      h0=h1
!      r0=r1
    end if

    taub= 0.0_fp
    z0cur=0.0_fp
    ustar= 0.0_fp
    vonkar = 0.4_fp
  ! Loop over all cells
    do nm = nmlb, nmub
     do l = 1, nfrac
      z0cur = 3.0 * sedd90(l)
      !write (707,*) 'z0cur', z0cur
      ustar = umod (nm)*vonkar/log(1. + h0(nm)/12.0/z0cur)
      !write (707,*)'ustar',ustar
      taub(nm) = taub(nm)+ rhow*ustar**2
      !write (707,*) 'tau', taub(nm)
     enddo
     !write (707,*) 'nfrac', nfrac
     taub(nm) = taub(nm)/(nfrac*1.0)
     !write (*,*) 'tau', taub(nm)
    enddo


    umod =umod * 1.2

    call getfrac_dummy (anymud,sedtyp,nfrac,nmlb,nmub,frac,mudfrac)

    !   Computing erosion fluxes

    if (.not.associated(BioEffects%ErodibilityEffect)) then

    call erosed( nmlb     , nmub    , flufflyr , mfluff ,frac , mudfrac, &
                & ws_convention_factor*ws        , umod    , h0        , chezy  , taub          , &
                & nfrac     , rhosol  , sedd50   , sedd90 , sedtyp        , &
                & sink      , sinkf   , sour     , sourf ,anymud             )
    else

     call erosed( nmlb     , nmub    , flufflyr , mfluff ,frac , mudfrac, &
                & ws_convention_factor*ws        , umod    , h0        , chezy  , taub          , &
                & nfrac     , rhosol  , sedd50   , sedd90 , sedtyp        , &
                & sink      , sinkf   , sour     , sourf  , anymud, BioEffects )
    end if





    !   Updating sediment concentration in water column over cells
    do l = 1, nfrac
      spm_concentration(1,1,l) = max (0.0_fp, spm_concentration(1,1,l) )
     do nm = nmlb, nmub
!                rn(l,nm) = r0(l,nm) ! explicit
!!                r1(l,nm) = r0(l,nm) + dt*(sour(l,nm) + sourf(l,nm))/h0(nm) - dt*(sink(l,nm) + sinkf(l,nm))*rn(l,nm)/h1(nm)

             write (707, '(I4,4x,I4,4x,I5,6(4x,F11.4))' ) advancecount, l, nm, sink(l,1)*spm_concentration(1,1,l), sour (l,nm)*1000.0,frac (l,nm), mudfrac(nm), taub(nm), sink(l,nm)
     enddo
      !> @todo check units and calculation of sediment upward flux, rethink ssus to be taken from FABM directly, not calculated by
      !! vanrjin84. So far, we add bed source due to sinking velocity and add material to water using constant bed porosity and
      !! sediment density.

      size_classes_of_upward_flux_of_pim_at_bottom(1,1,l) = &
          sour(l,1) *1000.0_fp - sink(l,1)*spm_concentration(1,1,l)

!          write (*,*) 'SPM',l,'=', spm_concentration(1,1,l)
    !      write (*,*) 'sour*1000.0', sour(l,1) *1000.0_fp
      !write (*,*) 'concentration', spm_concentration(1,1,l)
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

  end subroutine Run

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

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (.not.clockIsPresent) then
      call ESMF_LogWrite('Required clock not found in '//trim(name), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' finalizing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

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
    !! @todo uncomment next line
    !deallocate (pmcrit , depeff,  depfac, eropar, parfluff0,  parfluff1, &
    !             & tcrdep,  tcrero, tcrfluff)

    !! @todo The clockIsPresent statement does not detect if a clock has been destroyed 
    !! previously, thus, we comment the clock destruction code while this has not
    !! been fixed by ESMF
    !if (clockIsPresent) call ESMF_ClockDestroy(clock, rc=rc)
    !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finalized'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=rc)



  end subroutine Finalize

  function d90_from_d50(d50)
  real(ESMF_KIND_R8)            :: d90_from_d50
  real(ESMF_KIND_R8),intent(in) :: d50
  d90_from_d50 = 2.0_ESMF_KIND_R8 * d50
  end function d90_from_d50

end module erosed_component
