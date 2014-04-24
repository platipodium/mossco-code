program example
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2012.
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
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!  $Id: example.f90 7697 2012-11-16 14:10:17Z boer_aj $
!  $HeadURL: https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example/example.f90 $
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

    use erosed_driver
   ! use BioTypes , only :  BioturbationEffect
    !
    implicit none

  !  type BioturbationEffect
  !    real (fp),dimension(:,:,:), pointer  :: TauEffect => null()          !effect on critical bed shear stress
  !    real (fp),dimension(:,:,:), pointer  :: ErodibilityEffect => null()  !effect on erodibility parameter
  !    real (fp)          , pointer  :: d50=> null()                 !effect on changing sediment grain distribution
  !    real (fp)          , pointer  :: MudContent=> null()
  !  end type BioturbationEffect

    !type (BioturbationEffect)                           :: bioeffects
    !
    integer                                    :: nmlb           ! first (vertical) column number
    integer                                    :: nmub           ! last (vertical) column number-> as a matter of fact number of cells in plain
    integer                                    :: flufflyr       ! switch for fluff layer concept
    integer                                    :: iunderlyr      ! Underlayer mechanism
    integer                                    :: nfrac          ! number of sediment fractions
    real(fp)    , dimension(:,:)    , pointer :: mfluff         ! composition of fluff layer: mass of mud fractions [kg/m2]
    real(fp)    , dimension(:,:)    , pointer :: frac
    !
    ! Local variables
    !
    integer                                     :: i            ! diffusion layer counter
    integer                                     :: l            ! sediment counter
    integer                                     :: nm           ! cell counter
    integer                                     :: nstep        ! cell counter
   ! integer                                     :: istat        ! error flag
    integer     , dimension(:)  , allocatable   :: sedtyp       ! sediment type [-]
    real(fp)                                    :: dt           ! time step [s]
    real(fp)                                    :: g            ! gravitational acceleration [m/s2]
    real(fp)                                    :: morfac       ! morphological scale factor [-]
    real(fp)                                    :: rhow         ! density of water [kg/m3]
    real(fp)                                    :: tend         ! end time of computation [s]
    real(fp)                                    :: tstart       ! start time of computation [s]
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
    real(fp)    , dimension(:,:), allocatable   :: r0           ! concentration old time level[kg/m3]
    real(fp)    , dimension(:,:), allocatable   :: r1           ! concentration new time level[kg/m3]
    real(fp)    , dimension(:,:), allocatable   :: rn           ! concentration [kg/m3]
    real(fp)    , dimension(:,:), allocatable   :: sink         ! sediment sink flux [m/s]
    real(fp)    , dimension(:,:), allocatable   :: sinkf        ! sediment sink flux fluff layer [m/s]
    real(fp)    , dimension(:,:), allocatable   :: sour         ! sediment source flux [kg/m2/s]
    real(fp)    , dimension(:,:), allocatable   :: sourf        ! sediment source flux fluff layer [kg/m2/s]
    real(fp)    , dimension(:,:), allocatable   :: ws           ! settling velocity [m/s]
    real(fp)    , dimension(:)  , allocatable   :: mudfrac
    logical                                     ::lexist, anymud
    integer                    :: UnitNr, istat, ii, j
    logical                    :: opnd, exst

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



    inquire ( file = 'globaldata.nml', exist=exst , opened =opnd, Number = UnitNr )
    write (*,*) 'globaldata.nml exists ', exst, 'opened ', opnd, ' file unit', UnitNr

if (exst.and.(.not.opnd)) then
 UnitNr = 567

 open (unit = UnitNr, file = 'globaldata.nml', action = 'read ', status = 'old', iostat=istat )
if (istat/=0) write (*,*) 'error by openning unit number ', UnitNr
 write (*,*) ' in erosed standalone ', UnitNr, ' was just opened'

 read (UnitNr, nml=globaldata, iostat = istat)
if (istat/=0) write (*,*) 'error by reading from unit number ', UnitNr
 write (*,*)' g= ', g
write (*,*) ' rhow= ', rhow
 close (UnitNr)
end if
!
!! executable statements -------------------------------------------------------
!
    !   Constants
    !
!    g       = 9.81_fp   ! gravitational acceleration [m/s2]
!    rhow    = 1000.0_fp ! density of water [kg/m3]
!    !
!    !
!    ! ================================================================================
!    !   USER INPUT  H.N.=> ToDo: namelist
!    ! ================================================================================
!    !
!    nmlb    = 1                 ! first cell number
!    nmub    = 1                 ! last cell number
    tstart  = 0.0               ! start time of computation [s]
    tend    = 5000.0            ! end time of computation [s]
    dt      = 100.0             ! time step [s]
!    morfac  = 1.0               ! morphological scale factor [-]
    nstep  = (tend-tstart)/dt;  ! number of time steps
    !
    ! -----------------------------------------------------------
    !
!    nfrac       = 2             ! number of sediment fractions
!    iunderlyr   = 2             ! Underlayer mechanism (default = 1)
!    flufflyr    = 1             ! switch for fluff layer concept
!                                !  0: no fluff layer (default)
                                !  1: all mud to fluff layer, burial to bed layers
                                !  2: part mud to fluff layer, other part to bed layers (no burial)

    ! ================================================================================


        inquire ( file = 'benthic.nml', exist=exst , opened =opnd, Number = UnitNr )
    write (*,*) 'benthic.nml exists ', exst, 'opened ', opnd, ' file unit', UnitNr

if (exst.and.(.not.opnd)) then
 UnitNr = 568

 open (unit = UnitNr, file = 'benthic.nml', action = 'read ', status = 'old')
 write (*,*) ' in erosed-standalone ', UnitNr, ' was just opened'
if (istat/=0) write (*,*) 'error by openning unit number ', UnitNr

 read (UnitNr, nml=benthic, iostat = istat)
if (istat/=0) write (*,*) 'error by reading from unit number ', UnitNr

write (*,*)' nmlb ', nmlb, 'nmub ',  nmub, 'morfac ', morfac, 'nfrac ', nfrac, 'iunderlyr', iunderlyr &
    & , ' flufflyr', flufflyr,' anymud ', anymud

close (UnitNr)
end if


    Write (*, *) 'Initializing some sediment parameters ...'

    call initerosed(nmlb,   nmub,   nfrac )

       !
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
    allocate (r0        (nfrac,nmlb:nmub))
    allocate (r1        (nfrac,nmlb:nmub))
    allocate (rn        (nfrac,nmlb:nmub))
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

   inquire ( file = 'sedparams.txt', exist=exst , opened =opnd, Number = UnitNr )
    write (*,*) 'sedparams.txt exists ', exst, 'opened ', opnd, ' file unit', UnitNr

if (exst.and.(.not.opnd)) then
 UnitNr = 569

 open (unit = UnitNr, file = 'sedparams.txt', action = 'read ', status = 'old')

if (istat/=0) write (*,*) 'error by openning unit number ', UnitNr

 read (UnitNr,*, iostat = istat) (sedtyp(i),i=1,nfrac)
if (istat/=0) write (*,*) 'error by reading  sedtyp '

 write (*,*) 'sedtyp(i) ', (sedtyp(i),i=1,nfrac)
 if (istat ==0 ) read (UnitNr,*,iostat = istat) ( cdryb(i), i=1, nfrac)
 write (*,*) ' cdryb(i)', ( cdryb(i), i=1, nfrac)
 if (istat ==0 ) read (UnitNr,*, iostat = istat) (rhosol(i), i=1, nfrac)
 write (*,*)'rhosol(i) ',(rhosol(i), i=1, nfrac)
 if (istat ==0 ) read (UnitNr,*, iostat = istat) (sedd50(i), i=1, nfrac)
 write (*,*) 'sedd50(i) ', (sedd50(i), i=1, nfrac)
 if (istat ==0 ) read (UnitNr,*, iostat = istat) (sedd90(i), i=1, nfrac)
 write (*,*) 'sedd90(i) ', (sedd90(i), i=1, nfrac)
 if (istat ==0 ) read (UnitNr,*, iostat = istat) ((frac(i,j), i=1, nfrac), j=nmlb,nmub)
 write (*,*)' frac(i,j) ', ((frac(i,j), i=1, nfrac), j=nmlb,nmub)
  ! cohesive sediment
 if (istat ==0 ) read (UnitNr,*, iostat = istat) ((eropar(i,j), i=1, nfrac), j=nmlb,nmub)   ! erosion parameter for mud [kg/m2/s]
 if (istat ==0 ) read (UnitNr,*, iostat = istat) ((tcrdep(i,j), i=1, nfrac), j=nmlb,nmub)   ! critical bed shear stress for mud sedimentation [N/m2]
 if (istat ==0 ) read (UnitNr,*, iostat = istat) ((tcrero(i,j), i=1, nfrac), j=nmlb,nmub)   ! critical bed shear stress for mud erosion [N/m2]
 ! fluff layer
 if (istat ==0 ) read (UnitNr,*, iostat = istat) ((depeff(i,j), i=1, nfrac), j=nmlb,nmub)   ! deposition efficiency [-]
 if (istat ==0 ) read (UnitNr,*, iostat = istat) ((depfac(i,j), i=1, nfrac), j=nmlb,nmub)   ! deposition factor (flufflayer=2) [-]
 if (istat ==0 ) read (UnitNr,*, iostat = istat) ((parfluff0(i,j), i=1, nfrac), j=nmlb,nmub)! erosion parameter 1 [s/m]
 if (istat ==0 ) read (UnitNr,*, iostat = istat) ((parfluff1(i,j), i=1, nfrac), j=nmlb,nmub)! erosion parameter 2 [ms/kg]
 if (istat ==0 ) read (UnitNr,*, iostat = istat) ((tcrfluff(i,j), i=1, nfrac), j=nmlb,nmub) ! critical bed shear stress for fluff layer erosion [N/m2]
 ! cohesive sediment
 if (istat ==0 ) read (UnitNr,*, iostat = istat) (pmcrit (i), i = nmlb,nmub)
 if (istat ==0 ) read (UnitNr,*, iostat = istat) betam                                      ! power factor for adaptation of critical bottom shear stress [-]
 ! sediment transport formulation
 if (istat ==0 ) read (UnitNr,*, iostat = istat) alf1                                       ! calibration coefficient van Rijn (1984) [-]
 if (istat ==0 ) read (UnitNr,*, iostat = istat) rksc

 if (istat /=0) write (*,*) ' Error in reading sedparams !!!!'
 close (UnitNr)
end if

!initializing Bioeffects
!if (.not.associated(BioEffects%ErodibilityEffect)) allocate (BioEffects%ErodibilityEffect(1,1,1))
!if (.not.associated(BioEffects%TauEffect)) allocate (BioEffects%TauEffect(1,1,1))
!
!BioEffects%ErodibilityEffect = 4.0
!BioEffects%TauEffect = 0.5

    !
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


!       inquire ( file = 'transportparam.nml', exist=exst , opened =opnd, Number = UnitNr )
!    write (*,*) 'exist ', exst, 'opened ', opnd, ' file unit', UnitNr

!if (exst.and.(.not.opnd)) then
! UnitNr = 570

! open (unit = UnitNr, file = 'transportparam.nml', action = 'read ', status = 'old', delim = 'APOSTROPHE')
! write (*,*) ' in erosed-ESMF-component ', UnitNr, ' was just opened'

! read (UnitNr, nml=transportparam, iostat = istat)

! close (UnitNr)
!end if

    !   Initial flow conditions
    !
    chezy   = 50.0_fp       ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    h1      = 3.0_fp        ! water depth [m]
    umod    = 0.0_fp        ! depth averaged flow magnitude [m/s]
    ws      = 0.001_fp      ! Settling velocity [m/s]
    r1(1,:) = 2.0e-1_fp     ! sediment concentration [kg/m3]
    r1(2,:) = 2.0e-1_fp     ! sediment concentration [kg/m3]


    do nm = nmlb, nmub
        taub(nm) = umod(nm)*umod(nm)*rhow*g/(chezy(nm)*chezy(nm)) ! bottom shear stress [N/m2]
    enddo
    ! ================================================================================


    ! Open file for producing output

    inquire (file ='delft_sediment_test.out', exist = lexist)

    if (lexist) then
        write (*,*) ' The output file "delft_sediment_test.out" already exits. It will be overwritten!!!'
        open (unit = 707, file = 'delft_sediment_test.out', status = 'REPLACE', action = 'WRITE')
    else
        open (unit = 707, file = 'delft_sediment_test.out', status = 'NEW', action = 'WRITE')
    end if

    write (707, '(A4,2x,A8,2x, A5,3x,A10,3x,A11,4x,A5,6x,A7)') &
        'Step','Fractions','layer','Sink(m/s)','Source(m/s)', 'nfrac', 'mudfrac'
    write (*, '(A4,2x,A8,2x, A5,3x,A10,3x,A11,4x,A5,6x,A7)') &
        'Step','Fractions','layer','Sink(m/s)','Source(m/s)', 'nfrac', 'mudfrac'
    ! ================================
    !   TIME LOOP
    ! ================================


    write (*,*) ' Start running ...'

    do i = 1, nstep

    !
    !   Determine fractions of all sediments in the top layer and compute the mud fraction.

        call getfrac_dummy (anymud,sedtyp,nfrac,nmlb,nmub,frac,mudfrac)
       ! write (*,*) ' getfrac just finished '
!
        !
        r0 = r1
        h0 = h1

        !
        !   Computing erosion fluxes
        call erosed( nmlb     , nmub    , flufflyr , mfluff ,frac , mudfrac, &
                & ws        , umod    , h0        , chezy  , taub          , &
                & nfrac     , rhosol  , sedd50   , sedd90 , sedtyp        , &
                & sink      , sinkf   , sour     , sourf , anymud            )
        !   Compute flow
       ! write (*,*) ' erosed finished for step ',i, 'from total step ', nstep
        !HN. ToDo: the followings loop can be placed in a Module containing a generic procedure UPDATE
        !
        h1      = h0
        umod    = abs(1.0_fp*sin(2*3.14*i/nstep))
        !

!write (*,*) ' umod =', umod

        do nm = nmlb, nmub
            taub(nm) = umod(nm)*umod(nm)*rhow*g/(chezy(nm)*chezy(nm))
        enddo
        !
        !   Updating sediment concentration in water column
        !
        do l = 1, nfrac
            do nm = nmlb, nmub
                rn(l,nm) = r0(l,nm) ! explicit
!                r1(l,nm) = r0(l,nm) + dt*(sour(l,nm) + sourf(l,nm))/h0(nm) - dt*(sink(l,nm) + sinkf(l,nm))*rn(l,nm)/h1(nm)

             write (707, '(I4,4x,I4,4x,I5,4(4x,F8.4))' ) i, l, nm, sink(l,nm), sour (l,nm),frac (l,nm), mudfrac(nm)
          !   write (*,  '(I4,4x,I4,4x,I5,4(4x,F8.4))' )  i, l, nm, sink(l,nm), sour (l,nm),frac (l,nm), mudfrac(nm)
            enddo
        enddo
        !
        !   Compute change in sediment composition of top layer and fluff layer
        !
       mass       = 0.0_fp    ! change in sediment composition of top layer, [kg/m2]
       massfluff  = 0.0_fp    ! change in sediment composition of fluff layer [kg/m2]
        !
       do l = 1, nfrac
            do nm = nmlb, nmub
                !
                ! Update dbodsd value at nm
                !
                mass(l, nm) = mass(l, nm) + dt*morfac*( sink(l,nm)*rn(l,nm) - sour(l,nm) )
                !
                ! Update dfluff value at nm
                !
                if (flufflyr>0) then
                    massfluff(l, nm) = massfluff(l, nm) + dt*( sinkf(l,nm)*rn(l,nm) - sourf(l,nm) )
                endif
            enddo
        enddo
        !

        !
    enddo
    !
    write (*,*) ' Calculating sediment fluxes at bed has just finished'
    !   Cleaning up
    !
    close (707)


    ! H.N. ToDo: the following dealocations can be put into a module containig a subroutine "FINALIZE"

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
    deallocate (r0)
    deallocate (r1)
    deallocate (rn)
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
    ! @todo uncomment next line
    !deallocate (pmcrit , depeff,  depfac, eropar, parfluff0,  parfluff1, &
    !             & tcrdep,  tcrero, tcrfluff)

end program example

