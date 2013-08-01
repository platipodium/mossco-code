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
    !
    use precision
    use bedcomposition_module
    use message_module
    use erosed_driver
    use initsed_module
    !
    implicit none
    !
    include 'sedparams.inc'
    !
    ! Variables bed composition module
    !
    type (bedcomp_data)             , pointer :: morlyr         ! bed composition data
    type (message_stack)            , pointer :: messages       ! message stack
    logical                         , pointer :: exchlyr        ! Flag for exchange layer functionality
    integer                         , pointer :: nmlb           ! first cell number
    integer                         , pointer :: nmub           ! last cell number
    integer                         , pointer :: flufflyr       ! switch for fluff layer concept
    integer                         , pointer :: idiffusion     ! switch for diffusion between layers
    integer                         , pointer :: iporosity      ! switch for porosity
    integer                         , pointer :: iunderlyr      ! Underlayer mechanism
    integer                         , pointer :: maxwarn        ! maximum number of sediment shortage warnings remaining
    integer                         , pointer :: ndiff          ! number of diffusion coefficients in vertical direction
    integer                         , pointer :: neulyr         ! number of eulerian underlayers
    integer                         , pointer :: nfrac          ! number of sediment fractions
    integer                         , pointer :: nlalyr         ! number of lagrangian underlayers
    integer                         , pointer :: nlyr           ! total number of morphological layers (transport layer + nlalyr + neulyr + base layer)
    integer                         , pointer :: updbaselyr     ! switch for computing composition of base layer
    real(fp)                        , pointer :: minmass        ! minimum erosion thickness for a sediment shortage warning [kg]
    real(fp)                        , pointer :: theulyr        ! thickness eulerian layers [m]
    real(fp)                        , pointer :: thlalyr        ! thickness lagrangian layers [m]
    real(fp)    , dimension(:)      , pointer :: dpsed          ! total sediment thickness [m]
    real(fp)    , dimension(:)      , pointer :: thtrlyr        ! thickness of transport layer [m]
    real(fp)    , dimension(:)      , pointer :: zdiff          ! depth below bed level for which diffusion coefficients are defined [m]
    real(fp)    , dimension(:,:)    , pointer :: kdiff          ! diffusion coefficients for mixing between layers [m2/s]
    real(fp)    , dimension(:,:)    , pointer :: mfluff         ! composition of fluff layer: mass of mud fractions [kg/m2]
    real(fp)    , dimension(:,:)    , pointer :: svfrac         ! 1 - porosity coefficient [-]
    real(fp)    , dimension(:,:)    , pointer :: thlyr          ! thickness of morphological layers [m]
    real(fp)    , dimension(:,:,:)  , pointer :: msed           ! composition of morphological layers: mass of sediment fractions [kg/m2]
    real(prec)  , dimension(:,:)    , pointer :: bodsed         ! total sediment mass [kg/m2]

    !
    ! Local variables
    !
    character(message_len)                      :: message      ! message
    integer                                     :: i            ! diffusion layer counter
    integer                                     :: l            ! sediment counter
    integer                                     :: nm           ! cell counter
    integer                                     :: nstep        ! cell counter
    integer                                     :: istat        ! error flag
    integer     , dimension(:)  , allocatable   :: sedtyp       ! sediment type [-]
    real(fp)                                    :: t            ! current time level
    real(fp)                                    :: dt           ! time step [s]
    real(fp)                                    :: g            ! gravitational acceleration [m/s2]
    real(fp)                                    :: morfac       ! morphological scale factor [-]
    real(fp)                                    :: rhow         ! density of water [kg/m3]
    real(fp)                                    :: tend         ! end time of computation [s]
    real(fp)                                    :: tstart       ! start time of computation [s]
    real(fp)    , dimension(:)  , allocatable   :: cdryb        ! dry bed density [kg/m3]
    real(fp)    , dimension(:)  , allocatable   :: chezy        ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    real(fp)    , dimension(:)  , allocatable   :: dp           ! bed level [m]
    real(fp)    , dimension(:)  , allocatable   :: dz           ! bed level change [m]
    real(fp)    , dimension(:)  , allocatable   :: h0           ! water depth old time level [m]
    real(fp)    , dimension(:)  , allocatable   :: h1           ! water depth new time level [m]
    real(fp)    , dimension(:)  , allocatable   :: logsedsig    ! standard deviation on log scale (log of geometric std.) [-]
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

    logical                                     ::lexist

!HN. ToDo: Namelist for input

!namelist /transportparam/   chezy, h1, umod, ws, r1(1,:), r1(2,:), dp

!namelist /layerparam/nfrac, iunderlyr,neulyr,nlalyr,theulyr,thlalyr,updbaselyr, &
!                   maxwarn, minmass,idiffusion,ndiff,flufflyr

!namelist /sediparam/iporosity,sedtyp(1),sedtyp(2),cdryb,rhosol,sedd50,sedd90,logsedsig
!

!namelist /userparam/ g, rhow, nmlb,nmub,tstart,tend, dt, morfac


    ! -----------------------------------------------------------


!
!! executable statements -------------------------------------------------------
!
    !
    istat   = 0
    !
    !   Constants
    !
    g       = 9.81_fp   ! gravitational acceleration [m/s2]
    rhow    = 1000.0_fp ! density of water [kg/m3]
    !
    allocate (morlyr)
    allocate (messages)
    call initstack(messages)
    !
    !   Initializing bed composition with default values
    !
    message = 'initializing bed composition module'
    call addmessage(messages, message)
    if (initmorlyr(morlyr) /= 0 ) call adderror(messages, message)
    !
    !   Initializing bed composition logical and scalar values
    !
    message = 'initializing logical and scalar values'
    call addmessage(messages, message)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'flufflayer_model_type'          , flufflyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'diffusion_model_type'           , idiffusion)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'porosity_model_type'            , iporosity)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'bed_layering_type'              , iunderlyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'number_of_fractions'            , nfrac)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'first_column_number'            , nmlb)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'last_column_number'             , nmub)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'MaxNumShortWarning'             , maxwarn)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'number_of_diffusion_values'     , ndiff)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'number_of_eulerian_layers'      , neulyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'number_of_lagrangian_layers'    , nlalyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'base_layer_updating_type'       , updbaselyr)
    if (istat == 0) istat = bedcomp_getpointer_realfp (morlyr, 'MinMassShortWarning'            , minmass)
    if (istat == 0) istat = bedcomp_getpointer_realfp (morlyr, 'thickness_of_eulerian_layers'   , theulyr)
    if (istat == 0) istat = bedcomp_getpointer_realfp (morlyr, 'thickness_of_lagrangian_layers' , thlalyr)
    if (istat /= 0) call adderror(messages, message)
    !
    ! ================================================================================
    !   USER INPUT  H.N.=> ToDo: namelist
    ! ================================================================================
    !
    nmlb    = 1                 ! first cell number
    nmub    = 1                 ! last cell number
    tstart  = 0.0               ! start time of computation [s]
    tend    = 50000.0            ! end time of computation [s]
    dt      = 100.0             ! time step [s]
    morfac  = 1.0               ! morphological scale factor [-]
    nstep  = (tend-tstart)/dt;  ! number of time steps
    !
    ! -----------------------------------------------------------
    !
    nfrac       = 2             ! number of sediment fractions
    iunderlyr   = 2             ! Underlayer mechanism (default = 1)
    neulyr      = 3             ! number of eulerian underlayers
    nlalyr      = 0             ! number of lagrangian underlayers
    theulyr     = 0.1_fp        ! thickness eulerian layers [m] (default = 0.0)
    thlalyr     = 0.2_fp        ! thickness lagrangian layers [m] (default = 0.0)
    updbaselyr  = 1             ! switch for computing composition of base layer
                                !  1: base layer is an independent layer (default)
                                !  2: base layer composition is kept fixed
                                !  3: base layer composition is set equal to the
                                !     composition of layer above it
                                !  4: base layer composition and thickness constant
    maxwarn     = 0           ! maximum number of sediment shortage warnings remaining (default = 100)
    minmass     = 0.0_fp        ! minimum erosion thickness for a sediment shortage warning [kg] (default = 0.0)
    idiffusion  = 0             ! switch for diffusion between layers
                                !  0: no diffusion (default)
                                !  1: diffusion based on mass fractions
                                !  2: diffusion based on volume fractions
    ndiff       = 5             !  number of diffusion coefficients in vertical direction (default = 0)
    flufflyr    = 1             ! switch for fluff layer concept
                                !  0: no fluff layer (default)
                                !  1: all mud to fluff layer, burial to bed layers
                                !  2: part mud to fluff layer, other part to bed layers (no burial)
    iporosity   = 0             ! switch for porosity (simulate porosity if iporosity > 0)
                                !  0: porosity included in densities, set porosity to 0 (default)
                                !  1: ...
    !
    ! ================================================================================
    !
    !   Allocating arrays
    !
    message = 'allocating bed composition module'
    call addmessage(messages, message)
    if (allocmorlyr(morlyr) /=0 ) call adderror(messages, message)
    !
    ! Initializing diffusion coefficients and corresponding depth
    !
    if (idiffusion>0) then
        message = 'initializing diffusion'
        call addmessage(messages, message)
        if (istat == 0) istat = bedcomp_getpointer_realfp(morlyr, 'diffusion_coefficients'  , kdiff)
        if (istat == 0) istat = bedcomp_getpointer_realfp(morlyr, 'diffusion_levels'        , zdiff)
        if (istat /= 0) call adderror(messages, message)
        !
        do i = 1, ndiff
            zdiff(i) = i*0.1_fp;        ! depth below bed level for which diffusion coefficients are defined [m]
            do nm = nmlb, nmub
                kdiff(i,nm) = 0.5_fp    ! diffusion coefficients for mixing between layers [m2/s]
            enddo
        enddo
        !
    endif
    !
    !   Bed composition arrays
    !
    message = 'initializing bed compositon arrays'
    call addmessage(messages, message)
    if (iunderlyr==1) then
        if (istat==0) istat = bedcomp_getpointer_realprec(morlyr, 'total_sediment_mass'         , bodsed)
        if (istat==0) istat = bedcomp_getpointer_realfp  (morlyr, 'total_sediment_thickness'    , dpsed)
    elseif (iunderlyr==2) then
        if (istat==0) istat = bedcomp_getpointer_integer(morlyr, 'number_of_layers'    , nlyr)
        if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'layer_mass'                   , msed)
        if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'layer_thickness'              , thlyr)
        if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'thickness_of_transport_layer' , thtrlyr)
        if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'solid_volume_fraction'        , svfrac)
    endif
    if (flufflyr>0) then
        if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'flufflayer_mass'              , mfluff)
    endif
    if (istat/=0) call adderror(messages, message)

!**********************************************************************
! new additions


  message = 'initializing fluff layer'
   if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'Flufflyr' , flufflyr)
   if (flufflyr>0 .and. istat == 0) istat = bedcomp_getpointer_realfp (morlyr, 'mfluff'   , mfluff)
   if (flufflyr==1) then
   if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'Bfluff0'            , bfluff0)
   if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'Bfluff1'            , bfluff1)
   endif
   if (istat /= 0) call adderror(messages, message)

 call initsed(nmlb,   nmub,   nfrac, flufflyr) !, &
!                 & alf1,    betam,  rksc,   pmcrit, bfluff0, bfluff1, &
!                 & depeff,  depfac, eropar, parfluff0,  parfluff1, &
!                 & tcrdep,  tcrero, tcrfluff)

  ! end of new additions
  !**********************************************************************


    !
    allocate (cdryb     (nfrac))
    allocate (logsedsig (nfrac))
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
    allocate (dp        (nmlb:nmub))
    allocate (dz        (nmlb:nmub))
    !
    ! ================================================================================
    !   USER INPUT
    ! ================================================================================
    !
    !   Sediment properties (see also 'sedparams.inc')
    !
    sedtyp(1)   = SEDTYP_NONCOHESIVE_SUSPENDED  ! non-cohesive suspended sediment (sand)
    sedtyp(2)   = SEDTYP_COHESIVE               ! cohesive sediment (mud)
    cdryb       = 1650.0_fp                     ! dry bed density [kg/m3]
    rhosol      = 2650.0_fp                     ! specific density [kg/m3]
    sedd50      = 0.0001_fp                     ! 50% diameter sediment fraction [m]
    sedd90      = 0.0002_fp                     ! 90% diameter sediment fraction [m]
    logsedsig   = log(1.34_fp)                  ! standard deviation on log scale (log of geometric std.) [-]
    !
    !   Initial bed composition
    !
    if (iunderlyr==2) then
        thtrlyr = 0.1_fp        ! thickness of transport layer [m]
        thlyr   = 0.1_fp        ! thickness of morphological layers [m]
        svfrac  = 1.0_fp        ! 1 - porosity coefficient [-]
        if (flufflyr>0) then
            mfluff  = 0.0_fp        ! composition of fluff layer: mass of mud fractions [kg/m2]
        endif
        msed = 0.0_fp           ! composition of morphological layers: mass of sediment fractions [kg/m2]
        do l = 1, nfrac
            msed(l,:,:) = thlyr*cdryb(l)/nfrac
        enddo
    endif
    !
    !   Initial flow conditions
    !
    chezy   = 50.0_fp       ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    h1      = 3.0_fp        ! water depth [m]
    umod    = 0.0_fp        ! depth averaged flow magnitude [m/s]
    ws      = 0.001_fp      ! Settling velocity [m/s]
    r1(1,:) = 2.0e-1_fp     ! sediment concentration [kg/m3]
    r1(2,:) = 2.0e-1_fp     ! sediment concentration [kg/m3]
    dp      = 0.0_fp        ! bed level [m]
    do nm = nmlb, nmub
        taub(nm) = umod(nm)*umod(nm)*rhow*g/(chezy(nm)*chezy(nm)) ! bottom shear stress [N/m2]
    enddo
    ! ================================================================================
    !
    !   Set sediment properties for the morphological layers
    !
    call setbedfracprop(morlyr, sedtyp, sedd50, logsedsig, cdryb)
    !


    ! Open file for producing output

    inquire (file ='delft_sediment_test.out', exist = lexist)

    if (lexist) then
        write (*,*) ' The output file "delft_sediment_test.out" already exits. It will be overwritten!!!'
        open (unit = 707, file = 'delft_sediment_test.out', status = 'REPLACE', action = 'WRITE')
    else
        open (unit = 707, file = 'delft_sediment_test.out', status = 'NEW', action = 'WRITE')
    end if

    write (707, '(A4,2x,A8,2x, A5,2(3x,A10))') 'Step','Fractions','layer','Sink(m/s)','Source(m/s)'
    ! ================================
    !   TIME LOOP
    ! ================================
    do i = 1, nstep
        !
        r0 = r1
        h0 = h1
        !
        !   Computing erosion fluxes
        !
        call erosed(morlyr    , nmlb      , nmub  , dt    , morfac     , &
                  & ws        , umod      , h0    , chezy , taub       , &
                  & nfrac     , rhosol    , sedd50, sedd90, sedtyp     , &
                  & sink      , sinkf     , sour  , sourf , messages, flufflyr, mfluff   )
        !
        !   Compute flow
        !HN. ToDo: the followings loop can be placed in a Module containing a generic procedure UPDATE
        !
        h1      = h0
        umod    = abs(1.0_fp*sin(2*3.14*i/nstep))
        !
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

             write (707, '(I4,4x,I4,4x,I5,2(4x,F8.4))' ) i, l, nm, sink(l,nm), sour (l,nm)
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
        !   Updating bed composition
        !
        message = 'updating bed composition'
        if (updmorlyr(morlyr, mass, massfluff, rhosol, dt, morfac, dz, messages) /= 0) call adderror(messages, message)
        !
        print *, dz
        !
        !   Updating bed level
        !
        dp = dp + dz


        !
    enddo
    !
    !   Cleaning up
    !
    close (707)

    message = 'cleaning up variables'
    call addmessage(messages, message)
    if (clrmorlyr(morlyr) /= 0) call adderror(messages, message)
    !
    !   Write messages to terminal
    !
    call writemessages(messages,6)
    !
    ! H.N. ToDo: the foloowing deaalocations can be put into a module containig a subroutine "FINALIZE"
    deallocate (morlyr)
    deallocate (messages)
    deallocate (cdryb)
    deallocate (logsedsig)
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
    deallocate (ws)
    !
    deallocate (mass)
    deallocate (massfluff)
    deallocate (sink)
    deallocate (sinkf)
    deallocate (sour)
    deallocate (sourf)
    deallocate (dp)
    deallocate (dz)
    !


    deallocate (pmcrit , bfluff0, bfluff1,&
                 & depeff,  depfac, eropar, parfluff0,  parfluff1, &
                 & tcrdep,  tcrero, tcrfluff)
end program example

