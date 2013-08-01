module erosed_driver

contains


subroutine erosed(morlyr    , nmlb      , nmub  , dt    , morfac        , &
                & ws        , umod      , h     , chezy , taub          , &
                & nfrac     , rhosol    , sedd50, sedd90, sedtyp        , &
                & sink      , sinkf     , sour  , sourf , messages, flufflyr , mfluff   )
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
!  $Id: erosed.f90 7697 2012-11-16 14:10:17Z boer_aj $
!  $HeadURL: https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example/source/erosed.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes sedimentation and erosion fluxes
!
!!--declarations----------------------------------------------------------------

    use precision
    use bedcomposition_module
    use message_module
    use initsed_module
    !
    implicit none
    !
    include 'sedparams.inc'
    !
!    integer                         , pointer :: flufflyr                       ! switch for fluff layer concept

!    real(fp)    , dimension(:,:)    , pointer :: bfluff0                        ! burial coefficient 1 [kg/m2/s]
!    real(fp)    , dimension(:,:)    , pointer :: bfluff1                        ! burial coefficient 2 [1/s]
    real(fp)    , dimension(:,:)    , pointer :: mfluff                         ! composition of fluff layer: mass of mud fractions [kg/m2]
    !
    type(bedcomp_data)                          , intent(in)   :: morlyr        ! bed composition data
    ! new argument
    integer                                     , intent(in)   :: flufflyr                       ! switch for fluff layer concept
    ! new argument
    integer                                     , intent(in)   :: nfrac         ! number of sediment fractions
    integer                                     , intent(in)   :: nmlb          ! first cell number
    integer                                     , intent(in)   :: nmub          ! last cell number
    integer     , dimension(nfrac)              , intent(in)   :: sedtyp        ! sediment type
    real(fp)                                    , intent(in)   :: dt            ! time step [s]
    real(fp)                                    , intent(in)   :: morfac        ! morphological accelaration factor
    real(fp)    , dimension(nmlb:nmub)          , intent(in)   :: chezy         ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    real(fp)    , dimension(nmlb:nmub)          , intent(in)   :: h             ! water depth [m]
    real(fp)    , dimension(nfrac)              , intent(in)   :: rhosol        ! specific sediment density [kg/m3]
    real(fp)    , dimension(nmlb:nmub)          , intent(in)   :: sedd50        ! 50% diameter sediment fraction [m]
    real(fp)    , dimension(nmlb:nmub)          , intent(in)   :: sedd90        ! 90% diameter sediment fraction [m]
    real(fp)    , dimension(nmlb:nmub)          , intent(in)   :: taub          ! bottom shear stress [N/m2]
    real(fp)    , dimension(nmlb:nmub)          , intent(in)   :: umod          ! velocity magnitude (in bottom cell) [m/s]
    real(fp)    , dimension(nfrac,nmlb:nmub)    , intent(in)   :: ws            ! sediment settling velocity (hindered) [m/s]
    real(fp)    , dimension(nfrac,nmlb:nmub)    , intent(out)  :: sink          ! sediment sink flux [m/s]
    real(fp)    , dimension(nfrac,nmlb:nmub)    , intent(out)  :: sinkf         ! sediment sink flux fluff layer [m/s]
    real(fp)    , dimension(nfrac,nmlb:nmub)    , intent(out)  :: sour          ! sediment source flux [kg/m2/s]
    real(fp)    , dimension(nfrac,nmlb:nmub)    , intent(out)  :: sourf         ! sediment source flux fluff layer [kg/m2/s]
    type(message_stack)                                        :: messages      ! message stack
!
! Local variables
!
    character(message_len)                      :: message      ! message
    logical                                     :: anymud
    integer                                     :: istat
    integer                                     :: l            ! sediment counter
    integer                                     :: nm           ! cell counter
!    real(fp)                                    :: alf1         ! calibration coefficient van Rijn (1984) [-]
!    real(fp)                                    :: betam        ! power factor for adaptation of critical bottom shear stress [-]
    real(fp)                                    :: fracf
    real(fp)                                    :: mfltot
!    real(fp)                                    :: rksc         ! reference level van Rijn (1984) [m]
    real(fp)                                    :: sbot
    real(fp)                                    :: smfac        ! correction factor for critical bottom shear stress
    real(fp)                                    :: ssus
    real(fp)    , dimension(nmlb:nmub)          :: mudcnt
    real(fp)    , dimension(nmlb:nmub)          :: mudfrac      ! mud fraction [-]
!    real(fp)    , dimension(nmlb:nmub)          :: pmcrit       ! critical mud fraction [-]
    real(fp)    , dimension(nmlb:nmub)          :: seddep
    real(fp)    , dimension(nfrac          )    :: E            ! erosion velocity [m/s]
!    real(fp)    , dimension(nfrac,nmlb:nmub)    :: depeff       ! deposition efficiency [-]
!    real(fp)    , dimension(nfrac,nmlb:nmub)    :: depfac       ! deposition factor (flufflayer=2) [-]
!    real(fp)    , dimension(nfrac,nmlb:nmub)    :: eropar       ! erosion parameter for mud [kg/m2/s]
    real(fp)    , dimension(nfrac,nmlb:nmub)    :: fixfac       ! reduction factor in case of limited sediment availability [-]
    real(fp)    , dimension(nfrac,nmlb:nmub)    :: frac         ! sediment (mass) fraction [-]
!    real(fp)    , dimension(nfrac,nmlb:nmub)    :: parfluff0    ! erosion parameter 1 [s/m]
!    real(fp)    , dimension(nfrac,nmlb:nmub)    :: parfluff1    ! erosion parameter 2 [ms/kg]
    real(fp)    , dimension(nfrac,nmlb:nmub)    :: rsedeq       ! equilibrium concentration [kg/m3]
!    real(fp)    , dimension(nfrac,nmlb:nmub)    :: tcrdep       ! critical bed shear stress for mud sedimentation [N/m2]
!    real(fp)    , dimension(nfrac,nmlb:nmub)    :: tcrero       ! critical bed shear stress for mud erosion [N/m2]
!    real(fp)    , dimension(nfrac,nmlb:nmub)    :: tcrfluff     ! critical bed shear stress for fluff layer erosion [N/m2]
!
!! executable statements ------------------
!
    !
    istat   = 0
    !
    !   User defined parameters
    !
 !***********************************************************
 ! new newnewnewnewnew
  !  message = 'initializing fluff layer'
  !  if (istat == 0) istat = bedcomp_getpointer_integer(morlyr, 'Flufflyr' , flufflyr)
  !  if (flufflyr>0 .and. istat == 0) istat = bedcomp_getpointer_realfp (morlyr, 'mfluff'   , mfluff)
  !  if (flufflyr==1) then
  !      if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'Bfluff0'            , bfluff0)
  !      if (istat==0) istat = bedcomp_getpointer_realfp (morlyr, 'Bfluff1'            , bfluff1)
  !  endif
  !  if (istat /= 0) call adderror(messages, message)
    !
  !  call initsed(nmlb,   nmub,   nfrac, flufflyr, &
  !               & alf1,    betam,  rksc,   pmcrit, bfluff0, bfluff1, &
  !               & depeff,  depfac, eropar, parfluff0,  parfluff1, &
  !               & tcrdep,  tcrero, tcrfluff)
    !*******************************************************************
    !
    !
    mudcnt      = 0.0_fp
    anymud      = .true.
    !
    !   Determine fractions of all sediments in the top layer and compute the mud fraction.
    !
    call getfrac(morlyr, frac, anymud, mudcnt, mudfrac, nmlb, nmub)
    !
    !   Determine thickness of sediment
    !
    call getsedthick(morlyr, seddep)
    !
    !   Initialization
    !
    fixfac      = 1.0_fp
    rsedeq      = 0.0_fp
    ssus        = 0.0_fp
    sour        = 0.0_fp
    sink        = 0.0_fp
    sinkf       = 0.0_fp
    sourf       = 0.0_fp
    !
    !   Compute change in sediment composition (e.g. based on available fractions and sediment availability)
    !
    do nm = nmlb, nmub
        mfltot = 0.0_fp
        if (flufflyr>0) then
            do l = 1, nfrac
                mfltot = mfltot + mfluff(l,nm)
            enddo
        endif
        do l = 1, nfrac
            if (sedtyp(l)==SEDTYP_COHESIVE) then
                !
                !   Compute source and sink fluxes for cohesive sediment (mud)
                !
                fracf   = 0.0_fp
                if (mfltot>0.0_fp) fracf   = mfluff(l,nm)/mfltot
                !
                call eromud(ws(l,nm)      , fixfac(l,nm)  , taub(nm)      , frac(l,nm)     , fracf  , &
                          & tcrdep(l,nm)  , tcrero(l,nm)  , eropar(l,nm)  , flufflyr       , mfltot , &
                          & tcrfluff(l,nm), depeff(l,nm)  , depfac(l,nm)  , parfluff0(l,nm), parfluff1(l,nm) , &
                          & sink(l,nm)    , sour(l,nm)    , sinkf(l,nm)   , sourf(l,nm)               )
                 !
            else
                !
                ! Compute correction factor for critical bottom shear stress with sand-mud interaction
                !
                if ( pmcrit(nm) > 0.0_fp ) then
                    smfac = ( 1.0_fp + mudfrac(nm) ) ** betam
                else
                    smfac = 1.0_fp
                endif
                !
                !   Apply sediment transport formula ( in this case vanRijn (1984) )
                !

                call vanRijn84(umod(nm)  ,sedd50(nm),sedd90(nm),h(nm)     ,ws(l,nm)   , &
                             & rhosol(l) ,alf1      ,rksc      , &
                             & sbot      ,ssus      ,smfac     )
                !
                ssus =  ssus * rhosol(l)
                !
                !   Compute reference concentration
                !
                if (umod(nm)*h(nm)>0.0_fp) then
                    rsedeq(l,nm) = frac(l,nm) * ssus / (umod(nm)*h(nm))
                endif
                !
                !   Compute suspended sediment fluxes for non-cohesive sediment (sand)
                !
                call erosand(umod(nm)    ,chezy(nm)     ,ws(l,nm)  ,rsedeq(l,nm),  &
                           & sour(l,nm)  ,sink(l,nm)                 )
            endif
        enddo
    enddo
    !
    ! Recompute fluxes due to sand-mud interaction
    !
    do nm = nmlb, nmub
        ! Compute erosion velocities
        E = 0.0_fp
        do l = 1, nfrac
            if (frac(l,nm)>0.0_fp)  E(l) = sour(l,nm)/(rhosol(l)*frac(l,nm))
        enddo
        !
        ! Recompute erosion velocities
        !
        call sand_mud(nfrac, E, frac(:,nm), mudfrac(nm), sedtyp, pmcrit(nm))
        !
        ! Recompute erosion fluxes
        !
        do l = 1, nfrac
            sour(l,nm) = frac(l,nm)*rhosol(l)*E(l)
        enddo
    enddo
    !
    ! Add implicit part of source term to sink
    !
    do l = 1, nfrac
        do nm = nmlb, nmub
            sink(l,nm) = sink(l,nm)
        enddo
    enddo
    !
    !
end subroutine erosed

subroutine eromud(ws       , fixfac    , taub      , frac      , fracf     , &
                & tcrdep   , tcrero    , eropar    , flufflyr  , mflufftot , &
                & tcrfluff , depeff    , depfac    , parfluff0 , parfluff1 , &
                & sink     , sour      , sinkf     , sourf                    )
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
!  $Id: eromud.f90 7697 2012-11-16 14:10:17Z boer_aj $
!  $HeadURL: https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example/source/eromud.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes sediment fluxes at the bed using
!              the Partheniades-Krone formulations.
!
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    include 'sedparams.inc'
    !
    integer                                     , intent(in)   :: flufflyr      ! switch for fluff layer concept
    real(fp)                                    , intent(in)   :: eropar        ! erosion parameter for mud [kg/m2/s]
    real(fp)                                    , intent(in)   :: depeff        ! deposition efficiency [-]
    real(fp)                                    , intent(in)   :: depfac        ! deposition factor (flufflayer=2) [-]
    real(fp)                                    , intent(in)   :: fixfac        ! reduction factor in case of limited sediment availability [-]
    real(fp)                                    , intent(in)   :: frac          ! sediment (mass) fraction [-]
    real(fp)                                    , intent(in)   :: fracf         ! sediment (mass) fraction fluff layer [-]
    real(fp)                                    , intent(in)   :: mflufftot     ! total mass of fluff layer
    real(fp)                                    , intent(in)   :: parfluff0     ! erosion parameter 1 [s/m]
    real(fp)                                    , intent(in)   :: parfluff1     ! erosion parameter 2 [s/m]
    real(fp)                                    , intent(in)   :: taub          ! bottom shear stress [N/m2]
    real(fp)                                    , intent(in)   :: tcrdep        ! critical bed shear stress for mud sedimentation [N/m2]
    real(fp)                                    , intent(in)   :: tcrero        ! critical bed shear stress for mud erosion [N/m2]
    real(fp)                                    , intent(in)   :: tcrfluff      ! critical bed shear stress for fluff layer erosion [N/m2]
    real(fp)                                    , intent(in)   :: ws            ! settling velocity [m/s]
    real(fp)                                    , intent(out)  :: sink          ! sediment sink flux [m/s]
    real(fp)                                    , intent(out)  :: sinkf         ! sediment sink flux [m/s]
    real(fp)                                    , intent(out)  :: sour          ! sediment source flux [kg/m2/s]
    real(fp)                                    , intent(out)  :: sourf         ! sediment source flux fluff layer [kg/m2/s]
!
! Local variables
!
    real(fp) :: taum
!
!! executable statements ------------------
!
    sour    = 0.0_fp
    sourf   = 0.0_fp
    sink    = 0.0_fp
    sinkf   = 0.0_fp
    !
    ! Default Partheniades-Krone formula
    !
    taum = 0.0_fp
    if (tcrero>0.0_fp) then
        taum = max(0.0_fp, taub/tcrero - 1.0_fp)
    endif
    sour = eropar * taum
    if (tcrdep > 0.0) then
        sink = max(0.0_fp , 1.0_fp-taub/tcrdep)
    endif
    !
    ! Erosion and deposition to fluff layer
    !
    if (flufflyr>0) then
        taum    = max(0.0_fp, taub - tcrfluff)
        sourf   = min(mflufftot*parfluff1,parfluff0)*taum
        sinkf   = depeff
        sink    = 0.0_fp
    endif
    if (flufflyr==2) then
        sinkf   = (1.0_fp - depfac)*sinkf
        sink    = depeff*depfac
    endif
    !
    !   Sediment source and sink fluxes
    !
    sink    = ws * sink
    sinkf   = ws * sinkf
    sour    = fixfac * frac  * sour
    sourf   =          fracf * sourf
    !
end subroutine eromud

subroutine erosand(umod     , chezy    , ws    , rsedeq    , &
                 & sour     , sink                 )
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
!  $Id: erosand.f90 7697 2012-11-16 14:10:17Z boer_aj $
!  $HeadURL: https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example/source/erosand.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes the sour and sink terms for the 2D case
!              (Gallappatti aproach)
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    real(fp), intent(in)  :: chezy      ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    real(fp), intent(in)  :: umod       ! velocity magnitude (in bottom cell) [m/s]
    real(fp), intent(in)  :: ws         ! sediment settling velocity (hindered) [m/s]
    real(fp), intent(in)  :: rsedeq     ! equilibrium concentration [kg/m3]
    real(fp), intent(out) :: sour       ! sediment source flux [m/s]
    real(fp), intent(out) :: sink       ! sediment sink flux [m/s]
!
! Local variables
!
    real(fp) :: b
    real(fp) :: eps
    real(fp) :: hots
    real(fp) :: sg
    real(fp) :: tsd
    real(fp) :: u
    real(fp) :: ulog
    real(fp) :: ustarc
    real(fp) :: w
    real(fp) :: wsl
    real(fp) :: x
    real(fp) :: x2
    real(fp) :: x3
!
!! executable statements -------------------------------------------------------
!
    eps     = 1.0e-6
    sg      = sqrt(9.81_fp)
    !
    sour    = 0.0_fp
    sink    = 0.0_fp
    !
    ! local bed shear stress due to currents
    !
    ustarc  = umod*sg/chezy
    !
    wsl = max(1.0e-3_fp,ws)
    if (umod > eps .and. ustarc > eps) then
        !
        ! compute relaxation time using the Gallappatti formulations
        !
        u = ustarc/umod
        !
        ! limit u to prevent overflow in tsd below
        !
        u = min(u, 0.15_fp)
        if (ustarc > wsl) then
            w = wsl/ustarc
        else
            w = 1.0
        endif
        b    = 1.0
        x    = w/b
        x2   = x*x
        x3   = x2*x
        ulog = log(u)
        tsd  = x*exp((  1.547           - 20.12*u )*x3 &
           &     + (326.832 *u**2.2047 -  0.2   )*x2 &
           &     + (  0.1385*ulog      -  6.4061)*x  &
           &     + (  0.5467*u         +  2.1963) )
        !
        hots = wsl/tsd
        sour = rsedeq*hots
        sink = hots
    else
        sink = wsl
    endif
end subroutine erosand


subroutine sand_mud(nfrac, E, frac, mudfrac, sedtyp, pmcrit)
!
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
!  $Id: sand_mud.f90 7697 2012-11-16 14:10:17Z boer_aj $
!  $HeadURL: https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example/source/sand_mud.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes erosion velocities based
!              on sand-mud interaction (Van Ledden (2003), Van Kessel (2002))
!              Array E is recomputed.
! Method used:
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    include 'sedparams.inc'
    !
    integer                                     , intent(in)    :: nfrac    ! number of sediment fractions
    integer     , dimension(nfrac)              , intent(in)    :: sedtyp   ! sediment type
    real(fp)    , dimension(nfrac)              , intent(in)    :: frac     ! sediment (mass) fraction [-]
    real(fp)                                    , intent(in)    :: mudfrac  ! mud fraction [-]
    real(fp)                                    , intent(in)    :: pmcrit   ! critical mud fraction [-]
    real(fp)    , dimension(nfrac)              , intent(inout) :: E        ! sediment erosion velocity [m/s]
!
! Local variables
!
    integer                         :: istat        ! error flag
    integer                         :: l            ! sediment counter
    real(fp)                        :: Es_avg       ! average erosion velocity for sand fractions [m/s]
    real(fp)                        :: Em_avg       ! average erosion velocity for mud fractions [m/s]
    !
!
!! executable statements ------------------
!
    !
    ! No sand mud interaction if there is no mud, only mud or pmcrit<0
    if (pmcrit<0.0_fp) return
    if (comparereal(mudfrac,0.0_fp)==0) return
    if (comparereal(mudfrac,1.0_fp)==0) return
    !
    Es_avg = 0.0_fp
    Em_avg = 0.0_fp
    !
    ! Compute average erosion velocity for sand fractions
    !
    do l = 1, nfrac
        if (sedtyp(l)/= SEDTYP_COHESIVE) then
            Es_avg = Es_avg + frac(l)*E(l)
        endif
    enddo
    Es_avg = Es_avg/(1-mudfrac)
    !
    if ( mudfrac <= pmcrit ) then
        !
        ! Non-cohesive regime
        ! (mud is proportionally eroded with the sand)
        !
        do l = 1, nfrac
            if (sedtyp(l) == SEDTYP_COHESIVE ) then
                if (Es_avg>0.0_fp) then
                    E(l) = Es_avg
                else
                    E(l) = 0.0_fp
                endif
            endif
        enddo
    else
        !
        ! Cohesive regime
        !
        ! erosion velocity for mud is interpolated between the non-cohesive and fully mud regime
        ! fully mud regime   : mudfrac = 1       -> E(l) is not changed
        ! non-cohesive regime: mudfrac = pmcrit  -> E(l) = Es_avg
        !
        do l = 1, nfrac
            if ( sedtyp(l)==SEDTYP_COHESIVE ) then
                if (Es_avg>0.0_fp .and. E(l)>0.0_fp) then
                    E(l) = E(l)*(Es_avg/E(l))**((1.0_fp-mudfrac)/(1.0_fp-pmcrit))
                else
                    E(l) = 0.0_fp
                endif
                Em_avg     = Em_avg + frac(l)*E(l)
            endif
        enddo
        Em_avg = Em_avg/mudfrac
        !
        ! sand is proportionally eroded with the mud
        !
        do l = 1, nfrac
            if (sedtyp(l) /= SEDTYP_COHESIVE) then
                E(l) = Em_avg
            endif
        enddo
    endif
end subroutine sand_mud

function shld(dstar     )
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011.
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
!  $Id: shld.f90 7697 2012-11-16 14:10:17Z boer_aj $
!  $HeadURL: https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example/source/shld.f90 $
!!--description-----------------------------------------------------------------
!
! determines shields parameter according
! to shields curve
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    real(fp), intent(in) :: dstar ! critical dimensionless grain size parameter
    real(fp)         :: shld
!
!! executable statements -------------------------------------------------------
!
    if (dstar<=4.) then
       shld = 0.240/dstar
    elseif (dstar<=10.) then
       shld = 0.140/dstar**0.64
    elseif (dstar<=20.) then
       shld = 0.040/dstar**0.10
    elseif (dstar<=150.) then
       shld = 0.013*dstar**0.29
    else
       shld = 0.055
    endif
end function shld



subroutine vanRijn84(utot      ,d50       ,d90       ,h         ,ws       , &
                   & rhosol    ,alf1      ,rksc      , &
                   & sbot      ,ssus      ,smfac     )
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011.
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
!  $Id: vanRijn84.f90 7697 2012-11-16 14:10:17Z boer_aj $
!  $HeadURL: https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example/source/vanRijn84.f90 $
!!--description-----------------------------------------------------------------
!
! computes sediment transport according to
! van rijn (1984)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    real(fp)               , intent(in)  :: alf1
    real(fp)               , intent(in)  :: d50     ! grain size diameter (first specified diameter)
    real(fp)               , intent(in)  :: d90     ! grain size diameter (first specified diameter)
    real(fp)               , intent(in)  :: h       ! water depth
    real(fp)               , intent(in)  :: rhosol  ! density of sediment
    real(fp)               , intent(in)  :: rksc
    real(fp)               , intent(out) :: sbot    ! bed load transport
    real(fp)               , intent(in)  :: smfac   ! factor for sand-mud interaction
    real(fp)               , intent(out) :: ssus    ! suspended sediment transport
    real(fp)               , intent(in)  :: utot    ! flow velocity
    real(fp)               , intent(in)  :: ws      ! settling velocity
!
! Local variables
!
    real(fp)       :: a
    real(fp)       :: ah
    real(fp)       :: beta   ! lowest level of integration interval over vertical
    real(fp)       :: ca
    real(fp)       :: del
    real(fp)       :: dstar
    real(fp)       :: fc
    real(fp)       :: ff     ! coriolis coefficient
    real(fp)       :: ag     ! gravity acceleration
    real(fp)       :: psi
    real(fp)       :: rhowat ! density of water
    real(fp)       :: rmuc
    real(fp)       :: rnu    ! laminar viscosity of water
    real(fp)       :: t      ! time in seconds
    real(fp)       :: tbc
    real(fp)       :: tbce
    real(fp)       :: tbcr
    real(fp)       :: thetcr
    real(fp)       :: ustar
    real(fp)       :: vonkar
    real(fp)       :: zc
   ! real(fp)       :: shld
!
!! executable statements -------------------------------------------------------
!
    sbot = 0.0
    ssus = 0.0
    !
    ag      = 9.81_fp
    rhowat  = 1000.0_fp
    del     = (rhosol - rhowat) / rhowat
    rnu     = 1e-6_fp
    vonkar  = 0.41_fp
    !
    if (h/rksc<1.33 .or. utot<1.E-3) then
       return
    endif
    !
    a = rksc
    dstar = d50*(del*ag/rnu/rnu)**(1./3.)
    !
    rmuc = (log10(12.*h/rksc)/log10(12.*h/3./d90))**2
    fc = .24*(log10(12.*h/rksc))**( - 2)
    tbc = .125*rhowat*fc*utot**2
    tbce = rmuc*tbc
    thetcr = shld(dstar)
    tbcr = (rhosol - rhowat)*ag*d50*thetcr*smfac
    t = (tbce - tbcr)/tbcr
    !
    if (t<.000001) t = .000001
    ca = .015*alf1*d50/a*t**1.5/dstar**.3
    !
    ustar = sqrt(.125*fc)*utot
    zc = 0.
    beta = 1. + 2.*(ws/ustar)**2
    beta = min(beta, 1.5_fp)
    psi = 2.5*(ws/ustar)**0.8*(ca/0.65)**0.4
    if (ustar>0.) zc = ws/vonkar/ustar/beta + psi
    if (zc>20.) zc = 20.
    ah = a/h
    fc = 0.
    if (abs(zc - 1.2)>1.E-4) then
       fc = (ah**zc - ah**1.2)/(1. - ah)**zc/(1.2 - zc)
    else
       fc = -(ah/(1. - ah))**1.2*log(ah)
    endif
    ff = fc
    ssus = ff*utot*h*ca
    !
    if (t<3.) then
       sbot = 0.053*(del)**0.5*sqrt(ag)*d50**1.5*dstar**( - 0.3)*t**2.1
    else
       sbot = 0.100*(del)**0.5*sqrt(ag)*d50**1.5*dstar**( - 0.3)*t**1.5
    endif
end subroutine vanRijn84


end module erosed_driver
