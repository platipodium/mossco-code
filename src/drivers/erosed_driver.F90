!> @brief Implementation of a simplified erosed driver
!
!> This module implements the computation of sediment flux.
!! The program computes the sediment entrainment and erosion rates for given
!! fractions of cohesive and noncohesive sediment.

!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2012 Stichting Deltares
!>                          2013 Bundesanstalt für Wasserbau
!> @author Hassan Nasermoaddeli, Bundesanstalt für Wasserbau
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module erosed_driver

!This Module needs the following subroutines form delft-example programs:
!eromud.f90
!erosand.f90
!sand_mud.f90
!vanRijn84.f90
!  $HeadURL: https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example/source/     $
!!--description-----------------------------------------------------------------

!and also following Modules form delft-example programs:
!precision.f90
!precision_basics.f90
!-------------------------------------------------------------------------------
!  $Id: precision.f90 7697 2012-11-16 14:10:17Z boer_aj $
!  $HeadURL: https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example/modules/  $
!!--description-----------------------------------------------------------------

! and finally the following include file:
!sedparams.inc

!-------------------------------------------------------------------------------
!  $Id: sedparams.inc 7697 2012-11-16 14:10:17Z boer_aj $
!  $HeadURL: https://svn.oss.deltares.nl/repos/openearthtools/trunk/programs/SandMudBedModule/03_Fortran/example/example/include/sedparams.inc $$
!-------------------------------------------------------------------------------


use precision
use Biotypes ,only: BioturbationEffect

implicit none

! global parameters
!integer, parameter :: sp=kind(1.0e00)
!integer, parameter :: hp=kind(1.0d00)
!
! double precision integers:
!

!integer, parameter :: fp=hp

! These Parameters are defined in sedparam.inc seperately for delft-routine
integer, parameter :: SEDTYP_NONCOHESIVE_TOTALLOAD = 0
integer, parameter :: SEDTYP_NONCOHESIVE_SUSPENDED = 1
integer, parameter :: SEDTYP_COHESIVE              = 2

! Definition of the object classes
type , public :: mud_argument
    real(fp),    pointer      :: depeff   =>null()        ! deposition efficiency [-]
    real(fp),    pointer      :: depfac   =>null()        ! deposition factor (flufflayer=2) [-]
    real(fp),    pointer      :: eropar   =>null()        ! erosion parameter for mud [kg/m2/s]
    real(fp),    pointer      :: parfluff0=>null()     ! erosion parameter 1 [s/m]
    real(fp),    pointer      :: parfluff1=>null()     ! erosion parameter 2 [ms/kg]
    real(fp),    pointer      :: tcrdep   =>null()        ! critical bed shear stress for mud sedimentation [N/m2]
    real(fp),    pointer      :: tcrero   =>null()        ! critical bed shear stress for mud erosion [N/m2]
    real(fp),    pointer      :: tcrfluff =>null()      ! critical bed shear stress for fluff layer erosion [N/m2]

     integer     , pointer   :: flufflyr =>null()     ! switch for fluff layer concept
     real(fp)    , pointer   :: frac     =>null()     ! sediment (mass) fraction [-]
     real(fp)    , pointer   :: ws       =>null()     ! sediment settling velocity (hindered) [m/s]
     real(fp)    , pointer   :: taub     =>null()     ! bottom shear stress [N/m2]
     real(fp)    , pointer   :: sink     =>null()     ! sediment sink flux [m/s]
     real(fp)    , pointer   :: sinkf    =>null()     ! sediment sink flux fluff layer [m/s]
     real(fp)    , pointer   :: sour     =>null()     ! sediment source flux [kg/m2/s]
     real(fp)    , pointer   :: sourf    =>null()     ! sediment source flux fluff layer [kg/m2/s]
     real(fp)    , pointer   :: fracf    =>null()
     real(fp)    , pointer   :: mfltot   =>null()
     real(fp)    , pointer   :: fixfac   =>null()    ! reduction factor in case of limited sediment availability [-]

    contains
    procedure,public, pass ::initialize =>allocate_mudargu
    procedure,public, pass ::finalize   =>deallocate_mudargu
    procedure,public, pass ::set        =>set_mudargu
    procedure,public, pass ::run        =>run_eromud
    procedure,public, pass ::get        =>get_fluxes
end type mud_argument

! Definition of the object class for erosand for future OOP
type, public :: sand_argument

    real(fp), pointer  :: chezy  =>null()
    real(fp), pointer  :: umod   =>null()
    real(fp), pointer  :: ws     =>null()
    real(fp), pointer  :: rsedeq =>null()
    real(fp), pointer  :: sour   =>null()
    real(fp), pointer  :: sink   =>null()

 contains
    procedure,public, pass :: initialize =>allocate_sandargu
    procedure,public, pass :: finalize   =>deallocate_sandargu
    procedure,public, pass :: set        =>set_sandargu
    procedure,public, pass :: run        =>run_erosand
    procedure,public, pass :: get        =>get_fluxes_s
!

end type sand_argument

type , public :: vanrijn_argument

    real(fp)               , pointer  :: alf1  =>null()
    real(fp)               , pointer  :: sedd50=>null()    ! grain size diameter (first specified diameter)
    real(fp)               , pointer  :: sedd90=>null()   ! grain size diameter (first specified diameter)
    real(fp)               , pointer  :: h     =>null()    ! water depth
    real(fp)               , pointer  :: rhosol=>null()  ! density of sediment
    real(fp)               , pointer  :: rksc  =>null()
    real(fp)               , pointer  :: sbot  =>null()  ! bed load transport
    real(fp)               , pointer  :: smfac =>null()  ! factor for sand-mud interaction
    real(fp)               , pointer  :: ssus  =>null()  ! suspended sediment transport
    real(fp)               , pointer  :: umod  =>null()  ! flow velocity
    real(fp)               , pointer  :: ws    =>null()  ! settling velocity

 contains
    procedure,public, pass :: initialize  =>allocate_vanrijnargu
    procedure,public, pass :: finalize    =>deallocate_vanrijnargu
    procedure,public, pass :: set         =>set_vanrijnargu
    procedure,public, pass :: run         =>run_vanrijn
    procedure,public, pass :: get         =>get_sediment_capacity


end type vanrijn_argument

type , public :: sandmud_argument

    integer                                 , pointer    :: nfrac    ! number of sediment fractions
    integer     , dimension(:)              , pointer    :: sedtyp   ! sediment type
    real(fp)    , dimension(:)              , pointer    :: frac     ! sediment (mass) fraction [-]
    real(fp)                                , pointer    :: mudfrac  ! mud fraction [-]
    real(fp)                                , pointer    :: pmcrit   ! critical mud fraction [-]
    real(fp)    , dimension(:)              , pointer    :: E        ! sediment erosion velocity [m/s]

contains
    procedure,public, pass :: initialize   =>allocate_sandmud
    procedure,public, pass :: finalize     =>deallocate_sandmud
    procedure,public, pass :: set          =>set_sandmud
    procedure,public, pass :: run          =>run_sandmud
    procedure,public, pass :: get          =>get_erosion_velocity

end type sandmud_argument



save

    real(fp)                                      :: alf1          ! calibration coefficient van Rijn (1984) [-]
    real(fp)                                      :: betam         ! power factor for adaptation of critical bottom shear stress [-]
    real(fp)                                      :: rksc          ! reference level van Rijn (1984) [m]
    real(fp),    allocatable, dimension(:)        :: pmcrit        ! critical mud fraction [-]
    real(fp),    allocatable, dimension(:,:)      :: depeff        ! deposition efficiency [-]
    real(fp),    allocatable, dimension(:,:)      :: depfac        ! deposition factor (flufflayer=2) [-]
    real(fp),    allocatable, dimension(:,:)      :: eropar        ! erosion parameter for mud [kg/m2/s]
    real(fp),    allocatable, dimension(:,:)      :: parfluff0     ! erosion parameter 1 [s/m]
    real(fp),    allocatable, dimension(:,:)      :: parfluff1     ! erosion parameter 2 [ms/kg]
    real(fp),    allocatable, dimension(:,:)      :: tcrdep        ! critical bed shear stress for mud sedimentation [N/m2]
    real(fp),    allocatable, dimension(:,:)      :: tcrero        ! critical bed shear stress for mud erosion [N/m2]
    real(fp),    allocatable, dimension(:,:)      :: tcrfluff      ! critical bed shear stress for fluff layer erosion [N/m2]

contains


subroutine erosed( nmlb     , nmub    , flufflyr , mfluff ,frac, mudfrac  , &
                & ws        , umod    , h        , chezy  , taub          , &
                & nfrac     , rhosol  , sedd50   , sedd90 , sedtyp        , &
                & sink      , sinkf   , sour     , sourf , Bioeffects )
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



    !
    implicit none


    type (mud_argument)                            ::      eromud_arguments
    type (sand_argument)                           ::      erosand_arguments
    type (vanrijn_argument)                        ::      vanrijn84_arguments
    type (sandmud_argument)                        ::      sandmud_arguments
    !
    real(fp)    , dimension(:,:)                , pointer      :: mfluff                         ! composition of fluff layer: mass of mud fractions [kg/m2]
    !
    integer                                     , intent(in)   :: flufflyr                       ! switch for fluff layer concept
    integer                                     , intent(in)   :: nfrac         ! number of sediment fractions
    integer                                     , intent(in)   :: nmlb          ! first cell number
    integer                                     , intent(in)   :: nmub          ! last cell number
    integer     , dimension(nfrac)              , intent(in)   :: sedtyp        ! sediment type
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
    real(fp)    , dimension(nfrac,nmlb:nmub)    , intent (in)  :: frac          ! sediment (mass) fraction [-]
    real(fp)    , dimension(nmlb:nmub)          , intent (in)  :: mudfrac       ! mud fraction [-]
    type (BioturbationEffect) , optional        , intent (in)  :: Bioeffects

!
! Local variables
!
    integer                                     :: l            ! sediment counter
    integer                                     :: nm           ! cell counter
    real(fp)                                    :: fracf
    real(fp)                                    :: mfltot
    real(fp)                                    :: sbot
    real(fp)                                    :: smfac        ! correction factor for critical bottom shear stress
    real(fp)                                    :: ssus
    real(fp)    , dimension(nfrac          )    :: E            ! erosion velocity [m/s]
    real(fp)    , dimension(nfrac,nmlb:nmub)    :: fixfac       ! reduction factor in case of limited sediment availability [-]
    real(fp)    , dimension(nfrac,nmlb:nmub)    :: rsedeq       ! equilibrium concentration [kg/m3]
!
!! executable statements ------------------
!
    !

    !   User defined parameters
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
    call eromud_arguments%initialize()
    call erosand_arguments%initialize()
    call vanrijn84_arguments%initialize()
    call sandmud_arguments%initialize(nfrac)


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
                if (present (Bioeffects)) then
#ifdef DEBUG
                 write (*,*) 'bioeffects on erodibility :', Bioeffects%ErodibilityEffect (1,1,1)

                 write (*,*) 'bioeffects on critical tau :', Bioeffects%TauEffect (1,1,1)
#endif
                 eropar(l,nm)  = eropar(l,nm) * Bioeffects%ErodibilityEffect (1,1,1)
                 tcrero(l,nm)  = tcrero(l,nm) * Bioeffects%TauEffect (1,1,1)
                endif

                fracf   = 0.0_fp
                if (mfltot>0.0_fp) fracf   = mfluff(l,nm)/mfltot
                !

                call  eromud_arguments%set ( ws(l,nm)      , fixfac(l,nm)  , taub(nm)      , frac(l,nm)     , fracf  , &
                          & tcrdep(l,nm)  , tcrero(l,nm)  , eropar(l,nm)  , flufflyr       , mfltot , &
                          & tcrfluff(l,nm), depeff(l,nm)  , depfac(l,nm)  , parfluff0(l,nm), parfluff1(l,nm) )


                call eromud_arguments%run ()

                call eromud_arguments%get(sour (l,nm), sink (l,nm), sourf (l,nm), sinkf (l,nm) )
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

                if (present (Bioeffects)) then
#ifdef DEBUG
                    write (*,*) 'bioeffects on critical tau :', Bioeffects%TauEffect (1,1,1)
#endif
                    smfac =smfac * Bioeffects%TauEffect(1,1,1)
                end if
                !
                !   Apply sediment transport formula ( in this case vanRijn (1984) )
                !

                call vanrijn84_arguments%set ( umod(nm)  ,sedd50(nm),sedd90(nm),h(nm) ,ws(l,nm), &
                             & rhosol(l) ,alf1      ,rksc ,smfac )

                call vanrijn84_arguments%run()

                call vanrijn84_arguments%get(sbot, ssus)


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
                call erosand_arguments%set (umod(nm)    ,chezy(nm)     ,ws(l,nm)  ,rsedeq(l,nm))

                call erosand_arguments%run()

                call erosand_arguments%get(sour (l,nm), sink (l,nm) )

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

        call sandmud_arguments%set (nfrac, E, frac(:,nm), mudfrac(nm), sedtyp, pmcrit(nm))
        call sandmud_arguments%run()
        call sandmud_arguments%get(E)
        !
        ! Recompute erosion fluxes
        !
        do l = 1, nfrac
            sour(l,nm) = frac(l,nm)*rhosol(l)*E(l)
        enddo
    enddo
    !
    call eromud_arguments%finalize()
    call erosand_arguments%finalize()
    call vanrijn84_arguments%finalize()
    call sandmud_arguments%finalize()

end subroutine erosed


subroutine initerosed( nmlb,   nmub,   nfrac)
  implicit none

! Global variables

  integer                                     , intent(in)   :: nfrac         ! number of sediment fractions
  integer                                     , intent(in)   :: nmlb          ! first cell number
  integer                                     , intent(in)   :: nmub          ! first cell number

  allocate (   pmcrit (nmlb:nmub)             )

  allocate (   depeff  (nfrac,nmlb:nmub)      )        ! deposition efficiency [-]
  allocate (   depfac  (nfrac,nmlb:nmub)      )        ! deposition factor (flufflayer=2) [-]
  allocate (   eropar  (nfrac,nmlb:nmub)      )        ! erosion parameter for mud [kg/m2/s]
  allocate (   parfluff0  (nfrac,nmlb:nmub)   )        ! erosion parameter 1 [s/m]
  allocate (   parfluff1  (nfrac,nmlb:nmub)   )        ! erosion parameter 2 [ms/kg]
  allocate (   tcrdep  (nfrac,nmlb:nmub)      )        ! critical bed shear stress for mud sedimentation [N/m2]
  allocate (   tcrero  (nfrac,nmlb:nmub)      )        ! critical bed shear stress for mud erosion [N/m2]
  allocate (   tcrfluff  (nfrac,nmlb:nmub)    )        ! critical bed shear stress for fluff layer erosion [N/m2]

!! executable statements ------------------
!
    ! ================================================================================
    !   USER INPUT => HN. TODO: Include these parameters in namelist
    ! ================================================================================
    !
    !   Parameters sediment
    !
    eropar      = 1.0e-2_fp     ! erosion parameter for mud [kg/m2/s]
    tcrdep      = 1000.0_fp     ! critical bed shear stress for mud sedimentation [N/m2]
    tcrero      = 1.0_fp        ! critical bed shear stress for mud erosion [N/m2]
    !
    !   Parameters fluff layer
    !
    depeff      = 0.95_fp       ! deposition efficiency [-]
    depfac      = 0.2_fp        ! deposition factor (flufflayer=2) [-]
    parfluff0   = 2.0e-1_fp     ! erosion parameter 1 [s/m]
    parfluff1   = 1.0_fp        ! erosion parameter 2 [ms/kg]
    tcrfluff    = 0.05_fp       ! critical bed shear stress for fluff layer erosion [N/m2]

    !   Parameters sand-mud interaction
    !
    betam       =  1.0_fp       ! power factor for adaptation of critical bottom shear stress [-]
    pmcrit      =  0.6_fp       ! critical mud fraction [-]
    !
    !   Parameters sediment transport formulation
    !
    alf1        = 2.0_fp        ! calibration coefficient [-]
    rksc        = 0.1_fp        ! reference level [m]
    !
    ! ================================================================================

end subroutine initerosed


subroutine getfrac_dummy (anymud,sedtyp,nfrac,nmlb,nmub,frac,mudfrac)




implicit none

    integer                                                         , intent(in)  :: nmlb
    integer                                                         , intent(in)  :: nmub
    integer                                                         , intent(in)  :: nfrac
    logical                                                         , intent(in)  :: anymud
    real(fp), dimension(nmlb:nmub, nfrac)                           , intent(out) :: frac
    real(fp), dimension(nmlb:nmub)                                  , intent(out) :: mudfrac
    integer , dimension(nfrac)                                                    :: sedtyp
    integer                                                                       :: i,j


 if (anymud) then
  do j = 1,nfrac
    do i = nmlb, nmub
      frac (i,j) = 0.5_fp
    enddo
  enddo
       !
       ! Add simulated mud fractions.
       !
       mudfrac = 0.0
       do i = 1, nfrac
          if (sedtyp(i) == SEDTYP_COHESIVE) then
             do j = nmlb, nmub
                mudfrac(j) = mudfrac(j) + frac(j,i)
             enddo
          endif
       enddo
endif
end subroutine getfrac_dummy





!*****************************************************************
!********************   Eromud_argument Methods Block  ***********
subroutine allocate_mudargu (eromud_arguments)
implicit none
class (mud_argument)    ::    eromud_arguments

    !allocate (eromud_arguments)
    allocate (eromud_arguments%depeff)
    allocate (eromud_arguments%depfac)
    allocate (eromud_arguments%eropar)
    allocate (eromud_arguments%parfluff0)
    allocate (eromud_arguments%parfluff1)
    allocate (eromud_arguments%tcrdep)
    allocate (eromud_arguments%tcrero)
    allocate (eromud_arguments%tcrfluff)
    allocate (eromud_arguments%flufflyr)
     allocate (eromud_arguments%frac)
     allocate (eromud_arguments%ws)
     allocate (eromud_arguments%taub)
     allocate (eromud_arguments%sink)
     allocate (eromud_arguments%sinkf)
     allocate (eromud_arguments%sour)
     allocate (eromud_arguments%sourf)
     allocate (eromud_arguments%fracf)
     allocate (eromud_arguments%mfltot)
     allocate (eromud_arguments%fixfac)

    eromud_arguments%sour        = 0.0_fp
    eromud_arguments%sink        = 0.0_fp
    eromud_arguments%sinkf       = 0.0_fp
    eromud_arguments%sourf       = 0.0_fp

end subroutine allocate_mudargu

subroutine deallocate_mudargu (eromud_arguments)
implicit none
class (mud_argument) ::    eromud_arguments


    deallocate (eromud_arguments%depeff)
    deallocate (eromud_arguments%depfac)
    deallocate (eromud_arguments%eropar)
    deallocate (eromud_arguments%parfluff0)
    deallocate (eromud_arguments%parfluff1)
    deallocate (eromud_arguments%tcrdep)
    deallocate (eromud_arguments%tcrero)
    deallocate (eromud_arguments%tcrfluff)
    deallocate (eromud_arguments%flufflyr)
     deallocate (eromud_arguments%frac)
     deallocate (eromud_arguments%ws)
     deallocate (eromud_arguments%taub)
     deallocate (eromud_arguments%sink)
     deallocate (eromud_arguments%sinkf)
     deallocate (eromud_arguments%sour)
     deallocate (eromud_arguments%sourf)
     deallocate (eromud_arguments%fracf)
     deallocate (eromud_arguments%mfltot)
     deallocate (eromud_arguments%fixfac)
   !  deallocate (eromud_arguments)
end subroutine deallocate_mudargu

subroutine set_mudargu(eromud_arguments,  ws  , fixfac  , taub , frac     , fracf  , &
                            & tcrdep  , tcrero  , eropar  , flufflyr       , mfltot , &
                            & tcrfluff, depeff  , depfac  , parfluff0, parfluff1 )

implicit none
 class (mud_argument) :: eromud_arguments


     integer                                    , intent(in)   :: flufflyr      ! switch for fluff layer concept
    real(fp)                                    , intent(in)   :: eropar        ! erosion parameter for mud [kg/m2/s]
    real(fp)                                    , intent(in)   :: depeff        ! deposition efficiency [-]
    real(fp)                                    , intent(in)   :: depfac        ! deposition factor (flufflayer=2) [-]
    real(fp)                                    , intent(in)   :: fixfac        ! reduction factor in case of limited sediment availability [-]
    real(fp)                                    , intent(in)   :: frac          ! sediment (mass) fraction [-]
    real(fp)                                    , intent(in)   :: fracf         ! sediment (mass) fraction fluff layer [-]
    real(fp)                                    , intent(in)   :: mfltot     ! total mass of fluff layer
    real(fp)                                    , intent(in)   :: parfluff0     ! erosion parameter 1 [s/m]
    real(fp)                                    , intent(in)   :: parfluff1     ! erosion parameter 2 [s/m]
    real(fp)                                    , intent(in)   :: taub          ! bottom shear stress [N/m2]
    real(fp)                                    , intent(in)   :: tcrdep        ! critical bed shear stress for mud sedimentation [N/m2]
    real(fp)                                    , intent(in)   :: tcrero        ! critical bed shear stress for mud erosion [N/m2]
    real(fp)                                    , intent(in)   :: tcrfluff      ! critical bed shear stress for fluff layer erosion [N/m2]
    real(fp)                                    , intent(in)   :: ws            ! settling velocity [m/s]


    eromud_arguments%depeff=depeff
    eromud_arguments%depfac=depfac
    eromud_arguments%eropar=eropar
    eromud_arguments%parfluff0=parfluff0
    eromud_arguments%parfluff1=parfluff1
    eromud_arguments%tcrdep=tcrdep
    eromud_arguments%tcrero=tcrero
    eromud_arguments%tcrfluff=tcrfluff
    eromud_arguments%flufflyr=flufflyr
    eromud_arguments%frac=frac
    eromud_arguments%ws=ws
    eromud_arguments%taub=taub
    eromud_arguments%fracf=fracf
    eromud_arguments%mfltot=mfltot
    eromud_arguments%fixfac=fixfac

end subroutine

subroutine run_eromud (eromud_arguments)
implicit none
class (mud_argument) ::eromud_arguments

call eromud(eromud_arguments%ws      , eromud_arguments%fixfac  , eromud_arguments%taub, &
            eromud_arguments%frac     , eromud_arguments%fracf  , &
            eromud_arguments%tcrdep  , eromud_arguments%tcrero , &
            eromud_arguments%eropar  , eromud_arguments%flufflyr , &
            eromud_arguments%mfltot  , eromud_arguments%tcrfluff , &
            eromud_arguments%depeff  , eromud_arguments%depfac   , &
            eromud_arguments%parfluff0, eromud_arguments%parfluff1 , &
            eromud_arguments%sink    , eromud_arguments%sour    , &
            eromud_arguments%sinkf   , eromud_arguments%sourf)

!write (*,*) ' in run_eromud eromud_arguments%sink  ', eromud_arguments%sink
!write (*,*) ' in run_eromud eromud_arguments%sour  ', eromud_arguments%sour

end subroutine run_eromud

subroutine get_fluxes (eromud_arguments, source, sink, sourcef, sinkf)
implicit none
class (mud_argument) :: eromud_arguments

real (fp), intent (out) :: source, sink, sourcef, sinkf

sink = eromud_arguments%sink
source = eromud_arguments%sour
sinkf = eromud_arguments%sinkf
sourcef = eromud_arguments%sourf

!write (*,*) ' in get_fluxes :: sink ', sink

end subroutine get_fluxes

!*******END **********   Eromud_argument Methods Block  *****END ***
!*******************************************************************





!*****************************************************************
!********************   Erosand_argument Methods Block  ***********


subroutine allocate_sandargu (erosand_arguments)

implicit none

class (sand_argument) :: erosand_arguments
integer :: istat

allocate (erosand_arguments%chezy,erosand_arguments%umod,erosand_arguments%ws, &
  erosand_arguments%rsedeq,erosand_arguments%sour,erosand_arguments%sink, stat = istat)
if (istat/=0) then
write (*,*) ' error in pointer allocation  in allocate_sandargu '
stop
end if

    erosand_arguments%sour        = 0.0_fp
    erosand_arguments%sink        = 0.0_fp


End subroutine allocate_sandargu


subroutine deallocate_sandargu (erosand_arguments)

implicit none

class (sand_argument) :: erosand_arguments

deallocate (erosand_arguments%chezy,erosand_arguments%umod,erosand_arguments%ws, &
    erosand_arguments%rsedeq,erosand_arguments%sour,erosand_arguments%sink)

End subroutine deallocate_sandargu

subroutine set_sandargu (erosand_arguments, umod  ,chezy ,ws ,rsedeq)

implicit none
class (sand_argument) :: erosand_arguments

    real(fp), intent(in)  :: chezy      ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    real(fp), intent(in)  :: umod       ! velocity magnitude (in bottom cell) [m/s]
    real(fp), intent(in)  :: ws         ! sediment settling velocity (hindered) [m/s]
    real(fp), intent(in)  :: rsedeq     ! equilibrium concentration [kg/m3]


      erosand_arguments%umod   =umod
      erosand_arguments%chezy  =chezy
      erosand_arguments%ws     =ws
      erosand_arguments%rsedeq =rsedeq

   !   write (*,*) 'in set_sandgrau :: chezy = ', chezy


end subroutine set_sandargu


subroutine run_erosand (erosand_arguments)
implicit none
class (sand_argument) :: erosand_arguments

             call erosand(erosand_arguments%umod    ,erosand_arguments%chezy, &
                          erosand_arguments%ws ,erosand_arguments%rsedeq,  &
                          erosand_arguments%sour ,erosand_arguments%sink      )

end subroutine run_erosand

subroutine get_fluxes_s (erosand_arguments, source, sink)
implicit none
class (sand_argument) :: erosand_arguments

real (fp), intent (out) :: source, sink

sink = erosand_arguments%sink
source = erosand_arguments%sour

!write (*,*) ' in get_fluxes_s :: source ', source


end subroutine get_fluxes_s

!*******END **********   Eromud_argument Methods Block  *****END ***
!*******************************************************************

!*****************************************************************
!********************   Vanrijn84 Methods Block  ***********

subroutine allocate_vanrijnargu (vanrijn84_arguments)
implicit none
class ( vanrijn_argument) :: vanrijn84_arguments

allocate ( vanrijn84_arguments%umod  ,vanrijn84_arguments%sedd50, &
           vanrijn84_arguments%sedd90,vanrijn84_arguments%h,vanrijn84_arguments%ws, &
           vanrijn84_arguments%rhosol,vanrijn84_arguments%alf1, &
           vanrijn84_arguments%rksc, vanrijn84_arguments%sbot, &
           vanrijn84_arguments%ssus      ,vanrijn84_arguments%smfac     )


end subroutine allocate_vanrijnargu

subroutine deallocate_vanrijnargu (vanrijn84_arguments)
implicit none
class ( vanrijn_argument) :: vanrijn84_arguments

deallocate ( vanrijn84_arguments%umod  ,vanrijn84_arguments%sedd50, &
             vanrijn84_arguments%sedd90,vanrijn84_arguments%h,vanrijn84_arguments%ws, &
             vanrijn84_arguments%rhosol,vanrijn84_arguments%alf1,vanrijn84_arguments%rksc, &
             vanrijn84_arguments%sbot, vanrijn84_arguments%ssus, vanrijn84_arguments%smfac)

end subroutine deallocate_vanrijnargu

subroutine set_vanrijnargu (vanrijn84_arguments, umod  ,sedd50,sedd90,h,ws , &
                             & rhosol,alf1,rksc ,smfac )
implicit none
class ( vanrijn_argument) :: vanrijn84_arguments

    real(fp)               , intent(in)  :: alf1
    real(fp)               , intent(in)  :: sedd50     ! grain size diameter (first specified diameter)
    real(fp)               , intent(in)  :: sedd90     ! grain size diameter (first specified diameter)
    real(fp)               , intent(in)  :: h       ! water depth
    real(fp)               , intent(in)  :: rhosol  ! density of sediment
    real(fp)               , intent(in)  :: rksc
    real(fp)               , intent(in)  :: smfac   ! factor for sand-mud interaction
    real(fp)               , intent(in)  :: umod    ! flow velocity
    real(fp)               , intent(in)  :: ws      ! settling velocity

vanrijn84_arguments%alf1=alf1
vanrijn84_arguments%sedd50=sedd50
vanrijn84_arguments%sedd90=sedd90
vanrijn84_arguments%h=h
vanrijn84_arguments%rhosol=rhosol
vanrijn84_arguments%rksc=rksc
vanrijn84_arguments%smfac=smfac
vanrijn84_arguments%umod=umod
vanrijn84_arguments%ws=ws

end subroutine set_vanrijnargu

subroutine run_vanrijn (vanrijn84_arguments)
implicit none
class ( vanrijn_argument) :: vanrijn84_arguments

call vanRijn84 ( vanrijn84_arguments%umod  ,vanrijn84_arguments%sedd50, &
                 vanrijn84_arguments%sedd90,vanrijn84_arguments%h,vanrijn84_arguments%ws, &
                 vanrijn84_arguments%rhosol,vanrijn84_arguments%alf1,vanrijn84_arguments%rksc, &
                 vanrijn84_arguments%sbot,vanrijn84_arguments%ssus,vanrijn84_arguments%smfac )

end subroutine run_vanrijn

subroutine get_sediment_capacity (vanrijn84_arguments, sbot ,ssus)
implicit none
class ( vanrijn_argument) :: vanrijn84_arguments

real(fp)                    ,intent(out)    :: ssus
real(fp)                    ,intent(out)    :: sbot

ssus = vanrijn84_arguments%ssus
sbot = vanrijn84_arguments%sbot


end subroutine get_sediment_capacity

!*******END **********   Vanrijn84_argument Methods Block  *****END ***
!**********************************************************************

!*****************************************************************
!********************   Vanrijn84 Methods Block  ***********

subroutine allocate_sandmud (sandmud_arguments,nofrac)
implicit none
class (sandmud_argument)   :: sandmud_arguments

integer     , intent (in)  :: nofrac

allocate (sandmud_arguments%nfrac, sandmud_arguments%E(nofrac), sandmud_arguments%frac(nofrac), &
  &  sandmud_arguments%mudfrac, sandmud_arguments%sedtyp(nofrac), sandmud_arguments%pmcrit)

end subroutine allocate_sandmud

subroutine deallocate_sandmud (sandmud_arguments)
implicit none
class (sandmud_argument)   :: sandmud_arguments

deallocate (sandmud_arguments%nfrac, sandmud_arguments%E, sandmud_arguments%frac, &
  &  sandmud_arguments%mudfrac, sandmud_arguments%sedtyp, sandmud_arguments%pmcrit)

end subroutine deallocate_sandmud

subroutine set_sandmud (sandmud_arguments, nfrac, E, frac, mudfrac, sedtyp, pmcrit)
implicit none
class (sandmud_argument)   :: sandmud_arguments

    integer                                     , intent(in)    :: nfrac    ! number of sediment fractions
    integer     , dimension(nfrac)              , intent(in)    :: sedtyp   ! sediment type
    real(fp)    , dimension(nfrac)              , intent(in)    :: frac     ! sediment (mass) fraction [-]
    real(fp)                                    , intent(in)    :: mudfrac  ! mud fraction [-]
    real(fp)                                    , intent(in)    :: pmcrit   ! critical mud fraction [-]
    real(fp)    , dimension(nfrac)              , intent(in)    :: E        ! sediment erosion velocity [m/s]

sandmud_arguments%nfrac  = nfrac
sandmud_arguments%E      = E
sandmud_arguments%frac   = frac
sandmud_arguments%mudfrac= mudfrac
sandmud_arguments%sedtyp = sedtyp
sandmud_arguments%pmcrit = pmcrit

end subroutine set_sandmud

subroutine run_sandmud(sandmud_arguments)
implicit none
class (sandmud_argument)   :: sandmud_arguments

call sand_mud(sandmud_arguments%nfrac, sandmud_arguments%E, sandmud_arguments%frac, &
              sandmud_arguments%mudfrac, sandmud_arguments%sedtyp, sandmud_arguments%pmcrit)

end subroutine run_sandmud

subroutine get_erosion_velocity (sandmud_arguments, E)
implicit none
class (sandmud_argument)   :: sandmud_arguments

real(fp)    , dimension(sandmud_arguments%nfrac)              , intent(out)    :: E        ! sediment erosion velocity [m/s]

E = sandmud_arguments%E

end subroutine get_erosion_velocity

end module erosed_driver
