!> @brief Implementation of a simplified erosed driver
!
!> This module implements the computation of sediment flux.
!! The program computes the sediment entrainment and erosion rates for given
!! fractions of cohesive and noncohesive sediment.

!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013 Bundesanstalt für Wasserbau
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
! as well as compbsskin.f90, bedbc1993.f90 and soursin3d.f90
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


! These Parameters are defined in sedparam.inc seperately for delft-routine
integer, parameter :: SEDTYP_NONCOHESIVE_TOTALLOAD = 0
integer, parameter :: SEDTYP_NONCOHESIVE_SUSPENDED = 1
integer, parameter :: SEDTYP_COHESIVE              = 2

! Definition of the object classes
type , public :: mud_argument
    real(fp), pointer      :: depeff   =>null()     ! deposition efficiency [-]
    real(fp), pointer      :: depfac   =>null()     ! deposition factor (flufflayer=2) [-]
    real(fp), pointer      :: eropar   =>null()     ! erosion parameter for mud [kg/m2/s]
    real(fp), pointer      :: parfluff0=>null()     ! erosion parameter 1 [s/m]
    real(fp), pointer      :: parfluff1=>null()     ! erosion parameter 2 [ms/kg]
    real(fp), pointer      :: tcrdep   =>null()     ! critical bed shear stress for mud sedimentation [N/m2]
    real(fp), pointer      :: tcrero   =>null()     ! critical bed shear stress for mud erosion [N/m2]
    real(fp), pointer      :: tcrfluff =>null()     ! critical bed shear stress for fluff layer erosion [N/m2]
    integer , pointer      :: flufflyr =>null()     ! switch for fluff layer concept
    real(fp), pointer      :: frac     =>null()     ! sediment (mass) fraction [-]
    real(fp), pointer      :: ws       =>null()     ! sediment settling velocity (hindered) [m/s]
    real(fp), pointer      :: taub     =>null()     ! bottom shear stress [N/m2]
    real(fp), pointer      :: sink     =>null()     ! sediment sink flux [m/s]
    real(fp), pointer      :: sinkf    =>null()     ! sediment sink flux fluff layer [m/s]
    real(fp), pointer      :: sour     =>null()     ! sediment source flux [kg/m2/s]
    real(fp), pointer      :: sourf    =>null()     ! sediment source flux fluff layer [kg/m2/s]
    real(fp), pointer      :: fracf    =>null()
    real(fp), pointer      :: mfltot   =>null()
    real(fp), pointer      :: fixfac   =>null()    ! reduction factor in case of limited sediment availability [-]

    contains
    procedure,public, pass ::initialize =>allocate_mudargu
    procedure,public, pass ::finalize   =>deallocate_mudargu
    procedure,public, pass ::set        =>set_mudargu
    procedure,public, pass ::run        =>run_eromud
    procedure,public, pass ::get        =>get_fluxes
end type mud_argument

! Definition of the object class for erosand for future OOP
type, public :: sand_argument

    real(fp), pointer      :: chezy  =>null()
    real(fp), pointer      :: umod   =>null()
    real(fp), pointer      :: ws     =>null()
    real(fp), pointer      :: rsedeq =>null()
    real(fp), pointer      :: sour   =>null()
    real(fp), pointer      :: sink   =>null()

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
    procedure,public, pass            :: initialize  =>allocate_vanrijnargu
    procedure,public, pass            :: finalize    =>deallocate_vanrijnargu
    procedure,public, pass            :: set         =>set_vanrijnargu
    procedure,public, pass            :: run         =>run_vanrijn
    procedure,public, pass            :: get         =>get_sediment_capacity


end type vanrijn_argument

type , public :: sandmud_argument

    integer                , pointer  :: nfrac   =>null()   ! number of sediment fractions
    integer  , dimension(:), pointer  :: sedtyp  =>null()   ! sediment type
    real(fp) , dimension(:), pointer  :: frac    =>null()   ! sediment (mass) fraction [-]
    real(fp)               , pointer  :: mudfrac =>null()   ! mud fraction [-]
    real(fp)               , pointer  :: pmcrit  =>null()   ! critical mud fraction [-]
    real(fp) , dimension(:), pointer  :: E       =>null()   ! sediment erosion velocity [m/s]

contains
    procedure,public, pass            :: initialize   =>allocate_sandmud
    procedure,public, pass            :: finalize     =>deallocate_sandmud
    procedure,public, pass            :: set          =>set_sandmud
    procedure,public, pass            :: run          =>run_sandmud
    procedure,public, pass            :: get          =>get_erosion_velocity

end type sandmud_argument

type, public ::bedbc1993_argument

    real(fp),  pointer    :: aks    =>null()  !  van Rijn reference height (a)
    real(fp),  pointer    :: ce_nm  =>null()  ! reference concentration near bed
    real(fp),  pointer    :: d50    =>null()  !
    real(fp),  pointer    :: d90    =>null()  !
    real(fp),  pointer    :: delr   =>null()  ! ripple height : default = 0.025 m
    real(fp),  pointer    :: dss    =>null()  ! suspended sediment diameter of fraction [m], defined in Engelund-Hansen
    real(fp),  pointer    :: dstar  =>null()  ! dimensiosless corn diameter d* in van Rijn formula
    real(fp),  pointer    :: h1     =>null()  ! water depth
    real(fp),  pointer    :: muc    =>null()  ! mu(c) factor in calculation of current-wave bed shear stress
    real(fp),  pointer    :: mudfrac=>null()  ! mud fraction
    real(fp),  pointer    :: rhowat =>null()  ! roh of water
    real(fp),  pointer    :: ta     =>null()  ! dimensionless bed shear stress
    real(fp),  pointer    :: taubcw =>null()  ! combined current-wave bed shear stress according to van Rijn 1993
    real(fp),  pointer    :: tauc   =>null()  ! current only bed shear stress
    real(fp),  pointer    :: taucr  =>null()  ! critical bed shear stress
    real(fp),  pointer    :: taurat =>null()  ! ratio of tau current to tau wave (seesm this is corrcet:Ratio of total tau to to criitical tau)
    real(fp),  pointer    :: tauwav =>null()  ! wave only bed shear stress
    real(fp),  pointer    :: tp     =>null()  ! wave period
    real(fp),  pointer    :: ubed   =>null()  ! near_bed velocity magnitude
    real(fp),  pointer    :: uorb   =>null()  ! neear ed orbital velocity
    real(fp),  pointer    :: ustarc =>null()  ! u*-critical
    real(fp),  pointer    :: usus   =>null()  ! velocity magnitude near bed for current and at the top od the wave boundary layer for wave
    real(fp),  pointer    :: uwb    =>null()  ! sqrt (urob)
    real(fp),  pointer    :: z0cur  =>null()  ! current_related bed roughness
    real(fp),  pointer    :: z0rou  =>null()  ! wave_related bed roughness
    real(fp),  pointer    :: zubed  =>null()  ! elevation of ubed
    real(fp),  pointer    :: zusus  =>null()  ! elevation of usus
    real(fp),  pointer    :: eps    =>null()  ! 1e-6
    real(fp),  pointer    :: aksfac =>null()  ! a unser-defined factor in determination of aks = min [ max(aksfac*ks, delr/2,0.01), 0.2 h]
    real(fp),  pointer    :: rwave  =>null()  ! a user-defined value betwen 1-3.
    real(fp),  pointer    :: camax  =>null()
    real(fp),  pointer    :: rdc    =>null()
    real(fp),  pointer    :: rdw    =>null()
    integer ,  pointer    :: iopkcw =>null()  ! Flag for determination of ks ( or kw) in code or assignment by user in input data
                                              ! if iopkcw = 1: kw = RWAVE . delr and rc   = 30.*z0cur; else read from data file
    integer ,  pointer    :: iopsus =>null()  ! A flag for recalculation of charactersitic sediment diameter for suspension (dss)
    real(fp),  pointer    :: vonkar =>null()  ! von Karman constant
    logical ,  pointer    :: wave   =>null()  ! a flag for inclusion of wave-effect for bed sheaar stress
    real(fp),  pointer    :: tauadd =>null()  ! a user-defined additional bed shear stress added to the current-only bed shear stress


contains

    procedure,public, pass:: initialize   =>allocate_bedbc
    procedure,public, pass:: finalize     =>deallocate_bedbc
    procedure,public, pass:: set          =>set_bedbc
    procedure,public, pass:: run          =>run_bedbc
    procedure,public, pass:: get          =>get_tau


end type  bedbc1993_argument

type , public :: soursin3d_argument

    real(fp),  pointer     :: ce_nm =>null()      ! reference concentration near bed
    real(fp),  pointer     :: h1    =>null()      ! water depth
    real(fp),  pointer     :: r0    =>null()      ! near bed sediment concentration
    real(fp),  pointer     :: rhosol=>null()      ! rho of soil
    real(fp),  pointer     :: seddif=>null()      ! sediment vertcial diffusion coefficient near the bed
    real(fp),  pointer     :: sigsed=>null()      ! (elevatio) or level (sigma-layer) of the bed layer
    real(fp),  pointer     :: sigmol=>null()     ! molecular Prandtl number
    real(fp),  pointer     :: thick =>null()      ! relative layer thickness of the near-bed layer
    real(fp),  pointer     :: thick0=>null()     ! absolute layer thickness of the near-bed layer at present time step
    real(fp),  pointer     :: thick1=>null()     ! absolute layer thickness of the near-bed layer at old time step
    real(fp),  pointer     :: vicmol=>null()      ! moelcular viscosity
    real(fp),  pointer     :: ws    =>null()     ! settling velocity
    real(fp),  pointer     :: aks   =>null()      ! van Rijn reference height (a)
    real(fp),  pointer     :: sour  =>null()      ! source term for 3D
    real(fp),  pointer     :: sink  =>null()      ! sink term for 3D



contains

    procedure,public, pass :: initialize   =>allocate_soursin3d
    procedure,public, pass :: finalize     =>deallocate_soursin3d
    procedure,public, pass :: set          =>set_soursin3d
    procedure,public, pass :: run          =>run_soursin3d
    procedure,public, pass :: get          =>get_flux


end type soursin3d_argument


type , public    :: compbsskin_argument

    real(fp)               , pointer  :: umean =>null()  ! depth averaged flow velocity in u-direction
    real(fp)               , pointer  :: vmean =>null()  ! depth averaged flow velocity in v-direction
    real(fp)               , pointer  :: depth =>null()  ! local water depth
    real(fp)               , pointer  :: uorb  =>null()   ! orbital velocity based upon Hrms
    real(fp)               , pointer  :: tper  =>null()   ! wave period
    real(fp)               , pointer  :: teta  =>null()  ! angle between wave direction and local grid orientation
    real(fp)               , pointer  :: kssilt=>null() ! roughness height silt
    real(fp)               , pointer  :: kssand=>null() ! roughness height sand(not yet used)
    real(fp)               , pointer  :: thcmud=>null() ! Total hickness of mud layers(to be replaced by mudcnt in future)
    real(fp)               , pointer  :: taumax=>null() ! resulting (maximum) bed shear stress muddy silt bed
    logical                , pointer  :: wave  =>null() ! wave impacts included in flow comp. or not
    real(fp)               , pointer  :: rhowat=>null() ! water density
    real(fp)               , pointer  :: vicmol=>null() ! molecular viscosity

contains

    procedure,public, pass            :: initialize   =>allocate_compbsskin
    procedure,public, pass            :: finalize     =>deallocate_compbsskin
    procedure,public, pass            :: set          =>set_compbsskin
    procedure,public, pass            :: run          =>run_compbsskin
    procedure,public, pass            :: get          =>get_compbsskin

end type   compbsskin_argument

save

    real(fp)                             :: alf1          ! calibration coefficient van Rijn (1984) [-]
    real(fp)                             :: betam         ! power factor for adaptation of critical bottom shear stress [-]
    real(fp)                             :: rksc          ! reference level van Rijn (1984) [m]
    real(fp),allocatable, dimension(:)   :: pmcrit        ! critical mud fraction [-]
    real(fp),allocatable, dimension(:,:) :: depeff        ! deposition efficiency [-]
    real(fp),allocatable, dimension(:,:) :: depfac        ! deposition factor (flufflayer=2) [-]
    real(fp),allocatable, dimension(:,:) :: eropar        ! erosion parameter for mud [kg/m2/s]
    real(fp),allocatable, dimension(:,:) :: parfluff0     ! erosion parameter 1 [s/m]
    real(fp),allocatable, dimension(:,:) :: parfluff1     ! erosion parameter 2 [ms/kg]
    real(fp),allocatable, dimension(:,:) :: tcrdep        ! critical bed shear stress for mud sedimentation [N/m2]
    real(fp),allocatable, dimension(:,:) :: tcrero        ! critical bed shear stress for mud erosion [N/m2]
    real(fp),allocatable, dimension(:,:) :: tcrfluff      ! critical bed shear stress for fluff layer erosion [N/m2]

contains


subroutine erosed( nmlb     , nmub    , flufflyr , mfluff , frac   , mudfrac   , &
                 & ws       , umod    , h        , chezy  , taub               , &
                 & nfrac    , rhosol  , sedd50   , sedd90 , sedtyp             , &
                 & sink     , sinkf   , sour     , sourf  , anymud , wave      , &
                 & uorb     , tper    , teta     ,spm_concentration, Bioeffects  )

!
!    Function: Computes sedimentation and erosion fluxes
!
!!--declarations----------------------------------------------------------------



    !
    implicit none


    type (mud_argument)                                    :: eromud_arguments
    type (sand_argument)                                   :: erosand_arguments
    type (vanrijn_argument)                                :: vanrijn84_arguments
    type (sandmud_argument)                                :: sandmud_arguments
    type (bedbc1993_argument)                              :: bedbc1993_arguments
    type (soursin3d_argument)                              :: soursin3d_arguments
    type (compbsskin_argument)                             :: compbsskin_arguments
    !
    real(fp)    , dimension(:,:,:)          , pointer      :: spm_concentration
    real(fp)    , dimension(:,:)            , pointer      :: mfluff        ! composition of fluff layer: mass of mud fractions [kg/m2]
    integer                                 , intent(in)   :: flufflyr      ! switch for fluff layer concept
    integer                                 , intent(in)   :: nfrac         ! number of sediment fractions
    integer                                 , intent(in)   :: nmlb          ! first cell number
    integer                                 , intent(in)   :: nmub          ! last cell number
    integer     , dimension(nfrac)          , intent(in)   :: sedtyp        ! sediment type
    real(fp)    , dimension(nmlb:nmub)      , intent(inout):: chezy         ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    real(fp)    , dimension(nmlb:nmub)      , intent(in)   :: h             ! water depth [m]
    real(fp)    , dimension(nfrac)          , intent(in)   :: rhosol        ! specific sediment density [kg/m3]
  ! real(fp)    , dimension(nmlb:nmub)      , intent(in)   :: sedd50        ! 50% diameter sediment fraction [m]
  ! real(fp)    , dimension(nmlb:nmub)      , intent(in)   :: sedd90        ! 90% diameter sediment fraction [m]
    real(fp)    , dimension(nfrac)          , intent(in)   :: sedd50        ! 50% diameter sediment fraction [m]
    real(fp)    , dimension(nfrac)          , intent(in)   :: sedd90        ! 90% diameter sediment fraction [m]
    real(fp)    , dimension(nmlb:nmub)      , intent(inout):: taub          ! bottom shear stress [N/m2]
    real(fp)    , dimension(nmlb:nmub)      , intent(in)   :: umod          ! velocity magnitude (in bottom cell) [m/s]
    real(fp)    , dimension(nfrac,nmlb:nmub), intent(in)   :: ws            ! sediment settling velocity (hindered) [m/s]
    real(fp)    , dimension(nfrac,nmlb:nmub), intent(out)  :: sink          ! sediment sink flux [m/s]
    real(fp)    , dimension(nfrac,nmlb:nmub), intent(out)  :: sinkf         ! sediment sink flux fluff layer [m/s]
    real(fp)    , dimension(nfrac,nmlb:nmub), intent(out)  :: sour          ! sediment source flux [kg/m2/s]
    real(fp)    , dimension(nfrac,nmlb:nmub), intent(out)  :: sourf         ! sediment source flux fluff layer [kg/m2/s]
    real(fp)    , dimension(nfrac,nmlb:nmub), intent (in)  :: frac          ! sediment (mass) fraction [-]
    real(fp)    , dimension(nmlb:nmub)      , intent (in)  :: mudfrac       ! mud fraction [-]
    type (BioturbationEffect)               , intent (in)  :: Bioeffects    ! Biological effects on the sediment flux
    logical                                 , intent (in)  :: anymud, wave  ! if wave effect on sheare stress is requiried set to .true.
    real(fp)    , dimension(nmlb:nmub)      , intent (in)  :: uorb          ! wave orbital velocity (rms)
    real(fp)    , dimension(nmlb:nmub)      , intent (in)  :: tper          ! wave period
    real(fp)    , dimension(nmlb:nmub)      , intent (in)  :: teta          ! angle between wave and current (radian)

!
! Local variables
!
    integer                                     :: l            ! sediment counter
    integer                                     :: nm           ! cell counter
    real(fp)                                    :: fracf,rhowat
    real(fp)                                    :: mfltot,vicmol! vicmol: molecular viscosity
    real(fp)                                    :: sbot
    real(fp)                                    :: smfac        ! correction factor for critical bottom shear stress
    real(fp)                                    :: ssus
    real(fp)    , dimension(nfrac          )    :: E            ! erosion velocity [m/s]
    real(fp)    , dimension(nfrac,nmlb:nmub)    :: fixfac       ! reduction factor in case of limited sediment availability [-]
    real(fp)    , dimension(nfrac,nmlb:nmub)    :: rsedeq       ! equilibrium concentration [kg/m3]
    real(fp)                                    :: fc           ! Skin friction coefficient (Darcy-Weisbach)
    real(fp)    , dimension(nmlb:nmub)          :: thcmud       ! Total thickness of mud layers
    integer                                     :: inum,jnum,i,j! inum and jnum are the number of elements in x and y directions
    real(fp)                                    :: kssilt,kssand! Roughness height for silt and sand
    integer                                     :: iform        ! selection of different transport methods (default 1 (van Rijn)
    real(fp)                                    :: drho		! relative density
    logical                                     :: flow2d
! Bedbc1993 variables
    real(fp)                                    :: aks, ce_nm   ! aks: Van Rijn's reference height, ce_nm: equilibrium sand concentration [Kg/m^3]
    real(fp)                                    :: delr         ! Delta: ripple height
    real(fp)                                    :: dss          ! suspended sediment diameter of fraction [m], defined in Engelund-Hansen
    real(fp)                                    :: muc          ! mu(c) factor in calculation of current-wave bed shear stress
    real(fp)               		            	:: ta		! dimensionless bed shear stress
    real(fp) 			                        :: taubcw       ! combined current-wave bed shear stress according to van Rijn 1993 (muc*tauc + muw*tauwav)
    real(fp)    , dimension(nmlb:nmub)      	:: tauc		! Current only bed shear stress
    real(fp)					                :: taurat	! Ratio of total tau to to criitical tau
    real(fp)	, dimension(nmlb:nmub)		    :: tauwav	! Tau due to wave
    real(fp)   			                        :: ustarc	! u* due to current only
    real(fp)              			            :: uwb		! sqr(2.) * uorb
    real(fp)					                :: zusus  	! elevation of usus
    real(fp)              			            :: usus     ! Velocity magnitude at the top of boundary layer (in case of wave and the lowest cell in case of the current)
! Bedbc1993 required varaibles (input)
    real(fp)	, dimension(nfrac) 		        :: taucr	! Critical bed shear stress for sand
    real(fp)    , dimension(nfrac)              :: dstar        ! dimensiosless corn diameter d* in van Rijn formula
    real(fp)	, dimension(nfrac) 		        :: tetacr	! Dimensionless critical bed shear stress for sand
    real(fp)   	, dimension(nmlb:nmub)	        :: ubed		! near_bed velocity magnitude
    real(fp)	, dimension(nmlb:nmub)		    :: zubed        ! elevation of ubed
    real(fp)					                :: eps          ! 1e-6
    real(fp)	, dimension(nfrac)		        :: z0cur	! current_related bed roughness (i.e. z0= ks/30.0 (ks = 2.50 sedd50))
    real(fp)	, dimension(nfrac)		        :: z0rou  	! wave_related bed roughness
    real(fp)				                	:: aksfac	! a unser-defined factor in determination of aks = min [ max(aksfac*ks, delr/2,0.01), 0.2 h]
    real(fp)                					:: camax	! User-defined upper limit of the sediment concentration (sand)
    real(fp)				                	:: rdc		! Nikoradse roughness length (ks)
    real(fp)                					:: rdw		! wave related roughness ks,wave , ks,w= rdw * delr
    real(fp)				                	:: rwave	! user-defined wave related roughness coefficient 1-3 (default 2)
    integer                  					:: iopkcw   ! Flag for determination of ks ( or kw) in code or assignment by user in input data
                                             			    ! if iopkcw = 1: kw = RWAVE . delr and rc   = 30.*z0cur; else read from data file
    integer 				                	:: iopsus	! A flag for recalculation of charactersitic sediment diameter for suspension (dss)
    real(fp)		                			:: vonkar
    real(fp)	                				:: tauadd	! User-defined additional bed shear stress
    real(fp)                                    :: g        ! gravity acceleration: @TODO: It should be substituted by g in globaldat.nml
    real(fp)                                    :: factcr   ! user-defined factor multiplied by critical bed shear stress

! soursin_3D

    real(fp)                                    :: seddif   !@ToDO : sediment diffusion coefficent of teh loweset element should be received from GoTM
    real(fp)                                    :: sigsed
    real(fp)                                    :: sigmol
    real(fp)                                    :: thicksed
    real(fp)                                    :: thick0
    real(fp)                                    :: thick1
    real(fp)                                    :: thick

!
!! executable statements ------------------
!
!#define DEBUG
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
    rhowat      = 1000.0_fp
    vicmol      = 1.0e-6_fp
    thcmud      = 0.001_fp ! @ToDO: Total thickness of mud layer [m] should be read from data file
                        ! if thcmud > 0.01 m then the average d50 of sand will be used to calculated
                        ! z0 roughness for taub in compbsskin. Taub is the combined wave current shear
                        ! stress for cohesive soil

   !++++++++++ TEST Bedbc_1993 ++++++++++
flow2d = .true.
ubed = 0.35_fp
zubed = 0.50_fp
eps = 1e-6
z0cur = sedd50/12._fp
z0rou = z0cur
aksfac = 1.0_fp
rwave = 1.0_fp
camax=0.65_fp
rdc = 2.5_fp* sedd50(2)
rdw = rdc
rwave = 2.0_fp
iopkcw=1
iopsus = 1
vonkar = 0.4_fp
tauadd = 0.0_fp
iform = 1
g = 9.81_fp
factcr = 1.0
!+++++++++++TEST Soursin_3D++++++++++++++
sigsed = 0.1_fp
sigmol = 1.e-5_fp
seddif = 1.e-3_fp
thicksed = 1.0_fp
thick0  = 1.0_fp
thick1 = 1.0_fp
thick = 0.25_fp ! thickness of bed layer bed cell!?
!+++++++++++TEST++++++++++++++


!
    !
    !   Compute change in sediment composition (e.g. based on available fractions and sediment availability)
    !
    call eromud_arguments%initialize()
    call erosand_arguments%initialize()
    call vanrijn84_arguments%initialize()
    call sandmud_arguments%initialize(nfrac)
    call bedbc1993_arguments%initialize()
    call soursin3d_arguments%initialize ()
    call compbsskin_arguments%initialize ()

    inum = Size(Bioeffects%ErodibilityEffect,1)
    jnum = Size(Bioeffects%ErodibilityEffect,2)

    allocate (spm_concentration (inum,jnum,nfrac))
    kssilt = 0.0_fp
    kssand = 0.0_fp
    i = 0
    j = 0

    do l = 1, nfrac

       if (sedtyp(l)==SEDTYP_COHESIVE) then
           kssilt = sedd50(l)*2.5 + kssilt
           i      = i +1
        else
           kssand = sedd50(l)*2.5 + kssand
           j      = j + 1
       endif
    end do

       kssilt     = kssilt /(i *1.0_fp)
       kssand     = kssand /(j *1.0_fp)
       i          = 0
       j          = 0
! Main loop over elements organized in vector form
 elements: do nm = nmlb, nmub

        mfltot = 0.0_fp
        if (flufflyr>0) then
            do l = 1, nfrac
                mfltot = mfltot + mfluff(l,nm)
            enddo
        endif

        j= 1+ mod(nm,inum)
        i= nm - inum*(j -1)

        ! Taub is the bed shear stress under combined wave and current (Soulsby(2004))
        ! note here that kssilt and kssand could be either skin related roughness (2.5 d50)
        ! or total roughness (Soulsby, 1997, p.92). This taub is used only for cohesive sediment.
        ! For non-cohesive sediment in 3D, tau current is calculated using zocur (roughness length)
        ! within bedbc1993. For calculation of tau wave, z0rou is used as bed roughness in bedbc1993.



 fractions: do l = 1, nfrac
            if (sedtyp(l)==SEDTYP_COHESIVE) then


                !   Compute source and sink fluxes for cohesive sediment (mud)

                call compbsskin_arguments%set (umod(nm), 0.0_fp    , h(nm)   , wave  ,       &
                                             & uorb(nm), tper  (nm), teta(nm), kssilt,       &
                                             & kssand  , thcmud(nm), taub(nm), rhowat, vicmol)

                call compbsskin_arguments%run ()
                call compbsskin_arguments%get(taub(nm))

                fracf   = 0.0_fp
                if (mfltot>0.0_fp) fracf   = mfluff(l,nm)/mfltot

#ifdef DEBUG
                 write (*,*) 'bioeffects on erodibility :', Bioeffects%ErodibilityEffect (i,j)

                 write (*,*) 'bioeffects on critical tau :', Bioeffects%TauEffect (i,j)

                 write (*,*) 'eropar(l,nm)= in erosed',  eropar(l,nm)
                 write (*,*) 'tcrero(l,nm)=', tcrero(l,nm)

                 write (*,*) 'Bio eropar(l,nm)=', eropar(l,nm)* Bioeffects%ErodibilityEffect (i,j)
                 write (*,*) 'Bio tcrero(l,nm)=', tcrero(l,nm)* Bioeffects%TauEffect (i,j)
#endif

                 call  eromud_arguments%set(ws(l,nm) , fixfac(l,nm)  , taub(nm)      , frac(l,nm)    , fracf  , &
                         & tcrdep(l,nm)   , tcrero(l,nm) * Bioeffects%TauEffect (i,j), eropar(l,nm)* Bioeffects%ErodibilityEffect (i,j),&
                         & flufflyr       , mfltot   , tcrfluff(l,nm), depeff(l,nm)  , depfac(l,nm)  , parfluff0(l,nm), parfluff1(l,nm) )

                 call eromud_arguments%run ()

                 call eromud_arguments%get(sour (l,nm), sink (l,nm), sourf (l,nm), sinkf (l,nm) )
#ifdef DEBUG
                 write (*,*) 'erosed mud sour', sour (l,nm), l
                 write (*,*) 'erosed mud sink',sink (l,nm), l
#endif
            else
                !Non-Cohesive soil
                ! Compute correction factor for critical bottom shear stress with sand-mud interaction
                !
              if (flow2d) Then
                !write (*,*) 'pmcrit = ',pmcrit(nm)
                if ( pmcrit(nm) > 0.0_fp ) then
                    smfac = ( 1.0_fp + mudfrac(nm) ) ** betam
    !                write (*,*) 'betam ', betam
     !               write (*,*) ' mudfrac',mudfrac
                else
                    smfac = 1.0_fp
                endif
           !     write (*,*) ' smfac= ', smfac
         !       if (present (Bioeffects)) then
#ifdef DEBUG
!                    write (*,*) 'bioeffects on critical tau :', Bioeffects%TauEffect (1,1,1)
#endif
                    smfac =smfac * Bioeffects%TauEffect(i,j)
#ifdef DEBUG
!                    write (*,*) 'Bio smfac= ', smfac
#endif
          !      end if
                !
                !   Apply sediment transport formula ( in this case vanRijn (1984) )
                !
!                rksc = min(max(3.0 * sedd90(l),0.01 * h(nm)),0.2 * h(nm))    ! note that this ks-value is only applicable for grain related roughness
                                        ! for wave-related roughness it should be modified.
                !rksc = 3.0_fp * sedd90(l)

                call vanrijn84_arguments%set (umod(nm) ,sedd50(l),sedd90(l),h(nm) ,ws(l,nm), &
                                            & rhosol(l),alf1     ,rksc     ,smfac )

                call vanrijn84_arguments%run()

                call vanrijn84_arguments%get(sbot, ssus)


                !
                ssus =  ssus * rhosol(l)
                !mass of equilibrium suspended load
                !   Compute reference concentration
                !
                if (umod(nm)*h(nm)>0.0_fp) then
                    rsedeq(l,nm) = frac(l,nm) * ssus / (umod(nm)*h(nm))

                endif

                fc = .24*(log10(12.*h(nm)/rksc ))**( - 2)   ! rksc according to vanrijn subroutine
                chezy (nm) = sqrt(9.81_fp *8.0_fp / fc)
                !write (*,*) ' Chezy with rksc= ', chezy(nm)

                !fc = .24*(log10(12.*h(nm)/(2*sedd50(l)) ))**( - 2) ! ks = 2.50 d50 according to teh Delft3D manual pp. 210
                !chezy (nm) = sqrt(9.81_fp *8.0_fp / fc)
                !write (*,*) ' Chezy with ks= ', chezy(nm)

                !
                !   Compute suspended sediment fluxes for non-cohesive sediment (sand)
                !
                call erosand_arguments%set (umod(nm)    ,chezy(nm)     ,ws(l,nm)  ,rsedeq(l,nm))

                call erosand_arguments%run()

                call erosand_arguments%get(sour (l,nm), sink (l,nm) )
 !               write (*,*) ' sour and sink 2D', sour (l,nm), sink (l,nm)


#ifdef DEBUG
!                write (*,*) 'erosed sand sour', sour (l,nm), l
!                write (*,*) 'erosed sand sink',sink (l,nm), l
#endif
              else
                !(3D)
                 drho     = (rhosol(l)-rhowat) / rhowat
                 dstar(l) = sedd50(l) * (drho*g/vicmol**2)**0.3333_fp
                 if (dstar(l) < 1.0_fp) then
                    if (iform == -2) then
                       tetacr(l) = 0.115_fp / (dstar(l)**0.5_fp)
                    else
                       tetacr(l) = 0.24_fp / dstar(l)
                    endif
                 elseif (dstar(l) <= 4.0_fp) then
                    if (iform == -2) then
                       tetacr(l) = 0.115_fp / (dstar(l)**0.5_fp)
                    else
                       tetacr(l) = 0.24_fp / dstar(l)
                    endif
                 elseif (dstar(l)>4.0_fp .and. dstar(l)<=10.0_fp) then
                    tetacr(l) = 0.14_fp  / (dstar(l)**0.64_fp)
                 elseif (dstar(l)>10.0_fp .and. dstar(l)<=20.0_fp) then
                    tetacr(l) = 0.04_fp  / (dstar(l)**0.1_fp)
                 elseif (dstar(l)>20.0_fp .and. dstar(l)<=150.0_fp) then
                    tetacr(l) = 0.013_fp * (dstar(l)**0.29_fp)
                 else
                    tetacr(l) = 0.055_fp
                 endif

                 taucr(l) = factcr * (rhosol(l)-rhowat) * g * sedd50(l) * tetacr(l)* Bioeffects%TauEffect(i,j)

                 call bedbc1993_arguments%set (tper(nm) ,uorb(nm)   ,rhowat   ,h(nm)   ,ubed(nm), &
                           & zubed(nm)   ,sedd50(l)     ,sedd90(l)  ,z0cur(l) ,z0rou(l),dstar(l), &
                           & taucr(l)    ,mudfrac(nm)   ,eps        ,aksfac   ,rwave   ,camax   , &
                           & rdc         ,rdw           ,iopkcw    ,iopsus    ,vonkar  ,wave,tauadd )

                 call bedbc1993_arguments%run

    !                call bedbc1993(tper      ,uorb (nm) ,rhowat    ,h(nm)     ,ubed(nm)      , &
    !                             & zubed (nm),sedd50(l) ,sedd90(l) ,z0cur     ,z0rou     , &
    !                             & dstar     ,taucr(l)  ,aks       ,usus      ,zusus     , &
    !                             & uwb       ,delr      ,muc       ,tauwav    ,ustarc    , &
    !                             & tauc      ,taubcw    ,taurat    ,ta        ,ce_nm     , &
    !                             & dss       ,mudfrac   ,eps       ,aksfac    ,rwave     , &
    !                             & camax     ,rdc       ,rdw       ,iopkcw    ,iopsus    , &
    !                             & vonkar    ,wave      ,tauadd    )

                 call  bedbc1993_arguments%get (aks, ce_nm, taubcw, ta)
    !
    !               call soursin_3d  ( h(nm)             ,thick0         ,thick1             , &
    !                               &  sig(kmaxsd)       ,thick(kmaxsd)  ,r0(nm,kmaxsd,ll)   , &
    !                               &  vicmol            ,sigmol(ll)     ,seddif(nm,kmaxsd,l), &
    !                               &  rhosol(l)         ,ce_nmtmp       ,ws(nm,kmaxsd,l)    , &
    !                               &  aks               ,sourse(nm,l)   ,sinkse(nm,l) )

                 call soursin3d_arguments%set (h (nm) ,thick0   ,thick1 , sigsed ,thick , &
                                   &  spm_concentration(i,j,l)  , vicmol, sigmol ,seddif, &
                                   &  rhosol (l)      ,ce_nm    , ws (l,nm)      , aks  )

                 call soursin3d_arguments%run ()

                 call soursin3d_arguments%get ( sour (l,nm), sink (l,nm))
                 write (*,*)' sour and sink 3D',sour (l,nm), sink (l,nm)
              end if !(2D/3D)
            endif ! (cohesive /non-cohesive
        enddo     fractions
    enddo    elements
    !

    ! Recompute fluxes due to sand-mud interaction
    !
    if (anymud) then
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
      !      write (*,*) 'erosed sand_mud sour', sour (l,nm), l
      !       write (*,*) 'erosed sand_mud sink',sink (l,nm), l
        enddo
    enddo
    end if
    !

    call eromud_arguments%finalize()
    call erosand_arguments%finalize()
    call vanrijn84_arguments%finalize()
    call sandmud_arguments%finalize()
    call bedbc1993_arguments%finalize()
    call soursin3d_arguments%finalize()
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
!    eropar      = 1.0e-3_fp     ! erosion parameter for mud [kg/m2/s]
!    tcrdep      = 1000_fp     ! critical bed shear stress for mud sedimentation [N/m2]
!    tcrero      = 0.4 !0.288_fp        ! critical bed shear stress for mud erosion [N/m2]
!    !
!    !   Parameters fluff layer
!    !
!    depeff      = 0.95_fp       ! deposition efficiency [-]
!    depfac      = 0.2_fp        ! deposition factor (flufflayer=2) [-]
!    parfluff0   = 2.0e-1_fp     ! erosion parameter 1 [s/m]
!    parfluff1   = 1.0_fp        ! erosion parameter 2 [ms/kg]
!    tcrfluff    = 0.05_fp       ! critical bed shear stress for fluff layer erosion [N/m2]
!
!    !   Parameters sand-mud interaction
!    !
!    betam       =  1.0_fp       ! power factor for adaptation of critical bottom shear stress [-]
!    pmcrit      =  0.6_fp       ! critical mud fraction [-]
!    !
!    !   Parameters sediment transport formulation
!    !
!    alf1        = 2.0_fp        ! calibration coefficient [-]
!    rksc        = 0.1_fp        ! reference level [m]
    !
    ! ================================================================================

end subroutine initerosed


subroutine getfrac_dummy (anymud,sedtyp,nfrac,nmlb,nmub,frac,mudfrac)




implicit none

    integer                                                         , intent(in)  :: nmlb
    integer                                                         , intent(in)  :: nmub
    integer                                                         , intent(in)  :: nfrac
    logical                                                         , intent(in)  :: anymud
    real(fp), dimension(nmlb:nmub, nfrac)                           , intent(in)  :: frac
    real(fp), dimension(nmlb:nmub)                                  , intent(out) :: mudfrac
    integer , dimension(nfrac)                                                    :: sedtyp
    integer                                                                       :: i,j


 if (anymud) then
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

!write (*,*) ' in eromud get_fluxes :: sink ', sink
!write (*,*) ' in eromud get_fluxes :: source ', source

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

!write (*,*) ' in get_fluxes_s in erosand:: source ', source
!write (*,*) ' in get_fluxes_s in erosand:: sink ', sink

end subroutine get_fluxes_s

!*******END **********   Eromud_argument Methods Block  *****END ***
!*******************************************************************

!*****************************************************************
!********************   Vanrijn84 Methods Block  ***********

subroutine allocate_vanrijnargu (vanrijn84_arguments)
implicit none
class ( vanrijn_argument) :: vanrijn84_arguments

allocate ( vanrijn84_arguments%umod  ,vanrijn84_arguments%sedd50,                     &
           vanrijn84_arguments%sedd90,vanrijn84_arguments%h  ,vanrijn84_arguments%ws, &
           vanrijn84_arguments%rhosol,vanrijn84_arguments%alf1,                       &
           vanrijn84_arguments%rksc  , vanrijn84_arguments%sbot,                      &
           vanrijn84_arguments%ssus  ,vanrijn84_arguments%smfac                       )


end subroutine allocate_vanrijnargu

subroutine deallocate_vanrijnargu (vanrijn84_arguments)
implicit none
class ( vanrijn_argument) :: vanrijn84_arguments

deallocate ( vanrijn84_arguments%umod  ,vanrijn84_arguments%sedd50,                        &
             vanrijn84_arguments%sedd90,vanrijn84_arguments%h,vanrijn84_arguments%ws,      &
             vanrijn84_arguments%rhosol,vanrijn84_arguments%alf1,vanrijn84_arguments%rksc, &
             vanrijn84_arguments%sbot, vanrijn84_arguments%ssus, vanrijn84_arguments%smfac )

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

call vanRijn84 ( vanrijn84_arguments%umod  ,vanrijn84_arguments%sedd50,                        &
                 vanrijn84_arguments%sedd90,vanrijn84_arguments%h,vanrijn84_arguments%ws,      &
                 vanrijn84_arguments%rhosol,vanrijn84_arguments%alf1,vanrijn84_arguments%rksc, &
                 vanrijn84_arguments%sbot,vanrijn84_arguments%ssus,vanrijn84_arguments%smfac   )

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


!***************************************************************
!************   Routines for 3D model **************************

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!***************************************************************
!*******************  bedbc1993 : (van Rijn method) ************

subroutine allocate_bedbc (bedbc1993_arguments)
implicit none
class (bedbc1993_argument)   :: bedbc1993_arguments

allocate (               bedbc1993_arguments%tp        ,bedbc1993_arguments%uorb      ,bedbc1993_arguments%rhowat    ,bedbc1993_arguments%h1        ,bedbc1993_arguments%ubed      , &
                       & bedbc1993_arguments%zubed     ,bedbc1993_arguments%d50       ,bedbc1993_arguments%d90       ,bedbc1993_arguments%z0cur     ,bedbc1993_arguments%z0rou     , &
                       & bedbc1993_arguments%dstar     ,bedbc1993_arguments%taucr     ,bedbc1993_arguments%aks       ,bedbc1993_arguments%usus      ,bedbc1993_arguments%zusus     , &
                       & bedbc1993_arguments%uwb       ,bedbc1993_arguments%delr      ,bedbc1993_arguments%muc       ,bedbc1993_arguments%tauwav    ,bedbc1993_arguments%ustarc    , &
                       & bedbc1993_arguments%tauc     ,bedbc1993_arguments%taubcw    ,bedbc1993_arguments%taurat    ,bedbc1993_arguments%ta        ,bedbc1993_arguments%ce_nm     , &
                       & bedbc1993_arguments%dss       ,bedbc1993_arguments%mudfrac   ,bedbc1993_arguments%eps       ,bedbc1993_arguments%aksfac    ,bedbc1993_arguments%rwave     , &
                       & bedbc1993_arguments%camax     ,bedbc1993_arguments%rdc       ,bedbc1993_arguments%rdw       ,bedbc1993_arguments%iopkcw    ,bedbc1993_arguments%iopsus    , &
                       & bedbc1993_arguments%vonkar    ,bedbc1993_arguments%wave      ,bedbc1993_arguments%tauadd    )



end subroutine allocate_bedbc


subroutine deallocate_bedbc (bedbc1993_arguments)
implicit none
class (bedbc1993_argument)   :: bedbc1993_arguments

deallocate (             bedbc1993_arguments%tp        ,bedbc1993_arguments%uorb      ,bedbc1993_arguments%rhowat    ,bedbc1993_arguments%h1        ,bedbc1993_arguments%ubed      , &
                       & bedbc1993_arguments%zubed     ,bedbc1993_arguments%d50       ,bedbc1993_arguments%d90       ,bedbc1993_arguments%z0cur     ,bedbc1993_arguments%z0rou     , &
                       & bedbc1993_arguments%dstar     ,bedbc1993_arguments%taucr     ,bedbc1993_arguments%aks       ,bedbc1993_arguments%usus      ,bedbc1993_arguments%zusus     , &
                       & bedbc1993_arguments%uwb       ,bedbc1993_arguments%delr      ,bedbc1993_arguments%muc       ,bedbc1993_arguments%tauwav    ,bedbc1993_arguments%ustarc    , &
                       & bedbc1993_arguments%tauc      ,bedbc1993_arguments%taubcw    ,bedbc1993_arguments%taurat    ,bedbc1993_arguments%ta        ,bedbc1993_arguments%ce_nm     , &
                       & bedbc1993_arguments%dss       ,bedbc1993_arguments%mudfrac   ,bedbc1993_arguments%eps       ,bedbc1993_arguments%aksfac    ,bedbc1993_arguments%rwave     , &
                       & bedbc1993_arguments%camax     ,bedbc1993_arguments%rdc       ,bedbc1993_arguments%rdw       ,bedbc1993_arguments%iopkcw    ,bedbc1993_arguments%iopsus    , &
                       & bedbc1993_arguments%vonkar    ,bedbc1993_arguments%wave      ,bedbc1993_arguments%tauadd    )



end subroutine deallocate_bedbc

 subroutine set_bedbc ( bedbc1993_arguments   ,tp        ,uorb      ,rhowat    ,h1        ,ubed      , &
                       & zubed     ,d50       ,d90       ,z0cur     ,z0rou     , &
                       & dstar     ,taucr    , &
                       & mudfrac   ,eps       ,aksfac    ,rwave     , &
                       & camax     ,rdc       ,rdw       ,iopkcw    ,iopsus    , &
                       & vonkar    ,wave      ,tauadd    )
implicit none

    class (bedbc1993_argument)   :: bedbc1993_arguments

    real(fp), intent(in)  :: d50
    real(fp), intent(in)  :: d90
    real(fp), intent(in)  :: dstar
    real(fp), intent(in)  :: h1
    real(fp), intent(in)  :: mudfrac
    real(fp), intent(in)  :: rhowat !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: taucr
    real(fp), intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: ubed
    real(fp), intent(in)  :: uorb   !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: z0cur
    real(fp), intent(in)  :: z0rou
    real(fp), intent(in)  :: zubed
    real(fp), intent(in)  :: eps
    real(fp), intent(in)  :: aksfac
    real(fp), intent(in)  :: rwave
    real(fp), intent(in)  :: camax
    real(fp), intent(in)  :: rdc
    real(fp), intent(in)  :: rdw
    integer , intent(in)  :: iopkcw
    integer , intent(in)  :: iopsus
    real(fp), intent(in)  :: vonkar
    logical , intent(in)  :: wave
    real(fp), intent(in)  :: tauadd

    bedbc1993_arguments%d50   = d50
    bedbc1993_arguments%d90   = d90
    bedbc1993_arguments%dstar = dstar
    bedbc1993_arguments%h1    = h1
    bedbc1993_arguments%mudfrac=mudfrac
    bedbc1993_arguments%rhowat= rhowat
    bedbc1993_arguments%taucr = taucr
    bedbc1993_arguments%tp    = tp
    bedbc1993_arguments%ubed  = ubed
    bedbc1993_arguments%uorb  = uorb
    bedbc1993_arguments%z0cur = z0cur
    bedbc1993_arguments%z0rou = z0rou
    bedbc1993_arguments%zubed = zubed
    bedbc1993_arguments%eps   = eps
    bedbc1993_arguments%aksfac= aksfac
    bedbc1993_arguments%rwave = rwave
    bedbc1993_arguments%camax = camax
    bedbc1993_arguments%rdc   = rdc
    bedbc1993_arguments%rdw   = rdw
    bedbc1993_arguments%iopkcw= iopkcw
    bedbc1993_arguments%iopsus= iopsus
    bedbc1993_arguments%vonkar= vonkar
    bedbc1993_arguments%wave  = wave
    bedbc1993_arguments%tauadd= tauadd

end subroutine set_bedbc

subroutine run_bedbc(bedbc1993_arguments)

implicit none

class (bedbc1993_argument) :: bedbc1993_arguments

call bedbc1993(          bedbc1993_arguments%tp        ,bedbc1993_arguments%uorb      ,bedbc1993_arguments%rhowat    ,bedbc1993_arguments%h1        ,bedbc1993_arguments%ubed      , &
                       & bedbc1993_arguments%zubed     ,bedbc1993_arguments%d50       ,bedbc1993_arguments%d90       ,bedbc1993_arguments%z0cur     ,bedbc1993_arguments%z0rou     , &
                       & bedbc1993_arguments%dstar     ,bedbc1993_arguments%taucr     ,bedbc1993_arguments%aks       ,bedbc1993_arguments%usus      ,bedbc1993_arguments%zusus     , &
                       & bedbc1993_arguments%uwb       ,bedbc1993_arguments%delr      ,bedbc1993_arguments%muc       ,bedbc1993_arguments%tauwav    ,bedbc1993_arguments%ustarc    , &
                       & bedbc1993_arguments%tauc      ,bedbc1993_arguments%taubcw    ,bedbc1993_arguments%taurat    ,bedbc1993_arguments%ta        ,bedbc1993_arguments%ce_nm     , &
                       & bedbc1993_arguments%dss       ,bedbc1993_arguments%mudfrac   ,bedbc1993_arguments%eps       ,bedbc1993_arguments%aksfac    ,bedbc1993_arguments%rwave     , &
                       & bedbc1993_arguments%camax     ,bedbc1993_arguments%rdc       ,bedbc1993_arguments%rdw       ,bedbc1993_arguments%iopkcw    ,bedbc1993_arguments%iopsus    , &
                       & bedbc1993_arguments%vonkar    ,bedbc1993_arguments%wave      ,bedbc1993_arguments%tauadd    )

end subroutine run_bedbc

subroutine get_tau (bedbc1993_arguments, aks, ce_nm, taubcw, ta)
implicit none
class (bedbc1993_argument) :: bedbc1993_arguments

real (fp) , intent (out)  :: aks, ce_nm, taubcw, ta

aks    = bedbc1993_arguments%aks
ce_nm  = bedbc1993_arguments%ce_nm
taubcw = bedbc1993_arguments%taubcw
ta     = bedbc1993_arguments%ta

end subroutine get_tau

!***************************************************************
!*******************  soursin3d_3D *******************************

subroutine allocate_soursin3d (soursin3d_arguments)
implicit none
class (soursin3d_argument)   :: soursin3d_arguments

allocate (soursin3d_arguments%h1 ,soursin3d_arguments%thick0    ,soursin3d_arguments%thick1     , &
                               &  soursin3d_arguments%sigsed    ,soursin3d_arguments%thick      ,soursin3d_arguments%r0, &
                               &  soursin3d_arguments%vicmol    ,soursin3d_arguments%sigmol     ,soursin3d_arguments%seddif, &
                               &  soursin3d_arguments%rhosol    ,soursin3d_arguments%ce_nm      ,soursin3d_arguments%ws , &
                               &  soursin3d_arguments%aks       ,soursin3d_arguments%sour       ,soursin3d_arguments%sink )


end subroutine allocate_soursin3d

subroutine deallocate_soursin3d (soursin3d_arguments)

implicit none
class (soursin3d_argument)   :: soursin3d_arguments

deallocate (soursin3d_arguments%h1,soursin3d_arguments%thick0    ,soursin3d_arguments%thick1     , &
                               &  soursin3d_arguments%sigsed    ,soursin3d_arguments%thick      ,soursin3d_arguments%r0, &
                               &  soursin3d_arguments%vicmol    ,soursin3d_arguments%sigmol     ,soursin3d_arguments%seddif, &
                               &  soursin3d_arguments%rhosol    ,soursin3d_arguments%ce_nm      ,soursin3d_arguments%ws , &
                               &  soursin3d_arguments%aks       ,soursin3d_arguments%sour       ,soursin3d_arguments%sink)


end subroutine deallocate_soursin3d

subroutine set_soursin3d (soursin3d_arguments           ,h1             ,thick0            ,thick1             , &
                               &  sigsed            ,thick          ,r0    , &
                               &  vicmol            ,sigmol         ,seddif, &
                               &  rhosol            ,ce_nm          ,ws    , aks  )

implicit none
class (soursin3d_argument)  :: soursin3d_arguments
    real(fp), intent(in)  :: ce_nm
    real(fp), intent(in)  :: h1
    real(fp), intent(in)  :: r0
    real(fp), intent(in)  :: rhosol
    real(fp), intent(in)  :: seddif
    real(fp), intent(in)  :: sigsed
    real(fp), intent(in)  :: sigmol
    real(fp), intent(in)  :: thick
    real(fp), intent(in)  :: thick0
    real(fp), intent(in)  :: thick1
    real(fp), intent(in)  :: vicmol
    real(fp), intent(in)  :: ws
    real(fp), intent(in)  :: aks

!
soursin3d_arguments%ce_nm  = ce_nm
soursin3d_arguments%h1     = h1
soursin3d_arguments%thick0 = thick0
soursin3d_arguments%thick1 = thick1
soursin3d_arguments%thick  = thick
soursin3d_arguments%r0     = r0
soursin3d_arguments%sigsed = sigsed
soursin3d_arguments%vicmol = vicmol
soursin3d_arguments%sigmol = sigmol
soursin3d_arguments%rhosol = rhosol
soursin3d_arguments%ce_nm  = ce_nm
soursin3d_arguments%ws     = ws
soursin3d_arguments%aks    = aks

end subroutine set_soursin3d


subroutine run_soursin3d(soursin3d_arguments)

implicit none
class (soursin3d_argument)    :: soursin3d_arguments

call   soursin_3d(                soursin3d_arguments%h1            ,soursin3d_arguments%thick0   ,soursin3d_arguments%thick1 , &
                               &  soursin3d_arguments%sigsed        ,soursin3d_arguments%thick    ,soursin3d_arguments%r0    , &
                               &  soursin3d_arguments%vicmol        ,soursin3d_arguments%sigmol   ,soursin3d_arguments%seddif, &
                               &  soursin3d_arguments%rhosol        ,soursin3d_arguments%ce_nm    ,soursin3d_arguments%ws    , &
                               &  soursin3d_arguments%aks           ,soursin3d_arguments%sour     ,soursin3d_arguments%sink )

end subroutine run_soursin3d

subroutine get_flux(soursin3d_arguments, source, sink)
implicit none
class (soursin3d_argument)    :: soursin3d_arguments
real (fp) , intent (out)    :: source, sink

source = soursin3d_arguments%sour
sink   = soursin3d_arguments%sink
end subroutine get_flux

!***************************************************************
!*******************  compbsskin *******************************

Subroutine allocate_compbsskin (compbsskin_arguments)

    implicit none
    class (compbsskin_argument)  :: compbsskin_arguments


  allocate (compbsskin_arguments%umean )
  allocate (compbsskin_arguments%vmean )
  allocate (compbsskin_arguments%depth )
  allocate (compbsskin_arguments%uorb  )
  allocate (compbsskin_arguments%tper  )
  allocate (compbsskin_arguments%teta  )
  allocate (compbsskin_arguments%kssilt)
  allocate (compbsskin_arguments%kssand)
  allocate (compbsskin_arguments%thcmud)
  allocate (compbsskin_arguments%taumax)
  allocate (compbsskin_arguments%wave  )
  allocate (compbsskin_arguments%rhowat)
  allocate (compbsskin_arguments%vicmol)
 end subroutine  allocate_compbsskin

 Subroutine deallocate_compbsskin (compbsskin_arguments)

    implicit none
    class (compbsskin_argument)  :: compbsskin_arguments


  deallocate (compbsskin_arguments%umean )
  deallocate (compbsskin_arguments%vmean )
  deallocate (compbsskin_arguments%depth )
  deallocate (compbsskin_arguments%uorb  )
  deallocate (compbsskin_arguments%tper  )
  deallocate (compbsskin_arguments%teta  )
  deallocate (compbsskin_arguments%kssilt)
  deallocate (compbsskin_arguments%kssand)
  deallocate (compbsskin_arguments%thcmud)
  deallocate (compbsskin_arguments%taumax)
  deallocate (compbsskin_arguments%wave  )
  deallocate (compbsskin_arguments%rhowat)
  deallocate (compbsskin_arguments%vicmol)
 end subroutine  deallocate_compbsskin


subroutine set_compbsskin   (compbsskin_arguments, umean   , vmean     , depth      , wave    , &
                           & uorb, tper  , teta, kssilt  , &
                           & kssand  , thcmud, taumax    , rhowat, vicmol  )
 implicit none
    class (compbsskin_argument)  :: compbsskin_arguments

    real(fp), intent(in)  :: umean  ! depth averaged flow velocity in u-direction
    real(fp), intent(in)  :: vmean  ! depth averaged flow velocity in v-direction
    real(fp), intent(in)  :: depth  ! local water depth
    real(fp), intent(in)  :: uorb   ! orbital velocity based upon Hrms
    real(fp), intent(in)  :: tper   ! wave period
    real(fp), intent(in)  :: teta   ! angle between wave direction and local
                                    ! grid orientation
    real(fp), intent(in)  :: kssilt ! roughness height silt
    real(fp), intent(in)  :: kssand ! roughness height sand
                                    !(not yet used)
    real(fp), intent(in)  :: thcmud ! Total hickness of mud layers
                                    !(to be replaced by mudcnt in future)
    real(fp), intent(out) :: taumax ! resulting (maximum) bed shear stress muddy silt bed
    logical , intent(in)  :: wave   ! wave impacts included in flow comp. or not
    real(fp), intent(in)  :: rhowat ! water density
    real(fp), intent(in)  :: vicmol ! molecular viscosity

  compbsskin_arguments%umean = umean
  compbsskin_arguments%vmean = vmean
  compbsskin_arguments%depth = depth
  compbsskin_arguments%uorb  = uorb
  compbsskin_arguments%tper  = tper
  compbsskin_arguments%teta  = teta
  compbsskin_arguments%kssilt= kssilt
  compbsskin_arguments%kssand= kssand
  compbsskin_arguments%thcmud= thcmud
  compbsskin_arguments%taumax= taumax
  compbsskin_arguments%wave  = wave
  compbsskin_arguments%rhowat= rhowat
  compbsskin_arguments%vicmol= vicmol

end subroutine set_compbsskin

subroutine run_compbsskin (compbsskin_arguments)
 implicit none
    class (compbsskin_argument)  :: compbsskin_arguments

    call compbsskin (compbsskin_arguments%umean   , compbsskin_arguments%vmean     , compbsskin_arguments%depth      , compbsskin_arguments%wave    , &
                           & compbsskin_arguments%uorb, compbsskin_arguments%tper  , compbsskin_arguments%teta, compbsskin_arguments%kssilt  , &
                           & compbsskin_arguments%kssand  , compbsskin_arguments%thcmud, compbsskin_arguments%taumax,  &
                           & compbsskin_arguments%rhowat, compbsskin_arguments%vicmol  )
end subroutine run_compbsskin

subroutine get_compbsskin (compbsskin_arguments, taumax)
 implicit none
    class (compbsskin_argument)  :: compbsskin_arguments
     real(fp), intent(out)  ::  taumax

     taumax = compbsskin_arguments%taumax

end subroutine get_compbsskin
end module erosed_driver
