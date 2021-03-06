module BioTypes

! This module inculdes the infrastructure (types) to define the effect of Biota
! on sediment transport (erodibility, critical bed shear stress and
! Macrophytes effects on bed roughness, near bed velocity, bed shear stress
! as well as bio-resuspension and bio-deposition).
!It is assumed here that the impacts of benthos on sediment transport
! can be categorized to destabilizing and stabilizing effects on erodibility
! and critical bed shear stress and can be calculated as follow:

!****  Critical Tau (biotic) = critical tau (abiotic) * f1 (destabilizing) * g1(stabilizing)
!**** Erodibility factor (biotic) * f2 (destabilizing) * g2(stabilizing)

! Furthermore, the biological effects on bed roughness is assumed to be
! expressable as follows
!**** d50 (biotic) = d50 (abiotic) * F3
!**** ripple_height(biotic) =ripple_height(abiotic) * F4

!Dr.-Ing. M.Hassan Nasermoaddeli,
!Bundesanstalt Fuer Wasserbau
!Hamburg 19.11.2013

use precision, only : fp
!integer , parameter          :: fp= selected_real_kind (8)

! data type for defining biotic state variables
type statevariable
  character (len=10)        , pointer :: units  => null()
  real (fp), dimension (:,:), pointer :: amount => null()
end type statevariable

! data type for biotic effects on sediment properties
type SedEffect
  real (fp), pointer  :: Darcy=> null()               !roughness effect
  real (fp), pointer  :: Chezy=> null()               !roughness effect
  real (fp), pointer  :: Manning=> null()             !roughness effect
  real (fp), pointer  :: z0=> null()                  !roughness effect
  real (fp), pointer  :: Rippleheight=> null()        !effect on sand waves height (ripples)
end type SedEffect

! data type for biotic effects on near bed flow field
type NearBedEffect
  real (fp), pointer  :: BedShearStress=> null()              !effect on bed shear stress
  real (fp), pointer  :: NearBedVelocity=> null()             !effect on near bed velocity
end type NearBedEffect

! data type for biotic effects on flow and roughness
type BiogenicStructures
  type (SedEffect)         , pointer  :: roughness=> null()
  type (NearBedEffect)     , pointer  :: FlowEffect=> null()
end type BiogenicStructures

! data type for biotic effects on erosion
type BioturbationEffect
  real (fp), dimension(:,:), pointer  :: TauEffect => null()          !effect on critical bed shear stress
  real (fp), dimension(:,:), pointer  :: ErodibilityEffect => null()  !effect on erodibility parameter
  real (fp), dimension(:,:), pointer  :: d50=> null()                 !effect on changing sediment grain distribution
  real (fp), dimension(:,:), pointer  :: MudContent=> null()          !effect on changing mud content on the upper soil layers
end type BioturbationEffect

! data type for direct biological effects on sediment flux
! It is not set at the moment
type DirectBioEffect
  real (fp)                 , pointer  :: Bio_resuspensionRate=> null()   ! direct bioresuspension of suspended/or deposit feeders (macrofauna)
  real (fp)                 , pointer  :: Bio_depositionRate=> null()     ! direct biodeposition of suspended feeders (macrofauna)
  real (fp)                 , pointer  :: SettlingVelocityEffect=> null() ! direct effect of supension and deposit feeders on the settling velocity
end type DirectBioEffect

end module BioTypes
