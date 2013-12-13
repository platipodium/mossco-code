module Bio_critical_shear_stress

!Effect of biota on the critical bed shear stress is defined here as functions
!For each species a function is defined within this module.

use BioTypes
use macrofauna_class

implicit none

interface Crit_shear_bioeffect
! here new procedures for different species should be defined.
! it shoudl be noted that for generic functions the arguments of functions
! determine, which one should be used. It sould be avoided to deifne a generic function
! with the same arguments as an already avaialble function listed here.
! If it is inevitable then the function is not allowed to be defined within this interface,
! but as a seprate function within this module.

module procedure mircophyto_crit_shear_func
module procedure Mbalthica_crit_shear_func
end interface
contains

function mircophyto_crit_shear_func ( Chl) result (fcr_microphyto)
! The function to determine the effect of microphytobenthos on the critical bed shear stress.
! By production of biofilm, this value can increae an order of magnitude.

implicit none
real (fp)                          :: fcr_microphyto
type (statevariable)               :: Chl

!Uunit    ! Unit of Biomass (mgg: microgram/ g dry sediment weight)
          ! or     (mgm-2 :: microgram/ m**2 area)


!if (allocated (Chl%unitt) ) then

     if (trim(Chl%unitt) == 'mgg' ) then

       fcr_microphyto= 1. + 0.08 * Chl%amount ! Knaapen et al (2003)

     else

       fcr_microphyto= 1.0

      write (*,*) ' Error: the microphytobenthos effect on critical shear stress can be only calculated base on Chlorophyll a  &
      &             content in UNIT microgram /g dry Sediment, and not in microgram/ m**2 area. therefore, the effect was not calculated.'

     end if
!else
!
!       fcr_microphyto= 1. + 0.08 * Chl%MassContent ! Knaapen et al (2003)
!
!      write (*,*) ' Warning: the microphytobenthos effect on critical shear stress has been calculated based on the assumaption of  &
!      &            Chlorophyll a content in UNIT microgram /g dry Sediment, and not in microgram/ m**2 area.'
!
!
!end if
return

end function mircophyto_crit_shear_func

!************************************************************
function Mbalthica_crit_shear_func ( Mbalthica) result (fcr_macrofauna)
! Effect of Macoma balthica on the critical bed seahr stress.
use macrofauna_class
implicit none

real(fp)                         :: fcr_macrofauna
type (Mc_statevariable)          :: Mbalthica

!Uunit    ! Unit of Biomass (mgg: microgram/ g dry sediment weight)
          ! or     (mgm-2 :: microgram/ m**2 area)

!IT IS TO BE CHANGED
!if (ALLOCATED (Mbalthica%unitt) ) then

    if (trim(Mbalthica%unitt) == '-' )  then

      fcr_macrofauna = 0.0016 * log (Mbalthica%Intensity **2) -0.085 * log(Mbalthica%Intensity) +1.0    ! Knaapen et al (2003)

    else if (trim(Mbalthica%unitt) == '' ) then    ! according to Borsje et al. (2008)

      fcr_macrofauna = 1.0

      write (*,*) ' Error: the Macoma balthica effect on critical bed shear stress can at the moment be calculated base on   &
      &             intensity (refer to Knaapen et al. (2003)), therefore, the result based on Biomass is set to 1.0'

    end if

!end if
return
end function Mbalthica_crit_shear_func


end module Bio_critical_shear_stress
