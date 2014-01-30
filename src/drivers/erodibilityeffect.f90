module Bio_erodibility
!Effect of biota on the sediment erodibility is defined here as functions.
!For each species a function is defined within this module.

use BioTypes
use macrofauna_class
implicit none

interface erodibility_bioeffect
! here new procedures for different species should be defined.
! It shoudl be noted that for generic functions the arguments of functions
! determine, which one should be used. It sould be avoided to deifne a generic function
! with the same arguments as an already avaialble function listed here.
! If it is inevitable then the function is not allowed to be defined within this interface,
! but as a seprate function within this module.
module procedure mircophyto_erodibility_func
module procedure Mbalthica_erodibility_func
end interface

contains


function mircophyto_erodibility_func ( Chl) result (g_erod_microphyto)
! The function to determine the effect of microphytobenthos on the sediment erodibility.
! By production of biofilm, this value can be decreased.
implicit none
real (fp)                    :: g_erod_microphyto
type (statevariable)         :: Chl

!Uunit    ! Unit of Biomass (mgg: microgram/ g dry sediment weight)
          ! or     (mgm-2 :: microgram/ m**2 area)


!statements

!if (allocated (Chl%unitt) ) then

     if (trim(Chl%unitt) == 'mgg' ) then
        g_erod_microphyto= 1. - 0.018 * Chl%amount ! Paarlberg et al (2005)
     else
        g_erod_microphyto= 1.0

      write (*,*) ' Error: the microphytobenthos effect on the erodibility was calculated base on'// &
                  '  Chlorophyll a content in UNIT microgram /g dry Sediment, and not in microgram/ m**2'// &
                  '  area. Therefroe, the bioeffect was not considered.'

     end if

!else
!
!      g_erod_microphyto= 1. - 0.018 * Chl%amount ! Paarlberg et al (2005)
!
!      write (*,*) ' Warning: the microphytobenthos effect on the erodibility has been calculated based on the assumaption of  &
!          &        Chlorophyll a content in UNIT microgram /g dry Sediment, and not in microgram/ m**2 area.'
!
!end if
return
end function mircophyto_erodibility_func

!************************************************************

function Mbalthica_erodibility_func (Mbalthica)  result (g_erod_macrofauna)
! Effect of Macoma balthica on the sediment erodibility.
use macrofauna_class
implicit none
real (fp)                            :: g_erod_macrofauna
type (Mc_statevariable)              :: Mbalthica

!Intensity ! unit (-) : meaning dimensionless-> indv. /m-2 / 1 indiv. /m-2
!Biomass   ! unit (gcm: meaning gC/m-2)

! local variables

real (fp)    :: gammaa = 6.0e-7
real (fp)    :: I = 4.68e-8
real (fp)    :: b1 = 0.995
real (fp)    :: b2 = 5.08e-8


!statements


!if (ALLOCATED (Mbalthica%unitt) ) then

    if (trim(Mbalthica%unitt) == '-' )  then


       g_erod_Macrofauna=    b2 * gammaa /I/(b2 + gammaa * b1** Mbalthica%Intensity) ! Paarlberg et al (2005)

    else if (trim(Mbalthica%unitt) == '' ) then    ! according to Borsje et al. (2008)

       g_erod_Macrofauna= 1.0

      write (*,*) ' Error: the Macoma balthica effect on the erodibility can be calculated at the moment based'// &
                  ' on intensity (refer to Paarlberg et al. (2005)), therefore, the effect based on Biomass'// &
                  '  was set to 1.0'

    end if

!end if
return
end function Mbalthica_erodibility_func


end module Bio_erodibility
