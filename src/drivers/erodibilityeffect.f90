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


function mircophyto_erodibility_func ( Chl, inum, jnum) result (g_erod_microphyto)
! The function to determine the effect of microphytobenthos on the sediment erodibility.
! By production of biofilm, this value can be decreased.
implicit none
integer                            :: inum, jnum  !number of elements in x and y directions
real (fp), dimension (inum, jnum)  :: g_erod_microphyto
type (statevariable)               :: Chl
integer                            :: i, j

!units    ! Unit of Biomass (mgg: microgram/ g dry sediment weight)
          ! or     (mgm-2 :: microgram/ m**2 area)

 do j = 1, jnum
  do i = 1, inum
     if (trim(Chl%units) == 'mgg**-1' ) then
        g_erod_microphyto (i,j)= 1. - 0.018 * Chl%amount(i,j) ! Paarlberg et al (2005)
     else
        g_erod_microphyto= 1.0

      write (*,*) ' Error: the microphytobenthos effect on the erodibility was calculated base on'// &
                  '  Chlorophyll a content in UNIT microgram /g dry Sediment (mgg**-1)'// &
                  '  area. Therefore, the bioeffect was not considered.'
      exit
     end if
  end do
end do

return
end function mircophyto_erodibility_func

!************************************************************

function Mbalthica_erodibility_func (Mbalthica, inum, jnum)  result (g_erod_macrofauna)
! Effect of Macoma balthica on the sediment erodibility.
implicit none

type (Mc_statevariable)              :: Mbalthica
integer                              :: inum, jnum
real (fp) , dimension (inum,jnum)    :: g_erod_macrofauna

integer                              :: i,j
!Amount    ! unit (-) : meaning dimensionless-> indv. /m-2 / 1 indiv. /m-2
!Biomass   ! unit (gcm: meaning gC/m-2)

! local variables from Paarlberg et al. 2005

real (fp)    :: gammaa = 6.0e-7
real (fp)    :: II     = 4.68e-8
real (fp)    :: b1     = 0.995
real (fp)    :: b2     = 5.08e-8


!statements

 do j = 1, jnum
  do i = 1, inum

    if (trim(Mbalthica%units) == 'm**-2' )  then
      if ((Mbalthica%intensity (i,j)<=1.0_fp) ) then

        g_erod_Macrofauna (i,j)= 1.0
        cycle

      else
         g_erod_Macrofauna (i,j)= 1.0   ! due to large uncertainity on validity of the following equation it is left
!        g_erod_Macrofauna (i,j)=    b2 * gammaa /II/(b2 + gammaa * b1** Mbalthica%intensity(i,j)) ! Paarlberg et al (2005)
      end if

    else if(trim(Mbalthica%units) == 'gCm**-2' ) then   ! according to Borsje et al. (2008)
      if (Mbalthica%amount(i,j) <= 1.0_fp ) then

        g_erod_Macrofauna (i,j)= 1.0
        cycle
      else

        g_erod_Macrofauna (i,j) = 0.4989 * log (Mbalthica%amount(i,j)) +0.952


      endif

    else if (trim(Mbalthica%units) == '' ) then

       g_erod_Macrofauna = 1.0

      write (*,*) ' Warning: Macoma balthica misses units, the effect on the erodibility can be calculated at the moment based'// &
                  ' on intensity (refer to Paarlberg et al. (2005)) or biomas per square metere (Borsje et al (2008)'// &
                  '  therefore, the effect was set to 1.0'
     exit
    end if
  end do
end do
return
end function Mbalthica_erodibility_func


end module Bio_erodibility
