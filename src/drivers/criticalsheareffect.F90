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

function mircophyto_crit_shear_func ( Chl, inum, jnum) result (fcr_microphyto)
! The function to determine the effect of microphytobenthos on the critical bed shear stress.
! By production of biofilm, this value can increae an order of magnitude.

implicit none
integer                            :: inum, jnum
real (fp), dimension (inum, jnum)  :: fcr_microphyto
type (statevariable)               :: Chl
integer                            :: i,j
!units    ! Unit of Bioamount (mgg: microgram/ g dry sediment weight)
          ! or     (mgm-2 :: microgram/ m**2 area)


 do j = 1, jnum
  do i = 1, inum

     if (trim(Chl%units) == 'mgg' ) then

       fcr_microphyto (i,j) = 1. + 0.08 * Chl%amount(i,j) ! Knaapen et al (2003)

     else

       fcr_microphyto= 1.0

      write (*,*) ' Error: the microphytobenthos effect on critical shear stress can be only'// &
                  ' calculated base on Chlorophyll a content in UNIT microgram /g dry Sediment,'// &
                  '  and not in microgram/ m**2 area. therefore, the effect was not calculated.'
      exit
     end if
  end do
end do
return

end function mircophyto_crit_shear_func

!************************************************************
function Mbalthica_crit_shear_func (Mbalthica, inum, jnum) result (fcr_macrofauna)
! Effect of Macoma balthica intensity on the critical bed seahr stress.
implicit none

type (Mc_statevariable)          :: Mbalthica
integer                          :: inum, jnum
real(fp), dimension (inum, jnum) :: fcr_macrofauna

integer                          :: i,j
!Units    ! Unit of Bioamount (mgg: microgram/ g dry sediment weight)
          ! or     (mgm-2 :: microgram/ m**2 area)

!IT IS TO BE CHANGED
 do j = 1, jnum
  do i = 1, inum

    if (trim(Mbalthica%units) == '-' )  then       ! according to Borsje et al. (2008)
        if (Mbalthica%intensity(i,j) == 0.0_fp ) then

         fcr_macrofauna = 1.0

        else

         fcr_macrofauna (i,j)= 0.0016 * log (Mbalthica%intensity(i,j) * Mbalthica%intensity(i,j)) &
                            & -0.085  * log (Mbalthica%intensity(i,j)) +1.0    ! Knaapen et al (2003)

        endif

    elseif (trim(Mbalthica%units) == 'gCm-2' ) then
         if (Mbalthica%amount(i,j) == 0.0_fp ) then

          fcr_macrofauna = 1.0

         else

          fcr_macrofauna = 1.0
          write (*,*) ' WARNING!! At the moment computation of bioeffect of macrofauna on critical shear stress as a function of gCm-2 is not implemented yet.'// &
                      ' Therefore, it is ignored !!!!!!!'
         endif
         exit
    else if (trim(Mbalthica%units) == '' ) then    ! according to Borsje et al. (2008)

      fcr_macrofauna = 1.0

      write (*,*) ' WARNING!! Missing unit. The Macoma balthica effect on critical bed shear stress can at the'// &
                  ' moment be calculated base on intensity (refer to Knaapen et al. (2003)),'// &
                  ' therefore, without unit this effect is ignored.'
      exit
    end if
  end do
end do

return
end function Mbalthica_crit_shear_func


end module Bio_critical_shear_stress
