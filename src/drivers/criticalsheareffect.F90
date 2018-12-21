module Bio_critical_shear_stress
!Effects of biota on the critical bed shear stress are defined here.
!For each species a function is defined within this module.

use BioTypes
use macrofauna_class
implicit none

interface Crit_shear_bioeffect
! Here, new procedures for different species should be defined.
! It should be noted, that for generic functions, the arguments of functions
! determine the specific procedure to be used. It sould be avoided to define
! generic functions with the same arguments as already available functions
! listed here. If it is inevitable, then the function is not allowed to be
! defined within this interface, but as a separate function within this module.
  module procedure benthos_microphyto_critical_shearstress_function
  module procedure benthos_macrofauna_critical_shearstress_function
end interface

!------------------------------------------------------------
  contains
!------------------------------------------------------------

function benthos_microphyto_critical_shearstress_function ( Chl, inum, jnum) result (fcr_microphyto)
! Function to determine the effect of microphytobenthos on the critical bed shear stress.
! By production of biofilm, this value can increase an order of magnitude.

  implicit none
  integer                            :: inum, jnum
  real (fp), dimension (inum, jnum)  :: fcr_microphyto
  type (statevariable)               :: Chl
  integer                            :: i,j
  real(fp)                           :: a

  ! local parameters Knaapen et al. 2003
  real(fp), parameter :: b1     = 0.08

  !units    ! Unit of Bioamount (mug g-1: microgram/ g dry sediment weight)
            !        or       (mug m-2: microgram/ m**2 area)
            ! With b1 above and typical range of microphytobenthos 0--150 mug g-1
            ! the critical shear stress is between 1 and 10

  fcr_microphyto = 1.0

  select case (trim(Chl%units))

  case ( 'mug g-1' ) ! according to Knaapen et al (2003)

      do j = 1, jnum
        do i = 1, inum
          a = Chl%amount(i,j)
          fcr_microphyto(i,j) = 1. + b1*a  ! Knaapen et al (2003)
        end do
      end do

    case default

      write(0,*) 'FATAL ERROR criticalsheareffect.F90:',__LINE__,' microphytobenthos needs unit "mug g-1", got '//trim(Chl%units)
      stop
      ! Missing unit: the microphytobenthos effect on critical shear stress can be only'// &
      !            ' calculated base on Chlorophyll a content in UNIT microgram /g dry Sediment (mug g-1),'// &
      !            ' therefore, the effect was not calculated.'

  end select

  return

end function benthos_microphyto_critical_shearstress_function

!------------------------------------------------------------

function benthos_macrofauna_critical_shearstress_function (Mbalthica, inum, jnum) result (fcr_macrofauna)
! Effect of Macoma balthica intensity on the critical bed shear stress.

  implicit none

  type (Mc_statevariable)          :: Mbalthica
  integer                          :: inum, jnum
  real(fp), dimension (inum, jnum) :: fcr_macrofauna
  integer                          :: i,j
  real(fp)                         :: a

  ! local parameters Knaapen et al. 2003
  real(fp), parameter :: b1     = 0.0016
  real(fp), parameter :: b2     = 0.085

  ! local parameters Borsje et al. 2008
  real(fp), parameter :: c1     = -0.15
  real(fp), parameter :: c2     = 0.978

  !Units    ! Unit of Bioamount (mug g: microgram/ g dry sediment weight)
            ! or              (mug m-2: microgram/ m**2 area)

  ! initialize with default value
  fcr_macrofauna = 1.0

  select case (trim(Mbalthica%units))
    case ( 'm-2' ) ! according to Knaapen et al (2003)

      do j = 1, jnum
        do i = 1, inum

          a = Mbalthica%intensity(i,j)
          if ( a <= 1.0_fp ) then
            cycle
          else
            fcr_macrofauna(i,j) = b1 *log(a*a) - b2 *log(a) +1.0    ! Knaapen et al (2003)
          endif

        end do
      end do

    case ( 'gC m-2' ) ! according to Borsje et al. (2008)

      do j = 1, jnum
        do i = 1, inum

          a = Mbalthica%amount(i,j)
          if ( a <= 1.0_fp ) then
            cycle
          else
            fcr_macrofauna(i,j) = c1 *log(a) +c2  ! Borsje et al (2008), digitalized graphics
          endif

        end do
      end do

    case default

      !do nothing except applying default value

      write(0,*) 'FATAL ERROR criticalsheareffect.F90:',__LINE__,' macrofauna needs unit "gC m-2" or "m-2", got '//trim(Mbalthica%units)
      stop
      !write (*,*) ' WARNING!! Missing unit. The Macoma balthica effect on critical bed shear stress can at the'// &
      !            ' moment be calculated base on intensity (refer to Knaapen et al. (2003)),'// &
      !            ' ,therefore, without unit this effect is ignored.'

  end select

  return

end function benthos_macrofauna_critical_shearstress_function

!------------------------------------------------------------

end module Bio_critical_shear_stress
