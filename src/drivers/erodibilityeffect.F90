module Bio_erodibility
!Effects of biota on the sediment erodibility are defined here.
!For each species a function is defined within this module.

!use BioTypes
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

!------------------------------------------------------------
  contains
!------------------------------------------------------------

function mircophyto_erodibility_func ( Chl, inum, jnum) result (g_erod_microphyto)
! Function to determine the effect of microphytobenthos on sediment erodibility.
! By production of biofilm, this value can be decreased.

  implicit none
  integer                            :: inum, jnum  !number of elements in x and y directions
  real (fp), dimension (inum, jnum)  :: g_erod_microphyto
  type (statevariable)               :: Chl
  integer                            :: i, j
  real(fp)                           :: a

  ! local parameters Paarlberg et al. 2005
  real(fp), parameter :: b1     = 0.018

  !units    ! Unit of Biomass (mgg: microgram/ g dry sediment weight)
            ! or            (mgm-2: microgram/ m**2 area)

  g_erod_microphyto = 1.0

  select case (trim(Chl%units))

    case ( 'mgg**-1' ) ! according to Paarlberg et al (2005)

      do j = 1, jnum
        do i = 1, inum
          a = Chl%amount(i,j)
          g_erod_microphyto(i,j) = 1. - b1*a  ! Paarlberg et al (2005)
        end do
      end do

    case default

      !do nothing except applying default value

      !write (*,*) ' WARNING: the microphytobenthos effect on the erodibility was calculated base on'// &
      !            ' Chlorophyll a content in UNIT microgram /g dry Sediment (mgg**-1)'// &
      !            ' area. Therefore, the bioeffect was not considered.'

  end select

  return

end function mircophyto_erodibility_func

!------------------------------------------------------------

function Mbalthica_erodibility_func (Mbalthica, inum, jnum)  result (g_erod_macrofauna)
! Effect of Macoma balthica on the sediment erodibility.

  implicit none

  type (Mc_statevariable)              :: Mbalthica
  integer                              :: inum, jnum
  real (fp) , dimension (inum,jnum)    :: g_erod_macrofauna
  integer                              :: i,j
  real(fp)                             :: a

  ! local parameters Paarlberg et al. 2005
  real(fp), parameter :: gammaa = 6.0e-7
  real(fp), parameter :: II     = 4.68e-8
  real(fp), parameter :: b1     = 0.995
  real(fp), parameter :: b2     = 5.08e-8

  ! local parameters Borsje et al. 2008
  real(fp), parameter :: c1     = 0.4989
  real(fp), parameter :: c2     = 0.952

  !Amount    ! unit (-) : meaning dimensionless-> indv. /m-2 / 1 indiv. /m-2
  !Biomass   ! unit (gcm: meaning gC/m-2)

  ! initialize with default value
  g_erod_Macrofauna = 1.0

  select case (trim(Mbalthica%units))
    case ( 'm**-2' ) ! according to Paarlberg et al (2005)

      do j = 1, jnum
        do i = 1, inum

          a  = Mbalthica%intensity(i,j)
          if ( a <= 1.0_fp ) then
            cycle
          else
            g_erod_Macrofauna(i,j) = b2 *gammaa /II /(b2 + gammaa * b1**a)
          endif

        end do
      end do

    case ( 'gCm**-2' ) ! according to Borsje et al. (2008)

      do j = 1, jnum
        do i = 1, inum

          a = Mbalthica%amount(i,j)
          if ( a <= 1.0_fp ) then
            cycle
          else
            g_erod_Macrofauna(i,j) = c1 *log(a) +c2
          endif

        end do
      end do

    case default

      !do nothing except applying default value

      !write (*,*) ' WARNING: Macoma balthica misses units, the effect on the erodibility can be calculated at the moment based'// &
      !            ' on intensity (refer to Paarlberg et al. (2005)) or biomas per square metere (Borsje et al (2008)'// &
      !            '  therefore, the effect was set to 1.0'

  end select

  return

end function Mbalthica_erodibility_func

!------------------------------------------------------------

end module Bio_erodibility
