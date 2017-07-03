module BenthosEffect_class
! This class is the superclass of all benthos species.
! It is just a template to be extended for microphytobenthos and macrofauna.

use BioTypes

implicit none

type, abstract , public                :: BenthosEffect

! Species could be microphytobenthos, Macrofauna or Macrophytes, or
! the specific name of individual biota, such as Macoma balthica.

  character (50), pointer               :: Species=> null()           ! Name of the species
  integer                               :: UnitNr                     ! the file unit number of input data describing biomass or intensity of the species
  integer                               :: inum, jnum    ! inum and jnum are the number of elementes in x and y directions

  contains

  procedure(init)    ,public , deferred :: initialize    ! allocate and initialize variables, pointers
  procedure(setting) ,public , deferred :: set           ! read data, set variables, prove of validity of input data
  procedure(running) ,public , deferred :: run           ! run the calculations,set the result of calculation to the output and check for validity
  procedure(fin)     ,public , deferred :: finalize      ! deallocate arrays and pointers, close files

end type BenthosEffect

abstract interface

!------------------------------------------------------------

subroutine init (this, inum, jnum)
  import :: BenthosEffect

  implicit none

  class (BenthosEffect)  :: this
  integer, intent (in) :: inum, jnum
end subroutine init

!------------------------------------------------------------

subroutine setting (this, spatialvar, Biounit)
  import :: BenthosEffect
  implicit none

  class (BenthosEffect) :: this
  real(kind=8), dimension (:,:), pointer, optional  :: spatialvar
  character (len = 255), optional  :: Biounit

end subroutine setting

!------------------------------------------------------------

subroutine running (this)
  import :: BenthosEffect
  implicit none

  class (BenthosEffect)  :: this

end subroutine running

!------------------------------------------------------------

subroutine fin (this)
  import :: BenthosEffect
  implicit none

  class (BenthosEffect) :: this

end subroutine fin

!------------------------------------------------------------

end interface

end module BenthosEffect_class

