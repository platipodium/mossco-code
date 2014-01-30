module BenthosEffect_class
! This class is the superclass of all benthos species.
! It is just a template to be extended for microphytobenthos and macrofauna.

use BioTypes

implicit none

type, abstract , public  :: BenthosEffect

! Species could be microphytobenthos, Macrofauna or Macrophytes, or
! the specific name of individual biota, such as Macoma baltica.

 character (512), pointer  :: Species=> null()           ! Name of the species
 integer                 :: UnitNr                     ! the file unit number of input data describing biomass or intensity of the species

 contains

 procedure (init)   ,public , deferred :: initialize    ! allocate and initialize variables, pointers
 procedure(setting) ,public , deferred :: set           ! read data, set variables, prove of validity of input data
 procedure(running) ,public , deferred :: run           ! run the calculations,set the result of calculation to the output and check for validity
 procedure(fin)     ,public , deferred :: finilize      ! deallocate arrays and pointers, close files

end type BenthosEffect

abstract interface

subroutine init (this)
import :: BenthosEffect

implicit none

class (BenthosEffect)  :: this

end subroutine init


subroutine setting (this)
import :: BenthosEffect
implicit none

class (BenthosEffect) :: this

end subroutine setting

subroutine running (this)
import :: BenthosEffect
implicit none

class (BenthosEffect)  :: this

end subroutine running

subroutine fin (this)
import :: BenthosEffect
implicit none

class (BenthosEffect) :: this

end subroutine fin

end interface
end module BenthosEffect_class

