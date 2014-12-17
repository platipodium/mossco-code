module Microphytobenthos_class

! The microphytobenthos class is a subclass of superclass Benthos_Effect. It comprises The effect of
! microphytobenthos on erodibility and crticial bed shear stress.
use BioTypes
use BenthosEffect_class
use Bio_critical_shear_stress
use Bio_erodibility

implicit none
type , public, extends (BenthosEffect) :: Microphytobenthos

type (statevariable)      , pointer :: BioMass => null()
real (fp),dimension (:,:) , pointer :: TauEffect => null()
real (fp),dimension (:,:) , pointer :: ErodibilityEffect => null()


contains

 procedure, public, pass :: initialize=> init_microphyt
 procedure, public, pass :: set=> set_microphyt
 procedure, public, pass :: run=> run_microphyt
 procedure, public, pass :: finalize=> fin_microphyt
end type
!private :: init_microphyt, set_microphyt, run_microphyt,fin_microphyt
contains

subroutine init_microphyt (this,inum, jnum)

implicit none

class (Microphytobenthos) :: this
integer, intent (in)      :: inum, jnum ! inum and jnum are the number of elementes in x and y directions
integer                   :: istatus

allocate (this%Species)
allocate (this%BioMass)
allocate (This%BioMass%amount(inum,jnum))
allocate (this%TauEffect(inum,jnum))
allocate (this%ErodibilityEffect (inum,jnum),stat= istatus)

!if (istatus == 0) then
!    write (*,*) 'allocation of ErodibilityEffect was successfull'
!else
!    write (*,*) 'Error , allocation of ErodibilityEffect was NOT successfull'
!end if

  this%inum      = inum
  this%jnum      = jnum
  this%TauEffect = 1.0_fp
  this%ErodibilityEffect = 1.0_fp
  this%BioMass%amount    = 0._fp

end subroutine init_microphyt

subroutine set_microphyt (this)

implicit none

class (Microphytobenthos)  :: this
real (fp), dimension (:,:), allocatable  :: Mass
character (len = 10)       :: units
integer                    :: StringLength, UnitNr, istat
logical                    :: opnd, exst


namelist /Microphyto/ units, Mass

allocate ( Mass ( this%inum , this%jnum ) )

this%Species='Microphytobenthos'

inquire ( file = 'microphyt.nml', exist=exst , opened =opnd, Number = UnitNr )
!write (*,*) 'exist ', exst, 'opened ', opnd, ' file unit', UnitNr

if (exst.and.(.not.opnd)) then

 UnitNr = 11
 open (unit = UnitNr, file = 'microphyt.nml', action = 'read ', status = 'old', delim = 'APOSTROPHE')
! write (*,*) ' in Microphytobenthos the file unit ', UnitNr, ' was just opened'

 read (UnitNr, nml=Microphyto, iostat = istat)
 if (istat /= 0 ) write (*,*) ' Error in reading Microphytobenthos data'

elseif (opnd) then

 write (*,*) ' In Microphytobenthos the file unit ', UnitNr, ' already opened'
 read (UnitNr, nml=Microphyto, iostat = istat)

 if (istat /= 0 ) write (*,*) ' Error in reading Microphytobenthos data'

else

 write (*,*) ' Warning: The input file for Microphytobenthos doesnot exists!'
 write (*,*) ' Biological effects on erodibility and bed shear stress are set to 1.'

end if

 this%UnitNr = UnitNr
 !write (*,*) ' In Microphytobenthos_class, the amount of Chl biomass is ', Mass
 write (*,*) ' Units are ', units

 This%BioMass%amount = Mass

 StringLength = len_trim (units)

if (StringLength /= 0 ) then
    !allocate (character(StringLength) :: This%BioMass%units)
    allocate (This%BioMass%units)
    This%BioMass%units = trim (units)
end if



close (UnitNr)

end subroutine set_microphyt


subroutine run_microphyt (this)

implicit none
!#define DEBUG
class (Microphytobenthos) :: this

integer                   :: i,j

!do i = 1, this%inum
 !do j = 1, this%jnum
     this%TauEffect          =  Crit_shear_bioeffect(this%BioMass, this%inum, this%jnum)
     this%ErodibilityEffect  =  erodibility_bioeffect(this%BioMass, this%inum, this%jnum)
 !end do
!end do

#ifdef DEBUG
write (*,*) ' Biotic effect of ', this%Species, ' on tau ( Micro%TauEffect) ', this%TauEffect
write (*,*)
write (*,*) ' Biotic effect of ' , this%Species, ' on the Erodibility (Micro%ErodibilityEffect): ', this%ErodibilityEffect
write (*,*)
#endif

end subroutine run_microphyt

subroutine fin_microphyt (this)

implicit none

class (Microphytobenthos) :: this
integer                   :: UnitNr
logical                   :: opnd, exst


deallocate (This%BioMass%amount)
deallocate (This%BioMass%units)
deallocate (this%BioMass)
deallocate (this%Species)
deallocate (this%TauEffect)
deallocate (this%ErodibilityEffect)

inquire ( file = 'microphyt.nml', exist=exst , opened =opnd, Number = UnitNr )
if (opnd) close (UnitNr)

end subroutine fin_microphyt


end module Microphytobenthos_class
