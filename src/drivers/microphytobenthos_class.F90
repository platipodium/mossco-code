module Microphytobenthos_class

! The microphytobenthos class is a subclass of superclass Benthos_Effect. It comprises The effect of
! microphytobenthos on erodibility and crticial bed shear stress.

use BenthosEffect_class

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

!allocate (character (17) :: this%Species)
allocate (this%Species)
allocate (this%BioMass)
allocate (This%BioMass%amount)
!allocate (This%BioMass%Unitt)
allocate (this%TauEffect(inum,jnum))
allocate (this%ErodibilityEffect (inum,jnum),stat= istatus)

!if (istatus == 0) then
!    write (*,*) 'allocation of ErodibilityEffect was successfull'
!else
!    write (*,*) 'Error , allocation of ErodibilityEffect was NOT successfull'
!end

  this%inum      = inum
  this%jnum      = jnum
  this%TauEffect = 1.0_fp
  this%ErodibilityEffect = 1.0_fp

end subroutine init_microphyt

subroutine set_microphyt (this)
use BioTypes
implicit none

class (Microphytobenthos)  :: this
real (fp)                  :: Mass
character (len = 10)       :: Unitt
integer                    :: StringLength, UnitNr, istat
logical                    :: opnd, exst


namelist /Microphyto/ Unitt, Mass

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

 write (*,*) ' in Microphytobenthos the file unit ', UnitNr, ' already opened'
 read (UnitNr, nml=Microphyto, iostat = istat)

 if (istat /= 0 ) write (*,*) ' Error in reading Microphytobenthos data'

else

 write (*,*) 'ERROR: The input file for Microphytobenthos doesnot exists!'
 stop

end if

 this%UnitNr = UnitNr
 write (*,*) ' In Microphytobenthos_class, the amount of Chl biomass is ', Mass
 write (*,*) ' Unit is ', Unitt

 This%BioMass%amount = Mass

StringLength = len_trim (unitt)

if (StringLength /= 0 ) then
    !allocate (character(StringLength) :: This%BioMass%Unitt)
allocate (This%BioMass%Unitt)
    This%BioMass%Unitt = trim (unitt)
end if



close (UnitNr)

end subroutine set_microphyt


subroutine run_microphyt (this)
use Bio_critical_shear_stress
use Bio_erodibility


implicit none
!#define DEBUG
class (Microphytobenthos) :: this

integer                   :: i,j

do i = 1, this%jnum
 do j = 1, this%inum
     this%TauEffect (i,j)         =  Crit_shear_bioeffect(this%BioMass)
     this%ErodibilityEffect (i,j) =  erodibility_bioeffect(this%BioMass)
 end do
end do

#ifdef DEBUG
write (*,*) ' Biotic effect of ', this%Species, ' on tau ( Micro%TauEffect) ', this%TauEffect
write (*,*)
write (*,*) 'Biotic effect of ' , this%Species, ' on the Erodibility (Micro%ErodibilityEffect): ', this%ErodibilityEffect
write (*,*)
#endif

end subroutine run_microphyt

subroutine fin_microphyt (this)

implicit none

class (Microphytobenthos) :: this
integer                   :: UnitNr
logical                   :: opnd, exst


deallocate (This%BioMass%amount)
deallocate (This%BioMass%Unitt)
deallocate (this%BioMass)
deallocate (this%Species)
deallocate (this%TauEffect)
deallocate (this%ErodibilityEffect)

inquire ( file = 'microphyt.nml', exist=exst , opened =opnd, Number = UnitNr )
if (opnd) close (UnitNr)

end subroutine fin_microphyt


end module Microphytobenthos_class
