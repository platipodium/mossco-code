module Mbalthica_class
! this module defines Macoma balthica object and is an instance of Macrofauna class.
! Every other macrofauna species should be defined as a seprate object in a similar way.
! Diefferent features of macrofauna class can be activatated here.
! However, for M. balthicas only the biological effect on the critical bed shear stress
! and erodibility has been activated.

use BioTypes
use macrofauna_class
use Bio_critical_shear_stress
use Bio_erodibility

type , extends (Macrofauna),public  :: Mbalthica_Object

contains

 procedure, public, pass :: initialize=> init_Mbalthica
 procedure, public, pass :: set=> set_Mbalthica
 procedure, public, pass :: run=> run_Mbalthica
 procedure, public, pass :: finalize=> fin_Mbalthica

end type Mbalthica_Object

contains

subroutine  init_Mbalthica(this, inum, jnum)

implicit none


class (Mbalthica_Object) :: this
integer, intent (in)     :: inum, jnum ! dimesions of grid in x and y directions
!integer :: istatus

allocate (this%Species)
allocate (this%StateVar)
allocate (This%StateVar%amount(inum,jnum))
allocate (This%StateVar%intensity(inum,jnum))
allocate (this%Bioturbation)
allocate (this%Bioturbation%TauEffect(inum,jnum))
allocate (this%Bioturbation%ErodibilityEffect(inum,jnum))
!allocate (this%Bioturbation%ErodibilityEffect,stat= istatus)
!if (istatus == 0) then
!    write (*,*) 'allocation of ErodibilityEffect was successfull'
!else
!    write (*,*) 'Error , allocation of ErodibilityEffect was NOT successfull'
!end if

  this%inum      = inum
  this%jnum      = jnum

end subroutine init_Mbalthica


subroutine set_Mbalthica(this)

implicit none

class (Mbalthica_Object)  :: this
real (fp), dimension (:,:), allocatable :: amount
character (len = 10)      :: units
integer                   :: StringLength, UnitNr,istat
logical                   :: opnd, exst

namelist /Macrofaun/  units, amount
allocate ( amount ( this%inum, this%jnum ) )

units = ''
amount = 0.0_fp

this%Species='Mbalthica'

inquire ( file = 'mbalthica.nml', exist=exst , opened =opnd, Number = UnitNr )
!write (*,*) 'exist ', exst, 'opened ', opnd, ' file unit', UnitNr

if (exst.and.(.not.opnd)) then

 UnitNr = 11
 open (unit = UnitNr, file = 'mbalthica.nml', action = 'read ', status = 'old', delim = 'APOSTROPHE')
 !write (*,*) ' in Mbalthica the file unit ', UnitNr, ' was just opened'

 read (UnitNr, nml=Macrofaun, iostat = istat)
 if (istat /= 0 ) write (*,*) ' Error in reading Mbalthica data'

elseif (opnd) then

  write (*,*)  ' In Mbalthica the file unit ', UnitNr, ' alreday opened'

  read (UnitNr, nml=Macrofaun, iostat = istat)
  if (istat /= 0 ) write (*,*)' Error in reading Mbalthica data'

  write (*,*) ' units and  amount are ', units, amount

else

 write (*,*) ' Warning: The input file for Mbalthica doesnot exists!'
 write (*,*) ' Biological effects on erodibility and bed shear stress are set to 1.'

end if

 if (units == '-') then

  write (*,*) ' In Mbalthica_class, the dimensionless density of Mbalthica is ', amount

  This%StateVar%intensity = amount
  nullify (This%StateVar%amount)

 elseif  (units == 'gCm-2') then

  write (*,*) ' In Mbalthica_class, the biomass of Mbalthica is ', amount

  allocate (This%StateVar%amount( this%inum, this%jnum))

  This%StateVar%amount = amount
  nullify (This%StateVar%intensity)
 end if


 StringLength = len_trim (units)

if (StringLength /= 0 ) then
   ! allocate (character(StringLength) :: This%StateVar%units)
    allocate (This%StateVar%units)
    This%StateVar%units = trim (units)
end if

end subroutine set_Mbalthica

subroutine run_Mbalthica(this)

implicit none

class (Mbalthica_Object) :: this

integer                  :: i,j

!do j = 1, this%jnum
 !do i = 1, this%inum
    this%Bioturbation%TauEffect         =  Crit_shear_bioeffect (this%StateVar, this%inum, this%jnum)
    this%Bioturbation%ErodibilityEffect =  erodibility_bioeffect(this%StateVar, this%inum, this%jnum)
 !end do
!end do

#ifdef DEBUG
write (*,*)
write (*,*) 'Biotic effect of ', this%Species, ' on the tau (M_balthica%Bioturbation%TauEffect): '&
            , this%Bioturbation%TauEffect
write (*,*)
write (*,*) 'Biotic effect of ', this%Species, ' on the Erodibility (M_balthica%Bioturbation%ErodibilityEffect): '&
            , this%Bioturbation%ErodibilityEffect
write (*,*)
#endif

end subroutine run_Mbalthica

subroutine fin_Mbalthica (this)

implicit none

class (Mbalthica_Object)  :: this
integer                   :: UnitNr
logical                   :: opnd, exst

if (This%StateVar%units == '-') then

  deallocate (This%StateVar%amount)

elseif  (This%StateVar%units == 'gCm-2') then

  deallocate (This%StateVar%amount)

end if

deallocate (This%StateVar%units)
deallocate (this%StateVar)
deallocate (this%Species)
deallocate (this%Bioturbation%TauEffect)
deallocate (this%Bioturbation%ErodibilityEffect)

inquire ( file = 'microphyt.nml', exist=exst , opened =opnd, Number = UnitNr )
if (opnd) close (UnitNr)


end subroutine fin_Mbalthica

end module Mbalthica_class
