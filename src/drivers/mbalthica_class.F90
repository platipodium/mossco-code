module Mbalthica_class
! this module defines Macoma balthica object and is an instance of Macrofauna class.
! Every other macrofauna species should be defined as a seprate object in a similar way.
! Diefferent features of macrofauna class can be activatated here.
! However, for M. balthicas only the biological effect on the critical bed shear stress
! and erodibility has been activated.

use macrofauna_class


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

!allocate (character (9) :: this%Species)
allocate (this%Species)
allocate (this%StateVar)
!allocate (This%StateVar%amount)
!allocate (This%BioMass%units)
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

use BioTypes
implicit none

class (Mbalthica_Object)  :: this
real (fp)                 :: Mass,intensity
character (len = 10)      :: units
integer                   :: StringLength, UnitNr,istat
logical                   :: opnd, exst

namelist /Macrofaun/  units, Intensity

units = ''
Mass = 0.0
Intensity = 0.0

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

  write (*,*) ' units and  intensity are ', units, Intensity

else

 write (*,*) 'ERROR: The input file for Mbalthica doesnot exists!'
 stop

end if


 if (units == '-') then

  write (*,*) ' In Mbalthica_class, the intensity of Mbalthica is ', Intensity

  allocate (This%StateVar%Intensity)
  This%StateVar%Intensity = intensity

 elseif  (units == 'gCm-2') then

  write (*,*) ' In Mbalthica_class, the Mass of Mbalthica is ', Mass
  allocate (This%StateVar%amount)
  This%StateVar%amount = Mass

 end if


 StringLength = len_trim (units)

if (StringLength /= 0 ) then
   ! allocate (character(StringLength) :: This%StateVar%units)
    allocate (This%StateVar%units)
    This%StateVar%units = trim (units)
end if

end subroutine set_Mbalthica

subroutine run_Mbalthica(this)

use Bio_critical_shear_stress
use Bio_erodibility


implicit none

class (Mbalthica_Object) :: this

integer                  :: i,j
do i = 1, this%jnum
 do j = 1, this%inum
    this%Bioturbation%TauEffect(i,j)         =  Crit_shear_bioeffect(this%StateVar)
    this%Bioturbation%ErodibilityEffect(i,j) = erodibility_bioeffect(this%StateVar) 
 end do
end do


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

class (Mbalthica_Object) :: this
integer                   :: UnitNr
logical                   :: opnd, exst

if (This%StateVar%units == '-') then

  deallocate (This%StateVar%Intensity)

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
