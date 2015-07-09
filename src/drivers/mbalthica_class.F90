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


subroutine set_Mbalthica(this,spatialvar,Biounit)

implicit none

class (Mbalthica_Object)  :: this
real (fp), dimension (:,:), pointer, optional  :: spatialvar
character (len = 10), optional  :: Biounit


real (fp), dimension (:,:), allocatable :: amounts
character (len = 10)      :: units
integer                   :: StringLength, UnitNr,istat
logical                   :: opnd, exst
real (fp)                 :: amount

namelist /Macrofaun/  units, amount

if (.not.present(spatialvar)) then

  allocate ( amounts ( this%inum, this%jnum ) )
  units = ''
  amount = 0.0_fp
  amounts(:,:)=amount
  this%Species='Mbalthica'

  inquire ( file = 'mbalthica.nml', exist=exst , opened =opnd, Number = UnitNr )
  !write (*,*) 'exist ', exst, 'opened ', opnd, ' file unit', UnitNr

  if (exst.and.(.not.opnd)) then

    UnitNr = 11
    open (unit = UnitNr, file = 'mbalthica.nml', action = 'read ', status = 'old', delim = 'APOSTROPHE')
    !write (*,*) ' in Mbalthica the file unit ', UnitNr, ' was just opened'

    read (UnitNr, nml=Macrofaun, iostat = istat)
    if (istat /= 0 ) stop ' Error in reading Mbalthica data'

  elseif (opnd) then

    write (*,*)  ' In Mbalthica the file unit ', UnitNr, ' alreday opened'

    read (UnitNr, nml=Macrofaun, iostat = istat)
    if (istat /= 0 ) stop 'Error in reading Mbalthica data'

    write (*,*) ' units and  amount are ', units, amount

  else

    write (*,*) ' Warning: The input file for Mbalthica doesnot exists!'
    write (*,*) ' Biological effects on erodibility and bed shear stress are set to 1.'

    this%StateVar%intensity = amounts

    allocate (This%StateVar%units)
    This%StateVar%units = trim (units)

    return
  end if


  if (units == 'ind.m**-2') then

    !write (*,*) ' In Mbalthica_class, the dimensionless density of Mbalthica is ', amount
     amounts(:,:)= amount
     This%StateVar%intensity = amounts
     nullify (This%StateVar%amount)
    ! write (*,*) ' in mbalthica_class: intensity = ', This%StateVar%intensity

  elseif  (units == 'gCm-2') then

 ! write (*,*) ' In Mbalthica_class, the biomass of Mbalthica is ', amount

     !allocate (This%StateVar%amount( this%inum, this%jnum))
     amounts(:,:)= amount
     This%StateVar%amount = amounts
     nullify (This%StateVar%intensity)
  else

    write (*,*) 'Warning! no units has been defined in macrofauna data file. This leads to '
    write (*,*) 'assumption of no biological effect on the sediment transport.'
    This%StateVar%amount = 0.0_fp
    This%StateVar%intensity = 0.0_fp

  end if


  StringLength = len_trim (units)

  if (StringLength /= 0 ) then
   ! allocate (character(StringLength) :: This%StateVar%units)
    allocate (This%StateVar%units)
    This%StateVar%units = trim (units)
  end if


else

  if (biounit == 'ind.m**-2')then

    This%StateVar%intensity = spatialvar
    nullify (This%StateVar%amount)

  elseif(biounit == 'gCm-2') then

    This%StateVar%amount = spatialvar
    nullify (This%StateVar%intensity)
  else
    This%StateVar%amount = 0.0_fp
    This%StateVar%intensity = 0.0_fp
    write (*,*) 'Warning! no units has been defined in macrofauna data file. This leads to '
    write (*,*) 'assumption of no biological effect on the sediment transport.'
  endif

  StringLength = len_trim (biounit)

  if (StringLength /= 0 ) then
   ! allocate (character(StringLength) :: This%StateVar%units)
    if (.not.associated (This%StateVar%units)) allocate (This%StateVar%units)
    This%StateVar%units = trim (biounit)
  end if

endif


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

  deallocate (This%StateVar%intensity)

elseif  (This%StateVar%units == 'gCm-2') then

  deallocate (This%StateVar%amount)

end if

deallocate (This%StateVar%units)
deallocate (this%StateVar)
deallocate (this%Species)
deallocate (this%Bioturbation%TauEffect)
deallocate (this%Bioturbation%ErodibilityEffect)

inquire ( file = 'mbalthica.nml', exist=exst , opened =opnd, Number = UnitNr )
if (opnd) close (UnitNr)


end subroutine fin_Mbalthica

end module Mbalthica_class
