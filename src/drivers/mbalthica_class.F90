module Mbalthica_class
! this module defines Macoma balthica object and is an instance of Macrofauna class.
! Every other macrofauna species should be defined as a seprate object in a similar way.
! Diefferent features of macrofauna class can be activatated here.
! However, for M. balthica only the biological effect on the critical bed shear stress
! and erodibility has been activated.

use BioTypes
use macrofauna_class
use Bio_critical_shear_stress
use Bio_erodibility

type , extends (Macrofauna),public  :: Mbalthica_Object
!type , extends (Macrofauna_class),public  :: Mbalthica_class

  contains

  procedure, public, pass :: initialize=> init_Mbalthica
  procedure, public, pass :: set=> set_Mbalthica
  procedure, public, pass :: run=> run_Mbalthica
  procedure, public, pass :: finalize=> fin_Mbalthica

end type Mbalthica_Object

character(len=*), parameter :: nml_name = 'mbalthica.nml'

!------------------------------------------------------------
  contains
!------------------------------------------------------------

subroutine init_Mbalthica(this, inum, jnum)

  implicit none

  class (Mbalthica_Object) :: this
  integer, intent (in)     :: inum, jnum ! dimesions of grid in x and y directions
  integer :: istatus

  allocate (this%Species)
  allocate (this%StateVar)
  allocate (this%StateVar%amount(inum,jnum))
  allocate (this%StateVar%intensity(inum,jnum))
  allocate (this%Bioturbation)
  allocate (this%Bioturbation%TauEffect(inum,jnum))
  allocate (this%Bioturbation%ErodibilityEffect(inum,jnum),stat=istatus)

  !if (istatus == 0) then
  !    write (*,*) 'allocation of ErodibilityEffect was successfull'
  !else
  !    write (*,*) 'Error , allocation of ErodibilityEffect was NOT successfull'
  !end if

  this%inum      = inum
  this%jnum      = jnum

end subroutine init_Mbalthica

!------------------------------------------------------------

subroutine set_Mbalthica(this, spatialvar, Biounit)

  implicit none

  class (Mbalthica_Object)  :: this
  real (fp), dimension (:,:), pointer, optional  :: spatialvar
  character (len = 255), optional  :: Biounit

  real (fp), dimension (:,:), allocatable :: amounts
  character (len = 255)     :: units
  integer                   :: StringLength, UnitNr,istat
  logical                   :: opnd, exst
  real (fp)                 :: amount

  namelist /Macrofaun/  units, amount

  amount = 0.0_fp

  if (.not.present(spatialvar)) then

    this%Species = 'Mbalthica'
    units        = ''

    allocate ( amounts ( this%inum, this%jnum ) )
    amounts(:,:) = amount

    inquire ( file=nml_name, exist=exst, opened=opnd, Number=UnitNr )
    !write (*,*) 'exist ', exst, 'opened ', opnd, ' file unit', UnitNr

    if (exst.and.(.not.opnd)) then

      UnitNr = 11
      open (unit=UnitNr, file=nml_name, action='read ', &
          & status='old', delim='APOSTROPHE')
      !write (*,*) ' in Mbalthica the file unit ', UnitNr, ' was just opened'

      read (UnitNr, nml=Macrofaun, iostat=istat)
      if (istat /= 0 ) stop ' Error in reading '//nml_name

    elseif (opnd) then

      write (*,*)  ' In Mbalthica the file unit ', UnitNr, ' already opened'

      read (UnitNr, nml=Macrofaun, iostat = istat)
      if (istat /= 0 ) stop 'Error in reading '//nml_name

      write (*,*) ' units and amount are ', units, amount

    else

      write (*,*) ' WARNING: The input file for Mbalthica ('//nml_name//') does not exist!'
      write (*,*) ' Biological effects on erodibility and bed shear stress are set to 1.'

      this%StateVar%intensity = amounts

      allocate (this%StateVar%units)
      this%StateVar%units = trim (units)

      return
    end if

    if (trim(units) == 'm**-2') then

      !write (*,*) ' In Mbalthica_class, the dimensionless density of Mbalthica is ', amount
      amounts(:,:)= amount
      this%StateVar%intensity = amounts
      nullify (this%StateVar%amount)
      ! write (*,*) ' in mbalthica_class: intensity = ', this%StateVar%intensity

    elseif  (trim(units) == 'gCm**-2') then

  ! write (*,*) ' In Mbalthica_class, the biomass of Mbalthica is ', amount

      !allocate (this%StateVar%amount( this%inum, this%jnum))
      amounts(:,:)= amount
      this%StateVar%amount = amounts
      nullify (this%StateVar%intensity)
    else

      write (*,*) ' WARNING! No valid units has been defined in file '//nml_name//'. '
      write (*,*) ' Biological effects on erodibility and bed shear stress are set to 1.'
      this%StateVar%amount = 0.0_fp
      this%StateVar%intensity = 0.0_fp

    end if


    StringLength = len_trim (units)

    if (StringLength /= 0 ) then
    ! allocate(character(StringLength) :: this%StateVar%units)
      allocate(this%StateVar%units)
      this%StateVar%units = trim (units)
    end if

  else  ! use spatialvar

    if (trim(biounit) == 'm**-2')then

      this%StateVar%intensity = spatialvar
      nullify(this%StateVar%amount)

    elseif(trim(biounit) == 'gCm**-2') then

      this%StateVar%amount = spatialvar
      nullify(this%StateVar%intensity)

    else

      write (*,*) ' WARNING! no units has been defined for Mbalthica input data. '
      write (*,*) ' Biological effects on erodibility and bed shear stress are set to 1.'
      this%StateVar%amount = 0.0_fp
      this%StateVar%intensity = 0.0_fp

    endif

    StringLength = len_trim (biounit)

    if (StringLength /= 0 ) then
    ! allocate (character(StringLength) :: this%StateVar%units)
      if (.not.associated (this%StateVar%units)) allocate(this%StateVar%units)
      this%StateVar%units = trim (biounit)
    end if

  endif

end subroutine set_Mbalthica

!------------------------------------------------------------

subroutine run_Mbalthica(this)

  implicit none

  class (Mbalthica_Object) :: this
  integer                  :: i,j

  !do j = 1, this%jnum
  !  do i = 1, this%inum
       this%Bioturbation%TauEffect         = Crit_shear_bioeffect (this%StateVar, this%inum, this%jnum)
       this%Bioturbation%ErodibilityEffect = erodibility_bioeffect(this%StateVar, this%inum, this%jnum)
  !  end do
  !end do

#ifdef DEBUG
  write (*,*)' ------- mbalthica run ------------'
  write (*,*) 'Biotic effect of ', this%Species, ' on the tau (M_balthica%Bioturbation%TauEffect): '&
              , this%Bioturbation%TauEffect
  write (*,*)
  write (*,*) 'Biotic effect of ', this%Species, ' on the Erodibility (M_balthica%Bioturbation%ErodibilityEffect): '&
              , this%Bioturbation%ErodibilityEffect
  write (*,*)'-----------------------------'
#endif

end subroutine run_Mbalthica

!------------------------------------------------------------

subroutine fin_Mbalthica (this)

  implicit none

  class (Mbalthica_Object)  :: this
  integer                   :: UnitNr
  logical                   :: opnd, exst

  if ( this%StateVar%units == '-' )       deallocate (this%StateVar%intensity)
  if ( this%StateVar%units == 'gCm**-2' ) deallocate (this%StateVar%amount)
  deallocate (this%StateVar%units)
  deallocate (this%StateVar)
  deallocate (this%Species)
  deallocate (this%Bioturbation%TauEffect)
  deallocate (this%Bioturbation%ErodibilityEffect)

  inquire ( file = 'mbalthica.nml', exist=exst , opened =opnd, Number = UnitNr )
  if (opnd) close (UnitNr)

end subroutine fin_Mbalthica

!------------------------------------------------------------

end module Mbalthica_class
