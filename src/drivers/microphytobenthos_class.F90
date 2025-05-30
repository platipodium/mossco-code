!> @brief Implementation of a driver for microphytobenthos !
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Helmholtz-Zentrum Geesthacht, Bundesanstalt für Wasserbau
!> @author M. Hassan Nasermoaddeli
!> @author Carsten Lemmen
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
! The microphytobenthos class is a subclass of superclass Benthos_Effect.
! It comprises the effect of microphytobenthos on erodibility and
! crticial bed shear stress.
module Microphytobenthos_class

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

end type Microphytobenthos

character(len=*), parameter :: nml_name = 'microphyt.nml'

!private :: init_microphyt, set_microphyt, run_microphyt,fin_microphyt

!------------------------------------------------------------
  contains
!------------------------------------------------------------

subroutine init_microphyt (this,inum, jnum)

  implicit none

  class (Microphytobenthos) :: this
  integer, intent (in)      :: inum, jnum ! dimesions of grid in x and y directions
  integer                   :: istatus

  allocate (this%Species)
  allocate (this%BioMass)
  allocate (this%BioMass%amount(inum,jnum))
  allocate (this%TauEffect(inum,jnum))
  allocate (this%ErodibilityEffect (inum,jnum),stat=istatus)

  !if (istatus == 0) then
  !    write (*,*) 'allocation of ErodibilityEffect was successfull'
  !else
  !    write (*,*) 'Error , allocation of ErodibilityEffect was NOT successfull'
  !end if

  this%inum              = inum
  this%jnum              = jnum
  this%TauEffect         = 1.0_fp
  this%ErodibilityEffect = 1.0_fp
  this%BioMass%amount    = 0.0_fp

end subroutine init_microphyt

!------------------------------------------------------------

subroutine set_microphyt (this, spatialvar, biounit)

  implicit none

  class (Microphytobenthos)  :: this
  real (fp), dimension (:,:), pointer, optional  :: spatialvar
  character (len = *), optional  :: Biounit

  real (fp), dimension (:,:), allocatable  :: Biomass
  character (len = 255)      :: units
  integer                    :: StringLength, UnitNr, istat
  logical                    :: opnd, exst
  real (fp)                  :: mass

  namelist /Microphyto/ units, mass

  mass = 0.0_fp

  if (.not.present(spatialvar)) then

    this%Species = 'Microphytobenthos'
    Units        = 'mug g-1'

    allocate ( Biomass ( this%inum , this%jnum ) )
    Biomass(:,:) = mass

    inquire ( file=nml_name, exist=exst, opened=opnd, Number=UnitNr )
    !write (*,*) 'exist ', exst, 'opened ', opnd, ' file unit', UnitNr

    if (exst.and.(.not.opnd)) then

      UnitNr = 11
      open (unit=UnitNr, file=nml_name, action='read ', &
          & status='old', delim='APOSTROPHE')
    ! write (*,*) ' in Microphytobenthos the file unit ', UnitNr, ' was just opened'

      read (UnitNr, nml=Microphyto, iostat=istat)
      if (istat /= 0 ) stop ' Error in reading '//nml_name

    elseif (opnd) then

      write (*,*) ' In Microphytobenthos the file unit ', UnitNr, ' already opened'

      read (UnitNr, nml=Microphyto, iostat=istat)
      if (istat /= 0 ) stop ' Error in reading '//nml_name

    else

      write (*,*) ' WARNING: The input file for Microphytobenthos ('//nml_name//') does not exist!'
      write (*,*) ' Biological effects on erodibility and bed shear stress are set to 1.'

      this%BioMass%amount=Biomass

      allocate (this%BioMass%units)
      this%BioMass%units = trim (units)

      return

    end if

    this%UnitNr = UnitNr
    Biomass(:,:) = Mass
    write (*,*) ' In Microphytobenthos_class, the amount of Chl biomass is ', Mass
    write (*,*) ' Units are ', units

    this%BioMass%amount = Biomass

    StringLength = len_trim (units)

    if (StringLength /= 0 ) then
      !allocate (character(StringLength) :: this%BioMass%units)
      allocate (this%BioMass%units)
      this%BioMass%units = trim (units)
    end if

    close (UnitNr)

  else  ! use spatialvar

    this%BioMass%amount = spatialvar
    StringLength = len_trim (biounit)

    if (StringLength /= 0 ) then
      !allocate (character(StringLength) :: this%BioMass%units)
      if (.not.associated (this%Biomass%units)) allocate (this%BioMass%units)
      this%BioMass%units = trim (biounit)
    end if

  endif

end subroutine set_microphyt

!------------------------------------------------------------

subroutine run_microphyt (this)

  implicit none
  !#define DEBUG
  class (Microphytobenthos) :: this

  !integer                   :: i,j

  !do i = 1, this%inum
  !  do j = 1, this%jnum
       this%TauEffect         = Crit_shear_bioeffect (this%BioMass, this%inum, this%jnum)
       this%ErodibilityEffect = erodibility_bioeffect(this%BioMass, this%inum, this%jnum)
  !  do
  !end do

#ifdef DEBUG
  write (*,*) ' Biotic effect of ', this%Species, ' on tau ( Micro%TauEffect) ', this%TauEffect
  write (*,*)
  write (*,*) ' Biotic effect of ' , this%Species, ' on the Erodibility (Micro%ErodibilityEffect): ', this%ErodibilityEffect
  write (*,*)
#endif

end subroutine run_microphyt

!------------------------------------------------------------

subroutine fin_microphyt (this)

  implicit none

  class (Microphytobenthos) :: this
  integer                   :: UnitNr
  logical                   :: opnd, exst

  deallocate (this%BioMass%amount)
  deallocate (this%BioMass%units)
  deallocate (this%BioMass)
  deallocate (this%Species)
  deallocate (this%TauEffect)
  deallocate (this%ErodibilityEffect)

  inquire ( file = nml_name, exist=exst , opened =opnd, Number = UnitNr )
  if (opnd) close (UnitNr)

end subroutine fin_microphyt

!------------------------------------------------------------

end module Microphytobenthos_class
