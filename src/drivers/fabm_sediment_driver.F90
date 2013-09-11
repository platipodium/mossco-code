!> @file fabm_sediment_driver.F90
!! @brief FABM sediment driver
!!
!! The driver contains the sediment driver module
!! @author Richard Hofmeister

!> The FABM sediment driver module provides infrastructure for the
!! MOSSCO sediment component.
!! The driver provides tendencies for state variables as sum of
!! local rates (through FABM) and vertical diffusion.
#include "fabm_driver.h"

module fabm_sediment_driver

use fabm
use solver_library, only: rhs_driver

implicit none
private

integer, public, parameter :: rk=selected_real_kind(12) !< real kind

type, public :: fabm_sed_grid !< sediment grid type (part of type_sed)
   real(rk),dimension(:,:,:),allocatable :: zi,dz,zc,dzc
   integer  :: knum,inum,jnum
   real(rk) :: dzmin
end type fabm_sed_grid

type,extends(rhs_driver), public :: type_sed !< sediment driver class (extends rhs_driver)
   type(fabm_sed_grid)          :: grid
   type(type_model),pointer     :: model
   real(rk)                     :: bioturbation,diffusivity
   real(rk),dimension(:,:,:),pointer    :: fluxes,bdys
contains
   procedure :: get_rhs
end type type_sed

real(rk),dimension(:,:,:),allocatable :: porosity,temp
real(rk),dimension(:,:,:),allocatable :: zeros2dv,zeros3d,ones3d,diff
real(rk),dimension(:,:,:),allocatable,target :: temp3d
real(rk),dimension(:,:,:,:),allocatable :: transport,zeros3dv
real(rk),dimension(:,:),allocatable     :: zeros2d

#define _GRID_ sed%grid
#define _INUM_ _GRID_%inum
#define _JNUM_ _GRID_%jnum
#define _KNUM_ _GRID_%knum

public :: init_fabm_sed,init_sed_grid,finalize_fabm_sed
public :: init_fabm_sed_concentrations,fabm_sed_diagnostic_variables

contains

!> Initialise sediment grid
!!
!! Allocate memory, create a grid and fill the sed_grid_type. The number of
!! layers is set outside in beforehand by the sediment component.

subroutine init_sed_grid(grid)
implicit none
type(fabm_sed_grid),intent(inout) :: grid
real(rk)  :: grid_fac=10
integer :: i,j,k

grid%inum = 1
grid%jnum = 1
! total depth = 18cm
grid_fac = 0.18_rk/((grid%knum+1)/2.0_rk * grid%dzmin) - 1.0_rk ! the last layer is 10x thicker than the first layer

! create grid
allocate(grid%dz( grid%inum,grid%jnum,grid%knum))
allocate(grid%zc( grid%inum,grid%jnum,grid%knum))
allocate(grid%zi( grid%inum,grid%jnum,1:grid%knum+1))
allocate(grid%dzc(grid%inum,grid%jnum,grid%knum-1))

grid%zi(:,:,1) = 0_rk
do k=1,grid%knum
   do j=1,grid%jnum
      do i=1,grid%inum
         grid%dz(i,j,k) = (1.0_rk + (grid_fac-1.0_rk)*(k-1)/(grid%knum-1)) * grid%dzmin
         grid%zc(i,j,k) = grid%zi(i,j,k) + 0.5_rk*grid%dz(i,j,k)
         grid%zi(i,j,k+1) = grid%zi(i,j,k)+grid%dz(i,j,k)
      end do
   end do
end do

grid%dzc = grid%zc(:,:,2:grid%knum) - grid%zc(:,:,1:grid%knum-1)

end subroutine init_sed_grid


!> Initialise FABM sediment driver
!!
!! Assumes to have a grid, either created by e.g. init_sed_grid. Parameters are
!! read from namelist sed_nml, FABM is initialised and necessary arrays are
!! allocated. Porosity is set here.

subroutine init_fabm_sed(sed)
implicit none

class(type_sed),intent(inout) :: sed
integer :: i,j,k,n
integer :: nml_unit=128
real(rk) :: diffusivity,bioturbation,porosity_max,porosity_fac
namelist /sed_nml/ diffusivity,bioturbation,porosity_max,porosity_fac

! read parameters
diffusivity   = 0.9 ! cm2/d
bioturbation  = 0.9 ! cm2/d
porosity_max  = 0.7
porosity_fac  = 0.9 ! per m

read(33,nml=sed_nml)

sed%bioturbation = bioturbation
sed%diffusivity  = diffusivity

if (.not.(allocated(sed%grid%dz))) call init_sed_grid(sed%grid)
sed%inum = sed%grid%inum
sed%jnum = sed%grid%jnum
sed%knum = sed%grid%knum

! set porosity
allocate(porosity(_INUM_,_JNUM_,_KNUM_))
allocate(temp(_INUM_,_JNUM_,_KNUM_))
do k=1,_KNUM_
   porosity(:,:,k) = porosity_max * (1_rk - porosity_fac * sum(sed%grid%dzc(:,:,1:k)))
end do

temp = 5_rk

! build model tree
sed%model => fabm_create_model_from_file(nml_unit,'fabm_sed.nml')

! set fabm domain
call fabm_set_domain(sed%model,_INUM_,_JNUM_,_KNUM_)

! allocate state variables
sed%nvar = size(sed%model%info%state_variables)

allocate(diff(_INUM_,_JNUM_,_KNUM_))
allocate(transport(_INUM_,_JNUM_,_KNUM_,sed%nvar))
transport=0.0_rk
allocate(zeros2dv(_INUM_,_JNUM_,sed%nvar))
zeros2dv=0.0_rk
allocate(zeros3dv(_INUM_,_JNUM_,_KNUM_,sed%nvar))
zeros3dv=0.0_rk
allocate(ones3d(_INUM_,_JNUM_,_KNUM_))
ones3d=1.0_rk
allocate(zeros2d(_INUM_,_JNUM_))
zeros2d=0.0_rk
allocate(temp3d(_INUM_,_JNUM_,_KNUM_))
temp3d=-999.0_rk

diff = diffusivity

end subroutine init_fabm_sed

!> initialised sediment concentrations from namelist
subroutine init_fabm_sed_concentrations(sed)
implicit none

type(type_sed), intent(inout)      :: sed
integer                            :: n

do n=1,sed%nvar
   sed%conc(:,:,:,n) = sed%model%info%state_variables(n)%initial_value
   call fabm_link_bulk_state_data(sed%model,n,sed%conc(:,:,:,n))
end do
end subroutine init_fabm_sed_concentrations


!> fabm_sed_diagnostic_variables
!!
!! The function returns a pointer to the 3d diagnostic variables.
!! So far, only bulk diagnostic variables are supported. The function is a
!! wrapper of the related FABM function.

function fabm_sed_diagnostic_variables(sed,n) result(diag)
implicit none

type(type_sed),intent(in)          :: sed
integer,intent(in)                 :: n
real(rk),dimension(:,:,:),pointer  :: diag

diag => fabm_get_bulk_diagnostic_data(sed%model,n)
end function


!> get right-hand sides
!!
!! The right-hand sides for integration are provided for the state variables.
!! The local tendencies are provided through FABM, the local changes due to
!! diffusion are calculated in diff3d. Boundary conditions handled through the diffusion
!! routine, where particulate properties use a flux boundary condition and
!! dissolved properties use a concentration boundary condition. Diffusivities
!! are calculated here depending on temperature (first index in bdys vector)

subroutine get_rhs(rhsd,rhs)
use fabm
use fabm_types
implicit none

class(type_sed)      ,intent(inout)          :: rhsd
!real(rk),intent(out)                         :: rhs(1:rhsd%inum,1:rhsd%jnum,1:rhsd%knum,1:rhsd%nvar)
real(rk),intent(inout),dimension(:,:,:,:),pointer :: rhs

real(rk),dimension(1:rhsd%inum,1:rhsd%jnum,1:rhsd%knum)   :: conc_insitu
real(rk),dimension(1:rhsd%inum,1:rhsd%jnum,1:rhsd%knum+1) :: intFLux

integer :: n,i,j,k,bcup=1,bcdown=3

do k=1,rhsd%knum
   temp3d(:,:,k) = rhsd%bdys(:,:,1)
end do

!   link state variables
do n=1,size(rhsd%model%info%state_variables)
   call fabm_link_bulk_state_data(rhsd%model,n,rhsd%conc(:,:,:,n))
end do

!   link environment forcing
call fabm_link_bulk_data(rhsd%model,varname_temp,temp3d)
! not necessary: call fabm_link_bulk_data(rhsd%model,varname_porosity,porosity)

! calculate diffusivities (temperature)
do n=1,size(rhsd%model%info%state_variables)
   if (rhsd%model%info%state_variables(n)%properties%get_logical('particulate',default=.false.)) then
      bcup = 1
      diff = (rhsd%bioturbation + temp3d * 0.035) * porosity / 86400.0_rk / 10000_rk
      conc_insitu = rhsd%conc(:,:,:,n)/(porosity-ones3d)
      call diff3d(rhsd%grid,conc_insitu,rhsd%bdys(:,:,n+1), zeros2d, rhsd%fluxes(:,:,n), zeros2d, &
              bcup, bcdown, diff, porosity-ones3d, porosity-ones3d, intFlux, transport(:,:,:,n))
   else
      bcup = 2
      diff = (rhsd%diffusivity + temp3d * 0.035) * porosity / 86400.0_rk / 10000_rk
      conc_insitu = rhsd%conc(:,:,:,n)/porosity
      call diff3d(rhsd%grid,conc_insitu,rhsd%bdys(:,:,n+1), zeros2d, rhsd%fluxes(:,:,n), zeros2d, &
              bcup, bcdown, diff, porosity, porosity, intFlux, transport(:,:,:,n))
      ! set fluxes for output
      rhsd%fluxes(:,:,n) = intFlux(:,:,1)
   end if
end do

rhs=0.0_rk
do k=1,rhsd%knum
   do j=1,rhsd%jnum
      do i=1,rhsd%inum
         call fabm_do(rhsd%model,i,j,k,rhs(i,j,k,:))
      end do
   end do
end do

! return fabm-rhs + diff-tendencies
rhs = rhs + transport

end subroutine get_rhs


!> finalize the FABM sediment driver
!!
!! deallocate all the arrays
subroutine finalize_fabm_sed()

  if (allocated(zeros2dv)) deallocate(zeros2dv)
  if (allocated(zeros3dv)) deallocate(zeros3dv)
  if (allocated(zeros2d))  deallocate(zeros2d)
  if (allocated(ones3d))    deallocate(ones3d)
  if (allocated(transport)) deallocate(transport)
  if (allocated(diff))      deallocate(diff)

end subroutine finalize_fabm_sed


!> vertical diffusion in a 3d sediment grid
!!
!! Vertical diffusion in a porous sediment grid.

subroutine diff3d (grid, C, Cup, Cdown, fluxup, fluxdown,        &
                       BcUp, BcDown, D, VF, A, Flux, dC)

implicit none
type(fabm_sed_grid), intent(in)        :: grid
real(rk), dimension(grid%inum,grid%jnum,grid%knum), intent(in) :: C, D

! Boundary concentrations (used if Bc..=2,4), fluxes (used if Bc= 1) 
! and convection coeff (used if Bc=4)
real(rk), dimension(grid%inum,grid%jnum), intent(in)   :: Cup, Cdown, fluxup, fluxdown

! volume fraction, surface Area
real(rk), dimension(grid%inum,grid%jnum,grid%knum), intent(in) :: VF, A

! boundary concitions (1= flux, 2=conc, 3 = 0-grad, 4=convect)
integer, intent(in) :: BcUp, BcDown   

! output: fluxes and rate of change
real(rk), dimension(grid%inum,grid%jnum,grid%knum+1), intent(out) :: Flux
real(rk), dimension(grid%inum,grid%jnum,grid%knum),intent(out) :: dC

! locals 
integer  :: i,j,k
real(rk),dimension(grid%inum,grid%jnum)   :: AVF,restflux
real(rk),dimension(grid%inum,grid%jnum,grid%knum) :: flux_cap

! -------------------------------------------------------------------------------

!flux_cap=0.0_rk
!do j=1,grid%jnum
!do i=1,grid%inum
!flux_cap(i,j,1:4)=(/ 0.5,0.3,0.13,0.07 /)
!end do
!end do

! Flux - first internal cells
! positive flux is directed downward
do j=1,grid%jnum
   do i=1,grid%inum
      do k = 2,grid%knum
         Flux(i,j,k) = -VF(i,j,k)*D(i,j,k) * (C(i,j,k)-C(i,j,k-1)) /grid%dzc(i,j,k-1)
      end do

! Then the outer cells 
! upstream boundary
      IF (BcUp .EQ. 1) THEN
        Flux(i,j,1) = fluxup(i,j)

      ELSE IF (BcUp .EQ. 2) THEN
        Flux(i,j,1) = -VF(i,j,1)*D(i,j,1) * (C(i,j,1)-Cup(i,j)) /grid%dz(i,j,1)

      ELSE IF (BcUp .EQ. 3) THEN
        Flux(i,j,1) = 0.0_rk

      ELSE IF (BcUp .EQ. 4) THEN
        Flux(i,j,1) = fluxup(i,j) 
        k = 2
        restflux(i,j) = Flux(i,j,1) - flux_cap(i,j,1)*Flux(i,j,1)
        do while ((restflux(i,j) .gt. 0) .and. (k .le. grid%knum))
           Flux(i,j,k) = Flux(i,j,k) + restflux(i,j)
           restflux(i,j) = restflux(i,j) - flux_cap(i,j,k)*Flux(i,j,1)
           k = k + 1
        end do
        if (k .gt. grid%knum) then 
           Flux(i,j,grid%knum) = Flux(i,j,grid%knum) + restflux(i,j)
        endif 
      ENDIF
       
! downstream boundary
      IF (BcDown .EQ. 1) THEN
        Flux(i,j,grid%knum+1) = fluxdown(i,j)

      ELSE IF (BcDown .EQ. 2) THEN
        Flux(i,j,grid%knum+1) = -VF(i,j,grid%knum)*D(i,j,grid%knum) * (Cdown(i,j)-C(i,j,grid%knum)) / grid%dz(i,j,grid%knum)

      ELSE IF (BcDown .EQ. 3) THEN
        Flux(i,j,grid%knum+1) =0.0_rk

      ELSE
      ENDIF

      DO k = 1,grid%knum
        dC(i,j,k) = (Flux(i,j,k) - Flux(i,j,k+1))/grid%dz(i,j,k)
      ENDDO
   end do
end do

end subroutine diff3D


end module fabm_sediment_driver
