!> @file fabm_sediment_driver.F90
!! @brief FABM sediment driver
!!
!! The driver contains the sediment driver module
!! @author Richard Hofmeister

!> The FABM sediment driver module provides infrastructure for the
!! MOSSCO sediment component.
!! The driver provides tendencies for state variables as sum of
!! local rates (through FABM) and vertical diffusion.
!! The units of concentrations of state variables is handled inside
!! the driver as molar mass per volume pore water.
#include "fabm_driver.h"

module fabm_sediment_driver

use fabm
use fabm_types, only: rk
use solver_library, only: type_rhs_driver
use mossco_variable_types
use mossco_strings

implicit none
private

! Note (KK): we take rk from fabm_types
!integer, public, parameter :: rk=selected_real_kind(12) !< real kind
public :: rk

!> grid_type-s to be
integer, public, parameter :: LOCAL_GRID=0
integer, public, parameter :: UGRID=1
integer, public, parameter :: FOREIGN_GRID=2


type, public :: fabm_sed_grid !< sediment grid type (part of type_sed)
   ! dz - layer heights (difference between interface depths zi)
   ! dzc - distance between layer center positions zc
   real(rk),dimension(:,:,:),pointer :: zi=>null()
   real(rk),dimension(:,:,:),pointer :: zc=>null()
   real(rk),dimension(:,:,:),pointer :: dz=>null()
   real(rk),dimension(:,:,:),pointer :: dzc=>null()
   integer  :: knum,inum=-1,jnum=-1
   real(rk) :: dzmin
   logical  :: use_ugrid=.false.
   logical, dimension(:,:,:), pointer :: mask => null()
   integer  :: type=LOCAL_GRID
contains
   procedure :: init_grid
end type fabm_sed_grid

type :: fabm_sed_bulk_var
   real(rk),dimension(:,:,:),pointer :: data=>null()
   character(len=256)                :: name
   real(rk)                          :: factor=1.0d0
end type

type,extends(MOSSCO_VariableFArray3d),public :: export_state_type !< sediment driver type for export states
   integer            :: fabm_id=-1
   logical            :: particulate=.false.
end type

type,extends(type_rhs_driver), public :: type_sed !< sediment driver class (extends type_rhs_driver)
   type(fabm_sed_grid)          :: grid
   type(type_model),pointer     :: model
   real(rk)                     :: bioturbation,diffusivity
   integer                      :: bioturbation_profile
   real(rk)                     :: beta,k_l,b,L1,L2
   type(fabm_sed_bulk_var),pointer :: poc_classes(:)=>null()
   real(rk)                     :: k_par
   real(rk),dimension(:,:,:),pointer :: fluxes => null()
   real(rk),dimension(:,:,:),pointer :: bdys => null()
   integer                      :: bcup_dissolved_variables=2
   integer                      :: bcup_particulate_variables=1
   integer                      :: ndiag=0
   logical                      :: do_output=.true.
   real(rk)                     :: porosity_fac=1.0_rk
   real(rk)                     :: pom_flux_max=1.0_rk
   real(rk)                     :: missing_value=1.e20_rk
   type(export_state_type),dimension(:),allocatable :: export_states

   real(rk),dimension(:,:,:),pointer     :: porosity => null()
   real(rk),dimension(:,:,:),pointer     :: temp => null()
   real(rk),dimension(:,:,:),pointer     :: intf_porosity => null()
   real(rk),dimension(:,:,:),pointer     :: bioturbation_factor => null()
   real(rk),dimension(:,:,:),pointer     :: par => null()
   real(rk),dimension(:,:),pointer       :: par_surface => null()
   real(rk),dimension(:,:,:),allocatable :: zeros2dv,zeros3d,ones3d,diff
   real(rk),dimension(:,:,:),pointer     :: temp3d => null()
   real(rk),dimension(:,:,:,:),allocatable :: transport,zeros3dv
   real(rk),dimension(:,:),allocatable     :: zeros2d
   real(rk),dimension(:,:,:),pointer     :: flux_cap => null()
   real(rk),dimension(:,:,:),pointer     :: biomass=>null()
   real(rk),dimension(:,:,:),pointer     :: weighted_toc=>null()

contains
   procedure :: initialize
   procedure :: finalize
   procedure :: init_concentrations
   procedure :: diagnostic_variables
   procedure :: update_porosity
   procedure :: get_rhs
   procedure :: get_export_state_by_id
   procedure :: get_export_state_by_diag_id
   procedure :: get_all_export_states
   procedure :: check_domain => fabm_sed_check_domain
end type type_sed

#define _GRID_ sed%grid
#define _INUM_ _GRID_%inum
#define _JNUM_ _GRID_%jnum
#define _KNUM_ _GRID_%knum

contains

!> Initialise sediment grid
!!
!! Allocate memory, create a grid and fill the sed_grid_type. The number of
!! layers is set outside in beforehand by the sediment component.

subroutine init_grid(self, rc)

  implicit none

  class(fabm_sed_grid) :: self
  integer, optional    :: rc

  real(rk)             :: grid_fac=10
  integer              :: i,j,k, rc_
  real(rk)             :: self_fac

  !! grid%inum and grid%jnum are set outside
  if ((self%inum == -1).or.(self%jnum == -1)) then
    write(0,*) 'FATAL: fabm_sediment_driver: grid size < 0'
    stop
  endif

  !> @todo total depth should be a configurable parameter
  ! total depth = 18cm
  ! the last layer is 10x thicker than the first layer
  self_fac = 0.18_rk/((self%knum+1)/2.0_rk * self%dzmin) - 1.0_rk

  ! create grid with
  ! zi = depth of interfaces of layers (vector extended by 1)
  ! zc = depth of center of layer i
  ! dz = layer height of layer i
  ! dzc = difference between layer centers (vector reduced by 1)
  allocate(self%dz( self%inum,self%jnum,self%knum))
  allocate(self%zc( self%inum,self%jnum,self%knum))
  allocate(self%zi( self%inum,self%jnum,1:self%knum+1))
  allocate(self%dzc(self%inum,self%jnum,self%knum-1))

  self%zi(:,:,1) = 0_rk ! by definition
  do k=1,self%knum; do j=1,self%jnum; do i=1,self%inum
         self%dz(i,j,k) = (1.0_rk + (self_fac-1.0_rk)*(k-1)/(self%knum-1)) * self%dzmin
         self%zc(i,j,k) = self%zi(i,j,k) + 0.5_rk*self%dz(i,j,k)
         self%zi(i,j,k+1) = self%zi(i,j,k)+self%dz(i,j,k)
        end do
     end do
  end do

  self%dzc = self%zc(:,:,2:self%knum) - self%zc(:,:,1:self%knum-1)

#ifdef DEBUG
  write(0,*) 'zc=',self%zc(min(self%inum,50),min(self%jnum,10),:)
  write(0,*) 'zi=',self%zi(min(self%inum,50),min(self%jnum,10),:)
  write(0,*) 'dz=',self%dz(min(self%inum,50),min(self%jnum,10),:)
  write(0,*) 'dzc=',self%dzc(min(self%inum,50),min(self%jnum,10),:)
#endif

end subroutine init_grid


!> Initialise FABM sediment driver
!!
!! Assumes to have a grid, either created by e.g. init_sed_grid. Parameters are
!! read from namelist sed_nml, FABM is initialised and necessary arrays are
!! allocated. Porosity is set here.
!! The bioturbation profile can have different shapes:
!! bioturbation_profile:
!!   0 - constant
!!   1 - linear decrease over bioturbation_depth towards bioturbation_min
!!   2 - exponential descrease bioturbation*exp(-depth/bioturbation_depth)

subroutine initialize(sed)
implicit none

class(type_sed),intent(inout) :: sed
integer :: i,j,k,n
integer :: bioturbation_profile
logical :: distributed_pom_flux=.false.
integer :: nml_unit=128
real(rk) :: diffusivity,bioturbation,porosity_max,porosity_fac
real(rk) :: bioturb_k_l,bioturb_L1,bioturb_L2,bioturb_beta,bioturb_b,bioturb_dry_density
real(rk) :: k_par,bioturbation_depth,bioturbation_min
real(rk) :: pom_flux_max
namelist /sed_nml/ diffusivity,bioturbation_profile,bioturbation, &
        porosity_max,porosity_fac,k_par, distributed_pom_flux, pom_flux_max, &
        bioturbation_depth,bioturbation_min,bioturb_k_l,bioturb_L1, &
        bioturb_L2,bioturb_beta,bioturb_b,bioturb_dry_density

! read parameters
bioturbation_profile = 1
diffusivity   = 0.9 ! cm2/d
bioturbation  = 0.9 ! cm2/d
bioturbation_depth = 5.0 ! cm
bioturbation_min = 0.2 ! cm2/d
porosity_max  = 0.7
porosity_fac  = 0.9 ! per m
k_par         = 2.0d-3 ! light absorption length (m) (2.0 mm)
pom_flux_max  = 2.0d4  ! so far mmol/m2/d
bioturb_k_l=0.11
bioturb_L1=0.2
bioturb_L2=0.6
bioturb_beta=0.22
bioturb_b=1.334
bioturb_dry_density=1000. !kg/m3

read(33,nml=sed_nml)

sed%bioturbation = bioturbation
sed%bioturbation_profile = bioturbation_profile
sed%diffusivity  = diffusivity
sed%k_par        = k_par
if (distributed_pom_flux) then
  sed%bcup_particulate_variables=4
else
  sed%bcup_particulate_variables=1
end if

if (.not.(associated(sed%grid%dz))) call sed%grid%init_grid()
sed%inum = sed%grid%inum
sed%jnum = sed%grid%jnum
sed%knum = sed%grid%knum

if (.not.associated(sed%mask)) then
  allocate(sed%mask(sed%grid%inum,sed%grid%jnum,sed%grid%knum))
  sed%mask = .false.
end if
! add mask to grid
sed%grid%mask => sed%mask

! set porosity
allocate(sed%porosity(_INUM_,_JNUM_,_KNUM_))
allocate(sed%intf_porosity(_INUM_,_JNUM_,_KNUM_))
allocate(sed%bioturbation_factor(_INUM_,_JNUM_,_KNUM_))
allocate(sed%biomass(_INUM_,_JNUM_,_KNUM_))
sed%biomass=0.0d0
allocate(sed%weighted_toc(_INUM_,_JNUM_,_KNUM_))
sed%weighted_toc=0.0d0
allocate(sed%temp(_INUM_,_JNUM_,_KNUM_))
allocate(sed%par (_INUM_,_JNUM_,_KNUM_))
sed%par = 0.0d0
allocate(sed%par_surface(_INUM_,_JNUM_))
sed%par_surface = 0.0d0
allocate(sed%flux_cap(_INUM_,_JNUM_,_KNUM_))
sed%bioturbation_factor=1.0d0
sed%porosity_fac = porosity_fac
sed%pom_flux_max = pom_flux_max
do k=1,_KNUM_
   !> set porosity, located at cell centers
   sed%porosity(:,:,k) = porosity_max * (1_rk - porosity_fac * sed%grid%zc(:,:,k))

   ! todo: pom_flux_max units have to be unified - need to come in mg m-2 d-1 and then scaled in
   ! transport routine with the molar mass
   sed%flux_cap(:,:,k) = pom_flux_max/86400.0d0 * (1.0d0 - sed%porosity(:,:,k)) * sed%grid%dz(:,:,k)
   if (k .gt. 2) then
     do j=1,_JNUM_
       do i=1,_INUM_
         if (sed%flux_cap(i,j,k) .gt. sed%flux_cap(i,j,k-1)) sed%flux_cap(i,j,k) = sed%flux_cap(i,j,k-1)
       end do
     end do
   end if

   !> set bioturbation_factor, located at layer interfaces
   select case (bioturbation_profile)
   case (1) ! linear decrease
     sed%bioturbation_factor(:,:,k) = &
       max(bioturbation_min/bioturbation, &
       max(bioturbation_depth-100.0d0*sed%grid%zi(:,:,k),0.0d0)/bioturbation_depth)
   case (2) ! exponential decrease
     sed%bioturbation_factor(:,:,k) = &
       exp(-100.0d0*sed%grid%zi(:,:,k)/bioturbation_depth)
   case default
   end select
end do

#if 0
!! reduce porosity by 2% for each mesh element for testing purposes
do i=1,_INUM_
  sed%porosity(i,:,:) = sed%porosity(1,:,:)* (0.98**i)
end do
#endif

call sed%update_porosity(from_surface=.false.)

sed%temp = 5_rk

! build model tree
sed%model => fabm_create_model_from_file(nml_unit,'fabm_sed.nml')

! set fabm domain
call fabm_set_domain(sed%model,_INUM_,_JNUM_,_KNUM_)
call sed%model%set_surface_index(1)
call sed%model%set_bottom_index(_KNUM_)

! allocate state variables
sed%nvar = size(sed%model%state_variables)
sed%ndiag = size(sed%model%diagnostic_variables)

allocate(sed%diff(_INUM_,_JNUM_,_KNUM_))
allocate(sed%transport(_INUM_,_JNUM_,_KNUM_,sed%nvar))
sed%transport=0.0_rk
allocate(sed%zeros2dv(_INUM_,_JNUM_,sed%nvar))
sed%zeros2dv=0.0_rk
allocate(sed%zeros3dv(_INUM_,_JNUM_,_KNUM_,sed%nvar))
sed%zeros3dv=0.0_rk
allocate(sed%ones3d(_INUM_,_JNUM_,_KNUM_))
sed%ones3d=1.0_rk
allocate(sed%zeros2d(_INUM_,_JNUM_))
sed%zeros2d=0.0_rk
allocate(sed%temp3d(_INUM_,_JNUM_,_KNUM_))
sed%temp3d=-999.0_rk

sed%diff = diffusivity

! initialise bioturbation for bioturbation_profile=3 (Zhang&Wirtz)
sed%k_l  = bioturb_k_l
sed%L1   = bioturb_L1
sed%L2   = bioturb_L2
sed%beta = bioturb_beta
sed%b    = bioturb_b
! hard-coded two-class POC model, later replace by name-based
! link to state variables
allocate(sed%poc_classes(2))
sed%poc_classes(1)%data => null()
sed%poc_classes(1)%name = 'detritus_labile_carbon'
sed%poc_classes(1)%factor = 1.0d0/1.2d0*12.01d0/bioturb_dry_density/1000.0d0 ! mmolC/m3 -> mgC/g_dry_weight
sed%poc_classes(2)%data => null()
sed%poc_classes(2)%name = 'detritus_semilabile_carbon'
sed%poc_classes(2)%factor = 1.0d0/6.0d0*12.01d0/bioturb_dry_density/1000.0d0 ! mmolC/m3 -> mgC/g_dry_weight

end subroutine initialize


!> update porosity from surface values
!!   to be used during initialisation
subroutine update_porosity(sed, from_surface, reinitialize_concentrations)
implicit none

class(type_sed)   :: sed
logical, optional :: from_surface
logical           :: from_surface_eff
logical, optional :: reinitialize_concentrations
logical           :: reinitialize_concentrations_eff
integer           :: n,i,j,k

from_surface_eff = .false.
if (present(from_surface)) from_surface_eff=from_surface
reinitialize_concentrations_eff=.false.
if (present(reinitialize_concentrations)) reinitialize_concentrations_eff=reinitialize_concentrations

if (from_surface_eff) then
  do k=2,_KNUM_
    !> set porosity, located at cell centers
    sed%porosity(:,:,k) = sed%porosity(:,:,1) * &
      (1_rk - sed%porosity_fac * (sed%grid%zc(:,:,k)-sed%grid%zc(:,:,1)))
  end do

  do k=1,_KNUM_
    !> @todo: pom_flux_max units have to be unified -
    !!        need to come in mg m-2 d-1 and then scaled in
    !!        transport routine with the molar mass
    sed%flux_cap(:,:,k) = sed%pom_flux_max/86400.0d0 * (1.0d0 - sed%porosity(:,:,k)) * sed%grid%dz(:,:,k)
    if (k .gt. 2) then
      do j=1,_JNUM_
        do i=1,_INUM_
          if (sed%flux_cap(i,j,k) .gt. sed%flux_cap(i,j,k-1)) sed%flux_cap(i,j,k) = sed%flux_cap(i,j,k-1)
        end do
      end do
    end if
  end do
end if

! set porosity to 1.0 in the mask to avoid overflows in get_rhs
where(sed%mask) sed%porosity = 1.0d0

! update interface porosity
sed%intf_porosity(:,:,1) = sed%porosity(:,:,1)
sed%intf_porosity(:,:,2:_KNUM_) = 0.5d0*(sed%porosity(:,:,1:_KNUM_-1) + sed%porosity(:,:,2:_KNUM_))

! update effective concentrations (scaled per volume pore water)
if (reinitialize_concentrations_eff) then
  if (associated(sed%conc)) call sed%init_concentrations()
end if

end subroutine update_porosity



!> initialised sediment concentrations from namelist. Initial
!! concentrations in the namelist are taken as molar mass per
!! total cell volume
subroutine init_concentrations(sed)
implicit none

class(type_sed) :: sed
integer         :: n,i,j,k

do n=1,sed%nvar
   sed%conc(:,:,:,n) = sed%model%state_variables(n)%initial_value/sed%porosity(:,:,:)
   call fabm_link_bulk_state_data(sed%model,n,sed%conc(:,:,:,n))
end do
if(associated(sed%mask)) then
  do k=1,sed%knum
    do j=1,sed%jnum
      do i=1,sed%inum
        !> @todo: check adaptive solver for -1.d20 or other negative missing values
        if (sed%mask(i,j,k)) sed%conc(i,j,k,:)=sed%missing_value
      end do
    end do
  end do
end if

! initialise biotubation_profile=3
do n=1,sed%nvar
  !write(0,*) 'check for POC class ',trim(only_var_name(sed%model%state_variables(n)%long_name))
  do i=1,size(sed%poc_classes)
    if (trim(only_var_name(sed%model%state_variables(n)%long_name))==trim(sed%poc_classes(i)%name)) then
      !write(0,*) 'found matching POC class ',trim(sed%poc_classes(i)%name),' in state_variable',n
      sed%poc_classes(i)%data=>sed%conc(:,:,:,n)
    end if
  end do
end do

end subroutine init_concentrations


!> fabm_sed_check_grid
!!
!! Check, if dzc and dz are > 0.0 and 0<porosity<1.

subroutine fabm_sed_check_domain(sed, rc)

  implicit none

  class(type_sed) :: sed
  integer, optional :: rc
  integer         :: i,j,k,rc_

  rc_=0

#ifdef DEBUG
  write(0,*) 'Checking domain of size ',sed%inum,sed%jnum,sed%knum
  write(0,*) 'shape(cond)=',shape(sed%conc),' shape(mask)=', shape(sed%mask)
#endif

  if (any(.not.sed%mask.and.sed%porosity<=0)) then
    write(0,*) 'FATAL sediment porosity <=0 '
    stop
  endif

  if (any(.not.sed%mask.and.sed%porosity > 1)) then
    write(0,*) 'FATAL sediment porosity > 1 '
    stop
  endif

  if (any(.not.sed%mask(:,:,1:sed%knum-1).and..not.sed%mask(:,:,2:sed%knum) &
    .and.sed%grid%dzc <= 0)) then
    write(0,*) 'FATAL sediment central layer difference <= 0 '
    do k=1,sed%knum-1; do j=1,sed%jnum; do i=1,sed%inum
      if (.not.sed%mask(i,j,k).and..not.sed%mask(i,j,k+1).and.sed%grid%dzc(i,j,k) <= 0) &
        write(0,*) 'dz(i,j,k) ',i,j,k, sed%grid%dzc(i,j,k)
    enddo; enddo; enddo
    stop
  endif

  if (any(.not.sed%mask.and.sed%grid%dz < sed%grid%dzmin)) then
    write(0,*) 'FATAL sediment layer height < minimum value ', sed%grid%dzmin
    do k=1,sed%knum; do j=1,sed%jnum; do i=1,sed%inum
      if (.not.sed%mask(i,j,k).and.sed%grid%dz(i,j,k) < sed%grid%dzmin) &
        write(0,*) 'dz(i,j,k) ',i,j,k, sed%grid%dz(i,j,k)
    enddo; enddo; enddo
    stop
  endif

  if (associated(sed%conc)) then
    do k=1,sed%knum; do j=1,sed%jnum; do i=1,sed%inum
      if (sed%mask(i,j,k)) then
        sed%conc(i,j,k,:)=1.d20
      endif
    enddo; enddo; enddo
  end if

  ! set porosity to 1.0 in the mask to avoid overflows in get_rhs
  where(sed%mask) sed%porosity = 1.0d0

  if (present(rc)) rc=rc_

end subroutine fabm_sed_check_domain


!> fabm_sed_diagnostic_variables
!!
!! The function returns a pointer to the 3d diagnostic variables.
!! So far, only bulk diagnostic variables are supported. The function is a
!! wrapper of the related FABM function.
!! Diagnostic concentrations are given in FABM per volume pore water.

function diagnostic_variables(sed,n) result(diag)
implicit none

class(type_sed)                    :: sed
integer,intent(in)                 :: n
real(rk),dimension(:,:,:),pointer  :: diag

diag => fabm_get_bulk_diagnostic_data(sed%model,n)
end function diagnostic_variables


!> get right-hand sides
!!
!! The right-hand sides for integration are provided for the state variables.
!! The local tendencies are provided through FABM, the local changes due to
!! diffusion are calculated in diff3d. Boundary conditions handled through the diffusion
!! routine, where particulate properties use a flux boundary condition and
!! dissolved properties use a concentration boundary condition. Diffusivities
!! are calculated here depending on temperature (first index in bdys vector)

subroutine get_rhs(rhs_driver,rhs)
use fabm
use fabm_types
implicit none

class(type_sed)      ,intent(inout)          :: rhs_driver
real(rk),intent(inout),dimension(:,:,:,:),pointer :: rhs

real(rk),dimension(1:rhs_driver%inum,1:rhs_driver%jnum,1:rhs_driver%knum)   :: conc_insitu,f_T
real(rk),dimension(1:rhs_driver%inum,1:rhs_driver%jnum,1:rhs_driver%knum)   :: weighted_toc
real(rk),dimension(1:rhs_driver%inum,1:rhs_driver%jnum,1:rhs_driver%knum+1) :: intFLux
real(rk),dimension(1:rhs_driver%inum,1:rhs_driver%jnum)                     :: cumdepth, averaged_weighted_toc

integer :: n,i,j,k,bcup=1,bcdown=3

! calculate environmental properties in sediment
do k = 1, rhs_driver%knum
   if (k == 1) then
      cumdepth = 0.0
   else
      cumdepth = sum(rhs_driver%grid%dz(:,:,1:k-1),dim=3)  ! light is calculated at upper layer interfaces
   endif
   where (.not.rhs_driver%mask(:,:,k))
      rhs_driver%temp3d(:,:,k) = rhs_driver%bdys(:,:,1)
      rhs_driver%par(:,:,k) = rhs_driver%par_surface * exp(-cumdepth/rhs_driver%k_par)
   end where
end do

!   link state variables
do n=1,size(rhs_driver%model%state_variables)
   call fabm_link_bulk_state_data(rhs_driver%model,n,rhs_driver%conc(:,:,:,n))
end do

!   link environment forcing
call fabm_link_bulk_data(rhs_driver%model,standard_variables%temperature,rhs_driver%temp3d)
call fabm_link_bulk_data(rhs_driver%model,standard_variables%downwelling_photosynthetic_radiative_flux,rhs_driver%par)
!call fabm_link_bulk_data(rhs_driver%model,standard_variables%porosity,rhs_driver%porosity)

! calculate diffusivities (temperature)
if (rhs_driver%bioturbation_profile .eq. 3) then
  ! calculate bioturbation depending on infauna biomass, which is
  ! estimated from steady-state dependency on the TOC profile
  ! following Zhang & Wirtz (2017).
  f_T = _ONE_
  rhs_driver%bioturbation=_ONE_
  ! calculate weighted TOC profile
  weighted_toc = 0.0d0 ! [mg-C/g-dry_mass]
  do n=1,size(rhs_driver%poc_classes)
    weighted_toc = weighted_toc + rhs_driver%poc_classes(n)%factor*rhs_driver%porosity/(rhs_driver%ones3d-rhs_driver%porosity)*rhs_driver%poc_classes(n)%data
  end do
  do i=1,rhs_driver%inum
    do j=1,rhs_driver%jnum
      averaged_weighted_toc(i,j) = sum(rhs_driver%grid%dz(i,j,:)*weighted_toc(i,j,:))/cumdepth(i,j)
    end do
  end do
  ! save diagnostic values for output
  rhs_driver%weighted_toc(1:rhs_driver%inum,1:rhs_driver%jnum,1:rhs_driver%knum) = weighted_toc(1:rhs_driver%inum,1:rhs_driver%jnum,1:rhs_driver%knum)

  ! calculate infauna biomass [100 g/m2]
  do k=1,rhs_driver%knum
    rhs_driver%biomass(:,:,k) = weighted_toc(:,:,k) * exp(rhs_driver%grid%zc(:,:,k)*100.0d0*rhs_driver%k_l) * averaged_weighted_toc / &
                         (rhs_driver%L1 + rhs_driver%L2*exp(rhs_driver%grid%zc(:,:,k)*200.0d0*rhs_driver%k_l))
  end do

  ! calculate bioturbation [cm2/d]
  rhs_driver%bioturbation_factor = rhs_driver%beta * rhs_driver%biomass**rhs_driver%b / weighted_toc

else
  ! set temperature dependency factor for temporally constant bioturbation profile
  f_T = _ONE_*exp(-4500.d0*(1.d0/(rhs_driver%temp3d+273.d0) - (1.d0/288.d0)))
endif

do n=1,size(rhs_driver%model%state_variables)
   rhs_driver%diff = rhs_driver%bioturbation * f_T / 86400.0_rk / 10000_rk * &
              (rhs_driver%ones3d - rhs_driver%intf_porosity)*rhs_driver%bioturbation_factor

! print*,'fabm_sediment_driver#606 ',   trim(rhs_driver%model%state_variables(n)%long_name), &
!         rhs_driver%model%state_variables(n)%properties%get_logical('particulate',default=.false.), &
!         rhs_driver%conc(1,1,:,n)
   if (rhs_driver%model%state_variables(n)%properties%get_logical('particulate',default=.false.)) then
      bcup = rhs_driver%bcup_particulate_variables

      !write(0,*) rhs_driver%diff(1,1,:),'fac',rhs_driver%bioturbation_factor(1,1,:)
      !stop
      conc_insitu = rhs_driver%conc(:,:,:,n)*rhs_driver%porosity!/ &
! differing from original code: bioturbation mixes bulk concentrations
!              (rhs_driver%ones3d - rhs_driver%porosity)
      call diff3d(rhs_driver%grid,conc_insitu,rhs_driver%bdys(:,:,n+1), &
              rhs_driver%zeros2d, rhs_driver%fluxes(:,:,n), rhs_driver%zeros2d, &
              bcup, bcdown, rhs_driver%diff, &
              rhs_driver%ones3d - rhs_driver%porosity, intFlux, &
              rhs_driver%transport(:,:,:,n),flux_cap=rhs_driver%flux_cap)
! do k=1,2
!       print*,'fabm_sediment_driver#616 ',   conc_insitu(1,1,k), rhs_driver%transport(1,1,k,n), &
!               rhs_driver%bdys(1,1,n+1), rhs_driver%fluxes(1,1,n), intFlux(1,1,k)
! enddo
      rhs_driver%transport(:,:,:,n) = rhs_driver%transport(:,:,:,n) * &
              (rhs_driver%ones3d - rhs_driver%porosity)/rhs_driver%porosity
   else
      bcup = rhs_driver%bcup_dissolved_variables

      rhs_driver%diff = rhs_driver%diff+(rhs_driver%diffusivity + rhs_driver%temp3d * 0.035d0) &
             * rhs_driver%intf_porosity / 86400.0_rk / 10000_rk

      conc_insitu = rhs_driver%conc(:,:,:,n)
      call diff3d(rhs_driver%grid,conc_insitu,rhs_driver%bdys(:,:,n+1), &
              rhs_driver%zeros2d, rhs_driver%fluxes(:,:,n), rhs_driver%zeros2d, &
              bcup, bcdown, rhs_driver%diff, rhs_driver%porosity, intFlux, &
              rhs_driver%transport(:,:,:,n))
      ! set fluxes for output
      rhs_driver%fluxes(:,:,n) = intFlux(:,:,1)
   end if
end do

rhs=0.0_rk
do k=1,rhs_driver%knum
   do j=1,rhs_driver%jnum
      do i=1,rhs_driver%inum
         if (.not.rhs_driver%mask(i,j,k)) then
           call fabm_do(rhs_driver%model,i,j,k,rhs(i,j,k,:))
         else
           ! set transport to 0.0 - evtl. skip calculation of transport completely
           rhs(i,j,k,:) = 0.0d0
           rhs_driver%transport(i,j,k,:) = 0.0d0
         end if
      end do
   end do
end do

! return fabm-rhs + diff-tendencies
rhs = rhs + rhs_driver%transport

end subroutine get_rhs


!> finalize the FABM sediment driver
!!
!! deallocate all the arrays
subroutine finalize(sed)
class(type_sed) :: sed
  if (allocated(sed%zeros2dv))  deallocate(sed%zeros2dv)
  if (allocated(sed%zeros3dv))  deallocate(sed%zeros3dv)
  if (allocated(sed%zeros2d))   deallocate(sed%zeros2d)
  if (allocated(sed%ones3d))    deallocate(sed%ones3d)
  if (allocated(sed%transport)) deallocate(sed%transport)
  if (allocated(sed%diff))      deallocate(sed%diff)

end subroutine finalize


!> vertical diffusion in a 3d sediment grid
!!
!! Vertical diffusion in a porous sediment grid.

subroutine diff3d (grid, C, Cup, Cdown, fluxup, fluxdown,        &
                       BcUp, BcDown, D, VF, Flux, dC, flux_cap)
!! the code originates in the original omexdia module by Karline Soetaert
!! authors: Kai Wirtz & Richard Hofmeister

implicit none
class(fabm_sed_grid), intent(in)        :: grid
real(rk), dimension(grid%inum,grid%jnum,grid%knum), intent(in) :: C, D

! Boundary concentrations (used if Bc..=2,4), fluxes (used if Bc= 1)
! and convection coeff (used if Bc=4)
real(rk), dimension(grid%inum,grid%jnum), intent(in)   :: Cup, Cdown, fluxup, fluxdown

! volume fraction
real(rk), dimension(grid%inum,grid%jnum,grid%knum), intent(in) :: VF

! boundary concitions (1= flux, 2=conc, 3 = 0-grad, 4=convect)
integer, intent(in) :: BcUp, BcDown

! output: fluxes and rate of change
real(rk), dimension(grid%inum,grid%jnum,grid%knum+1), intent(out) :: Flux
real(rk), dimension(grid%inum,grid%jnum,grid%knum),intent(out) :: dC

! locals
integer  :: i,j,k
real(rk),dimension(grid%inum,grid%jnum)   :: AVF,restflux
real(rk),dimension(grid%inum,grid%jnum,grid%knum),optional :: flux_cap

! -------------------------------------------------------------------------------

dC = 0.0_rk

! Flux - first internal cells
! positive flux is directed downward
do j=1,grid%jnum
   do i=1,grid%inum
     if (.not.grid%mask(i,j,1)) then
      do k = 2,grid%knum
         Flux(i,j,k) = -D(i,j,k) * (C(i,j,k)-C(i,j,k-1)) /grid%dzc(i,j,k-1)
      end do

! Then the outer cells
! upstream boundary
      IF (BcUp .EQ. 1) THEN
        Flux(i,j,1) = fluxup(i,j)

      ELSE IF (BcUp .EQ. 2) THEN
        Flux(i,j,1) = -D(i,j,1) * (C(i,j,1)-Cup(i,j)) /grid%dz(i,j,1)

      ELSE IF (BcUp .EQ. 3) THEN
        Flux(i,j,1) = 0.0_rk

      ELSE IF (BcUp .EQ. 4) THEN
        Flux(i,j,1) = fluxup(i,j)
        k = 2
        restflux(i,j) = Flux(i,j,1) - flux_cap(i,j,1)
        do while ((restflux(i,j) .gt. 0) .and. (k .le. grid%knum))
           Flux(i,j,k) = Flux(i,j,k) + restflux(i,j)
           restflux(i,j) = restflux(i,j) - flux_cap(i,j,k)
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
        Flux(i,j,grid%knum+1) = -D(i,j,grid%knum) * (Cdown(i,j)-C(i,j,grid%knum)) / grid%dz(i,j,grid%knum)

      ELSE IF (BcDown .EQ. 3) THEN
        Flux(i,j,grid%knum+1) =0.0_rk

      ELSE
      ENDIF

      DO k = 1,grid%knum
        dC(i,j,k) = (Flux(i,j,k) - Flux(i,j,k+1))/(VF(i,j,k) * grid%dz(i,j,k))
      ENDDO
    end if
   end do
end do

end subroutine diff3D

!> Initializes a sediment export state by FABM state_variable id
function get_export_state_by_id(self,fabm_id) result(export_state)
   class(type_sed)         :: self
   type(export_state_type) :: export_state
   integer, intent(in)     :: fabm_id
   integer                 :: n

   export_state%fabm_id=fabm_id
   export_state%data => self%conc(:,:,:,export_state%fabm_id)
   !> first check for present standard name
   if (self%model%state_variables(fabm_id)%standard_variable%name/='') then
     export_state%standard_name = &
       trim(self%model%state_variables(fabm_id)%standard_variable%name)
     export_state%units = &
       trim(self%model%state_variables(fabm_id)%standard_variable%units)
   else
   !> otherwise use CF-ed version of long_name
     export_state%standard_name = only_var_name( &
           self%model%state_variables(fabm_id)%long_name)
     export_state%units = self%model%state_variables(fabm_id)%units
   end if
   export_state%fabm_id = fabm_id
end function get_export_state_by_id

!> Initializes a sediment export state by FABM diagnostic_variable id
function get_export_state_by_diag_id(self,fabm_id) result(export_state)
   class(type_sed)         :: self
   type(export_state_type) :: export_state
   integer, intent(in)     :: fabm_id
   integer                 :: n

   export_state%fabm_id=fabm_id
   !> data needs to be linked in the driver component

   !> first check for present standard name
   if (self%model%diagnostic_variables(fabm_id)%standard_variable%name/='') then
     export_state%standard_name = &
       trim(self%model%diagnostic_variables(fabm_id)%standard_variable%name)
     export_state%units = &
       trim(self%model%diagnostic_variables(fabm_id)%standard_variable%units)
   else
   !> otherwise use CF-ed version of long_name
     export_state%standard_name = only_var_name( &
           self%model%diagnostic_variables(fabm_id)%long_name)
     export_state%units = self%model%diagnostic_variables(fabm_id)%units
   end if
end function get_export_state_by_diag_id


!> create a list of export states from FABM state_variables
subroutine get_all_export_states(self)
   class(type_sed) :: self
   integer  :: n,fabm_id
   integer  :: nvar_export

   nvar_export = self%nvar+5
   if (self%bioturbation_profile.eq.3) nvar_export=nvar_export+3
   allocate(self%export_states(nvar_export))

   self%export_states(1)%standard_name='porosity'
   self%export_states(1)%data => self%porosity
   self%export_states(1)%units ='m3 m-3'

   self%export_states(2)%standard_name='layer_height'
   self%export_states(2)%data => self%grid%dz
   self%export_states(2)%units ='m'

   self%export_states(3)%standard_name='layer_center_depth'
   self%export_states(3)%data => self%grid%zc
   self%export_states(3)%units ='m'

   self%export_states(4)%standard_name='temperature'
   self%export_states(4)%data => self%temp3d
   self%export_states(4)%units ='degC'

   self%export_states(5)%standard_name='photosynthetically_active_radiation'
   self%export_states(5)%data => self%par
   self%export_states(5)%units ='W m-2'

   do fabm_id=1,self%nvar
       self%export_states(5+fabm_id) = self%get_export_state_by_id(fabm_id)
   end do

   if (self%bioturbation_profile.eq.3) then
     self%export_states(6+self%nvar)%standard_name='biomass'
     self%export_states(6+self%nvar)%data => self%biomass
     self%export_states(6+self%nvar)%units ='100g m-3'

     self%export_states(7+self%nvar)%standard_name='bioturbation'
     self%export_states(7+self%nvar)%data => self%bioturbation_factor
     self%export_states(7+self%nvar)%units ='cm2 d-1'

     self%export_states(8+self%nvar)%standard_name='weighted_toc'
     self%export_states(8+self%nvar)%data => self%weighted_toc
     self%export_states(8+self%nvar)%units ='mg/g-dry_weight'
   end if

end subroutine get_all_export_states

end module fabm_sediment_driver
