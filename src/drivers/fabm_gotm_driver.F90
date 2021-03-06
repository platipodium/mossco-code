#include "fabm_driver.h"
! defines taken from GOTM
#define POINT           0
#define Z_SHAPE         1
#define T_SHAPE         2
#define XY_SHAPE        3
#define XYT_SHAPE       4
#define XYZT_SHAPE      5

! Old definitions
#ifndef LEVEL1
#define STDERR write(0,*)
#define FATAL  STDERR 'FATAL ERROR: ',
#define LEVEL1 STDERR '   ',
#define LEVEL2 STDERR '       ',
#define LEVEL3 STDERR '           ',
#define LEVEL4 STDERR '               ',
#endif

!> @file fabm_gotm_driver.F90
!> @brief MOSSCO's GOTM driver for the Framework for Aquatic Biogeochemical Models (FABM),
!>        adopted from the GOTM model
!!
!! @author Jorn Bruggeman
!! @author Richard Hofmeister
!! @author Carsten Lemmen

module gotm_mossco_fabm

use fabm
use fabm_types
use fabm_driver
use fabm_config
use solver_library
use mossco_strings

implicit none

private

public init_gotm_mossco_fabm, do_gotm_mossco_fabm
public set_env_gotm_fabm
public get_export_state_by_id,get_export_state_from_variable_name
public get_all_export_states, update_export_states

type,extends(type_rhs_driver), public :: type_gotm_fabm !< gotm_fabm driver class (extends type_rhs_driver)
   type(type_model),pointer         :: model
   integer                          :: nvar_ben=0
   integer                          :: nvar_pel=0
   real(rk),dimension(:),pointer    :: layer_height
contains
   procedure :: get_rhs
end type type_gotm_fabm

   type,public :: export_state_type !< GOTM-FABM driver type for export states
       character(len=256) :: standard_name=''
       character(len=256) :: units=''
       integer            :: fabm_id=-1
       logical            :: particulate=.false.
       real(rk),dimension(:,:,:),pointer   :: conc
       real(rk),dimension(:,:,:),pointer   :: ws
   end type


type(type_gotm_fabm),public :: gotmfabm

type,extends(type_base_driver) :: type_mossco_driver
contains
   procedure :: fatal_error => mossco_driver_fatal_error
   procedure :: log_message => mossco_driver_log_message
end type


!  Arrays for state and diagnostic variables
   REALTYPE,allocatable,target,dimension(:,:,:,:),public :: cc
   REALTYPE,allocatable,dimension(:,:,:,:),public        :: cc_diag
   REALTYPE,allocatable,dimension(:,:,:),                  public        :: cc_diag_hz

   type (type_bulk_variable_id),      save :: temp_id,salt_id,rho_id,h_id
   type (type_horizontal_variable_id),save :: lon_id,lat_id,windspeed_id,par_sf_id,cloud_id,taub_id

   ! Namelist variables
   REALTYPE                  :: cnpar
   integer                   :: w_adv_method,w_adv_discr,ode_method,split_factor
   logical                   :: fabm_calc,repair_state, &
                                bioshade_feedback,bioalbedo_feedback,biodrag_feedback, &
                                no_precipitation_dilution,salinity_relaxation_to_freshwater_flux, &
                                save_inputs

   ! Arrays for work, vertical movement, and cross-boundary fluxes
   REALTYPE,allocatable,dimension(:,:,:,:) :: ws
   REALTYPE,public,allocatable,dimension(:,:,:)            :: sfl,bfl,total
   REALTYPE,allocatable,dimension(:),target                :: Qsour,Lsour,DefaultRelaxTau,curh,curnuh
   logical,allocatable                                     :: cc_transport(:)

   ! Arrays for environmental variables not supplied externally.
   REALTYPE,allocatable,dimension(:,:,:)   :: par,pres,swr,k_par
   REALTYPE,allocatable,dimension(:,:,:)   :: temp3d,salt3d,rho3d,nuh3d,h3d,w3d,bioshade3d,z3d
   REALTYPE, target, dimension(1:1,1:1)                     :: I_02d,cloud2d,wnd2d,precip2d,evap2d,taub2d
   REALTYPE, target, dimension(1:1,1:1)                     :: latitude2d,longitude2d

   ! External variables
   integer  :: w_adv_ctr   ! Scheme for vertical advection (0 if not used)
   REALTYPE,pointer,dimension(:)    :: nuh,h,w,z,rho
   REALTYPE,pointer,dimension(:)    :: bioshade
   REALTYPE,pointer,dimension(:)    :: SRelaxTau,sProf,salt
   REALTYPE,pointer                 :: precip,evap,bio_drag_scale,bio_albedo

   REALTYPE,pointer :: I_0,A,g1,g2
   integer,pointer  :: yearday,secondsofday
   REALTYPE, target :: decimal_yearday
   logical          :: fabm_ready

   contains

   !> Initializes the GOTM-FABM driver module by reading settings from fabm.nml
   subroutine init_gotm_mossco_fabm(nlev,fname,dt)

   integer,          intent(in)        :: nlev
   character(len=*), intent(in)        :: fname
   REALTYPE        , intent(in)        :: dt

   !  Original author(s): Jorn Bruggeman

   integer                   :: i,namlst=55
   REALTYPE                  :: dt_min=1.0, relative_change_min=0.1
   namelist /gotm_fabm_nml/ fabm_calc,                                               &
                            cnpar,w_adv_discr,ode_method,split_factor,               &
                            bioshade_feedback,bioalbedo_feedback,biodrag_feedback,   &
                            repair_state,no_precipitation_dilution,                  &
                            dt_min, relative_change_min,                             &
                            salinity_relaxation_to_freshwater_flux,save_inputs
!
!-----------------------------------------------------------------------
!BOC

   LEVEL1 'init_gotm_mossco_fabm'

   nullify(gotmfabm%model)

   ! Initialize all namelist variables to reasonable default values.
   fabm_calc         = .false.
   cnpar             = _ONE_
   w_adv_discr       = 6
   ode_method        = 1
   split_factor      = 1
   bioshade_feedback = .true.
   bioalbedo_feedback = .true.
   biodrag_feedback  = .true.
   repair_state      = .false.
   salinity_relaxation_to_freshwater_flux = .false.
   no_precipitation_dilution = .false. ! useful to check mass conservation
   save_inputs = .false.

   ! Open the namelist file and read the namelist.
   ! Note that the namelist file is left open until the routine terminates,
   ! so FABM can read more namelists from it during initialization.
   open(namlst,file=fname,action='read',status='old')
   read(namlst,nml=gotm_fabm_nml)
   close(namlst)

   gotmfabm%dt_min=dt_min
   gotmfabm%relative_change_min=relative_change_min
   gotmfabm%knum=nlev
   gotmfabm%inum=1
   gotmfabm%jnum=1
   if (fabm_calc) then
      ! Provide FABM with an object for communication with host
      allocate(type_mossco_driver::driver)

      fabm_ready = .false.

      ! Create model tree
      gotmfabm%model => fabm_create_model_from_file(namlst)

      ! Initialize model tree (creates metadata and assigns variable identifiers)
      call fabm_set_domain(gotmfabm%model,1,1,nlev,dt)
      call gotmfabm%model%set_bottom_index(1)
      call gotmfabm%model%set_surface_index(gotmfabm%knum)
      gotmfabm%nvar_pel=size(gotmfabm%model%info%state_variables)
      gotmfabm%nvar_ben=size(gotmfabm%model%info%state_variables_ben)
      gotmfabm%nvar=gotmfabm%nvar_pel + gotmfabm%nvar_ben

      ! set mask
      allocate(gotmfabm%mask(1,1,nlev))
      gotmfabm%mask(:,:,:) = .false.

      ! Report prognostic variable descriptions
      LEVEL2 'FABM pelagic state variables:'
      do i=1,size(gotmfabm%model%info%state_variables)
         LEVEL3 trim(gotmfabm%model%info%state_variables(i)%name), '  ', &
                trim(gotmfabm%model%info%state_variables(i)%units),'  ',&
                trim(gotmfabm%model%info%state_variables(i)%long_name)
      end do

      LEVEL2 'FABM benthic state variables:'
      do i=1,size(gotmfabm%model%info%state_variables_ben)
         LEVEL3 trim(gotmfabm%model%info%state_variables_ben(i)%name), '  ', &
                trim(gotmfabm%model%info%state_variables_ben(i)%units),'  ',&
                trim(gotmfabm%model%info%state_variables_ben(i)%long_name)
      end do

      ! Report diagnostic variable descriptions
      LEVEL2 'FABM diagnostic variables defined on the full model domain:'
      do i=1,size(gotmfabm%model%info%diagnostic_variables)
         LEVEL3 trim(gotmfabm%model%info%diagnostic_variables(i)%name), '  ', &
                trim(gotmfabm%model%info%diagnostic_variables(i)%units),'  ',&
                trim(gotmfabm%model%info%diagnostic_variables(i)%long_name)
      end do

      LEVEL2 'FABM diagnostic variables defined on a horizontal slice of the model domain:'
      do i=1,size(gotmfabm%model%info%diagnostic_variables_hz)
         LEVEL3 trim(gotmfabm%model%info%diagnostic_variables_hz(i)%name), '  ', &
                trim(gotmfabm%model%info%diagnostic_variables_hz(i)%units),'  ',&
                trim(gotmfabm%model%info%diagnostic_variables_hz(i)%long_name)
      end do

      ! Initialize spatially explicit variables
      call init_var_gotm_mossco_fabm()

      ! Get ids for standard variables, to be used later to send data to FABM.
      temp_id = gotmfabm%model%get_bulk_variable_id(standard_variables%temperature)
      salt_id = gotmfabm%model%get_bulk_variable_id(standard_variables%practical_salinity)
      rho_id  = gotmfabm%model%get_bulk_variable_id(standard_variables%density)
      h_id    = gotmfabm%model%get_bulk_variable_id(standard_variables%cell_thickness)
      lon_id       = gotmfabm%model%get_horizontal_variable_id(standard_variables%longitude)
      lat_id       = gotmfabm%model%get_horizontal_variable_id(standard_variables%latitude)
      windspeed_id = gotmfabm%model%get_horizontal_variable_id(standard_variables%wind_speed)
      par_sf_id    = gotmfabm%model%get_horizontal_variable_id(standard_variables%surface_downwelling_photosynthetic_radiative_flux)
      cloud_id     = gotmfabm%model%get_horizontal_variable_id(standard_variables%cloud_area_fraction)
      taub_id      = gotmfabm%model%get_horizontal_variable_id(standard_variables%bottom_stress)

   allocate(temp3d(1,1,nlev))
   allocate(salt3d(1,1,nlev))
   allocate(rho3d(1,1,nlev))
   allocate(nuh3d(1,1,nlev))
   allocate(h3d(1,1,nlev))
   allocate(w3d(1,1,nlev))
   allocate(bioshade3d(1,1,nlev))
   allocate(z3d(1,1,nlev))

   end if

   end subroutine init_gotm_mossco_fabm


   !>This routine allocates memory for all FABM variables.
   subroutine init_var_gotm_mossco_fabm()

   !  Original author(s): Jorn Bruggeman
   integer   :: i,k,rc
   REALTYPE  :: rhs(1,1,1,1:size(gotmfabm%model%info%state_variables))
   REALTYPE  :: bottom_flux(1,1,1:gotmfabm%nvar_ben)

   ! Allocate state variable array for pelagic and benthos combined and provide initial values.
   ! In terms of memory use, it is a waste to allocate storage for benthic variables across the entire
   ! column (the bottom layer should suffice). However, it is important that all values at a given point
   ! in time are integrated simultaneously in multi-step algorithms. This currently can only be arranged
   ! by storing benthic values together with the pelagic, in a fully depth-explicit array.
   allocate(cc(1,1,0:gotmfabm%knum,1:size(gotmfabm%model%info%state_variables) + &
       size(gotmfabm%model%info%state_variables_ben)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (gotmfabm%conc)'
   gotmfabm%conc => cc(:,:,1:gotmfabm%knum,:)
   gotmfabm%conc = _ZERO_
   do i=1,size(gotmfabm%model%info%state_variables)
      gotmfabm%conc(1,1,:,i) = gotmfabm%model%info%state_variables(i)%initial_value
      call fabm_link_bulk_state_data(gotmfabm%model,i,cc(:,:,1:gotmfabm%knum,i))
   end do
   do i=1,size(gotmfabm%model%info%state_variables_ben)
      gotmfabm%conc(1,1,1,size(gotmfabm%model%info%state_variables)+i) = &
             gotmfabm%model%info%state_variables_ben(i)%initial_value
      call fabm_link_bottom_state_data(gotmfabm%model,i,cc(:,:,1,size(gotmfabm%model%info%state_variables)+i))
   end do

   ! Allocate array for pelagic diagnostic variables; set all values to zero.
   ! (zeroing is needed because time-integrated/averaged variables will increment rather than set the array)
   allocate(cc_diag(1,1,1:gotmfabm%knum,1:size(gotmfabm%model%info%diagnostic_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (cc_diag)'
   cc_diag = _ZERO_

   ! Allocate array for diagnostic variables on horizontal surfaces; set all values to zero.
   ! (zeroing is needed because time-integrated/averaged variables will increment rather than set the array)
   allocate(cc_diag_hz(1,1,1:size(gotmfabm%model%info%diagnostic_variables_hz)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (cc_diag_hz)'
   cc_diag_hz = _ZERO_

   ! Allocate array for vertical movement rates (m/s, positive for upwards),
   ! and set these to the values provided by the model.
   allocate(ws(1,1,0:gotmfabm%knum,1:size(gotmfabm%model%info%state_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (ws)'
   do i=1,size(gotmfabm%model%info%state_variables)
      ws(1,1,:,i) = gotmfabm%model%info%state_variables(i)%vertical_movement
   end do

   ! Allocate array for surface fluxes and initialize these to zero (no flux).
   allocate(sfl(1,1,1:size(gotmfabm%model%info%state_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (sfl)'
   sfl = _ZERO_

   ! Allocate array for bottom fluxes and initialize these to zero (no flux).
   allocate(bfl(1,1,1:size(gotmfabm%model%info%state_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (bfl)'
   bfl = _ZERO_

      ! Allocate array for photosynthetically active radiation (PAR).
   ! This will be calculated internally during each time step.
   allocate(par(1,1,1:gotmfabm%knum),stat=rc)
   par=_ZERO_
   if (rc /= 0) stop 'allocate_memory(): Error allocating (par)'
   call fabm_link_bulk_data(gotmfabm%model,standard_variables%downwelling_photosynthetic_radiative_flux,par)

   ! Allocate array for attenuation coefficient pf photosynthetically active radiation (PAR).
   ! This will be calculated internally during each time step.
   allocate(k_par(1,1,1:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (k_par)'
   k_par = _ZERO_
!   call fabm_link_bulk_data(gotmfabm%model,varname_extc,k_par(1:_LOCATION_))

   ! Allocate array for shortwave radiation (swr).
   ! This will be calculated internally during each time step.
   allocate(swr(1,1,1:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (swr)'
!   call fabm_link_bulk_data(gotmfabm%model,varname_swr,swr(1:_LOCATION_))

   ! Allocate array for local pressure.
   ! This will be calculated from layer depths and density internally during each time step.
   allocate(pres(1,1,1:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (pres)'
!   call fabm_link_bulk_data(gotmfabm%model,varname_pres,pres(1,1,1:gotmfabm%knum))

   ! Initialize scalar to hold day of the year (floating point value),
   ! and link it to FABM.
   decimal_yearday = _ZERO_
   call fabm_link_scalar_data(gotmfabm%model,standard_variables%number_of_days_since_start_of_the_year,decimal_yearday)

   allocate(Qsour(0:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (Qsour)'
   Qsour    = _ZERO_

   allocate(Lsour(0:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (Lsour)'
   Lsour    = _ZERO_

   allocate(DefaultRelaxTau(0:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (DefaultRelaxTau)'
   DefaultRelaxTau = 1.d15

   allocate(curh(0:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (curh)'
   curh = _ZERO_
   gotmfabm%layer_height=>curh

   allocate(curnuh(0:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (curnuh)'
   curnuh = _ZERO_

   ! call fabm_do to fill diagnostic variables and pre-fetch data
   call fabm_do_surface(gotmfabm%model,1,1,rhs(1,1,1,:))
   call fabm_do_bottom(gotmfabm%model,1,1,rhs(1,1,1,:),bottom_flux(1,1,:))
   rhs=0.0_rk
   do k=1,gotmfabm%knum
     call fabm_do(gotmfabm%model,1,1,k,rhs(1,1,1,:))
   end do

   end subroutine init_var_gotm_mossco_fabm

   !> This routine is called once from GOTM to provide pointers to the arrays that describe
   !! the physical environment relevant for biogeochemical processes (temprature, salinity, etc.)
   subroutine set_env_gotm_fabm(latitude,longitude,dt_,w_adv_method_,w_adv_ctr_,temp,salt_,rho_,nuh_,h_,w_, &
                                bioshade_,I_0_,cloud,taub,wnd,precip_,evap_,z_,A_,g1_,g2_, &
                                yearday_,secondsofday_,SRelaxTau_,sProf_,bio_albedo_,bio_drag_scale_)

   REALTYPE, intent(in),target                              :: latitude,longitude
   REALTYPE, intent(in) :: dt_
   integer,  intent(in) :: w_adv_method_,w_adv_ctr_
   REALTYPE, intent(in),target,dimension(1:gotmfabm%knum)   :: temp,salt_,rho_
   REALTYPE, intent(in),target,dimension(0:gotmfabm%knum)   :: nuh_,h_,w_
   REALTYPE, intent(in),target,dimension(1:gotmfabm%knum)   :: bioshade_,z_

   REALTYPE, intent(in),target                              :: I_0_,cloud,wnd,precip_,evap_,taub

   REALTYPE, intent(in),target :: A_,g1_,g2_
   integer,  intent(in),target :: yearday_,secondsofday_
   REALTYPE, intent(in),optional,target,dimension(:) :: SRelaxTau_,sProf_
   REALTYPE, intent(in),optional,target              :: bio_albedo_,bio_drag_scale_

!  Original author(s): Jorn Bruggeman

   if (.not. fabm_calc) return

   temp3d(1,1,:)=temp
   salt3d(1,1,:)=salt_
   rho3d(1,1,:)=rho_
   nuh3d(1,1,:)=nuh_(1:)
   h3d(1,1,:)=h_(1:)
   w3d(1,1,:)=w_(1:)
   bioshade3d(1,1,:)=bioshade_
   z3d(1,1,:)=z_
   I_02d(1,1)=I_0_
   cloud2d(1,1)=cloud
   wnd2d(1,1)=wnd
   precip2d(1,1)=precip_
   evap2d(1,1)=evap_
   taub2d(1,1)=taub
   latitude2d(1,1)=latitude
   longitude2d(1,1)=longitude

   ! Provide pointers to arrays with environmental variables to FABM.
   call fabm_link_bulk_data      (gotmfabm%model,temp_id,     temp3d)
   call fabm_link_bulk_data      (gotmfabm%model,salt_id,     salt3d)
   call fabm_link_bulk_data      (gotmfabm%model,rho_id,      rho3d)
   call fabm_link_bulk_data      (gotmfabm%model,h_id,        h3d)
   call fabm_link_horizontal_data(gotmfabm%model,lon_id,      longitude2d)
   call fabm_link_horizontal_data(gotmfabm%model,lat_id,      latitude2d)
   call fabm_link_horizontal_data(gotmfabm%model,windspeed_id,wnd2d)
   call fabm_link_horizontal_data(gotmfabm%model,par_sf_id,   I_02d)
   call fabm_link_horizontal_data(gotmfabm%model,cloud_id,    cloud2d)
   call fabm_link_horizontal_data(gotmfabm%model,taub_id,     taub2d)

   ! Save pointers to external dynamic variables that we need later (in do_gotm_fabm)
   nuh      => nuh_        ! turbulent heat diffusivity [1d array] used to diffuse biogeochemical state variables
   h        => h_          ! layer heights [1d array] needed for advection, diffusion
   w        => w_          ! vertical medium velocity [1d array] needed for advection of biogeochemical state variables
   bioshade => bioshade_   ! biogeochemical light attenuation coefficients [1d array], output of biogeochemistry, input for physics
   z        => z_          ! depth [1d array], used to calculate local light intensity
   precip   => precip_     ! precipitation [scalar] - used to calculate dilution due to increased water volume
   evap     => evap_       ! evaporation [scalar] - used to calculate concentration due to decreased water volume
   salt     => salt_       ! salinity [1d array] - used to calculate virtual freshening due to salinity relaxation
   rho      => rho_        ! density [1d array] - used to calculate bottom stress from bottom friction velocity.

   if (biodrag_feedback.and.present(bio_drag_scale_)) then
      bio_drag_scale => bio_drag_scale_
   else
      nullify(bio_drag_scale)
   end if
   if (bioalbedo_feedback.and.present(bio_albedo_)) then
      bio_albedo => bio_albedo_
   else
      nullify(bio_albedo)
   end if

   if (present(SRelaxTau_) .and. present(sProf_)) then
      SRelaxTau => SRelaxTau_ ! salinity relaxation times  [1d array] - used to calculate virtual freshening due to salinity relaxation
      sProf     => sProf_     ! salinity relaxation values [1d array] - used to calculate virtual freshening due to salinity relaxation
   else
      if (salinity_relaxation_to_freshwater_flux) &
         stop 'gotm_fabm:set_env_gotm_fabm: salinity_relaxation_to_freshwater_flux is set, &
              &but salinity relaxation arrays are not provided.'
      nullify(SRelaxTau)
      nullify(sProf)
   end if

   ! Copy scalars that will not change during simulation, and are needed in do_gotm_fabm)
   w_adv_method = w_adv_method_
   w_adv_ctr = w_adv_ctr_

   I_0 => I_0_
   A => A_
   g1 => g1_
   g2 => g2_

   yearday => yearday_
   secondsofday => secondsofday_

   ! At this stage, FABM has been provided with arrays for all state variables, any variables
   ! read in from file (gotm_fabm_input), and all variables exposed by GOTM. If FABM is still
   ! lacking variable references, this should now trigger an error.
   if (.not.fabm_ready) then
      call fabm_check_ready(gotmfabm%model)
      fabm_ready = .true.
   end if

   end subroutine set_env_gotm_fabm


   !> do a timestep of FABM integration in MOSSCO's GOTM component
   subroutine do_gotm_mossco_fabm(dt)

   use util,only: flux,Neumann

   real(rk), intent(in) :: dt

   integer, parameter        :: adv_mode_0=0
   integer, parameter        :: adv_mode_1=1
   REALTYPE                  :: dilution,virtual_dilution,dt_eff
   integer                   :: i,j,k
   integer                   :: split,posconc
   integer(8)                :: clock_start,clock_end

   if (.not. fabm_calc) return

   ! Set contiguous arrays with layer heights and diffusivity, needed in calls to adv_center and diff_center.
   curh   = h
   curnuh = nuh

   ! Calculate local pressure from layer height and density
   pres(1,1,gotmfabm%knum) = rho(gotmfabm%knum)*curh(gotmfabm%knum)*0.5d0
   do i=gotmfabm%knum-1,1,-1
      pres(1,1,i) = pres(1,1,i+1) + (rho(i)*curh(i)+rho(i+1)*curh(i+1))*0.5d0
   end do
   pres(1,1,1:gotmfabm%knum) = pres(1,1,1:gotmfabm%knum)*9.81d-4

!  Calculate decimal day of the year (1 jan 00:00 = 0.)
   decimal_yearday = yearday-1 + dble(secondsofday)/86400.d0

   ! Get updated vertical movement (m/s, positive for upwards) for biological state variables.
   do k=1,gotmfabm%knum
      call fabm_get_vertical_movement(gotmfabm%model,1,1,k,ws(1,1,k,:))
   end do

   ! Get updated air-sea fluxes for biological state variables.
   call fabm_get_surface_exchange(gotmfabm%model,1,1,sfl(1,1,:))

   ! Calculate dilution due to surface freshwater flux (m/s)
   dilution = precip+evap

   ! If salinity is relaxed to observations, the change in column-integrated salintiy can converted into a
   ! a virtual freshwater flux. Optionally, this freshwater flux can be imposed at the surface on biogoeochemical
   ! variables, effectively mimicking precipitation or evaporation. This makes sense only if the salinity change
   ! is primarily due to surface fluxes - not if it is meant to represent lateral input of other water masses.
   virtual_dilution = _ZERO_
   if (salinity_relaxation_to_freshwater_flux) then
      ! NB unit of virtual_dilution is relative dilution across column, i.e., fraction/s
      if (any(SRelaxTau(1:gotmfabm%knum)<1.e10) .and. any(salt>0.)) &
         virtual_dilution = sum((salt(1:gotmfabm%knum)-sProf(1:gotmfabm%knum))/ &
             SRelaxTau(1:gotmfabm%knum)*curh(1:gotmfabm%knum))/ &
             sum(salt(1:gotmfabm%knum)*curh(1:gotmfabm%knum))
   end if

   ! Add surface flux due to evaporation/precipitation, unless the model explicitly says otherwise.
   do i=1,size(gotmfabm%model%info%state_variables)
      if (.not. (gotmfabm%model%info%state_variables(i)%no_precipitation_dilution .or. no_precipitation_dilution)) then
         sfl(1,1,i) = sfl(1,1,i)-cc(1,1,gotmfabm%knum,i)*dilution
         if (virtual_dilution/=_ZERO_) sfl(1,1,i) = sfl(1,1,i) - &
             sum(cc(1,1,1:gotmfabm%knum,i)*curh(1:gotmfabm%knum))*virtual_dilution
      end if
   end do

   ! Vertical advection and residual movement (sinking/floating)
   do i=1,size(gotmfabm%model%info%state_variables)

      ! Do advection step due to settling or rising
      call adv_center(gotmfabm%knum,dt,curh,curh,ws(1,1,:,i),flux, &
          flux,_ZERO_,_ZERO_,w_adv_discr,adv_mode_1,cc(1,1,0:gotmfabm%knum,i))

      ! Do advection step due to vertical velocity
      if (w_adv_method/=0) call adv_center(gotmfabm%knum,dt,curh,curh,w,flux, &
          flux,_ZERO_,_ZERO_,w_adv_ctr,adv_mode_0,cc(1,1,0:gotmfabm%knum,i))
   end do

   ! Vertical diffusion
   do i=1,size(gotmfabm%model%info%state_variables)

      ! Determine whether the variable is positive-definite based on its lower allowed bound.
      posconc = 0
      if (gotmfabm%model%info%state_variables(i)%minimum>=_ZERO_) posconc = 1

      ! Do diffusion step
      ! Euler integration of boundary fluxes
      cc(1,1,1,i) = cc(1,1,1,i) + dt*bfl(1,1,i)/curh(1)
      bfl(1,1,i) = 0.0d0
      call diff_center(gotmfabm%knum,dt,cnpar,posconc,curh,Neumann,Neumann,&
            sfl(1,1,i),bfl(1,1,i),curnuh,Lsour,Qsour,DefaultRelaxTau,cc(1,1,:,i),cc(1,1,:,i))
   end do

   ! Repair state before calling FABM
   !call do_repair_state(gotmfabm%knum,'gotm_mossco_fabm::do_gotm_mossco_fabm, after advection/diffusion')
   dt_eff = dt/dble(split_factor)

   do split=1,split_factor
      ! Update local light field (self-shading may have changed through changes in biological state variables)
      call light(gotmfabm%knum,bioshade_feedback)

      ! Time-integrate one biological time step
      call ode_solver(gotmfabm,dt_eff,ode_method)

      ! Provide FABM with (pointers to) updated state variables.
      ! (integration scheme has redirected FABM to a temporary biogeochemical state)
      do i=1,size(gotmfabm%model%info%state_variables)
        call fabm_link_bulk_state_data(gotmfabm%model,i,gotmfabm%conc(:,:,:,i))
      end do

      do i=1,size(gotmfabm%model%info%state_variables_ben)
        call fabm_link_bottom_state_data(gotmfabm%model,i,cc(:,:,1,size(gotmfabm%model%info%state_variables)+i))
      end do

      ! Repair state
      !  call do_repair_state(gotmfabm%knum,'gotm_mossco_fabm::do_gotm_mossco_fabm, after time integration')
      do i=1,gotmfabm%nvar
        do k=1,gotmfabm%knum
          if (gotmfabm%conc(1,1,k,i) .lt. gotmfabm%model%info%state_variables(i)%minimum) then
            gotmfabm%conc(1,1,k,i) = gotmfabm%model%info%state_variables(i)%minimum
          end if
        end do
      end do

      ! Time-integrate diagnostic variables defined on horizontal slices, where needed.
      do i=1,size(gotmfabm%model%info%diagnostic_variables_hz)
         if (gotmfabm%model%info%diagnostic_variables_hz(i)%time_treatment==time_treatment_last) then
            ! Simply use last value
            cc_diag_hz(:,:,i) = fabm_get_horizontal_diagnostic_data(gotmfabm%model,i)
         else
            ! Integration or averaging in time needed: for now do simple Forward Euler integration.
            ! If averaging is required, this will be done upon output by diving by the elapsed period.
            cc_diag_hz(:,:,i) = cc_diag_hz(:,:,i) + fabm_get_horizontal_diagnostic_data(gotmfabm%model,i)*dt_eff
         end if
      end do

      ! Time-integrate diagnostic variables defined on the full domain, where needed.
      do i=1,size(gotmfabm%model%info%diagnostic_variables)
         if (gotmfabm%model%info%diagnostic_variables(i)%time_treatment==time_treatment_last) then
            ! Simply use last value
            cc_diag(:,:,:,i) = fabm_get_bulk_diagnostic_data(gotmfabm%model,i)
         else
            ! Integration or averaging in time needed: for now do simple Forward Euler integration.
            ! If averaging is required, this will be done upon output by diving by the elapsed period.
            cc_diag(:,:,:,i) = cc_diag(:,:,:,i) + fabm_get_bulk_diagnostic_data(gotmfabm%model,i)*dt_eff
         end if
      end do
   end do

!   if (associated(bio_albedo))     call fabm_get_albedo(gotmfabm%model,bio_albedo)
!   if (associated(bio_drag_scale)) call fabm_get_drag(gotmfabm%model,bio_drag_scale)

   end subroutine do_gotm_mossco_fabm


   !> provide right-hand sides for MOSSCO's solver library
   subroutine get_rhs(rhs_driver,rhs)
   use fabm
   use fabm_types
   implicit none

   class(type_gotm_fabm),intent(inout)               :: rhs_driver
   real(rk),intent(inout),dimension(:,:,:,:),pointer :: rhs
   integer :: i,k,n

   n = rhs_driver%nvar_pel
   if (.not.associated(rhs)) &
     allocate(rhs(rhs_driver%inum,rhs_driver%jnum,rhs_driver%knum,rhs_driver%nvar))

   do i=1,size(gotmfabm%model%info%state_variables)
      call fabm_link_bulk_state_data(rhs_driver%model,i,rhs_driver%conc(:,:,:,i))
   end do
   do i=1,size(gotmfabm%model%info%state_variables_ben)
      call fabm_link_bottom_state_data(rhs_driver%model,i,rhs_driver%conc(:,:,1,n+i))
   end do

   rhs = _ZERO_
   call fabm_do_benthos(rhs_driver%model,1,1,rhs(1,1,1,1:n),rhs(1,1,1,n+1:))
   rhs(1,1,1,1:n) = rhs(1,1,1,1:n)/rhs_driver%layer_height(1)

   ! Add pelagic sink and source terms for all depth levels.
   do k=1,gotmfabm%knum
      call fabm_do(rhs_driver%model,1,1,k,rhs(1,1,k,1:n))
   end do

   end subroutine get_rhs


   subroutine clean_gotm_mossco_fabm

!  Original author(s): Jorn Bruggeman

   LEVEL1 'clean_gotm_mossco_fabm'

   ! Deallocate internal arrays
   if (allocated(cc_diag))    deallocate(cc_diag)
   if (allocated(cc_diag_hz)) deallocate(cc_diag_hz)
   if (allocated(ws))         deallocate(ws)
   if (allocated(sfl))        deallocate(sfl)
   if (allocated(bfl))        deallocate(bfl)
   if (allocated(par))        deallocate(par)
   if (allocated(k_par))      deallocate(k_par)
   if (allocated(swr))        deallocate(swr)
   if (allocated(pres))       deallocate(pres)
   if (allocated(total))      deallocate(total)
   LEVEL1 'done.'

   end subroutine clean_gotm_mossco_fabm

   !> Calculate photosynthetically active radiation (PAR) and short wave
   !! radiation (SWR) over entire column, using surface short wave radiation,
   !! and background and biotic extinction.
   subroutine light(nlev,bioshade_feedback)

   integer, intent(in)                 :: nlev
   logical, intent(in)                 :: bioshade_feedback

!  Original author(s): Jorn Bruggeman

   integer :: i
   REALTYPE :: bioext,localext

   bioext = _ZERO_

   do i=nlev,1,-1
      call fabm_get_light_extinction(gotmfabm%model,1,1,i,localext)

      ! Add the extinction of the first half of the grid box.
      bioext = bioext+localext*0.5_rk*curh(i)

      ! Calculate photosynthetically active radiation (PAR), shortwave radiation, and PAR attenuation.
      par(1,1,i) = I_0*(_ONE_-A)*exp(z(i)/g2-bioext)
      swr(1,1,i) = par(1,1,i)+I_0*A*exp(z(i)/g1)
      k_par(1,1,i) = _ONE_/g2+localext

      ! Add the extinction of the second half of the grid box.
      bioext = bioext+localext*0.5_rk*curh(i)

      if (bioshade_feedback) bioshade(i)=exp(-bioext)
   end do

   end subroutine light


   subroutine init_gotm_mossco_fabm_state()

      integer :: i

      if (.not.fabm_calc) return

      do i=1,gotmfabm%knum
         call fabm_initialize_state(gotmfabm%model,1,1,i)
      end do
      call fabm_initialize_surface_state(gotmfabm%model,1,1)
      call fabm_initialize_bottom_state(gotmfabm%model,1,1)

   end subroutine init_gotm_mossco_fabm_state


   !> Initializes a GOTM-FABM export state by FABM state_variable id

   function get_export_state_by_id(fabm_id) result(export_state)
   type(export_state_type) :: export_state
   integer, intent(in)     :: fabm_id
   integer                 :: n

   export_state%fabm_id=fabm_id
   export_state%conc => gotmfabm%conc(:,:,:,export_state%fabm_id)
   allocate(export_state%ws(1,1,gotmfabm%knum))
   export_state%ws = 0.0d0
   !> first check for present standard name
   if (gotmfabm%model%info%state_variables(fabm_id)%standard_variable%name/='') then
     export_state%standard_name = &
       trim(gotmfabm%model%info%state_variables(fabm_id)%standard_variable%name)
     export_state%units = &
       trim(gotmfabm%model%info%state_variables(fabm_id)%standard_variable%units)
   else
   !> otherwise use CF-ed version of long_name
     export_state%standard_name = only_var_name( &
           gotmfabm%model%info%state_variables(fabm_id)%long_name)
     export_state%units = gotmfabm%model%info%state_variables(fabm_id)%units
   end if
   end function get_export_state_by_id

!> Initializes a GOTM-FABM export state by FABM variable name

   function get_export_state_from_variable_name(varname) result(export_state)
   type(export_state_type)        :: export_state
   character(len=256), intent(in) :: varname
   integer                        :: n,fabm_id

   fabm_id=-1
   do n=1,size(gotmfabm%model%info%state_variables)
     if (trim(gotmfabm%model%info%state_variables(n)%name).eq.trim(varname)) &
         fabm_id=n
   end do
   do n=1,size(gotmfabm%model%info%state_variables_ben)
     if (trim(gotmfabm%model%info%state_variables_ben(n)%name).eq.trim(varname)) &
         fabm_id=n
   end do
   export_state= get_export_state_by_id(fabm_id)

   end function get_export_state_from_variable_name

!> create a list of export states from FABM state_variables
   subroutine get_all_export_states(export_states)
   type(export_state_type),dimension(:),allocatable :: export_states
   integer  :: n,fabm_id

   allocate(export_states(gotmfabm%nvar_pel))
   do fabm_id=1,gotmfabm%nvar_pel
       export_states(fabm_id) = get_export_state_by_id(fabm_id)
   end do
   call update_export_states(export_states)
   !> @todo: add benthic state variables

   end subroutine get_all_export_states

!> update GOTM-FABM export states pointers and sinking velocities using a list of export states

   subroutine update_export_states(export_states)
   type(export_state_type), target :: export_states(:)
   real(rk),allocatable :: wstmp(:,:,:,:)
   type(export_state_type),pointer :: export_state
   integer :: n

   allocate(wstmp(1,1,1,gotmfabm%nvar))
   call fabm_get_vertical_movement(gotmfabm%model,1,1,1,wstmp(1,1,1,:))
   do n=1,size(export_states)
     export_state => export_states(n)
     export_state%conc => gotmfabm%conc(:,:,:,export_state%fabm_id)
     export_state%ws = wstmp(1,1,1,export_state%fabm_id)
     export_state%units = trim(gotmfabm%model%state_variables(n)%units)
   end do
   deallocate(wstmp)
   !> @todo add benthic state variables

   end subroutine update_export_states

   subroutine mossco_driver_fatal_error(self,location,message)
      class (type_mossco_driver), intent(inout) :: self
      character(len=*),  intent(in)    :: location,message

      FATAL trim(location)//': '//trim(message)
      stop 1
   end subroutine

   subroutine mossco_driver_log_message(self,message)
      class (type_mossco_driver), intent(inout) :: self
      character(len=*),  intent(in)    :: message

      write (*,*) trim(message)
   end subroutine

end module
