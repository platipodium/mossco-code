#include "fabm_driver.h"
!> @file fabm_gotm_driver.F90
!> @brief MOSSCO's GOTM driver for the Framework for Aquatic Biogeochemical Models (FABM),
!>        adopted from the GOTM model
!!
!! @author Jorn Bruggeman
!! @author Richard Hofmeister

module gotm_mossco_fabm

use fabm
use fabm_types
use solver_library

implicit none

private

public init_gotm_mossco_fabm, do_gotm_mossco_fabm

type,extends(rhs_driver), public :: type_gotm_fabm !< gotm_fabm driver class (extends rhs_driver)
   type(type_model),pointer      :: model
contains
   procedure :: get_rhs
end type type_gotm_fabm

type(type_gotm_fabm),public :: gotmfabm


!  Arrays for state and diagnostic variables
   REALTYPE,pointer,dimension(_LOCATION_DIMENSIONS_,:),public            :: cc
   REALTYPE,allocatable,dimension(_LOCATION_DIMENSIONS_,:),public        :: cc_diag
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
   REALTYPE,allocatable,dimension(_LOCATION_DIMENSIONS_,:) :: ws
   REALTYPE,allocatable,dimension(:,:,:)                   :: sfl,bfl,total
   REALTYPE,allocatable _ATTR_DIMENSIONS_1_                :: local
   REALTYPE,allocatable,dimension(_LOCATION_DIMENSIONS_)   :: Qsour,Lsour,DefaultRelaxTau,curh,curnuh
   logical,allocatable                                     :: cc_transport(:)

   ! Arrays for environmental variables not supplied externally.
   REALTYPE,allocatable,dimension(_LOCATION_DIMENSIONS_)   :: par,pres,swr,k_par

   ! External variables
   integer  :: w_adv_ctr   ! Scheme for vertical advection (0 if not used)
   REALTYPE,pointer,dimension(:)                     :: nuh,h,w,z,rho
   REALTYPE,pointer,dimension(:)                     :: bioshade
   REALTYPE,allocatable,dimension(_LOCATION_DIMENSIONS_)   :: currho,curtemp,cursalt
   REALTYPE,pointer,dimension(_LOCATION_DIMENSIONS_) :: SRelaxTau,sProf,salt
   REALTYPE,pointer _ATTR_LOCATION_DIMENSIONS_HZ_    :: precip,evap,bio_drag_scale,bio_albedo

   REALTYPE,pointer :: I_0,A,g1,g2
   integer,pointer  :: yearday,secondsofday
   REALTYPE, target :: decimal_yearday
   logical          :: fabm_ready

   contains

   subroutine init_gotm_mossco_fabm(nlev,fname)
!
! !DESCRIPTION:
! Initializes the GOTM-FABM driver module by reading settings from fabm.nml.
!
! !INPUT PARAMETERS:
   integer,          intent(in)        :: nlev
   character(len=*), intent(in)        :: fname
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
!  local variables
   integer                   :: i,namlst=55
   namelist /gotm_fabm_nml/ fabm_calc,                                               &
                            cnpar,w_adv_discr,ode_method,split_factor,               &
                            bioshade_feedback,bioalbedo_feedback,biodrag_feedback,   &
                            repair_state,no_precipitation_dilution,                  &
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

   gotmfabm%knum=nlev
   gotmfabm%inum=1
   gotmfabm%jnum=1
   if (fabm_calc) then

      fabm_ready = .false.

      ! Create model tree
      gotmfabm%model => fabm_create_model_from_file(namlst)

      ! Initialize model tree (creates metadata and assigns variable identifiers)
      call fabm_set_domain(gotmfabm%model,1,1,nlev)
      gotmfabm%nvar=size(gotmfabm%model%info%state_variables) + &
          size(gotmfabm%model%info%state_variables_ben)

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
      temp_id = fabm_get_bulk_variable_id(gotmfabm%model,varname_temp)
      salt_id = fabm_get_bulk_variable_id(gotmfabm%model,varname_salt)
      rho_id  = fabm_get_bulk_variable_id(gotmfabm%model,varname_dens)
      h_id    = fabm_get_bulk_variable_id(gotmfabm%model,varname_layer_ht)
      lon_id       = fabm_get_horizontal_variable_id(gotmfabm%model,varname_lon)
      lat_id       = fabm_get_horizontal_variable_id(gotmfabm%model,varname_lat)
      windspeed_id = fabm_get_horizontal_variable_id(gotmfabm%model,varname_wind_sf)
      par_sf_id    = fabm_get_horizontal_variable_id(gotmfabm%model,varname_par_sf)
      cloud_id     = fabm_get_horizontal_variable_id(gotmfabm%model,varname_cloud)
      taub_id      = fabm_get_horizontal_variable_id(gotmfabm%model,varname_taub)

   end if

   end subroutine init_gotm_mossco_fabm

   subroutine init_var_gotm_mossco_fabm()
!
! !DESCRIPTION:
! This routine allocates memory for all FABM variables.
!
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i,rc
!
!-----------------------------------------------------------------------
!BOC
   ! Allocate state variable array for pelagic and benthos combined and provide initial values.
   ! In terms of memory use, it is a waste to allocate storage for benthic variables across the entire
   ! column (the bottom layer should suffice). However, it is important that all values at a given point
   ! in time are integrated simultaneously in multi-step algorithms. This currently can only be arranged
   ! by storing benthic values together with the pelagic, in a fully depth-explicit array.
   allocate(gotmfabm%conc(1,1,1:gotmfabm%knum,1:size(gotmfabm%model%info%state_variables) + &
       size(gotmfabm%model%info%state_variables_ben)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (gotmfabm%conc)'
   cc => gotmfabm%conc
   gotmfabm%conc = _ZERO_
   do i=1,size(gotmfabm%model%info%state_variables)
      gotmfabm%conc(1,1,:,i) = gotmfabm%model%info%state_variables(i)%initial_value
!      call fabm_link_bulk_state_data(gotmfabm%model,i,cc(1,1,1:,i))
   end do
   do i=1,size(gotmfabm%model%info%state_variables_ben)
      gotmfabm%conc(1,1,1,size(gotmfabm%model%info%state_variables)+i) = &
             gotmfabm%model%info%state_variables_ben(i)%initial_value
!      call fabm_link_bottom_state_data(gotmfabm%model,i,cc(1,1,1,size(gotmfabm%model%info%state_variables)+i))
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
   allocate(ws(1,1,1:gotmfabm%knum,1:size(gotmfabm%model%info%state_variables)),stat=rc)
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
   if (rc /= 0) stop 'allocate_memory(): Error allocating (par)'
   call fabm_link_bulk_data(gotmfabm%model,varname_par,par)

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
   call fabm_link_scalar_data(gotmfabm%model,varname_yearday,decimal_yearday)

   allocate(Qsour(1,1,1:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (Qsour)'
   Qsour    = _ZERO_

   allocate(Lsour(1,1,1:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (Lsour)'
   Lsour    = _ZERO_

   allocate(DefaultRelaxTau(1,1,1:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (DefaultRelaxTau)'
   DefaultRelaxTau = 1.d15

   allocate(curh(1,1,1:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (curh)'
   curh = _ZERO_

   allocate(curnuh(1,1,1:gotmfabm%knum),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (curnuh)'
   curnuh = _ZERO_

   end subroutine init_var_gotm_mossco_fabm

   subroutine do_gotm_mossco_fabm(dt)
!
! !DESCRIPTION:
! TODO
!
! !USES:
   use util,only: flux,Neumann
!
   real(rk), intent(in) :: dt
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer, parameter        :: adv_mode_0=0
   integer, parameter        :: adv_mode_1=1
   REALTYPE                  :: dilution,virtual_dilution,dt_eff
   integer                   :: i,j,k
   integer                   :: split,posconc
   integer(8)                :: clock_start,clock_end
!
!-----------------------------------------------------------------------
!BOC

   if (.not. fabm_calc) return

   ! Set contiguous arrays with layer heights and diffusivity, needed in calls to adv_center and diff_center.
   curh(1,1,:)   = h
   curnuh(1,1,:) = nuh

   ! Calculate local pressure from layer height and density
   pres(1,1,gotmfabm%knum) = rho(gotmfabm%knum)*curh(1,1,gotmfabm%knum)*0.5d0
   do i=gotmfabm%knum-1,1,-1
      pres(1,1,i) = pres(1,1,i+1) + (rho(i)*curh(1,1,i)+rho(i+1)*curh(1,1,i+1))*0.5d0
   end do
   pres(1,1,1:gotmfabm%knum) = pres(1,1,1:gotmfabm%knum)*9.81d-4

!  Calculate decimal day of the year (1 jan 00:00 = 0.)
   decimal_yearday = yearday-1 + dble(secondsofday)/86400.d0

   ! Get updated vertical movement (m/s, positive for upwards) for biological state variables.
   do k=1,gotmfabm%knum
      call fabm_get_vertical_movement(gotmfabm%model,1,1,k,ws(1,1,k,:))
   end do

   ! Get updated air-sea fluxes for biological state variables.
   call fabm_get_surface_exchange(gotmfabm%model,1,1,gotmfabm%knum,sfl(1,1,:))

   ! Calculate dilution due to surface freshwater flux (m/s)
   dilution = precip(1,1)+evap(1,1)

   ! If salinity is relaxed to observations, the change in column-integrated salintiy can converted into a
   ! a virtual freshwater flux. Optionally, this freshwater flux can be imposed at the surface on biogoeochemical
   ! variables, effectively mimicking precipitation or evaporation. This makes sense only if the salinity change
   ! is primarily due to surface fluxes - not if it is meant to represent lateral input of other water masses.
   virtual_dilution = _ZERO_
   if (salinity_relaxation_to_freshwater_flux) then
      ! NB unit of virtual_dilution is relative dilution across column, i.e., fraction/s
      if (any(SRelaxTau(1,1,1:gotmfabm%knum)<1.e10) .and. any(salt>0.)) &
         virtual_dilution = sum((salt(1,1,1:gotmfabm%knum)-sProf(1,1,1:gotmfabm%knum))/ &
             SRelaxTau(1,1,1:gotmfabm%knum)*curh(1,1,1:gotmfabm%knum))/ &
             sum(salt(1,1,1:gotmfabm%knum)*curh(1,1,1:gotmfabm%knum))
   end if

   ! Add surface flux due to evaporation/precipitation, unless the model explicitly says otherwise.
   do i=1,size(gotmfabm%model%info%state_variables)
      if (.not. (gotmfabm%model%info%state_variables(i)%no_precipitation_dilution .or. no_precipitation_dilution)) then
         sfl(1,1,i) = sfl(1,1,i)-cc(1,1,gotmfabm%knum,i)*dilution
         if (virtual_dilution/=_ZERO_) sfl(1,1,i) = sfl(1,1,i) - &
             sum(cc(1,1,1:gotmfabm%knum,i)*curh(1,1,1:gotmfabm%knum))*virtual_dilution
      end if
   end do

   ! Vertical advection and residual movement (sinking/floating)
   call system_clock(clock_start)
   do i=1,size(gotmfabm%model%info%state_variables)
      if (.not.cc_transport(i)) cycle

      ! Do advection step due to settling or rising
      call adv_center(gotmfabm%knum,dt,curh,curh,ws(1,1,:,i),flux, &
          flux,_ZERO_,_ZERO_,w_adv_discr,adv_mode_1,cc(1,1,:,i))

      ! Do advection step due to vertical velocity
      if (w_adv_method/=0) call adv_center(gotmfabm%knum,dt,curh,curh,w,flux, &
          flux,_ZERO_,_ZERO_,w_adv_ctr,adv_mode_0,cc(1,1,:,i))
   end do

   ! Vertical diffusion
   do i=1,size(gotmfabm%model%info%state_variables)
      if (.not.cc_transport(i)) cycle

      ! Determine whether the variable is positive-definite based on its lower allowed bound.
      posconc = 0
      if (gotmfabm%model%info%state_variables(i)%minimum>=_ZERO_) posconc = 1

      ! Do diffusion step
      call diff_center(gotmfabm%knum,dt,cnpar,posconc,curh,Neumann,Neumann,&
            sfl(1,1,i),bfl(1,1,i),curnuh,Lsour,Qsour,DefaultRelaxTau,cc(1,1,:,i),cc(1,1,:,i))
   end do

   ! Repair state before calling FABM
   call do_repair_state(gotmfabm%knum,'gotm_mossco_fabm::do_gotm_mossco_fabm, after advection/diffusion')

   do split=1,split_factor
      ! Update local light field (self-shading may have changed through changes in biological state variables)
      call light(gotmfabm%knum,bioshade_feedback)
      
      do k=1,gotmfabm%knum
        ! Time-integrate one biological time step
        call ode_solver(gotmfabm,dt_eff,ode_method)

        ! Provide FABM with (pointers to) updated state variables.
        ! (integration scheme has redirected FABM to a temporary biogeochemical state)
        do i=1,size(gotmfabm%model%info%state_variables)
            call fabm_link_bulk_state_data(gotmfabm%model,i,gotmfabm%conc(:,:,:,i))
        end do
      end do

      do i=1,size(gotmfabm%model%info%state_variables_ben)
        call fabm_link_bottom_state_data(gotmfabm%model,i,cc(:,:,1,size(gotmfabm%model%info%state_variables)+i))
      end do

      ! Repair state
        call do_repair_state(gotmfabm%knum,'gotm_mossco_fabm::do_gotm_mossco_fabm, after time integration')

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

!   if (associated(bio_albedo))     call fabm_get_albedo(gotmfabm%model,gotmfabm%knum,bio_albedo)
!   if (associated(bio_drag_scale)) call fabm_get_drag(gotmfabm%model,gotmfabm%knum,bio_drag_scale)

   end subroutine do_gotm_mossco_fabm


   subroutine get_rhs(rhsd,rhs)
   use fabm
   use fabm_types
   implicit none

   class(type_gotm_fabm),intent(inout) :: rhsd
   real(rk),intent(inout),dimension(:,:,:,:),pointer :: rhs
   integer :: i,k,n

   n = size(gotmfabm%model%info%state_variables)

   do i=1,size(gotmfabm%model%info%state_variables)
     do k=1,gotmfabm%knum
      call fabm_link_bulk_state_data(gotmfabm%model,i,rhsd%conc(:,:,:,i))
     end do
   end do
   do i=1,size(gotmfabm%model%info%state_variables_ben)
      call fabm_link_bottom_state_data(gotmfabm%model,i,rhsd%conc(:,:,1,n+i))
   end do

   rhs = _ZERO_
   !call fabm_do_benthos(gotmfabm%model,1,rhs(1,1:n),rhs(1,n+1:))
   !rhs(1,1,1,1:n) = rhs(1,1,1,1:n)/curh(1,1,1)

   ! Add pelagic sink and source terms for all depth levels.
   do k=1,gotmfabm%knum
      call fabm_do(gotmfabm%model,1,1,k,rhs(1,1,k,1:n))
   end do

   end subroutine get_rhs


   subroutine clean_gotm_mossco_fabm
!
! !DESCRIPTION:
!  Report timing results and deallocate memory.
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP

!-----------------------------------------------------------------------
!BOC


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
   if (allocated(local))      deallocate(local)
   LEVEL1 'done.'

   end subroutine clean_gotm_mossco_fabm

      subroutine light(nlev,bioshade_feedback)
!
! !DESCRIPTION:
! Calculate photosynthetically active radiation (PAR) and short wave
! radiation (SWR) over entire column, using surface short wave radiation,
! and background and biotic extinction.
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   logical, intent(in)                 :: bioshade_feedback
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer :: i
   REALTYPE :: bioext,localext
#ifdef _FABM_USE_1D_LOOP_
   REALTYPE :: localexts(1:nlev)
#endif
!
!-----------------------------------------------------------------------
!BOC
   bioext = _ZERO_

   do i=nlev,1,-1
      call fabm_get_light_extinction(gotmfabm%model,1,1,i,localext)

      ! Add the extinction of the first half of the grid box.
      bioext = bioext+localext*0.5*curh(1,1,i)

      ! Calculate photosynthetically active radiation (PAR), shortwave radiation, and PAR attenuation.
      par(1,1,i) = I_0*(_ONE_-A)*exp(z(i)/g2-bioext)
      swr(1,1,i) = par(1,1,i)+I_0*A*exp(z(i)/g1)
      k_par(1,1,i) = _ONE_/g2+localext

      ! Add the extinction of the second half of the grid box.
      bioext = bioext+localext*0.5*curh(1,1,i)

      if (bioshade_feedback) bioshade(i)=exp(-bioext)
   end do

   end subroutine light


   subroutine init_gotm_mossco_fabm_state()

      integer :: i

      if (.not.fabm_calc) return

      do i=1,gotmfabm%knum
         call fabm_initialize_state(gotmfabm%model,1,1,i)
      end do
      call fabm_initialize_surface_state(gotmfabm%model,1,1,gotmfabm%knum)
      call fabm_initialize_bottom_state(gotmfabm%model,1,1,1)

   end subroutine init_gotm_mossco_fabm_state

end module
