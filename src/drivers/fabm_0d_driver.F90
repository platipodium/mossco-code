#include "fabm_driver.h"
!> @file fabm_0d_driver.F90
!> @brief 0D independent driver for the Framework for Aquatic Biogeochemical Models (FABM)
!!
!! @author Jorn Bruggeman
!! @author Richard Hofmeister

   module mossco_fabm0d

   use time
   use input
   use fabm
   use fabm_types
   use solver_library

   implicit none
   private

   public init_run, time_loop, clean_up
   public get_export_state_from_variable_name, update_export_state

   integer, parameter        :: namlst=10, out_unit = 12, bio_unit=22
   integer, parameter        :: READ_SUCCESS=1
   integer, parameter        :: END_OF_FILE=-1
   integer, parameter        :: READ_ERROR=-2
   integer, parameter        :: CENTER=0,SURFACE=1,BOTTOM=2
   character, parameter      :: separator = char(9)

!>  private data members initialised via namelists
   character(len=80)         :: title
   real(rk)                  :: dt
!>  station description
   real(rk)                  :: latitude,longitude
   character(len=256)        :: din_variable,pon_variable

   !> Bio model info
   integer  :: ode_method
   integer(timestepkind)::nsave
   integer  :: swr_method
   real(rk) :: cloud
   real(rk) :: par_fraction
   real(rk) :: par_background_extinction
   logical  :: apply_self_shading
   logical  :: add_environment
   logical  :: add_conserved_quantities
   logical  :: add_diagnostic_variables
   logical  :: temp_from_file

   !> Environment
   real(rk),target         :: temp,salt,par,current_depth,dens,wind_sf,taub   
   real(rk)                :: par_sf,par_bt,par_ct,column_depth

   real(rk),allocatable      :: totals(:,:,:,:)
   character(len=128)        :: cbuf

   type,extends(rhs_driver), public :: type_fabm0d !< sediment driver class (extends rhs_driver)
       type(type_model),pointer       :: model
       real(rk),dimension(1:1,1:1,1:1) :: temp,salt,par,dens,current_depth
       real(rk),dimension(1:1,1:1)     :: wind_sf,taub,par_sf
       real(rk)                        :: decimal_yearday
   contains
       procedure :: get_rhs
   end type type_fabm0d

   type,public :: export_state_type
       character(len=256) :: standard_name=''
       integer            :: fabm_id=-1
       logical            :: particulate=.false.
       real(rk),dimension(:,:,:),pointer   :: conc
       real(rk),dimension(:,:,:),pointer   :: ws
   end type
   
   type(type_fabm0d),public        :: zerod

   interface
      function short_wave_radiation(jul,secs,dlon,dlat,cloud,bio_albedo) result(swr)
         import
         integer, intent(in)                 :: jul,secs
         real(rk), intent(in)                :: dlon,dlat
         real(rk), intent(in)                :: cloud
         real(rk), intent(in)                :: bio_albedo
         real(rk)                            :: swr
      end function short_wave_radiation
   end interface

   contains

!> Initialise the model
   subroutine init_run(forcing_from_coupler)

!>  This internal routine triggers the initialization of the model.
!!  The first section reads the namelists of {\tt run.nml} with
!!  the user specifications. Then, one by one each of the modules are
!!  initialised.

   logical, intent(in), optional :: forcing_from_coupler
   character(len=PATH_MAX)   :: env_file,output_file
   integer                   :: i
   real(rk)                  :: depth

   namelist /model_setup/ title,start,stop,dt,ode_method,din_variable,pon_variable
   namelist /environment/ env_file,swr_method, &
                          latitude,longitude,cloud,par_fraction, &
                          depth,par_background_extinction,apply_self_shading
   namelist /output/      output_file,nsave,add_environment, &
                          add_diagnostic_variables, add_conserved_quantities

   LEVEL1 'init_run'
   STDERR LINE

   ! evaluate forcing from coupler:
   if (present(forcing_from_coupler)) then
       temp_from_file=.not.(forcing_from_coupler)
   else
       temp_from_file=.true.
   endif

   ! Open the namelist file.
   LEVEL2 'reading model setup namelists..'
   open(namlst,file='run.nml',status='old',action='read',err=90)

   ! Initialize environment
   temp = 0.0_rk
   salt = 0.0_rk
   par = 0.0_rk
   dens = 0.0_rk
   wind_sf = 0.0_rk
   par_sf = 0.0_rk
   par_bt = 0.0_rk
   par_ct = 0.0_rk
   zerod%decimal_yearday = 0.0_rk
   taub  = 0.0_rk

   ! Read all namelists
   title = ''
   start = ''
   stop = ''
   dt = 0.0_rk
   ode_method = 1
   read(namlst,nml=model_setup,err=91)
   
   ! Read environment namelist
   env_file = ''
   swr_method = 0
   latitude = -100.0_rk
   longitude = -400.0_rk
   cloud = 0.0_rk
   par_fraction = 1.0_rk
   depth = -1.0_rk
   par_background_extinction = 0.0_rk
   apply_self_shading = .true.
   read(namlst,nml=environment,err=92)

   ! Read output namelist
   output_file = ''
   nsave = 1
   add_environment = .false.
   add_conserved_quantities = .false.
   add_diagnostic_variables=.false.
   read(namlst,nml=output     ,err=93)

   ! Close the namelist file.
   close (namlst)

   if (start=='') then
      FATAL 'run.nml: start time "start" must be set in "model_setup" namelist.'
      stop 'init_run'
   end if

   if (stop=='') then
      FATAL 'run.nml: stop time "stop" must be set in "model_setup" namelist.'
      stop 'init_run'
   end if

   if (dt<=0.0_rk) then
      FATAL 'run.nml: time step "dt" must be set to a positive value in "model_setup" namelist.'
      stop 'init_run'
   end if

   if (env_file=='') then
      FATAL 'run.nml: "env_file" must be set to a valid file path in "environment" namelist.'
      stop 'init_run'
   end if

   ! Make sure depth has been provided.
   if (depth<=0.0_rk) then
      FATAL 'run.nml: a positive value for "depth" must be provided in "environment" namelist.'
      stop 'init_run'
   end if
   column_depth = depth ! For now we have a single depth value only. Use that for both column depth and evaluation depth.
   call update_depth(CENTER)
   
   ! If longitude and latitude are used, make sure they have been provided and are valid.
   if (swr_method==0) then
      if (latitude<-90._rk.or.latitude>90._rk) then
         FATAL 'run.nml: a valid value for "latitude" must be provided in "environment" if "swr_method" is 0.'
         stop 'init_run'
      end if
      if (longitude<-360._rk.or.longitude>360._rk) then
         FATAL 'run.nml: a valid value for "longitude" must be provided  &
                in "environment" if "swr_method" is 0.'
         stop 'init_run'
      end if
   end if
   
   if (output_file=='') then
      FATAL 'run.nml: "output_file" must be set to a valid file path in "output" namelist.'
      stop 'init_run'
   end if

   LEVEL2 'done.'

   ! Configure the time module to use actual start and stop dates.
   timefmt = 2

   ! Transfer the time step to the time module.
   timestep = dt

   ! Write information for this run to the console.
   LEVEL2 'Simulation: '//trim(title)
   select case (swr_method)
      case (0)
         LEVEL2 'Surface photosynthetically active radiation will be calculated from time,'
         LEVEL2 'cloud cover, and the simulated location at (lat,long)'
         LEVEL2 latitude,longitude
         LEVEL2 'Local PAR will be calculated from the surface value,'
         LEVEL2 'depth, and light extinction coefficient.'
      case (1)
         LEVEL2 'Surface photosynthetically active radiation (PAR) is provided as input.'
         LEVEL2 'Local PAR will be calculated from the surface value,'
         LEVEL2 'depth, and light extinction coefficient.'
      case (2)
         LEVEL2 'Local photosynthetically active radiation is provided as input.'
   end select

   LEVEL2 'initializing modules....'

   ! Initialize the time module.
   call init_time(MinN,MaxN)

   ! Open the file with observations of the local environment.
   LEVEL2 'Reading local environment data from:'
   LEVEL3 trim(env_file)
   call init_input()
   call register_input_0d(env_file,1,par_sf)
   if (temp_from_file) call register_input_0d(env_file,2,temp)
   call register_input_0d(env_file,3,salt)

   ! Build FABM model tree.
   zerod%model => fabm_create_model_from_file(namlst)

   ! Send information on spatial domain to FABM (this also allocates memory for diagnostics)
   call fabm_set_domain(zerod%model,1,1,1)

   ! Set dimensions in rhs_driver class
   zerod%inum=1
   zerod%jnum=1
   zerod%knum=1
   zerod%nvar=size(zerod%model%info%state_variables)+size(zerod%model%info%state_variables_ben)

   ! Allocate space for totals of conserved quantities.
   allocate(totals(1,1,1,1:size(zerod%model%info%conserved_quantities)))

   ! Create state variable vector, using the initial values specified by the model,
   ! and link state data to FABM.
   allocate(zerod%conc(1,1,1,size(zerod%model%info%state_variables)+size(zerod%model%info%state_variables_ben)))
   do i=1,size(zerod%model%info%state_variables)
      zerod%conc(1,1,1,i) = zerod%model%info%state_variables(i)%initial_value
      call fabm_link_bulk_state_data(zerod%model,i,zerod%conc(1:1,1:1,1:1,i))
   end do

   ! Create benthos variable vector, using the initial values specified by the model,
   ! and link state data to FABM.
   do i=1,size(zerod%model%info%state_variables_ben)
      zerod%conc(1,1,1,size(zerod%model%info%state_variables)+i) = zerod%model%info%state_variables_ben(i)%initial_value
      call fabm_link_bottom_state_data(zerod%model,i,zerod%conc(1:1,1:1,1,size(zerod%model%info%state_variables)+i))
   end do

   ! Link environmental data to FABM
   call fabm_link_bulk_data(zerod%model,varname_temp,   zerod%temp)
   call fabm_link_bulk_data(zerod%model,varname_salt,   zerod%salt)
   call fabm_link_bulk_data(zerod%model,varname_par,    zerod%par)
   call fabm_link_bulk_data(zerod%model,varname_pres,   zerod%current_depth)
   call fabm_link_bulk_data(zerod%model,varname_dens,   zerod%dens)
   call fabm_link_horizontal_data(zerod%model,varname_wind_sf,zerod%wind_sf)
   call fabm_link_horizontal_data(zerod%model,varname_par_sf, zerod%par_sf)
   call fabm_link_horizontal_data(zerod%model,varname_taub, zerod%taub)
   call fabm_link_scalar_data(zerod%model,varname_yearday, zerod%decimal_yearday)

   ! Open the output file.
   open(out_unit,file=output_file,action='write', &
        status='replace',err=96)
   LEVEL2 'Writing results to:'
   LEVEL3 trim(output_file)

   ! Write header to the output file.
   write(out_unit,FMT='(''# '',A)') title
   write(out_unit,FMT='(''# '',A)',ADVANCE='NO') 'time'
   if (add_environment) then
      write(out_unit,FMT=100,ADVANCE='NO') separator,'photosynthetically active radiation','W/m2'
      write(out_unit,FMT=100,ADVANCE='NO') separator,'temperature',                        'degrees C'
      write(out_unit,FMT=100,ADVANCE='NO') separator,'salinity',                           'kg/m3'
   end if
   do i=1,size(zerod%model%info%state_variables)
      write(out_unit,FMT=100,ADVANCE='NO') separator, &
            trim(zerod%model%info%state_variables(i)%long_name), &
            trim(zerod%model%info%state_variables(i)%units)
   end do
   do i=1,size(zerod%model%info%state_variables_ben)
      write(out_unit,FMT=100,ADVANCE='NO') separator, &
            trim(zerod%model%info%state_variables_ben(i)%long_name), &
            trim(zerod%model%info%state_variables_ben(i)%units)
   end do
   if (add_diagnostic_variables) then
      do i=1,size(zerod%model%info%diagnostic_variables)
         write(out_unit,FMT=100,ADVANCE='NO') separator, &
               trim(zerod%model%info%diagnostic_variables(i)%long_name), &
               trim(zerod%model%info%diagnostic_variables(i)%units)
      end do
      do i=1,size(zerod%model%info%diagnostic_variables_hz)
         write(out_unit,FMT=100,ADVANCE='NO') separator, &
               trim(zerod%model%info%diagnostic_variables_hz(i)%long_name),&
               trim(zerod%model%info%diagnostic_variables_hz(i)%units)
      end do
   end if
   if (add_conserved_quantities) then
      do i=1,size(zerod%model%info%conserved_quantities)
         write(out_unit,FMT=100,ADVANCE='NO') separator, &
               trim(zerod%model%info%conserved_quantities(i)%long_name), &
               trim(zerod%model%info%conserved_quantities(i)%units)
      end do
   end if
   write(out_unit,*)

   LEVEL2 'done.'
   STDERR LINE

   return

90 FATAL 'I could not open run.nml for reading'
   stop 'init_run'
91 FATAL 'I could not read the "model_setup" namelist'
   stop 'init_run'
92 FATAL 'I could not read the "environment" namelist'
   stop 'init_run'
93 FATAL 'I could not read the "output" namelist'
   stop 'init_run'
95 FATAL 'I could not open ',trim(env_file)
   stop 'init_run'
96 FATAL 'I could not open ',trim(output_file)
   stop 'init_run'
97 FATAL 'I could not open fabm.nml for reading'
   stop 'init_run'
100 format (A, A, ' (', A, ')')
   end subroutine init_run
!EOC

   subroutine update_depth(location)
      integer, intent(in) :: location
      
      select case (location)
         case (SURFACE)
            current_depth = 0.0_rk
            zerod%par = par_sf
         case (BOTTOM)
            current_depth = column_depth            
            zerod%par = par_bt
         case (CENTER)
            current_depth = 0.5_rk*column_depth
            zerod%par = par_ct
      end select
   end subroutine update_depth


subroutine get_rhs(rhsd,rhs)
use fabm
use fabm_types
implicit none

class(type_fabm0d)      ,intent(inout)            :: rhsd
real(rk),intent(inout),dimension(:,:,:,:),pointer :: rhs
integer :: n

rhs=0.0_rk

   ! Shortcut to the number of pelagic state variables.
   n = size(rhsd%model%info%state_variables)

   ! Calculate temporal derivatives due to surface exchange.
   call update_depth(SURFACE)
   call fabm_get_surface_exchange(rhsd%model,1,1,1,rhs(1,1,1,1:n))

   ! Calculate temporal derivatives due to benthic processes.
   call update_depth(BOTTOM)
   call fabm_do_benthos(rhsd%model,1,1,1,rhs(1,1,1,1:n),rhs(1,1,1,n+1:))

   ! For pelagic variables: surface and bottom flux (rate per surface area) to concentration (rate per volume)
   rhs(1,1,1,1:n) = rhs(1,1,1,1:n)/column_depth

   ! Add change in pelagic variables.
   call update_depth(CENTER)
   call fabm_do(rhsd%model,1,1,1,rhs(1,1,1,:))

end subroutine get_rhs

!> This internal routine is the heart of the code. It contains
!! the main time-loop inside of which all routines required
!! during the time step are called.
   subroutine time_loop()

   integer                   :: i
   integer(timestepkind)     :: n
   real(rk)                  :: extinction,bio_albedo

   do n=MinN,MaxN

      ! Update time
      call update_time(n)

      zerod%decimal_yearday = yearday-1 + dble(secondsofday)/86400.

      ! Update environment
      call do_input(julianday,secondsofday)

      ! Calculate photosynthetically active radiation if it is not provided in the input file.
      if (swr_method==0) then
         ! Calculate photosynthetically active radiation from geographic location, time, cloud cover.
         call fabm_get_albedo(zerod%model,1,1,1,bio_albedo)
         par_sf = short_wave_radiation(julianday,secondsofday,longitude,latitude,cloud,bio_albedo)
      end if

      ! Multiply by fraction of short-wave radiation that is photosynthetically active.
      par_sf = par_fraction*par_sf

      ! Apply light attentuation with depth, unless local light is provided in the input file.
      if (swr_method/=2) then
         ! Either we calculate surface PAR, or surface PAR is provided.
         ! Calculate the local PAR at the given depth from par fraction, extinction coefficient, and depth.
         extinction = 0.0_rk
         if (apply_self_shading) call fabm_get_light_extinction(zerod%model,1,1,1,extinction)
         extinction = extinction + par_background_extinction
         par_ct = par_sf*exp(-0.5_rk*column_depth*extinction)
         par_bt = par_sf*exp(-column_depth*extinction)
      else
         par_ct = par_sf
         par_bt = par_sf
      end if

      if (temp_from_file) then
          zerod%temp=temp
      else
          ! get tempa from inportState
      end if
      
      zerod%salt=salt
      call update_depth(CENTER)

      ! Integrate one time step
      call ode_solver(zerod,dt,ode_method)

      ! Reset to global array.
      do i=1,size(zerod%model%info%state_variables)
         call fabm_link_bulk_state_data(zerod%model,i,zerod%conc(1:1,1:1,1:1,i))
      end do
      do i=1,size(zerod%model%info%state_variables_ben)
         call fabm_link_bottom_state_data(zerod%model,i,zerod%conc(1:1,1:1,1,size(zerod%model%info%state_variables)+i))
      end do

      ! Do output
      if (mod(n,nsave)==0) then
         call write_time_string(julianday,secondsofday,timestr)
         write (out_unit,FMT='(A)',ADVANCE='NO') timestr
         if (add_environment) then
            write (out_unit,FMT='(A,E15.8E3)',ADVANCE='NO') separator,zerod%par(1,1,1)
            write (out_unit,FMT='(A,E15.8E3)',ADVANCE='NO') separator,zerod%temp(1,1,1)
            write (out_unit,FMT='(A,E15.8E3)',ADVANCE='NO') separator,zerod%salt(1,1,1)
         end if
         do i=1,(size(zerod%model%info%state_variables)+size(zerod%model%info%state_variables_ben))
            write (out_unit,FMT='(A,E15.8E3)',ADVANCE='NO') separator,zerod%conc(1,1,1,i)
         end do
         if (add_diagnostic_variables) then
            do i=1,size(zerod%model%info%diagnostic_variables)
               write (out_unit,FMT='(A,E15.8E3)',ADVANCE='NO') separator,fabm_get_bulk_diagnostic_data(zerod%model,i)
            end do
            do i=1,size(zerod%model%info%diagnostic_variables_hz)
               write (out_unit,FMT='(A,E15.8E3)',ADVANCE='NO') separator,fabm_get_horizontal_diagnostic_data(zerod%model,i)
            end do
         end if
         if (add_conserved_quantities) then
            call fabm_get_conserved_quantities(zerod%model,1,1,1,totals(1,1,1,:))
            do i=1,size(zerod%model%info%conserved_quantities)
               write (out_unit,FMT='(A,E15.8E3)',ADVANCE='NO') separator,totals(1,1,1,i)
            end do
         end if
         write (out_unit,*)
      end if

   end do

   return
   end subroutine time_loop


   subroutine clean_up()

   LEVEL1 'clean_up'
   call close_input()
   close(out_unit)
   
   end subroutine clean_up


   subroutine get_export_state_from_variable_name(export_state,varname)
   type(export_state_type), intent(inout) :: export_state
   character(len=256), intent(in)         :: varname
   real(rk),allocatable,save :: wstmp(:,:,:,:)
   integer  :: n

   export_state%fabm_id=-1
   do n=1,size(zerod%model%info%state_variables)
     if (trim(zerod%model%info%state_variables(n)%name).eq.trim(varname)) &
         export_state%fabm_id=n
   end do
   export_state%conc => zerod%conc(:,:,:,export_state%fabm_id)
   allocate(export_state%ws(1,1,1))
   allocate(wstmp(1,1,1,zerod%nvar))
   call fabm_get_vertical_movement(zerod%model,1,1,1,wstmp(1,1,1,:))
   export_state%ws = wstmp(1,1,1,export_state%fabm_id)
   deallocate(wstmp)

   end subroutine get_export_state_from_variable_name


   subroutine update_export_state(export_state)
   type(export_state_type), intent(inout) :: export_state
   real(rk),allocatable :: wstmp(:,:,:,:)

   export_state%conc => zerod%conc(:,:,:,export_state%fabm_id)
   allocate(wstmp(1,1,1,zerod%nvar))
   call fabm_get_vertical_movement(zerod%model,1,1,1,wstmp(1,1,1,:))
   export_state%ws = wstmp(1,1,1,export_state%fabm_id)
   deallocate(wstmp)

   end subroutine update_export_state

   end module mossco_fabm0d

