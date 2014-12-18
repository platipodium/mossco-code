!> @file fabm0d_driver.F90
!> @brief 3D generic driver for the Framework for Aquatic Biogeochemical Models (FABM)
!!
!! @author Richard Hofmeister
!!
!! This class provides a routine to create a pelagic FABM instance (called "pf" here),
!! which initialises FABM and allocates memory for the state variable. The pf instance
!! then these methods:
!!   * set_environment - set the environment forcing
!!   * get_rhs - get the temporal rates
!!   * light - calculate 3d PAR based on radiation at water surface and FABM's extinction
!!   * update_export_states - renew pointers and data (incl. sinking velocities) for export

  module mossco_fabm_pelagic

#define RANGE3D 1:pf%inum,1:pf%jnum,1:pf%knum
#define RANGE2D 1:pf%inum,1:pf%jnum
  use solver_library
  use mossco_strings
  use fabm
  use fabm_types
  use fabm_standard_variables
  implicit none
  private

  public mossco_create_fabm_pelagic

  type, extends(type_rhs_driver), public :: type_mossco_fabm_pelagic
    type(type_model),pointer           :: model
    type(export_state_type),dimension(:),pointer :: export_states
    real(rk),dimension(:,:,:),pointer  :: temp,salt,par,dens,current_depth
    real(rk),dimension(:,:,:),pointer  :: layer_height=>null()
    real(rk),dimension(:,:,:),pointer  :: zi=>null() !> layer interface depth
    real(rk),dimension(:,:),pointer    :: wind_sf,taub,par_sf,I_0=>null()
    real(rk)                           :: decimal_yearday
    real(rk)                           :: background_extinction=7.9 ![m] - Jerlov 6
    integer                            :: ndiag
    logical                            :: fabm_ready
    type(type_bulk_standard_variable),dimension(:),pointer :: bulk_dependencies
    type(type_horizontal_standard_variable), dimension(:), pointer :: horizontal_dependencies
    contains
    procedure :: get_rhs
    procedure :: get_dependencies
    procedure :: set_environment
    procedure :: light
    procedure :: check_ready
    procedure :: get_export_state_by_id
    procedure :: get_all_export_states
    procedure :: update_export_states
    procedure :: diagnostic_variables
    procedure :: initialize_concentrations
    procedure :: update_pointers
    procedure :: initialize_domain
    procedure :: update_grid
  end type

  type,public :: export_state_type !< pelagic FABM driver type for export states
    character(len=256) :: standard_name=''
    character(len=256) :: units=''
    integer            :: fabm_id=-1
    logical            :: particulate=.false.
    real(rk),dimension(:,:,:),pointer   :: conc
    real(rk),dimension(:,:,:),pointer   :: ws
  end type

  contains

  !> creates instance of pelagic fabm class
  function mossco_create_fabm_pelagic() result(pf)
  integer  :: n
  integer  :: namlst=123
  real(rk) :: dt
  type(type_mossco_fabm_pelagic), allocatable :: pf

  allocate(pf)
  pf%fabm_ready=.false.
  pf%conc => null()
  pf%par => null()
  ! Build FABM model tree.
  pf%model => fabm_create_model_from_file(namlst)

  pf%nvar = size(pf%model%info%state_variables)
  pf%ndiag = size(pf%model%info%diagnostic_variables)

  ! initialise the export states and dependencies
  call pf%get_all_export_states()
  call pf%get_dependencies()

  end function mossco_create_fabm_pelagic

  
  !> initialize domain of pelagic fabm class
  subroutine initialize_domain(pf,inum,jnum,knum,dt)
  class(type_mossco_fabm_pelagic) :: pf
  integer  :: inum,jnum,knum,n
  real(rk) :: dt
  type(export_state_type), pointer :: export_state

  pf%fabm_ready=.false.
  pf%inum=inum
  pf%jnum=jnum
  pf%knum=knum

  ! Send information on spatial domain
  call fabm_set_domain(pf%model,inum,jnum,knum)

! Note (KK): Why don't we provide the totalDomain to FABM and mask the
!            HALO zones with fabm_set_mask???
  !call fabm_set_mask(pf%model,...)

  ! Allocate array for photosynthetically active radiation (PAR).
  allocate(pf%par(1:inum,1:jnum,1:knum))
  call fabm_link_bulk_data(pf%model,standard_variables%downwelling_photosynthetic_radiative_flux,pf%par)

  do n=1,size(pf%export_states)
    export_state => pf%export_states(n)
    !! memory handling should be shifted to component, which has total grid layout
    if (.not.associated(export_state%ws)) then
      allocate(export_state%ws(pf%inum,pf%jnum,pf%knum))
      export_state%ws = 0.0d0
    end if
  end do

  end subroutine initialize_domain

  subroutine initialize_concentrations(pf)
    class(type_mossco_fabm_pelagic) :: pf
    integer :: n
    do n=1,pf%nvar
      pf%conc(:,:,:,n) = pf%model%info%state_variables(n)%initial_value
    end do
  end subroutine


  subroutine update_pointers(pf)
    class(type_mossco_fabm_pelagic) :: pf
    integer :: n
    do n=1,pf%nvar
      call fabm_link_bulk_state_data(pf%model,n,pf%conc(RANGE3D,n))
    end do
    call fabm_link_bulk_data(pf%model,standard_variables%downwelling_photosynthetic_radiative_flux,pf%par)
  end subroutine



  subroutine check_ready(pf)
    class(type_mossco_fabm_pelagic) :: pf
    integer  :: i,j,k
    real(rk) :: rhs(1:pf%nvar),bottom_flux(1:0)

    if (.not.pf%fabm_ready) then
      call fabm_check_ready(pf%model)
      pf%fabm_ready = .true.
    end if
    !> check for I_0 and zi to be associated
    if (.not.associated(pf%zi)) then
      write(0,*) 'layer interface depth not linked yet.'
      stop
    else
      if (.not.associated(pf%layer_height)) then
        write(0,*) 'allocate layer height'
        allocate(pf%layer_height(pf%inum,pf%jnum,pf%knum))
        call pf%update_grid()
      end if
    end if
    if (.not.associated(pf%I_0)) then
      write(0,*) 'surface light not linked yet.'
      stop
    end if
    !> call fabm_do to fill diagnostic variables and pre-fetch data
    do i=1,pf%inum
      do j=1,pf%jnum
        call fabm_do_surface(pf%model,1,1,pf%knum,rhs(:))
        call fabm_do_bottom(pf%model,1,1,1,rhs(:),bottom_flux(:))
        rhs=0.0_rk
        do k=1,pf%knum
          call fabm_do(pf%model,1,1,k,rhs(:))
        end do
      end do
    end do
  end subroutine check_ready


  subroutine update_grid(pf)
    class(type_mossco_fabm_pelagic) :: pf
    integer  :: i,j,k

    do k=1,pf%knum
      pf%layer_height(RANGE2D,k) = pf%zi(RANGE2D,k) - pf%zi(RANGE2D,k-1)
    end do
  end subroutine


  subroutine get_rhs(rhs_driver,rhs)
  use fabm
  use fabm_types
  implicit none

  class(type_mossco_fabm_pelagic),intent(inout)     :: rhs_driver
  real(rk),intent(inout),dimension(:,:,:,:),pointer :: rhs
  integer :: n,i,j,k

  rhs=0.0_rk
  !   link state variables
#define RHSRANGE3D 1:rhs_driver%inum,1:rhs_driver%jnum,1:rhs_driver%knum
  do n=1,size(rhs_driver%model%info%state_variables)
    call fabm_link_bulk_state_data(rhs_driver%model,n,rhs_driver%conc(RHSRANGE3D,n))
  end do

  do k=1,rhs_driver%knum
    do j=1,rhs_driver%jnum
      do i=1,rhs_driver%inum
         call fabm_do(rhs_driver%model,i,j,k,rhs(i,j,k,:))
      end do
    end do
  end do

  end subroutine get_rhs

  !> append external bulk dependency
  subroutine add_bulk_dependency(deps, var)
  type(type_bulk_standard_variable),dimension(:), pointer :: deps
  type(type_bulk_standard_variable),dimension(:), pointer :: deps_tmp
  type(type_standard_variable),target                     :: var
  integer :: n_bulk  

  if (associated(deps)) then
    n_bulk = size(deps)
    allocate(deps_tmp(n_bulk))
    deps_tmp = deps
    deallocate(deps)
    allocate(deps(n_bulk+1))
    deps(1:n_bulk)=deps_tmp
    deallocate(deps_tmp)
  else
    n_bulk=0
    allocate(deps(1))
  end if

  deps(n_bulk+1)%units = var%units
  deps(n_bulk+1)%name = var%name

  end subroutine

  !> append external horizontal dependency
  subroutine add_horizontal_dependency(deps, var)
  type(type_horizontal_standard_variable),dimension(:), pointer :: deps
  type(type_horizontal_standard_variable),dimension(:), pointer :: deps_tmp
  type(type_standard_variable),target                           :: var
  integer :: n_hor  

  if (associated(deps)) then
    n_hor = size(deps)
    allocate(deps_tmp(n_hor))
    deps_tmp = deps
    deallocate(deps)
    allocate(deps(n_hor+1))
    deps(1:n_hor)=deps_tmp
    deallocate(deps_tmp)
  else
    n_hor=0
    allocate(deps(1))
  end if

  deps(n_hor+1)%units = var%units
  deps(n_hor+1)%name = var%name

  end subroutine


  !> get list of external dependencies
  subroutine get_dependencies(pf)
  class(type_mossco_fabm_pelagic) :: pf
  type(type_link), pointer        :: link

  ! get number of external dependencies in FABM,
  ! allocate list of dependencies names (here: required by the driver)
  nullify(pf%horizontal_dependencies)
  nullify(pf%bulk_dependencies)
  allocate(pf%horizontal_dependencies(1))
  pf%horizontal_dependencies(1)=standard_variables%surface_downwelling_photosynthetic_radiative_flux
  
  !> add required dependencies
  link => pf%model%links_postcoupling%first
  do while (associated(link))
    if (.not.link%target%read_indices%is_empty().and.link%target%state_indices%is_empty()) then
      select case (link%target%domain)
        case (domain_bulk)
          if (.not.associated(pf%model%environment%data(link%target%read_indices%pointers(1)%p)%p) &
              .and..not.(link%target%presence==presence_internal) &
              .and.associated(link%target%standard_variable)) then
            call add_bulk_dependency(pf%bulk_dependencies,link%target%standard_variable)
          end if
  case (domain_horizontal,domain_bottom,domain_surface)
          if (.not.associated(pf%model%environment%data_hz(link%target%read_indices%pointers(1)%p)%p) &
              .and..not.(link%target%presence==presence_internal) &
              .and.associated(link%target%standard_variable)) then
            call add_horizontal_dependency(pf%horizontal_dependencies,link%target%standard_variable)
          end if
        case (domain_scalar)
          if (.not.associated(pf%model%environment%data_scalar(link%target%read_indices%pointers(1)%p)%p) &
              .and..not.(link%target%presence==presence_internal) &
              .and.associated(link%target%standard_variable)) then
            write(0,*) 'global dependencies not implemented yet'
          end if
      end select
    end if
    link => link%next
  end do

  end subroutine


  !> set environment forcing for FABM
  subroutine set_environment(pf,varname,ptr_bulk,ptr_horizontal,ptr_scalar)
  class(type_mossco_fabm_pelagic) :: pf

  character(len=*), intent(in)                  :: varname
  real(rk), dimension(:,:,:), optional, pointer :: ptr_bulk
  real(rk), dimension(:,:), optional, pointer   :: ptr_horizontal
  real(rk), optional, pointer                   :: ptr_scalar

  type (type_bulk_variable_id)            :: bulk_id
  type (type_horizontal_variable_id)      :: horizontal_id
  type (type_scalar_variable_id)          :: scalar_id

  ! read name of import state and get FABM's id
  if (present(ptr_bulk)) then
    bulk_id = fabm_get_bulk_variable_id(pf%model,varname)
    ! link data if variable is used
    if (fabm_is_variable_used(bulk_id)) call fabm_link_bulk_data(pf%model,bulk_id,ptr_bulk(RANGE3D))
    if (varname == 'cell_thickness') pf%layer_height => ptr_bulk(RANGE3D)

  else if (present(ptr_horizontal)) then
    horizontal_id = fabm_get_horizontal_variable_id(pf%model,varname)
    ! link data if variable is used
    if (fabm_is_variable_used(horizontal_id)) call fabm_link_horizontal_data(pf%model,horizontal_id,ptr_horizontal(RANGE2D))
    ! keep link to necessary surface radiation
    if (varname == 'surface_downwelling_photosynthetic_radiative_flux') pf%I_0 => ptr_horizontal

  else if (present(ptr_scalar)) then
    scalar_id = fabm_get_scalar_variable_id(pf%model,varname)
    ! link data if variable is used
    if (fabm_is_variable_used(scalar_id)) call fabm_link_scalar_data(pf%model,scalar_id,ptr_scalar)

  end if

  end subroutine set_environment


  !> Initializes a pelagic FABM export state by FABM state_variable id
  function get_export_state_by_id(pf,fabm_id) result(export_state)
  class(type_mossco_fabm_pelagic) :: pf   
  type(export_state_type) :: export_state
  integer, intent(in)     :: fabm_id
  integer                 :: n

  export_state%fabm_id=fabm_id
  export_state%conc => null()
  export_state%ws => null()
!  !! memory handling should be shifted to component, which has total grid layout
!  export_state%conc => pf%conc(:,:,:,export_state%fabm_id)
!  allocate(export_state%ws(pf%inum,pf%jnum,pf%knum))
!  export_state%ws = 0.0d0
  !> first check for present standard name
  if (pf%model%info%state_variables(fabm_id)%standard_variable%name/='') then
    export_state%standard_name = &
      trim(pf%model%info%state_variables(fabm_id)%standard_variable%name)
    export_state%units = &
      trim(pf%model%info%state_variables(fabm_id)%standard_variable%units)
  else
  !> otherwise use CF-ed version of long_name
    export_state%standard_name = only_var_name( &
          pf%model%info%state_variables(fabm_id)%long_name)
    export_state%units = pf%model%info%state_variables(fabm_id)%units
  end if
  end function get_export_state_by_id

!> Initializes a pelagic FABM export state by FABM variable name

   function get_export_state_from_variable_name(pf,varname) result(export_state)
   class(type_mossco_fabm_pelagic) :: pf
   type(export_state_type)         :: export_state
   character(len=256), intent(in)  :: varname
   integer                         :: n,fabm_id

   fabm_id=-1
   do n=1,size(pf%model%info%state_variables)
     if (trim(pf%model%info%state_variables(n)%name).eq.trim(varname)) &
         fabm_id=n
   end do
   do n=1,size(pf%model%info%state_variables_ben)
     if (trim(pf%model%info%state_variables_ben(n)%name).eq.trim(varname)) &
         fabm_id=n
   end do
   export_state = pf%get_export_state_by_id(fabm_id)
   end function get_export_state_from_variable_name


  !> create a list of export states from FABM state_variables
  subroutine get_all_export_states(pf)
  class(type_mossco_fabm_pelagic) :: pf
  integer  :: n,fabm_id

  allocate(pf%export_states(pf%nvar))
  do fabm_id=1,pf%nvar
    pf%export_states(fabm_id) = pf%get_export_state_by_id(fabm_id)
  end do
  !> @todo: add benthic state variables
  end subroutine get_all_export_states

  !> update pelagic FABM export states pointers and sinking velocities using a list of export states

  subroutine update_export_states(pf,update_sinking)
  class(type_mossco_fabm_pelagic) :: pf
  real(rk),allocatable :: wstmp(:,:,:,:)
  type(export_state_type),pointer :: export_state
  integer :: n,i,j,k
  logical,optional :: update_sinking
  logical :: update_sinking_eff

  allocate(wstmp(pf%inum,pf%jnum,pf%knum,pf%nvar))
  wstmp=0.0_rk
  update_sinking_eff=.true.
  if (present(update_sinking)) update_sinking_eff=update_sinking
  if (update_sinking_eff) then
    do i=1,pf%inum
      do j=1,pf%jnum
        do k=1,pf%knum
          call fabm_get_vertical_movement(pf%model,i,j,k,wstmp(i,j,k,:))
        end do
      end do
    end do
  end if
  do n=1,size(pf%export_states)
    export_state => pf%export_states(n)
    export_state%conc => pf%conc(:,:,:,export_state%fabm_id)
    export_state%ws(RANGE3D) = wstmp(:,:,:,export_state%fabm_id)
  end do
  deallocate(wstmp)
  !> @todo add benthic state variables
  end subroutine update_export_states

  !> fabm_pelagic_diagnostic_variables
  !!
  !! The function returns a pointer to the 3d diagnostic variables.
  !! So far, only bulk diagnostic variables are supported. The function is a
  !! wrapper of the related FABM function.

  function diagnostic_variables(pf,n) result(diag)
  implicit none

  class(type_mossco_fabm_pelagic)    :: pf
  integer,intent(in)                 :: n
  real(rk),dimension(:,:,:),pointer  :: diag

  diag => fabm_get_bulk_diagnostic_data(pf%model,n)
  end function diagnostic_variables


   !> Calculate photosynthetically active radiation (PAR) and short wave
   !! radiation (SWR) over entire column, using surface short wave radiation,
   !! and background and biotic extinction.
   subroutine light(pf)

   class(type_mossco_fabm_pelagic)     :: pf
   integer  :: i,j,k
   real(rk) :: bioext(1:pf%inum,1:pf%jnum,1:pf%knum),localext

   bioext = 0.0_rk

   do i=1,pf%inum
     do j=1,pf%jnum
       do k=pf%knum,2,-1
         call fabm_get_light_extinction(pf%model,i,j,k,localext)

         ! Add the extinction of the first half of the grid box.
         bioext(i,j,k) = bioext(i,j,k) + &
           (localext+pf%background_extinction)*0.5_rk*pf%layer_height(i,j,k)

         ! Add the extinction of the second half of the grid box.
         bioext(i,j,k-1) = bioext(i,j,k) + &
           (localext+pf%background_extinction)*0.5_rk*pf%layer_height(i,j,k)
       end do
       ! Add te extinction of the upper, last layer
       call fabm_get_light_extinction(pf%model,i,j,1,localext)
       bioext(i,j,1) = bioext(i,j,1) + &
         (localext+pf%background_extinction)*0.5_rk*pf%layer_height(i,j,1)

       pf%par(i,j,:) = pf%I_0(i,j) * exp(-bioext(i,j,:))
     end do
   end do

   end subroutine light


  end module mossco_fabm_pelagic

