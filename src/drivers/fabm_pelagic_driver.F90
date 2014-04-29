!> @file fabm0d_driver.F90
!> @brief 3D generic driver for the Framework for Aquatic Biogeochemical Models (FABM)
!!
!! @author Richard Hofmeister

  module mossco_fabm_pelagic

  use solver_library
  use mossco_strings
  use fabm
  use fabm_types
  implicit none
  private

  public mossco_create_fabm_pelagic

  type, extends(type_rhs_driver), public :: type_mossco_fabm_pelagic
    type(type_model),pointer           :: model
    real(rk),dimension(:,:,:),pointer  :: temp,salt,par,dens,current_depth
    real(rk),dimension(:,:),pointer    :: wind_sf,taub,par_sf
    real(rk)                           :: decimal_yearday
    integer                            :: ndiag
    contains
    procedure :: get_rhs
    procedure :: set_environment
    procedure :: get_export_state_by_id
    procedure :: get_all_export_states
    procedure :: update_export_states
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
  function mossco_create_fabm_pelagic(inum,jnum,knum,dt) result(pf)
  integer  :: inum,jnum,knum
  integer  :: namlst=123
  real(rk) :: dt
  type(type_mossco_fabm_pelagic), allocatable :: pf

  allocate(pf)
  pf%inum=inum
  pf%jnum=jnum
  pf%knum=knum
  ! Build FABM model tree.
  pf%model => fabm_create_model_from_file(namlst)

  ! Send information on spatial domain
  call fabm_set_domain(pf%model,inum,jnum,knum)

  ! allocate memory
  pf%nvar = size(pf%model%info%state_variables)
  pf%ndiag = size(pf%model%info%diagnostic_variables)
  allocate(pf%conc(1:inum,1:jnum,1:knum,1:pf%nvar))

  end function mossco_create_fabm_pelagic


  subroutine get_rhs(rhs_driver,rhs)
  use fabm
  use fabm_types
  implicit none

  class(type_mossco_fabm_pelagic),intent(inout)     :: rhs_driver
  real(rk),intent(inout),dimension(:,:,:,:),pointer :: rhs
  integer :: n,i,j,k

  rhs=0.0_rk
  !   link state variables
  do n=1,size(rhs_driver%model%info%state_variables)
    call fabm_link_bulk_state_data(rhs_driver%model,n,rhs_driver%conc(:,:,:,n))
  end do

  do k=1,rhs_driver%knum
    do j=1,rhs_driver%jnum
      do i=1,rhs_driver%inum
         call fabm_do(rhs_driver%model,i,j,k,rhs(i,j,k,:))
      end do
    end do
  end do

  end subroutine get_rhs

  !> set environment forcing for FABM
  subroutine set_environment(pf)
  class(type_mossco_fabm_pelagic) :: pf

  end subroutine set_environment


  !> Initializes a pelagic FABM export state by FABM state_variable id
  function get_export_state_by_id(pf,fabm_id) result(export_state)
  class(type_mossco_fabm_pelagic) :: pf   
  type(export_state_type) :: export_state
  integer, intent(in)     :: fabm_id
  integer                 :: n

  export_state%fabm_id=fabm_id
  export_state%conc => pf%conc(:,:,:,export_state%fabm_id)
  allocate(export_state%ws(pf%inum,pf%jnum,pf%knum))
  export_state%ws = 0.0d0
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
  subroutine get_all_export_states(pf,export_states)
  class(type_mossco_fabm_pelagic) :: pf
  type(export_state_type),dimension(:),allocatable :: export_states
  integer  :: n,fabm_id

  allocate(export_states(pf%nvar))
  do fabm_id=1,pf%nvar
    export_states(fabm_id) = pf%get_export_state_by_id(fabm_id)
  end do
  !> @todo: add benthic state variables
  end subroutine get_all_export_states

  !> update pelagic FABM export states pointers and sinking velocities using a list of export states

  subroutine update_export_states(pf,export_states)
  class(type_mossco_fabm_pelagic) :: pf
  type(export_state_type), target :: export_states(:)
  real(rk),allocatable :: wstmp(:,:,:,:)
  type(export_state_type),pointer :: export_state
  integer :: n,i,j,k

  allocate(wstmp(pf%inum,pf%jnum,pf%knum,pf%nvar))
  do i=1,pf%inum
    do j=1,pf%jnum
      do k=1,pf%knum
        call fabm_get_vertical_movement(pf%model,i,j,k,wstmp(i,j,k,:))
      end do
    end do
  end do
  do n=1,size(export_states)
    export_state => export_states(n)
    export_state%conc => pf%conc(:,:,:,export_state%fabm_id)
    export_state%ws = wstmp(:,:,:,export_state%fabm_id)
  end do
  deallocate(wstmp)
  !> @todo add benthic state variables
  end subroutine update_export_states

  end module mossco_fabm_pelagic

