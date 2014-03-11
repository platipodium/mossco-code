module mossco_netcdf

  use mossco_variable_types, only: mossco_variableInfo
  use esmf
  use netcdf

  public mossco_netcdfCreate,mossco_netcdfOpen

  type, extends(mossco_variableInfo), public :: type_mossco_netcdf_variable
    integer               :: varid
    integer               :: ncid
    integer               :: rank
    integer, allocatable  :: dimids(:), dimlens(:)
    contains
    procedure :: put => mossco_netcdf_variable_put
    procedure :: create => mossco_netcdf_variable_create
  end type type_mossco_netcdf_variable

  type, public :: type_mossco_netcdf
    integer      :: ncid
    integer      :: timeDimId
    type(type_mossco_netcdf_variable), pointer, dimension(:) :: variables
    contains
    procedure :: close => mossco_netcdf_close
    procedure :: add_timestep => mossco_netcdf_add_timestep
    procedure :: use_grid_dimensions => mossco_netcdf_use_grid_dimensions
    procedure :: init_time => mossco_netcdf_init_time
    procedure :: update_variables => mossco_netcdf_update_variables
  end type type_mossco_netcdf

  contains

  subroutine mossco_netcdf_variable_put(self,seconds,field)
  class(type_mossco_netcdf_variable) :: self
  type(ESMF_Field), intent(in)   :: field
  real(ESMF_KIND_R8), intent(in) :: seconds
  end subroutine mossco_netcdf_variable_put

  subroutine mossco_netcdf_variable_create(self,field)
  class(type_mossco_netcdf_variable) :: self
  type(ESMF_Field), intent(in)   :: field
  end subroutine mossco_netcdf_variable_create

  subroutine mossco_netcdf_add_timestep(self,seconds)
  class(type_mossco_netcdf) :: self
  real(ESMF_KIND_R8), intent(in) :: seconds
  end subroutine mossco_netcdf_add_timestep


  subroutine mossco_netcdf_close(self,rc)
  class(type_mossco_netcdf)      :: self
  integer, optional, intent(out) :: rc
  integer                        :: ncStatus
  ncStatus = nf90_close(self%ncid)
  if (present(rc)) rc=ncStatus
  end subroutine mossco_netcdf_close


  function mossco_netcdfOpen(filename,rc) result(nc)
  character(len=ESMF_MAXSTR)    :: filename
  type(type_mossco_netcdf)      :: nc
  integer, intent(out),optional :: rc
  integer                       :: ncStatus
  ncStatus = nf90_open(trim(filename), mode=NF90_WRITE, ncid=nc%ncid)
  call nc%update_variables()
  if (present(rc)) rc=ncStatus
  end function mossco_netcdfOpen


  function mossco_netcdfCreate(filename,timeUnit,rc) result(nc)
  character(len=ESMF_MAXSTR)    :: filename
  type(type_mossco_netcdf)      :: nc
  integer, intent(out),optional :: rc
  integer                       :: ncStatus
  character(len=*),optional     :: timeUnit
  ncStatus = nf90_create(trim(filename), NF90_CLOBBER, nc%ncid)
  if (present(rc)) rc=ncStatus
  if (present(timeUnit)) call nc%init_time(timeUnit)
  end function mossco_netcdfCreate


  subroutine mossco_netcdf_init_time(self,timeUnit,rc)
  class(type_mossco_netcdf)      :: self
  character(len=ESMF_MAXSTR)     :: timeUnit
  integer, optional, intent(out) :: rc
  integer                        :: ncStatus,varid
  type(type_mossco_netcdf_variable), pointer :: var

  ncStatus = nf90_def_dim(self%ncid, 'time', NF90_UNLIMITED, self%timeDimId)
  ncStatus = nf90_def_var(self%ncid, 'time', NF90_DOUBLE, self%timeDimId, varid)

  if (ncStatus /= NF90_NOERR) then
    allocate(self%variables(1))
    var => self%variables(1)
    var%name='time'
    var%unit=trim(timeUnit)
    var%rank=1
  end if
  end subroutine mossco_netcdf_init_time


  subroutine mossco_netcdf_update_variables(self)
  class(type_mossco_netcdf)      :: self
  integer                        :: ncStatus,i,nvars,natts
  integer                        :: nvardims,nvaratts
  type(type_mossco_netcdf_variable), pointer :: var

  ncStatus = nf90_inquire(self%ncid,nVariables=nvars,nAttributes=natts)
  allocate(self%variables(nvars))
  do i=1,nvars
    var => self%variables(i)
    var%varid = i
    ncStatus = nf90_inquire_variable(self%ncid,i,ndims=var%rank,natts=nvaratts)
    ncStatus = nf90_get_att(self%ncid,var%varid,'long_name',var%standard_name)
    ncStatus = nf90_get_att(self%ncid,var%varid,'units',var%unit)
  end do
  end subroutine mossco_netcdf_update_variables


  subroutine mossco_netcdf_use_grid_dimensions(self,grid,rc)
  class(type_mossco_netcdf)     :: self
  type(ESMF_Grid)               :: grid
  integer, intent(out),optional :: rc
  integer                       :: ncStatus

  end subroutine mossco_netcdf_use_grid_dimensions


end module
