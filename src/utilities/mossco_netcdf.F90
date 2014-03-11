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
    type(type_mossco_netcdf_variable), allocatable, dimension(:) :: variables
    contains
    procedure :: close => mossco_netcdf_close
    procedure :: add_timestep => mossco_netcdf_add_timestep
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
  if (present(rc)) rc=ncStatus
  end function mossco_netcdfOpen


  function mossco_netcdfCreate(filename,rc) result(nc)
  character(len=ESMF_MAXSTR)    :: filename
  type(type_mossco_netcdf)      :: nc
  integer, intent(out),optional :: rc
  integer                       :: ncStatus
  ncStatus = nf90_create(trim(filename), NF90_CLOBBER, nc%ncid)
  if (present(rc)) rc=ncStatus
  end function mossco_netcdfCreate


  subroutine mossco_netcdf_create_dimensions_from_grid(self,grid)
  class(type_mossco_netcdf) :: self
  type(ESMF_Grid)           :: grid
  end subroutine mossco_netcdf_create_dimensions_from_grid


end module
