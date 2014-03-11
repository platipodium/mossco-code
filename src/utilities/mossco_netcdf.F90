module mossco_netcdf

  use mossco_variable_types, only: mossco_variableInfo
  use esmf

  type, extends(mossco_variableInfo), public :: type_mossco_netcdf_variable
    integer               :: varid
    integer               :: ncid
    integer               :: rank
    integer, allocatable  :: dimids(:), dimlens(:)
    contains
    procedure :: put => mossco_netcdf_variable_put
    procedure :: create => mossco_netcdf_variable_create
  end type type_mossco_netcdf_variable

  type type_mossco_netcdf
    integer      :: ncid
    type(type_mossco_netcdf_variable), allocatable, dimension(:) :: variables
    contains
    procedure :: open => mossco_netcdf_open
    procedure :: create => mossco_netcdf_create
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

  subroutine mossco_netcdf_close(self)
  class(type_mossco_netcdf) :: self
  end subroutine mossco_netcdf_close

  subroutine mossco_netcdf_open(self)
  class(type_mossco_netcdf) :: self
  end subroutine mossco_netcdf_open

  subroutine mossco_netcdf_create(self)
  class(type_mossco_netcdf) :: self
  end subroutine mossco_netcdf_create

  subroutine mossco_netcdf_create_dimensions_from_grid(self,grid)
  class(type_mossco_netcdf) :: self
  type(ESMF_Grid)           :: grid
  end subroutine mossco_netcdf_create_dimensions_from_grid

#if 0
  subroutine mossco_netcdf_variable_put(self,seconds,field)
  class(type_mossco_netcdf_variable) :: self
  type(ESMF_Field), intent(in)   :: field
  real(ESMF_KIND_R8), intent(in) :: seconds

    ncStatus = nf90_inq_varid(ncid, 'time', dimvarid)
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
    ! get length of time dimension

    if (dimlens(udimid)==0) then
      ncStatus = nf90_put_var(ncid, dimvarid, (/seconds/), start=(/1/), count=(/1/))
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
      !write(*,*) ncid, varid, seconds
    else
      ncStatus = nf90_get_var(ncid, dimvarid, time, start=(/dimlens(udimid)/))
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
   
      if (time<seconds) then
        !! append data
        ncStatus = nf90_put_var(ncid, dimvarid, seconds, start=(/dimlens(udimid)+1/))!, count=(/1/))
        if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
      elseif (time>seconds) then
        write(message,'(A)') 'Not implemented: inserting time'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      endif
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
    endif

    ncStatus = nf90_inquire_dimension(ncid, dimids(4), len=dimlens(udimid) )
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

    ncStatus = nf90_inquire_variable(ncid, varid, ndims=nDims, natts=nAtts, dimids=dimids)
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

    ncStatus = nf90_put_var(ncid, varid, farrayPtr3, start=(/1,1,1,dimlens(1)/))
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

  end subroutine
#endif

end module
