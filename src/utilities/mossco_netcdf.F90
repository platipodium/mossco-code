module mossco_netcdf

  use mossco_variable_types, only: mossco_variableInfo
  use mossco_strings 
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
  end type type_mossco_netcdf_variable

  type, public :: type_mossco_netcdf
    integer      :: ncid
    integer      :: timeDimId
    type(type_mossco_netcdf_variable), pointer, dimension(:) :: variables
    contains
    procedure :: close => mossco_netcdf_close
    procedure :: add_timestep => mossco_netcdf_add_timestep
    procedure :: grid_dimensions => mossco_netcdf_grid_dimensions
    procedure :: init_time => mossco_netcdf_init_time
    procedure :: update_variables => mossco_netcdf_update_variables
    procedure :: create_variable => mossco_netcdf_variable_create
    procedure :: variable_present => mossco_netcdf_variable_present
  end type type_mossco_netcdf

  integer, parameter :: MOSSCO_NC_ERROR=-1
  integer, parameter :: MOSSCO_NC_NOERR=0
  integer, parameter :: MOSSCO_NC_EXISTING=1

  contains

  subroutine mossco_netcdf_variable_put(self,seconds,field)
  class(type_mossco_netcdf_variable) :: self
  type(ESMF_Field), intent(in)   :: field
  real(ESMF_KIND_R8), intent(in) :: seconds
  end subroutine mossco_netcdf_variable_put


  function mossco_netcdf_variable_present(self,name) result(varpresent)
  class(type_mossco_netcdf)          :: self
  character(len=*)                   :: name
  logical                            :: varpresent
  integer                            :: ncStatus,varid
  varpresent = .false.
  ncStatus = nf90_inq_varid(self%ncid,name,varid)
  if (ncStatus == NF90_NOERR) varpresent=.true.
  end function mossco_netcdf_variable_present


  subroutine mossco_netcdf_variable_create(self,field,name,rc)
  class(type_mossco_netcdf)      :: self
  type(ESMF_Field), intent(in)   :: field
  type(ESMF_Grid)                :: grid
  character(len=*),optional      :: name
  character(len=ESMF_MAXSTR)     :: varname,gridname,fieldname,coordinates=''
  integer                        :: ncStatus,esmfrc,rc_,varid,dimcheck=0
  integer                        :: dimids_1d(2),dimids_2d(3),dimids_3d(4),rank
  integer, dimension(:),pointer  :: dimids => null()
  integer, optional              :: rc

  call ESMF_FieldGet(field,name=fieldname,rc=esmfrc)
  varname = trim(fieldname)
  if (present(name)) varname=trim(name)

  if (.not.self%variable_present(varname)) then
    call ESMF_FieldGet(field,grid=grid,rc=esmfrc)
    call ESMF_GridGet(grid,name=gridname,rc=esmfrc)

    call replace_character(gridname, ' ', '_')
    dimids => self%grid_dimensions(grid)

    write(0,*) trim(gridname), trim(varname), dimids
    select case(ubound(dimids,1))
    case(2)
      coordinates='time '//trim(gridname)//'_x '
    case(3)
      coordinates='time '//trim(gridname)//'_y '//trim(gridname)//'_x '
    case(4)
      coordinates='time '//trim(gridname)//'_z '//trim(gridname)//'_y '//trim(gridname)//'_x '
    end select

    !! define variable
    ncStatus = nf90_redef(self%ncid)
    ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_DOUBLE,dimids,varid)
    write(0,*) nf90_strerror(ncStatus)
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
    
    ncStatus = nf90_put_att(self%ncid,varid,'standard_name',fieldname)
    ncStatus = nf90_put_att(self%ncid,varid,'long_name',fieldname)
    ncStatus = nf90_put_att(self%ncid,varid,'coordinates',trim(coordinates))
    ncStatus = nf90_put_att(self%ncid,varid,'missing_value',-99._ESMF_KIND_R8)
    ncStatus = nf90_put_att(self%ncid,varid,'_FillValue',-99._ESMF_KIND_R8)
    !! @todo get unit from field attributes

    ncStatus = nf90_enddef(self%ncid)
  end if
  end subroutine mossco_netcdf_variable_create


  subroutine mossco_netcdf_add_timestep(self,seconds, rc)
  class(type_mossco_netcdf) :: self
  real(ESMF_KIND_R8), intent(in) :: seconds
  integer, optional              :: rc

  integer           :: ncStatus, dimlen, varid

  ncStatus = nf90_inq_varid(self%ncid, 'time', varid)
  !>@todo check for exsiting time
  ncStatus = nf90_inquire_dimension(self%ncid, self%timedimid, len=dimlen)

  ncStatus = nf90_put_var(self%ncid, varid, seconds, start=(/dimlen+1/))
  if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

  end subroutine mossco_netcdf_add_timestep


  subroutine mossco_netcdf_close(self,rc)
  class(type_mossco_netcdf)      :: self
  integer, optional, intent(out) :: rc
  integer                        :: ncStatus
  ncStatus = nf90_close(self%ncid)
  if (present(rc)) rc=ncStatus
  end subroutine mossco_netcdf_close


  function mossco_netcdfOpen(filename, timeUnit, rc) result(nc)
  character(len=ESMF_MAXSTR)    :: filename
  type(type_mossco_netcdf)      :: nc
  character(len=*),optional     :: timeUnit
  integer, intent(out),optional :: rc
  integer                       :: ncStatus
  ncStatus = nf90_open(trim(filename), mode=NF90_WRITE, ncid=nc%ncid)
  
  if (ncStatus /= NF90_NOERR) then
    if (present(timeUnit))  then
      nc = MOSSCO_NetcdfCreate(trim(filename), timeUnit=trim(timeUnit), rc = rc)
    else
      nc = MOSSCO_NetcdfCreate(trim(filename), rc = rc)
    endif 
  endif
  
  ncStatus = nf90_inq_dimid(nc%ncid,'time',nc%timeDimId)
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
  ncStatus = nf90_enddef(nc%ncid)

  end function mossco_netcdfCreate


  subroutine mossco_netcdf_init_time(self,timeUnit,rc)
  class(type_mossco_netcdf)      :: self
  character(len=ESMF_MAXSTR)     :: timeUnit
  integer, optional, intent(out) :: rc
  integer                        :: ncStatus,varid,rc_
  type(type_mossco_netcdf_variable), pointer :: var

  rc_=MOSSCO_NC_NOERR
  ncStatus = nf90_def_dim(self%ncid, 'time', NF90_UNLIMITED, self%timeDimId)
  if (ncStatus==NF90_ENAMEINUSE) rc_=MOSSCO_NC_EXISTING
  ncStatus = nf90_def_var(self%ncid, 'time', NF90_DOUBLE, self%timeDimId, varid)
  if (ncStatus==NF90_ENAMEINUSE) then
    rc_=MOSSCO_NC_EXISTING
  else
    ncStatus = nf90_put_att(self%ncid, varid, 'units', timeUnit)
  end if

  if (rc_ == MOSSCO_NC_NOERR) then
    allocate(self%variables(1))
    var => self%variables(1)
    var%name='time'
    var%unit=trim(timeUnit)
    var%rank=1
  end if
  if (present(rc)) rc=rc_
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


  function mossco_netcdf_grid_dimensions(self,grid) result(dimids)
  class(type_mossco_netcdf)     :: self
  type(ESMF_Grid)               :: grid
  integer                       :: ncStatus,rc_,esmfrc,dimcheck=0
  character(len=ESMF_MAXSTR)    :: gridName
  integer,allocatable           :: ubounds(:),lbounds(:)
  integer                       :: dimid,rank=3
  integer,pointer,dimension(:)  :: dimids

  rc_ = MOSSCO_NC_NOERR
  call ESMF_GridGet(grid,name=gridName,rank=rank,rc=esmfrc)

  call replace_character(gridname, ' ', '_')
  allocate(ubounds(rank))
  ubounds(:)=1
  allocate(lbounds(rank))
  lbounds(:)=1
  allocate(dimids(rank+1))
  dimids(:)=-1
  dimids(rank+1)=self%timeDimId

  call ESMF_GridGetFieldBounds(grid=grid, localDe=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, totalCount=ubounds, rc=esmfrc)

  ! get grid dimension-ids
  select case(rank)
  case(1)
      ncStatus = nf90_inq_dimid(self%ncid,trim(gridname)//'_x',dimids(1))
      if (ncStatus /= NF90_NOERR) dimcheck=-1
  case(2)
      ncStatus = nf90_inq_dimid(self%ncid,trim(gridname)//'_y',dimids(2))
      if (ncStatus /= NF90_NOERR) dimcheck=-1      
      ncStatus = nf90_inq_dimid(self%ncid,trim(gridname)//'_x',dimids(1))
      if (ncStatus /= NF90_NOERR) dimcheck=-1      
  case(3)
      ncStatus = nf90_inq_dimid(self%ncid,trim(gridname)//'_z',dimids(3))
      if (ncStatus /= NF90_NOERR) dimcheck=-1      
      ncStatus = nf90_inq_dimid(self%ncid,trim(gridname)//'_y',dimids(2))
      if (ncStatus /= NF90_NOERR) dimcheck=-1      
      ncStatus = nf90_inq_dimid(self%ncid,trim(gridname)//'_x',dimids(1))
      if (ncStatus /= NF90_NOERR) dimcheck=-1      
  case default
      call ESMF_LogWrite('create netcdf variable: only grid ranks 1,2,3 supported', ESMF_LOGMSG_INFO)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end select

  !! if grid not present, create grid
  if (dimcheck == -1) then
    ncStatus = nf90_redef(self%ncid)
    if (rank>0) then
      !! assume to have just one gridded dimension
      ncStatus = nf90_def_dim(self%ncid, trim(gridName)//'_x', &
          ubounds(1)-lbounds(1)+1,dimids(1))
      if (ncStatus==NF90_ENAMEINUSE) rc_=MOSSCO_NC_EXISTING
    end if
    if (rank>1) then
      !! assume to have horizontal grid
      ncStatus = nf90_def_dim(self%ncid, trim(gridName)//'_y', ubounds(2)-lbounds(2)+1,dimids(2))
      if (ncStatus==NF90_ENAMEINUSE) then
        rc_=MOSSCO_NC_EXISTING
      elseif (ncStatus==NF90_NOERR) then
        rc_=MOSSCO_NC_NOERR
      else
        rc_=MOSSCO_NC_ERROR
      end if
    end if
    if (rank>2) then
      !! assume to have 3d grid
      ncStatus = nf90_def_dim(self%ncid, trim(gridName)//'_z', ubounds(3)-lbounds(3)+1,dimids(3))
      if (ncStatus==NF90_ENAMEINUSE) then
        rc_=MOSSCO_NC_EXISTING
      elseif (ncStatus==NF90_NOERR) then
        rc_=MOSSCO_NC_NOERR
      else
        rc_=MOSSCO_NC_ERROR
      end if
    end if
    ncStatus = nf90_enddef(self%ncid)
  end if
  end function mossco_netcdf_grid_dimensions


end module
