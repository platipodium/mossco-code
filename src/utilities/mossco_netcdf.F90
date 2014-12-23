!> @brief Implementation ESMF/NetCDF utility functions
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2014, Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister
!> @author Carsten Lemmen

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
module mossco_netcdf

  use mossco_variable_types, only: mossco_variableInfo
  use mossco_strings
  use esmf
  use netcdf

  private

  public MOSSCO_NetcdfCreate, MOSSCO_NetcdfOpen

  type, extends(MOSSCO_VariableInfo), public :: type_mossco_netcdf_variable
    integer               :: varid
    integer               :: ncid
    integer               :: rank
    integer, allocatable  :: dimids(:), dimlens(:)
  end type type_mossco_netcdf_variable

  type, public :: type_mossco_netcdf
    integer      :: ncid
    integer      :: timeDimId
    type(type_mossco_netcdf_variable), pointer, dimension(:) :: variables
    contains
    procedure :: close => mossco_netcdf_close
    procedure :: add_timestep => mossco_netcdf_add_timestep
    procedure :: grid_dimensions => mossco_netcdf_grid_dimensions
    procedure :: mesh_dimensions => mossco_netcdf_mesh_dimensions
    procedure :: init_time => mossco_netcdf_init_time
    procedure :: update_variables => mossco_netcdf_update_variables
    procedure :: create_variable => mossco_netcdf_variable_create
    procedure :: variable_present => mossco_netcdf_variable_present
    procedure :: put_variable => mossco_netcdf_variable_put
    procedure :: create_coordinate =>mossco_netcdf_coordinate_create
    procedure :: create_mesh_coordinate =>mossco_netcdf_mesh_coordinate_create
  end type type_mossco_netcdf

  integer, parameter :: MOSSCO_NC_ERROR=-1
  integer, parameter :: MOSSCO_NC_NOERR=ESMF_SUCCESS
  integer, parameter :: MOSSCO_NC_EXISTING=1
#include "git-sha.h"

  contains

  subroutine mossco_netcdf_variable_put(self,field,seconds,name)

    implicit none
    class(type_mossco_netcdf)               :: self
    type(ESMF_Field), intent(in)            :: field
    real(ESMF_KIND_R8), intent(in),optional :: seconds
    character(len=*),optional               :: name

    integer                     :: ncStatus, varid, rc, esmfrc, rank
    integer                     :: nDims, nAtts, udimid, dimlen
    character(len=ESMF_MAXSTR)  :: varname, message, fmt

    integer(ESMF_KIND_I4), dimension(:), allocatable :: lbnd, ubnd, exclusiveCount
    integer(ESMF_KIND_I4)       :: localDeCount

    real(ESMF_KIND_R8), pointer, dimension(:,:,:,:)  :: farrayPtr4
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)    :: farrayPtr3
    real(ESMF_KIND_R8), pointer, dimension(:,:)      :: farrayPtr2
    real(ESMF_KIND_R8), pointer, dimension(:)        :: farrayPtr1

    call ESMF_FieldGet(field, name=varname, rank=rank, &
      localDeCount=localDeCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (localDeCount==0) return

    allocate(lbnd(rank))
    allocate(ubnd(rank))
    allocate(exclusiveCount(rank))
    call ESMF_FieldGetBounds(field, localDe=0, exclusiveLBound=lbnd, &
      exclusiveUBound=ubnd, exclusiveCount=exclusiveCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !write(fmt,'(A,I1,A,I1,A,I1,A)') '(A,I2.2,A,',rank, 'I2,A,', rank, 'I2,A,', rank, 'I2)'
    !write(message,fmt) 'localDeCount=', localDeCount,' bounds ',lbnd,' : ', &
    !  ubnd, ' exclusiveCount ', exclusiveCount
    !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, rc=rc)
    !call ESMF_LogFlush( rc=rc)

    if (any(exclusiveCount==0)) return

    if (present(name)) varname=trim(name)

    if (rank>4 .or. rank<1) then
       write(message,'(A)')  'Writing of fields with rank<1 or rank>4 not supported.'
       call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
       return
    endif

    !> If the variable does not exist, create it
    if (.not.self%variable_present(varname)) then
      call self%create_variable(field, trim(varname), rc=rc)
    endif
    !> @todo what happens if variable exists but on different grid?

    ncStatus = nf90_inq_varid(self%ncid, trim(varname), varid)
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

    ncStatus = nf90_inquire_variable(self%ncid, varid, ndims=nDims, natts=nAtts)
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

    if (rank /= nDims-1) then
       write(message,'(A)')  'Field rank and netcdf dimension count do not match'
       call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
       return
    endif

    ncStatus = nf90_inq_dimid(self%ncid, 'time', udimid)
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

    ncStatus = nf90_inquire_dimension(self%ncid, udimid, len=dimlen)
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

    if (rank==4) then
      call  ESMF_FieldGet(field, farrayPtr=farrayPtr4, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      ncStatus = nf90_put_var(self%ncid, varid, farrayPtr4(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3),lbnd(4):ubnd(4)), &
        start=(/1,1,1,1,dimlen/))
    elseif (rank==3) then
      call  ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      ncStatus = nf90_put_var(self%ncid, varid, farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)), &
        start=(/1,1,1,dimlen/))
    elseif (rank==2) then
      call  ESMF_FieldGet(field, farrayPtr=farrayPtr2, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      ncStatus = nf90_put_var(self%ncid, varid, farrayPtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)), &
        start=(/1,1,dimlen/))
    elseif (rank==1) then
      call  ESMF_FieldGet(field, farrayPtr=farrayPtr1, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      ncStatus = nf90_put_var(self%ncid, varid, farrayPtr1(lbnd(1):ubnd(1)), &
        start=(/1,dimlen/))
    endif
    if (ncStatus /= NF90_NOERR) call &
      ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(lbnd)) deallocate(lbnd)
    if (allocated(exclusiveCount)) deallocate(exclusiveCount)

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
    type(ESMF_Mesh)                :: mesh
    character(len=*),optional      :: name
    character(len=ESMF_MAXSTR)     :: varname,geomName,fieldname,coordinates=''
    character(len=ESMF_MAXSTR)     :: units='', attributeName, string, message
    integer                        :: ncStatus,esmfrc,rc_,varid,dimcheck=0
    integer                        :: dimids_1d(2),dimids_2d(3),dimids_3d(4),rank
    integer, dimension(:),pointer  :: dimids
    integer, optional              :: rc
    character(len=1), dimension(3) :: coordNames = (/'x','y','z'/)
    integer                        :: external_index=-1
    real(ESMF_KIND_R8)             :: mean_diameter, real8
    real(ESMF_KIND_R4)             :: real4
    integer(ESMF_KIND_I4)          :: i, attributeCount, int4
    integer(ESMF_KIND_I8)          :: int8
    type(ESMF_TypeKind_Flag)       :: typekind
    type(ESMF_GeomType_Flag)       :: geomType


    call ESMF_FieldGet(field,name=fieldname,rc=esmfrc)
    if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    varname = trim(fieldname)
    if (present(name)) varname=trim(name)

    if (.not.self%variable_present(varname)) then

      call ESMF_FieldGet(field,geomType=geomType,rc=esmfrc)
      if (geomType==ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(field,grid=grid,rc=esmfrc)
        if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        call ESMF_GridGet(grid,name=geomName,rc=esmfrc)
        if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        dimids => self%grid_dimensions(grid)
      elseif (geomType==ESMF_GEOMTYPE_MESH) then
        !call ESMF_FieldGet(field,mesh=mesh,rc=esmfrc)
        !if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        write(geomname,'(A)') 'mesh'
        dimids => self%mesh_dimensions(field)
        !write(0,*) 'mesh write, dimids:',dimids
        !write(message,'(A)')  'Geometry type MESH cannot be handled sufficiently yet'
        !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        !return
      elseif (geomType==ESMF_GEOMTYPE_LOCSTREAM) then
        write(message,'(A)')  'Geometry type LOCSTREAM cannot be handled yet'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        return
      elseif (geomType==ESMF_GEOMTYPE_XGRID) then
        write(message,'(A)')  'Geometry type XGRID cannot be handled yet'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        return
      endif

      call replace_character(geomName, ' ', '_')

      if (ubound(dimids,1)>1) then
        write(coordinates,'(A)') trim(geomName)//'_'//coordnames(ubound(dimids,1)-1)
        do i=ubound(dimids,1)-2,1,-1
          write(coordinates,'(A)') trim(coordinates)//' '//trim(geomName)//'_'//coordnames(i)
        enddo
      endif

      !! define variable
      ncStatus = nf90_redef(self%ncid)
      ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_DOUBLE,dimids,varid)
      if (ncStatus /= NF90_NOERR) call &
        ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

      ncStatus = nf90_put_att(self%ncid,varid,'standard_name',fieldname)
      ncStatus = nf90_put_att(self%ncid,varid,'long_name',fieldname)
      ncStatus = nf90_put_att(self%ncid,varid,'coordinates',trim(coordinates))
      ncStatus = nf90_put_att(self%ncid,varid,'missing_value',-99._ESMF_KIND_R8)
      ncStatus = nf90_put_att(self%ncid,varid,'_FillValue',-99._ESMF_KIND_R8)

      !@todo this paragraph has to be deleted once the next one (more generic) is working
      ! write external index, that is used to e.g. communicate FABM variable index
      call ESMF_AttributeGet(field,'external_index',external_index,defaultvalue=-1,rc=rc)
      if (external_index > -1) &
        ncStatus = nf90_put_att(self%ncid,varid,'external_index',external_index)
      call ESMF_AttributeGet(field,'mean_particle_diameter',mean_diameter,defaultvalue=-1.0d0,rc=rc)
      if (mean_diameter > 0.0d0) &
        ncStatus = nf90_put_att(self%ncid,varid,'mean_particle_diameter',mean_diameter)
      call ESMF_AttributeGet(field,'units',units,defaultvalue='',rc=rc)
      ncStatus = nf90_put_att(self%ncid,varid,'units',trim(units))

      call ESMF_AttributeGet(field, count=attributeCount, rc=rc)
      do i=1, attributeCount
         call ESMF_AttributeGet(field, attributeIndex=i, name=attributeName, &
           typekind=typekind, rc=rc)
         !write(0,*) name, attributeCount, i, attributeName, typekind
         if (typekind==ESMF_TYPEKIND_I4) then
           call ESMF_AttributeGet(field, attributeName, int4, rc=rc)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),int4)
         elseif (typekind==ESMF_TYPEKIND_I8) then
           call ESMF_AttributeGet(field, attributeName, int8, rc=rc)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),int8)
         elseif (typekind==ESMF_TYPEKIND_R4) then
           call ESMF_AttributeGet(field, attributeName, real4, rc=rc)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),real4)
         elseif (typekind==ESMF_TYPEKIND_R8) then
           call ESMF_AttributeGet(field, attributeName, real8, rc=rc)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),real8)
         else
           call ESMF_AttributeGet(field, attributeName, string, rc=rc)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),trim(string))
         endif
      enddo

      ncStatus = nf90_enddef(self%ncid)
    end if

    if (present(rc)) rc = ESMF_SUCCESS

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
  character(len=*)              :: filename
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

    use iso_fortran_env
    implicit none

    character(len=*)              :: filename
    type(type_mossco_netcdf)      :: nc
    integer, intent(out),optional :: rc
    integer                       :: ncStatus
    character(len=*),optional     :: timeUnit

    character(len=255)            :: string

    ncStatus = nf90_create(trim(filename), NF90_CLOBBER, nc%ncid)
    if (present(rc)) rc=ncStatus
    if (present(timeUnit)) call nc%init_time(timeUnit)
    ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'mossco_sha_key',MOSSCO_GIT_SHA_KEY)

#ifndef NO_ISO_FORTRAN_ENV
    !> @todo check cross-platform compatibility of the iso_fortran_env calls
    ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'compile_compiler_version',compiler_version())
    ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'compile_compiler_options',compiler_options())
#endif
    call get_command(string)
    ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'run_command_line',trim(string))
    call getcwd(string)
    ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'run_working_directory',trim(string))
#ifndef NO_ISO_FORTRAN_ENV
    ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'run_process_id',getpid())
#endif
    !> @todo check cross-platform compatibility of these gnu extensions
    call getlog(string)
#ifndef NO_ISO_FORTRAN_ENV
    write(string,'(A,I5,A,I5,A)') trim(string)// '(id=',getuid(),', gid=',getgid(),')'
#endif
    ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'run_user',trim(string))
    call hostnm(string)
    ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'run_hostname',trim(string))

    ncStatus = nf90_enddef(nc%ncid)

  end function mossco_netcdfCreate


  subroutine mossco_netcdf_init_time(self,timeUnit,rc)
    class(type_mossco_netcdf)      :: self
    character(len=*)               :: timeUnit
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
    var%units=trim(timeUnit)
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
      ncStatus = nf90_get_att(self%ncid,var%varid,'units',var%units)
    end do
  end subroutine mossco_netcdf_update_variables

  function mossco_netcdf_mesh_dimensions(self,field) result(dimids)

    implicit none

    class(type_mossco_netcdf)     :: self
    type(ESMF_Field)              :: field
    type(ESMF_Mesh)               :: mesh
    integer                       :: ncStatus,rc_,esmfrc,dimcheck
    character(len=ESMF_MAXSTR)    :: geomName, name
    integer,allocatable           :: totalubound(:),totallbound(:),gridToFieldMap(:)
    integer,allocatable           :: ungriddedubound(:),ungriddedlbound(:)
    integer,pointer,dimension(:)  :: dimids

    integer(ESMF_KIND_I4)         :: dimCount, dimid, rank, i
    character(len=ESMF_MAXSTR)    :: message
    integer(ESMF_KIND_I4)         :: parametricDim, spatialDim, numOwnedElements

    rc_ = MOSSCO_NC_NOERR
    dimcheck=0
    call ESMF_FieldGet(field,mesh=mesh,dimCount=dimCount,rank=rank,rc=esmfrc)
    if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_MeshGet(mesh,parametricDim=parametricDim, &
      spatialDim=spatialDim,numOwnedElements=numOwnedElements,rc=esmfrc)
    if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !!@todo get the name by ESMF_MeshGet once this is implemented by ESMF
    write(geomname,'(A)') 'mesh'
    allocate(totalubound(rank))
    allocate(totallbound(rank))
    !! if ungridded rank > 1, then assume ungridded dimension in field and define extended mesh
    if (rank > 1) write(geomname,'(A)') 'ext_'//trim(geomName)

    allocate(dimids(rank+1))

    dimids(:)=-1
    dimids(rank+1)=self%timeDimId

    !!@ todo check gridToFieldMap for order of dimensions
    call ESMF_FieldGetBounds(field, totalUBound=totalUBound, totalLBound=totalLBound, rc=esmfrc)
    if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! get grid dimension-ids
    do i=1,rank
      write(name,'(A,I1)') trim(geomName)//'_',i
      ncStatus = nf90_inq_dimid(self%ncid,trim(name),dimids(i))
      if (ncStatus /= NF90_NOERR) then
        dimcheck=-1
        exit
        endif
    enddo

    !! if mesh not present, create mesh
    if (dimcheck == -1) then
      ncStatus = nf90_redef(self%ncid)
      do i=1,rank
        write(name,'(A,I1)') trim(geomName)//'_',i
        !write(0,*) 'define dimension ',trim(name),' of size',totalubound(i)-totallbound(i)+1
        ncStatus = nf90_def_dim(self%ncid, trim(name), &
          totalubound(i)-totallbound(i)+1,dimids(i))
        if (ncStatus==NF90_ENAMEINUSE) then
          rc_=MOSSCO_NC_EXISTING
        elseif  (ncStatus==NF90_NOERR) then
          rc_=MOSSCO_NC_NOERR
        else
          rc_=MOSSCO_NC_ERROR
        end if
      enddo
      ncStatus = nf90_enddef(self%ncid)
    end if

   !! if grid not present, also create the coordinate variables
   ! if (dimcheck == -1) call self%create_mesh_coordinate(mesh)

   !! deallocate memory
   deallocate(totalubound)
   deallocate(totallbound)

  end function mossco_netcdf_mesh_dimensions

  function mossco_netcdf_grid_dimensions(self,grid) result(dimids)
    class(type_mossco_netcdf)     :: self
    type(ESMF_Grid)               :: grid
    integer                       :: ncStatus,rc_,esmfrc,dimcheck
    character(len=ESMF_MAXSTR)    :: geomName, name
    integer,allocatable           :: ubounds(:),lbounds(:)
    integer,pointer,dimension(:)  :: dimids

    integer(ESMF_KIND_I4)         :: dimCount, dimid, rank
    character(len=ESMF_MAXSTR)    :: message

    rc_ = MOSSCO_NC_NOERR
    dimcheck=0
    call ESMF_GridGet(grid,name=geomName,rank=rank,rc=esmfrc)
    if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call replace_character(geomName, ' ', '_')
    allocate(ubounds(rank))
    ubounds(:)=1
    allocate(lbounds(rank))
    lbounds(:)=1
    allocate(dimids(rank+1))
    dimids(:)=-1
    dimids(rank+1)=self%timeDimId

    call ESMF_GridGet(grid,ESMF_STAGGERLOC_CENTER,0,exclusiveCount=ubounds,rc=esmfrc)
    if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! get grid dimension-ids
    do i=1,rank
      write(name,'(A,I1)') trim(geomName)//'_',i
      ncStatus = nf90_inq_dimid(self%ncid,trim(name),dimids(i))
      if (ncStatus /= NF90_NOERR) then
        dimcheck=-1
        exit
        endif
    enddo

    !! if grid not present, create grid
    if (dimcheck == -1) then
      ncStatus = nf90_redef(self%ncid)
      do i=1,rank
        write(name,'(A,I1)') trim(geomName)//'_',i
        ncStatus = nf90_def_dim(self%ncid, trim(name), &
          ubounds(i)-lbounds(i)+1,dimids(i))
        if (ncStatus==NF90_ENAMEINUSE) then
          rc_=MOSSCO_NC_EXISTING
        elseif  (ncStatus==NF90_NOERR) then
          rc_=MOSSCO_NC_NOERR
        else
          rc_=MOSSCO_NC_ERROR
        end if
      enddo
      ncStatus = nf90_enddef(self%ncid)
    end if

    !! if grid not present, also create the coordinate variables
    if (dimcheck == -1) call self%create_coordinate(grid)

  end function mossco_netcdf_grid_dimensions

  subroutine mossco_netcdf_mesh_coordinate_create(self,mesh)

    implicit none
    class(type_mossco_netcdf)               :: self
    type(ESMF_Mesh), intent(in)             :: mesh

    integer                     :: ncStatus, varid, rc, esmfrc, rank
    integer                     :: nDims, nAtts, udimid, dimlen, i, dimid, j
    character(len=ESMF_MAXSTR)  :: varName, geomName, message, dimName

    character(len=ESMF_MAXSTR), dimension(3) :: coordNames, coordUnits
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)    :: farrayPtr3
    real(ESMF_KIND_R8), pointer, dimension(:,:)      :: farrayPtr2
    real(ESMF_KIND_R8), pointer, dimension(:)        :: farrayPtr1
    integer, pointer, dimension(:)     :: dimids
    type(ESMF_CoordSys_Flag)                         :: coordSys
    integer(ESMF_KIND_I4), dimension(:), allocatable :: exclusiveCount
    real(ESMF_KIND_R8), dimension(:), allocatable    :: ownedNodeCoords
    integer(ESMF_KIND_I4)  :: parametricDim, spatialDim, numOwnedNodes

    write(geomname,'(A)') 'mesh'
    call ESMF_MeshGet(mesh, coordSys=coordSys, parametricDim=parametricDim, &
      spatialDim=spatialDim, numownedNodes=numOwnedNodes, &
      rc=esmfrc)
    call replace_character(geomName, ' ', '_')
    !if (dimCount<1) return
    write(0,*) parametricDim, spatialDim, numOwnedNodes

    !if (coordSys == ESMF_COORDSYS_CART) then
    !  coordnames=(/'lon   ','lat   ','radius'/)
    !  coordunits=(/'degree_east ','degree_north','m           '/)
    !elseif (coordSys == ESMF_COORDSYS_CART) then
    !  coordnames=(/'lon   ','lat   ','radius'/)
    !  coordunits=(/'rad','rad','m  '/)
    !else !(coordSys == ESMF_COORDSYS_CART) then
      coordnames=(/'x','y','z'/)
      coordunits=(/' ',' ','m'/)
    !endif

    allocate(ownedNodeCoords(numOwnedNodes))
    call ESMF_MeshGet(mesh, ownedNodeCoords=ownedNodeCoords, rc=esmfrc)
    if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !dimids => self%mesh_dimensions(mesh)
#if 0
   do i=1,dimCount

      !write(0,*)  i,dimCount,trim(geomName), trim(coordNames(i)), trim(coordUnits(i))
      write(varName,'(A)') trim(geomName)//'_'//trim(coordNames(i))
      if (self%variable_present(varName)) then
        write(message,'(A)') 'A variable with this name already exists'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      do j=1,coordDimCount(i)
        write(dimName,'(A,I1)') trim(geomName)//'_',j
        ncStatus = nf90_inq_dimid(self%ncid,trim(dimName),dimids(j))
      enddo

      ncStatus = nf90_redef(self%ncid)
      ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_DOUBLE,dimids(1:coordDimCount(i)),varid)
      if (ncStatus /= NF90_NOERR) call &
          ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

      ncStatus = nf90_put_att(self%ncid,varid,'standard_name',varName)
      ncStatus = nf90_put_att(self%ncid,varid,'long_name',varName)
      ncStatus = nf90_put_att(self%ncid,varid,'missing_value',-99._ESMF_KIND_R8)
      ncStatus = nf90_put_att(self%ncid,varid,'_FillValue',-99._ESMF_KIND_R8)
      ncStatus = nf90_enddef(self%ncid)

      if (coordDimCount(i) == 1) then
        call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr1, rc=esmfrc)
        if (esmfrc == ESMF_SUCCESS) &
          ncStatus = nf90_put_var(self%ncid, varid, farrayPtr1)
      elseif (coordDimCount(i) == 2) then
        call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr2, rc=esmfrc)
        if (esmfrc == ESMF_SUCCESS) &
          ncStatus = nf90_put_var(self%ncid, varid, farrayPtr2)
      elseif (coordDimCount(i) == 3) then
        call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr3, rc=esmfrc)
        if (esmfrc == ESMF_SUCCESS) &
          ncStatus = nf90_put_var(self%ncid, varid, farrayPtr3)
      endif
      esmfrc = 0 ! reset esmfrc after checking its status above
    enddo
#endif
    if (allocated(ownedNodeCoords)) deallocate(ownedNodeCoords)

  end subroutine mossco_netcdf_mesh_coordinate_create


  subroutine mossco_netcdf_coordinate_create(self,grid)

    implicit none
    class(type_mossco_netcdf)               :: self
    type(ESMF_Grid), intent(in)             :: grid

    integer                     :: ncStatus, varid, rc, esmfrc, rank
    integer                     :: nDims, nAtts, udimid, dimlen, i, dimid, j
    character(len=ESMF_MAXSTR)  :: varName, geomName, message, dimName

    character(len=ESMF_MAXSTR), dimension(3) :: coordNames, coordUnits
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)    :: farrayPtr3
    real(ESMF_KIND_R8), pointer, dimension(:,:)      :: farrayPtr2
    real(ESMF_KIND_R8), pointer, dimension(:)        :: farrayPtr1
    integer, pointer, dimension(:)     :: dimids
    integer, dimension(:), allocatable :: coordDimids
    integer :: eLBound1(1),eLBound2(2),eLBound3(3),eLBound4(4)
    integer :: eUBound1(1),eUBound2(2),eUBound3(3),eUBound4(4)
    type(ESMF_CoordSys_Flag)                         :: coordSys
    integer(ESMF_KIND_I4), dimension(:), allocatable :: coordDimCount, exclusiveCount
    integer(ESMF_KIND_I4)                            :: dimCount

    call ESMF_GridGet(grid, coordSys=coordSys, dimCount=dimCount, &
      name=geomName, rc=esmfrc)
    call replace_character(geomName, ' ', '_')
    if (dimCount<1) return

    !if (coordSys == ESMF_COORDSYS_CART) then
    !  coordnames=(/'lon   ','lat   ','radius'/)
    !  coordunits=(/'degree_east ','degree_north','m           '/)
    !elseif (coordSys == ESMF_COORDSYS_CART) then
    !  coordnames=(/'lon   ','lat   ','radius'/)
    !  coordunits=(/'rad','rad','m  '/)
    !else !(coordSys == ESMF_COORDSYS_CART) then
      coordnames=(/'x','y','z'/)
      coordunits=(/' ',' ','m'/)
    !endif

    allocate(coordDimCount(dimCount))
    call ESMF_GridGet(grid, coordDimCount=coordDimCount, rc=esmfrc)
    if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    dimids => self%grid_dimensions(grid)
    do i=1,dimCount

      !write(0,*)  i,dimCount,trim(geomName), trim(coordNames(i)), trim(coordUnits(i))
      write(varName,'(A)') trim(geomName)//'_'//trim(coordNames(i))
      if (self%variable_present(varName)) then
        write(message,'(A)') 'A variable with this name already exists'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      allocate(coordDimids(coordDimCount(i)))
      if (allocated(exclusiveCount)) deallocate(exclusiveCount)
      allocate(exclusiveCount(coordDimCount(i)))

!     TODO: The following is a really dirty hack for non-rectilinear coordinates.
!           Correct would be use of coordDimMap.
      if (coordDimCount(i) .gt. 1) then
      do j=1,coordDimCount(i)
        write(dimName,'(A,I1)') trim(geomName)//'_',j
        ncStatus = nf90_inq_dimid(self%ncid,trim(dimName),coordDimids(j))
      enddo
      else
        write(dimName,'(A,I1)') trim(geomName)//'_',i
        ncStatus = nf90_inq_dimid(self%ncid,trim(dimName),coordDimids(1))
      end if
      !write(0,*), i, dimCount, trim(varname), coordDimCount, dimids(1:j)


      ncStatus = nf90_redef(self%ncid)
      ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_DOUBLE,coordDimids,varid)
      if (ncStatus /= NF90_NOERR) call &
          ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

      ncStatus = nf90_put_att(self%ncid,varid,'standard_name',varName)
      ncStatus = nf90_put_att(self%ncid,varid,'long_name',varName)
      ncStatus = nf90_put_att(self%ncid,varid,'missing_value',-99._ESMF_KIND_R8)
      ncStatus = nf90_put_att(self%ncid,varid,'_FillValue',-99._ESMF_KIND_R8)
      ncStatus = nf90_enddef(self%ncid)

      if (coordDimCount(i) == 1) then
        call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr1, exclusiveLBound=eLBound1, exclusiveUBound=eUBound1,rc=esmfrc)
        if (esmfrc == ESMF_SUCCESS) then
          ncStatus = nf90_put_var(self%ncid, varid, farrayPtr1(eLBound1(1):eUBound1(1)))
        else
          write(message,'(A)')  'This error will be fixed in the future, disregard for now'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif
      elseif (coordDimCount(i) == 2) then
        call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr2, exclusiveLBound=eLBound2, exclusiveUBound=eUBound2,rc=esmfrc)
        if (esmfrc == ESMF_SUCCESS) then
          ncStatus = nf90_put_var(self%ncid, varid, farrayPtr2(eLBound2(1):eUBound2(1),eLBound2(2):eUBound2(2)))
        else
          write(message,'(A)')  'This error will be fixed in the future, disregard for now'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif
      elseif (coordDimCount(i) == 3) then
        call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr3, exclusiveLBound=eLBound3, exclusiveUBound=eUBound3,rc=esmfrc)
        if (esmfrc == ESMF_SUCCESS) then
          ncStatus = nf90_put_var(self%ncid, varid, farrayPtr3(eLBound3(1):eUBound3(1),eLBound3(2):eUBound3(2),eLBound3(3):eUBound3(3)))
        else
          write(message,'(A)')  'This error will be fixed in the future, disregard for now'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif
      endif
      deallocate(coordDimids)
      esmfrc = 0 ! reset esmfrc after checking its status above
    enddo
    if (allocated(coordDimCount)) deallocate(coordDimCount)

  end subroutine mossco_netcdf_coordinate_create




end module
