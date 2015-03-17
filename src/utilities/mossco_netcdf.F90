!> @brief Implementation ESMF/NetCDF utility functions
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister
!> @author Carsten Lemmen

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "mossco_netcdf.F90"

module mossco_netcdf

  use mossco_variable_types, only: mossco_variableInfo
  use mossco_strings
  use mossco_field
  use mossco_state
  use esmf
  use netcdf


  implicit none

  private

  public MOSSCO_NetcdfCreate, MOSSCO_NetcdfOpen

  type, extends(MOSSCO_VariableInfo), public :: type_mossco_netcdf_variable
    integer               :: varid
    integer               :: ncid
    integer               :: rank
    integer, allocatable  :: dimids(:)
  end type type_mossco_netcdf_variable

  type, public :: type_mossco_netcdf
    integer      :: ncid, nvars, natts
    integer      :: timeDimId, ndims
    integer, allocatable :: dimlens(:), dimids(:)

    character(len=ESMF_MAXSTR) :: name
    type(type_mossco_netcdf_variable), pointer, dimension(:) :: variables
    contains
    procedure :: close => mossco_netcdf_close
    procedure :: add_timestep => mossco_netcdf_add_timestep
    procedure :: grid_dimensions => mossco_netcdf_grid_dimensions
    procedure :: mesh_dimensions => mossco_netcdf_mesh_dimensions
    procedure :: init_time => mossco_netcdf_init_time
    procedure :: update_variables => mossco_netcdf_update_variables
    procedure :: update => mossco_netcdf_update
    procedure :: create_variable => mossco_netcdf_variable_create
    procedure :: variable_present => mossco_netcdf_variable_present
    procedure :: put_variable => mossco_netcdf_variable_put
    procedure :: create_coordinate =>mossco_netcdf_coordinate_create
    procedure :: create_mesh_coordinate =>mossco_netcdf_mesh_coordinate_create
    procedure :: ungridded_dimension_id => mossco_netcdf_ungridded_dimension_id
    procedure :: gridget  => mossco_netcdf_grid_get
    procedure :: getvarvar => mossco_netcdf_var_get_var
    procedure :: getvar => mossco_netcdf_var_get
    procedure :: getAxis => grid_get_coordinate_axis
  end type type_mossco_netcdf

  integer, parameter :: MOSSCO_NC_ERROR=-1
  integer, parameter :: MOSSCO_NC_NOERR=ESMF_SUCCESS
  integer, parameter :: MOSSCO_NC_EXISTING=1
#include "git-sha.h"

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_variable_put"
  subroutine mossco_netcdf_variable_put(self, field, seconds, name)

    implicit none
    class(type_mossco_netcdf)                    :: self
    type(ESMF_Field), intent(inout)              :: field
    real(ESMF_KIND_R8), intent(in),optional      :: seconds
    character(len=*),optional                    :: name

    !>@todo make this an optional output var
    !integer(ESMF_KIND_I4),intent(out),optional  :: rc

    integer                     :: ncStatus, varid, rc_, esmfrc, rank, localrc, rc
    integer                     :: nDims, nAtts, udimid, dimlen
    character(len=ESMF_MAXSTR)  :: varname, message, fmt

    integer(ESMF_KIND_I4), dimension(:), allocatable :: lbnd, ubnd, exclusiveCount
    integer(ESMF_KIND_I4)       :: grid2Lbnd(2), grid2Ubnd(2), grid3Lbnd(3), grid3Ubnd(3)
    integer(ESMF_KIND_I4)       :: localDeCount, i, j, k

    real(ESMF_KIND_R8), pointer, dimension(:,:,:,:)  :: farrayPtr4
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)    :: farrayPtr3
    real(ESMF_KIND_R8), pointer, dimension(:,:)      :: farrayPtr2
    real(ESMF_KIND_R8), pointer, dimension(:)        :: farrayPtr1
    real(ESMF_KIND_R8)                               :: missingValue=-1.0E30

    integer, pointer                  :: gridmask3(:,:,:)=>null(), gridmask2(:,:)=> null()
    type(ESMF_Grid)                   :: grid
    integer(ESMF_KIND_I4)             :: gridRank
    type(ESMF_GeomType_Flag)          :: geomType
    logical                           :: catchNaN=.false., isPresent

    rc_ = ESMF_SUCCESS

    call ESMF_FieldGet(field, name=varname, rank=rank, &
      localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (localDeCount==0) return

    allocate(lbnd(rank))
    allocate(ubnd(rank))
    allocate(exclusiveCount(rank))
    call ESMF_FieldGetBounds(field, localDe=0, exclusiveLBound=lbnd, &
      exclusiveUBound=ubnd, exclusiveCount=exclusiveCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !write(fmt,'(A,I1,A,I1,A,I1,A)') '(A,I2.2,A,',rank, 'I2,A,', rank, 'I2,A,', rank, 'I2)'
    !write(message,fmt) 'localDeCount=', localDeCount,' bounds ',lbnd,' : ', &
    !  ubnd, ' exclusiveCount ', exclusiveCount
    !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
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
      call self%create_variable(field, trim(varname), rc=localrc)
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

    call ESMF_FieldGet(field, geomType=geomType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (geomType == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridGet(grid, rank=gridRank, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (gridRank == 2) then
        call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=gridmask2, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
          nullify(gridmask2)
          call ESMF_LogWrite('Disregard error above', ESMF_LOGMSG_INFO)
        else
          call ESMF_GridGetItemBounds(grid, ESMF_GRIDITEM_MASK, exclusiveLbound=grid2Lbnd, &
            exclusiveUBound=grid2Ubnd, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      elseif (gridRank == 3) then
        call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=gridmask3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
          nullify(gridmask3)
          call ESMF_LogWrite('Disregard error above', ESMF_LOGMSG_INFO)
        else
          call ESMF_GridGetItemBounds(grid, ESMF_GRIDITEM_MASK, exclusiveLbound=grid3Lbnd, &
            exclusiveUBound=grid3Ubnd, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          gridmask2 => gridmask3(:,:,1)
        endif
      endif
    end if

    call ESMF_AttributeGet(field, 'missing_value', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(field, 'missing_value', missingValue, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (rank==4) then

      call  ESMF_FieldGet(field, farrayPtr=farrayPtr4, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (associated(gridmask3)) then
        do i=lbnd(1),ubnd(1)
          do j=lbnd(2),ubnd(2)
            do k=lbnd(3),ubnd(3)
              if (gridmask3(grid3lbnd(1)-1+i,grid3lbnd(2)-1+j,grid3lbnd(3)-1+k) ==0) &
                farrayPtr4(lbnd(1)-1+i,lbnd(2)-1+j,lbnd(3)-1+k,lbnd(4):ubnd(4))=missingValue
            enddo
          enddo
        enddo
      elseif (associated(gridmask2)) then
        do i=lbnd(1),ubnd(1)
          do j=lbnd(2),ubnd(2)
            if (gridmask2(grid2lbnd(1)-1+i,grid2lbnd(2)-1+j) == 0) &
              farrayPtr4(lbnd(1)-1+i,lbnd(2)-1+j,lbnd(3):ubnd(3),lbnd(4):ubnd(4))=missingValue
          enddo
        enddo
      end if

      if (catchNan) then
        if (missingValue < 0.0) then
          if (any(farrayPtr4(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3),lbnd(4):ubnd(4)) < missingValue)) then
            call self%close()
            write(message,'(A)')  '  Possible NaN detected in field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif
        elseif (missingValue > 0.0) then
          if (any(farrayPtr4(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3),lbnd(4):ubnd(4)) > missingValue)) then
            call self%close()
            write(message,'(A)')  '  Possible NaN detected in field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif
        else
          if (any(farrayPtr4(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3),lbnd(4):ubnd(4)) /= &
                  farrayPtr4(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3),lbnd(4):ubnd(4)))) then
            call self%close()
            write(message,'(A)')  '  NaN detected in field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif
        endif
      endif

      ! it is recommended to check of nans with x /= x, as this is true for NaN
      ! it is recommended to check for inf with abs(x) > huge(x)

      ncStatus = nf90_put_var(self%ncid, varid, farrayPtr4(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3),lbnd(4):ubnd(4)), &
        start=(/1,1,1,1,dimlen/))

    elseif (rank==3) then

      call  ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (associated(gridmask3)) then
        !write(0,*) varname, shape(gridmask3), grid3lbnd, grid3ubnd, shape(farrayPtr3)
        do i=lbnd(1),ubnd(1)
          do j=lbnd(2),ubnd(2)
            do k=lbnd(3),ubnd(3)
              if (gridmask3(grid3lbnd(1)-1+i,grid3lbnd(2)-1+j,grid3lbnd(3)-1+k) == 0) &
                farrayPtr3(lbnd(1)-1+i,lbnd(2)-1+j,lbnd(3)-1+k)=missingValue
             enddo
          enddo
        enddo
      elseif (associated(gridmask2)) then
        do i=lbnd(1),ubnd(1)
          do j=lbnd(2),ubnd(2)
            if (gridmask2(grid2lbnd(1)-1+i,grid2lbnd(2)-1+j) == 0) &
                farrayPtr3(lbnd(1)-1+i,lbnd(2)-1+j,lbnd(3):ubnd(3))=missingValue
          enddo
        enddo
      end if

      if (catchNan) then

        call ESMF_LogWrite('Catching NaN: ...',ESMF_LOGMSG_INFO)
        if (missingValue < 0.0) then
          if ( any(farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) < missingValue) .or. &
               any(farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) < -1E30) .or. &
               any(farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) > 1E30)) then
            call self%close()
            write(message,'(A)')  '  Possible NaN detected in field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            endif
        else
          if ( any(farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) > missingValue) .or. &
               any(farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) < -1E30) .or. &
               any(farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) > 1E30)) then
            call self%close()
            write(message,'(A)')  '  Possible NaN detected in field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            endif
        endif
      endif

      ncStatus = nf90_put_var(self%ncid, varid, farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)), &
        start=(/1,1,1,dimlen/))

    elseif (rank==2) then

      call  ESMF_FieldGet(field, farrayPtr=farrayPtr2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (associated(gridmask2)) then
        do i=lbnd(1),ubnd(1)
          do j=lbnd(2),ubnd(2)
            if (gridmask2(grid2lbnd(1)-1+i,grid2lbnd(2)-1+j) == 0) &
                farrayPtr2(lbnd(1)-1+i,lbnd(2)-1+j)=missingValue
          enddo
        enddo
      end if

      if (catchNan) then
        if (missingValue < 0.0) then
          if (any(farrayPtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) < missingValue)) then
            call self%close()
            write(message,'(A)')  '  Possible NaN detected in field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            endif
        else
          if (any(farrayPtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) > missingValue)) then
            call self%close()
            write(message,'(A)')  '  Possible NaN detected in field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            endif
        endif
      endif
      ncStatus = nf90_put_var(self%ncid, varid, farrayPtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)), &
        start=(/1,1,dimlen/))

    elseif (rank==1) then

      call  ESMF_FieldGet(field, farrayPtr=farrayPtr1, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (catchNan) then
        if (missingValue < 0.0) then
          if (any(farrayPtr1(lbnd(1):ubnd(1)) < missingValue)) then
            call self%close()
            write(message,'(A)')  '  possible NaN detected in field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            endif
        else
          if (any(farrayPtr1(lbnd(1):ubnd(1)) > missingValue)) then
            call self%close()
            write(message,'(A)')  '  possible NaN detected in field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            endif
        endif
      endif

      ncStatus = nf90_put_var(self%ncid, varid, farrayPtr1(lbnd(1):ubnd(1)), &
        start=(/1,dimlen/))

    endif
    if (ncStatus /= NF90_NOERR) call &
      ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(lbnd)) deallocate(lbnd)
    if (allocated(exclusiveCount)) deallocate(exclusiveCount)

    nullify(gridmask2)
    nullify(gridmask3)

    ! if (present(rc)) rc=rc_

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


  subroutine mossco_netcdf_variable_create(self, field, name, rc)

    class(type_mossco_netcdf)        :: self
    type(ESMF_Field), intent(inout)  :: field
    character(len=*),optional        :: name
    integer, optional                :: rc

    type(ESMF_Grid)                :: grid
    type(ESMF_Mesh)                :: mesh
    character(len=ESMF_MAXSTR)     :: varname, geomName, fieldname, coordinates=''
    character(len=ESMF_MAXSTR)     :: units='', attributeName, string, message
    integer                        :: ncStatus,esmfrc,rc_,varid,dimcheck=0
    integer                        :: dimids_1d(2), dimids_2d(3), dimids_3d(4), rank
    integer, dimension(:),pointer  :: dimids, tmpDimids
    character(len=1), dimension(3) :: coordNames = (/'x','y','z'/)
    integer                        :: external_index=-1
    real(ESMF_KIND_R8)             :: mean_diameter, real8
    real(ESMF_KIND_R4)             :: real4
    integer(ESMF_KIND_I4)          :: i, attributeCount, int4, dimCount, ungriddedDimCount
    integer(ESMF_KIND_I8)          :: int8
    type(ESMF_TypeKind_Flag)       :: typekind
    type(ESMF_GeomType_Flag)       :: geomType
    integer                        :: ungriddedID, ungriddedLength,dimrank
    integer(ESMF_KIND_I4), allocatable, dimension(:) :: uubnd,ulbnd
    logical                        :: isPresent
    real(ESMF_KIND_R8)             :: missingValue=-1D30
    integer(ESMF_KIND_I4)          :: localrc

    integer :: petCount, localPet, vas, ssiId, peCount
    type(ESMF_Vm)                  :: vm
    integer, allocatable           :: iarray1(:), iarray2(:,:), iarray3(:,:,:), dimlen(:)

    rc_ = ESMF_SUCCESS

    call ESMF_FieldGet(field,name=fieldname,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    varname = trim(fieldname)
    if (present(name)) varname=trim(name)

    if (.not.self%variable_present(varname)) then

      call ESMF_FieldGet(field,geomType=geomType,dimCount=dimCount,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (geomType==ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(field,grid=grid,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_GridGet(grid,name=geomName,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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

      ncStatus = nf90_redef(self%ncid)
      !! add ungridded dimension
      ! ask field for ungridded dimension
      dimrank=ubound(dimids,1)
      ungriddedDimCount=dimCount-dimrank+1
      if (ungriddedDimCount .ge. 1) then
        allocate(ulbnd(ungriddedDimCount))
        allocate(uubnd(ungriddedDimCount))
        call ESMF_FieldGet(field,ungriddedLBound=ulbnd,ungriddedUBound=uubnd,rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        ! re-allocate dimids and add dimension-id(s) of ungridded dimension
        allocate(tmpDimids(1:dimrank))
        tmpDimids = dimids
        deallocate(dimids)
        allocate(dimids(dimrank+ungriddedDimCount))
        dimids(1:dimrank-1) = tmpDimids(1:dimrank-1)
        dimids(dimrank+ungriddedDimCount) = tmpDimids(dimrank)
        do i=1,ungriddedDimCount
          ! get id or create non-existing ungridded dimension
          dimids(dimrank-1+i) = self%ungridded_dimension_id(uubnd(i)-ulbnd(i)+1)
          ! evtl. add ungridded dimension to coordinates
        end do
        deallocate(tmpDimids)
      end if

      !! define variable
      ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_DOUBLE,dimids,varid)
      if (ncStatus /= NF90_NOERR) call &
        ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

      ncStatus = nf90_put_att(self%ncid,varid,'standard_name',fieldname)
      ncStatus = nf90_put_att(self%ncid,varid,'long_name',fieldname)
      ncStatus = nf90_put_att(self%ncid,varid,'coordinates',trim(coordinates))
      ncStatus = nf90_put_att(self%ncid,varid,'missing_value',missingValue)
      ncStatus = nf90_put_att(self%ncid,varid,'_FillValue',missingValue)
      call ESMF_AttributeGet(field, 'standard_name', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (.not.isPresent) call ESMF_AttributeSet(field, 'standard_name', trim(fieldName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeGet(field, 'long_name', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (.not.isPresent) call ESMF_AttributeSet(field, 'long_name', trim(fieldName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeGet(field, 'coordinates', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (.not.isPresent) call ESMF_AttributeSet(field, 'coordinates', trim(coordinates), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeGet(field, 'missing_value', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (.not.isPresent) call ESMF_AttributeSet(field, 'missing_value', missingValue, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeGet(field, '_FillValue', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (.not.isPresent) call ESMF_AttributeSet(field, '_FillValue', missingValue, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeGet(field, count=attributeCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      do i=1, attributeCount
         call ESMF_AttributeGet(field, attributeIndex=i, name=attributeName, &
           typekind=typekind, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         if (typekind==ESMF_TYPEKIND_I4) then
           call ESMF_AttributeGet(field, attributeName, int4, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),int4)
         elseif (typekind==ESMF_TYPEKIND_I8) then
           call ESMF_AttributeGet(field, attributeName, int8, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),int8)
         elseif (typekind==ESMF_TYPEKIND_R4) then
           call ESMF_AttributeGet(field, attributeName, real4, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),real4)
         elseif (typekind==ESMF_TYPEKIND_R8) then
           call ESMF_AttributeGet(field, attributeName, real8, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),real8)
         else
           call ESMF_AttributeGet(field, attributeName, string, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),trim(string))
         endif
      enddo

      !> @too remove time from dimensions
      varname='pet_'//trim(geomName)
      if (.not.self%variable_present(varname)) then
        if (geomType==ESMF_GEOMTYPE_GRID) then
          dimids => self%grid_dimensions(grid)
        elseif (geomType==ESMF_GEOMTYPE_MESH) then
          dimids => self%mesh_dimensions(field)
        endif

        ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_INT,dimids(1:ubound(dimids,1)-1),varid)
        if (ncStatus /= NF90_NOERR) call &
          ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

        ncStatus = nf90_put_att(self%ncid,varid,'standard_name','persistent_execution_thread')
        ncStatus = nf90_put_att(self%ncid,varid,'long_name','persistent_execution_thread')
        ncStatus = nf90_put_att(self%ncid,varid,'coordinates',trim(coordinates))
        ncStatus = nf90_put_att(self%ncid,varid,'missing_value',-1)
        ncStatus = nf90_put_att(self%ncid,varid,'_FillValue',-1)

        call ESMF_VMGetGlobal(vm=vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, peCount=peCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        ncStatus = nf90_put_att(self%ncid,varid,'vm_pet_count',petCount)
        ncStatus = nf90_put_att(self%ncid,varid,'vm_processing_element_count',peCount)

        call ESMF_VMGet(vm, localPet, peCount=peCount, ssiId=ssiId, vas=vas, rc=rc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        ncStatus = nf90_put_att(self%ncid,varid,'pet_virtual_address_space',vas)
        ncStatus = nf90_put_att(self%ncid,varid,'pet_single_system_image_id',ssiId)
        ncStatus = nf90_put_att(self%ncid,varid,'pet_processing_element_count',peCount)

        ncStatus = nf90_enddef(self%ncid)

        allocate(dimlen(ubound(dimids,1)-1))
        do i=1,ubound(dimids,1)-1
          ncStatus = nf90_inquire_dimension(self%ncid,dimids(i),len=dimlen(i))
        enddo

        if (ubound(dimids,1)==2) then
          allocate(iarray1(dimlen(1)))
          iarray1(:)=localPet
          ncStatus = nf90_put_var(self%ncid, varid, iarray1)
          deallocate(iarray1)
        elseif (ubound(dimids,1)==3) then
          allocate(iarray2(dimlen(1),dimlen(2)))
          iarray2(:,:)=localPet
          ncStatus = nf90_put_var(self%ncid, varid, iarray2)
          deallocate(iarray2)
        elseif (ubound(dimids,1)==4) then
          allocate(iarray3(dimlen(1),dimlen(2),dimlen(3)))
          iarray3(:,:,:)=localPet
          ncStatus = nf90_put_var(self%ncid, varid, iarray3)
          deallocate(iarray3)
        endif
        deallocate(dimlen)

      else
        ncStatus = nf90_enddef(self%ncid)
      endif

    end if

    call self%update_variables()
    call self%update()

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

  function MOSSCO_NetcdfOpen(filename, timeUnit, mode, rc) result(nc)

    character(len=*), intent(in)               :: filename
    type(type_mossco_netcdf)                   :: nc
    character(len=*), optional, intent(inout)  :: timeUnit
    character(len=1), optional, intent(in)     :: mode
    integer, intent(out), optional             :: rc

    integer                       :: ncStatus
    character(len=1)              :: mode_
    character(len=255)            :: timeUnit_

    if (present(mode)) then
      mode_= mode
    else
      mode_ = 'W'
    endif

    if (mode_ == 'W' .or. mode_ == 'w') then
      ncStatus = nf90_open(trim(filename), mode=NF90_WRITE, ncid=nc%ncid)

      if (ncStatus /= NF90_NOERR) then
        if (present(timeUnit))  then
          nc = MOSSCO_NetcdfCreate(trim(filename), timeUnit=trim(timeUnit), rc = rc)
        else
          nc = MOSSCO_NetcdfCreate(trim(filename), rc = rc)
        endif
      endif
      ncStatus = nf90_inq_dimid(nc%ncid,'time',nc%timeDimId)
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(trim(nf90_strerror(ncStatus)), ESMF_LOGMSG_ERROR)
    else
      ncStatus = nf90_open(trim(filename), mode=NF90_NOWRITE, ncid=nc%ncid)
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(trim(nf90_strerror(ncStatus)), ESMF_LOGMSG_ERROR)
      ncStatus = nf90_inq_dimid(nc%ncid,'time',nc%timeDimId)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  did not find time dim ID: '//trim(nf90_strerror(ncStatus)), ESMF_LOGMSG_WARNING)
        ncStatus = NF90_NOERR
      else
        ncStatus = nf90_get_att(nc%ncid, nc%timeDimId, 'units', timeUnit_)
        if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(trim(nf90_strerror(ncStatus)), ESMF_LOGMSG_ERROR)
        if (present(timeUnit)) write(timeUnit,'(A)') trim(timeUnit_)
      endif
    endif

    nc%name=trim(filename)
    call nc%update_variables()
    call nc%update()

    if (present(rc)) then
      rc=ncStatus
      if (ncStatus == NF90_NOERR) rc=ESMF_SUCCESS
    endif

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
    var%standard_name='time'
    var%units=trim(timeUnit)
    var%rank=1
  end if
  if (present(rc)) rc=rc_

    call self%update()

    return

  end subroutine mossco_netcdf_init_time

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_update_variables"
  subroutine mossco_netcdf_update_variables(self)

    implicit none

    class(type_mossco_netcdf)      :: self
    integer                        :: localrc, i, j, nvars, natts
    integer                        :: nvardims, nvaratts
    type(type_mossco_netcdf_variable), pointer :: var
    character(ESMF_MAXSTR)         :: message

    localrc = nf90_inquire(self%ncid, nVariables=nvars, nAttributes=natts)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite(trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    nullify(self%variables)
    allocate(self%variables(nvars))
    do i=1, nvars
      var => self%variables(i)
      var%varid = i
      localrc = nf90_inquire_variable(self%ncid, i, ndims=var%rank, natts=nvaratts, name=var%name)
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite(trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif

      localrc = nf90_get_att(self%ncid,var%varid, 'standard_name', var%standard_name)
      if (localrc /= NF90_NOERR) var%standard_name=var%name

      localrc = nf90_get_att(self%ncid,var%varid, 'units', var%units)
      if (localrc /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(var%name)//' did not specify units in '//trim(self%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        var%units=''
      endif

      !call ESMF_LogWrite(trim(var%standard_name)//' '//trim(var%units), ESMF_LOGMSG_INFO)

      if (var%rank <= 0) cycle

      if (allocated(var%dimids)) deallocate(var%dimids)
      allocate(var%dimids(var%rank))

      localrc = nf90_inquire_variable(self%ncid, i, dimids=var%dimids)
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite(trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif

    end do

    return

  end subroutine mossco_netcdf_update_variables

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_update"
  subroutine mossco_netcdf_update(self)

    implicit none

    class(type_mossco_netcdf)      :: self
    integer                        :: localrc, i, nvars, natts, ndims
    character(ESMF_MAXSTR)         :: message

    localrc = nf90_inquire(self%ncid, nVariables=nvars, nAttributes=natts)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite(trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    self%natts=natts
    self%nvars=nvars

    if (allocated(self%dimids))  deallocate(self%dimids)
    if (allocated(self%dimlens)) deallocate(self%dimlens)

    localrc = nf90_inquire(self%ncid, unlimitedDimId=self%timeDimId)

    localrc = nf90_inquire(self%ncid, nDimensions=ndims)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite(trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    self%ndims=ndims

    allocate(self%dimids(ndims))
    allocate(self%dimlens(ndims))

    do i=1, ndims
      localrc=nf90_inquire_dimension(self%ncid, self%dimids(i), len=self%dimlens(i))
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite(trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo

    return

  end subroutine mossco_netcdf_update

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
   call self%update()

   return

  end function mossco_netcdf_mesh_dimensions

  recursive function mossco_netcdf_grid_dimensions(self,grid) result(dimids)
    class(type_mossco_netcdf)     :: self
    type(ESMF_Grid)               :: grid
    integer                       :: ncStatus,rc_,esmfrc,dimcheck
    character(len=ESMF_MAXSTR)    :: geomName, name
    integer,allocatable           :: ubounds(:),lbounds(:)
    integer,pointer,dimension(:)  :: dimids

    integer(ESMF_KIND_I4)         :: dimCount, dimid, rank, i
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
    call self%update()

    return

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
    !write(0,*) parametricDim, spatialDim, numOwnedNodes

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

    call self%update_variables()
    call self%update()

  end subroutine mossco_netcdf_mesh_coordinate_create


#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_coordinate_create"
  subroutine mossco_netcdf_coordinate_create(self,grid)

    implicit none
    class(type_mossco_netcdf)               :: self
    type(ESMF_Grid), intent(in)             :: grid

    integer                     :: ncStatus, varid, rc, esmfrc, rank, localrc
    integer                     :: nDims, nAtts, udimid, dimlen, dimid, j
    character(len=ESMF_MAXSTR)  :: varName, geomName, message, dimName

    character(len=ESMF_MAXSTR), dimension(3) :: coordNames, coordUnits, axisNameList
    character(len=ESMF_MAXSTR)               :: attributeName
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)    :: farrayPtr3
    real(ESMF_KIND_R8), pointer, dimension(:,:)      :: farrayPtr2
    real(ESMF_KIND_R8), pointer, dimension(:)        :: farrayPtr1
    integer(ESMF_KIND_I4), pointer, dimension(:)     :: intPtr1
    integer, pointer, dimension(:)     :: dimids
    integer, dimension(:), allocatable :: coordDimids
    integer :: eLBound1(1),eLBound2(2),eLBound3(3),eLBound4(4)
    integer :: eUBound1(1),eUBound2(2),eUBound3(3),eUBound4(4)
    type(ESMF_CoordSys_Flag)                         :: coordSys
    integer(ESMF_KIND_I4), dimension(:), allocatable :: coordDimCount, exclusiveCount
    integer(ESMF_KIND_I4)                            :: dimCount, attributeCount, i
    type(ESMF_Array)                                 :: array
    logical                                          :: isPresent

    type(ESMF_TypeKind_Flag)         :: typekind
    real(ESMF_KIND_R8)               :: real8
    real(ESMF_KIND_R4)               :: real4
    integer(ESMF_KIND_I8)            :: int8
    integer(ESMF_KIND_I4)            :: int4
    character(len=ESMF_MAXSTR)       :: string


    call ESMF_GridGet(grid, coordSys=coordSys, dimCount=dimCount, &
      name=geomName, rc=esmfrc)
    call replace_character(geomName, ' ', '_')
    if (dimCount<1) return

    if (coordSys == ESMF_COORDSYS_SPH_DEG) then
      coordnames=(/'lon  ','lat  ','layer'/)
      coordunits=(/'degree','degree','1     '/)
    elseif (coordSys == ESMF_COORDSYS_SPH_RAD) then
      coordnames=(/'lon  ','lat  ','layer'/)
      coordunits=(/'rad','rad','1  '/)
    else
      coordnames=(/'x','y','z'/)
      coordunits=(/'1','1','1'/)
    endif

    axisNameList=(/'X','Y','Z'/)

    allocate(coordDimCount(dimCount))
    call ESMF_GridGet(grid, coordDimCount=coordDimCount, rc=esmfrc)
    if (esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    dimids => self%grid_dimensions(grid)

    ! Write the auxiliary coordinate variables x, y, z
    ! These are 1-dimensional irrespective of the actual coordinates
    do i=1,dimCount

      write(varName,'(A)') trim(geomName)//'_'//trim(axisNameList(i))
      if (self%variable_present(varName)) then
        write(message,'(A)') 'A variable with this name already exists'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      if (.not.allocated(coordDimids)) allocate(coordDimids(1))

      write(dimName,'(A,I1)') trim(geomName)//'_',i
      ncStatus = nf90_inq_dimid(self%ncid,trim(dimName),coordDimids(1))

      call self%getAxis(grid, coordDim=i, intPtr1=intPtr1, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ncStatus = nf90_redef(self%ncid)
      ncStatus = nf90_def_var(self%ncid, trim(varName), NF90_Int, coordDimids, varid)
      if (ncStatus /= NF90_NOERR) call &
          ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

      !! Write default attributes into netCDF
      ncStatus = nf90_put_att(self%ncid,varid,'standard_name',trim(varName))
      ncStatus = nf90_put_att(self%ncid,varid,'units',1)
      ncStatus = nf90_put_att(self%ncid,varid,'missing_value',-9999_ESMF_KIND_I4)
      ncStatus = nf90_put_att(self%ncid,varid,'_FillValue',-9999_ESMF_KIND_I4)
      ncStatus = nf90_put_att(self%ncid,varid,'axis',axisNameList(i))

      ncStatus = nf90_enddef(self%ncid)
      ncStatus = nf90_put_var(self%ncid, varid, intPtr1(:))
    enddo

    if (allocated(coordDimids)) deallocate(coordDimids)

    do i=1,dimCount

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

      !! Write default attributes into netCDF
      ncStatus = nf90_put_att(self%ncid,varid,'standard_name',trim(varName))
      ncStatus = nf90_put_att(self%ncid,varid,'long_name',trim(varName))
      ncStatus = nf90_put_att(self%ncid,varid,'units',trim(coordUnits(i)))
      ncStatus = nf90_put_att(self%ncid,varid,'missing_value',-99._ESMF_KIND_R8)
      ncStatus = nf90_put_att(self%ncid,varid,'_FillValue',-99._ESMF_KIND_R8)
      ncStatus = nf90_put_att(self%ncid,varid,'horizontal_stagger_location','center')
      !! axis attribute added only for 1-D coordinate variables
      if (coordDimCount(i)==1) then
        ncStatus = nf90_put_att(self%ncid,varid,'axis',axisNameList(i))
      end if

      !! Inquire array for attributes and create / overwrite attributes
      call ESMF_GridGetCoord(grid, i, staggerloc=ESMF_STAGGERLOC_CENTER, array=array, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeGet(array, count=attributeCount, rc=rc)
      do j=1, attributeCount
         call ESMF_AttributeGet(array, attributeIndex=j, name=attributeName, &
           typekind=typekind, rc=rc)
         if (typekind==ESMF_TYPEKIND_I4) then
           call ESMF_AttributeGet(array, attributeName, int4, rc=rc)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),int4)
         elseif (typekind==ESMF_TYPEKIND_I8) then
           call ESMF_AttributeGet(array, attributeName, int8, rc=rc)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),int8)
         elseif (typekind==ESMF_TYPEKIND_R4) then
           call ESMF_AttributeGet(array, attributeName, real4, rc=rc)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),real4)
         elseif (typekind==ESMF_TYPEKIND_R8) then
           call ESMF_AttributeGet(array, attributeName, real8, rc=rc)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),real8)
         else
           call ESMF_AttributeGet(array, attributeName, string, rc=rc)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),trim(string))
         endif
      enddo

      !! End definition phase of netcdf
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

    call self%update_variables()
    call self%update()

    return

  end subroutine mossco_netcdf_coordinate_create


#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_ungridded_dimension_id"
  function mossco_netcdf_ungridded_dimension_id(self,length) result(dimid)

    implicit none
    class(type_mossco_netcdf)   :: self
    integer, intent(in)         :: length

    integer                     :: ncStatus
    integer                     :: dimid
    character(len=ESMF_MAXSTR)  :: message, dimName

    dimid = -1
    write(dimName,'(A9,I5.5)')  'ungridded',length
    ncStatus = nf90_inq_dimid(self%ncid,trim(dimName),dimid)
    if (ncStatus /= NF90_NOERR) then
      ncStatus = nf90_def_dim(self%ncid,trim(dimName),length,dimid=dimid)
    end if

    call self%update()

  end function mossco_netcdf_ungridded_dimension_id

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_grid_get"
  subroutine mossco_netcdf_grid_get(self, grid, var, rc)

    implicit none
    class(type_mossco_netcdf)                    :: self
    type(ESMF_Grid), intent(inout)               :: grid
    type(type_mossco_netcdf_variable)            :: var
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                        :: localrc, i, udimid, varid
    integer(ESMF_KIND_I4)                        :: j, k, localDeCount, itemCount
    integer(ESMF_KIND_I4), allocatable           :: dimids(:), ubnd(:), lbnd(:)
    character(len=ESMF_MAXSTR)                   :: coordinates, units, message
    character(len=ESMF_MAXSTR), allocatable      :: coordNameList(:)
    type(type_mossco_netcdf_variable)            :: coordVar
    type(ESMF_CoordSys_Flag)                     :: coordSys
    type(ESMF_Array)                             :: array
    type(ESMF_ArrayBundle)                       :: arrayBundle
    real(ESMF_KIND_R8), pointer                  :: farrayPtr1(:), farrayPtr2(:,:)
    type(ESMF_DistGrid)                          :: distGrid

    rc = ESMF_SUCCESS

    !> Ask for coordinates attribute, if not present, then return (@todo: implement alternative solution)
    localrc = nf90_get_att(self%ncid, var%varid, 'coordinates', coordinates)
    if (localrc /= NF90_NOERR) then
      write(message,'(A)') 'Cannot determine grid, "coordinates" attribute is missing from variable '//trim(var%name)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      return
    end if

    coordSys=ESMF_COORDSYS_CART

    allocate(coordNameList(var%rank))
    allocate(dimids(var%rank))
    do i=1, var%rank
      j=index(coordinates,' ')
      if (j<=1) exit

      coordNameList(i) = trim(adjustl(coordinates(1:j)))
      coordinates=coordinates(j+1:len_trim(coordinates))

      ! Try to find the corresponding coordinate variable
      localrc = nf90_inq_varid(self%ncid, trim(coordNameList(i)), varid)
      if (localrc /= NF90_NOERR) then
        write(message,'(A)') 'No variable found for coordinate '//trim(coordNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        return
      end if

      ! search for coordinate variable in self%variables
      do j=1, ubound(self%variables,1)
        if (trim(self%variables(j)%name) == trim(coordNameList(j))) then
          coordVar = self%variables(j)
          exit
        endif
      enddo

      do j=1, ubound(coordVar%dimids,1)
        if (all(var%dimids /= coordVar%dimids(j))) then
          write(message,'(A)') 'No corresponding dimensions in variable and its coordinate '//trim(coordNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          return
        end if
        k=k+1
        dimids(k)=coordVar%dimids(j)
      enddo

      localrc = nf90_get_att(self%ncid, coordVar%varid, 'units', units)
      if (localrc /= NF90_NOERR) then
        write(message,'(A)') 'Cannot determine unit of coordinate variable '//trim(coordVar%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      end if

      if (index(units,'degree')>0) then
        coordSys = ESMF_COORDSYS_SPH_DEG
      elseif (index(units,'rad')>0) then
        coordSys = ESMF_COORDSYS_SPH_RAD
      endif

      if (localrc /= NF90_NOERR) then
       ! write(message,'(A)') 'No variable found for coordinate '//trim(coordNameList(i))
       ! call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
       ! return
      end if

!      array = ESMF_ArrayCreate(name=trim(coordVar%name), typeKind=ESMF_TYPEKIND_R8, &
!        rank=coordVar%rank, rc=localrc)
!
!      call ESMF_ArrayGet(array, localDeCount=localDeCount, rank=rank, rc=localrc)
!      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!      if (localDeCount==0) return
!
!      allocate(lbnd(rank))
!      allocate(ubnd(rank))
!
!      call ESMF_ArrayGetBounds(array, localDe=0, exclusiveLBound=lbnd, &
!        exclusiveUBound=ubnd, rc=localrc)
!      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!      if (coordVar%rank == 1) then
!        call ESMF_ArrayGet(array, farrayPtr=farrayPtr1, rc=localrc)
!        localrc = nf90_var_get(self%ncid, coordVar%varid, farrayPtr1, lbnd, ubnd)
!      elseif (coordVar%rank == 2) then
!        call ESMF_ArrayGet(array, farrayPtr=farrayPtr2, rc=localrc)
!        localrc = nf90_var_get(self%ncid, coordVar%varid, farrayPtr2, lbnd, ubnd)
!      else
!        write(message,'(A)') 'Not implemented: rank>2 coordinate variable '//trim(coordVar%name)
!        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
!        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!      end if
!      deallocate(lbnd)
!      deallocate(ubnd)
!
!      call ESMF_ArrayBundleAdd(arrayBundle, (/array/), rc=localrc)
    enddo
    deallocate(coordNameList)
    deallocate(dimids)

!    call ESMF_ArrayBundleGet(arrayBundle, itemCount=itemCount, rc=localrc)

!    grid = ESMF_GridCreateNoPeriDim(rc=localrc)
!    call ESMF_GridAddCoords(grid, rc=localrc)

!    do i=1, itemCount
!      call ESMF_ArrayBundleGet(arrayBundle, i, array, rc=localrc)
!      call ESMF_GridSetCoord(grid, coordDim=i, rc=localrc)
!    enddo

      rc=ESMF_RC_NOT_IMPL

    return

  end subroutine mossco_netcdf_grid_get

#undef  ESMF_METHOD
#define ESMF_METHOD "grid_get_coordinate_axis"
  subroutine grid_get_coordinate_axis(self, grid, coordDim, intPtr1, rc)

    implicit none
    class(type_mossco_netcdf)                    :: self
    type(ESMF_grid), intent(in)                  :: grid
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    integer(ESMF_KIND_I4), intent(in)            :: coordDim
    integer(ESMF_KIND_I4), pointer, intent(inout):: intPtr1(:)

    integer(ESMF_KIND_I4)                        :: localrc, i, n, rc_
    integer(ESMF_KIND_I4)                        :: rank, decount,localPet
    integer(ESMF_KIND_I4),allocatable            :: minIndexPDe(:,:), maxIndexPDe(:,:), deBlockList(:,:,:)
    type(ESMF_DistGrid)                          :: distGrid
    type(ESMF_DELayout)                          :: delayout
    type(ESMF_Vm)                                :: Vm

    rc_=ESMF_SUCCESS

    nullify(intPtr1)

    call ESMF_GridGet(grid, distGrid=distGrid, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_DistGridGet(distGrid, delayout=delayout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_DELayoutGet(delayout, deCount=deCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(minIndexPDe(rank, deCount))
    allocate(maxIndexPDe(rank, deCount))
    allocate(deBlockList(rank, 2, deCount))

    call ESMF_DistGridGet(distGrid, minIndexPDe=minIndexPDe, &
                                    maxIndexPDe=maxIndexPDe, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    deBlockList(:,1,:) = minIndexPDe
    deBlockList(:,2,:) = maxIndexPDe

    call ESMF_VmGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @todo decount instead of petCount
    call ESMF_VmGet(vm, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    n=deBlockList(coordDim,2,localPet+1)-deBlockList(coordDim,1,localPet+1)+1
    allocate(intPtr1(n))
    do i=1,n
      intPtr1(i)=deBlockList(coordDim,1,localPet+1)+i-1
    enddo

   ! write (0,*) 'c, deBlockList1 = ', coordDim, deBlockList(coordDim,1,localPet+1), deBlockList(coordDim,2,localPet+1)

    if (present(rc)) rc=rc_

  end subroutine grid_get_coordinate_axis

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_var_get"
  subroutine mossco_netcdf_var_get(self, field, var, itime, rc)

    implicit none
    class(type_mossco_netcdf)                    :: self
    type(ESMF_Field), intent(inout)               :: field
    type(type_mossco_netcdf_variable)            :: var
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    integer(ESMF_KIND_I4), intent(in), optional  :: itime

    integer(ESMF_KIND_I4)                        :: localrc, udimid, localDeCount, rc_
    integer(ESMF_KIND_I4)                        :: rank, itime_, j, i
    type(ESMF_FieldStatus_Flag)                  :: fieldStatus
    integer(ESMF_KIND_I4), allocatable           :: start(:), count(:), ubnd(:)
    real(ESMF_KIND_R8), pointer                  :: farrayPtr1(:), farrayPtr2(:,:)
    real(ESMF_KIND_R8), pointer                  :: farrayPtr3(:,:,:), farrayPtr4(:,:,:,:)
    character(len=ESMF_MAXSTR)                   :: message

    integer(ESMF_KIND_I4)                        :: deCount,localPet
    integer(ESMF_KIND_I4),allocatable            :: minIndexPDe(:,:), maxIndexPDe(:,:)
    type(ESMF_Grid)                              :: grid
    type(ESMF_DistGrid)                          :: distGrid
    type(ESMF_DELayout)                          :: delayout
    type(ESMF_Vm)                                :: Vm

    rc_ = ESMF_SUCCESS

    if (present(itime)) then
      itime_=itime
    else
      itime_=1
    endif

    ! Test for field completeness and terminate if not complete
    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
      write(message, '(A)')  'Cannot read into non-complete field '
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, rank=rank, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (localDeCount==0) return

    if (rank > 4) then
      write(message, '(A)')  'Rank > 4 not implemented for reading field '
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      rc = ESMF_RC_NOT_IMPL
    endif

    call ESMF_FieldGet(field, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridGet(grid, distGrid=distGrid, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_DistGridGet(distGrid, delayout=delayout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_DELayoutGet(delayout, deCount=deCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(minIndexPDe(rank, deCount))
    allocate(maxIndexPDe(rank, deCount))

    call ESMF_DistGridGet(distGrid, minIndexPDe=minIndexPDe, &
                                    maxIndexPDe=maxIndexPDe, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_VmGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_VmGet(vm, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(start(rank))
    allocate(count(rank))
    allocate(ubnd(rank))


    start=minIndexPDe(:,localPet+1)
    where (start < 1)
      start=1
    endwhere

    ubnd=maxIndexPDe(:,localPet+1)

    j=0
    do i=1, rank
      j=j+1
      if (var%dimids(i)==self%timeDimId) j=j+1
      if (ubnd(i)>self%dimlens(var%dimids(j))) ubnd(i)=self%dimlens(var%dimids(j))
    enddo

    count(:)=1
    count=count+ubnd-start

    write(0,*) var%name, 'dimids=',var%dimids
    write(0,*) var%name, 'dimlens=',self%dimlens
    write(0,*) 'start=', start
    write(0,*) 'maxIndPDe=', maxIndexPDe(:,localPet+1)
    write(0,*) 'ubnd=', ubnd
    write(0,*) 'count=', count

    if (any(count <= 0)) return

    write(0,*) 'start=', start, ' count=', count, 'rank=', rank, 'var%rank=', var%rank, 'itime=', itime_, 'name=', var%name

    if (rank == 1) then
      call ESMF_FieldGet(field, farrayPtr=farrayPtr1, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (var%rank==rank) then
        localrc = nf90_get_var(self%ncid, var%varid, farrayPtr1, start, count)
      elseif (var%rank==rank+1 .and. var%dimids(rank+1) == self%timeDimId ) then
        localrc = nf90_get_var(self%ncid, var%varid, farrayPtr1, (/start(1),itime_/), (/count(1),1/))
      else
        rc = ESMF_RC_NOT_IMPL
        return
      endif
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite(trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    elseif (rank == 2) then
      call ESMF_FieldGet(field, farrayPtr=farrayPtr2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
     ! write(0,*) 'rank=', rank, 'var%rank=', var%rank, 'var%dimids=', var%dimids(:), 'udimid=',self%timeDimId
      if (var%rank==rank) then
        localrc = nf90_get_var(self%ncid, var%varid, farrayPtr2, start, count)
      elseif (var%rank==rank+1 .and. var%dimids(rank+1) == self%timeDimId ) then
        localrc = nf90_get_var(self%ncid, var%varid, farrayPtr2, (/start(1),start(2),itime/), (/count(1),count(2),1/))
      else
        rc = ESMF_RC_NOT_IMPL
        return
      endif
      !write(0,*) 'farrayPtr2=', farrayPtr2
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite(trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    elseif (rank == 3) then
      call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (var%rank==rank) then
        localrc = nf90_get_var(self%ncid, var%varid, farrayPtr3, start, count)
      elseif (var%rank==rank+1 .and. var%dimids(rank+1) == self%timeDimId ) then
        localrc = nf90_get_var(self%ncid, var%varid, farrayPtr3, &
          (/start(1),start(2),start(3),itime_/), (/count(1),count(2), count(3),1/))
      else
        rc = ESMF_RC_NOT_IMPL
        return
      endif
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite(trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    elseif (rank == 4) then
      call ESMF_FieldGet(field, farrayPtr=farrayPtr4, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (var%rank==rank) then
        localrc = nf90_get_var(self%ncid, var%varid, farrayPtr4, start, count)
      elseif (var%rank==rank+1 .and. var%dimids(rank+1) == self%timeDimId ) then
        localrc = nf90_get_var(self%ncid, var%varid, farrayPtr4, &
          (/start(1),start(2),start(3),start(4),itime_/), (/count(1),count(2),count(3),count(4),1/))
      else
        rc = ESMF_RC_NOT_IMPL
        return
      endif
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite(trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    if (allocated(ubnd))  deallocate(ubnd)
    if (allocated(count)) deallocate(count)
    if (allocated(start)) deallocate(start)

    if (present(rc)) rc=rc_

    return

  end subroutine mossco_netcdf_var_get

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_var_get_var"
  function mossco_netcdf_var_get_var(self, varname, rc) result(var)

    implicit none
    class(type_mossco_netcdf)                    :: self
    character(len=ESMF_MAXSTR)                   :: varname
    type(type_mossco_netcdf_variable)            :: var
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                        :: i

    rc = ESMF_SUCCESS

    do i=1, ubound(self%variables,1)
      if (trim(self%variables(i)%name) == trim(varname)) then
        var = self%variables(i)
        return
      endif
      if (trim(self%variables(i)%standard_name) == trim(varname)) then
        var = self%variables(i)
        return
      endif
    enddo

    rc = ESMF_RC_NOT_FOUND
    return

  end function mossco_netcdf_var_get_var

end module
