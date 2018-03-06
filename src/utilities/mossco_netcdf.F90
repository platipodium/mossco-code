!> @brief Implementation ESMF/NetCDF utility functions
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2014, 2015, 2016, 2017, 2018 Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>

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
#define DEBUG_NAN
#define DEBUG_INF

#define RANGE1D lbnd(1):ubnd(1)
#define RANGE2D RANGE1D,lbnd(2):ubnd(2)
#define RANGE3D RANGE2D,lbnd(3):ubnd(3)
#define RANGE4D RANGE3D,lbnd(4):ubnd(4)

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module mossco_netcdf

  use mossco_variable_types, only: mossco_variableInfo
  use mossco_strings
  use mossco_field
  use mossco_state
  use mossco_time
  use mossco_gridspec
  use esmf
  use netcdf

  implicit none

  private

  public MOSSCO_NetcdfCreate, MOSSCO_NetcdfOpen
  public MOSSCO_GridAddMaskFromVariable

  type, extends(MOSSCO_VariableInfo), public :: type_mossco_netcdf_variable
    integer               :: varid
    integer               :: ncid
    integer               :: rank
    integer, allocatable  :: dimids(:)
    character(len=11)     :: precision='NF90_REAL'
  end type type_mossco_netcdf_variable

  type, public :: type_mossco_netcdf
    integer      :: ncid, nvars, natts
    integer      :: timeDimId, ndims
    integer, allocatable  :: dimlens(:)
    character(len=ESMF_MAXSTR), allocatable :: dimNames(:)
    character(len=11)    :: precision='NF90_REAL'

    character(len=ESMF_MAXSTR) :: name, timeUnit
    type(type_mossco_netcdf_variable), pointer, dimension(:) :: variables


    contains
    procedure :: close => mossco_netcdf_close
    procedure :: add_timestep => mossco_netcdf_add_timestep
    procedure :: locstream_dimensions => mossco_netcdf_locstream_dimensions
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
    procedure :: refTimeString => mossco_netcdf_reftime_string
    procedure :: refTime => mossco_netcdf_reftime
    procedure :: timeIndex => mossco_netcdf_find_time_index
    procedure :: timeGet => MOSSCO_NcGetTime
    procedure :: getatt => mossco_netcdf_var_get_att
    procedure :: putattstring => MOSSCO_NcPutAttString
    procedure :: putattint => MOSSCO_NcPutAttInt
    procedure :: putattdouble => MOSSCO_NcPutAttDouble
    procedure :: putattfloat => MOSSCO_NcPutAttFloat
    procedure :: create_bounds_variable

  end type type_mossco_netcdf

  integer, parameter :: MOSSCO_NC_ERROR=-1
  integer, parameter :: MOSSCO_NC_NOERR=ESMF_SUCCESS
  integer, parameter :: MOSSCO_NC_EXISTING=1

  interface MOSSCO_AttributeNetcdfWrite
    module procedure MOSSCO_AttributeNetcdfWriteField
    module procedure MOSSCO_AttributeNetcdfWriteState
    module procedure MOSSCO_AttributeNetcdfWriteArray
  end interface MOSSCO_AttributeNetcdfWrite

#include "git-sha.h"

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_variable_put"
  subroutine mossco_netcdf_variable_put(self, field, kwe, seconds, name, &
    checkNaN, checkInf, precision, rc)

    implicit none
    class(type_mossco_netcdf)                    :: self
    type(ESMF_Field), intent(inout)              :: field
    logical, intent(in), optional                :: kwe
    real(ESMF_KIND_R8), intent(in), optional     :: seconds
    character(len=*), optional                   :: name
    logical, intent(in), optional                :: checkNaN
    logical, intent(in), optional                :: checkInf
    character(len=*),intent(in), optional        :: precision
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer                     :: ncStatus, varid, rc_, rank=0, localrc
    integer                     :: nDims=0, nAtts, udimid, dimlen
    character(len=ESMF_MAXSTR)  :: varname, message
    type(type_mossco_netcdf_variable),pointer :: var=> null()

    integer(ESMF_KIND_I4), dimension(:), allocatable :: lbnd, ubnd, exclusiveCount
    integer(ESMF_KIND_I4)       :: grid2Lbnd(2), grid2Ubnd(2), grid3Lbnd(3), grid3Ubnd(3)
    integer(ESMF_KIND_I4)       :: localDeCount, i, j, k
    logical                     :: checkNaN_=.true., checkInf_=.true.

    real(ESMF_KIND_R8), pointer, dimension(:,:,:,:)  :: farrayPtr4=>null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)    :: farrayPtr3=>null()
    real(ESMF_KIND_R8), pointer, dimension(:,:)      :: farrayPtr2=>null()
    real(ESMF_KIND_R8), pointer, dimension(:)        :: farrayPtr1=>null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:,:)  :: ncarray4=>null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)    :: ncarray3=>null()
    real(ESMF_KIND_R8), pointer, dimension(:,:)      :: ncarray2=>null()
    real(ESMF_KIND_R8), pointer, dimension(:)        :: ncarray1=>null()
    real(ESMF_KIND_R4)                               :: missingValueR4=-1.0E30
    real(ESMF_KIND_R8)                               :: missingValueR8=-1.0D30, missingValue=-1.0D30
    real(ESMF_KIND_I4)                               :: missingValueI4=-9999
    real(ESMF_KIND_I8)                               :: missingValueI8=-9999
    real(ESMF_KIND_R8)                               :: representableValue
    type(ESMF_TypeKind_Flag)                         :: mvTypeKind, typeKind

    character(len=11)                 :: precision_

    integer, pointer                  :: gridmask3(:,:,:)=>null(), gridmask2(:,:)=> null()
    type(ESMF_Grid)                   :: grid
    type(ESMF_StaggerLoc)             :: staggerloc
    integer(ESMF_KIND_I4)             :: gridRank
    type(ESMF_Mesh)                   :: mesh
    type(ESMF_MeshLoc)                :: meshLoc
    type(ESMF_GeomType_Flag)          :: geomType
    logical                           :: isPresent, gridIsPresent

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = rc_
    if (present(checkNaN)) checkNaN_ = checkNaN
    if (present(checkInf)) checkInf_ = checkInf
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, name=varname, rank=rank, &
      localDeCount=localDeCount, typeKind=typeKind, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (localDeCount == 0) return
    if (typeKind /= ESMF_TYPEKIND_R8) return

    allocate(lbnd(rank), stat=localrc)
    allocate(ubnd(rank), stat=localrc)
    allocate(exclusiveCount(rank), stat=localrc)

    call ESMF_FieldGetBounds(field, localDe=0, exclusiveLBound=lbnd, &
      exclusiveUBound=ubnd, exclusiveCount=exclusiveCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (any(exclusiveCount==0)) return

    if (present(name)) varname=trim(name)

    if (rank>4 .or. rank<1) then
      write(message,'(A)')  '  writing fields with rank<1 or rank>4 not supported, field skipped.'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
      !> @todo reconsider the return value here
      return
    endif

    !> If the variable does not exist, create it
    if (.not.self%variable_present(varname)) then

      precision_=self%precision
      if (present(precision)) precision_=precision

      call self%create_variable(field, trim(varname), precision=precision_, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call self%update_variables()
      call self%update()
    endif
    !> @todo what happens if variable exists but on different grid?

    var=>self%getvarvar(trim(varname))
    precision_=var%precision

    if (precision_=='NF90_DOUBLE') then
      representableValue=huge(0.0_ESMF_KIND_R8)
    else
      representableValue=huge(0.0_ESMF_KIND_R4)
    endif

    if (.not.associated(var)) then
      call ESMF_LogWrite('  could not find variable '//trim(varname), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc=ESMF_RC_NOT_FOUND
      return
    endif

    ncStatus=nf90_inq_varid(self%ncid, var%name, varid)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  could not find variable '//trim(varname), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc=ESMF_RC_NOT_FOUND
      return
    endif

    if (any(var%dimids==self%timeDimId)) ndims=size(var%dimids)-1

    if (rank /= nDims) then
       write(message,'(A)')  'Field rank and netcdf dimension count do not match'
       call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
       if (present(rc)) rc=ESMF_RC_NOT_FOUND
       return
    endif

    udimid = self%timeDimId
    if (udimid<0) then
      dimlen=0
    else
      dimlen=self%dimlens(self%timeDimId)
    endif

    call ESMF_FieldGet(field, geomType=geomType, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (geomType == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, grid=grid, staggerloc=staggerloc, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_GridGet(grid, rank=gridRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

#if ESMF_VERSION_MAJOR > 6
!! This is only implemented from 7b29
      if (gridRank == 2) then
        call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, staggerloc=staggerloc, isPresent=gridIsPresent, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        if (gridIsPresent) then
          call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, staggerloc=staggerloc, farrayPtr=gridmask2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

          call ESMF_GridGetItemBounds(grid, ESMF_GRIDITEM_MASK, staggerloc=staggerloc, exclusiveLbound=grid2Lbnd, &
            exclusiveUBound=grid2Ubnd, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        else
          nullify(gridmask2)
        endif
      elseif (gridRank == 3) then
        call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, staggerloc=staggerloc, isPresent=gridIsPresent, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        if (gridIsPresent) then
          call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, staggerloc=staggerloc, farrayPtr=gridmask3, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

          call ESMF_GridGetItemBounds(grid, ESMF_GRIDITEM_MASK, staggerloc=staggerloc, &
            exclusiveLbound=grid3Lbnd, exclusiveUBound=grid3Ubnd, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

          gridmask2 => gridmask3(:,:,1)
        else
          nullify(gridmask3)
        endif
      endif
#else
      if (gridRank == 2) then
        call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, staggerloc=staggerloc, farrayPtr=gridmask2, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
          nullify(gridmask2)
          call ESMF_LogWrite('Disregard five errors above', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        else
          call ESMF_GridGetItemBounds(grid, ESMF_GRIDITEM_MASK, staggerloc=staggerloc, exclusiveLbound=grid2Lbnd, &
            exclusiveUBound=grid2Ubnd, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        endif
      elseif (gridRank == 3) then
        call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, staggerloc=staggerloc, farrayPtr=gridmask3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
          nullify(gridmask3)
          call ESMF_LogWrite('Disregard five errors above', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        else
          call ESMF_GridGetItemBounds(grid, ESMF_GRIDITEM_MASK, staggerloc=staggerloc, exclusiveLbound=grid3Lbnd, &
            exclusiveUBound=grid3Ubnd, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

          gridmask2 => gridmask3(:,:,1)
        endif
      endif
#endif
    end if

    call ESMF_AttributeGet(field, 'missing_value', isPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (isPresent) then
      call ESMF_AttributeGet(field, 'missing_value', typeKind=mvTypeKind, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (mvTypeKind == ESMF_TYPEKIND_R8) then
        call ESMF_AttributeGet(field, 'missing_value', missingValueR8, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        missingValue = missingValueR8
      elseif (mvTypeKind == ESMF_TYPEKIND_R4) then
        call ESMF_AttributeGet(field, 'missing_value', missingValueR4, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        missingValue = dble(missingValueR4)
      elseif (mvTypeKind == ESMF_TYPEKIND_I8) then
        call ESMF_AttributeGet(field, 'missing_value', missingValueI8, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        missingValue = dble(missingValueI8)
      elseif (mvTypeKind == ESMF_TYPEKIND_I4) then
        call ESMF_AttributeGet(field, 'missing_value', missingValueI4, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        missingValue = dble(missingValueI4)
      else
        write(message,'(A)')  '  missing value of non-implemented type '
        call MOSSCO_FieldString(field, message, rc=localrc)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc=ESMF_RC_NOT_IMPL
        return
      endif
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    if (abs(missingValue) > representableValue) then
      write(message,'(A)')  '  missing value out of range in '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc=ESMF_RC_NOT_IMPL
      return
    endif

    if (rank==4) then

      call  ESMF_FieldGet(field, farrayPtr=farrayPtr4, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      !> @todo We should *not* write into any parts of the pointer, rather make a copy
      if (associated(ncarray4)) deallocate(ncarray4)
      allocate(ncarray4(RANGE4D),stat=localrc)
      ncarray4 = farrayPtr4(RANGE4D)

      if (associated(gridmask3)) then
        do i=lbnd(1),ubnd(1)
          do j=lbnd(2),ubnd(2)
            do k=lbnd(3),ubnd(3)
              if (gridmask3(grid3lbnd(1)-lbnd(1)+i,grid3lbnd(2)-lbnd(2)+j,grid3lbnd(3)-lbnd(3)+k) .le. 0) &
                ncarray4(i,j,k,lbnd(4):ubnd(4))=missingValue
            enddo
          enddo
        enddo
      elseif (associated(gridmask2)) then
        do i=lbnd(1),ubnd(1)
          do j=lbnd(2),ubnd(2)
            if (gridmask2(grid2lbnd(1)-lbnd(1)+i,grid2lbnd(2)-lbnd(2)+j) .le. 0) &
              ncarray4(i,j,lbnd(3):ubnd(3),lbnd(4):ubnd(4))=missingValue
          enddo
        enddo
      end if

      ! it is recommended to check of nans with x /= x, as this is true for NaN
      ! it is recommended to check for inf with abs(x) > huge(x)
      if (checkNaN_ .and. any(ncarray4(RANGE4D) /= ncarray4(RANGE4D))) then
        call self%close()
#ifdef DEBUG_NAN
        if (associated(gridmask3)) then
          do i=lbnd(1),ubnd(1)
            do j=lbnd(2),ubnd(2)
              do k=lbnd(3),ubnd(3)
                if (gridmask3(grid3lbnd(1)-lbnd(1)+i,grid3lbnd(2)-lbnd(2)+j,grid3lbnd(3)-lbnd(3)+k) .le. 0) cycle
                if (any (ncarray4(i,j,k,lbnd(4):ubnd(4)) /= ncarray4(i,j,k,lbnd(4):ubnd(4)) )) then
                  write(message,'(A,3i4)')  '  NaN detected in field ',i,j,k
                  call MOSSCO_FieldString(field, message, rc=localrc)
                  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
                endif
              enddo
            enddo
          enddo
        elseif (associated(gridmask2)) then
          do i=lbnd(1),ubnd(1)
            do j=lbnd(2),ubnd(2)
              if (gridmask2(grid2lbnd(1)-lbnd(1)+i,grid2lbnd(2)-lbnd(2)+j) .le. 0) cycle
              if (any (ncarray4(i,j,lbnd(3):ubnd(3),lbnd(4):ubnd(4)) /= ncarray4(i,j,lbnd(3):ubnd(3),lbnd(4):ubnd(4)) )) then
                write(message,'(A,3i4)')  '  NaN detected in field ',i,j
                call MOSSCO_FieldString(field, message, rc=localrc)
                call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
              endif
            enddo
          enddo
        end if
#else
        write(message,'(A)')  '  NaN detected in field '
        call MOSSCO_FieldString(field, message, rc=localrc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
#endif
        if (present(rc)) rc = ESMF_RC_VAL_OUTOFRANGE
      endif

      where (ncarray4(RANGE4D) /= ncarray4(RANGE4D))
        ncarray4(RANGE4D)=missingValue
      endwhere

      if (any(abs(ncarray4(RANGE4D)) > representableValue)) then
        write(message,'(A)')  '-- Inf detected in field '
        call MOSSCO_FieldString(field, message, rc=localrc)

        if (checkInf_) then
          call self%close()
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
#ifdef DEBUG_INF
          if (associated(gridmask3)) then
            do i=lbnd(1),ubnd(1)
              do j=lbnd(2),ubnd(2)
                do k=lbnd(3),ubnd(3)
                  if (gridmask3(grid3lbnd(1)-lbnd(1)+i,grid3lbnd(2)-lbnd(2)+j,grid3lbnd(3)-lbnd(3)+k) .le. 0) cycle
                  if (any( abs(ncarray4(i,j,k,lbnd(4):ubnd(4))) > representableValue )) then
                    write(message,'(A,3i4)')  '  INF detected in field ',i,j,k
                    call MOSSCO_FieldString(field, message, rc=localrc)
                    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
                  endif
                enddo
              enddo
            enddo
          elseif (associated(gridmask2)) then
            do i=lbnd(1),ubnd(1)
              do j=lbnd(2),ubnd(2)
                if (gridmask2(grid2lbnd(1)-lbnd(1)+i,grid2lbnd(2)-lbnd(2)+j) .le. 0) cycle
                if (any( abs(ncarray4(i,j,lbnd(3):ubnd(3),lbnd(4):ubnd(4))) > representableValue )) then
                  write(message,'(A,3i4)')  '  INF detected in field ',i,j
                  call MOSSCO_FieldString(field, message, rc=localrc)
                  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
                endif
              enddo
            enddo
          else
            i=lbnd(1)
            j=lbnd(2)
            do k=lbnd(3),ubnd(3)
              if ( abs(ncarray3(i,j,k)) > representableValue ) then
                write(message,'(A,3i4)')  '  INF detected in field ',i,j,k
                call MOSSCO_FieldString(field, message, rc=localrc)
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
                exit
              endif
            enddo
          endif
#endif
          if (present(rc)) rc = ESMF_RC_VAL_OUTOFRANGE
          return
        endif
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
        write(message,'(A)')  '-- Inf values replaced with missing value '
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
      endif

      where (abs(ncarray4) > RepresentableValue)
        ncarray4=missingValue
      endwhere

      if (any(var%dimids==self%timeDimId)) then
        ncStatus = nf90_put_var(self%ncid, var%varid, ncarray4(RANGE4D), &
        start=(/1,1,1,1,dimlen/))
      else
        ncStatus = nf90_put_var(self%ncid, var%varid, ncarray4(RANGE4D))
      endif
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', could not write variable '//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_FILE_WRITE
        return
      endif

      if (associated(ncarray4)) deallocate(ncarray4); nullify(ncarray4)

    elseif (rank==3) then

      call  ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (associated(ncarray3)) deallocate(ncarray3)
      allocate(ncarray3(RANGE3D),stat=localrc)
      ncarray3(RANGE3D) = farrayPtr3(RANGE3D)
      if (associated(gridmask3)) then
        do i=lbnd(1),ubnd(1)
          do j=lbnd(2),ubnd(2)
            do k=lbnd(3),ubnd(3)
              if (gridmask3(grid3lbnd(1)-lbnd(1)+i,grid3lbnd(2)-lbnd(2)+j,grid3lbnd(3)-lbnd(3)+k) .le. 0) &
                ncarray3(i,j,k)=missingValue
             enddo
          enddo
        enddo
      elseif (associated(gridmask2)) then
        do i=lbnd(1),ubnd(1)
          do j=lbnd(2),ubnd(2)
            if (gridmask2(grid2lbnd(1)-lbnd(1)+i,grid2lbnd(2)-lbnd(2)+j) .le. 0) &
                ncarray3(i,j,lbnd(3):ubnd(3))=missingValue
          enddo
        enddo
      end if

      if (checkNaN_ .and. any(ncarray3(RANGE3D) /= ncarray3(RANGE3D))) then

        call self%close()

#ifdef DEBUG_NAN
        if (associated(gridmask3)) then
          do i=lbnd(1),ubnd(1)
            do j=lbnd(2),ubnd(2)
              do k=lbnd(3),ubnd(3)
                if (gridmask3(grid3lbnd(1)-lbnd(1)+i,grid3lbnd(2)-lbnd(2)+j,grid3lbnd(3)-lbnd(3)+k) .le. 0) cycle
                if ( ncarray3(i,j,k) /= ncarray3(i,j,k) ) then
                  write(message,'(A,3i4)')  '  NaN detected in field ',i,j,k
                  call MOSSCO_FieldString(field, message, rc=localrc)
                  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
                  exit
                endif
              enddo
            enddo
          enddo
        elseif (associated(gridmask2)) then
          do i=lbnd(1),ubnd(1)
            do j=lbnd(2),ubnd(2)
              if (gridmask2(grid2lbnd(1)-lbnd(1)+i,grid2lbnd(2)-lbnd(2)+j) .le. 0) cycle
              if (any (ncarray3(i,j,lbnd(3):ubnd(3)) /= ncarray3(i,j,lbnd(3):ubnd(3)) )) then
                write(message,'(A,3i4)')  '  NaN detected in field ',i,j
                call MOSSCO_FieldString(field, message, rc=localrc)
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
                exit
              endif
            enddo
          enddo
        else
          i=lbnd(1)
          j=lbnd(2)
          do k=lbnd(3),ubnd(3)
            if ( ncarray3(i,j,k) /= ncarray3(i,j,k) ) then
              write(message,'(A,3i4)')  '  NaN detected in field ',i,j,k
              call MOSSCO_FieldString(field, message, rc=localrc)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
              exit
            endif
          enddo
        end if
#else
        write(message,'(A)')  '  NaN detected in field '
        call MOSSCO_FieldString(field, message, rc=localrc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
#endif
        if (present(rc)) rc = ESMF_RC_VAL_OUTOFRANGE
        return
      endif

      where (ncarray3(RANGE3D) /= ncarray3(RANGE3D))
        ncarray3(RANGE3D) = missingValue
      endwhere

      if (any(abs(ncarray3(RANGE3D)) > representableValue)) then
        write(message,'(A)')  '-- Inf detected in field '
        call MOSSCO_FieldString(field, message, rc=localrc)

        if (checkInf_) then
          call self%close()
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
#ifdef DEBUG_INF
          if (associated(gridmask3)) then
            do i=lbnd(1),ubnd(1)
              do j=lbnd(2),ubnd(2)
                do k=lbnd(3),ubnd(3)
                  if (gridmask3(grid3lbnd(1)-lbnd(1)+i,grid3lbnd(2)-lbnd(2)+j,grid3lbnd(3)-lbnd(3)+k) .le. 0) cycle
                  if ( ncarray3(i,j,k) > representableValue ) then
                    write(message,'(A,3i4)')  '  INF detected in field ',i,j,k
                    call MOSSCO_FieldString(field, message, rc=localrc)
                    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
                    exit
                  endif
                enddo
              enddo
            enddo
          elseif (associated(gridmask2)) then
            do i=lbnd(1),ubnd(1)
              do j=lbnd(2),ubnd(2)
                if (gridmask2(grid2lbnd(1)-lbnd(1)+i,grid2lbnd(2)-lbnd(2)+j) .le. 0) cycle
                if (any( abs(ncarray3(i,j,lbnd(3):ubnd(3))) > representableValue )) then
                  write(message,'(A,3i4)')  '  INF detected in field ',i,j
                  call MOSSCO_FieldString(field, message, rc=localrc)
                  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
                  exit
                endif
              enddo
            enddo
          else
            i=lbnd(1)
            j=lbnd(2)
            do k=lbnd(3),ubnd(3)
              if ( abs(ncarray3(i,j,k)) > representableValue ) then
                write(message,'(A,3i4)')  '  INF detected in field ',i,j,k
                call MOSSCO_FieldString(field, message, rc=localrc)
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
                exit
              endif
            enddo
          endif
#endif
          if (present(rc)) rc = ESMF_RC_VAL_OUTOFRANGE
          return
        endif
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
        write(message,'(A)')  '-- Inf values replaced with missing value '
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
      endif

      where (abs(ncarray3(RANGE3D)) > representableValue)
        ncarray3(RANGE3D) = missingValue
      endwhere

      if (any(var%dimids==self%timeDimId)) then
        ncStatus = nf90_put_var(self%ncid, var%varid, real(ncarray3(RANGE3D)), &
        start=(/1,1,1,dimlen/))
      else
        ncStatus = nf90_put_var(self%ncid, var%varid, real(ncarray3(RANGE3D)))
      endif
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', could not write variable '//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_FILE_WRITE
        return
      endif

      if (associated(ncarray3)) deallocate(ncarray3); nullify(ncarray3)

    elseif (rank==2) then

      call  ESMF_FieldGet(field, farrayPtr=farrayPtr2, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (associated(ncarray2)) deallocate(ncarray2)
      allocate(ncarray2(RANGE2D),stat=localrc)
      ncarray2 = farrayPtr2(RANGE2D)

      if (associated(gridmask2)) then
        do i=lbnd(1),ubnd(1)
          do j=lbnd(2),ubnd(2)
            if (gridmask2(grid2lbnd(1)-lbnd(1)+i,grid2lbnd(2)-lbnd(2)+j) .le. 0) &
                ncarray2(i,j)=missingValue
          enddo
        enddo
      end if

      if (checkNaN_ .and. any(ncarray2(RANGE2D) /= ncarray2(RANGE2D))) then
        call self%close()

#ifdef DEBUG_NAN
        if (associated(gridmask2)) then
          do i=lbnd(1),ubnd(1)
            do j=lbnd(2),ubnd(2)
              if (gridmask2(grid2lbnd(1)-lbnd(1)+i,grid2lbnd(2)-lbnd(2)+j) .le. 0) cycle
              if ( ncarray2(i,j) /= ncarray2(i,j) ) then
                write(message,'(A,3i4)')  '  NaN detected in field ',i,j
                call MOSSCO_FieldString(field, message, rc=localrc)
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
              endif
            enddo
          enddo
        else
          i=lbnd(1)
          j=lbnd(2)
          if ( ncarray2(i,j) /= ncarray2(i,j) ) then
            write(message,'(A,3i4)')  '  NaN detected in field ',i,j
            call MOSSCO_FieldString(field, message, rc=localrc)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
          endif
        end if
#else
        write(message,'(A)')  '  NaN detected in field '
        call MOSSCO_FieldString(field, message, rc=localrc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
#endif
        if (present(rc)) rc = ESMF_RC_VAL_OUTOFRANGE
        return
      endif

      where (ncarray2(RANGE2D) /= ncarray2(RANGE2D))
        ncarray2(RANGE2D) = missingValue
      endwhere

      if (any(abs(ncarray2(RANGE2D)) > representableValue)) then
        write(message,'(A)')  '-- Inf detected in field '
        call MOSSCO_FieldString(field, message, rc=localrc)

        if (checkInf_) then
          call self%close()
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
#ifdef DEBUG_INF
        if (associated(gridmask2)) then
          do i=lbnd(1),ubnd(1)
            do j=lbnd(2),ubnd(2)
              if (gridmask2(grid2lbnd(1)-lbnd(1)+i,grid2lbnd(2)-lbnd(2)+j) .le. 0) cycle
              if ( abs(ncarray2(i,j)) > representableValue ) then
                write(message,'(A,3i4)')  '  INF detected in field ',i,j
                call MOSSCO_FieldString(field, message, rc=localrc)
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
              endif
            enddo
          enddo
        else
          i=lbnd(1)
          j=lbnd(2)
          if ( abs(ncarray2(i,j)) > representableValue ) then
            write(message,'(A,3i4)')  '  INF detected in field ',i,j
            call MOSSCO_FieldString(field, message, rc=localrc)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
          endif
        end if
#endif
          if (present(rc)) rc = ESMF_RC_VAL_OUTOFRANGE
          return
        endif
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
        write(message,'(A)')  '-- Inf values replaced with missing value '
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
      endif

      where (abs(ncarray2(RANGE2D)) > representableValue)
        ncarray2(RANGE2D) = missingValue
      endwhere

      if (any(var%dimids==self%timeDimId)) then
        ncStatus = nf90_put_var(self%ncid, var%varid, ncarray2(RANGE2D), &
        start=(/1,1,dimlen/))
      else
        ncStatus = nf90_put_var(self%ncid, var%varid, ncarray2(RANGE2D))
      endif
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', could not write variable '//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_FILE_WRITE
        return
      endif

      if (associated(ncarray2)) deallocate(ncarray2); nullify(ncarray2)

    elseif (rank==1) then

      call  ESMF_FieldGet(field, farrayPtr=farrayPtr1, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (associated(ncarray1)) deallocate(ncarray1)
      allocate(ncarray1(RANGE1D),stat=localrc)
      ncarray1 = farrayPtr1(RANGE1D)

      if (checkNaN_ .and. any(ncarray1(RANGE1D) /= ncarray1(RANGE1D))) then
        call self%close()
#ifdef DEBUG_NAN
#else
        write(message,'(A)')  '  NaN detected in field '
        call MOSSCO_FieldString(field, message, rc=localrc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
#endif
        if (present(rc)) rc = ESMF_RC_VAL_OUTOFRANGE
        return
      endif

      where (ncarray1(RANGE1D) /= ncarray1(RANGE1D))
        ncarray1(RANGE1D) = missingValue
      endwhere

      if (any(abs(ncarray1(RANGE1D)) > representableValue)) then
        write(message,'(A)')  '-- Inf detected in field '
        call MOSSCO_FieldString(field, message, rc=localrc)

        if (checkInf_) then
          call self%close()
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
#ifdef DEBUG_INF
#endif
          if (present(rc)) rc = ESMF_RC_VAL_OUTOFRANGE
          return
        endif
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
        write(message,'(A)')  '-- Inf values replaced with missing value '
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
      endif

      where (abs(ncarray1(RANGE1D)) > representableValue)
        ncarray1(RANGE1D) = missingValue
      endwhere

      if (any(var%dimids==self%timeDimId)) then
        ncStatus = nf90_put_var(self%ncid, var%varid, ncarray1(RANGE1D), &
        start=(/1,dimlen/))
      else
        ncStatus = nf90_put_var(self%ncid, var%varid, ncarray1(RANGE1D))
      endif
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', could not write variable '//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_FILE_WRITE
        return
      endif

      if (associated(ncarray1)) deallocate(ncarray1)

    endif

    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(lbnd)) deallocate(lbnd)
    if (allocated(exclusiveCount)) deallocate(exclusiveCount)

    nullify(gridmask2)
    nullify(gridmask3)

  end subroutine mossco_netcdf_variable_put

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_variable_present"
  function mossco_netcdf_variable_present(self,name) result(varpresent)

    class(type_mossco_netcdf)          :: self
    character(len=*)                   :: name
    logical                            :: varpresent

    integer                            :: ncStatus,varid
    varpresent = .false.
    ncStatus = nf90_inq_varid(self%ncid,name,varid)
    if (ncStatus == NF90_NOERR) varpresent=.true.

  end function mossco_netcdf_variable_present

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_variable_create"
  subroutine mossco_netcdf_variable_create(self, field, name, precision, rc)

    class(type_mossco_netcdf)        :: self
    type(ESMF_Field), intent(inout)  :: field
    character(len=*),optional        :: name
    character(len=*),optional        :: precision
    integer, intent(out), optional   :: rc

    type(ESMF_Grid)                :: grid
    type(ESMF_Mesh)                :: mesh
    type(ESMF_LocStream)           :: locstream
    type(ESMF_XGrid)               :: xgrid
    character(len=ESMF_MAXSTR)     :: varname, geomName, fieldname, coordinates=''
    character(len=ESMF_MAXSTR)     :: units='', attributeName, string, message
    integer                        :: ncStatus,esmfrc,rc_,varid,dimcheck=0
    integer                        :: dimids_1d(2), dimids_2d(3), dimids_3d(4), rank
    integer, dimension(:),pointer  :: dimids, tmpDimids
    type(ESMF_CoordSys_Flag)       :: coordSys
    character(len=ESMF_MAXSTR), dimension(3) :: coordNames=(/'x','y','z'/)
    integer                        :: external_index=-1
    real(ESMF_KIND_R8)             :: mean_diameter, real8
    real(ESMF_KIND_R4)             :: real4
    integer(ESMF_KIND_I4)          :: i, attributeCount, int4, dimCount, ungriddedDimCount
    integer(ESMF_KIND_I8)          :: int8
    type(ESMF_TypeKind_Flag)       :: typekind
    type(ESMF_GeomType_Flag)       :: geomType
    type(ESMF_StaggerLoc)          :: staggerloc
    type(ESMF_MeshLoc)             :: meshLoc
    integer                        :: ungriddedID, ungriddedLength,dimrank
    integer(ESMF_KIND_I4), allocatable, dimension(:) :: uubnd,ulbnd
    logical                        :: isPresent
    real(ESMF_KIND_R4)             :: missingValueR4=-1E30
    real(ESMF_KIND_R8)             :: missingValueR8=-1D30
    integer(ESMF_KIND_I4)          :: missingValueI4=-9999
    integer(ESMF_KIND_I8)          :: missingValueI8=-9999
    integer(ESMF_KIND_I4)          :: localrc
    character(len=11)              :: precision_

    integer :: petCount, localPet, vas, ssiId, peCount
    type(ESMF_Vm)                  :: vm
    integer, allocatable           :: iarray1(:), iarray2(:,:), iarray3(:,:,:), dimlen(:)

    integer(ESMF_KIND_I4),allocatable   :: ubnd(:), lbnd(:)
    real(ESMF_KIND_R8),pointer     :: farrayPtr1(:), farrayPtr2(:,:), farrayPtr3(:,:,:)

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field,name=fieldname,rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    varname = trim(fieldname)
    if (present(name)) varname=trim(name)

    !> return if variable is already defined in netcdf file
    if (self%variable_present(varname)) return

    call ESMF_FieldGet(field, geomType=geomType, dimCount=dimCount, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (geomType==ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, staggerloc=staggerloc, grid=grid, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_GridGet(grid, name=geomName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      dimids => self%grid_dimensions(grid, staggerloc)

      call ESMF_GridGet(grid, coordSys=coordSys,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    elseif (geomType==ESMF_GEOMTYPE_MESH) then
      call ESMF_FieldGet(field, mesh=mesh, meshloc=meshloc, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(geomname,'(A)') 'mesh'
      dimids => self%mesh_dimensions(field)

      call ESMF_MeshGet(mesh, coordSys=coordSys, rc=localrc)
      dimCount = 1

    elseif (geomType==ESMF_GEOMTYPE_LOCSTREAM) then
      call ESMF_FieldGet(field, locStream=locStream, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(geomname,'(A)') 'locstream'
      dimids => self%locstream_dimensions(field)

      call ESMF_LocStreamGet(locStream, coordSys=coordSys, rc=localrc)
      dimCount = 1

    elseif (geomType==ESMF_GEOMTYPE_XGRID) then
      write(message,'(A)')  '  geometry type XGRID cannot be handled yet'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
      return
    endif

    if (coordSys == ESMF_COORDSYS_SPH_DEG) then
      coordnames=(/'lon  ','lat  ','level'/)
    elseif (coordSys == ESMF_COORDSYS_SPH_RAD) then
      coordnames=(/'lon  ','lat  ','level'/)
    else
      coordnames=(/'x','y','z'/)
    endif

    !> enter definition mode to use netcdf_write commands
    ncStatus = nf90_redef(self%ncid)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot enter definition mode', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc=ESMF_RC_FILE_WRITE
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT)) return
      else
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      endif
    endif

    !! add ungridded dimension
    ! ask field for ungridded dimension
    dimrank=ubound(dimids,1)
    ungriddedDimCount=dimCount-dimrank+1
    if (ungriddedDimCount .ge. 1) then
      allocate(ulbnd(ungriddedDimCount))
      allocate(uubnd(ungriddedDimCount))
      call ESMF_FieldGet(field, ungriddedLBound=ulbnd, ungriddedUBound=uubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

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

    call replace_character(geomName, ' ', '_')

    !> The CF-standard demands that only the 2D (lat lon) coordinates are written
    !> to this attribute.  We assume that these are the first two coordinates.
    if (geomType == ESMF_GEOMTYPE_GRID .and. dimRank >= 2) then
      write(coordinates,'(A)') trim(geomName)//'_'//trim(coordnames(1))
      write(coordinates,'(A)') trim(coordinates)//' '//trim(geomName)//'_'//trim(coordnames(2))
    endif

    !! define variable
    if (present(precision)) then
      precision_=precision
    else
      precision=self%precision
    endif

    call ESMF_FieldGet(field, typeKind=typeKind, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (typekind==ESMF_TYPEKIND_I4) then
      ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_INT,dimids,varid)
    elseif (typekind==ESMF_TYPEKIND_I8) then
      ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_INT,dimids,varid)
    elseif (typekind==ESMF_TYPEKIND_R4) then
      ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_REAL,dimids,varid)
    elseif (typekind==ESMF_TYPEKIND_R8) then
      if (precision=='NF90_DOUBLE') then
        ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_DOUBLE,dimids,varid)
      else
        ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_REAL,dimids,varid)
      endif
    elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
      !> @todo
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    elseif (typekind==ESMF_TYPEKIND_LOGICAL) then
      !> @todo
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot define variable '//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    if (typeKind == ESMF_TYPEKIND_R8) then
      call ESMF_AttributeGet(field, 'numeric_precision', isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.isPresent) call ESMF_AttributeSet(field, 'numeric_precision', trim(precision), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    endif

    !call ESMF_AttributeGet(field, 'standard_name', isPresent=isPresent, rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !if (.not.isPresent) then
    !  write(message,'(A)')  '  field '//trim(fieldName)//' has no standard_name attribute. Using its name instead.'
    !  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    !  call ESMF_AttributeSet(field, 'standard_name', trim(fieldName), rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    !    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !endif
    call ESMF_AttributeGet(field, 'long_name', isPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (.not.isPresent) call ESMF_AttributeSet(field, 'long_name', trim(fieldName), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_AttributeGet(field, 'coordinates', isPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (.not.isPresent) call ESMF_AttributeSet(field, 'coordinates', trim(coordinates), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (typeKind == ESMF_TYPEKIND_R4) then
      call ESMF_AttributeGet(field, 'missing_value', isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.isPresent) call ESMF_AttributeSet(field, 'missing_value', missingValueR4, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_AttributeGet(field, '_FillValue', isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.isPresent) call ESMF_AttributeSet(field, '_FillValue', missingValueR4, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    elseif (typeKind == ESMF_TYPEKIND_R8) then
      call ESMF_AttributeGet(field, 'missing_value', isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.isPresent) call ESMF_AttributeSet(field, 'missing_value', missingValueR8, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_AttributeGet(field, '_FillValue', isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.isPresent) call ESMF_AttributeSet(field, '_FillValue', missingValueR8, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    elseif (typeKind == ESMF_TYPEKIND_I4) then
      call ESMF_AttributeGet(field, 'missing_value', isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.isPresent) call ESMF_AttributeSet(field, 'missing_value', missingValueI4, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_AttributeGet(field, '_FillValue', isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.isPresent) call ESMF_AttributeSet(field, '_FillValue', missingValueI4, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    elseif (typeKind == ESMF_TYPEKIND_I8) then
      call ESMF_AttributeGet(field, 'missing_value', isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.isPresent) call ESMF_AttributeSet(field, 'missing_value', missingValueI8, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_AttributeGet(field, '_FillValue', isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (.not.isPresent) call ESMF_AttributeSet(field, '_FillValue', missingValueI8, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    !call MOSSCO_FieldLog(field)
    call MOSSCO_AttributeNetcdfWrite(field, self%ncid, varid=varid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    ncStatus = nf90_enddef(self%ncid)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot end definition mode', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    !> @todo remove time from dimensions
    varname='pet_'//trim(geomName)
    if (.not.self%variable_present(varname)) then

      ncStatus = nf90_redef(self%ncid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot enter definition mode', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      endif

      if (geomType==ESMF_GEOMTYPE_GRID) then
        dimids => self%grid_dimensions(grid)
      elseif (geomType==ESMF_GEOMTYPE_MESH) then
        dimids => self%mesh_dimensions(field)
      endif

      ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_INT,dimids(1:ubound(dimids,1)-1),varid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot define variable '//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
      call self%putattstring(varid,'mossco_name','persistent_execution_thread', rc=localrc)
      ncStatus = nf90_put_att(self%ncid,varid,'long_name','persistent execution thread')
      ncStatus = nf90_put_att(self%ncid,varid,'coordinates',trim(coordinates))
      ncStatus = nf90_put_att(self%ncid,varid,'units','1')
      ncStatus = nf90_put_att(self%ncid,varid,'missing_value',-1)
      ncStatus = nf90_put_att(self%ncid,varid,'valid_min',0)
      ncStatus = nf90_put_att(self%ncid,varid,'_FillValue',-1)

      call ESMF_VMGetGlobal(vm=vm, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, peCount=peCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      ncStatus = nf90_put_att(self%ncid,varid,'vm_pet_count',petCount)
      ncStatus = nf90_put_att(self%ncid,varid,'vm_processing_element_count',peCount)

      call ESMF_VMGet(vm, localPet, peCount=peCount, ssiId=ssiId, vas=vas, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      ncStatus = nf90_put_att(self%ncid,varid,'pet_virtual_address_space',vas)
      ncStatus = nf90_put_att(self%ncid,varid,'pet_single_system_image_id',ssiId)
      ncStatus = nf90_put_att(self%ncid,varid,'pet_processing_element_count',peCount)

      ncStatus = nf90_enddef(self%ncid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot end definition mode', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      endif

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
    endif ! present(pet_var)

    !> If there is an item in the grid with GRIDITEM_AREA, then write
    !> this item to the netcdf file with standard name area
    !> @todo implement this for meshes, too

    if (geomType==ESMF_GEOMTYPE_GRID) then
      call ESMF_GridGetItem(grid, itemFlag=ESMF_GRIDITEM_AREA, &
        staggerloc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    else
      isPresent = .false.
    endif

    varname='cell_area_'//trim(geomName)
    if (.not.self%variable_present(varname) .and. isPresent) then

      ncStatus = nf90_redef(self%ncid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot enter definition mode', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      endif

      dimids => self%grid_dimensions(grid)

      ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_FLOAT,dimids(1:ubound(dimids,1)-1),varid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot define variable '//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
      ncStatus = nf90_put_att(self%ncid,varid,'standard_name','area')
      ncStatus = nf90_put_att(self%ncid,varid,'mossco_name','cell_area')
      ncStatus = nf90_put_att(self%ncid,varid,'long_name','Area of grid cell from grid'//trim(geomName))
      ncStatus = nf90_put_att(self%ncid,varid,'coordinates',trim(coordinates))
      ncStatus = nf90_put_att(self%ncid,varid,'units','m-2')
      ncStatus = nf90_put_att(self%ncid,varid,'missing_value',-1.0E30)
      ncStatus = nf90_put_att(self%ncid,varid,'_FillValue',-1.0E30)

      ncStatus = nf90_enddef(self%ncid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot end definition mode', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, ESMF_ERR_PASSTHRU, ESMF_CONTEXT)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      allocate(dimlen(ubound(dimids,1)-1))
      do i=1,ubound(dimids,1)-1
        ncStatus = nf90_inquire_dimension(self%ncid,dimids(i),len=dimlen(i))
      enddo

      if (allocated(ubnd)) deallocate(ubnd)
      if (allocated(lbnd)) deallocate(lbnd)
      allocate(ubnd(ubound(dimids,1)-1))
      allocate(lbnd(ubound(dimids,1)-1))
      call ESMF_GridGetItemBounds(grid, itemFlag=ESMF_GRIDITEM_AREA, &
        staggerloc=ESMF_STAGGERLOC_CENTER, exclusiveLBound=lbnd, &
        exclusiveUbound=ubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (ubound(dimids,1)==2) then
        call ESMF_GridGetItem(grid, itemFlag=ESMF_GRIDITEM_AREA, &
          staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        ncStatus = nf90_put_var(self%ncid, varid, farrayPtr1(RANGE1D))
        nullify(farrayPtr1)
      elseif (ubound(dimids,1)==3) then
        call ESMF_GridGetItem(grid, itemFlag=ESMF_GRIDITEM_AREA, &
          staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        ncStatus = nf90_put_var(self%ncid, varid, farrayPtr2(RANGE2D))
        nullify(farrayPtr2)
      elseif (ubound(dimids,1)==4) then
        call ESMF_GridGetItem(grid, itemFlag=ESMF_GRIDITEM_AREA, &
          staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=farrayPtr3, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        ncStatus = nf90_put_var(self%ncid, varid, farrayPtr3(RANGE3D))
        nullify(farrayPtr3)
      endif
      deallocate(dimlen)
      deallocate(ubnd)
      deallocate(lbnd)
    endif ! present(pet_var) .and. isPresent

    call self%update_variables()
    call self%update()

  end subroutine mossco_netcdf_variable_create

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_add_timestep"
  subroutine mossco_netcdf_add_timestep(self, seconds, rc)

    class(type_mossco_netcdf)        :: self
    real(ESMF_KIND_R8), intent(in)   :: seconds
    integer(ESMF_KIND_I4), intent(out), optional  :: rc

    character(ESMF_MAXSTR)           :: message
    integer                          :: ncStatus, dimlen, varid, rc_, localrc
    real(ESMF_KIND_R8)               :: maxSeconds, wallSecond
    real(ESMF_KIND_R8), allocatable  :: time(:), wallSeconds(:)

    type(ESMF_Time)                  :: refTime, wallTime
    type(ESMF_TimeInterval)          :: timeInterval
    integer(ESMF_KIND_I4)            :: doy, yy
    character(ESMF_MAXSTR)           :: timeString, refTimeISOString

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_SUCCESS

    if (self%timeDimid < 0) then
      call self%init_time(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
        call ESMF_LogWrite('  cannot initialize time', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_WRITE
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      return
      endif
    endif

    ncStatus = nf90_inquire_dimension(self%ncid, self%timedimid, len=dimlen)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot find time dimension',ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_NOT_FOUND
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    ncStatus = nf90_inq_varid(self%ncid, 'time', varid)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot find time variable',ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_NOT_FOUND
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    if (dimlen>0) then
      allocate(time(dimlen))

      ncStatus = nf90_get_var(self%ncid, varid, time)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot read variable time',ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_READ
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      maxSeconds=time(dimlen)
    else
      maxSeconds = -1.0D0
    endif

    if (maxSeconds >  seconds) then
      write(message,'(A,ES10.3,A,ES10.3,A)') '   addition of non-monotonic time ',seconds,' < ',maxSeconds,' not possible (yet)'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_VAL_WRONG
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    elseif (maxSeconds<seconds) then
      !write(message,'(A,ES10.3,A,ES10.3)') '   addition of monotonic time ',seconds,' > ',maxSeconds
      !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      !write(message,'(A,I2,A,I2)') '   varid = ',varid,' start = ',dimlen + 1
      !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

      ncStatus = nf90_put_var(self%ncid, varid, seconds, start=(/dimlen+1/))
      if (ncStatus /= NF90_NOERR) then
        write(message, '(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot write variable time'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_WRITE
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      ncStatus = nf90_inq_varid(self%ncid, 'doy', varid)
      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot find variable doy'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_NOT_FOUND
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      call self%reftime(refTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_TimeIntervalSet(timeInterval, startTime=refTime, s_r8=seconds, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_TimeGet(refTime + timeInterval, dayOfYear=doy, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      ncStatus = nf90_put_var(self%ncid, varid, doy, start=(/dimlen+1/))
      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot write variable doy'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_WRITE
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      ncStatus = nf90_inq_varid(self%ncid, 'year', varid)
      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot find variable year'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_NOT_FOUND
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      call ESMF_TimeGet(refTime + timeInterval, yy=yy, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

       ncStatus = nf90_put_var(self%ncid, varid, yy, start=(/dimlen+1/))
      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot write variable year'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_WRITE
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif



      ncStatus = nf90_inq_varid(self%ncid, 'date_string', varid)
      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot find variable date_string'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_NOT_FOUND
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      call ESMF_TimeGet(refTime + timeInterval, timeStringISOFrac=timeString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      ncStatus = nf90_put_var(self%ncid, varid, timeString(1:19), start=(/1,dimlen+1/))
      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot write variable date_string'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_WRITE
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      ncStatus = nf90_inq_varid(self%ncid, 'wallclock_time', varid)
      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot find variable wallclock_time'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_NOT_FOUND
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      call ESMF_TimeSet(wallTime, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_TimeSyncToRealTime(wallTime, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_TimeGet(wallTime, timeStringISOFrac=timeString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      ncStatus = nf90_put_var(self%ncid, varid, timeString(1:19), start=(/1,dimlen+1/))
      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot write variable wallclock_time'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_WRITE
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      ncStatus = nf90_inq_varid(self%ncid, 'elapsed_wallclock_time', varid)
      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot find variable elapsed_wallclock_time'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_NOT_FOUND
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      if (dimLen > 0) then
        allocate(wallSeconds(dimLen))
        ncStatus = nf90_get_var(self%ncid, varid, wallSeconds)
        if (ncStatus /= NF90_NOERR) then
          write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot read variable elapsed_wallclock_time'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
          if (present(rc)) then
            rc = ESMF_RC_FILE_READ
            return
          else
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif
        endif
      endif

      call ESMF_VMWtime(wallSecond, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ncStatus = nf90_put_var(self%ncid, varid, wallsecond, start=(/dimlen+1/))
      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot write variable elapsed_wallclock_time'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_WRITE
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      ncStatus = nf90_inq_varid(self%ncid, 'speedup', varid)
      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot find variable speedup'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_NOT_FOUND
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      if (dimLen > 0) then
        ncStatus = nf90_put_var(self%ncid, varid, (seconds - time(dimLen)) / ( wallSecond-wallSeconds(dimLen) ), start=(/dimlen+1/))
      else
        ncStatus = nf90_put_var(self%ncid, varid, -1D30)
      endif

      if (ncStatus /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(ncStatus))//', cannot write variable speedup'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_WRITE
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif


      write(message,'(A,I4,A,F10.0,A)') '  added timestep ',dimlen+1,' (', seconds,' s) to file'
      call MOSSCO_MessageAdd(message,' '//trim(self%name))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    else
      write(message,'(A,I4,A,F10.0,A)') '  did not add existing timestep ',dimlen,' (', seconds,' s) to file'
      call MOSSCO_MessageAdd(message,' '//trim(self%name))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    endif

    if (allocated(wallSeconds)) deallocate(wallSeconds)
    if (allocated(time)) deallocate(time)

  end subroutine mossco_netcdf_add_timestep

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_close"
  subroutine mossco_netcdf_close(self,rc)

    class(type_mossco_netcdf)      :: self
    integer, optional, intent(out) :: rc

    integer                        :: ncStatus

    ncStatus = nf90_close(self%ncid)
    if (present(rc)) rc = ncStatus

  end subroutine mossco_netcdf_close

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_NetcdfOpen"
  function MOSSCO_NetcdfOpen(filename, kwe, timeUnit, state, mode, checkVersion, rc) result(nc)

    character(len=*), intent(in)               :: filename
    logical, intent(in), optional              :: kwe
    character(len=*), optional, intent(inout)  :: timeUnit
    type(ESMF_State), optional, intent(inout)  :: state
    character(len=1), optional, intent(in)     :: mode
    logical, optional, intent(in)              :: checkVersion
    integer, intent(out), optional             :: rc
    type(type_mossco_netcdf)                   :: nc

    integer                       :: localrc, rc_, varid
    character(len=1)              :: mode_
    character(len=255)            :: timeUnit_, string, message
    logical                       :: fileIsPresent, checkVersion_
    integer                       :: fileUnit=1555

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_SUCCESS

    if (present(checkVersion)) then
      checkVersion_ = checkVersion
    else
      checkVersion_ = .true.
    endif

    if (present(mode)) then
      mode_= mode
    else
      mode_ = 'W'
    endif

    if (mode_ == 'W' .or. mode_ == 'w') then
      localrc = nf90_open(trim(filename), mode=NF90_WRITE, ncid=nc%ncid)

      if (localrc /= NF90_NOERR) then
        if (present(timeUnit) .and. present(state))  then
          nc = MOSSCO_NetcdfCreate(trim(filename), timeUnit=trim(timeUnit), state=state, rc = rc_)
        elseif (present(timeUnit) .and. .not. present(state))  then
          nc = MOSSCO_NetcdfCreate(trim(filename), timeUnit=trim(timeUnit), rc = rc_)
        elseif (present(state) .and. .not. present(timeUnit))  then
          nc = MOSSCO_NetcdfCreate(trim(filename), state=state, rc = rc_)
        else
          nc = MOSSCO_NetcdfCreate(trim(filename), rc = rc_)
        endif
      endif
      localrc = nf90_inq_dimid(nc%ncid,  'time', nc%timeDimId)
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite('-- '//trim(fileName)//' has no time dimension', ESMF_LOGMSG_WARNING)
        nc%timeDimID=-1
      endif
    else
      inquire(file=trim(fileName), exist=fileIsPresent)
      if (.not.fileIsPresent) then
        call ESMF_LogWrite('  file '//trim(filename)//' does not exist', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_OPEN
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      !> @todo what happens with a broken link?

      !> read the first 4 bytes of the file
      fileUnit=MOSSCO_GetFreeLun(start=fileUnit)
      open(file=trim(fileName), unit=fileUnit, form='formatted', recl=4)
      read(fileUnit, '(A)') string
      close(fileUnit)
      if (string(1:3) == 'CDF') then
        write(message,'(A)')  '  file '//trim(fileName)//' is in netCDF3 format'
        if (checkVersion_) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (string(2:4) == 'HDF') then
        write(message,'(A)')  '  file '//trim(fileName)//' is in netCDF4 format'
        if (checkVersion_) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        write(message,'(A)')  '  file '//trim(fileName)//' has unknown format'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      endif

      localrc = nf90_open(trim(filename), mode=NF90_NOWRITE, ncid=nc%ncid)
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot open '//trim(filename), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_OPEN
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif
      localrc = nf90_inq_dimid(nc%ncid,'time',nc%timeDimId)
      if (localrc /= NF90_NOERR) then
        !call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', no time dimension', ESMF_LOGMSG_WARNING)
        nc%timeDimID=-1
      endif

      localrc = nf90_inq_varid(nc%ncid,'time', varid)
      timeUnit_='none'
      if (localrc /= NF90_NOERR) then
        !call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', no time variable present', ESMF_LOGMSG_WARNING)
      else
        localrc = nf90_get_att(nc%ncid, varid, 'units', timeUnit_)
        if (localrc /= NF90_NOERR) then
          call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', no time unit', ESMF_LOGMSG_WARNING)
        else
          if (present(timeUnit)) write(timeUnit,'(A)') trim(timeUnit_)
        endif
      endif
    endif

    nc%name=trim(filename)
    call nc%update()
    call nc%update_variables()

    if (present(rc)) rc=rc_

  end function mossco_netcdfOpen

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdfCreate"
  function mossco_netcdfCreate(filename, kwe, timeUnit, state, rc) result(nc)

    use iso_fortran_env
    implicit none

    character(len=*), intent(in)          :: filename
    logical, optional, intent(in)         :: kwe ! Keyword-enforcer
    character(len=*),optional, intent(in) :: timeUnit
    type(ESMF_State), optional, intent(in):: state
    integer, intent(out),optional :: rc
    type(type_mossco_netcdf)      :: nc

    integer                       :: ncStatus
    character(len=255)            :: string
    integer                       :: rc_, localrc
    logical                       :: isPresent

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    inquire(file=trim(filename), exist=isPresent)
    if (ispresent) then
      call ESMF_LogWrite('  overwriting file '//trim(filename), ESMF_LOGMSG_INFO)
    else
      call ESMF_LogWrite('  created new file '//trim(filename), ESMF_LOGMSG_INFO)
    endif

    ncStatus = nf90_create(trim(filename), NF90_CLOBBER, nc%ncid)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot create file '//trim(filename), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    if (present(timeUnit)) then
      nc%timeUnit=trim(timeUnit)
      call nc%init_time(rc=localrc)
      call ESMF_LogWrite('  file '//trim(filename)//' has time unit '//trim(timeUnit), ESMF_LOGMSG_INFO)
    else
      nc%timeUnit=''
      call ESMF_LogWrite('  file '//trim(filename)//' has no time unit', ESMF_LOGMSG_WARNING)
    endif

    ncStatus = nf90_redef(nc%ncid)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot enter define mode for '//trim(filename), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    !> @todo write global attributes
    if (present(state)) then
      !call MOSSCO_AttributeNetcdfWrite(state, nc%ncid, varid=NF90_GLOBAL, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      call ESMF_LogWrite('  '//' obtained no information for global attributes in '//trim(filename), ESMF_LOGMSG_WARNING)
    endif

    ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'mossco_sha_key',MOSSCO_GIT_SHA_KEY)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute mossco_sha_key', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

! #ifndef NO_ISO_FORTRAN_ENV
!     !> @todo check cross-platform compatibility of the iso_fortran_env calls
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'compile_compiler_version',compiler_version())
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute compile_compiler_version', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'compile_compiler_options',compiler_options())
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute compile_compiler_options', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
! #endif
!
!     call get_command(string)
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'run_command_line',trim(string))
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute run_command_line', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     call getcwd(string)
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'run_working_directory',trim(string))
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute run_working_directory', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
! #ifndef NO_ISO_FORTRAN_ENV
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'run_process_id',getpid())
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute run_process_id', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
! #endif
!     !> @todo check cross-platform compatibility of these gnu extensions
!     call getlog(string)
! #ifndef NO_ISO_FORTRAN_ENV
!     write(string,'(A,I5,A,I5,A)') trim(string)// '(id=',getuid(),', gid=',getgid(),')'
! #endif
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'run_user',trim(string))
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute run_user', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     call hostnm(string)
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'run_hostname',trim(string))
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute run_hostname', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     !>@todo move this to a place where it reads attributes of a state/gridComp (toplevel/main), such that information
!     !> from the copuling specification is represented here
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'title','MOSSCO coupled simulation')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute title', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'institution','MOSSCO partners (HZG, IOW, and BAW)')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute institution', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'institution_hzg','Helmholtz-Zentrum Geesthacht')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute institution_hzg', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'institution_iow','Institut für Ostseeforschung Warnemünde')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute institution_iow', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'institution_baw','Bundesanstalt für Wasserbau')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute institution_baw', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'history','Created by MOSSCO')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute history', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'source','model_mossco')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute source', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'references','http://www.mossco.de/doc')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute references', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(nc%ncid,NF90_GLOBAL,'comment','')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute comment', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif

    ncStatus = nf90_enddef(nc%ncid)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot end definition mode', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    nc%name=trim(filename)


  end function mossco_netcdfCreate

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_init_time"
  subroutine mossco_netcdf_init_time(self, rc)

    class(type_mossco_netcdf)      :: self
    integer, optional, intent(out) :: rc

    integer                        :: varid, rc_, localrc, dimId
    type(type_mossco_netcdf_variable), pointer :: var

    rc_ = ESMF_SUCCESS

    localrc = nf90_redef(self%ncid)

    localrc = nf90_def_dim(self%ncid, 'time', NF90_UNLIMITED, self%timeDimId)
    if (localrc==NF90_ENAMEINUSE) then
      rc_ = MOSSCO_NC_EXISTING
    elseif (localrc /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot define dimension time in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_WRITE
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
    endif

    ! Define a dimension for holding data/time information as 19 character YYYY-MM-DDThh:mm:ss array
    localrc = nf90_def_dim(self%ncid, 'date_len', 19, dimId)
    if (localrc==NF90_ENAMEINUSE) then
      rc_ = MOSSCO_NC_EXISTING
    elseif (localrc /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot define dimension date_len in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) then
          rc = ESMF_RC_FILE_WRITE
          return
        else
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
    endif

    localrc = nf90_def_var(self%ncid, 'time', NF90_DOUBLE, self%timeDimId, varid)
    if (localrc==NF90_ENAMEINUSE) then
      rc_ = MOSSCO_NC_EXISTING
    elseif (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot define variable time in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_put_att(self%ncid, varid, 'units', trim(self%timeUnit))
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_put_att(self%ncid, varid, 'standard_name', 'time')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_def_var(self%ncid, 'doy', NF90_INT, self%timeDimId, varid)
    if (localrc==NF90_ENAMEINUSE) then
      rc_ = MOSSCO_NC_EXISTING
    elseif (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot define variable doy in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(self%ncid, varid, 'units', 'days')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_put_att(self%ncid, varid, 'mossco_name', 'day_of_year')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif


    localrc = nf90_def_var(self%ncid, 'year', NF90_INT, self%timeDimId, varid)
    if (localrc==NF90_ENAMEINUSE) then
      rc_ = MOSSCO_NC_EXISTING
    elseif (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot define variable yer in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(self%ncid, varid, 'units', 'a')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_put_att(self%ncid, varid, 'mossco_name', 'year_in_common_era')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_def_var(self%ncid, 'date_string', NF90_CHAR, (/dimId, self%timeDimId/), varid)
    if (localrc==NF90_ENAMEINUSE) then
      rc_ = MOSSCO_NC_EXISTING
    elseif (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot define variable date_string in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(self%ncid, varid, 'units', '')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_put_att(self%ncid, varid, 'mossco_name', 'date_string')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_def_var(self%ncid, 'wallclock_time', NF90_CHAR, (/dimId, self%timeDimId/), varid)
    if (localrc==NF90_ENAMEINUSE) then
      rc_ = MOSSCO_NC_EXISTING
    elseif (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot define variable wallclock_time in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(self%ncid, varid, 'units', '')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_put_att(self%ncid, varid, 'mossco_name', 'wallclock_time')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_def_var(self%ncid, 'speedup', NF90_FLOAT, (/self%timeDimId/), varid)
    if (localrc==NF90_ENAMEINUSE) then
      rc_ = MOSSCO_NC_EXISTING
    elseif (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot define variable speedup in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    call self%putattstring(varid, 'units', '1', rc=localrc)
    call self%putattstring(varid, 'mossco_name', 'speedup', rc=localrc)
    call self%putattfloat(varid, 'valid_min', 0.0, rc=localrc)
    call self%putattfloat(varid, 'missing_value', -1E30, rc=localrc)

    localrc = nf90_def_var(self%ncid, 'elapsed_wallclock_time', NF90_DOUBLE, (/self%timeDimId/), varid)
    if (localrc==NF90_ENAMEINUSE) then
      rc_ = MOSSCO_NC_EXISTING
    elseif (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot define variable elapsed_wallclock_time in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(self%ncid, varid, 'units', 's')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_put_att(self%ncid, varid, 'mossco_name', 'elapsed_wallclock_time')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_put_att(self%ncid, varid, 'description', 'Elapsed wallclock time (in seconds) since start of output')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot put attribute in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    localrc = nf90_enddef(self%ncid)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot end define mode for '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    call self%update()
    call self%update_variables()
    if (present(rc)) rc=rc_

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
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot inquire file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    nullify(self%variables)
    allocate(self%variables(nvars))
    do i=1, nvars
      var => self%variables(i)
      var%varid = i
      localrc = nf90_inquire_variable(self%ncid, i, ndims=var%rank, natts=nvaratts, name=var%name)
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot inquire variable in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif

      localrc = nf90_get_att(self%ncid,var%varid, 'standard_name', var%standard_name)
      if (localrc /= NF90_NOERR) var%standard_name=''

      localrc = nf90_get_att(self%ncid,var%varid, 'units', var%units)
      if (localrc /= NF90_NOERR) then
        !write(message,'(A)') '  '//trim(var%name)
        !call MOSSCO_MESSAGEAdd(message,' did not specify units in '//trim(self%name))
        !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        var%units=''
      endif

      !call ESMF_LogWrite(trim(var%standard_name)//' '//trim(var%units), ESMF_LOGMSG_INFO)

      if (var%rank <= 0) cycle

      if (allocated(var%dimids)) deallocate(var%dimids)
      allocate(var%dimids(var%rank))

      localrc = nf90_inquire_variable(self%ncid, i, dimids=var%dimids)
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', could inquire variable '//trim(var%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif

    end do

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
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot inquire file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    self%natts=natts
    self%nvars=nvars

    if (allocated(self%dimlens)) deallocate(self%dimlens)
    if (allocated(self%dimNames)) deallocate(self%dimNames)

    localrc = nf90_inquire(self%ncid, nDimensions=ndims)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot inquire file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    self%ndims=ndims

    if (ndims > 0) then
      if ( .not. allocated(self%dimlens)) allocate(self%dimlens(ndims), stat=localrc)
      if ( .not. allocated(self%dimNames)) allocate(self%dimNames(ndims), stat=localrc)
    endif

    do i=1, ndims
      localrc=nf90_inquire_dimension(self%ncid, i, len=self%dimlens(i), name=self%dimNames(i))
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot inquire dimension in file '//trim(self%name), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo

  end subroutine mossco_netcdf_update

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_locstream_dimensions"
  function mossco_netcdf_locstream_dimensions(self, field, kwe, owner, rc) result(dimids)

    implicit none

    class(type_mossco_netcdf)     :: self
    type(ESMF_Field)              :: field
    type(ESMF_KeyWordEnforcer), optional, intent(in)     :: kwe
    character(len=*), optional, intent(in)               :: owner
    integer(ESMF_KIND_I4), optional, intent(out)         :: rc

    type(ESMF_LocStream)          :: locStream
    integer                       :: ncStatus,rc_, localrc, dimcheck
    character(len=ESMF_MAXSTR)    :: geomName, name
    integer,pointer,dimension(:)  :: dimids

    integer(ESMF_KIND_I4)         :: rank, i, keyCount, locationCount
    character(len=ESMF_MAXSTR)    :: message, owner_

    rc_ = ESMF_SUCCESS
    owner_ = '--'

    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_SUCCESS
    if (present(owner)) call MOSSCO_StringCopy(owner_, owner)

    dimcheck=0
    call ESMF_FieldGet(field, locStream=locStream, rank=rank, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_LocStreamGet(locStream, keyCount=keyCount, name=geomName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (keyCount < 1) then
      write(message,'(A)') trim(owner_)//' cannot find any (required) keys in locSstream'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc = ESMF_RC_NOT_FOUND
      return
    endif

    if (rank  /= 1) then
      write(message,'(A)') trim(owner_)//' currently only handles rank 1 fields'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    call ESMF_LocStreamGetBounds(locStream, exclusiveCount=locationCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    nullify(dimids)
    allocate(dimids(rank+1))

    dimids(:)=-1
    dimids(rank+1)=self%timeDimId

    ! get location dimension-id
    write(name,'(A)') trim(geomName)//'_location'
    ncStatus = nf90_inq_dimid(self%ncid,trim(name),dimids(rank))
    if (ncStatus /= NF90_NOERR) dimcheck=-1

    !! if dimension not present, create it
    if (dimcheck == -1) then
      ncStatus = nf90_redef(self%ncid)
      ncStatus = nf90_def_dim(self%ncid, trim(name), locationCount,dimids(1))
      if (ncStatus==NF90_ENAMEINUSE) then
        rc_ = MOSSCO_NC_EXISTING
      elseif  (ncStatus==NF90_NOERR) then
        rc_ = ESMF_SUCCESS
      else
        rc_ = ESMF_SUCCESS
      end if
      ncStatus = nf90_enddef(self%ncid)
    end if

    !! if grid not present, also create the coordinate variables
    ! if (dimcheck == -1) call self%create_mesh_coordinate(mesh)

    call self%update()

    return

 end function mossco_netcdf_locstream_dimensions

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_mesh_dimensions"
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

    rc_ = ESMF_SUCCESS

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
        ncStatus = nf90_def_dim(self%ncid, trim(name), &
          totalubound(i)-totallbound(i)+1,dimids(i))
        if (ncStatus==NF90_ENAMEINUSE) then
          rc_ = MOSSCO_NC_EXISTING
        elseif  (ncStatus==NF90_NOERR) then
          rc_ = ESMF_SUCCESS
        else
          rc_ = ESMF_SUCCESS
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

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_grid_dimensions"
  recursive function mossco_netcdf_grid_dimensions(self,grid,staggerloc) result(dimids)
    class(type_mossco_netcdf)     :: self
    type(ESMF_Grid)               :: grid
    type(ESMF_StaggerLoc),optional :: staggerloc

    type(ESMF_StaggerLoc)          :: staggerloc_
    integer                       :: ncStatus,rc_,esmfrc,dimcheck
    character(len=ESMF_MAXSTR)    :: geomName, name
    integer,allocatable           :: ubounds(:),lbounds(:)
    integer,pointer,dimension(:)  :: dimids

    integer(ESMF_KIND_I4)         :: dimCount, dimid, rank, i, localrc
    character(len=ESMF_MAXSTR)    :: message,staggerlocsuffix

    rc_ = ESMF_SUCCESS

    dimcheck=0
    call ESMF_GridGet(grid, name=geomName, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call replace_character(geomName, ' ', '_')
    allocate(ubounds(rank))
    ubounds(:)=1
    allocate(lbounds(rank))
    lbounds(:)=1
    allocate(dimids(rank+1))
    dimids(:)=-1
    dimids(rank+1)=self%timeDimId

    if (present(staggerloc)) then
      staggerloc_ = staggerloc
    else
      staggerloc_ = ESMF_STAGGERLOC_CENTER
    end if

    call ESMF_GridGet(grid, staggerloc_, 0, exclusiveCount=ubounds, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (staggerloc_ .eq. ESMF_STAGGERLOC_CENTER) then
      staggerlocsuffix=''
    else
      write(staggerlocsuffix,'(A,I0)') '_',staggerloc
    end if

    ! get grid dimension-ids
    do i=1,rank
      write(name,'(A,I1,A)') trim(geomName)//'_',i,trim(staggerlocsuffix)
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
        write(name,'(A,I1,A)') trim(geomName)//'_',i,trim(staggerlocsuffix)
        ncStatus = nf90_def_dim(self%ncid, trim(name), &
          ubounds(i)-lbounds(i)+1,dimids(i))
        if (ncStatus==NF90_ENAMEINUSE) then
          rc_ = MOSSCO_NC_EXISTING
        elseif  (ncStatus==NF90_NOERR) then
          rc_ = ESMF_SUCCESS
        else
          rc_ = ESMF_SUCCESS
        end if
      enddo

      ncStatus = nf90_enddef(self%ncid)
    end if

    !! if grid not present, also create the coordinate variables
    if (dimcheck == -1) call self%create_coordinate(grid)
    call self%update()

    return

  end function mossco_netcdf_grid_dimensions

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_mesh_coordinate_create"
  subroutine mossco_netcdf_mesh_coordinate_create(self, mesh)

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

      write(varName,'(A)') trim(geomName)//'_'//trim(coordNames(i))
      if (self%variable_present(varName)) then
        !write(message,'(A)') 'A variable with this name already exists'
        !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      do j=1,coordDimCount(i)
        write(dimName,'(A,I1)') trim(geomName)//'_',j
        ncStatus = nf90_inq_dimid(self%ncid,trim(dimName),dimids(j))
      enddo

      ncStatus = nf90_redef(self%ncid)
      ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_DOUBLE,dimids(1:coordDimCount(i)),varid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//' cannot define variable '//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      !ncStatus = nf90_put_att(self%ncid,varid,'standard_name',varName)
      ncStatus = nf90_put_att(self%ncid,varid,'long_name',varName)
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
#define ESMF_METHOD "create_bounds_variable"
  recursive subroutine create_bounds_variable(self, varname, kwe, rc)

    !> The bound coordinate variables define the bound or the
    !> corner coordinates of a cell. The bound variable name is specified in
    !> the bounds attribute of the latitude and longitude variables.  The bound
    !> variables are 2D arrays for a regular lat/lon grid and a 3D array for a
    !> curvilinear grid. The first dimension of the bound array is 2 for a
    !> regular lat/lon grid and 4 for a curvilinear grid. The bound coordinates
    !> for a curvilinear grid is defined in counterclockwise order.

    implicit none
    class(type_mossco_netcdf)                         :: self
    character(len=*), intent(in) :: varname
    type(ESMF_KeyWordEnforcer), intent(in), optional  :: kwe
    integer(ESMF_KIND_I4), intent(out), optional      :: rc

    integer                     :: ncStatus, rc_, localrc
    character(len=ESMF_MAXSTR)  :: message, boundsName

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    boundsName = trim(varname)//'_bounds'

    if (self%variable_present(boundsName)) then
      write(message,'(A)') 'A variable with the name "'//trim(boundsName)//'" already exists'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      return
    endif

    !> @todo implement creating bounds dim based on 2*coorddimids
    !> def bounds variable, and fill with data

  end subroutine create_bounds_variable

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_coordinate_create"
  recursive subroutine mossco_netcdf_coordinate_create(self, grid, kwe, rc)

    !> CF standard: The cell center coordinate variables are determined by the
    !> value of its attribute units. The longitude variable has the attribute
    !> value set to either degrees_east, degree_east, degrees_E, degree_E,
    !> degreesE or degreeE. The latitude variable has the attribute value set
    !> to degrees_north, degree_north, degrees_N, degree_N, degreesN or degreeN.
    !> The latitude and the longitude variables are one-dimensional arrays if
    !> the grid is a regular lat/lon grid, two- dimensional arrays if the grid
    !> is curvilinear.

    use mossco_grid

    implicit none
    class(type_mossco_netcdf)                         :: self
    type(ESMF_Grid), intent(in)                       :: grid
    type(ESMF_KeyWordEnforcer), intent(in), optional  :: kwe
    integer(ESMF_KIND_I4), intent(out), optional      :: rc

    integer                     :: ncStatus, varid, rc_, esmfrc, rank, localrc
    integer                     :: nDims, nAtts, udimid, dimlen, dimid
    character(len=ESMF_MAXSTR)  :: varName, geomName, message, dimName

    character(len=ESMF_MAXSTR), dimension(3) :: coordNames, coordUnits, axisNameList, standardNameList
    character(len=ESMF_MAXSTR)               :: attributeName
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)    :: farrayPtr3
    real(ESMF_KIND_R8), pointer, dimension(:,:)      :: farrayPtr2
    real(ESMF_KIND_R8), pointer, dimension(:)        :: farrayPtr1
    integer(ESMF_KIND_I4), pointer, dimension(:)     :: intPtr1
    integer, pointer, dimension(:)                   :: dimids
    integer(ESMF_KIND_I4),allocatable                :: ubnd(:), lbnd(:), coordDimids(:), coordDimLens(:)
    type(ESMF_CoordSys_Flag)                         :: coordSys
    integer(ESMF_KIND_I4), dimension(:), allocatable :: coordDimCount, exclusiveCount
    integer(ESMF_KIND_I4)                            :: dimCount, attributeCount, i, j ,k
    integer(ESMF_KIND_I4)                            :: staggerLocCount
    type(ESMF_StaggerLoc)                            :: staggerLoc
    type(ESMF_Array)                                 :: array
    logical                                          :: isPresent

    type(ESMF_TypeKind_Flag)         :: typekind
    real(ESMF_KIND_R8)               :: real8
    real(ESMF_KIND_R4)               :: real4
    integer(ESMF_KIND_I8)            :: int8
    integer(ESMF_KIND_I4)            :: int4
    character(len=ESMF_MAXSTR)       :: string
    logical                          :: logvalue
    type(ESMF_Field)                 :: field

    call ESMF_GridGet(grid, coordSys=coordSys, dimCount=dimCount, &
      name=geomName, staggerLocCount=staggerLocCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call replace_character(geomName, ' ', '_')
    if (dimCount<1) return

    if (coordSys == ESMF_COORDSYS_SPH_DEG) then
      coordnames=(/'lon  ','lat  ','level'/)
      coordunits=(/'degree','degree','1     '/)
      standardNameList=(/'longitude         ','latitude          ','model_level_number'/)
    elseif (coordSys == ESMF_COORDSYS_SPH_RAD) then
      coordnames=(/'lon  ','lat  ','level'/)
      coordunits=(/'rad','rad','1  '/)
      standardNameList=(/'longitude         ','latitude          ','model_level_number'/)
    else
      coordnames=(/'x','y','z'/)
      coordunits=(/'m','m','1'/)
      standardNameList=(/'projection_y_coordinate','projection_y_coordinate','model_level_number     '/)
    endif

    axisNameList=(/'X','Y','Z'/)

    allocate(coordDimCount(dimCount))
    call ESMF_GridGet(grid, coordDimCount=coordDimCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> from here, it is really assumed that the staggerloc is ESMF_STAGGERLOC_CENTER
    staggerLoc = ESMF_STAGGERLOC_CENTER
    dimids => self%grid_dimensions(grid)

    ! Write the auxiliary coordinate variables x, y, z
    ! These are 1-dimensional irrespective of the actual coordinates
    do i=1, dimCount

      write(varName,'(A)') trim(geomName)//'_'//trim(axisNameList(i))
      if (self%variable_present(varName)) then
        !write(message,'(A)') 'A variable with the name "'//trim(varName)//'" already exists'
        !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      if (.not.allocated(coordDimids)) allocate(coordDimids(1))

      write(dimName,'(A,I1)') trim(geomName)//'_',i
      ncStatus = nf90_inq_dimid(self%ncid,trim(dimName),coordDimids(1))

      call self%getAxis(grid, coordDim=i, intPtr1=intPtr1, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      ncStatus = nf90_redef(self%ncid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot enter definition mode',ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      ncStatus = nf90_def_var(self%ncid, trim(varName), NF90_Int, coordDimids, varid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot define variable '//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      !! Write default attributes into netCDF
      !!ncStatus = nf90_put_att(self%ncid,varid,'standard_name',trim(varName))
      !if (ncStatus /= NF90_NOERR) then
      !  call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot put attribute standard_name='//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !endif

      ncStatus = nf90_put_att(self%ncid,varid,'units','1')
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot put attribute units=1',ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      ncStatus = nf90_put_att(self%ncid,varid,'axis',axisNameList(i))
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot put attribute axis='//axisNameList(i),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      ncStatus = nf90_enddef(self%ncid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot end definition mode',ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      endif

      ncStatus = nf90_put_var(self%ncid, varid, intPtr1(:))
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write data for variable'//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

    enddo

    do i=1,dimCount

      write(varName,'(A)') trim(geomName)//'_'//trim(coordNames(i))
      if (self%variable_present(varName)) then
        !write(message,'(A)') 'A variable with the name "'//trim(varName)//'" already exists'
        !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      if (allocated(coordDimids)) deallocate(coordDimids)
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

      ncStatus = nf90_redef(self%ncid)
      ncStatus = nf90_def_var(self%ncid,trim(varname),NF90_DOUBLE,coordDimids,varid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot define variable '//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
      !! Write default attributes into netCDF
      ncStatus = nf90_put_att(self%ncid,varid,'standard_name',trim(standardNameList(i)))
      ncStatus = nf90_put_att(self%ncid,varid,'long_name',trim(varName))
      ncStatus = nf90_put_att(self%ncid,varid,'units',trim(coordUnits(i)))
      !ncStatus = nf90_put_att(self%ncid,varid,'formula_terms','')
      ncStatus = nf90_put_att(self%ncid,varid,'horizontal_stagger_location','center')
      !! axis attribute added only for 1-D coordinate variables
      if (coordDimCount(i)==1) then
        ncStatus = nf90_put_att(self%ncid,varid,'axis',axisNameList(i))
      end if


      !! Inquire array for attributes and create / overwrite attributes
      call ESMF_GridGetCoord(grid, i, staggerloc=ESMF_STAGGERLOC_CENTER, array=array, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      call ESMF_AttributeGet(array, count=attributeCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      do j=1, attributeCount
         call ESMF_AttributeGet(array, attributeIndex=j, name=attributeName, &
           typekind=typekind, rc=localrc)
         _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

         if (typekind==ESMF_TYPEKIND_I4) then
           call ESMF_AttributeGet(array, attributeName, int4, rc=localrc)
           _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),int4)
         elseif (typekind==ESMF_TYPEKIND_I8) then
           call ESMF_AttributeGet(array, attributeName, int8, rc=localrc)
           _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),int8)
         elseif (typekind==ESMF_TYPEKIND_R4) then
           call ESMF_AttributeGet(array, attributeName, real4, rc=localrc)
           _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),real4)
         elseif (typekind==ESMF_TYPEKIND_R8) then
           call ESMF_AttributeGet(array, attributeName, real8, rc=localrc)
           _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),real8)
         elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
           call ESMF_AttributeGet(array, attributeName, string, rc=localrc)
           _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
           ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName), trim(string))
         elseif (typekind==ESMF_TYPEKIND_LOGICAL) then
           call ESMF_AttributeGet(array, attributeName, logvalue, rc=localrc)
           _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
           if (logValue) then
             ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),'.true.')
          else
              ncStatus = nf90_put_att(self%ncid,varid,trim(attributeName),'.false.')
          endif
        else
          write(message, '(A,I2)') '  attribute '//trim(attributeName)//' has unknown typekind ',typeKind
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        endif
      enddo

      if (allocated(coordDimids)) deallocate(coordDimids)

      if (coordDimCount(i)==1) then
        ncStatus = nf90_put_att(self%ncid,varid,'axis',axisNameList(i))
      end if

      if (staggerLoc == ESMF_STAGGERLOC_CENTER &
        .or. staggerLoc == ESMF_STAGGERLOC_CENTER_VFACE &
        .or. staggerLoc == ESMF_STAGGERLOC_CENTER_VCENTER ) then

        ncStatus = nf90_put_att(self%ncid,varid,'bounds',trim(varname)//'_bound')
        call self%create_bounds_variable(trim(varname), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      end if

    enddo
    !! End definition phase of netcdf
    ncStatus = nf90_enddef(self%ncid)

#if 0
    if (rank == 2) then
      field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    elseif  (rank == 3) then
      field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    ! @todo this only works if the grid item AREA has been set in the grid.  We could do this here (
    ! the area calculation or rely on it being calculated in the creator component ...).  Problem again
    ! as with GRIDITEM_MASK is that we do not know how to check for its presence.

    !call ESMF_FieldRegridGetArea(field, rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !call self%put_variable(field, .0D00, 'grid_area')
    if (rank>1 .and. rank < 4) then
      call ESMF_FieldDestroy(field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
#endif

    do i=1, dimCount

      if (allocated(lbnd)) deallocate(lbnd)
      if (allocated(ubnd)) deallocate(ubnd)

      allocate(lbnd(coordDimCount(i)))
      allocate(ubnd(coordDimCount(i)))

      write(varName,'(A)') trim(geomName)//'_'//trim(coordNames(i))
      ncStatus=nf90_inq_varid(self%ncid, trim(varName), varid)
      if (ncStatus /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot find coordinate variable '//trim(varname),ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      select case (coordDimCount(i))
        case (1)
          call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr1, exclusiveLBound=lbnd, exclusiveUBound=ubnd, rc=localrc)
          ncStatus = nf90_put_var(self%ncid, varid, farrayPtr1(RANGE1D))
        case (2)
          call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr2, exclusiveLBound=lbnd, exclusiveUBound=ubnd, rc=localrc)
          ncStatus = nf90_put_var(self%ncid, varid, farrayPtr2(RANGE2D))
        case (3)
          call ESMF_GridGetCoord(grid, i, farrayPtr=farrayPtr3, exclusiveLBound=lbnd, exclusiveUBound=ubnd, rc=localrc)
          ncStatus = nf90_put_var(self%ncid, varid, farrayPtr3(RANGE3D))
        case default
          write(message,'(A)')  '  cannot deal with less than 1 or more than 3 coordinate dimensions'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      end select

      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
        write(message,'(A)')  '  this error will be fixed in the future, disregard for now'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        write(message,'(A)')  '  did not write coordinate variable '//trim(varname)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        cycle
      endif

      call ESMF_GridGetCoord(grid, coordDim=i, staggerloc=ESMF_STAGGERLOC_CENTER, array=array, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (allocated(lbnd)) deallocate(lbnd)
      if (allocated(ubnd)) deallocate(ubnd)

    enddo
    if (allocated(coordDimCount)) deallocate(coordDimCount)

    call self%update_variables()
    call self%update()

    return

  end subroutine mossco_netcdf_coordinate_create


#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_coordinate_create_from_field"
    subroutine mossco_netcdf_coordinate_create_from_field(self, field, rc)

      implicit none
      class(type_mossco_netcdf)               :: self
      type(ESMF_Field), intent(in)            :: field
      integer(ESMF_KIND_I4), optional         :: rc

      integer                     :: ncStatus, varid, rank, localrc, rc_
      integer                     :: nDims, nAtts, udimid, dimlen, dimid, j
      character(len=ESMF_MAXSTR)  :: varName, geomName, message, dimName

      character(len=ESMF_MAXSTR), dimension(3) :: coordNames, coordUnits, axisNameList, standardNameList
      character(len=ESMF_MAXSTR)               :: attributeName
      real(ESMF_KIND_R8), pointer, dimension(:,:,:)    :: farrayPtr3
      real(ESMF_KIND_R8), pointer, dimension(:,:)      :: farrayPtr2
      real(ESMF_KIND_R8), pointer, dimension(:)        :: farrayPtr1
      integer(ESMF_KIND_I4), pointer, dimension(:)     :: intPtr1
      integer, pointer, dimension(:)                   :: dimids
      integer(ESMF_KIND_I4),allocatable                :: ubnd(:), lbnd(:), coordDimids(:), coordDimLens(:)
      integer(ESMF_KIND_I4),allocatable                :: uubnd(:), ulbnd(:)
      type(ESMF_CoordSys_Flag)                         :: coordSys
      integer(ESMF_KIND_I4), dimension(:), allocatable :: coordDimCount, exclusiveCount
      integer(ESMF_KIND_I4)                            :: dimCount, attributeCount, i, dimrank, ungriddedDimCount
      type(ESMF_Array)                                 :: array
      logical                                          :: isPresent

      type(ESMF_TypeKind_Flag)         :: typekind
      real(ESMF_KIND_R8)               :: real8
      real(ESMF_KIND_R4)               :: real4
      integer(ESMF_KIND_I8)            :: int8
      integer(ESMF_KIND_I4)            :: int4
      character(len=ESMF_MAXSTR)       :: string

      type(ESMF_FieldStatus_Flag)     :: fieldStatus
      type(ESMF_GeomType_Flag)        :: geomType
      type(ESMF_Grid)                 :: grid

      if (present(rc)) rc=ESMF_SUCCESS

      call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! Return from an empty field, no information on coordinates can be obtained
      ! The information from grid can be obtained in GRIDSET or COMPLETE state
      if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) return

      call ESMF_FieldGet(field, geomType=geomType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (geomType /= ESMF_GEOMTYPE_GRID) then
        write(message,'(A)') '    Not implemented: creating coordinates from field that is not on a grid'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        if (present(rc)) rc=ESMF_RC_NOT_IMPL
        return
      endif

      call ESMF_FieldGet(field, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! call self%create_coordinate(grid, rc=localrc)
      ! if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      !   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !
      ! ! Return from a non-complete field, no furhter information can be obtained
      ! if (fieldStatus == ESMF_FIELDSTATUS_GRIDSET) return
      !
      ! dimids => self%grid_dimensions(grid)
      !
      ! ! Return if maximum number of coordinates (3) is reached
      ! if (len(dimids) == 3) return
      !
      ! call ESMF_FieldGet(field, dimCount=dimCount, rc=localrc)
      ! if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      !   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !
      ! dimrank=ubound(dimids,1)
      ! ungriddedDimCount=dimCount-dimrank+1
      !
      ! ! Return from, no further dimensions are present
      ! if (ungriddedDimCount == 0) return
      !
      ! allocate(ulbnd(ungriddedDimCount))
      ! allocate(uubnd(ungriddedDimCount))
      ! call ESMF_FieldGet(field,ungriddedLBound=ulbnd,ungriddedUBound=uubnd,rc=localrc)
      ! if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      !   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !
      ! ncStatus = nf90_redef(self%ncid)

      !@ todo from here, exit with not-implemented rc
      write(message,'(A)') '    Not implemented: mossco_netcdf_coordinate_create_from_field'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (present(rc)) rc=ESMF_RC_NOT_IMPL

      return

    end subroutine mossco_netcdf_coordinate_create_from_field

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
    integer(ESMF_KIND_I4)                        :: j, k=1, localDeCount, itemCount
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
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
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
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
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
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
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
       ! call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
       ! return
      end if

!      array = ESMF_ArrayCreate(name=trim(coordVar%name), typeKind=ESMF_TYPEKIND_R8, &
!        rank=coordVar%rank, rc=localrc)
!
!      call ESMF_ArrayGet(array, localDeCount=localDeCount, rank=rank, rc=localrc)
!      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
!        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!      if (localDeCount==0) return
!
!      allocate(lbnd(rank))
!      allocate(ubnd(rank))
!
!      call ESMF_ArrayGetBounds(array, localDe=0, exclusiveLBound=lbnd, &
!        exclusiveUBound=ubnd, rc=localrc)
!      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
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
!        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
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

    rc_ = ESMF_SUCCESS

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
    type(ESMF_Field), intent(inout)              :: field
    type(type_mossco_netcdf_variable)            :: var
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    integer(ESMF_KIND_I4), intent(in), optional  :: itime

    integer(ESMF_KIND_I4)                        :: localrc, udimid, localDeCount, rc_
    integer(ESMF_KIND_I4)                        :: fieldRank, gridRank, itime_, j, i, k
    type(ESMF_FieldStatus_Flag)                  :: fieldStatus
    integer(ESMF_KIND_I4), allocatable           :: start(:), count(:), ubnd(:), lbnd(:)
    integer(ESMF_KIND_I4), allocatable           :: ncubnd(:),fstart(:)
    real(ESMF_KIND_R8), pointer                  :: farrayPtr1(:)=>null(), farrayPtr2(:,:)=>null()
    real(ESMF_KIND_R8), pointer                  :: netcdfPtr1(:)=>null()
    real(ESMF_KIND_R8), pointer                  :: netcdfPtr2(:,:)=>null()
    real(ESMF_KIND_R8), pointer                  :: netcdfPtr3(:,:,:)=>null()
    real(ESMF_KIND_R8), pointer                  :: netcdfPtr4(:,:,:,:)=>null()
    real(ESMF_KIND_R8), pointer                  :: farrayPtr3(:,:,:)=>null(), farrayPtr4(:,:,:,:)=>null()
    character(len=ESMF_MAXSTR)                   :: message, name

    integer(ESMF_KIND_I4)                        :: deCount,localPet
    integer(ESMF_KIND_I4),allocatable            :: minIndexPDe(:,:), maxIndexPDe(:,:)
    type(ESMF_Grid)                              :: grid
    type(ESMF_DistGrid)                          :: distGrid
    type(ESMF_DELayout)                          :: delayout
    type(ESMF_Vm)                                :: Vm

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    if (present(itime)) then
      itime_ = itime
    else
      itime_ = 1
    endif

    ! Test for field completeness and terminate if not complete
    call ESMF_FieldGet(field, status=fieldStatus, name=name, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
      write(message, '(A)')  'Cannot read into non-complete field '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

      if (present(rc)) rc = ESMF_RC_ARG_BAD
      return
    endif

    call ESMF_FieldGet(field, rank=fieldRank, localDeCount=localDeCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (localDeCount == 0) return

    if (fieldRank > 4) then
      write(message, '(A)')  'Rank > 4 not implemented for reading field '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      return
    endif

    call ESMF_FieldGet(field, grid=grid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_GridGet(grid, distGrid=distGrid, rank=gridRank, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_DistGridGet(distGrid, delayout=delayout, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_DELayoutGet(delayout, deCount=deCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> @todo check error state
    allocate(minIndexPDe(gridRank, deCount), stat=localrc)
    allocate(maxIndexPDe(gridRank, deCount), stat=localrc)

    call ESMF_DistGridGet(distGrid, minIndexPDe=minIndexPDe, &
                                    maxIndexPDe=maxIndexPDe, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_VmGetGlobal(vm, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_VmGet(vm, localPet=localPet, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> @todo check error state
    allocate(start(fieldRank)); start(1:fieldRank) = 1
    allocate(count(fieldRank)); count(1:fieldRank) = 1
    allocate(ubnd(fieldRank))
    allocate(ncubnd(fieldRank))
    allocate(fstart(fieldRank)); fstart(1:fieldRank) = 1
    allocate(lbnd(fieldRank))

    call ESMF_FieldGetBounds(field, exclusiveUbound=ubnd, exclusiveLBound=lbnd, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    ncubnd = ubnd ! initialize array size from local field bounds

    !> First asssume to read part of a global field in the netcdf file
    !! adjust array size to match global field indexation:
    ncubnd(1:gridRank)=maxIndexPDe(:,localPet+1)
    start(1:gridRank)=minIndexPDe(:,localPet+1)
    where (start < 1)
      start=1
    endwhere

    !! set start indices for target field
    fstart(1:gridRank)=start(1:gridRank)-minIndexPDe(1:gridRank,localPet+1)+1

    !! restrict length of field to read from netcdf to available size, only do this
    !! on the gridded dimensions
    do i=1, gridRank
      if (ncubnd(i)>self%dimlens(var%dimids(i))) ncubnd(i)=self%dimlens(var%dimids(i))
    enddo
    count=count+ncubnd-start

    !> overwrite global indexing, if matching netcdf data domain
    !! then simply copy whole array:
    do i=1, gridRank
      if (ubnd(i)-lbnd(i)+1==self%dimlens(var%dimids(i))) then
        start(i) = 1
        count(i) = ubnd(i)-lbnd(i)+1
        fstart(i) = 1
      end if
    enddo

    if (any(count <= 0)) then
      write(0,*) '  fstart = ', fstart
      write(0,*) '  start = ', start
      write(0,*) '  count = ', count
      write(0,*) '  ncubnd = ', ncubnd
      write(message,'(A)') '  count < 0 for '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc = ESMF_RC_CANNOT_GET
      return
    end if

    if (fieldRank == 1) then
      call ESMF_FieldGet(field, farrayPtr=farrayPtr1, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      allocate(netcdfPtr1(count(1)), stat=localrc)
      if (var%rank==fieldRank) then
        localrc = nf90_get_var(self%ncid, var%varid, netcdfPtr1, start, count)
      elseif (var%rank==fieldRank+1 .and. var%dimids(fieldRank+1) == self%timeDimId ) then
        if (self%dimlens(self%timeDimId) < itime_) then
          write(message,'(A)') '  requested index exceeds time dimension when reading '//trim(var%name)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
          if (present(rc)) rc = ESMF_RC_ARG_OUTOFRANGE
          return
        endif
        localrc = nf90_get_var(self%ncid, var%varid, netcdfPtr1, (/start(1),itime_/), (/count(1),1/))
      else
        ! ungridded bounds not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
      if (localrc /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(localrc))//', could not read variable '//trim(var%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_FILE_READ
        return
      endif
      farrayPtr1(fstart(1):fstart(1)+count(1)-1) = netcdfPtr1
      if (associated(netcdfPtr1)) deallocate(netcdfPtr1)

    elseif (fieldRank == 2) then
      call ESMF_FieldGet(field, farrayPtr=farrayPtr2, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      !write(0,*) 'rank=', fieldrank, 'var%rank=', var%rank, 'var%dimids=', var%dimids(:), 'udimid=',self%timeDimId
      allocate(netcdfPtr2(1:count(1),1:count(2)))

      ! If the variable rank corresponds to the field's rank, just read it
      if (var%rank==fieldRank) then
        localrc = nf90_get_var(self%ncid, var%varid, netcdfPtr2, start, count)
      ! Otherwise, allow reading of a reduced variable rank, if the unlimited dimension (last dimension)
      ! is the time dimension.
      elseif (var%rank==fieldRank+1 .and. var%dimids(fieldRank+1) == self%timeDimId ) then
        if (self%dimlens(self%timeDimId) < itime_) then
          write(message,'(A,I3,A,I3,A)') 'requested index ',itime_,' exceeds time dimension ',self%dimlens(self%timeDimId)
          call MOSSCO_MessageAdd(message, ' when reading '//trim(var%name))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        localrc = nf90_get_var(self%ncid, var%varid, netcdfPtr2, (/start(1),start(2),itime_/), (/count(1),count(2),1/))
        !write(0,*) 'shape = ',shape(netcdfPtr2), ' start = ',  (/start(1),start(2),itime_/), ' count = ', (/count(1),count(2),1/)
      else
        ! ungridded bounds not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
      if (localrc /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(localrc))//', could not read variable '//trim(var%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      farrayPtr2(fstart(1):fstart(1)+count(1)-1, &
                 fstart(2):fstart(2)+count(2)-1) &
        = netcdfPtr2

      if (associated(netcdfPtr2)) deallocate(netcdfPtr2)

    elseif (fieldRank == 3) then
      call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      allocate(netcdfPtr3(1:count(1),1:count(2),1:count(3)))
      if (var%rank==fieldRank) then
        localrc = nf90_get_var(self%ncid, var%varid, netcdfPtr3, start, count)
      elseif (var%rank==fieldRank+1 .and. var%dimids(fieldRank+1) == self%timeDimId ) then
        localrc = nf90_get_var(self%ncid, var%varid, netcdfPtr3, &
          (/start(1),start(2),start(3),itime_/), (/count(1),count(2), count(3),1/))
      else
        ! ungridded bounds not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
      if (localrc /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(localrc))//', could not read variable '//trim(var%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      farrayPtr3(fstart(1):fstart(1)+count(1)-1, &
                 fstart(2):fstart(2)+count(2)-1, &
                 fstart(3):fstart(3)+count(3)-1) &
        = netcdfPtr3

      ! If the variable is an _in_water quality and the input is a collapsed 3rd dimension, then
      ! fill the entire water column with this collapsed 3rd dimensions
      if (index(name, '_in_water') > 1 .and. count(3) == 1 .and. (ubnd(3)-lbnd(3)+1 > 1)) then
        do k = 0, ubnd(3) - lbnd(3)
          farrayPtr3(fstart(1):fstart(1)+count(1)-1, &
                     fstart(2):fstart(2)+count(2)-1, &
                     fstart(3)+k:fstart(3)+k) &
            = netcdfPtr3
        enddo
      endif

      ! If the variable is an _in_water quality and the input is a 2-dimensional dimension, then
      ! fill the upper and the lower level only
      !> @todo this should be configurable
      if (index(name, '_in_water') > 1 .and. count(3) == 2 .and. (ubnd(3)-lbnd(3)+1 > 1)) then
        farrayPtr3(fstart(1):fstart(1)+count(1)-1, &
                   fstart(2):fstart(2)+count(2)-1, &
                   fstart(3)+ 0) = netcdfPtr3(:,:,1)
        farrayPtr3(fstart(1):fstart(1)+count(1)-1, &
                   fstart(2):fstart(2)+count(2)-1, &
                   fstart(3)+ ubnd(3) - lbnd(3)) = netcdfPtr3(:,:,2)
        do k = 1, ubnd(3) - lbnd(3) - 1
          farrayPtr3(fstart(1):fstart(1)+count(1)-1, &
                     fstart(2):fstart(2)+count(2)-1, &
                     fstart(3)+k:fstart(3)+k) = 0.0D0
        enddo
      endif

      if (associated(netcdfPtr3)) deallocate(netcdfPtr3)
    elseif (fieldRank == 4) then
      call ESMF_FieldGet(field, farrayPtr=farrayPtr4, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      allocate(netcdfPtr4(1:count(1),1:count(2),1:count(3),1:count(4)))
      if (var%rank==fieldRank) then
        localrc = nf90_get_var(self%ncid, var%varid, netcdfPtr4, start, count)
      elseif (var%rank==fieldRank+1 .and. var%dimids(fieldRank+1) == self%timeDimId ) then
        localrc = nf90_get_var(self%ncid, var%varid, netcdfPtr4, &
          (/start(1),start(2),start(3),start(4),itime_/), (/count(1),count(2),count(3),count(4),1/))
      else
        ! ungridded bounds not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
      if (localrc /= NF90_NOERR) then
        write(message,'(A)') '  '//trim(nf90_strerror(localrc))//', could not read variable '//trim(var%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      farrayPtr4(fstart(1):fstart(1)+count(1)-1, &
                 fstart(2):fstart(2)+count(2)-1, &
                 fstart(3):fstart(3)+count(3)-1, &
                 fstart(4):fstart(4)+count(4)-1) &
        = netcdfPtr4

      if (associated(netcdfPtr4)) deallocate(netcdfPtr4)
    endif

    if (allocated(lbnd))  deallocate(lbnd)
    if (allocated(ubnd))  deallocate(ubnd)
    if (allocated(ncubnd))  deallocate(ncubnd)
    if (allocated(count)) deallocate(count)
    if (allocated(start)) deallocate(start)
    if (allocated(fstart)) deallocate(fstart)

    call self%getatt(field, var, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    return

  end subroutine mossco_netcdf_var_get

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_var_get_var"
  function mossco_netcdf_var_get_var(self, varname, rc) result(var)

    implicit none
    class(type_mossco_netcdf)                    :: self
    character(len=*)                             :: varname
    type(type_mossco_netcdf_variable), pointer   :: var
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                        :: i, rc_

    rc_ = ESMF_SUCCESS
    nullify(var)

    do i=1, self%nvars
      if (trim(self%variables(i)%name) == trim(varname)) then
        var => self%variables(i)
        return
      endif
      if (trim(self%variables(i)%standard_name) == trim(varname)) then
        var => self%variables(i)
        return
      endif
    enddo

    if (present(rc)) rc=rc_

    return

  end function mossco_netcdf_var_get_var

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_find_time_index"
!> given an ESMF_Time, the routine finds the two indices in the
!> time coordinate variable that bound the input time.  It can
!> also return the weight of the two bounding time points
  subroutine mossco_netcdf_find_time_index(self, currTime, itime, kwe, jtime, &
    weight, verbose, rc)

    implicit none
    class(type_mossco_netcdf)                    :: self
    type(ESMF_Time), intent(in)                  :: currTime
    integer(ESMF_KIND_I4), intent(out)           :: itime
    logical, intent(in), optional                :: kwe
    integer(ESMF_KIND_I4), intent(out), optional :: jtime
    real(ESMF_KIND_R8), intent(out), optional    :: weight
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    logical, intent(in), optional                :: verbose

    type(ESMF_Time)                              :: refTime
    type(ESMF_TimeInterval)                      :: timeInterval
    integer(ESMF_KIND_I4)                        :: i, rc_, jtime_, ticks4
    integer(ESMF_KIND_I4)                        :: localrc, ntime, varid
    real(ESMF_KIND_R8), allocatable              :: farray(:)
    real(ESMF_KIND_R8)                           :: weight_
    integer(ESMF_KIND_I8)                        :: ticks
    character(ESMF_MAXSTR)                       :: timeUnit, message, refTimeISOString
    logical                                      :: isShort = .false., verbose_

    rc_ = ESMF_SUCCESS
    verbose_ = .true.
    if (present(kwe)) rc_ = rc_
    if (present(rc)) rc = rc_
    if (present(verbose)) verbose_ = verbose

    itime = 1
    jtime_ = 1
    weight_ = 0.0
    if (present(weight)) weight = weight_
    if (present(jtime))  jtime = jtime_

    localrc = nf90_inq_varid(self%ncid, 'time', varid)
    if (localrc /= NF90_NOERR .and. verbose_) then
      write(message,'(A)') '-- no time variable in '//trim(self%name)//', choosing default time index 1'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    else
      call self%reftimeString(refTimeISOString, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (len_trim(refTimeISOString) > 0) then
        call MOSSCO_TimeSet(refTime, refTimeISOString, localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      else
        !> Create a copy of currTime
        reftime=currTime
        write(message,'(A)') '-- no reference time in '//trim(self%name)//'::time'
        if (verbose_) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      endif

      localrc = nf90_get_att(self%ncid, varid, 'units', timeUnit)
      if (localrc /= NF90_NOERR) then
        write(message,'(A)') '-- '//trim(nf90_strerror(localrc))//', no time unit in '//trim(self%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_FOUND
        return
      endif

      ! Allow climato* prefix before unit specification
      if (timeUnit(1:10) == 'climatolog') then
        i=index(timeUnit,' ')
        timeUnit=timeUnit(i+1:len_trim(timeUnit))
      endif

      i=index(timeUnit,' ')
      if (i>0) timeUnit=timeUnit(1:i-1)

      timeInterval = currTime - refTime

      isShort = .false.

      if (timeUnit(1:6) == 'second') then
        call ESMF_TimeIntervalGet(timeInterval, startTime=refTime, s_i8=ticks, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      elseif (timeUnit(1:6) == 'minute') then
        isShort = .true.
        call ESMF_TimeIntervalGet(timeInterval, startTime=refTime, m=ticks4, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      elseif (timeUnit(1:4) == 'hour') then
        isShort = .true.
        call ESMF_TimeIntervalGet(timeInterval, startTime=refTime, h=ticks4, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      elseif (timeUnit(1:3) == 'day') then
        call ESMF_TimeIntervalGet(timeInterval, startTime=refTime, d_i8=ticks, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      elseif (timeUnit(1:5) == 'month') then
        !> @todo this is a workaround as long as ESMF does not implement a
        !> time difference in TimeIntervalSetDur().  Count the days and
        !> get the month of a Gregorian-calendar average year
        call ESMF_TimeIntervalGet(currTime - refTime, d_i8=ticks, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        ticks = int(floor(real(ticks) * 12.0 / 365.2425), ESMF_KIND_I8)

        !call ESMF_TimeIntervalPrint(timeInterval)
        !call ESMF_TimeIntervalGet(timeInterval, startTime=refTime, mm_i8=ticks, rc=localrc)

      elseif (timeUnit(1:4) == 'year') then
        call ESMF_TimeIntervalGet(currTime - refTime, startTime=refTime, yy_i8=ticks, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      else
        write(message, '(A)') '-- time unit '//trim(timeUnit)//' not implemented from '//trim(self%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif

      if (isShort) ticks = ticks4

      ntime = self%dimlens(self%timeDimId)
      if (ntime < 1) then
        !> @todo: this can actually be possible (if time is unlimited and no time-dependent data)
        write(message, '(A)') '-- time dimension cannot have length zero in '//trim(self%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif

      if (.not.allocated(farray)) then
        allocate(farray(ntime), stat=localrc)
        if (localrc /= 0) then
          write(message,'(A)') '-- could not allocate memory for farray time from '//trim(self%name)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
          if (present(rc)) rc = ESMF_RC_MEM
          return
        endif
      endif

      localrc = nf90_get_var(self%ncid, varid, farray)
      if (localrc /= NF90_NOERR) then
        write(message,'(A)') '-- could not read variable time from '//trim(self%name)
        call ESMF_LogWrite(trim(message)//trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_FOUND
        return
      endif

      !! Search for the largest index i with farray(i) <= ticks*1.0D0
      do i = 1, ntime
        if (farray(i) <= ticks*1.0D0) then
          itime=i
        else
          exit
        endif
      enddo
      if (farray(ntime) <= ticks*1.0D0) itime = ntime

      if (farray(itime) == ticks*1.0D0) then
        jtime_ = itime
      else
        jtime_ = itime + 1
      endif

      if (jtime_ > ntime) jtime_ = ntime

      if (farray(jtime_) > farray(itime)) then
        weight_ = (ticks*1.0D0 - farray(itime)) / (farray(jtime_) - farray(itime))
      endif

      if (allocated(farray)) deallocate(farray)
    endif

    if (present(weight)) weight=weight_
    if (present(jtime)) jtime=jtime_

  end subroutine mossco_netcdf_find_time_index

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_reftime_string"
  subroutine mossco_netcdf_reftime_string(self, refTimeISOString, rc)

    implicit none
    class(type_mossco_netcdf)                    :: self
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    character(len=*), intent(out)                :: refTimeISOString

    integer(ESMF_KIND_I4)                        :: i, rc_, localrc
    type(ESMF_Time)                              :: refTime

    rc_ = ESMF_SUCCESS

    call mossco_netcdf_reftime(self, refTime, rc=localrc)
    if (localrc == ESMF_SUCCESS) then

      call ESMF_TimeGet(refTime, timeStringISOFrac=refTimeISOString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    elseif (localrc == ESMF_RC_NOT_FOUND) then
      rc_ = localrc
      refTimeISOString = ''
    else
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    if (present(rc)) rc=rc_

  end subroutine mossco_netcdf_reftime_string

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_reftime"
  subroutine mossco_netcdf_reftime(self, refTime, kwe, rc)

    implicit none

    class(type_mossco_netcdf)                        :: self
    type(ESMF_Time), intent(out)                     :: refTime
    type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    integer(ESMF_KIND_I4)                        :: i, rc_, itime_, localrc, varid
    character(ESMF_MAXSTR)                       :: timeUnit, ISOString

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc=rc_
    if (present(kwe)) rc_=ESMF_SUCCESS

    localrc = nf90_inq_varid(self%ncid, 'time', varid)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', no time variable for reference time', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc=ESMF_RC_NOT_FOUND
      return
    endif

    localrc = nf90_get_att(self%ncid, varid, 'units', timeUnit)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', no time unit for reference time', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc=ESMF_RC_NOT_FOUND
      return
    endif

    i=index(timeunit,'since ')
    if (i<1) then
      call ESMF_LogWrite('  no reference time given in unit '//trim(timeUnit), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) rc = ESMF_RC_NOT_FOUND
      return
    endif

    timeunit = timeunit(i+6:len_trim(timeunit))
    ! Make sure that this is in ISO format, i.e. YYYY-MM-DDThh:mm:ss
    ! Some implementations do not write 4 (or 2) digits single digit components.
    call timeString2ISOTimeString(timeUnit, ISOString, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> @todo consider "climatological month" as possible unit, and have a look at
    !> CF conventions on their climatological time handling.

    call MOSSCO_TimeSet(refTime, ISOString, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  end subroutine mossco_netcdf_reftime


#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_NcGetTime"
!> @brief the method MOSSCO_NcGetTime returns for a netcdf object
!> the ESMF Time for a requested index
!> @param[currTime] :: required, out
!> @param[searchIndex] :: optional, in, the index in the time array, default is 1
!> @param[stopTime] :: the maximum time (last index) in the time array
!> @param[startTime] :: the minimum time (first index) in the time array
!> @param[refTime] :: the reference time  in the time variable
  subroutine MOSSCO_NcGetTime(self, currTime, searchIndex, kwe, stopTime, startTime, refTime, rc)

    implicit none
    class(type_mossco_netcdf)                    :: self
    type(ESMF_Time), intent(out)                 :: currTime
    integer(ESMF_KIND_I4), intent(in), optional  :: searchIndex
    logical, optional                            :: kwe
    type(ESMF_Time), intent(out), optional       :: startTime, refTime, stopTime
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                        :: i, rc_, itime_, localrc, varid, ntime, index_
    character(ESMF_MAXSTR)                       :: timeUnit, message, timeString
    type(ESMF_TimeInterval)                      :: timeInterval
    real(ESMF_KIND_R8), allocatable              :: farray(:)
    type(ESMF_Time)                              :: refTime_

    rc_ = ESMF_SUCCESS
    if (present(searchIndex)) then
      index_ = searchIndex
    else
      index_ = 1
    endif

    call self%refTime(refTime_, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
      if (present(rc)) rc=localrc
      return
    endif

    if (present(refTime)) refTime=refTime_

    call ESMF_TimeGet(refTime_, timeStringISOFrac=timeString, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    write(message,'(A)') 'mossco_netcdf ref time is '//trim(timeString)
    !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

    ! Default time is refTime
    currTime = refTime_

    localrc = nf90_inq_varid(self%ncid, 'time', varid)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', no time variable to get time', ESMF_LOGMSG_INFO, ESMF_CONTEXT)
      if (present(rc)) rc=ESMF_RC_NOT_FOUND
      return
    endif

    localrc = nf90_get_att(self%ncid, varid, 'units', timeUnit)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', no time unit', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    i=index(timeUnit,'since ')
    if (i<1) then
      call ESMF_LogWrite('  unknown time unit "'//trim(timeUnit)//'"', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    timeUnit=timeUnit(1:i-2)

    ntime = self%dimlens(self%timeDimId)
    if (allocated(farray)) deallocate(farray)
    allocate(farray(ntime), stat=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (index_ < 1) index_ = 1
    if (index_ > ntime) index_ = ntime

    localrc = nf90_get_var(self%ncid, varid, farray)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (size(farray) == 0) then
      currTime = refTime_
      if (present(rc)) rc=ESMF_RC_NOT_FOUND
      return
    endif

    call MOSSCO_TimeIntervalFromTimeValue(timeInterval, timeUnit, farray(index_), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    currTime = refTime_ + timeInterval
    call ESMF_TimeGet(currTime, timeStringISOFrac=timeString, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    write(message,'(A)') 'mossco_netcdf cur time is '//trim(timeString)
    !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

    if (present(startTime)) then
      call MOSSCO_TimeIntervalFromTimeValue(timeInterval, timeUnit, farray(1), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      startTime = refTime_ + timeInterval

      call ESMF_TimeGet(startTime, timeStringISOFrac=timeString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message,'(A)') 'mossco_netcdf start time is '//trim(timeString)
      !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
    endif

    if (present(stopTime)) then
      call MOSSCO_TimeIntervalFromTimeValue(timeInterval, timeUnit, farray(1), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      stopTime = refTime_ + timeInterval
      call ESMF_TimeGet(stopTime, timeStringISOFrac=timeString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      write(message,'(A)') 'mossco_netcdf stop time is '//trim(timeString)
      !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
    endif

    if (allocated(farray)) deallocate(farray)
    if (present(rc)) rc=rc_

  end subroutine MOSSCO_NcGetTime

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_TimeIntervalFromTimeValue"
  subroutine MOSSCO_TimeIntervalFromTimeValue(timeInterval, unit, value, rc)

    type(ESMF_TimeInterval), intent(out) :: timeInterval
    character(len=*), intent(in)         :: unit
    real(ESMF_KIND_R8)                   :: value
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    type(ESMF_CalKind_Flag)              :: calKind
    integer(ESMF_KIND_I4)                :: localrc, rc_

    !> @todo make this an optional argument
    calKind = ESMF_CALKIND_GREGORIAN

    select case(trim(unit))
    case ('seconds')
      call ESMF_TimeIntervalSet(timeInterval, calKindFlag=calKind, s_r8=value, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    case ('minutes')
      call ESMF_TimeIntervalSet(timeInterval, calKindFlag=calKind, m=int(value, kind=ESMF_KIND_I4), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    case ('hours')
      call ESMF_TimeIntervalSet(timeInterval, calKindFlag=calKind, h_r8=value, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    case ('days')
      call ESMF_TimeIntervalSet(timeInterval, calKindFlag=calKind, d_r8=value, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    case ('months')
      call ESMF_TimeIntervalSet(timeInterval, calKindFlag=calKind, mm=int(value, kind=ESMF_KIND_I4), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    case ('years')
      call ESMF_TimeIntervalSet(timeInterval, calKindFlag=calKind, yy=int(value), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    case default
      call ESMF_LogWrite('  time unit "'//trim(unit)//'" not implemented', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end select

    if (present(rc)) rc=ESMF_SUCCESS
    return
  end subroutine MOSSCO_TimeIntervalFromTimeValue

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GetFreeLun"
  function MOSSCO_GetFreeLun(start, rc) result(unit)

    integer(ESMF_KIND_I4), intent(in) :: start
    integer(ESMF_KIND_I4) :: unit
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4) :: localrc
    logical               :: isopen

    if(present(rc)) rc=ESMF_SUCCESS

    unit=start
    do unit=1, huge(unit) - 1
      inquire(unit=unit, opened=isopen, iostat=localrc)
      if (localrc /= 0) cycle
      if (.not.isopen) return
    enddo

    unit=-1

  end function MOSSCO_GetFreeLun

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_AttributeNetcdfWriteArray"
  subroutine MOSSCO_AttributeNetcdfWriteArray(array, ncid, kwe, varid, rc)

    type(ESMF_Array), intent(in)     :: array
    integer, intent(in)              :: ncid
    logical, optional                :: kwe ! dummy keyword enforcer
    integer, intent(in), optional    :: varid
    integer, intent(out), optional   :: rc

    integer(ESMF_KIND_I4)            :: attributeCount, rc_, localrc, int4, ncStatus
    integer(ESMF_KIND_I4)            :: varid_, i
    integer(ESMF_KIND_I8)            :: int8
    real(ESMF_KIND_R8)               :: real8
    real(ESMF_KIND_R4)               :: real4
    character(len=ESMF_MAXSTR)       :: attributeName, string, message
    type(ESMF_Typekind_Flag)         :: typeKind
    logical                          :: logValue

    rc_ = ESMF_SUCCESS
    ncStatus=NF90_NOERR

    if (present(varid)) then
      varid_=varid
    else
      varid_=0 !> @todo get_globals here!
    endif

    call ESMF_AttributeGet(array, count=attributeCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, attributeCount
      call ESMF_AttributeGet(array, attributeIndex=i, name=attributeName, &
        typekind=typekind, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (typekind==ESMF_TYPEKIND_I4) then
        call ESMF_AttributeGet(array, attributeName, int4, rc=localrc)
        ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int4)
      elseif (typekind==ESMF_TYPEKIND_I8) then
        call ESMF_AttributeGet(array, attributeName, int8, rc=localrc)
        ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int8)
      elseif (typekind==ESMF_TYPEKIND_R4) then
        call ESMF_AttributeGet(array, attributeName, real4, rc=localrc)
        ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real4)
      elseif (typekind==ESMF_TYPEKIND_R8) then
        call ESMF_AttributeGet(array, attributeName, real8, rc=localrc)
        ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real8)
      elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
        call ESMF_AttributeGet(array, attributeName, string, rc=localrc)
        ncStatus = nf90_put_att(ncid,varid,trim(attributeName), trim(string))
      elseif (typekind==ESMF_TYPEKIND_LOGICAL) then
        call ESMF_AttributeGet(array, attributeName, logvalue, rc=localrc)
        if (logValue) then
          ncStatus = nf90_put_att(ncid,varid,trim(attributeName),'.true.')
       else
           ncStatus = nf90_put_att(ncid,varid,trim(attributeName),'.false.')
       endif
     else
       write(message, '(A,I2)') '  attribute '//trim(attributeName)//' has unknown typekind ',typeKind
       call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
     endif
    enddo

    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  could not write attribute '//trim(attributeName), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_AttributeNetcdfWriteArray

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_AttributeNetcdfWriteState"
  subroutine MOSSCO_AttributeNetcdfWriteState(state, ncid, kwe, varid, rc)

    type(ESMF_State), intent(in)     :: state
    integer, intent(in)              :: ncid
    logical, optional                :: kwe ! dummy keyword enforcer
    integer, intent(in), optional    :: varid
    integer, intent(out), optional   :: rc

    integer(ESMF_KIND_I4)            :: attributeCount, rc_, localrc, int4, ncStatus
    integer(ESMF_KIND_I4)            :: varid_, i
    integer(ESMF_KIND_I8)            :: int8
    real(ESMF_KIND_R8)               :: real8
    real(ESMF_KIND_R4)               :: real4
    character(len=ESMF_MAXSTR)       :: attributeName, string, message
    type(ESMF_Typekind_Flag)         :: typeKind
    logical                          :: logValue

    rc_ = ESMF_SUCCESS
    ncStatus=NF90_NOERR

    if (present(varid)) then
      varid_=varid
    else
      varid_=0 !> @todo get_globals here!
    endif

    call ESMF_AttributeGet(state, count=attributeCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, attributeCount
      call ESMF_AttributeGet(state, attributeIndex=i, name=attributeName, &
        typekind=typekind, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (typekind==ESMF_TYPEKIND_I4) then
        call ESMF_AttributeGet(state, attributeName, int4, rc=localrc)
        ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int4)
      elseif (typekind==ESMF_TYPEKIND_I8) then
        call ESMF_AttributeGet(state, attributeName, int8, rc=localrc)
        ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int8)
      elseif (typekind==ESMF_TYPEKIND_R4) then
        call ESMF_AttributeGet(state, attributeName, real4, rc=localrc)
        ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real4)
      elseif (typekind==ESMF_TYPEKIND_R8) then
        call ESMF_AttributeGet(state, attributeName, real8, rc=localrc)
        ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real8)
      elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
        call ESMF_AttributeGet(state, attributeName, string, rc=localrc)
        ncStatus = nf90_put_att(ncid,varid,trim(attributeName), trim(string))
      elseif (typekind==ESMF_TYPEKIND_LOGICAL) then
        call ESMF_AttributeGet(state, attributeName, logvalue, rc=localrc)
        if (logValue) then
          ncStatus = nf90_put_att(ncid,varid,trim(attributeName),'.true.')
       else
           ncStatus = nf90_put_att(ncid,varid,trim(attributeName),'.false.')
       endif
     else
       write(message, '(A,I2)') '  attribute '//trim(attributeName)//' has unknown typekind ',typeKind
       call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
     endif
    enddo

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  could not write attribute '//trim(attributeName), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_AttributeNetcdfWriteState

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_AttributeNetcdfWriteField"
  subroutine MOSSCO_AttributeNetcdfWriteField(field, ncid, kwe, varid, rc)

    type(ESMF_Field), intent(in)     :: field
    integer, intent(in)              :: ncid
    logical, optional                :: kwe ! dummy keyword enforcer
    integer, intent(in), optional    :: varid
    integer, intent(out), optional   :: rc

    integer(ESMF_KIND_I4)            :: attributeCount, rc_, localrc, ncStatus
    integer(ESMF_KIND_I4)            :: varid_, i, precision
    integer(ESMF_KIND_I8)            :: longint
    integer(ESMF_KIND_I4)            :: shortint
    real(ESMF_KIND_R8)               :: real8
    real(ESMF_KIND_R4)               :: real4
    character(len=ESMF_MAXSTR)       :: attributeName, string, message
    type(ESMF_Typekind_Flag)         :: typeKind
    logical                          :: logValue

    rc_ = ESMF_SUCCESS
    ncStatus = NF90_NOERR

    if (present(varid)) then
      varid_=varid
    else
      varid_=0 !> @todo get_globals here!
    endif

    ncStatus = nf90_inquire_variable(ncid, varid_, xtype = precision)
    if (ncStatus /= NF90_NOERR) then
      write(message, '(A)') '  '//trim(nf90_strerror(ncStatus)//', could not get precision')
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_AttributeGet(field, count=attributeCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, attributeCount
      call ESMF_AttributeGet(field, attributeIndex=i, name=attributeName, &
        typekind=typekind, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
        !write(0,*) i, attributeName, typeKind
        cycle
        !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      if (typekind==ESMF_TYPEKIND_I4) then
        call ESMF_AttributeGet(field, attributeName, shortint, rc=localrc)
        if ((  trim(attributeName) == 'missing_value' &
          .or. trim(attributeName) == '_FillValue')) then
          if (precision == NF90_INT) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int(shortint,kind=ESMF_KIND_I8))
          if (precision == NF90_SHORT) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int(shortint,kind=ESMF_KIND_I4))
          if (precision == NF90_DOUBLE) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real(shortint,kind=ESMF_KIND_R8))
          if (precision == NF90_REAL) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real(shortint,kind=ESMF_KIND_R4))
        else
          ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),shortint)
        endif
      elseif (typekind==ESMF_TYPEKIND_I8) then
        call ESMF_AttributeGet(field, attributeName, longint, rc=localrc)
        if ((  trim(attributeName) == 'missing_value' &
          .or. trim(attributeName) == '_FillValue')) then
          if (precision == NF90_INT) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int(longint,kind=ESMF_KIND_I8))
          if (precision == NF90_SHORT) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int(longint,kind=ESMF_KIND_I4))
          if (precision == NF90_DOUBLE) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real(longint,kind=ESMF_KIND_R8))
          if (precision == NF90_REAL) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real(longint,kind=ESMF_KIND_R4))
        else
          ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),longint)
        endif
      elseif (typekind==ESMF_TYPEKIND_R4) then
        call ESMF_AttributeGet(field, attributeName, real4, rc=localrc)
        if ((  trim(attributeName) == 'missing_value' &
          .or. trim(attributeName) == '_FillValue')) then
          if (precision == NF90_INT) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int(real4,kind=ESMF_KIND_I8))
          if (precision == NF90_SHORT) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int(real4,kind=ESMF_KIND_I4))
          if (precision == NF90_DOUBLE) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real(real4,kind=ESMF_KIND_R8))
          if (precision == NF90_REAL) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real(real4,kind=ESMF_KIND_R4))
        else
          ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real4)
        endif
      elseif (typekind==ESMF_TYPEKIND_R8) then
        call ESMF_AttributeGet(field, attributeName, real8, rc=localrc)
        if ((  trim(attributeName) == 'missing_value' &
          .or. trim(attributeName) == '_FillValue'))  then
          if (precision == NF90_INT) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int(real8,kind=ESMF_KIND_I8))
          if (precision == NF90_SHORT) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),int(real8,kind=ESMF_KIND_I4))
          if (precision == NF90_DOUBLE) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real(real8,kind=ESMF_KIND_R8))
          if (precision == NF90_REAL) ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real(real8,kind=ESMF_KIND_R4))
        else
          ncStatus = nf90_put_att(ncid,varid_,trim(attributeName),real8)
        endif
      elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
        call ESMF_AttributeGet(field, attributeName, string, rc=localrc)
        ncStatus = nf90_put_att(ncid,varid,trim(attributeName), trim(string))
      elseif (typekind==ESMF_TYPEKIND_LOGICAL) then
        call ESMF_AttributeGet(field, attributeName, logvalue, rc=localrc)
        if (logValue) then
          ncStatus = nf90_put_att(ncid,varid,trim(attributeName),'.true.')
       else
           ncStatus = nf90_put_att(ncid,varid,trim(attributeName),'.false.')
       endif
     else
       write(message, '(A,I2)') '  attribute '//trim(attributeName)//' has unknown typekind ',typeKind
       call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
     endif
    enddo

    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  could not write attribute '//trim(attributeName), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_AttributeNetcdfWriteField

#undef  ESMF_METHOD
#define ESMF_METHOD "mossco_netcdf_var_get_att"
  subroutine mossco_netcdf_var_get_att(self, field, var, rc)

    implicit none
    class(type_mossco_netcdf)                    :: self
    type(ESMF_Field), intent(inout)              :: field
    type(type_mossco_netcdf_variable)            :: var
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                        :: localrc, rc_, attributeCount
    integer(ESMF_KIND_I4)                        :: i

    integer(ESMF_KIND_I4)                        :: int4, attributeType, attributeLength
    integer(ESMF_KIND_I8)                        :: int8
    real(ESMF_KIND_R8)                           :: real8
    real(ESMF_KIND_R4)                           :: real4
    character(len=ESMF_MAXSTR)                   :: attributeName, string, message

    rc_ = ESMF_SUCCESS

    localrc = nf90_inquire_variable(self%ncid, var%varid, nAtts=attributeCount)
    if (localrc /= NF90_NOERR) then
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    do i=1, attributeCount

      localrc = nf90_inq_attname(self%ncid, var%varid, i, attributeName)
      if (localrc /= NF90_NOERR) then
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      localrc = nf90_inquire_attribute(self%ncid, var%varid, attributeName, &
        xtype=attributeType, len=attributeLength)
      if (localrc /= NF90_NOERR) then
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      if (attributeLength>1) then
        cycle
        !> @todo how do we handle attributes that are of lenght>1?
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = localrc
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) return
      endif

      ! Defined values for attributeType are:
      ! NF90_BYTE, NF90_CHAR, NF90_SHORT, NF90_INT, NF90_FLOAT, and NF90_DOUBLE.

      if (attributeType == NF90_CHAR) then
        localrc = nf90_get_att(self%ncid, var%varid, attributeName,  string)
        if (localrc /= NF90_NOERR) then
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        call ESMF_AttributeSet(field, trim(attributeName), trim(string), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        cycle
      endif

      if (attributeType == NF90_DOUBLE) then
        localrc = nf90_get_att(self%ncid, var%varid, attributeName,  real8)
        if (localrc /= NF90_NOERR) then
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        call ESMF_AttributeSet(field, trim(attributeName), real8, rc=localrc)
      elseif (attributeType == NF90_FLOAT) then
        localrc = nf90_get_att(self%ncid, var%varid, attributeName,  real4)
        if (localrc /= NF90_NOERR) then
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        call ESMF_AttributeSet(field, trim(attributeName), real4, rc=localrc)
      elseif (attributeType == NF90_INT) then
        localrc = nf90_get_att(self%ncid, var%varid, attributeName, int8)
        if (localrc /= NF90_NOERR) then
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        call ESMF_AttributeSet(field, trim(attributeName),int8, rc=localrc)
      elseif (attributeType == NF90_SHORT) then
        localrc = nf90_get_att(self%ncid, var%varid, attributeName,  int4)
        if (localrc /= NF90_NOERR) then
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        call ESMF_AttributeSet(field, trim(attributeName), int4, rc=localrc)
      else
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = localrc
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) return
      endif
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    ! Issue warnings for (almost always) required attributes units and missing_value
    localrc = nf90_inquire_attribute(self%ncid, var%varid, 'missing_value')
    if (localrc /= NF90_NOERR) then
      write(message,'(A)') '  there is no missing_value attribute in variable '//trim(var%name)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      write(message,'(A)') '  this might severely slow down your simulation'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    endif

    if (present(rc)) rc = rc_

  end subroutine mossco_netcdf_var_get_att

#undef  ESMF_METHOD
#define ESMF_METHOD "create"
   subroutine create(self, filename, kwe, timeUnit, state, rc)

    use iso_fortran_env
    implicit none

    class(type_mossco_netcdf)             :: self
    character(len=*), intent(in)          :: filename
    logical, optional, intent(in)         :: kwe ! Keyword-enforcer
    character(len=*),optional, intent(in) :: timeUnit
    type(ESMF_State), optional, intent(in):: state
    integer, intent(out),optional :: rc

    integer                       :: ncStatus
    character(len=255)            :: string
    integer                       :: rc_, localrc
    logical                       :: isPresent

    inquire(file=trim(filename), exist=isPresent)

    ncStatus = nf90_create(trim(filename), NF90_CLOBBER, self%ncid)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot create file '//trim(filename), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    else
      if (ispresent) then
        call ESMF_LogWrite('  overwriting file '//trim(filename), ESMF_LOGMSG_INFO)
      else
        call ESMF_LogWrite('  created new file '//trim(filename), ESMF_LOGMSG_INFO)
      endif
    endif

    if (present(timeUnit)) then
      self%timeUnit=trim(timeUnit)
      call self%init_time(rc=rc_)
      call ESMF_LogWrite('  file '//trim(filename)//' has time unit '//trim(timeUnit), ESMF_LOGMSG_INFO)
    else
      self%timeUnit=''
      call ESMF_LogWrite('  file '//trim(filename)//' has no time unit', ESMF_LOGMSG_WARNING)
    endif

    ncStatus = nf90_redef(self%ncid)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot enter define mode for '//trim(filename), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    if (present(state)) then
      !call MOSSCO_AttributeNetcdfWrite(state, self%ncid, varid=NF90_GLOBAL, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      call ESMF_LogWrite('  '//' obtained no information for global attributes in '//trim(filename), ESMF_LOGMSG_WARNING)
    endif

    ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'mossco_sha_key',MOSSCO_GIT_SHA_KEY)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute mossco_sha_key', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

! #ifndef NO_ISO_FORTRAN_ENV
!     !> @todo check cross-platform compatibility of the iso_fortran_env calls
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'compile_compiler_version',compiler_version())
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute compile_compiler_version', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'compile_compiler_options',compiler_options())
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute compile_compiler_options', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
! #endif
!
!     call get_command(string)
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'run_command_line',trim(string))
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute run_command_line', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     call getcwd(string)
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'run_working_directory',trim(string))
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute run_working_directory', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
! #ifndef NO_ISO_FORTRAN_ENV
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'run_process_id',getpid())
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute run_process_id', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
! #endif
!     !> @todo check cross-platform compatibility of these gnu extensions
!     call getlog(string)
! #ifndef NO_ISO_FORTRAN_ENV
!     write(string,'(A,I5,A,I5,A)') trim(string)// '(id=',getuid(),', gid=',getgid(),')'
! #endif
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'run_user',trim(string))
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute run_user', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     call hostnm(string)
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'run_hostname',trim(string))
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute run_hostname', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     !>@todo move this to a place where it reads attributes of a state/gridComp (toplevel/main), such that information
!     !> from the copuling specification is represented here
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'title','MOSSCO coupled simulation')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute title', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'institution','MOSSCO partners (HZG, IOW, and BAW)')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute institution', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'institution_hzg','Helmholtz-Zentrum Geesthacht')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute institution_hzg', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'institution_iow','Institut für Ostseeforschung Warnemünde')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute institution_iow', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'institution_baw','Bundesanstalt für Wasserbau')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute institution_baw', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'history','Created by MOSSCO')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute history', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'source','model_mossco')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute source', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'references','http://www.mossco.de/doc')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute references', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif
!
!     ncStatus = nf90_put_att(self%ncid,NF90_GLOBAL,'comment','')
!     if (ncStatus /= NF90_NOERR) then
!       call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot write attribute comment', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
!       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     endif

    ncStatus = nf90_enddef(self%ncid)
    if (ncStatus /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(ncStatus))//', cannot end definition mode', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      if (present(rc)) then
        rc = ESMF_RC_FILE_WRITE
        return
      else
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif

    self%name=trim(filename)

  end subroutine create

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_NcPutAttString"
  subroutine MOSSCO_NcPutAttString(self, varid, key, value, kwe, rc)

    class(type_mossco_netcdf)                           :: self
    integer, intent(in)                                 :: varid
    character(len=*), intent(in)                        :: key, value
    type(ESMF_KeyWordEnforcer), optional, intent(in)    :: kwe
    integer(ESMF_KIND_I4), intent(out), optional        :: rc

    integer(ESMF_KIND_I4)                  :: localrc, rc_

    rc_ = ESMF_SUCCESS

    localrc = nf90_put_att(self%ncid, varid, trim(key), trim(value))
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call ESMF_LogWrite('   cannot write attribute "'//trim(key)//'"="'//trim(value) &
        //'" to file "'//trim(self%name)//'"', ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      rc_ = ESMF_RC_FILE_WRITE
    endif

    if (present(rc)) rc = rc_
  end subroutine MOSSCO_NcPutAttString

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_NcPutAttInt"
  subroutine MOSSCO_NcPutAttInt(self, varid, key, value, kwe, rc)

    class(type_mossco_netcdf)                           :: self
    integer, intent(in)                                 :: varid
    character(len=*), intent(in)                        :: key
    integer(ESMF_KIND_I4)                               :: value
    type(ESMF_KeyWordEnforcer), optional, intent(in)    :: kwe
    integer(ESMF_KIND_I4), intent(out), optional        :: rc

    integer(ESMF_KIND_I4)                  :: localrc, rc_
    character(len=ESMF_MAXSTR)             :: message

    rc_ = ESMF_SUCCESS

    localrc = nf90_put_att(self%ncid, varid, trim(key), value)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      write(message,'(A,ES10.3,A)') '   cannot write attribute "'//trim(key)//'"="', &
        value,'" to file "'//trim(self%name)//'"'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      rc_ = ESMF_RC_FILE_WRITE
    endif

    if (present(rc)) rc = rc_
  end subroutine MOSSCO_NcPutAttInt

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_NcPutAttDouble"
    subroutine MOSSCO_NcPutAttDouble(self, varid, key, value, kwe, rc)

      class(type_mossco_netcdf)                           :: self
      integer, intent(in)                                 :: varid
      character(len=*), intent(in)                        :: key
      real(ESMF_KIND_R8)                                  :: value
      type(ESMF_KeyWordEnforcer), optional, intent(in)    :: kwe
      integer(ESMF_KIND_I4), intent(out), optional        :: rc

      integer(ESMF_KIND_I4)                  :: localrc, rc_
      character(len=ESMF_MAXSTR)             :: message

      rc_ = ESMF_SUCCESS

      localrc = nf90_put_att(self%ncid, varid, trim(key), value)
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        write(message,'(A,ES10.3,A)') '   cannot write attribute "'//trim(key)//'"="', &
          value,'" to file "'//trim(self%name)//'"'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        rc_ = ESMF_RC_FILE_WRITE
      endif

      if (present(rc)) rc = rc_
    end subroutine MOSSCO_NcPutAttDouble

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_NcPutAttFloat"
  subroutine MOSSCO_NcPutAttFloat(self, varid, key, value, kwe, rc)

    class(type_mossco_netcdf)                           :: self
    integer, intent(in)                                 :: varid
    character(len=*), intent(in)                        :: key
    real(ESMF_KIND_R4)                                  :: value
    type(ESMF_KeyWordEnforcer), optional, intent(in)    :: kwe
    integer(ESMF_KIND_I4), intent(out), optional        :: rc

    integer(ESMF_KIND_I4)                  :: localrc, rc_
    character(len=ESMF_MAXSTR)             :: message

    rc_ = ESMF_SUCCESS

    localrc = nf90_put_att(self%ncid, varid, trim(key), value)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc)), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      write(message,'(A,ES10.3,A)') '   cannot write attribute "'//trim(key)//'"="', &
        value,'" to file "'//trim(self%name)//'"'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      rc_ = ESMF_RC_FILE_WRITE
    endif

    if (present(rc)) rc = rc_
  end subroutine MOSSCO_NcPutAttFloat

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridAddMaskFromVariable"
!> Add from a variable and its missingValue/_FillValue/out of valid_range
!> attributes a mask to a grid
  subroutine MOSSCO_GridAddMaskFromVariable(grid, filename, varname, kwe, owner, rc)

    type(ESMF_Grid), intent(inout)                     :: grid
    character(len=*), intent(in)                       :: filename, varname
    type(ESMF_KeyWordEnforcer), intent(in), optional   :: kwe
    character(len=*), intent(in), optional             :: owner
    integer(ESMF_KIND_I4), intent(out), optional       :: rc

    integer(ESMF_KIND_I4)         :: rc_, localrc, rank
    logical                       :: isPresent
    character(len=ESMF_MAXSTR)    :: owner_, message
    type(type_mossco_netcdf)      :: nc
    type(ESMF_Array)              :: array
    real(ESMF_KIND_R8)            :: real8
    real(ESMF_KIND_R8), pointer   :: farrayPtr2(:,:) => null()
    integer(ESMF_KIND_I4), allocatable :: mask(:,:)
    type(type_mossco_netcdf_variable), pointer  :: var => null()
    type(ESMF_Field)              :: field
    type(ESMF_DistGrid)           :: distGrid
    integer(ESMF_KIND_I4), allocatable :: ubnd(:), lbnd(:)

    rc_ = ESMF_SUCCESS
    owner_ = '--'
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    if (present(owner)) call MOSSCO_StringCopy(owner_, owner)

    inquire(file=trim(filename), exist=isPresent)
    if (.not.isPresent) then
      if (present(rc)) rc = ESMF_RC_FILE_OPEN
      write(message,'(A)') trim(owner_)//' cannot find file '//trim(fileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      return
    endif

    nc = MOSSCO_NetcdfOpen(trim(fileName), mode='r', rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call nc%update_variables()

    var => nc%getvarvar(trim(varname), rc=localrc)
    if (localrc /= ESMF_SUCCESS) then
      if (present(rc)) rc = ESMF_RC_CANNOT_GET
      write(message,'(A)') trim(owner_)//' cannot find in file '//trim(fileName)//' variable '//trim(varname)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      call nc%close()
      return
    endif

    field = ESMF_FieldEmptyCreate(name='mask', rc=localrc)
    call ESMF_FieldEmptySet(field, grid=grid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_FieldEmptyComplete(field, typeKind=ESMF_TYPEKIND_R8, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call nc%getvar(field, var, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_FieldGet(field, rank=rank, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    allocate(ubnd(rank))
    allocate(lbnd(rank))

    call ESMF_FieldGet(field, farrayPtr=farrayPtr2, exclusiveUBound=ubnd, &
      exclusiveLbound=lbnd, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    allocate(mask(RANGE2D))
    mask(RANGE2D) = 0
    where(farrayPtr2(RANGE2D) /= farrayPtr2(RANGE2D))
      mask(RANGE2D) = 1
    endwhere

    call ESMF_AttributeGet(field, 'missing_value', real8, isPresent=isPresent, &
      defaultValue=-1.0D30, rc=localrc)
    if (isPresent) then
      where(farrayPtr2(RANGE2D) == real8)
        mask(RANGE2D) = 1
      endwhere
    endif

    call ESMF_AttributeGet(field, '_FillValue', real8, isPresent=isPresent, &
    defaultValue=-1.0D30, rc=localrc)
    if (isPresent) then
      where(farrayPtr2(RANGE2D) == real8)
        mask(RANGE2D) = 1
      endwhere
    endif

    call ESMF_AttributeGet(field, 'valid_min', real8, isPresent=isPresent, &
    defaultValue=-1.0D30, rc=localrc)
    if (isPresent) then
      where(farrayPtr2(RANGE2D) < real8)
        mask(RANGE2D) = 1
      endwhere
    endif

    call ESMF_AttributeGet(field, 'valid_max', real8, isPresent=isPresent, &
    defaultValue=-1.0D30, rc=localrc)
    if (isPresent) then
      where(farrayPtr2(RANGE2D) > real8)
        mask(RANGE2D) = 1
      endwhere
    endif

    call ESMF_GridGet(grid, distGrid=distGrid, rc=localrc)
    array = ESMF_ArrayCreate(distGrid, mask , indexflag=ESMF_INDEX_DELOCAL, rc=localrc)

    call ESMF_GridAddItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      itemflag=ESMF_GRIDITEM_MASK, rc=rc)

    call ESMF_GridSetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)

    call ESMF_FieldDestroy(field, rc=localrc)

    call nc%close()
    nullify(var)
    nullify(farrayPtr2)
    if (allocated(lbnd)) deallocate(lbnd)
    if (allocated(ubnd)) deallocate(ubnd)

  end subroutine MOSSCO_GridAddMaskFromVariable

end module
