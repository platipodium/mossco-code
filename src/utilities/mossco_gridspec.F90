!> @brief Implementation GRIDSPEC utility functions
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2015 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "mossco_gridspec.F90"

module mossco_gridspec

  use mossco_strings
  use esmf
  use netcdf

  implicit none

  public MOSSCO_GridWriteGridSpec
  private

  integer, parameter :: MOSSCO_NC_NOERR=ESMF_SUCCESS

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridWriteGridSpec"
  subroutine MOSSCO_GridWriteGridSpec(grid, name, rc)

    implicit none

    type(ESMF_Grid), intent(in)                  :: grid
!    logical, optional, intent(in)                :: keyWordEnforcer
    character(len=*),optional, intent(in)        :: name
    integer(ESMF_KIND_I4), optional, intent(out) :: rc

    integer(ESMF_KIND_I4)                        :: rc_, localrc, dimCount, coordDimCount, staggerLocCount
    integer(ESMF_KIND_I4)                        :: localDeCount, rank, i
    character(len=ESMF_MAXSTR)                   :: message, gridName, dimName
    type(ESMF_TypeKind_Flag)                     :: coordTypeKind
    type(ESMF_CoordSys_Flag)                     :: coordSys
    type(ESMF_GridStatus_Flag)                   :: status

    integer      :: ncid, varid, dimid
    integer(ESMF_KIND_I4), allocatable           :: dimids(:), ubnd(:), lbnd(:)

    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_GridGet(grid, status=status, name=gridName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (status /= ESMF_GRIDSTATUS_COMPLETE) then
      write(message,'(A)')  'Grid '//trim(gridName)//' is not complete, cannot create GRIDSPEC.'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      return
    endif

    call ESMF_GridGet(grid, coordSys=coordSys, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (coordSys /= ESMF_COORDSYS_SPH_DEG) then
      write(message,'(A)')  'Grid '//trim(gridName)//' is not spherical/degrees type, cannot create GRIDSPEC.'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      return
    endif

    localrc = nf90_create(trim(gridName)//'_gridspec.nc', NF90_CLOBBER, ncid)
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot create file '// &
        trim(gridName)//'_gridspec.nc', ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    !>@todo move this to a place where it reads attributes of a state/gridComp (toplevel/main), such that information
    !> from the copuling specification is represented here
    localrc = nf90_put_att(ncid,NF90_GLOBAL,'title','MOSSCO coupled simulation grid specification')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot write attribute title', ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(ncid,NF90_GLOBAL,'institution','MOSSCO partners (HZG, IOW, and BAW)')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot write attribute institution', ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(ncid,NF90_GLOBAL,'institution_hzg','Helmholtz-Zentrum Geesthacht')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot write attribute institution_hzg', ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(ncid,NF90_GLOBAL,'institution_iow','Institut für Ostseeforschung Warnemünde')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot write attribute institution_iow', ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(ncid,NF90_GLOBAL,'institution_baw','Bundesanstalt für Wasserbau')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot write attribute institution_baw', ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(ncid,NF90_GLOBAL,'history','Created by MOSSCO')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot write attribute history', ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(ncid,NF90_GLOBAL,'source','model_mossco')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot write attribute source', ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(ncid,NF90_GLOBAL,'references','http://www.mossco.de/doc')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot write attribute references', ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    localrc = nf90_put_att(ncid,NF90_GLOBAL,'comment','')
    if (localrc /= NF90_NOERR) then
      call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot write attribute comment', ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    call ESMF_GridGet(grid, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(ubnd(rank))
    ubnd(:)=1
    allocate(lbnd(rank))
    lbnd(:)=1
    allocate(dimids(rank))
    dimids(:)=-1

    call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, localDe=0, exclusiveCount=ubnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, rank
      write(dimName, '(A,I1.1)') 'dim_',i
      localrc = nf90_def_dim(ncid, trim(dimName), ubnd(i)-lbnd(i)+1,dimids(i))
      if (localrc /= NF90_NOERR) then
        call ESMF_LogWrite('  '//trim(nf90_strerror(localrc))//', cannot create dimension '//trim(dimName), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo

   ! nc.createDimension('bound',2)
   ! nc.createDimension('lon',nlon)
   ! nc.createDimension('lat',nlat)



    !call ESMF_GridGet(grid, coordTypeKind=coordTypeKind, dimCount=dimCount, staggerlocCount=staggerlocCount, &
    !  localDeCount=localDeCount, coordSys=coordSys, coordDimCount=coordDimCount, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   ! call ESMF_GridGet(grid, coordTypeKind=coordTypeKind, dimCount=dimCount, staggerlocCount=staggerlocCount, &
   !   localDeCount=localDeCount, coordSys=coordSys, coordDimCount=coordDimCount, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    localrc = nf90_close(ncid)

    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(lbnd)) deallocate(lbnd)
    if (allocated(dimids)) deallocate(dimids)

    if (present(rc)) rc=localrc

  end subroutine MOSSCO_GridWriteGridSpec

end module mossco_gridspec
