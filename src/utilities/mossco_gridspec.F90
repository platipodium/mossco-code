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

  private

  integer, parameter :: MOSSCO_NC_NOERR=ESMF_SUCCESS

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridWriteGridSpec"
  subroutine MOSSCO_GridWriteGridSpec(grid, KeyWordEnforcer, name, rc)

    implicit none

    type(ESMF_Grid), intent(in)                  :: grid
    logical, optional, intent(in)                :: keyWordEnforcer
    character(len=*),optional, intent(in)        :: name
    integer(ESMF_KIND_I4), optional, intent(out) :: rc

    integer(ESMF_KIND_I4)                        :: rc_, localrc, dimCount, coordDimCount, staggerLocCount
    integer(ESMF_KIND_I4)                        :: localDeCount, rank
    character(len=ESMF_MAXSTR)                   :: message, gridName
    type(ESMF_TypeKind_Flag)                     :: coordTypeKind
    type(ESMF_CoordSys_Flag)                     :: coordSys
    type(ESMF_GridStatus_Flag)                   :: status

    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_GridGet(grid, status=status, name=gridName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (status /= ESMF_GRIDSTATUS_COMPLETE) then
      write(message,'(A)')  'Grid '//trim(gridName)//' is not complete, cannot create GRIDSPEC'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      return
    endif

    !call ESMF_GridGet(grid, coordTypeKind=coordTypeKind, dimCount=dimCount, staggerlocCount=staggerlocCount, &
    !  localDeCount=localDeCount, coordSys=coordSys, coordDimCount=coordDimCount, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   ! call ESMF_GridGet(grid, coordTypeKind=coordTypeKind, dimCount=dimCount, staggerlocCount=staggerlocCount, &
   !   localDeCount=localDeCount, coordSys=coordSys, coordDimCount=coordDimCount, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    if (present(rc)) rc=localrc

  end subroutine MOSSCO_GridWriteGridSpec

end module mossco_gridspec
