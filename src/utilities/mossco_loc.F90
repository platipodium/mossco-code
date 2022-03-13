!> @brief Implementation of staggerloc and meshloc utilities
!!
!! This computer program is part of MOSSCO.
!> @copyright 2021-2022 Helmholtz-Zentrum Hereon
!> @copyright 2018-2021 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hereon.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#define ESMF_FILENAME "mossco_loc.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module mossco_loc

  use esmf
  use mossco_strings

  implicit none
  private

  public MOSSCO_LocString

  interface MOSSCO_LocString
    module procedure MOSSCO_StaggerLocString
    module procedure MOSSCO_MeshLocString
  end interface MOSSCO_LocString

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StaggerLocString"
subroutine MOSSCO_StaggerLocString(staggerloc, message, kwe, length, options, rc)

  type(ESMF_StaggerLoc), intent(in)              :: staggerLoc
  character(len=ESMF_MAXSTR), intent(inout)      :: message
  logical, intent(in), optional                  :: kwe
  integer(ESMF_KIND_I4), intent(inout), optional :: length
  character(len=ESMF_MAXSTR), intent(in), allocatable, optional :: options(:)
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  integer(ESMF_KIND_I4)   :: rc_, length_, localrc, i

  character(len=ESMF_MAXSTR), allocatable  :: options_(:)

  rc_ = ESMF_SUCCESS

  if (present(kwe)) rc_ = ESMF_SUCCESS
  if (present(options)) then
    if (allocated(options)) then
      allocate(options_(size(options)), stat=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      do i=lbound(options,1),ubound(options,1)
        call MOSSCO_StringCopy(options_(i),options(i))
      enddo
    endif
  endif

  if (staggerLoc == ESMF_STAGGERLOC_CENTER) then
    call MOSSCO_MessageAdd(message,' O')
  elseif (staggerLoc == ESMF_STAGGERLOC_CORNER) then
    call MOSSCO_MessageAdd(message,' C')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE1) then
    call MOSSCO_MessageAdd(message,' X')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE2) then
    call MOSSCO_MessageAdd(message,' Y')
  elseif (staggerLoc == ESMF_STAGGERLOC_CENTER_VCENTER) then
    call MOSSCO_MessageAdd(message,' OO')
  elseif (staggerLoc == ESMF_STAGGERLOC_CORNER_VCENTER) then
    call MOSSCO_MessageAdd(message,' CO')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE1_VCENTER) then
    call MOSSCO_MessageAdd(message,' XO')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE2_VCENTER) then
    call MOSSCO_MessageAdd(message,' YO')
  elseif (staggerLoc == ESMF_STAGGERLOC_CORNER_VFACE) then
    call MOSSCO_MessageAdd(message,' CC')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE1_VFACE) then
    call MOSSCO_MessageAdd(message,' XC')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE2_VFACE) then
    call MOSSCO_MessageAdd(message,' YC')
  elseif (staggerLoc == ESMF_STAGGERLOC_CENTER_VFACE) then
    call MOSSCO_MessageAdd(message,' OC')
  endif

  length_=len_trim(message)
  if (present(length)) length=length_
  if (present(rc)) rc=rc_

end subroutine MOSSCO_StaggerLocString

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_MeshLocString"
subroutine MOSSCO_MeshLocString(meshLoc, message, kwe, length, options, rc)

  type(ESMF_MeshLoc), intent(in)              :: meshLoc
  character(len=ESMF_MAXSTR), intent(inout)      :: message
  logical, intent(in), optional                  :: kwe
  integer(ESMF_KIND_I4), intent(inout), optional :: length
  character(len=ESMF_MAXSTR), intent(in), allocatable, optional :: options(:)
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  integer(ESMF_KIND_I4)   :: rc_, length_, localrc, i

  character(len=ESMF_MAXSTR), allocatable  :: options_(:)

  rc_ = ESMF_SUCCESS

  if (present(kwe)) rc_ = ESMF_SUCCESS
  if (present(options)) then
    if (allocated(options)) then
      allocate(options_(size(options)), stat=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      do i=lbound(options,1),ubound(options,1)
        call MOSSCO_StringCopy(options_(i),options(i))
      enddo
    endif
  endif

  if (meshLoc == ESMF_MESHLOC_NODE) then
    call MOSSCO_MessageAdd(message,'N')
  elseif (meshLoc == ESMF_MESHLOC_ELEMENT) then
    call MOSSCO_MessageAdd(message,'E')
  endif

  length_=len_trim(message)
  if (present(length)) length=length_
  if (present(rc)) rc=rc_

end subroutine MOSSCO_MeshLocString

end module mossco_loc
