!> @brief Implementation of staggerloc utilities
!!
!! This computer program is part of MOSSCO.
!! @copyright Copyright 2018 Helmholtz-Zentrum Geesthacht
!! @author Carsten Lemmen <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#define ESMF_FILENAME "mossco_staggerloc.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module mossco_staggerloc

  use esmf
  use mossco_strings

  implicit none
  private

  public MOSSCO_StaggerLocString

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
    call MOSSCO_MessageAdd(message,'O')
  elseif (staggerLoc == ESMF_STAGGERLOC_CORNER) then
    call MOSSCO_MessageAdd(message,'C')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE1) then
    call MOSSCO_MessageAdd(message,'X')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE2) then
    call MOSSCO_MessageAdd(message,'Y')
  elseif (staggerLoc == ESMF_STAGGERLOC_CENTER_VCENTER) then
    call MOSSCO_MessageAdd(message,'OO')
  elseif (staggerLoc == ESMF_STAGGERLOC_CORNER_VCENTER) then
    call MOSSCO_MessageAdd(message,'CO')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE1_VCENTER) then
    call MOSSCO_MessageAdd(message,'XO')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE2_VCENTER) then
    call MOSSCO_MessageAdd(message,'YO')
  elseif (staggerLoc == ESMF_STAGGERLOC_CORNER_VFACE) then
    call MOSSCO_MessageAdd(message,'CC')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE1_VFACE) then
    call MOSSCO_MessageAdd(message,'XC')
  elseif (staggerLoc == ESMF_STAGGERLOC_EDGE2_VFACE) then
    call MOSSCO_MessageAdd(message,'YC')
  elseif (staggerLoc == ESMF_STAGGERLOC_CENTER_VFACE) then
    call MOSSCO_MessageAdd(message,'OC')
  endif

  length_=len_trim(message)
  if (present(length)) length=length_
  if (present(rc)) rc=rc_

end subroutine MOSSCO_StaggerLocString

end module mossco_staggerloc
