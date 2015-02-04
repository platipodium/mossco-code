!> @brief Implementation of additional ESMF Field utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2015, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen
!> @author Richard Hofmeister
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "mossco_field.F90"

module mossco_field

  use mossco_strings
  use esmf

  implicit none

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldString"
subroutine MOSSCO_FieldString(field, message, length, rc)

  type(ESMF_Field), intent(in)                   :: field
  character(len=ESMF_MAXSTR), intent(inout)      :: message
  integer(ESMF_KIND_I4), intent(inout), optional :: length
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  integer(ESMF_KIND_I4)   :: rc_, length_, rank, localrc
  integer(ESMF_KIND_I4)   :: ubnd1(1), ubnd2(2), ubnd3(3), ubnd4(4)
  integer(ESMF_KIND_I4)   :: lbnd1(1), lbnd2(2), lbnd3(3), lbnd4(4)
  character(ESMF_MAXSTR)  :: geomName, stringValue, name
  type(ESMF_Grid)         :: grid

  type(ESMF_GeomType_Flag) :: geomType
  type(ESMF_FieldStatus_Flag) :: fieldStatus
  logical                     :: isPresent

  rc_ = ESMF_SUCCESS

  call ESMF_FieldGet(field, name=name, status=fieldStatus, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_AttributeGet(field, name='creator', isPresent=isPresent, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  if (isPresent) then
    call ESMF_AttributeGet(field, name='creator', value=stringValue, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call MOSSCO_MessageAdd(message, ' ['//stringValue)
    call MOSSCO_MessageAdd(message, ']'//name)
  else
    call MOSSCO_MessageAdd(message,' '//name)
  endif

  if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
    call MOSSCO_MessageAdd(message,' (empty)')
  elseif (fieldStatus == ESMF_FIELDSTATUS_GRIDSET) then
    call MOSSCO_MessageAdd(message,' (gridset)')
  endif

  if (fieldStatus /= ESMF_FIELDSTATUS_EMPTY) then
    call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (geomtype==ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridGet(grid, name=geomName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call MOSSCO_MessageAdd(message,' '//geomName)
      call ESMF_GridGet(grid, rank=rank, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    elseif (geomtype==ESMF_GEOMTYPE_MESH) then
      call MOSSCO_MessageAdd(message,' mesh')
    elseif (geomtype==ESMF_GEOMTYPE_LOCSTREAM) then
      call MOSSCO_MessageAdd(message,' locstream')
    elseif (geomtype==ESMF_GEOMTYPE_XGRID) then
      call MOSSCO_MessageAdd(message,' xgrid')
    else
      write(message,'(A)') 'Unknown geometry type.'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
  endif

  if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
    call ESMF_FieldGet(field, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (len_trim(message) + 7<=len(message)) write(message,'(A,I1)') trim(message)//' rank ',rank

    !if (rank==1) then
    !  call ESMF_FieldGetBounds(field, localDe=0, exclusiveUBound=ubnd1, exclusiveLBound=lbnd1, rc=rc)
    !elseif (rank==2) then
    !  call ESMF_FieldGetBounds(field, localDe=0, exclusiveUBound=ubnd2, exclusiveLBound=lbnd2, rc=rc)
    !elseif (rank==3) then
    !  call ESMF_FieldGetBounds(field, localDe=0, exclusiveUBound=ubnd3, exclusiveLBound=lbnd3, rc=rc)
    !elseif (rank==4) then
    !  call ESMF_FieldGetBounds(field, localDe=0, exclusiveUBound=ubnd4, exclusiveLBound=lbnd4, rc=rc)
    !else
    !  write(0,*) 'NOT implemented: rank > 4'
    !endif
  endif

  length_=len_trim(message)
  if (present(length)) length=length_
  if (present(rc)) rc=rc_

end subroutine MOSSCO_FieldString

subroutine MOSSCO_FieldCopy(to, from, rc)

  type(ESMF_Field), intent(out)                  :: to
  type(ESMF_Field), intent(in)                   :: from
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  character(len=ESMF_MAXSTR)               :: message
  integer(ESMF_KIND_I4)                    :: rc_, toRank, fromRank, localrc
  integer(ESMF_KIND_I4), allocatable       :: fromUbnd(:), fromLbnd(:), toUbnd(:), toLbnd(:)
  character(ESMF_MAXSTR)                   :: fromName, toName

  real(ESMF_KIND_R8), pointer  :: fromFarrayPtr1(:), toFarrayPtr1(:)
  real(ESMF_KIND_R8), pointer  :: fromFarrayPtr2(:,:), toFarrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer  :: fromFarrayPtr3(:,:,:), toFarrayPtr3(:,:,:)
  real(ESMF_KIND_R8), pointer  :: fromFarrayPtr4(:,:,:,:), toFarrayPtr4(:,:,:,:)
  real(ESMF_KIND_R8), pointer  :: fromFarrayPtr5(:,:,:,:,:), toFarrayPtr5(:,:,:,:,:)
  real(ESMF_KIND_R8), pointer  :: fromFarrayPtr6(:,:,:,:,:,:), toFarrayPtr6(:,:,:,:,:,:)
  real(ESMF_KIND_R8), pointer  :: fromFarrayPtr7(:,:,:,:,:,:,:), toFarrayPtr7(:,:,:,:,:,:,:)

  type(ESMF_FieldStatus_Flag) :: fromStatus, toStatus

  rc_ = ESMF_SUCCESS

	call ESMF_FieldGet(from, status=fromStatus, rank=fromRank, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (fromStatus /= ESMF_FIELDSTATUS_COMPLETE) then
    write(message,'(A)') 'Cannot copy from incomplete field'
    call MOSSCO_FieldString(from, message)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  endif

  call ESMF_FieldGet(to, status=toStatus, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (toStatus /= ESMF_FIELDSTATUS_EMPTY) then
    !call MOSSCO_FieldComplete(to, from, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  endif

  call ESMF_FieldGet(to, rank=toRank, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (toRank /= fromRank) then
    write(message,'(A)') 'Cannot copy fields with incompatible rank, field'
    call MOSSCO_FieldString(from, message)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  endif

  allocate(fromUbnd(toRank), toUbnd(toRank), fromLbnd(toRank), toLBnd(toRank))
  call ESMF_FieldGetbounds(from, localDe=0,  exclusiveUBound=fromUBnd, exclusiveLBound=fromLbnd, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (toRank /= fromRank) then
    write(message,'(A)') 'Cannot copy fields with incompatible rank, field'
    call MOSSCO_FieldString(from, message)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  endif

  if (  any(toUbnd-toLBnd /= fromUBnd-fromLBnd) ) then
    write(message,'(A)') 'Cannot copy fields with incompatible bounds, field'
    call MOSSCO_FieldString(from, message)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  endif

  if (toRank == 1) then
    call ESMF_FieldGet(from, localDe=0,  farrayPtr=fromFarrayPtr1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldGet(from, localDe=0,  farrayPtr=toFarrayPtr1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    toFarrayPtr1(toLbnd(1):toUbnd(1)) = fromFarrayPtr1(fromLbnd(1):fromUbnd(1))
  else
    write(message,'(A)') 'Not yet implemented, copy rank>1 field'
    call MOSSCO_FieldString(from, message)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
  endif

  deallocate(fromUbnd, toUbnd, fromLbnd, toLbnd)

  if (present(rc)) rc = rc_

end subroutine MOSSCO_FieldCopy

end module mossco_field
