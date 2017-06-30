!> @brief Implementation of additional ESMF Field utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>
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

#define RANGE2D lbnd(1):ubnd(1),lbnd(2):ubnd(2)
#define RANGE3D lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)

module mossco_field

  use mossco_memory
  use mossco_strings
  use mossco_attribute
  use esmf

  implicit none

  public MOSSCO_FieldAttributesIdentical

  interface MOSSCO_FieldGetMissingValue
    module procedure MOSSCO_FieldGetMissingValueR8
  end interface MOSSCO_FieldGetMissingValue

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldString"
subroutine MOSSCO_FieldString(field, message, kwe, length, prefix, rc)

  type(ESMF_Field), intent(in)                     :: field
  character(len=*), intent(inout)                  :: message
  type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
  integer(ESMF_KIND_I4), intent(out), optional     :: length
  character(len=*), intent(in), optional           :: prefix
  integer(ESMF_KIND_I4), intent(out), optional     :: rc

  integer(ESMF_KIND_I4)   :: rc_, rank, localrc, gridRank, n, i, width
  integer(ESMF_KIND_I4), allocatable :: lbnd(:), ubnd(:), ungriddedLbnd(:), ungriddedUbnd(:)

  character(len=ESMF_MAXSTR)  :: geomName, stringValue, name, form
  character(len=ESMF_MAXSTR)  :: prefix_
  type(ESMF_Grid)             :: grid
  type(ESMF_GeomType_Flag)    :: geomType
  type(ESMF_FieldStatus_Flag) :: fieldStatus
  logical                     :: isPresent

  if (present(kwe)) localrc = ESMF_SUCCESS
  prefix_ = 'none'
  !> @todo The following line produced a segmentation fault
  if (present(prefix)) then
    if (len(prefix) > len(prefix_)) then
      prefix_ = trim(prefix(1:len(prefix_)))
    else
      prefix_ = trim(prefix)
    endif
  endif
  rc_ = ESMF_SUCCESS
  rank = 0
  gridRank = 0

  call ESMF_FieldGet(field, name=name, status=fieldStatus, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !if (trim(prefix_) == 'none') then
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
  !endif

  if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
    call MOSSCO_MessageAdd(message,' (empty)')
  elseif (fieldStatus == ESMF_FIELDSTATUS_GRIDSET) then
    call MOSSCO_MessageAdd(message,' (gridset)')
  endif

  if (fieldStatus /= ESMF_FIELDSTATUS_EMPTY) then
    call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if

    if (geomtype==ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridGet(grid, name=geomName, rank=gridRank, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_MessageAdd(message,' '//trim(geomName))


      if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
        call ESMF_FieldGet(field, rank=rank, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        rank=gridRank ! fall back to gridRank, if field not completed
        if (len_trim(message) + 7<=len(message)) write(message,'(A,I1)') trim(message)//' rank ',rank

        if (rank > 0) then
          allocate(ubnd(rank), stat=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          allocate(lbnd(rank), stat=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, localDe=0, &
            exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (len_trim(message) + 5 <=len(message)) then
            write(form,'(A)') '(A,'//intformat(ubnd(1)-lbnd(1)+1)//')'
            write(message,form) trim(message)//' (', ubnd(1)-lbnd(1)+1
          endif

          do i=2,rank
            if (ubnd(i)<lbnd(i)) then
              write(message,'(A)') '  bounds problem, please check your foreign_grid specification'
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            endif

            width=order(ubnd(i)-lbnd(i)+1)+1
            write(form,'(A)') '(A,'//intformat(ubnd(i)-lbnd(i)+1)//')'
            if (len_trim(message) + 1 + width <=len(message)) write(message,form) trim(message)//'x', ubnd(i)-lbnd(i)+1
          enddo

          if (len_trim(message) + 1 <=len(message)) write(message,'(A)') trim(message)//')'
        endif
      endif

      !! Check for ungridded dimensions
      n=rank-gridRank
      if (n>0) then
        allocate(ungriddedUbnd(n))
        allocate(ungriddedLbnd(n))
        call ESMF_FieldGet(field, ungriddedLBound=ungriddedLbnd, ungriddedUbound=ungriddedUbnd, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

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

    if (rank > 0) then
      allocate(ubnd(rank), stat=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      allocate(lbnd(rank), stat=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGetBounds(field, exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (len_trim(message) + 5 <=len(message)) then
        write(form,'(A)') '(A,'//intformat(ubnd(1)-lbnd(1)+1)//')'
        write(message,form) trim(message)//' (', ubnd(1)-lbnd(1)+1
      endif

      do i=2,gridRank
        if (ubnd(i)<lbnd(i)) then
          write(message,'(A)') '  bounds problem, please check your foreign_grid specification'
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

        width=order(ubnd(i)-lbnd(i)+1)+1
        write(form,'(A)') '(A,'//intformat(ubnd(i)-lbnd(i)+1)//')'
        if (len_trim(message) + 1 + width <=len(message)) write(message,form) trim(message)//'x', ubnd(i)-lbnd(i)+1
     enddo

      do i=gridRank+1, rank
        width=order(ubnd(i)-lbnd(i)+1)+1
        write(form,'(A)') '(A,'//intformat(ubnd(i)-lbnd(i)+1)//',A)'
        !write(0,*) i, gridRank, ubnd(i)-lbnd(i)+1, width, intformat(ubnd(i)-lbnd(i)+1)
        if (len_trim(message) + 2 + width <=len(message)) write(message,form) trim(message)//'x', ubnd(i)-lbnd(i)+1,'u'
      enddo

      if (len_trim(message) + 1 <=len(message)) write(message,'(A)') trim(message)//')'
    endif
  endif

  if (allocated(ubnd)) deallocate(ubnd)
  if (allocated(lbnd)) deallocate(lbnd)
  if (allocated(ungriddedUbnd)) deallocate(ungriddedUbnd)
  if (allocated(ungriddedLbnd)) deallocate(ungriddedLbnd)

  if (present(length)) length=len_trim(message)
  if (present(rc)) rc=rc_

end subroutine MOSSCO_FieldString

subroutine MOSSCO_FieldCopy(to, from, rc)

  type(ESMF_Field), intent(inout)                :: to
  type(ESMF_Field), intent(in)                   :: from
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  character(len=ESMF_MAXSTR)               :: message
  integer(ESMF_KIND_I4)                    :: rc_, toRank, fromRank, localrc
  integer(ESMF_KIND_I4), allocatable       :: fromUbnd(:), fromLbnd(:), toUbnd(:), toLbnd(:)

  real(ESMF_KIND_R8), pointer  :: fromFarrayPtr1(:), toFarrayPtr1(:)
  real(ESMF_KIND_R8), pointer  :: fromFarrayPtr2(:,:), toFarrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer  :: fromFarrayPtr3(:,:,:), toFarrayPtr3(:,:,:)
  !real(ESMF_KIND_R8), pointer  :: fromFarrayPtr4(:,:,:,:), toFarrayPtr4(:,:,:,:)
  !real(ESMF_KIND_R8), pointer  :: fromFarrayPtr5(:,:,:,:,:), toFarrayPtr5(:,:,:,:,:)
  !real(ESMF_KIND_R8), pointer  :: fromFarrayPtr6(:,:,:,:,:,:), toFarrayPtr6(:,:,:,:,:,:)
  !real(ESMF_KIND_R8), pointer  :: fromFarrayPtr7(:,:,:,:,:,:,:), toFarrayPtr7(:,:,:,:,:,:,:)

  type(ESMF_FieldStatus_Flag) :: fromStatus, toStatus
  type(ESMF_Grid)             :: grid
  type(ESMF_TypeKind_Flag)    :: typeKind
  type(ESMF_StaggerLoc)       :: staggerloc

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

  if (toStatus == ESMF_FIELDSTATUS_EMPTY) then
    call ESMF_FieldGet(from, grid=grid, staggerloc=staggerloc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldEmptySet(to, grid=grid, staggerloc=staggerloc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  endif

  if (toStatus /= ESMF_FIELDSTATUS_COMPLETE) then
    call ESMF_FieldGet(from, typeKind=typeKind, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldEmptyComplete(to, typeKind=typeKind, rc=localrc)
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

  call ESMF_FieldGetbounds(to, localDe=0,  exclusiveUBound=toUBnd, exclusiveLBound=toLbnd, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (  any(toUbnd-toLBnd /= fromUBnd-fromLBnd) ) then
    write(message,'(A)') 'Cannot copy fields with incompatible bounds, field'
    call MOSSCO_FieldString(from, message)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    !write(message,*) toUbnd, toLbnd, fromUbnd, fromLBnd
    !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  endif

  if (toRank == 1) then
    call ESMF_FieldGet(from, localDe=0,  farrayPtr=fromFarrayPtr1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldGet(to, localDe=0,  farrayPtr=toFarrayPtr1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    toFarrayPtr1(toLbnd(1):toUbnd(1)) = fromFarrayPtr1(fromLbnd(1):fromUbnd(1))
  elseif (toRank == 2) then
    call ESMF_FieldGet(from, localDe=0,  farrayPtr=fromFarrayPtr2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldGet(to, localDe=0,  farrayPtr=toFarrayPtr2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    toFarrayPtr2(toLbnd(1):toUbnd(1),toLbnd(2):toUbnd(2)) &
      = fromFarrayPtr2(fromLbnd(1):fromUbnd(1), fromLbnd(2):fromUbnd(2))
  elseif (toRank == 3) then
    call ESMF_FieldGet(from, localDe=0,  farrayPtr=fromFarrayPtr3, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldGet(to, localDe=0,  farrayPtr=toFarrayPtr3, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    toFarrayPtr3(toLbnd(1):toUbnd(1),toLbnd(2):toUbnd(2),toLbnd(3):toUbnd(3)) &
      = fromFarrayPtr3(fromLbnd(1):fromUbnd(1),fromLbnd(2):fromUbnd(2), fromLbnd(3):fromUbnd(3))
  else
    write(message,'(A)') 'Not yet implemented, copy rank>3 field '
    call MOSSCO_FieldString(from, message)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    rc_ = ESMF_RC_NOT_IMPL
  endif

  if (allocated(fromUbnd)) deallocate(fromUbnd)
  if (allocated(toUbnd))   deallocate(toUbnd)
  if (allocated(fromLbnd)) deallocate(fromLbnd)
  if (allocated(toLbnd))   deallocate(toLbnd)

  if (present(rc)) rc = rc_

end subroutine MOSSCO_FieldCopy

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldNameCheck"
  subroutine MOSSCO_FieldNameCheck(field, rc)

    type(ESMF_Field), intent(inout)              :: field
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc
    character(ESMF_MAXSTR)              :: name, standardName

    logical                             :: isPresent

    rc_ = ESMF_SUCCESS

    call ESMF_FieldGet(field, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(field, 'standard_name', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !if (.not.isPresent) then

      select case(trim(name))
        case ('Chl_chl_bottom_flux_hz')
          standardName='upward_flux_of_chlorophyll_at_soil_surface'
        case ('Chl_chl_surface_flux_hz')
          standardName='upward_flux_of_chlorophyll_at_water_surface'
        case ('Detritus_Carbon_detC_bottom_flux_hz')
          standardName='upward_flux_of_detritus_carbon_at_soil_surface'
        case ('Detritus_Carbon_detC_surface_flux_hz')
          standardName='upward_flux_of_detritus_carbon_at_water_surface'
        case ('Detritus_Carbon_detC_in_water')
          standardName='detritus_carbon_in_water'
        case ('Detritus_Carbon_detC_z_velocity_in_water')
          standardName='detritus_carbon_z_velocity_in_water'
        case ('Detritus_Nitrogen_detN_bottom_flux_hz')
          standardName='upward_flux_of_detritus_nitrogen_at_soil_surface'
        case ('Detritus_Nitrogen_detN_surface_flux_hz')
          standardName='upward_flux_of_detritus_nitrogen_at_water_surface'
        case ('Detritus_Nitrogen_detN_in_water')
          standardName='detritus_nitrogen_in_water'
        case ('Detritus_Nitrogen_detN_z_velocity_in_water')
          standardName='detritus_nitrogen_z_velocity_in_water'
        case ('Detritus_Phosphorous_detP_bottom_flux_hz')
          standardName='upward_flux_of_detritus_phosphorous_at_soil_surface'
        case ('Detritus_Phosphorous_detP_surface_flux_hz')
          standardName='upward_flux_of_detritus_phosphorous_at_water_surface'
        case ('Detritus_Phosphorous_detP_in_water')
          standardName='detritus_phosphorous_in_water'
        case ('Detritus_Phosphorous_detP_z_velocity_in_water')
          standardName='detritus_phosphorous_z_velocity_in_water'
        case ('Dissolved_Inorganic_Nitrogen_DIN_nutN_bottom_flux_hz')
          standardName='upward_flux_of_dissolved_inorganic_nitrogen_at_soil_surface'
        case ('Dissolved_Inorganic_Nitrogen_DIN_nutN_surface_flux_hz')
          standardName='upward_flux_of_dissolved_inorganic_at_water_surface'
        case ('Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water')
          standardName='dissolved_inorganic_nitrogen_in_water'
        case ('Dissolved_Inorganic_Nitrogen_DIN_nutN_z_velocity_in_water')
          standardName='dissolved_inorganic_nitrogen_z_velocity_in_water'
        case ('Dissolved_Inorganic_Phosphorus_DIP_nutP_bottom_flux_hz')
          standardName='upward_flux_of_dissolved_inorganic_phosphorous_at_soil_surface'
        case ('Dissolved_Inorganic_Phosphorus_DIP_nutP_surface_flux_hz')
          standardName='upward_flux_of_dissolved_inorganic_phosphorous_at_water_surface'
        case ('Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water')
          standardName='dissolved_inorganic_phosphorous_in_water'
        case ('Dissolved_Inorganic_Phosphorus_DIP_nutP_z_velocity_in_water')
          standardName='dissolved_inorganic_phosphorous_z_velocity_in_water'
        case ('Dissolved_Organic_Carbon_domC_bottom_flux_hz')
          standardName='upward_flux_of_dissolved_organic_carbon_at_soil_surface'
        case ('Dissolved_Organic_Carbon_domC_surface_flux_hz')
          standardName='upward_flux_of_dissolved_organic_carbon_at_water_surface'
        case ('Dissolved_Organic_Carbon_domC_in_water')
          standardName='dissolved_organic_carbon_in_water'
        case ('Dissolved_Organic_Carbon_domC_z_velocity_in_water')
          standardName='dissolved_organic_carbon_z_velocity_in_water'
        case ('Dissolved_Organic_Nitrogen_domN_bottom_flux_hz')
          standardName='upward_flux_of_dissolved_organic_nitrogen_at_soil_surface'
        case ('Dissolved_Organic_Nitrogen_domN_surface_flux_hz')
          standardName='upward_flux_of_dissolved_organic_nitrogen_at_water_surface'
        case ('Dissolved_Organic_Nitrogen_domN_in_water')
          standardName='dissolved_organic_nitrogen_in_water'
        case ('Dissolved_Organic_Nitrogen_domN_z_velocity_in_water')
          standardName='dissolved_organic_nitrogen_z_velocity_in_water'
        case ('Dissolved_Organic_Phosphorus_domP_bottom_flux_hz')
          standardName='upward_flux_of_dissolved_organic_phosphorous_at_soil_surface'
        case ('Dissolved_Organic_Phosphorus_domP_surface_flux_hz')
          standardName='upward_flux_of_dissolved_organic_phosphorous_at_water_surface'
        case ('Dissolved_Organic_Phosphorus_domP_in_water')
          standardName='dissolved_organic_phosphorous_in_water'
        case ('Dissolved_Organic_Phosphorus_domP_z_velocity_in_water')
          standardName='dissolved_organic_phosphorous_z_velocity_in_water'
        case ('Nitrate_no3_in_water')
          standardName='nitrate_in_water'
        case ('Photosynthetically_Active_Radiation_dPAR_in_water')
          standardName='photosynthetically_active_radiation_in_water'
        case ('Phytoplankton_Carbon_phyC_bottom_flux_hz')
          standardName='upward_flux_of_phytoplankton_carbon_at_soil_surface'
        case ('Phytoplankton_Carbon_phyC_surface_flux_hz')
          standardName='upward_flux_of_phytoplankton_carbon_at_water_surface'
        case ('Phytoplankton_Carbon_phyC_in_water')
          standardName='phytoplankton_carbon_in_water'
        case ('Phytoplankton_Carbon_phyC_z_velocity_in_water')
          standardName='phytoplankton_carbon_z_velocity_in_water'
        case ('Phytoplankton_Nitrogen_phyN_bottom_flux_hz')
          standardName='upward_flux_of_phytoplankton_nitrogen_at_soil_surface'
        case ('Phytoplankton_Nitrogen_phyN_surface_flux_hz')
          standardName='upward_flux_of_phytoplankton_nitrogen_at_water_surface'
        case ('Phytoplankton_Nitrogen_phyN_in_water')
          standardName='phytoplankton_nitrogen_in_water'
        case ('Phytoplankton_Nitrogen_phyN_z_velocity_in_water')
          standardName='phytoplankton_nitrogen_z_velocity_in_water'
        case ('Phytoplankton_Phosphorous_phyP_bottom_flux_hz')
          standardName='upward_flux_of_phytoplankton_phosphorous_at_soil_surface'
        case ('Phytoplankton_Phosphorous_phyP_surface_flux_hz')
          standardName='upward_flux_of_phytoplankton_phosphorous_at_water_surface'
        case ('Phytoplankton_Phosphorous_phyP_in_water')
          standardName='phytoplankton_phosphorous_in_water'
        case ('Phytoplankton_Phosphorous_phyP_z_velocity_in_water')
          standardName='phytoplankton_phosphorous_z_velocity_in_water'
        case ('Zooplankton_Carbon_zooC_bottom_flux_hz')
          standardName='upward_flux_of_zooplankton_carbon_at_soil_surface'
        case ('Zooplankton_Carbon_zooC_surface_flux_hz')
          standardName='upward_flux_of_zooplankton_carbon_at_water_surface'
        case ('Zooplankton_Carbon_zooC_in_water')
          standardName='zooplankton_carbon_in_water'
        case ('Zooplankton_Carbon_zooC_z_velocity_in_water')
          standardName='zooplankton_carbon_z_velocity_in_water'
        case ('Zooplankton_Nitrogen_zooN_bottom_flux_hz')
          standardName='upward_flux_of_zooplankton_nitrogen_at_soil_surface'
        case ('Zooplankton_Nitrogen_zooN_surface_flux_hz')
          standardName='upward_flux_of_zooplankton_nitrogen_at_water_surface'
        case ('Zooplankton_Nitrogen_zooN_in_water')
          standardName='zooplankton_nitrogen_in_water'
        case ('Zooplankton_Nitrogen_zooN_z_velocity_in_water')
          standardName='zooplankton_nitrogen_z_velocity_in_water'
        case ('Zooplankton_Phosphorous_zooP_bottom_flux_hz')
          standardName='upward_flux_of_zooplankton_phosphorous_at_soil_surface'
        case ('Zooplankton_Phosphorous_zooP_surface_flux_hz')
          standardName='upward_flux_of_zooplankton_phosphorous_at_water_surface'
        case ('Zooplankton_Phosphorous_zooP_in_water')
          standardName='zooplankton_phosphorous_in_water'
        case ('Zooplankton_Phosphorous_zooP_z_velocity_in_water')
          standardName='zooplankton_phosphorous_z_velocity_in_water'
!concentration_of_SPM_bottom_flux_hz
!concentration_of_SPM_in_water_001
!concentration_of_SPM_in_water_002
!concentration_of_SPM_surface_flux_hz
!concentration_of_SPM_upward_flux_at_soil_surface_001
!concentration_of_SPM_upward_flux_at_soil_surface_002
!concentration_of_SPM_upward_flux_at_soil_surface_003
!concentration_of_SPM_upward_flux_at_soil_surface_004
!concentration_of_SPM_z_velocity_in_water_001
!concentration_of_SPM_z_velocity_in_water_002
!denitrification_rate_Denitr_in_water
!denitrification_rate_in_soil
!depth_averaged_x_velocity_in_water
!depth_averaged_y_velocity_in_water
!detritus-P_in_soil
!detritus-P_sources-sinks_in_soil
!detritus-P_upward_flux_at_soil_surface
!dissolved_ammonium_nh3_bottom_flux_hz
!dissolved_ammonium_nh3_in_water
!dissolved_ammonium_nh3_sources-sinks_in_water
!dissolved_ammonium_nh3_surface_flux_hz
!dissolved_ammonium_nh3_z_velocity_in_water
!dissolved_ammonium_sources-sinks_in_soil
!dissolved_nitrate_sources-sinks_in_soil
!dissolved_oxygen_in_soil
!dissolved_oxygen_oxy_bottom_flux_hz
!dissolved_oxygen_oxy_in_water
!dissolved_oxygen_oxy_sources-sinks_in_water
!dissolved_oxygen_oxy_surface_flux_hz
!dissolved_oxygen_oxy_z_velocity_in_water
!dissolved_oxygen_sources-sinks_in_soil
!dissolved_oxygen_upward_flux_at_soil_surface
!dissolved_phosphate_sources-sinks_in_soil
!dissolved_reduced_substances_in_soil
!dissolved_reduced_substances_odu_bottom_flux_hz
!dissolved_reduced_substances_odu_in_water
!dissolved_reduced_substances_odu_sources-sinks_in_water
!dissolved_reduced_substances_odu_surface_flux_hz
!dissolved_reduced_substances_odu_z_velocity_in_water
!dissolved_reduced_substances_sources-sinks_in_soil
!dissolved_reduced_substances_upward_flux_at_soil_surface
!dtheta_fac4_in_water
!dtheta_fac5_in_water
!fast_detritus_C_in_soil
!fast_detritus_C_sources-sinks_in_soil
!fast_detritus_C_upward_flux_at_soil_surface
!fraction_of_Rubisco_Rub_bottom_flux_hz
!fraction_of_Rubisco_Rub_in_water
!fraction_of_Rubisco_Rub_sources-sinks_in_water
!fraction_of_Rubisco_Rub_surface_flux_hz
!fraction_of_Rubisco_Rub_z_velocity_in_water
!gross_primary_production_GPPR_in_water
!massflux_in_the_exchange_layer_hz
!massfraction_in_the_exchange_layer_hz
!mole_concentration_of_ammonium_in_soil
!mole_concentration_of_ammonium_upward_flux_at_soil_surface
!mole_concentration_of_nitrate_in_soil
!mole_concentration_of_nitrate_upward_flux_at_soil_surface
!mole_concentration_of_phosphate_in_soil
!mole_concentration_of_phosphate_upward_flux_at_soil_surface
!oxygen_flux_between_sea_water_and_air_hz
!phosphate_adsorption_in_soil
!porosity_in_soil
!slow_detritus_C_in_soil
!slow_detritus_C_sources-sinks_in_soil
!slow_detritus_C_upward_flux_at_soil_surface
!surface_downwelling_photosynthetic_radiative_flux
        case default
          standardName=trim(name)
      end select

      call ESMF_AttributeSet(field, 'standard_name', trim(standardName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   ! endif

    !No implemented in ESMF: changing field name
    !call ESMF_FieldSet(field, name=name, rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc = rc_

    return

  end subroutine MOSSCO_FieldNameCheck

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldInitialize"
  subroutine MOSSCO_FieldInitialize(field, kwe, value, rc)

    type(ESMF_Field), intent(inout)                  :: field
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe ! Keyword enforcer
    real(ESMF_KIND_R8), intent(in), optional         :: value
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    character(len=ESMF_MAXSTR)               :: message
    integer(ESMF_KIND_I4)                    :: rc_, rank, localrc
    integer(ESMF_KIND_I4), allocatable       :: ubnd(:), lbnd(:)
    type(ESMF_TypeKind_Flag)                 :: typeKind
    real(ESMF_KIND_R8)                       :: value_

    real(ESMF_KIND_R8), pointer  :: farrayPtr1(:), farrayPtr2(:,:)
    real(ESMF_KIND_R8), pointer  :: farrayPtr3(:,:,:), farrayPtr4(:,:,:,:)
    !real(ESMF_KIND_R8), pointer  :: farrayPtr5(:,:,:,:,:), farrayPtr6(:,:,:,:,:,:)
    !real(ESMF_KIND_R8), pointer  :: farrayPtr7(:,:,:,:,:,:,:)

    type(ESMF_FieldStatus_Flag) :: fieldStatus

    rc_ = ESMF_SUCCESS
    if (present(kwe)) localrc = ESMF_SUCCESS
    if (present(value)) then
      value_ = value
    else
      value_ = 0.0D0
    endif

    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
      if (present(rc)) rc=ESMF_RC_OBJ_BAD
      return
    endif

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
      write(message,'(A)') 'Cannot initialize incomplete '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(lbnd)) deallocate(lbnd)

    allocate(ubnd(rank), stat=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    allocate(lbnd(rank), stat=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGetbounds(field, localDe=0,  exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, typeKind=typeKind, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @todo: handle different typekinds

    if (rank == 1) then
      call ESMF_FieldGet(field, localDe=0,  farrayPtr=farrayPtr1, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      farrayPtr1(lbnd(1):ubnd(1)) = value_
    elseif (rank == 2) then
      call ESMF_FieldGet(field, localDe=0,  farrayPtr=farrayPtr2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      farrayPtr2(RANGE2D) = value_
    elseif (rank == 3) then
      call ESMF_FieldGet(field, localDe=0,  farrayPtr=farrayPtr3, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      farrayPtr3(RANGE3D) = value_
    elseif (rank == 4) then
      call ESMF_FieldGet(field, localDe=0,  farrayPtr=farrayPtr4, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      farrayPtr4(RANGE3D,lbnd(4):ubnd(4)) = value_
    else
      write(message,'(A)') 'Not yet implemented, initialize rank>7 '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    endif

    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(lbnd)) deallocate(lbnd)

    if (present(rc)) rc = rc_

  end subroutine MOSSCO_FieldInitialize

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldMatchFieldsFromState"
  !> @brief finds for a field the matching fields in a atate
  !> @return fieldList: a list of fields from the state
  !> @param field: field to match to
  !> @param state: state to search for matching fields
  !> @param rc: [optional] return code
  subroutine MOSSCO_FieldMatchFieldsFromState(field, state, fieldList, kwe, rc)

    type(ESMF_Field), intent(in)                     :: field
    type(ESMF_State), intent(in)                     :: state
    type(ESMF_Field),  allocatable                   :: fieldList(:)
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, i, itemCount, fieldCount
    character(len=ESMF_MAXSTR)              :: name
    character(len=ESMF_MAXPATHLEN)          :: message
    type(ESMF_StateItem_Flag)               :: itemType
    type(ESMF_FieldBundle)                  :: fieldBundle

    rc_ = ESMF_SUCCESS
    if (present(kwe)) localrc = ESMF_SUCCESS

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(state, itemSearch=trim(name), itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount == 0) then
      rc_ = ESMF_RC_NOT_FOUND
    elseif (itemCount > 1) then
      rc_ = ESMF_RC_NOT_IMPL
    else
      call ESMF_StateGet(state, itemName=trim(name), itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemType == ESMF_STATEITEM_FIELD) then
        allocate(fieldList(1), stat=localrc)

        call ESMF_StateGet(state, trim(name), fieldList(1), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      elseif (itemType == ESMF_STATEITEM_FIELDBUNDLE) then

        call ESMF_StateGet(state, trim(name), fieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleGet(fieldBundle, fieldName=trim(name), &
          fieldCount=fieldCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (fieldCount==0) then
          rc_ = ESMF_RC_NOT_FOUND
        else
          allocate(fieldList(fieldcount), stat=localrc)
          call ESMF_FieldBundleGet(fieldBundle, fieldName=trim(name), &
            fieldList=fieldList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif
    endif

    if (present(rc)) rc = rc_
    return

  end subroutine MOSSCO_FieldMatchFieldsFromState

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldGetMissingValueR8"
  subroutine MOSSCO_FieldGetMissingValueR8(field, missing_value, kwe, rc)

    type(ESMF_Field), intent(in)                 :: field
    real(ESMF_KIND_R8), intent(out)              :: missing_value
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                        :: localrc, rc_, missingValueI4
    integer(ESMF_KIND_I4)                        :: missingValueI8
    real(ESMF_KIND_R4)                           :: missingValueR4
    logical                                      :: isPresent
    character(len=ESMF_MAXSTR)                   :: message
    type(ESMF_TypeKind_Flag)                     :: typeKind

    !> @todo This method is redundant, please consider removal

    rc_ = ESMF_SUCCESS
    if (present(kwe)) localrc = ESMF_SUCCESS

    call MOSSCO_AttributeGet(field, 'missing_value', missing_value, &
      defaultValue=-1D30, convert=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc))  rc = rc_
    return

  end subroutine MOSSCO_FieldGetMissingValueR8

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldCopyAttributes"
  subroutine MOSSCO_FieldCopyAttributes(dstField, srcField, kwe, rc)

    type(ESMF_Field), intent(inout)              :: dstField
    type(ESMF_Field), intent(in)                 :: srcField
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    real(ESMF_KIND_R4)                           :: real4
    real(ESMF_KIND_R8)                           :: real8
    integer(ESMF_KIND_I8)                        :: int8
    integer(ESMF_KIND_I4)                        :: localrc, rc_, int4
    integer(ESMF_KIND_I4)                        :: attributeCount, i
    logical                                      :: isPresent, bool
    character(len=ESMF_MAXSTR)                   :: message, attributeName, string
    type(ESMF_TypeKind_Flag)                     :: typeKind

    rc_ = ESMF_SUCCESS

    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_AttributeGet(srcField, count=attributeCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i = 1, attributeCount

      call ESMF_AttributeGet(srcField, attributeIndex=i , name=attributeName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeGet(dstField, name=attributeName, isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! do not overwrite attributes that exist in dstField
      if (isPresent) cycle

      call ESMF_AttributeGet(srcField, name=attributeName, typeKind=typeKind, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (typeKind == ESMF_TYPEKIND_I4) then
        call ESMF_AttributeGet(srcField, name=attributeName, value=int4, rc=localrc)
        call ESMF_AttributeSet(dstField, name=attributeName, value=int4, rc=localrc)
      elseif (typeKind == ESMF_TYPEKIND_I8) then
        call ESMF_AttributeGet(srcField, name=attributeName, value=int8, rc=localrc)
        call ESMF_AttributeSet(dstField, name=attributeName, value=int8, rc=localrc)
      elseif (typeKind == ESMF_TYPEKIND_R4) then
        call ESMF_AttributeGet(srcField, name=attributeName, value=real4, rc=localrc)
        call ESMF_AttributeSet(dstField, name=attributeName, value=real4, rc=localrc)
      elseif (typeKind == ESMF_TYPEKIND_R8) then
        call ESMF_AttributeGet(srcField, name=attributeName, value=real8, rc=localrc)
        call ESMF_AttributeSet(dstField, name=attributeName, value=real8, rc=localrc)
      elseif (typeKind == ESMF_TYPEKIND_CHARACTER) then
        call ESMF_AttributeGet(srcField, name=attributeName, value=string, rc=localrc)
        call ESMF_AttributeSet(dstField, name=attributeName, value=string, rc=localrc)
      elseif (typeKind == ESMF_TYPEKIND_LOGICAL) then
        call ESMF_AttributeGet(srcField, name=attributeName, value=bool, rc=localrc)
        call ESMF_AttributeSet(dstField, name=attributeName, value=bool, rc=localrc)
      endif

      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    enddo

  end subroutine MOSSCO_FieldCopyAttributes


#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldAttributesIdentical"
  function MOSSCO_FieldAttributesIdentical(importField, exportField, kwe, &
    exclude, differList, rc) result(differCount)

    type(ESMF_Field), intent(in)                 :: importField, exportField
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    character(len=*), dimension(*), optional     :: exclude(:)
    character(len=*), allocatable, optional, intent(inout)   :: differList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    integer(ESMF_KIND_I4)                        :: differCount

    real(ESMF_KIND_R4)                           :: importReal4, exportReal4
    real(ESMF_KIND_R8)                           :: importReal8, exportReal8
    integer(ESMF_KIND_I8)                        :: importInt8, exportInt8
    integer(ESMF_KIND_I4)                        :: localrc, rc_, importInt4, exportInt4
    integer(ESMF_KIND_I4)                        :: importCount, exportcount, i, j, count
    logical                                      :: isPresent
    character(len=ESMF_MAXSTR)                   :: message, attributeName, fieldName, string
    character(len=ESMF_MAXSTR)                   :: importString, exportString
    type(ESMF_TypeKind_Flag)                     :: importTypeKind, exportTypeKind
    character(len=ESMF_MAXSTR), allocatable      :: excludeList(:)

    rc_ = ESMF_SUCCESS
    differCount = 0

    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    if (present(exclude)) then
      call MOSSCO_Reallocate(excludeList, ubound(exclude,1)-lbound(exclude,1)+1, rc=localrc)
      excludeList(:) = exclude(:)
    else
      call MOSSCO_Reallocate(excludeList, 1, rc=localrc)
      excludeList(1) = 'creator'
    endif

    call ESMF_AttributeGet(importField, count=importCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (importCount == 0) return

    call ESMF_AttributeGet(exportField, count=exportCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (exportCount == 0) return

    if (present(differList)) then
      call MOSSCO_Reallocate(differList, importCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(importField, name=fieldName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    do i = 1, importCount

      call ESMF_AttributeGet(importfield, attributeIndex=i , name=attributeName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      ! If this attribute's name is in the exclude list, then cycle to next attribute
      isPresent = .false.
      do j = lbound(excludeList,1), ubound(excludeList,1)
        if ( trim(excludeList(j)) /= trim(attributeName) ) cycle
        isPresent = .true.
        exit
      enddo
      if (isPresent) cycle

      call ESMF_AttributeGet(exportfield, name=attributeName, isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (.not.isPresent) cycle

      if (MOSSCO_FieldAttributeIsSameValue(importField, exportField, attributeName, &
        rc=localrc)) cycle

#ifdef DEBUG
      call ESMF_FieldGet(importField, name=message)
      write(0,*) '  non-matching ',trim(message),':',trim(attributeName)
#endif

      differCount = differCount + 1

      if (present(differList)) then
        write(message, '(A)') '  '//trim(fieldName)//':'//trim(attributeName)
        call MOSSCO_AttributeGet(importField, label=trim(attributeName), value=importString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_MessageAdd(message,' '//trim(importString))

        call MOSSCO_AttributeGet(exportField, label=trim(attributeName), value=exportString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_MessageAdd(message,' /= '//trim(exportString))

        differList(differCount) = trim(message)
      endif
    enddo

    if (present(differList)) call MOSSCO_Reallocate(differList, differCount, keep=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(excludeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end function MOSSCO_FieldAttributesIdentical

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldAttributeIsSameValue"
  function MOSSCO_FieldAttributeIsSameValue(importField, exportField, attributeName, rc) result(isSame)

    type(ESMF_Field), intent(in)                 :: importField, exportField
    character(len=*), intent(in)                 :: attributeName
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    logical                                      :: isSame

    real(ESMF_KIND_R4)                           :: importReal4, exportReal4
    real(ESMF_KIND_R8)                           :: importReal8, exportReal8
    integer(ESMF_KIND_I8)                        :: importInt8, exportInt8
    integer(ESMF_KIND_I4)                        :: localrc, rc_, importInt4, exportInt4
    logical                                      :: isPresent, importBool, exportBool
    character(len=ESMF_MAXSTR)                   :: message
    character(len=ESMF_MAXSTR)                   :: importString, exportString
    type(ESMF_TypeKind_Flag)                     :: importTypeKind, exportTypeKind

    rc_ = ESMF_SUCCESS
    isSame = .false.
    if (present(rc)) rc = rc_

    !> Define importReal8 and exportReal8, as the comparison later throws an
    !error if undefined.  This should be revised, however to avoid unlucky
    !constellations

    importReal8=-1.23456789D22
    exportReal8=-1.98765432D21

    call ESMF_AttributeGet(importfield, name=attributeName, typeKind=importTypeKind, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(exportfield, name=attributeName, typeKind=exportTypeKind, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (importTypeKind == ESMF_TYPEKIND_I4) then
      call ESMF_AttributeGet(importfield, name=attributeName, value=importInt4, rc=localrc)
      importReal8 = dble(importInt4)
    elseif (importTypeKind == ESMF_TYPEKIND_I8) then
      call ESMF_AttributeGet(importfield, name=attributeName, value=importInt8, rc=localrc)
      importReal8 = dble(importInt8)
    elseif (importTypeKind == ESMF_TYPEKIND_R4) then
      call ESMF_AttributeGet(importfield, name=attributeName, value=importReal4, rc=localrc)
      importReal8 = dble(importReal4)
    elseif (importTypeKind == ESMF_TYPEKIND_R8) then
      call ESMF_AttributeGet(importfield, name=attributeName, value=importReal8, rc=localrc)
    elseif (importTypeKind == ESMF_TYPEKIND_CHARACTER) then
      call ESMF_AttributeGet(importfield, name=attributeName, value=importString, rc=localrc)
    elseif (importTypeKind == ESMF_TYPEKIND_LOGICAL) then
      call ESMF_AttributeGet(importfield, name=attributeName, value=importBool, rc=localrc)
    endif

    if (exportTypeKind == ESMF_TYPEKIND_I4) then
      call ESMF_AttributeGet(exportField, name=attributeName, value=exportInt4, rc=localrc)
      exportReal8 = dble(exportInt4)
    elseif (exportTypeKind == ESMF_TYPEKIND_I8) then
      call ESMF_AttributeGet(exportField, name=attributeName, value=exportInt8, rc=localrc)
      exportReal8 = dble(exportInt8)
    elseif (exportTypeKind == ESMF_TYPEKIND_R4) then
      call ESMF_AttributeGet(exportField, name=attributeName, value=exportReal4, rc=localrc)
      exportReal8 = dble(exportReal4)
    elseif (exportTypeKind == ESMF_TYPEKIND_R8) then
      call ESMF_AttributeGet(exportField, name=attributeName, value=exportReal8, rc=localrc)
    elseif (exportTypeKind == ESMF_TYPEKIND_CHARACTER) then
      call ESMF_AttributeGet(exportField, name=attributeName, value=exportString, rc=localrc)
    elseif (exportTypeKind == ESMF_TYPEKIND_LOGICAL) then
      call ESMF_AttributeGet(exportfield, name=attributeName, value=exportBool, rc=localrc)
    endif

    isSame = .false.

    if (importTypeKind == ESMF_TYPEKIND_CHARACTER .and. exportTypeKind == ESMF_TYPEKIND_CHARACTER) then
      if (trim(exportString) == trim(importString)) then
        isSame = .true.
        return
      endif
    endif

    if (importTypeKind == ESMF_TYPEKIND_LOGICAL .and. exportTypeKind == ESMF_TYPEKIND_LOGICAL) then
      if (importBool .eqv. exportBool) then
        isSame = .true.
        return
      endif
    endif

    if (importTypeKind == ESMF_TYPEKIND_LOGICAL .and.  exportTypeKind /= ESMF_TYPEKIND_LOGICAL) then
      isSame = .false.
      return
    endif

    if (importTypeKind == ESMF_TYPEKIND_CHARACTER .and.  exportTypeKind /= ESMF_TYPEKIND_CHARACTER) then
      isSame = .false.
      return
    endif

    ! Disregard numeric precision here
    if (importReal8 == exportReal8) then
      isSame = .true.
    endif

  end function MOSSCO_FieldAttributeIsSameValue

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldWeightField"
  subroutine MOSSCO_FieldWeightField(exportField, importField, weight, kwe, &
    owner, tagOnly, rc)

    type(ESMF_Field), intent(inout)        :: exportField
    type(ESMF_Field), intent(in)           :: importField
    real(ESMF_KIND_R8), intent(in)         :: weight
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    character(len=*), intent(in), optional :: owner
    logical, intent(in), optional          :: tagOnly
    integer(ESMF_KIND_I4), optional        :: rc

    character(ESMF_MAXSTR)                 :: message
    type(ESMF_FieldStatus_Flag)            :: fieldStatus
    integer(ESMF_KIND_I4)                  :: i, j, fieldCount, rank, exportRank, int4
    integer(ESMF_KIND_I8)                  :: numChanged, advanceCount, int8
    integer(ESMF_KIND_I4), allocatable     :: ubnd(:), lbnd(:)
    integer(ESMF_KIND_I4), allocatable     :: gridUbnd(:), gridLbnd(:)
    integer(ESMF_KIND_I4), allocatable     :: exportUbnd(:), exportLbnd(:)

    real(ESMF_KIND_R8), pointer            :: importPtr3(:,:,:), exportPtr3(:,:,:)
    real(ESMF_KIND_R8), pointer            :: importPtr2(:,:), exportPtr2(:,:)
    logical, allocatable                   :: mask2(:,:), mask3(:,:,:)
    integer(ESMF_KIND_I4), pointer         :: gridMask2(:,:), gridMask3(:,:,:)
    logical                                :: tagOnly_, isPresent
    real(ESMF_KIND_R8)                     :: exportMissingValue, importMissingValue
    real(ESMF_KIND_R8)                     :: weight_, real8
    real(ESMF_KIND_R4)                     :: real4
    type(ESMF_TypeKind_Flag)               :: typeKind

    integer(ESMF_KIND_I4)                  :: localrc, rc_, gridRank
    type(ESMF_Grid)                        :: grid

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    if (present(kwe)) rc = rc_
    if (present(tagOnly)) then
      tagOnly_ = tagOnly
    else
      tagOnly_ = .false.
    endif
    weight_ = weight
    if (weight > 1.0) weight_ = 1.0
    if (weight <= 0.0) return

    call ESMF_FieldGet(exportField, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) return

    call ESMF_FieldGet(exportField, rank=exportRank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(importField, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) return

    call ESMF_FieldGet(importField, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rank /= exportRank) then
      if (present(owner)) then
        write(message,'(A)')  trim(owner)//' rank mismatch in '
      else
        write(message,'(A)')  '  rank mismatch in '
      endif

      call MOSSCO_FieldString(exportField, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      if (present(rc)) rc = ESMF_RC_ARG_INCOMP
      return
    endif

    if (rank > 0) then
        if (allocated(exportUbnd)) deallocate(exportUbnd)
        allocate(exportUbnd(rank), stat=localrc)
        if (allocated(exportLbnd)) deallocate(exportLbnd)
        allocate(exportLbnd(rank), stat=localrc)
        if (allocated(lbnd)) deallocate(lbnd)
        allocate(lbnd(rank), stat=localrc)
        lbnd(:) = 1
        if (allocated(ubnd)) deallocate(ubnd)
        allocate(ubnd(rank), stat=localrc)
        ubnd(:) = 0
    endif

    call ESMF_FieldGetBounds(importField, exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGetBounds(exportField, exclusiveUBound=exportUbnd, exclusiveLBound=exportLbnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (any(exportLbnd - lbnd > 0) .or. any(exportUbnd - ubnd > 0))  then
      if (present(owner)) then
        write(message,'(A)')  trim(owner)//' exclusive bounds mismatch in '
      else
        write(message,'(A)')  '  exclusive bounds mismatch in '
      endif
        call MOSSCO_FieldString(exportField, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        if (present(rc)) rc = ESMF_RC_ARG_INCOMP
        return
    endif

    if (allocated(exportUbnd)) deallocate(exportUbnd)
    if (allocated(exportLbnd)) deallocate(exportLbnd)

    call MOSSCO_AttributeGet(importField, 'missing_value', importMissingValue, &
      defaultValue=-1D30, convert=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_AttributeGet(exportField, 'missing_value', exportMissingValue, &
      defaultValue=-1D30, convert=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @todo The has_boundary_data attribute is set to .true. by default here,
    !> this should be changed by a configuration attribute and the attribute name MOSSCO_AttributeGet
    !> be synchronized with the transporting component (e.g. getm_component)
    call ESMF_AttributeSet(exportField, 'has_boundary_data', .true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not. tagOnly_) then
      numChanged = 0

      if (allocated(mask2)) deallocate(mask2)
      allocate(mask2(RANGE2D), stat=localrc)
      mask2 = .true.

      if (allocated(mask3)) deallocate(mask3)
      if (rank == 3) allocate(mask3(RANGE3D), stat=localrc)
      if (rank == 2) allocate(mask3(RANGE2D,1:1), stat=localrc)
      mask3 = .true.

      call ESMF_FieldGet(importField, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isPresent) then
        call ESMF_GridGet(grid, rank=gridRank, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (allocated(gridUbnd)) deallocate(gridUbnd)
        allocate(gridUbnd(gridRank))

        if (allocated(gridLbnd)) deallocate(gridLbnd)
        allocate(gridLbnd(gridRank))

        call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, localDe=0, &
          exclusiveUBound=gridUbnd, exclusiveLBound=gridLbnd, rc=localrc)

        if (gridRank == 2) then
          call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=gridMask2, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          mask2 = (mask2 .and. (gridMask2(gridLbnd(1):gridUbnd(1),gridLbnd(2):gridUbnd(2)) > 0))

        elseif (gridRank == 3) then
          call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=gridMask3, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          mask3 = (mask3 .and. &
            (gridMask3(gridLbnd(1):gridUbnd(1),gridLbnd(2):gridUbnd(2),gridLbnd(3):gridUbnd(3))  > 0))
        endif
      endif

      call ESMF_FieldGet(exportField, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isPresent) then
        call ESMF_GridGet(grid, rank=gridRank, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (gridRank == 2) then
          call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=gridMask2, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          mask2 = (mask2 .and. (gridMask2(gridLbnd(1):gridUbnd(1),gridLbnd(2):gridUbnd(2)) > 0))

        elseif (gridRank == 3) then
          call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=gridMask3, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          mask3 = (mask3 .and. &
            (gridMask3(gridLbnd(1):gridUbnd(1),gridLbnd(2):gridUbnd(2),gridLbnd(3):gridUbnd(3))  > 0))
        endif
      endif

      select case (rank)
        case(2)
          call ESMF_FieldGet(importField, farrayPtr=importPtr2, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldGet(exportField, farrayPtr=exportPtr2, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          ! Mask all values that are missing value
          mask2 = (mask2 .and. exportPtr2(RANGE2D) .ne. exportMissingValue)
          mask2 = (mask2 .and. importPtr2(RANGE2D) .ne. importMissingValue)

          ! Mask all values that are NaN
          mask2 = (mask2 .and. exportPtr2(RANGE2D) .eq. exportPtr2(RANGE2D))
          mask2 = (mask2 .and. importPtr2(RANGE2D) .eq. importPtr2(RANGE2D))

          !> @todo add infinity to mask

          call ESMF_AttributeGet(importField, 'valid_min', isPresent=isPresent, rc=localrc)
          if (isPresent) then
            call MOSSCO_AttributeGet(importField, label='valid_min', value=real8, &
              convert=.true., rc=localrc)
            mask2 = (mask2 .and. importPtr2(RANGE2D) >= real8)
          endif

          call ESMF_AttributeGet(exportField, 'valid_min', isPresent=isPresent, rc=localrc)
          if (isPresent) then
            call MOSSCO_AttributeGet(importField, label='valid_min', value=real8, &
              convert=.true., rc=localrc)
            mask2 = (mask2 .and. importPtr2(RANGE2D) >= real8)
          endif

          call ESMF_AttributeGet(importField, 'valid_max', isPresent=isPresent, rc=localrc)
          if (isPresent) then
            call MOSSCO_AttributeGet(importField, label='valid_max', value=real8, &
              convert=.true., rc=localrc)
            mask2 = (mask2 .and. importPtr2(RANGE2D) <= real8)
          endif

          call ESMF_AttributeGet(exportField, 'valid_max', isPresent=isPresent, rc=localrc)
          if (isPresent) then
            call MOSSCO_AttributeGet(importField, label='valid_min', value=real8, &
              convert=.true., rc=localrc)
            mask2 = (mask2 .and. importPtr2(RANGE2D) <= real8)
          endif

          numChanged = count(mask2)
          if (numChanged>0) then
            where (mask2)
              exportPtr2 = (1.0 - weight_) * exportPtr2 + weight_ * importPtr2
            endwhere
          endif
          if (allocated(mask2)) deallocate(mask2)

        case(3)
          call ESMF_FieldGet(importField, farrayPtr=importPtr3, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldGet(exportField, farrayPtr=exportPtr3, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !write(message,'(A,I5.5,A)') ' base mask changing ', count(mask3), ' cells '
          !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          ! Mask all values that are missing value
          mask3 = (mask3 .and. exportPtr3(RANGE3D) .ne. exportMissingValue)
          mask3 = (mask3 .and. importPtr3(RANGE3D) .ne. importMissingValue)

          !write(message,'(A,I5.5,A)') ' missing mask changing ', count(mask3), ' cells '
          !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          ! Mask all values that are NaN
          mask3 = (mask3 .and. exportPtr3(RANGE3D) .eq. exportPtr3(RANGE3D))
          mask3 = (mask3 .and. importPtr3(RANGE3D) .eq. importPtr3(RANGE3D))

          !write(message,'(A,I5.5,A)') ' NaN mask changing ', count(mask3), ' cells '
          !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          !> @todo add infinity to mask

          call ESMF_AttributeGet(importField, 'valid_min', isPresent=isPresent, rc=localrc)
          if (isPresent) then
            call MOSSCO_AttributeGet(importField, label='valid_min', value=real8, &
              convert=.true., rc=localrc)
            mask3 = (mask3 .and. importPtr3(RANGE3D) >= real8)

            !write(message,'(A,I5.5,A)') ' valid_min mask changing ', count(mask3), ' cells '
            !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          endif

          call ESMF_AttributeGet(exportField, 'valid_min', isPresent=isPresent, rc=localrc)
          if (isPresent) then
            call MOSSCO_AttributeGet(importField, label='valid_min', value=real8, &
              convert=.true., rc=localrc)
            mask3 = (mask3 .and. importPtr3(RANGE3D) >= real8)

            !write(message,'(A,I5.5,A)') ' valid_min mask changing ', count(mask3), ' cells '
            !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          endif

          call ESMF_AttributeGet(importField, 'valid_max', isPresent=isPresent, rc=localrc)
          if (isPresent) then
            call MOSSCO_AttributeGet(importField, label='valid_max', value=real8, &
              convert=.true., rc=localrc)
            mask3 = (mask3 .and. importPtr3(RANGE3D) <= real8)

            !write(message,'(A,I5.5,A)') ' valid_max mask changing ', count(mask3), ' cells '
            !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          endif

          call ESMF_AttributeGet(exportField, 'valid_max', isPresent=isPresent, rc=localrc)
          if (isPresent) then
            call MOSSCO_AttributeGet(importField, label='valid_min', value=real8, &
              convert=.true., rc=localrc)
            mask3 = (mask3 .and. importPtr3(RANGE3D) <= real8)

            !write(message,'(A,I5.5,A)') ' valid_max mask changing ', count(mask3), ' cells '
            !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          endif

          numChanged = count(mask3)
          if (numChanged>0) then
            where (mask3)
              exportPtr3(RANGE3D) = (1.0 - weight_) &
                * exportPtr3(RANGE3D) &
                + weight_ * importPtr3(RANGE3D)
            endwhere
          endif
          if (allocated(mask3)) deallocate(mask3)
        case default
          if (allocated(lbnd)) deallocate(lbnd)
          if (allocated(ubnd)) deallocate(ubnd)
          if (present(rc)) rc=ESMF_RC_NOT_IMPL
          return
      endselect

      !if (numChanged>0) then
        if (present(owner)) then
          write(message,'(A)') trim(owner)//' weight'
        else
          write(message,'(A)') '  weight'
        endif
        write(message,'(A,ES9.2,A,I5.5,A)') trim(message), weight_, ' changed ', numChanged, ' cells '
        call MOSSCO_FieldString(exportField, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      !endif
    endif

    call ESMF_AttributeSet(exportField, 'nudging_weight', weight_, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importField, 'creator', message, defaultValue='unknown', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(exportField, 'nudging_component', trim(message), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(lbnd)) deallocate(lbnd)
    if (allocated(ubnd)) deallocate(ubnd)

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_FieldWeightField

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldLog"
  subroutine MOSSCO_FieldLog(field, kwe, log, prefix, rc)

    type(ESMF_Field)                :: field
    type(ESMF_KeywordEnforcer), intent(in ), optional :: kwe !keyword-enforcer
    type(ESMF_Log), optional        :: log
    character(len=*), optional, intent(in) :: prefix
    integer(ESMF_KIND_I4), optional :: rc

    integer(ESMF_KIND_I4)           :: localRc, i, j, maxDigits, count, itemCount
    character(len=ESMF_MAXPATHLEN)  :: string, message
    character(len=ESMF_MAXSTR)      :: name, attributeName
    integer(ESMF_KIND_I4)           :: totalLWidth(7), totalUWidth(7)
    type(ESMF_TypeKind_Flag)        :: typeKind
    logical, allocatable            :: logicalValueList(:)
    real(kind=ESMF_KIND_R4), allocatable    :: real4ValueList(:)
    real(kind=ESMF_KIND_R8), allocatable    :: real8ValueList(:)
    integer(kind=ESMF_KIND_I4), allocatable :: integer4ValueList(:)
    integer(kind=ESMF_KIND_I8), allocatable :: integer8ValueList(:)
    character(len=4096)       , allocatable :: characterValueList(:)
    type(ESMF_Log)                  :: log_
    character(len=ESMF_MAXSTR)      :: prefix_

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(kwe)) localrc = ESMF_SUCCESS
    !> @todo: find out how to get a reference to the default log
    ! log = ESMF_GetLog()
    if (present(log)) log_ = log
    prefix_ = ''
    if (present(prefix)) prefix_ = trim(prefix)

    call ESMF_FieldGet(field, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message, '(A)') 'field '
    call MOSSCO_FieldString(field, message, prefix=prefix_, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(log)) then
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log_)
    else
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_AttributeGet(field, count=count, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, count
      call ESMF_AttributeGet(field, attributeIndex=i , name=attributeName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (len_trim(prefix_) > 0) then
        write(message,'(A)') trim(prefix_)//trim(name)//':'
      else
        write(message,'(A)')  trim(name)//':'
      endif
      call MOSSCO_MessageAdd(message,trim(attributeName)//' =')

      call ESMF_AttributeGet(field, name=attributeName, typekind=typekind,  itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (typekind==ESMF_TYPEKIND_LOGICAL) then
        call MOSSCO_MessageAdd(message,' (L)')
        allocate(logicalValueList(itemCount))
        call ESMF_AttributeGet(field, name=attributeName, valueList=logicalValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,L)') trim(message)//' ',logicalValueList(1)
        do j=2, itemCount-1
          write(string,'(A, L)') ', ',logicalValueList(j)
          call MOSSCO_MessageAdd(message,trim(string))
        enddo
        deallocate(logicalValueList)
      elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
        if (allocated(characterValueList)) deallocate(characterValueList)
        allocate(characterValueList(itemCount))
        call ESMF_AttributeGet(field, name=attributeName, valueList=characterValueList, rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          !(ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
          !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          !>@ todo: how to deal with very long attributes that don't fit into valueList?
          rc=ESMF_SUCCESS
        endif

        if (len_trim(message) + len_trim(characterValueList(1)) + 1 <= len(message)) then
          write(message,'(A)') trim(message)//' "'//trim(characterValueList(1))//'"'
          do j=2, itemCount-1
            write(string,'(A)') ', "'//trim(characterValueList(j))//'"'
            call MOSSCO_MessageAdd(message,trim(string))
          enddo
        endif
        if (allocated(characterValueList)) deallocate(characterValueList)
      elseif (typekind==ESMF_TYPEKIND_I4) then
        call MOSSCO_MessageAdd(message,' (I4)')
        allocate(integer4ValueList(itemCount))
        call ESMF_AttributeGet(field, name=attributeName, valueList=integer4ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,I3.3)') trim(message)//' ',integer4ValueList(1)
        do j=2, itemCount-1
          write(string,'(A,I3.3)') ', ',integer4ValueList(j)
          call MOSSCO_MessageAdd(message,trim(string))
        enddo
        deallocate(integer4ValueList)
      elseif (typekind==ESMF_TYPEKIND_I8) then
        call MOSSCO_MessageAdd(message,' (I8)')
        allocate(integer8ValueList(itemCount))
        call ESMF_AttributeGet(field, name=attributeName, valueList=integer8ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,I3.3)') trim(message)//' ',integer8ValueList(1)
        do j=2, itemCount-1
          write(string,'(A,I3.3)') ', ',integer8ValueList(j)
          call MOSSCO_MessageAdd(message,trim(string))
        enddo
        deallocate(integer8ValueList)
      elseif (typekind==ESMF_TYPEKIND_R4) then
        call MOSSCO_MessageAdd(message,' (R4)')
        allocate(real4ValueList(itemCount))
        call ESMF_AttributeGet(field, name=attributeName, valueList=real4ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,ES9.2)') trim(message)//' ',real4ValueList(1)
        do j=2, itemCount-1
          write(string,'(A,ES9.2)') ', ',real4ValueList(j)
          call MOSSCO_MessageAdd(message,trim(string))
        enddo
        deallocate(real4ValueList)
      elseif (typekind==ESMF_TYPEKIND_R8) then
        call MOSSCO_MessageAdd(message,' (R8)')
        allocate(real8ValueList(itemCount))
        call ESMF_AttributeGet(field, name=attributeName, valueList=real8ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,ES9.2)') trim(message)//' ',real8ValueList(1)
        do j=2, itemCount-1
          write(string,'(A,ES9.2)') ', ',real8ValueList(j)
          call MOSSCO_MessageAdd(message,trim(string))
        enddo
        deallocate(real8ValueList)
      endif
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    enddo

  end subroutine MOSSCO_FieldLog

! #undef  ESMF_METHOD
! #define ESMF_METHOD "MOSSCO_FieldIsFinite"
!   function MOSSCO_FieldIsFinite(field, kwe, checkNaN, checkInf, rc) result(isFinite)
!
!     type(ESMF_Field)                :: field
!     type(ESMF_KeywordEnforcer),intent(in ),optional    :: kwe
!     logical,intent(in ),optional    :: checkNaN, checkInf
!     integer(ESMF_KIND_I4), optional :: rc
!
!     integer(ESMF_KIND_I4)           :: localRc, rc_
!     character(len=ESMF_MAXPATHLEN)  :: message
!     character(len=ESMF_MAXSTR)      :: name
!     type(ESMF_TypeKind_Flag)        :: typeKind
!     logical                         :: checkInf_, checkNaN_
!     type(ESMF_FieldStatus_Flag)     :: fieldStatus
!
!     if (present(rc)) rc = ESMF_SUCCESS
!     if (present(kwe)) rc_ = ESMF_SUCCESS
!     checkNaN = .true.
!     if (present(checkNaN)) checkNaN_ = checkNaN
!     checkInf = .true.
!     if (present(checkInf)) checkInf_ = checkInf
!
!     call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
!     if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) return
!
!     call ESMF_FieldGet(field, rank=rank, typeKind=typeKind, rc=localrc)
!     if (rank > 4) then
!       if (present(rc)) rc = ESMF_RC_NOT_IMPL
!       return
!     endif
!
!     call ESMF_FieldGet(field, grid=grid, rc=localrc)
!     call ESMF_GridGetItem(grid, GRIDITEM_MASK, rc=localrc)
!
!     select case(typeKind)
!     case(ESMF_TYPEKIND_R8)
!       select case(rank))
!       case(1)
!         call ESMF_FieldGet(field, farrayPtr=farrayPtr1, rc=localrc)
!
!         if (any(ncarray3(RANGE3D) /= &
!                 ncarray3(RANGE3D))) then
!           call self%close()
!           write(message,'(A)')  '  NaN detected in field '
!           call MOSSCO_FieldString(field, message, rc=localrc)
!           call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
!           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!         endif

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldMatchFields"
  subroutine MOSSCO_FieldMatchFields(field, fieldList, index, kwe, score, owner, rc)

    type(ESMF_Field), intent(in)                  :: field
    type(ESMF_Field), intent(in), allocatable     :: fieldList(:)
    integer(ESMF_KIND_I4), intent(out)            :: index
    type(ESMF_KeywordEnforcer), optional          :: kwe
    integer(ESMF_KIND_I4), intent(out), optional  :: score
    character(len=*), optional, intent(in)        :: owner
    integer(ESMF_KIND_I4), intent(out), optional  :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc, fieldCount, index_, i
    integer(ESMF_KIND_I4), allocatable  :: matchScore(:)
    character(len=ESMF_MAXSTR)          :: name, message, fieldName


    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_SUCCESS
    index = -1

    if (.not.allocated(fieldList)) return

    fieldCount = ubound(fieldList,1)
    if (fieldCount < 1 ) return

    allocate(matchScore(fieldCount))
    matchScore(:) = 9999

    call ESMF_FieldGet(field, name=fieldName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i = 1, fieldCount
      matchScore(i) = MOSSCO_FieldAttributesIdentical(field, fieldList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    if (count(matchScore == minval(matchScore,1)) /= 1) then
      if (present(owner)) then
        write(message,'(A)') trim(owner)//' ambiguous matching for '
      else
        write(message,'(A)') '  ambiguous matching for'
      endif
      call MOSSCO_MessageAdd(message,' '//trim(fieldName))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      index = 0
    else
      index=minloc(matchScore,1)
    end if

    if (present(score)) score=minval(matchScore,1)

  end subroutine MOSSCO_FieldMatchFields


end module mossco_field
