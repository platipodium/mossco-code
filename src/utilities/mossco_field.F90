!> @brief Implementation of additional ESMF Field utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2015, 2016 Helmholtz-Zentrum Geesthacht
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

module mossco_field

  use mossco_strings
  use esmf

  implicit none

  interface MOSSCO_Reallocate
    module procedure MOSSCO_FieldListReallocate
    module procedure MOSSCO_ItemTypeListReallocate
    module procedure MOSSCO_StringListReallocate
    module procedure MOSSCO_StringList2Reallocate
  end interface MOSSCO_Reallocate

  interface MOSSCO_FieldGetMissingValue
    module procedure MOSSCO_FieldGetMissingValueR8
  end interface MOSSCO_FieldGetMissingValue

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldString"
subroutine MOSSCO_FieldString(field, message, kwe, length, prefix, rc)

  type(ESMF_Field), intent(in)                   :: field
  character(len=*), intent(inout)                :: message
  integer(ESMF_KIND_I4), intent(inout), optional :: length
  type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
  character(len=*), intent(in), optional         :: prefix
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

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
  !> The following line produces a segmentation fault
  !if (present(prefix)) prefix_ = trim(prefix)
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
      call MOSSCO_MessageAdd(message,' '//geomName)

      if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
        call ESMF_FieldGet(field, rank=rank, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        rank=gridRank ! fall back to gridRank, if field not completed
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

  type(ESMF_Field), intent(out)                  :: to
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
  elseif (toRank == 2) then
    call ESMF_FieldGet(from, localDe=0,  farrayPtr=fromFarrayPtr2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldGet(from, localDe=0,  farrayPtr=toFarrayPtr2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    toFarrayPtr2(toLbnd(1):toUbnd(1),toLbnd(2):toUbnd(2)) &
      = fromFarrayPtr2(fromLbnd(1):fromUbnd(1), fromLbnd(2):fromUbnd(2))
  elseif (toRank == 3) then
    call ESMF_FieldGet(from, localDe=0,  farrayPtr=fromFarrayPtr3, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldGet(from, localDe=0,  farrayPtr=toFarrayPtr3, rc=localrc)
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

  deallocate(fromUbnd, toUbnd, fromLbnd, toLbnd)

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
  subroutine MOSSCO_FieldInitialize(field, kwe, rc)

    type(ESMF_Field), intent(inout)                  :: field
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe ! Keyword enforcer
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    character(len=ESMF_MAXSTR)               :: message
    integer(ESMF_KIND_I4)                    :: rc_, rank, localrc
    integer(ESMF_KIND_I4), allocatable       :: ubnd(:), lbnd(:)
    type(ESMF_TypeKind_Flag)                 :: typeKind

    real(ESMF_KIND_R8), pointer  :: farrayPtr1(:), farrayPtr2(:,:)
    real(ESMF_KIND_R8), pointer  :: farrayPtr3(:,:,:), farrayPtr4(:,:,:,:)
    !real(ESMF_KIND_R8), pointer  :: farrayPtr5(:,:,:,:,:), farrayPtr6(:,:,:,:,:,:)
    !real(ESMF_KIND_R8), pointer  :: farrayPtr7(:,:,:,:,:,:,:)

    type(ESMF_FieldStatus_Flag) :: fieldStatus

    rc_ = ESMF_SUCCESS
    if (present(kwe)) localrc = ESMF_SUCCESS

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
      farrayPtr1(lbnd(1):ubnd(1)) = 0.0
    elseif (rank == 2) then
      call ESMF_FieldGet(field, localDe=0,  farrayPtr=farrayPtr2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      farrayPtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) = 0.0
    elseif (rank == 3) then
      call ESMF_FieldGet(field, localDe=0,  farrayPtr=farrayPtr3, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) = 0.0
    elseif (rank == 4) then
      call ESMF_FieldGet(field, localDe=0,  farrayPtr=farrayPtr4, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      farrayPtr4(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3),lbnd(4):ubnd(4)) = 0.0
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

    if (allocated(fieldList)) deallocate(fieldList)

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
#define ESMF_METHOD "MOSSCO_FieldListReallocate"
  subroutine MOSSCO_FieldListReallocate(fieldList, fieldCount, kwe, keep, rc)

    type(ESMF_Field), intent(inout), allocatable     :: fieldList(:)
    integer(ESMF_KIND_I4), intent(in)                :: fieldCount
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional                    :: keep
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, listSize
    logical                                 :: keep_
    character(len=ESMF_MAXPATHLEN)          :: message
    type(ESMF_Field),  allocatable          :: tempList(:)

    rc_ = ESMF_SUCCESS
    keep_ = .true.
    if (present(keep)) keep_ = keep
    if (present(rc)) rc = rc_
    if (present(kwe)) localrc = ESMF_SUCCESS

    ! Save deallocation with fieldCount < 1
    if (fieldCount < 1) then
      if (allocated(fieldList)) deallocate(fieldList)
      return
    endif

    ! Deallocate if not keep
    if (.not.keep_) then
      if (allocated(fieldList)) deallocate(fieldList)
    endif

    if (allocated(fieldList)) then
      ! Don't do anything if requested size is equal
      listSize = size(fieldList)
      if (listSize == fieldCount) return
      if (allocated(tempList)) deallocate(tempList)
      allocate(tempList(listSize), stat=localrc)
      tempList(:) = fieldList(:)
      deallocate(fieldList)
      allocate(fieldList(fieldCount), stat=localrc)
      if (fieldCount < listSize) then
        fieldList(1:fieldCount) = tempList(1:fieldCount)
      else
        fieldList(1:listSize) = tempList(1:listSize)
      endif
    else
      allocate(fieldList(fieldCount), stat=localrc)
    endif

    if (allocated(tempList)) deallocate(tempList)

    if (localrc /= 0) then
      write(message,'(A,I5)') '  failed to allocate memory for fieldList size ',fieldCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_FieldListReallocate

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ItemTypeListReallocate"
  subroutine MOSSCO_ItemTypeListReallocate(itemTypeList, itemCount, kwe, keep, rc)

    type(ESMF_StateItem_Flag), intent(inout), allocatable :: itemTypeList(:)
    integer(ESMF_KIND_I4), intent(in)                :: itemCount
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional                    :: keep
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, listSize
    logical                                 :: keep_
    character(len=ESMF_MAXPATHLEN)          :: message
    type(ESMF_StateItem_Flag),  allocatable :: tempList(:)

    rc_ = ESMF_SUCCESS
    keep_ = .true.
    if (present(keep)) keep_ = keep
    if (present(rc)) rc = rc_
    if (present(kwe)) localrc = ESMF_SUCCESS

    ! Save deallocation with fieldCount < 1
    if (itemCount < 1) then
      if (allocated(itemTypeList)) deallocate(itemTypeList)
      return
    endif

    ! Deallocate if not keep
    if (.not.keep_) then
      if (allocated(itemTypeList)) deallocate(itemTypeList)
    endif

    if (allocated(itemTypeList)) then
      ! Don't do anything if requested size is equal
      listSize = size(itemTypeList)
      if (listSize == itemCount) return
      if (allocated(tempList)) deallocate(tempList)
      allocate(tempList(listSize), stat=localrc)
      tempList(:) = itemTypeList(:)
      deallocate(itemTypeList)
      allocate(itemTypeList(itemCount), stat=localrc)
      if (itemCount < listSize) then
        itemTypeList(1:itemCount) = tempList(1:itemCount)
      else
        itemTypeList(1:listSize) = tempList(1:listSize)
      endif
    else
      allocate(itemTypeList(itemCount), stat=localrc)
    endif

    if (allocated(tempList)) deallocate(tempList)

    if (localrc /= 0) then
      write(message,'(A,I5)') '  failed to allocate memory for itemTypeList size ',itemCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_ItemTypeListReallocate

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StringListReallocate"
  subroutine MOSSCO_StringListReallocate(stringList, itemCount, kwe, keep, rc)

    character(len=*), intent(inout), allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(in)            :: itemCount
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional                :: keep
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, listSize
    logical                                 :: keep_
    character(len=ESMF_MAXPATHLEN)          :: message
    character(len=ESMF_MAXPATHLEN),  allocatable :: tempList(:)

    rc_ = ESMF_SUCCESS
    keep_ = .true.
    if (present(keep)) keep_ = keep
    if (present(rc)) rc = rc_
    if (present(kwe)) localrc = ESMF_SUCCESS

    ! Safe deallocation with itemCount < 1
    if (itemCount < 1) then
      if (allocated(stringList)) deallocate(stringList)
      return
    endif

    ! Deallocate if not keep
    if (.not.keep_) then
      if (allocated(stringList)) deallocate(stringList)
    endif

    if (allocated(stringList)) then
      ! Don't do anything if requested size is equal
      listSize = size(stringList)
      if (listSize == itemCount) return
      if (allocated(tempList)) deallocate(tempList)
      allocate(tempList(listSize), stat=localrc)
      tempList(:) = stringList(:)
      deallocate(stringList)
      allocate(stringList(itemCount), stat=localrc)
      if (itemCount < listSize) then
        stringList(1:itemCount) = tempList(1:itemCount)
      else
        stringList(1:listSize) = tempList(1:listSize)
      endif
    else
      allocate(stringList(itemCount), stat=localrc)
    endif

    if (allocated(tempList)) deallocate(tempList)

    if (localrc /= 0) then
      write(message,'(A,I5)') '  failed to allocate memory for stringList size ',itemCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_StringListReallocate

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StringList2Reallocate"
  subroutine MOSSCO_StringList2Reallocate(stringList, itemCount, kwe, keep, rc)

    character(len=*), intent(inout), allocatable :: stringList(:,:)
    integer(ESMF_KIND_I4), intent(in)            :: itemCount
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional                :: keep
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, listSize
    logical                                 :: keep_
    character(len=ESMF_MAXPATHLEN)          :: message
    character(len=ESMF_MAXPATHLEN),  allocatable :: tempList(:,:)

    rc_ = ESMF_SUCCESS
    keep_ = .true.
    if (present(keep)) keep_ = keep
    if (present(rc)) rc = rc_
    if (present(kwe)) localrc = ESMF_SUCCESS

    ! Safe deallocation with itemCount < 1
    if (itemCount < 1) then
      if (allocated(stringList)) deallocate(stringList)
      return
    endif

    ! Deallocate if not keep
    if (.not.keep_) then
      if (allocated(stringList)) deallocate(stringList)
    endif

    if (allocated(stringList)) then
      ! Don't do anything if requested size is equal
      listSize = size(stringList)
      if (listSize == itemCount) return
      if (allocated(tempList)) deallocate(tempList)
      allocate(tempList(listSize, listSize), stat=localrc)
      tempList(:,:) = stringList(:,:)
      deallocate(stringList)
      allocate(stringList(itemCount,itemCount), stat=localrc)
      if (itemCount < listSize) then
        stringList(1:itemCount,1:itemCount) = tempList(1:itemCount,1:itemCount)
      else
        stringList(1:listSize,1:listSize) = tempList(1:listSize,1:listSize)
      endif
    else
      allocate(stringList(itemCount,itemCount), stat=localrc)
    endif

    if (allocated(tempList)) deallocate(tempList)

    if (localrc /= 0) then
      write(message,'(A,I5)') '  failed to allocate memory for stringList size ',itemCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_StringList2Reallocate

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

    rc_ = ESMF_SUCCESS
    if (present(kwe)) localrc = ESMF_SUCCESS

    call ESMF_AttributeGet(field, 'missing_value', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(field, 'missing_value', typeKind=typeKind, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (TypeKind == ESMF_TYPEKIND_R8) then
        call ESMF_AttributeGet(field, 'missing_value', missing_value, rc=localrc)
      elseif (TypeKind == ESMF_TYPEKIND_R4) then
        call ESMF_AttributeGet(field, 'missing_value', missingValueR4, rc=localrc)
        missing_value = dble(missingValueR4)
      elseif (TypeKind == ESMF_TYPEKIND_I8) then
        call ESMF_AttributeGet(field, 'missing_value', missingValueI8, rc=localrc)
        missing_value = dble(missingValueI8)
      elseif (TypeKind == ESMF_TYPEKIND_I4) then
        call ESMF_AttributeGet(field, 'missing_value', missingValueI4, rc=localrc)
        missing_value = dble(missingValueI4)
      else
        write(message,'(A)')  '  missing value non-implemented type '
        call MOSSCO_FieldString(field, message, rc=localrc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      missing_value = -1.0D30
    endif

    if (present(rc))  rc = rc_
    return

  end subroutine MOSSCO_FieldGetMissingValueR8

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldAttributesIdentical"
  function MOSSCO_FieldAttributesIdentical(importField, exportField, kwe, &
    exclude, rc) result(differCount)

    type(ESMF_Field), intent(in)                 :: importField, exportField
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    character(len=*), dimension(*), optional     :: exclude(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    integer(ESMF_KIND_I4)                        :: differCount

    real(ESMF_KIND_R4)                           :: importReal4, exportReal4
    real(ESMF_KIND_R8)                           :: importReal8, exportReal8
    integer(ESMF_KIND_I8)                        :: importInt8, exportInt8
    integer(ESMF_KIND_I4)                        :: localrc, rc_, importInt4, exportInt4
    integer(ESMF_KIND_I4)                        :: importCount, exportcount, i, j, count
    logical                                      :: isPresent
    character(len=ESMF_MAXSTR)                   :: message, attributeName
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
    if (importCount == 0) return

    call ESMF_AttributeGet(exportField, count=exportCount, rc=localrc)
    if (exportCount == 0) return

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

      if (MOSSCO_FieldAttributeIsSameValue(importField, exportField, attributeName, rc=localrc)) cycle

#ifdef DEBUG
      call ESMF_FieldGet(importField, name=message)
      write(0,*) 'non-matching attribute ',trim(attributeName),' in field ',trim(message)
#endif

      differCount = differCount + 1

    enddo

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
    logical                                      :: isPresent
    character(len=ESMF_MAXSTR)                   :: message
    character(len=ESMF_MAXSTR)                   :: importString, exportString
    type(ESMF_TypeKind_Flag)                     :: importTypeKind, exportTypeKind

    rc_ = ESMF_SUCCESS
    isSame = .false.
    if (present(rc)) rc = rc_

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
      !read(importString,*) importReal8
    else
      return ! not implemented: logical
    endif

    if (exportTypeKind == ESMF_TYPEKIND_I4) then
      call ESMF_AttributeGet(exportField, name=attributeName, value=exportInt4, rc=localrc)
      exportReal8 = dble(exportInt4)
    elseif (importTypeKind == ESMF_TYPEKIND_I8) then
      call ESMF_AttributeGet(exportField, name=attributeName, value=exportInt8, rc=localrc)
      exportReal8 = dble(exportInt8)
    elseif (importTypeKind == ESMF_TYPEKIND_R4) then
      call ESMF_AttributeGet(exportField, name=attributeName, value=exportReal4, rc=localrc)
      exportReal8 = dble(exportReal4)
    elseif (importTypeKind == ESMF_TYPEKIND_R8) then
      call ESMF_AttributeGet(exportField, name=attributeName, value=exportReal8, rc=localrc)
    elseif (importTypeKind == ESMF_TYPEKIND_CHARACTER) then
      call ESMF_AttributeGet(exportField, name=attributeName, value=exportString, rc=localrc)
      !read(exportString,*) exportReal8
    else
      return ! not implemented: logical
    endif

    if (importTypeKind == ESMF_TYPEKIND_CHARACTER .and. exportTypeKind == ESMF_TYPEKIND_CHARACTER) then
        if (trim(exportString) /= trim(importString)) then
!write(0,*) 'difference in character attribute '//trim(attributeName)//': ',trim(exportString)//' vs. '//trim(importString)
          isSame = .false.
        else
          isSame = .true.
        endif
      return
    endif

    if (importReal8 == exportReal8) then
      isSame = .true.
!    else
!      write(0,*) 'difference in numeric attribute '//trim(attributeName)//': ',exportReal8,' vs. ',importReal8
    endif

  end function MOSSCO_FieldAttributeIsSameValue

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldWeightField"
  subroutine MOSSCO_FieldWeightField(exportField, importField, weight, kwe, tagOnly, rc)

    type(ESMF_Field), intent(inout)        :: exportField
    type(ESMF_Field), intent(in)           :: importField
    real(ESMF_KIND_R8), intent(in)         :: weight
    type(ESMF_KeywordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional          :: tagOnly
    integer(ESMF_KIND_I4), optional        :: rc

    character(ESMF_MAXSTR)                 :: message
    type(ESMF_FieldStatus_Flag)            :: fieldStatus
    integer(ESMF_KIND_I4)                  :: i, j, fieldCount, rank, exportRank, int4
    integer(ESMF_KIND_I8)                  :: numChanged, advanceCount, int8
    integer(ESMF_KIND_I4), allocatable     :: ubnd(:), lbnd(:)
    integer(ESMF_KIND_I4), allocatable     :: exportUbnd(:), exportLbnd(:)

    real(ESMF_KIND_R8), pointer            :: importPtr3(:,:,:), exportPtr3(:,:,:)
    real(ESMF_KIND_R8), pointer            :: importPtr2(:,:), exportPtr2(:,:)
    logical, allocatable                   :: mask2(:,:), mask3(:,:,:)
    logical                                :: tagOnly_, isPresent
    real(ESMF_KIND_R8)                     :: exportMissingValue, importMissingValue
    real(ESMF_KIND_R8)                     :: weight_, real8
    real(ESMF_KIND_R4)                     :: real4
    type(ESMF_TypeKind_Flag)               :: typeKind

    integer(ESMF_KIND_I4)                  :: localrc, rc_

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
      write(message,'(A)')  '  rank mismatch in '
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
        write(message,'(A)')  '  exclusive bounds mismatch in '
        call MOSSCO_FieldString(exportField, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        if (present(rc)) rc = ESMF_RC_ARG_INCOMP
        return
    endif

    if (allocated(exportUbnd)) deallocate(exportUbnd)
    if (allocated(exportLbnd)) deallocate(exportLbnd)

    call ESMF_AttributeGet(importField, 'missing_value', &
      isPresent=isPresent,  rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (isPresent) then
      call ESMF_AttributeGet(importField, 'missing_value', typeKind=typeKind, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (typeKind == ESMF_TYPEKIND_R8) then
        call ESMF_AttributeGet(importField, 'missing_value', real8, rc=localrc)
        importMissingValue = real8
      elseif (typeKind == ESMF_TYPEKIND_R4) then
        call ESMF_AttributeGet(importField, 'missing_value', real4, rc=localrc)
        importMissingValue = dble(real4)
      elseif (typeKind == ESMF_TYPEKIND_I8) then
        call ESMF_AttributeGet(importField, 'missing_value', int8, rc=localrc)
        importMissingValue = dble(int8)
      elseif (typeKind == ESMF_TYPEKIND_I4) then
        call ESMF_AttributeGet(importField, 'missing_value', int4, rc=localrc)
        importMissingValue = dble(int4)
      else
        write(message,'(A)')  '  missing value non-implemented type '
        call MOSSCO_FieldString(importField, message)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        if (present(rc)) rc = ESMF_RC_ARG_INCOMP
        return
      endif
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      importMissingValue=-1.0E30
    endif

    call ESMF_AttributeGet(exportField, name='missing_value', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(exportField, 'missing_value', typeKind=typeKind, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (typeKind == ESMF_TYPEKIND_R8) then
        call ESMF_AttributeGet(exportField, 'missing_value', real8, rc=localrc)
        exportMissingValue = real8
      elseif (typeKind == ESMF_TYPEKIND_R4) then
        call ESMF_AttributeGet(exportField, 'missing_value', real4, rc=localrc)
        exportMissingValue = dble(real4)
      elseif (typeKind == ESMF_TYPEKIND_I8) then
        call ESMF_AttributeGet(exportField, 'missing_value', int8, rc=localrc)
        exportMissingValue = dble(int8)
      elseif (typeKind == ESMF_TYPEKIND_I4) then
        call ESMF_AttributeGet(exportField, 'missing_value', int4, rc=localrc)
        exportMissingValue = dble(int4)
      else
        write(message,'(A)')  '  missing value non-implemented type '
        call MOSSCO_FieldString(exportField, message)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        if (present(rc)) rc = ESMF_RC_ARG_INCOMP
        return
      endif
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      exportMissingValue=-1.0E30
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

    !> @todo The has_boundary_data attribute is set to .true. by default here,
    !> this should be changed by a configuration attribute and the attribute name MOSSCO_AttributeGet
    !> be synchronized with the transporting component (e.g. getm_component)
    call ESMF_AttributeSet(exportField, 'has_boundary_data', .true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not. tagOnly_) then
      numChanged = 0
      select case (rank)
        case(2)
          call ESMF_FieldGet(importField, farrayPtr=importPtr2, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_FieldGet(exportField, farrayPtr=exportPtr2, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (allocated(mask2)) deallocate(mask2)
          allocate(mask2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)), stat=localrc)
          !> @todo add a mask (also for 3d) working on the missingValue
          !mask2 = (abs(exportPtr2 - exportMissingValue) > tiny(1.0))
          !mask2 = ((abs(importPtr2 - importMissingValue) >  tiny(1.0)) .and. mask2)
          mask2 = (exportPtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) .ge. 0.0 &
            .and. (importPtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) .ge. 0.0 ))
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
          if (allocated(mask3)) deallocate(mask3)
          allocate(mask3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)), stat=localrc)
          mask3 = (exportPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) .ge. 0.0 &
            .and. (importPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) .ge. 0.0 ))

          numChanged = count(mask3)
          if (numChanged>0) then
            where (mask3)
              exportPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) = (1.0 - weight_) &
                * exportPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) &
                + weight_ * importPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3))
            endwhere
          endif
          if (allocated(mask3)) deallocate(mask3)
        case default
          if (allocated(lbnd)) deallocate(lbnd)
          if (allocated(ubnd)) deallocate(ubnd)
          if (present(rc)) rc=ESMF_RC_NOT_IMPL
          return
      endselect

      if (numChanged>0) then
        write(message,'(A,ES9.2,A,I5.5,A)') '  weight ', weight_, ' changed ', numChanged, ' cells '
        call MOSSCO_FieldString(exportField, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

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
!         if (any(ncarray3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)) /= &
!                 ncarray3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)))) then
!           call self%close()
!           write(message,'(A)')  '  NaN detected in field '
!           call MOSSCO_FieldString(field, message, rc=localrc)
!           call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
!           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!         endif

end module mossco_field
