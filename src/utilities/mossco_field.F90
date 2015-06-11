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

  integer(ESMF_KIND_I4)   :: rc_, length_, rank, localrc, gridRank, n, i, width
  integer(ESMF_KIND_I4), allocatable :: lbnd(:), ubnd(:), ungriddedLbnd(:), ungriddedUbnd(:)

  character(ESMF_MAXSTR)  :: geomName, stringValue, name, form
  type(ESMF_Grid)         :: grid

  type(ESMF_GeomType_Flag) :: geomType
  type(ESMF_FieldStatus_Flag) :: fieldStatus
  logical                     :: isPresent

  rc_ = ESMF_SUCCESS
  rank = 0
  gridRank = 0

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

    allocate(ubnd(rank))
    allocate(lbnd(rank))

    call ESMF_FieldGetBounds(field, exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rank>0 .and. (len_trim(message) + 5 <=len(message))) then
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

  if (allocated(ubnd)) deallocate(ubnd)
  if (allocated(lbnd)) deallocate(lbnd)
  if (allocated(ungriddedUbnd)) deallocate(ungriddedUbnd)
  if (allocated(ungriddedLbnd)) deallocate(ungriddedLbnd)

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


end module mossco_field
