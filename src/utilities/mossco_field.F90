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

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldInitialize"
  subroutine MOSSCO_FieldInitialize(field, kwe, rc)

    type(ESMF_Field), intent(inout)                :: field
    logical, intent(in), optional                  :: kwe ! Keyword enforcer
    integer(ESMF_KIND_I4), intent(out), optional   :: rc

    character(len=ESMF_MAXSTR)               :: message
    integer(ESMF_KIND_I4)                    :: rc_, rank, localrc
    integer(ESMF_KIND_I4), allocatable       :: ubnd(:), lbnd(:)
    type(ESMF_TypeKind_Flag)                 :: typeKind

    real(ESMF_KIND_R8), pointer  :: farrayPtr1(:), farrayPtr2(:,:)
    real(ESMF_KIND_R8), pointer  :: farrayPtr3(:,:,:), farrayPtr4(:,:,:,:)
    real(ESMF_KIND_R8), pointer  :: farrayPtr5(:,:,:,:,:), farrayPtr6(:,:,:,:,:,:)
    real(ESMF_KIND_R8), pointer  :: farrayPtr7(:,:,:,:,:,:,:)

    type(ESMF_FieldStatus_Flag) :: fieldStatus

    rc_ = ESMF_SUCCESS

    call ESMF_FieldGet(field, status=fieldStatus, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
      write(message,'(A)') 'Cannot initialize incomplete '
      call MOSSCO_FieldString(field, message)
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
      call MOSSCO_FieldString(field, message)
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

    type(ESMF_Field), intent(in)                 :: field
    type(ESMF_State), intent(in)                 :: state
    type(ESMF_Field),  allocatable               :: fieldList(:)
    logical, intent(in), optional                :: kwe
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, i, itemCount, fieldCount
    character(len=ESMF_MAXSTR)              :: name
    character(len=ESMF_MAXPATHLEN)          :: message
    type(ESMF_StateItem_Flag)               :: itemType
    type(ESMF_FieldBundle)                  :: fieldBundle

    rc_ = ESMF_SUCCESS

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

    type(ESMF_Field), intent(inout), allocatable :: fieldList(:)
    integer(ESMF_KIND_I4), intent(in)            :: fieldCount
    logical, intent(in), optional                :: kwe
    logical, intent(in), optional                :: keep
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, listSize
    logical                                 :: keep_
    character(len=ESMF_MAXPATHLEN)          :: message
    type(ESMF_Field),  allocatable          :: tempList(:)

    rc_ = ESMF_SUCCESS
    keep_ = .true.
    if (present(keep)) keep_ = keep
    if (present(rc)) rc = rc_

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
    integer(ESMF_KIND_I4), intent(in)            :: itemCount
    logical, intent(in), optional                :: kwe
    logical, intent(in), optional                :: keep
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, listSize
    logical                                 :: keep_
    character(len=ESMF_MAXPATHLEN)          :: message
    type(ESMF_StateItem_Flag),  allocatable :: tempList(:)

    rc_ = ESMF_SUCCESS
    keep_ = .true.
    if (present(keep)) keep_ = keep
    if (present(rc)) rc = rc_

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
    logical, intent(in), optional                :: kwe
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
    logical, intent(in), optional                :: kwe
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
    logical, intent(in), optional                :: kwe
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                        :: localrc, rc_, missingValueI4
    integer(ESMF_KIND_I4)                        :: missingValueI8
    real(ESMF_KIND_R4)                           :: missingValueR4
    logical                                      :: isPresent
    character(len=ESMF_MAXSTR)                   :: message
    type(ESMF_TypeKind_Flag)                     :: typeKind

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS

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
        call MOSSCO_FieldString(field, message)
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
    logical, intent(in), optional                :: kwe
    character(len=*), dimension(*), optional     :: exclude(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc
    integer(ESMF_KIND_I4)                        :: differCount

    real(ESMF_KIND_R4)                           :: importReal4, exportReal4
    real(ESMF_KIND_R8)                           :: importReal8, exportReal8
    integer(ESMF_KIND_I8)                        :: importInt8, exportInt8
    integer(ESMF_KIND_I4)                        :: localrc, rc_, importInt4, exportInt4
    integer(ESMF_KIND_I4)                        :: importCount, exportcount, i, j
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

      call ESMF_AttributeGet(importfield, name=attributeName, typeKind=importTypeKind, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeGet(exportfield, name=attributeName, typeKind=exportTypeKind, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (importTypeKind /= exportTypeKind) then
        differCount = differCount + 1
        cycle
      endif

      if (importTypeKind == ESMF_TYPEKIND_CHARACTER) then
        call ESMF_AttributeGet(importField, name=attributeName, value=importString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AttributeGet(exportField, name=attributeName, value=exportString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (trim(exportString) /= trim(importString)) then
          differCount = differCount + 1
          cycle
        endif
      endif
    enddo

    call MOSSCO_Reallocate(excludeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end function MOSSCO_FieldAttributesIdentical

end module mossco_field
