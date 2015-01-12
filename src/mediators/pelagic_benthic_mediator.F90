!> @brief Implementation of an ESMF link coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015, Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister
!> @author Carsten Lemmen
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "pelagic_benthic_mediator.F90"

module pelagic_benthic_mediator

  use esmf
  use mossco_state
  use mossco_component

  implicit none

  private
  type(ESMF_GRID) :: pelagic_bdy_grid
  type(ESMF_ARRAYSPEC) :: pelagic_bdy_array
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DETN,DIN,vDETN
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DIP=>null(),DETP,vDETP
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: vDETC,DETC
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: nit,amm
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: oxy=>null(),odu=>null()

  public SetServices

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(GridComp, rc)

    implicit none

    type(ESMF_GridComp)   :: GridComp
    integer, intent(out) :: rc

    integer              :: localrc

    rc = ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(GridComp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetEntryPoint(GridComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetEntryPoint(GridComp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetEntryPoint(GridComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(GridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)    :: GridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer              :: localrc
    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)       :: currTime

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(GridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(GridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(GridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(GridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(Gridcomp, importState, exportState, externalclock, rc)

    type(ESMF_GridComp)   :: Gridcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc

    type(ESMF_Field)            :: newfield
    character(len=ESMF_MAXSTR)  :: name, message, stateName, fieldName, geomName
    type(ESMF_Time)             :: currTime, stopTime
    integer                     :: localrc, i
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_STATEITEM_Flag), allocatable  :: itemTypeList(:)
    type(ESMF_STATEITEM_Flag)   :: stateItem, itemType
    type(ESMF_FIELDSTATUS_Flag) :: fieldStatus
    type(ESMF_GEOMTYPE_Flag)    :: geomType
    logical                     :: found = .false.

    type(ESMF_Grid)             :: grid
    type(ESMF_Field)            :: field
    integer(ESMF_KIND_I4)       :: rank, ubnd2(2), lbnd2(2), itemCount
    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3 => null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: ptr_f2 => null()

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(GridComp, externalClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! Search for suitable 2D field in export state (first found will be used)
    call ESMF_StateGet(exportState, itemCount=itemCount, name=stateName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount == 0) then
      write(message,'(A)') trim(name)//' needs at least one field in its export state '//trim(stateName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    allocate(itemNameList(itemCount), itemTypeList(itemCount))
    call ESMF_StateGet(exportState, itemNameList=itemNameList, itemTypeList=itemTypeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, itemCount
      if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) cycle

      call ESMF_StateGet(exportState, itemNameList(i), field=field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) cycle

      call ESMF_FieldGet(field, geomType=geomType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (geomType == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(field, grid=grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_GridGet(grid, rank=rank, name=geomName, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (rank /= 2) cycle
      else
        write(message,'(A)') trim(name)//' not yet implemented obtaining geo information from non-grids. Skipped.'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      write(message,'(A)') trim(name)//' uses grid '//trim(geomName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      pelagic_bdy_grid=grid
      found=.true.
      exit

    enddo

    deallocate(itemNameList)
    deallocate(itemTypeList)

    if (.not.found) then
      write(message,'(A)') trim(name)//' found no suitable 2D field with grid in state '//trim(stateName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    ! create omexdia_p-related fields, if not existing
    call create_required_fields(exportState,pelagic_bdy_grid)

    ! Search for suitable 3D field in import state (first found will be used)
    found = .false.

    call ESMF_StateGet(importState, itemCount=itemCount, name=stateName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount == 0) then
      write(message,'(A)') trim(name)//' needs at least one field in its import state '//trim(stateName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    allocate(itemNameList(itemCount), itemTypeList(itemCount))
    call ESMF_StateGet(importState, itemNameList=itemNameList, itemTypeList=itemTypeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, itemCount
      if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) cycle

      call ESMF_StateGet(importState, itemNameList(i), field=field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) cycle

      call ESMF_FieldGet(field, geomType=geomType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (geomType == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(field, grid=grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_GridGet(grid, rank=rank, name=geomName, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (rank /= 3) cycle
      else
        write(message,'(A)') trim(name)//' not yet implemented obtaining geo information from non-grids. Skipped.'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      write(message,'(A)') trim(name)//' uses grid '//trim(geomName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      found=.true.
      exit

    enddo

    deallocate(itemNameList)
    deallocate(itemTypeList)

    if (.not.found) then
      write(message,'(A)') trim(name)//' found no suitable 3D field with grid in state '//trim(stateName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif


		! Create gridset field in import that needs to be filled
		call ESMF_StateGet(importState, 'dissolved_oxygen_in_water', itemType=itemType, rc=localrc)
		if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

		if (itemType==ESMF_STATEITEM_NOTFOUND) then
			field = ESMF_FieldEmptyCreate(name='dissolved_oxygen_in_water', rc=localrc)
		  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

		  call ESMF_FieldEmptySet(field, grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
		  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateAdd(importState,(/field/), rc=localrc)
		  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call MOSSCO_CompExit(GridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1


#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
 subroutine Run(Gridcomp, importState, exportState, externalclock, rc)

    type(ESMF_GridComp)   :: Gridcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc
    integer              :: ammrc,nitrc

    integer                     :: myrank
    integer                     :: i,j,inum,jnum
    integer                     :: lbnd(3)=1,ubnd(3)=1
    integer                     :: Clbnd(3),AMMlbnd(3),Plbnd(3)
    integer                     :: Cubnd(3),AMMubnd(3),Pubnd(3)
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    type(ESMF_Field)            :: field
    real(ESMF_KIND_R8),parameter      :: sinking_factor=0.3d0 !> 30% of Det sinks into sediment
    real(ESMF_KIND_R8),dimension(:,:),pointer :: CN_det=>null()
    !> @todo read NC_fdet dynamically from fabm model info?  This would not comply with our aim to separate fabm/esmf
    real(ESMF_KIND_R8),parameter    :: NC_fdet=0.20d0
    real(ESMF_KIND_R8),parameter    :: NC_sdet=0.04d0
    real(ESMF_KIND_R8),dimension(:,:),pointer :: fac_fdet
    real(ESMF_KIND_R8),dimension(:,:),pointer :: fac_sdet
    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3 => null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: ptr_f2 => null()

    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime, stopTime
    integer                     :: localrc

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(GridComp, externalClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> fdet + sdet = CN_det*det
    !> NC_fdet*fdet + NC_sdet*sdet = det
    !> fdet = fac_fdet*det
    !> sdet = fac_sdet*det

    ! water temperature:
    call mossco_state_get(importState,(/'temperature_in_water'/),ptr_f3,lbnd=lbnd,ubnd=ubnd,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call mossco_state_get(exportState,(/'temperature_at_soil_surface'/),ptr_f2,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ptr_f2 = ptr_f3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))

    ! dissolved_oxygen:
    call mossco_state_get(importState,(/ &
        'concentration_of_dissolved_oxygen_in_water', &
        'oxygen_in_water                           ', &
        'dissolved_oxygen_in_water                 '/),ptr_f3,lbnd=lbnd,ubnd=ubnd,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call mossco_state_get(exportState,(/'dissolved_oxygen_at_soil_surface'/),ptr_f2,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.associated(oxy)) allocate(oxy(lbnd(1):ubnd(1),lbnd(2):ubnd(2)))
    if (.not.associated(odu)) allocate(odu(lbnd(1):ubnd(1),lbnd(2):ubnd(2)))

    do j=lbnd(2),ubnd(2)
       do i=lbnd(1),ubnd(1)
          oxy = max(0.0d0,ptr_f3(i,j,lbnd(3)))
          odu = max(0.0d0,-ptr_f3(i,j,lbnd(3)))
       end do
    end do
    ptr_f2 = oxy(:,:)

      !   Det flux:
    call mossco_state_get(importState,(/ &
            'detritus_in_water              ', &
            'detN_in_water                  ', &
            'Detritus_Nitrogen_detN_in_water'/),DETN,lbnd=lbnd,ubnd=ubnd,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if ( ubnd(1)-lbnd(1)<0 .or. ubnd(2)-lbnd(2)<0 .or. ubnd(3)-lbnd(3)<0 ) then
      write(message,'(A)')  trim(name)//' received zero-length data for detritus nitrogen'
      write(0,*) 'lbnd = ', lbnd, 'ubnd = ', ubnd

      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call mossco_state_get(importState,(/ &
            'detritus_z_velocity_in_water              ', &
            'detN_z_velocity_in_water                  ', &
            'Detritus_Nitrogen_detN_z_velocity_in_water'/),vDETN,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      inum=ubnd(1)-lbnd(1)+1
      jnum=ubnd(2)-lbnd(2)+1
      if (.not.associated(CN_det)) allocate(CN_det(1:inum,1:jnum))
      if (.not.associated(fac_fdet)) allocate(fac_fdet(1:inum,1:jnum))
      if (.not.associated(fac_sdet)) allocate(fac_sdet(1:inum,1:jnum))
      !> search for Detritus-C, if present, use Detritus C-to-N ratio and apply flux
      call mossco_state_get(importState,(/'Detritus_Carbon_detC_in_water'/),DETC,lbnd=Clbnd,ubnd=Cubnd,rc=localrc)

      if (localrc /= 0) then
         CN_det=106.0d0/16.0d0
      else
        if ( Cubnd(1)-Clbnd(1)<0 .or. Cubnd(2)-Clbnd(2)<0 .or. Cubnd(3)-Clbnd(3)<0 ) then
          write(message,'(A)')  trim(name)//' received zero-length data for detritus carbon'
          write(0,*) 'Clbnd = ', lbnd, 'Cubnd = ', ubnd

          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          call ESMF_LogFlush()
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

        CN_det = DETC(Clbnd(1):Cubnd(1),Clbnd(2):Cubnd(2),Clbnd(3))/ &
                   DETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
      end if
      fac_fdet = (1.0d0-NC_sdet*CN_det)/(NC_fdet-NC_sdet)
      fac_sdet = (1.0d0-NC_fdet*CN_det)/(NC_sdet-NC_fdet)

      call mossco_state_get(exportState, &
        (/'fast_detritus_C_at_soil_surface'/), ptr_f2, rc=localrc)
      if (localrc==0) ptr_f2 = fac_fdet * DETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
      call mossco_state_get(exportState, &
        (/'slow_detritus_C_at_soil_surface'/), ptr_f2, rc=localrc)
      if(localrc==0) ptr_f2 = fac_sdet * DETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))

      call mossco_state_get(exportState,(/'fast_detritus_C_z_velocity_at_soil_surface'/),ptr_f2,rc=localrc)
      if (localrc==0) ptr_f2 = sinking_factor * vDETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
      call mossco_state_get(exportState,(/'slow_detritus_C_z_velocity_at_soil_surface'/),ptr_f2,rc=localrc)
      if (localrc==0) ptr_f2 = sinking_factor * vDETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))

      !> check for Detritus-P and calculate flux either N-based
      !> or as present through the Detritus-P pool
      call mossco_state_get(exportState,(/'detritus-P_at_soil_surface'/),ptr_f2,rc=localrc)
      call mossco_state_get(importState,(/ &
          'detP_in_water                    ', &
          'Detritus_Phosphorus_detP_in_water'/),DETP,lbnd=Plbnd,ubnd=Pubnd,rc=localrc)
      if (localrc == 0) then
        ptr_f2 = DETP(Plbnd(1):Pubnd(1),Plbnd(2):Pubnd(2),plbnd(3))
      else
        ptr_f2 = 1.0d0/16.0d0 * DETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
      end if

      call mossco_state_get(exportState,(/'detritus-P_z_velocity_at_soil_surface'/),ptr_f2,rc=localrc)
      call mossco_state_get(importState,(/ &
              'detP_z_velocity_in_water                    ', &
              'Detritus_Phosphorus_detP_z_velocity_in_water'/),vDETP,rc=localrc)
      if (localrc==0) then
        ptr_f2 = sinking_factor * vDETP(Plbnd(1):Pubnd(1),Plbnd(2):Pubnd(2),Plbnd(3))
      else
        ptr_f2 = sinking_factor * vDETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
      end if

      ! DIM concentrations:
      !  oxygen is coming from constant component
      !  set reduced substances to zero
      call mossco_state_get(exportState,(/'dissolved_reduced_substances_at_soil_surface'/),ptr_f2,rc=localrc)
      ptr_f2 = odu(:,:)

      call mossco_state_get(importState,(/'nitrate_in_water'/),nit,ubnd=ubnd,lbnd=lbnd,rc=nitrc)
      if (nitrc /= 0) then
        call mossco_state_get(importState,(/ &
              'nutrients_in_water                            ', &
              'DIN_in_water                                  ', &
              'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water'/),DIN,lbnd=lbnd,ubnd=ubnd,rc=localrc)
      end if
      call mossco_state_get(importState,(/'ammonium_in_water'/),amm,lbnd=AMMlbnd,ubnd=AMMubnd,rc=ammrc)

      call ESMF_StateGet(exportState,'mole_concentration_of_ammonium_at_soil_surface',field,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f2,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (ammrc == 0) then
        ptr_f2 = amm(AMMlbnd(1):AMMubnd(1),AMMlbnd(2):AMMubnd(2),AMMlbnd(3))
      else
        ptr_f2 = 0.5d0 * DIN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
      end if
      call ESMF_StateGet(exportState,'mole_concentration_of_nitrate_at_soil_surface',field,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f2,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (nitrc == 0) then
        ptr_f2 = nit(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
      else
        ptr_f2 = 0.5d0 * DIN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
      end if

      !> check for DIP, if present, take as is, if not calculate it N-based
      call mossco_state_get(importState,(/ &
          'DIP_in_water                                    ', &
          'phosphate_in_water                              ', &
          'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water'/),DIP,lbnd=Plbnd,ubnd=Pubnd,rc=localrc)
    if (localrc /= 0) then
        if (.not.(associated(DIP))) allocate(DIP(lbnd(1):ubnd(1),lbnd(2):ubnd(2),1))
        DIP(lbnd(1):ubnd(1),lbnd(2):ubnd(2),1) = 1.0d0/16.0d0 * DIN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
        Plbnd(3)=1
    end if

    call ESMF_StateGet(exportState,'mole_concentration_of_phosphate_at_soil_surface',field,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f2,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ptr_f2 = DIP(lbnd(1):ubnd(1),lbnd(2):ubnd(2),Plbnd(3))

    call MOSSCO_CompExit(GridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(Gridcomp, importState, exportState, externalclock, rc)
    type(ESMF_GridComp)   :: Gridcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer,intent(out)  :: rc

    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime, stopTime
    integer                     :: localrc

    call MOSSCO_CompEntry(GridComp, externalClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !call ESMF_ArraySpecDestroy(pelagic_bdy_array, rc)
    call ESMF_GridDestroy(pelagic_bdy_grid, rc=localrc)

    call MOSSCO_CompExit(GridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize


end module pelagic_benthic_mediator

