!> @brief Implementation of an ESMF soil to pelagic mediation
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015, 2016, 2017, 2018 Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>
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
#define ESMF_FILENAME "pelagic_soil_connector.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define RANGE1D lbnd(1):ubnd(1)
#define RANGE2D RANGE1D,lbnd(2):ubnd(2)
#define RANGE3D RANGE2D,lbnd(3):ubnd(3)

module pelagic_soil_connector

  use esmf
  use mossco_state
  use mossco_field
  use mossco_component

  implicit none

  private

  !> parameters
  real(ESMF_KIND_R8) :: sinking_factor=0.3d0 !> 30% of Det sinks into sediment
  real(ESMF_KIND_R8) :: NC_ldet=0.23d0 !> CAUTION!!! make sure this parameter is set according to omexdia namelist
  real(ESMF_KIND_R8) :: NC_sdet=0.01d0 !> CAUTION!!! make sure this parameter is set according to omexdia namelist
  real(ESMF_KIND_R8) :: convertN=1.0d0
  real(ESMF_KIND_R8) :: convertP=1.0d0
  real(ESMF_KIND_R8) :: sinking_factor_min=0.02 !> minimum of 2% of Det sinks always into sediment
  real(ESMF_KIND_R8) :: half_sedimentation_depth=0.1 !> [m] use 50% of prescribed sinking factor at this depth2
  real(ESMF_KIND_R8) :: half_sedimentation_tke=1.0d3 !> [m2/s2] use 50% of prescribed sinking factor for this tke
  real(ESMF_KIND_R8) :: critical_detritus=60.0 !> [mmolC/m3] use minimum sinking for det above critical_detritus

  public SetServices

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(cplComp, rc)

    implicit none

    type(ESMF_CplComp)   :: cplComp
    integer, intent(out) :: rc

    integer              :: localrc

    rc = ESMF_SUCCESS

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(cplComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_cplComp)    :: cplComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer              :: localrc
    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)       :: currTime

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(cplcomp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
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
    integer(ESMF_KIND_I4)       :: rank, ubnd(2), lbnd(2), itemCount
    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: farrayPtr3 => null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: farrayPtr2 => null()
    logical                     :: isPresent
    integer                     :: nmlunit=127

    namelist /pelagic_soil_connector/ sinking_factor,sinking_factor_min,NC_ldet,NC_sdet, &
                                      half_sedimentation_depth,critical_detritus, &
                                      half_sedimentation_tke,convertN,convertP

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !read namelist,
    !@>todo Read from .cfg file and write attributes to component's attributes
    inquire(file=trim(name)//'.nml', exist=isPresent)
    if (isPresent) then
      open(nmlunit,file='pelagic_soil_connector.nml',action='read',status='old')
      read(nmlunit,pelagic_soil_connector)
      close(nmlunit)
    endif

    !> @todo: check for necessary fields in export state?

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
 subroutine Run(cplcomp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    logical  :: hasAmmonium = .false.
    logical  :: hasNitrate = .false.
    logical  :: hasDIN = .false.
    logical  :: hasDIP = .false.

    integer                     :: rank
    integer                     :: i,j,inum,jnum
    integer(ESMF_KIND_I4), allocatable :: lbnd(:), ubnd(:)
    integer(ESMF_KIND_I4), allocatable :: lbnd2(:), ubnd2(:)
    integer                     :: Clbnd(3),AMMlbnd(3),Plbnd(3)
    integer                     :: Cubnd(3),AMMubnd(3),Pubnd(3)
    type(ESMF_Time)             :: localtime, startTime
    character (len=ESMF_MAXSTR) :: timestring
    type(ESMF_Field)            :: field

    real(ESMF_KIND_R8),dimension(:,:),pointer :: CN_det2=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: fac_ldet2=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: fac_sdet2=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: frac_ldet2=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: frac_sdet2=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: fac_env2=>null()
    real(ESMF_KIND_R8),dimension(:),pointer :: CN_det1=>null()
    real(ESMF_KIND_R8),dimension(:),pointer :: fac_ldet1=>null()
    real(ESMF_KIND_R8),dimension(:),pointer :: fac_sdet1=>null()
    real(ESMF_KIND_R8),dimension(:),pointer :: frac_ldet1=>null()
    real(ESMF_KIND_R8),dimension(:),pointer :: frac_sdet1=>null()
    real(ESMF_KIND_R8),dimension(:),pointer :: fac_env1=>null()

    real(ESMF_KIND_R8),dimension(:),pointer :: odu1=>null(), oxy1=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: odu2=>null(), oxy2=>null()
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: odu3=>null(), oxy3=>null()

    real(ESMF_KIND_R8),dimension(:),pointer :: detN1=>null(), detC1=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: detN2=>null(), detC2=>null()
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: detN3=>null(), detC3=>null()

    real(ESMF_KIND_R8),dimension(:),pointer :: vdetN1=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: vdetN2=>null()
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: vdetN3=>null()

    real(ESMF_KIND_R8),dimension(:),pointer :: detP1=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: detP2=>null()
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: detP3=>null()

    real(ESMF_KIND_R8),dimension(:),pointer :: nit1=>null(), amm1=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: nit2=>null(), amm2=>null()
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: nit3=>null(), amm3=>null()

    real(ESMF_KIND_R8),dimension(:),pointer :: din1=>null(), dip1=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: din2=>null(), dip2=>null()
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: din3=>null(), dip3=>null()

    real(ESMF_KIND_R8),dimension(:),pointer :: depth1=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: depth2=>null()

    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: farrayPtr3 => null()
    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: farrayPtr32 => null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: farrayPtr2 => null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: farrayPtr22 => null()
    real(ESMF_KIND_R8),dimension(:),     pointer :: farrayPtr1 => null()
    real(ESMF_KIND_R8),dimension(:),     pointer :: farrayPtr12 => null()

    character(len=ESMF_MAXSTR)  :: name, message, fieldName
    type(ESMF_Time)             :: currTime, stopTime
    integer                     :: localrc, oxyrc, odurc, fieldCount
    logical                             :: verbose=.true.
    type(ESMF_Field), allocatable       :: fieldList(:)
    character(len=ESMF_MAXSTR), pointer :: includeList(:) => null()
    type(ESMF_Clock)                    :: clock

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(parentClock, startTime=startTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (currTime > startTime) verbose=.false.

    !> Try to obtain (optional) hydrodynamic pelagic 3D variables and map their
    !> lowest layer to the surface layer
    call MOSSCO_Map3D2D(importState, &
      (/'photosynthetically_active_radiation_in_water      ',   &
        'downwelling_photosynthetic_radiative_flux_in_water'/), &
        exportState, (/'photosynthetically_active_radiation_at_soil_surface'/), &
        verbose=verbose, rc=localrc)
    if (localrc /= ESMF_RC_NOT_FOUND) then
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call MOSSCO_Map3D2D(importState, (/'temperature_in_water'/), &
      exportState, (/'temperature_at_soil_surface'/), verbose=verbose, rc=localrc)
    if (localrc /= ESMF_RC_NOT_FOUND) then
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call MOSSCO_Map3D2D(importState, (/'practical_salinity_in_water', &
                                             'salinity_in_water          '/), &
        exportState, (/'practical_salinity_at_soil_surface'/), verbose=verbose, rc=localrc)
    if (localrc /= ESMF_RC_NOT_FOUND) then
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    !> Get oxygen, both positive and negative (odu), and transfer it to the
    !> soil surface (optional)
    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(5))
    includeList(1)='concentration_of_dissolved_oxygen_in_water'
    includeList(2)='oxygen_in_water'
    includeList(3)='dissolved_oxygen_oxy_in_water'
    includeList(4)='hzg_ecosmo_oxy_in_water'
    includeList(5)='dissolved_oxygen_in_water'
    call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, owner=trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    oxyrc = ESMF_RC_NOT_FOUND
    if (fieldCount > 0) then
      oxyrc = ESMF_SUCCESS
      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      allocate(lbnd(rank), ubnd(rank))

      if (rank==3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr3, exclusiveLBound=lbnd, &
          exclusiveUbound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, exclusiveLBound=lbnd, &
          exclusiveUbound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    ! dissolved_reduced_substances:
    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(2))
    includeList(1) = 'dissolved_reduced_substances_odu_in_water'
    includeList(2) = 'dissolved_reduced_substances_in_water'
    call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, owner=trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    odurc = ESMF_RC_NOT_FOUND
    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      allocate(lbnd2(rank), ubnd2(rank))
      odurc = ESMF_SUCCESS

      if (rank==3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr32, exclusiveLBound=lbnd, &
          exclusiveUbound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr22, exclusiveLBound=lbnd, &
          exclusiveUbound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    allocate(includeList(1))
    includeList='dissolved_oxygen_at_soil_surface'
    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, owner=trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      ! this is always true for OMexDia

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank==2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    allocate(includeList(1))
    includeList='dissolved_reduced_substances_at_soil_surface'
    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, owner=trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      ! this is always true for OMexDia

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank==2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr22, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr12, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    if (odurc == ESMF_SUCCESS .and. oxyrc == ESMF_SUCCESS) then
      ! importState provides both oxy and odu

      ! Use association as proxy for rank
      if (associated(farrayPtr3)) then
        farrayPtr2(RANGE2D)  = farrayPtr3(RANGE2D,lbnd(3))
        farrayPtr22(RANGE2D) = farrayPtr32(RANGE2D,lbnd(3))
      elseif (associated(farrayPtr2)) then
        farrayPtr1(RANGE1D)  = farrayPtr2(RANGE1D,lbnd(2))
        farrayPtr12(RANGE1D) = farrayPtr22(RANGE1D,lbnd(2))
      endif
    elseif (odurc == ESMF_SUCCESS) then
      ! importState provides only odu, split negative part to oxy

      if (associated(farrayPtr32)) then
        farrayPtr2(RANGE2D)  =  max(0.0, -farrayPtr32(RANGE2D,lbnd(3)))
        farrayPtr22(RANGE2D) =  max(0.0, farrayPtr32(RANGE2D,lbnd(3)))
      elseif (associated(farrayPtr22)) then
        farrayPtr1(RANGE1D)  = max(0.0, -farrayPtr22(RANGE1D,lbnd(2)))
        farrayPtr12(RANGE1D) =  max(0.0, -farrayPtr22(RANGE1D,lbnd(2)))
      endif

    elseif (oxyrc == ESMF_SUCCESS) then
      ! importState provides only oxy, split negative part to odu

      if (associated(farrayPtr32)) then
        farrayPtr2(RANGE2D)  =  max(0.0, -farrayPtr32(RANGE2D,lbnd(3)))
        farrayPtr22(RANGE2D) =  max(0.0, farrayPtr32(RANGE2D,lbnd(3)))
      elseif (associated(farrayPtr22)) then
        farrayPtr1(RANGE1D)  = max(0.0, -farrayPtr22(RANGE1D,lbnd(2)))
        farrayPtr12(RANGE1D) =  max(0.0, -farrayPtr22(RANGE1D,lbnd(2)))
      endif

    else ! no vertical information found
      allocate(includeList(1))
      includeList(1) = 'hzg_ecosmo_oxy_at_soil_surface'
      call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
        include=includeList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      do i=1, fieldCount
        field = fieldList(i)
        includeList(1) = 'dissolved_oxygen_at_soil_surface'

        call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
          include=includeList, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        if (fieldCount < 1) exit

        call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (rank==2) then
          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr22, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldGet(field, farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr22(RANGE2D) = farrayPtr2(RANGE2D)
        elseif (rank==1) then

          allocate(ubnd(1), lbnd(1), stat=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, &
            exclusiveLBound=lbnd, exclusiveUbound=ubnd, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldGet(field, farrayPtr=farrayPtr12, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr12(RANGE1D) = farrayPtr1(RANGE1D)
          deallocate(ubnd, lbnd, stat=localrc)
        endif
      enddo
    endif

    !> Clean up oxygen-related fields
    if (allocated(fieldList)) deallocate(fieldList)
    if (associated(includeList)) deallocate(includeList)
    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(lbnd)) deallocate(lbnd)
    nullify(farrayPtr3, farrayPtr32, farrayPtr2, farrayPtr22, farrayPtr1, farrayPtr12)

    !> Get detritus and transfer it to the
    !> soil surface (optional), if not found, then skip the rest of
    !> this routine (@todo for now)

    allocate(includeList(8))
    includeList(1) = 'detritus_at_soil_surface'
    includeList(2) = 'detritus_in_water'
    includeList(3) = 'detN_at_soil_surface'
    includeList(4) = 'detN_in_water'
    includeList(5) = 'Detritus_Nitrogen_detN_at_soil_surface'
    includeList(6) = 'Detritus_Nitrogen_detN_in_water'
    includeList(7) = 'hzg_ecosmo_det_at_soil_surface'
    includeList(8) = 'hzg_ecosmo_det_in_water'

    call MOSSCO_StateGet(importState, fieldList, include=includeList, &
      fieldCount=fieldCount, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      allocate(ubnd(rank), lbnd(rank), stat=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank==1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detN1, exclusiveLBound=lbnd, &
          exclusiveUBound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detN2, exclusiveLBound=lbnd, &
          exclusiveUBound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detN3, exclusiveLBound=lbnd, &
          exclusiveUBound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      call ESMF_FieldGet(fieldList(1), name=fieldName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      i = index('_in_water', fieldName)
      if (i>0) then
        fieldName = fieldName(1:i)//'z_velocity_in_water'
      else
        i = index('_at_soil_surface', fieldName)
        fieldName = fieldName(1:i)//'z_velocity_at_soil_surface'
      endif

      call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
        itemSearch=trim(fieldName), verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank==1 .and. fieldcount > 0) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=vdetN1, exclusiveLBound=lbnd, &
          exclusiveUBound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==2 .and. fieldcount > 0) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=vdetN2, exclusiveLBound=lbnd, &
          exclusiveUBound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==3 .and. fieldcount > 0) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=vdetN3, exclusiveLBound=lbnd, &
          exclusiveUBound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

    endif

    !> search for Detritus-C, if present, use Detritus C-to-N ratio and apply flux
    deallocate(includeList)
    allocate(includeList(2))
    includeList(1) = 'Detritus_Carbon_detC_at_soil_surface'
    includeList(2) = 'Detritus_Carbon_detC_in_water'

    call MOSSCO_StateGet(importState, fieldList, include=includeList, &
      fieldCount=fieldCount, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      allocate(ubnd(rank), lbnd(rank), stat=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detC1, exclusiveLBound=lbnd, &
          exclusiveUBound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detC2, exclusiveLBound=lbnd, &
          exclusiveUBound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detC3, exclusiveLBound=lbnd, &
          exclusiveUBound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    write(message,*) trim(name)//' debug at ',__LINE__
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)

    ! Determine detritus C:N ratio (default is Redfield)
    if (associated(detN2)) then
      allocate(CN_det1(RANGE1D))
      if (associated(detC2)) then
        CN_det1(RANGE1D) = detC2(RANGE1D,lbnd(2)) / (1E-5 + detN2(RANGE1D,lbnd(2)))
      else
        CN_det1(RANGE1D) = 106.0d0/16.0d0
      endif
    elseif (associated(detN3)) then
      allocate(CN_det2(RANGE2D))
      if (associated(detC3)) then
        CN_det2(RANGE2D) = detC3(RANGE2D,lbnd(3)) / (1E-5 + detN3(RANGE2D,lbnd(3)))
      else
        CN_det2(RANGE2D) = 106.0d0/16.0d0
      endif
    endif

    ! From two endmembers NC_sdet and NC_ldet for semilabile and labile
    ! material, determine partitioning
    if (associated(CN_det2)) then

      allocate(fac_ldet2(RANGE2D), frac_ldet2(RANGE2D), stat=localrc)
      allocate(fac_sdet2(RANGE2D), frac_sdet2(RANGE2D), stat=localrc)
      allocate(fac_env2(RANGE2D), stat=localrc)

      fac_ldet2  = (1.0d0-NC_sdet*CN_det2)/(NC_ldet-NC_sdet)
      frac_ldet2 = fac_ldet2 * NC_ldet

      where (fac_ldet2 .gt. CN_det2)
        fac_ldet2  = CN_det2
        frac_ldet2 = 1.0d0
      endwhere

      where (fac_ldet2 .lt. 0.0d0)
        fac_ldet2  = 0.0d0
        frac_ldet2 = 0.0d0
      endwhere

      fac_sdet2 = CN_det2 - fac_ldet2
      frac_sdet2 = 1.0d0 - frac_ldet2
      fac_env2 = 1.0d0

    elseif (associated(CN_det1)) then

      allocate(fac_ldet1(RANGE1D), frac_ldet1(RANGE1D), stat=localrc)
      allocate(fac_sdet1(RANGE1D), frac_sdet1(RANGE1D), stat=localrc)
      allocate(fac_env1(RANGE1D), stat=localrc)

      fac_ldet1  = (1.0d0-NC_sdet*CN_det1)/(NC_ldet-NC_sdet)
      frac_ldet1 = fac_ldet1 * NC_ldet

      where (fac_ldet1 .gt. CN_det1)
        fac_ldet1  = CN_det1
        frac_ldet1 = 1.0d0
      endwhere

      where (fac_ldet1 .lt. 0.0d0)
        fac_ldet1  = 0.0d0
        frac_ldet1 = 0.0d0
      endwhere

      fac_sdet1 = CN_det1 - fac_ldet1
      frac_sdet1 = 1.0d0 - frac_ldet1
      fac_env1 = 1.0d0

    endif

    deallocate(includeList, stat=localrc)
    allocate(includeList(1), stat=localrc)
    includeList(1) = 'water_depth_at_soil_surface'

    ! get depth2 from exportState, where the physical model has put its data
    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)


    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=depth1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=depth2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    if (half_sedimentation_depth .gt. 1E-3) then
      ! reduce sedimentation due to depth, assuming higher wave erosion in shallow areas
      if (associated(depth1)) &
        fac_env1 = fac_env1 * depth1(RANGE1D)**2/(depth1(RANGE1D)**2 + half_sedimentation_depth**2)
      if (associated(depth2)) &
        fac_env2 = fac_env2 * depth2(RANGE2D)**2/(depth2(RANGE2D)**2 + half_sedimentation_depth**2)
    endif

    deallocate(includeList, stat=localrc)
    allocate(includeList(1), stat=localrc)
    includeList(1) = 'turbulent_kinetic_energy_at_soil_surface'

    ! get tke from exportState, where the physical model has put its data
    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (half_sedimentation_tke .lt. 9E9) then
          fac_env1 = fac_env1 * half_sedimentation_tke/(farrayPtr1(RANGE1D) + half_sedimentation_tke)
        endif
      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (half_sedimentation_tke .lt. 9E9) then
          fac_env2 = fac_env2 * half_sedimentation_tke/(farrayPtr2(RANGE2D) + half_sedimentation_tke)
        endif
      endif

    endif

    ! ensure minimum sedimentation
    if (associated(fac_env2)) fac_env2 = fac_env2 + sinking_factor_min/sinking_factor
    if (associated(fac_env1)) fac_env1 = fac_env1 + sinking_factor_min/sinking_factor

    ! reduce sedimentation due to detritus-C (assuming higher detN in shallow areas)
    if (associated(detC3) .and. associated(fac_env2)) then
      if (critical_detritus .gt. 1E-3 .and. critical_detritus .lt. 9E9) then
        fac_env2 = fac_env2 * 1.0d0/(1.0d0 + (detC3(RANGE2D,lbnd(3))/critical_detritus)**4)
      end if
    elseif (associated(detC2) .and. associated(fac_env1)) then
      if (critical_detritus .gt. 1E-3 .and. critical_detritus .lt. 9E9) then
        fac_env1 = fac_env1 * 1.0d0/(1.0d0 + (detC2(RANGE1D,lbnd(2))/critical_detritus)**4)
      end if
    end if

    !> check for Detritus-C and calculate N-based flux

    deallocate(includeList, stat=localrc)
    allocate(includeList(2), stat=localrc)
    includeList(1) = 'detritus_labile_carbon_at_soil_surface'
    includeList(2) = 'detritus_labile_carbon_z_velocity_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr1 = fac_ldet1 * convertN*detN2(RANGE1D,lbnd(2))

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr1 = sinking_factor * fac_env1 * detN2(RANGE1D,lbnd(2))

        endif

      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr2 = fac_ldet2 * convertN*detN3(RANGE2D,lbnd(3))

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr2 = sinking_factor * fac_env2 * detN3(RANGE2D,lbnd(3))
        endif
      endif
    endif

    includeList(1) = 'detritus_semilabile_carbon_at_soil_surface'
    includeList(2) = 'detritus_semilabile_carbon_z_velocity_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr1 = fac_sdet1 * convertN*detN2(RANGE1D,lbnd(2))

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr1 = sinking_factor * fac_env1 * detN2(RANGE1D,lbnd(2))

        endif

      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr2 = fac_sdet2 * convertN*detN3(RANGE2D,lbnd(3))

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr2 = sinking_factor * fac_env2 * detN3(RANGE2D,lbnd(3))
        endif
      endif
    endif

    !> check for Detritus-N and N-based flux
    deallocate(includeList, stat=localrc)
    allocate(includeList(2), stat=localrc)
    includeList(1) = 'detritus_labile_nitrogen_at_soil_surface'
    includeList(2) = 'detritus_labile_nitrogen_z_velocity_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr1 = frac_ldet1 * convertN*detN2(RANGE1D,lbnd(2))

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr1 = sinking_factor * fac_env1 * detN2(RANGE1D,lbnd(2))

        endif

      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr2 = frac_ldet2 * convertN*detN3(RANGE2D,lbnd(3))

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr2 = sinking_factor * fac_env2 * detN3(RANGE2D,lbnd(3))
        endif
      endif
    endif

    includeList(1) = 'detritus_semilabile_nitrogen_at_soil_surface'
    includeList(2) = 'detritus_semilabile_nitrogen_z_velocity_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr1 = frac_sdet1 * convertN*detN2(RANGE1D,lbnd(2))

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr1 = sinking_factor * fac_env1 * detN2(RANGE1D,lbnd(2))

        endif

      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr2 = frac_sdet2 * convertN*detN3(RANGE2D,lbnd(3))

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr2 = sinking_factor * fac_env2 * detN3(RANGE2D,lbnd(3))
        endif
      endif
    endif

    !> check for Detritus-P and calculate flux either N-based
    !> or as present through the Detritus-P pool

    includeList(1) = 'detritus_phosphorus_at_soil_surface'
    includeList(2) = 'detritus_labile_phosphorus_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Assume Redfield at first, later overwrite with actual data,
      !> if this is found in import state
      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr1 = 1.0d0/16.0d0 * convertN*detN2(RANGE1D,lbnd(2))

      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr2 = 1.0d0/16.0d0 * convertN*detN3(RANGE2D,lbnd(3))
      endif

      includeList(1) = 'detP_in_water'
      includeList(2) = 'Detritus_Phosphorus_detP_in_water'

      call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
        include=includeList, verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldCount > 0) then

        call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (rank == 2) then
          call ESMF_FieldGet(fieldList(1), farrayPtr=detP2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr1 = detP2(RANGE1D,lbnd(2))

        elseif (rank == 3) then
          call ESMF_FieldGet(fieldList(1), farrayPtr=detP3, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr2 = detP3(RANGE2D,lbnd(3))
        endif
      endif
    endif

    !> check for Detritus-P velocities and calculate flux either N-based
    !> or as present through the Detritus-P pool

    includeList(1) = 'detritus_phosphorus_z_velocity_at_soil_surface'
    includeList(2) = 'detritus_labile_phosphorus_z_velocity_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Assume Redfield at first, later overwrite with actual data,
      !> if this is found in import state
      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr1 = sinking_factor * fac_env1 * detN2(RANGE1D,lbnd(2))

      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        farrayPtr2 = sinking_factor * fac_env2 * detN3(RANGE2D,lbnd(3))
      endif

      includeList(1) = 'detP_z_velocity_in_water'
      includeList(2) = 'Detritus_Phosphorus_detP_z_velocity_in_water'

      call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
        include=includeList, verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldCount > 0) then

        call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (rank == 2) then
          call ESMF_FieldGet(fieldList(1), farrayPtr=detP2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr1 = sinking_factor * fac_env1 * detP2(RANGE1D,lbnd(2))

        elseif (rank == 3) then
          call ESMF_FieldGet(fieldList(1), farrayPtr=detP3, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr2 = sinking_factor * fac_env2 * detP3(RANGE2D,lbnd(3))
        endif
      endif
    endif

    ! Dissolved inorganic matter, i.e. nitrate, ammonium or DIN
    call MOSSCO_StateGet(importState, fieldList, itemSearchList= (/ &
      'nitrate_in_water'/), &
      fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      deallocate(lbnd,ubnd)
      allocate(ubnd(rank), lbnd(rank))

      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd, &
        exclusiveUBound=ubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=nit1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=nit2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=nit3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      write(message,'(A)') trim(name)//' obtains NO3 from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      hasNitrate = .true.

    endif

    deallocate(includeList)
    allocate(includeList(3))
    includeList(1) = 'nutrients_in_water'
    includeList(2) = 'DIN_in_water'
    includeList(3) = 'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water'
    call MOSSCO_StateGet(importState, fieldList, include=includeList, &
      fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      deallocate(lbnd,ubnd)
      allocate(ubnd(rank), lbnd(rank))

      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd, &
        exclusiveUBound=ubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=din1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=din2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=din3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      write(message,'(A)') trim(name)//' obtains DIN from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      hasDIN = .true.

    endif

    deallocate(includeList)
    allocate(includeList(2))
    includeList(1) = 'ammonium_in_water'
    includeList(2) = 'dissolved_ammonium_nh3_in_water'
    call MOSSCO_StateGet(importState, fieldList, include=includeList, &
      fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      deallocate(lbnd,ubnd)
      allocate(ubnd(rank), lbnd(rank))

      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd, &
        exclusiveUBound=ubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=amm1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=amm2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=amm3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      write(message,'(A)') trim(name)//' obtains NH4 from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      hasAmmonium = .true.

    endif


    ! Get mandatory ammonium field from export State and save either
    ! the pelagic ammonium (if present) or ammonium derived from combinations
    ! of NO3 and DIN

    call MOSSCO_StateGet(exportState, fieldList, &
      itemSearch='mole_concentration_of_ammonium_at_soil_surface', &
      fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount /= 1) then
      write(message,'(A,I1)') 'Expected exactly one complete field for mole_concentration_of_ammonium_at_soil_surface, received ',fieldCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_BAD
      return
    endif

    call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (rank == 1) then

      call ESMF_FieldGet(fieldList(1), localde=0, farrayPtr=farrayPtr1, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (hasAmmonium) then
        farrayPtr1 = convertN*amm2(RANGE1D,lbnd(2))
      elseif (hasDIN .and. hasNitrate) then
        farrayPtr1 = convertN*(din2(RANGE1D,lbnd(2)) - nit2(RANGE1D,lbnd(2)))
        write(message,'(A)') trim(name)//' calculates NH4 as DIN - NO3'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasDIN) then
        farrayPtr1 = convertN*0.5d0 * din2(RANGE1D,lbnd(2))
        write(message,'(A)') trim(name)//' calculates NH4 as 0.5 * DIN'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasNitrate) then
        farrayPtr1 = convertN*nit2(RANGE1D,lbnd(2))
        write(message,'(A)') trim(name)//' calculates NH4 as equal to NO3'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        write(message,'(A)') trim(name)//' did not receive any information on nitrogen'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_NOT_FOUND
        return
      endif

    elseif (rank == 2) then

      call ESMF_FieldGet(fieldList(1), localde=0, farrayPtr=farrayPtr2, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (hasAmmonium) then
        farrayPtr2 = convertN*amm3(RANGE2D,lbnd(3))
      elseif (hasDIN .and. hasNitrate) then
        farrayPtr2 = convertN*(din3(RANGE2D,lbnd(3)) - nit3(RANGE2D,lbnd(3)))
        write(message,'(A)') trim(name)//' calculates NH4 as DIN - NO3'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasDIN) then
        farrayPtr2 = convertN*0.5d0 * din3(RANGE2D,lbnd(3))
        write(message,'(A)') trim(name)//' calculates NH4 as 0.5 * DIN'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasNitrate) then
        farrayPtr2 = convertN*nit3(RANGE2D,lbnd(3))
        write(message,'(A)') trim(name)//' calculates NH4 as equal to NO3'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        write(message,'(A)') trim(name)//' did not receive any information on nitrogen'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_NOT_FOUND
        return
      endif
    endif

    ! Get mandatory nitrate field from export State and save either
    ! pelagic nitrate (if present) or nitrate derived from DIN and/or NH4

    call ESMF_StateGet(exportState, &
      'mole_concentration_of_nitrate_at_soil_surface', field, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (rank == 1) then

      call ESMF_FieldGet(fieldList(1), localde=0, farrayPtr=farrayPtr1, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (hasNitrate) then
        farrayPtr1 = convertN*nit2(RANGE1D,lbnd(2))
      elseif (hasAmmonium .and. hasDIN) then
        farrayPtr1 = convertN*din2(RANGE1D,lbnd(2)) &
          - amm2(RANGE1D,lbnd(2))
        write(message,'(A)') trim(name)//' calculates NO3 = DIN - NH4'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasDIN) then
        farrayPtr1 = convertN*0.5d0 * din2(RANGE1D,lbnd(2))
        write(message,'(A)') trim(name)//' calculates NO3 = 0.5 * DIN'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasAmmonium) then
        farrayPtr1 = convertN*amm2(RANGE1D,lbnd(2))
        write(message,'(A)') trim(name)//' calculates NO3 equal to NH4'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        write(message,'(A)') trim(name)//' did not receive any information on nitrogen'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_NOT_FOUND
        return
      end if

    elseif (rank == 2) then

      if (hasNitrate) then
        farrayPtr2 = convertN*nit3(RANGE2D,lbnd(3))
      elseif (hasAmmonium .and. hasDIN) then
        farrayPtr2 = convertN*din3(RANGE2D,lbnd(3)) &
          - amm3(RANGE2D,lbnd(3))
        write(message,'(A)') trim(name)//' calculates NO3 = DIN - NH4'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasDIN) then
        farrayPtr2 = convertN*0.5d0 * din3(RANGE2D,lbnd(3))
        write(message,'(A)') trim(name)//' calculates NO3 = 0.5 * DIN'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasAmmonium) then
        farrayPtr2 = convertN*amm3(RANGE2D,lbnd(3))
        write(message,'(A)') trim(name)//' calculates NO3 equal to NH4'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        write(message,'(A)') trim(name)//' did not receive any information on nitrogen'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_NOT_FOUND
        return
      end if
    endif

    !> check for DIP, if present, take as is, if not calculate it N-based
    !> with Redfield Stoichiometry

    deallocate(includeList)
    allocate(includeList(3))
    includeList(1) = 'DIP_in_water'
    includeList(2) = 'phosphate_in_water'
    includeList(3) = 'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water'
    call MOSSCO_StateGet(importState, fieldList, include=includeList, &
      fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      deallocate(lbnd,ubnd)
      allocate(ubnd(rank), lbnd(rank))

      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd, &
        exclusiveUBound=ubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=dip1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=dip2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=dip3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      write(message,'(A)') trim(name)//' obtains DIP from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      hasAmmonium = .true.

    else
      if (hasAmmonium .and. associated(amm3) .or. hasDIN .and. associated(din3) &
        .or. hasNitrate .and. associated(nit3)) then
        rank = 3
      elseif (hasAmmonium .and. associated(amm2) .or. hasDIN .and. associated(din2) &
        .or. hasNitrate .and. associated(nit2)) then
        rank = 2
      endif

      if (rank == 3) then

        if (.not.(associated(din3))) then
          allocate(din3(RANGE2D,lbnd(3)))

          if (hasAmmonium .and. hasNitrate) then
            din3(RANGE2D,lbnd(3)) = nit3(RANGE2D,lbnd(3)) + amm3(RANGE2D,lbnd(3))
            write(message,'(A)') trim(name)//' calculates DIN = NH4 + NO3'
            if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          elseif (hasAmmonium) then
            din3(RANGE2D,lbnd(3)) = 2 * amm3(RANGE2D,lbnd(3))
            write(message,'(A)') trim(name)//' calculates DIN = 2 * NH4'
            if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          else
            din3(RANGE2D,lbnd(3)) = 2 * nit3(RANGE2D,lbnd(3))
            write(message,'(A)') trim(name)//' calculates DIN = 2 * NO3'
            if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          endif
        endif


      elseif (rank == 2) then

        if (.not.(associated(din2))) then
          allocate(din2(RANGE1D,lbnd(2)))
        endif

        if (hasAmmonium .and. hasNitrate) then
          din2(RANGE1D,lbnd(2)) = nit2(RANGE1D,lbnd(2)) + amm2(RANGE1D,lbnd(2))
          write(message,'(A)') trim(name)//' calculates DIN = NH4 + NO3'
          if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        elseif (hasAmmonium) then
          din2(RANGE1D,lbnd(2)) = 2 * amm2(RANGE1D,lbnd(2))
          write(message,'(A)') trim(name)//' calculates DIN = 2 * NH4'
          if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        else
          din2(RANGE1D,lbnd(2)) = 2 * nit2(RANGE1D,lbnd(2))
          write(message,'(A)') trim(name)//' calculates DIN = 2 * NO3'
          if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif
      endif

    endif

    if (rank == 3) then

      if (.not.(associated(dip3))) allocate(dip3(RANGE3D))
      dip3(RANGE3D) = 1.0d0/16.0d0 * convertN*din3(RANGE3D)

      write(message,'(A)') trim(name)//' calculates DIP from Redfield DIN'
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateGet(exportState,'mole_concentration_of_phosphate_at_soil_surface', field, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(field,localde=0,farrayPtr=farrayPtr2,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      farrayPtr2 = convertP*dip3(RANGE2D,lbnd(3))

    elseif (rank == 2) then

      if (.not.(associated(dip2))) allocate(dip2(RANGE2D))
      dip2(RANGE2D) = 1.0d0/16.0d0 * convertN*din2(RANGE2D)

      write(message,'(A)') trim(name)//' calculates DIP from Redfield DIN'
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateGet(exportState,'mole_concentration_of_phosphate_at_soil_surface', field, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(field,localde=0,farrayPtr=farrayPtr1,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      farrayPtr1 = convertP*dip2(RANGE1D,lbnd(2))
    endif

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(cplcomp, importState, exportState, parentClock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer,intent(out)  :: rc

    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime, stopTime
    integer                     :: localrc

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Finalize

  subroutine MOSSCO_Map3D2D(importState, importFieldList, exportState, &
    exportFieldList, kwe, verbose, rc)

    type(ESMF_State), intent(in)             :: importState
    type(ESMF_State), intent(inout)          :: exportState
    character(len=*), intent(in)             :: importFieldList(:)
    character(len=*), intent(in)             :: exportFieldList(:)
    type(ESMF_KeywordEnforcer), optional, intent(in) :: kwe
    logical, intent(in), optional            :: verbose
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: localrc, fieldCount
    type(ESMF_Field), allocatable       :: fieldList3(:), fieldList2(:)
    integer(ESMF_KIND_I4)               :: lbnd(3), ubnd(3), lbnd2(2), ubnd2(2)
    real(ESMF_KIND_R8), pointer         :: farrayPtr3(:,:,:), farrayPtr2(:,:)
    logical                             :: verbose_

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(verbose)) then
      verbose_ = verbose
    else
      verbose_ = .false.
    endif
    if (present(kwe)) verbose_ = verbose_

    call MOSSCO_StateGet(importState, fieldList=fieldList3, &
      itemSearchList=importFieldList, fieldstatus=ESMF_FIELDSTATUS_COMPLETE, &
      fieldCount=fieldCount, verbose=verbose_, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Return if import field not found
    if (fieldCount == 0) then
      if (present(rc)) rc = ESMF_RC_NOT_FOUND
      return
    endif

    call MOSSCO_StateGet(exportState, fieldList=fieldList2, &
      itemSearchList=exportFieldList, verbose=verbose_, &
      fieldCount=fieldCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Return if export field not found
    if (fieldCount == 0) then
      if (present(rc)) rc = ESMF_RC_NOT_FOUND
      nullify(farrayPtr3)
      return
    endif

    call MOSSCO_FieldReduce(fieldList3(1), fieldList2(1), indexmask=(/1/), &
      owner='pelagic_soil_connector', rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    nullify(farrayPtr3)

  end subroutine MOSSCO_Map3D2D

  subroutine MOSSCO_Map2D1D(importState, importFieldList, exportState, &
    exportFieldList, kwe, verbose, rc)

    type(ESMF_State), intent(in)             :: importState
    type(ESMF_State), intent(inout)          :: exportState
    character(len=*), intent(in)             :: importFieldList(:)
    character(len=*), intent(in)             :: exportFieldList(:)
    type(ESMF_KeywordEnforcer), optional, intent(in) :: kwe
    logical, intent(in), optional            :: verbose
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: localrc, fieldCount
    type(ESMF_Field), allocatable       :: fieldList2(:), fieldList1(:)
    logical                             :: verbose_

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(verbose)) then
      verbose_ = verbose
    else
      verbose_ = .false.
    endif
    if (present(kwe)) verbose_ = verbose_

    call MOSSCO_StateGet(importState, fieldList=fieldList2, &
      itemSearchList=importFieldList, fieldstatus=ESMF_FIELDSTATUS_COMPLETE, &
      fieldCount=fieldCount, verbose=verbose_, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Return if import field not found
    if (fieldCount == 0) then
      if (present(rc)) rc = ESMF_RC_NOT_FOUND
      return
    endif

    call MOSSCO_StateGet(exportState, fieldList=fieldList1, &
      itemSearchList=exportFieldList, verbose=verbose_, &
      fieldCount=fieldCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Return if export field not found
    if (fieldCount == 0) then
      if (present(rc)) rc = ESMF_RC_NOT_FOUND
      return
    endif

    call MOSSCO_FieldReduce(fieldList2(1), fieldList1(1), indexmask=(/1/), &
      owner='pelagic_soil_connector', rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine MOSSCO_Map2D1D

end module pelagic_soil_connector
