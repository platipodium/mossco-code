!> @brief Implementation of an ESMF soil to pelagic mediation
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014-2020 Helmholtz-Zentrum Geesthacht
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

  type psVariable
    type(ESMF_Field)            :: field
    real(ESMF_KIND_R8), pointer :: data1(:) => null()
    real(ESMF_KIND_R8), pointer :: data2(:,:) => null()
    real(ESMF_KIND_R8), pointer :: data3(:,:,:) => null()
    integer(ESMF_KIND_I4)       :: rank = 0
    character(len=ESMF_MAXSTR)  :: unit = ''
    integer(ESMF_KIND_I4), allocatable :: lbnd(:), ubnd(:)
  end type psVariable

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

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", &
      valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, rc=localrc)
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

    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    integer                     :: localrc
    !> @todo dynamically find free unit
    integer                     :: nmlunit=127
    logical                     :: isPresent

    namelist /pelagic_soil_connector/ sinking_factor,sinking_factor_min, &
      NC_ldet,NC_sdet, half_sedimentation_depth,critical_detritus, &
      half_sedimentation_tke,convertN,convertP

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, &
      rc=localrc)
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

    call MOSSCO_CompExit(cplComp, rc=localrc)
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

    logical                     :: hasAmmonium, hasNitrate, hasDIN, hasDIP
    integer                     :: rank=-1, exportRank=-1
    integer                     :: i,j,inum,jnum
    integer(ESMF_KIND_I4)       :: lbnd(3), ubnd(3)
    integer(ESMF_KIND_I4), allocatable :: exportLbnd(:), exportUbnd(:)
!    integer                     :: Clbnd(3),AMMlbnd(3),Plbnd(3)
!    integer                     :: Cubnd(3),AMMubnd(3),Pubnd(3)
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
    logical                             :: verbose=.false.
    type(ESMF_Field), allocatable       :: fieldList(:)
    character(len=ESMF_MAXSTR), pointer :: includeList(:) => null()
    type(ESMF_Clock)                    :: clock

    type(psVariable) :: soilNitrate, soilAmmonium, soilLabileCarbon
    type(psVariable) :: soilSemilabileCarbon, soilPhosphorous
    type(psVariable) :: waterNitrate, waterAmmonium, waterNutrient
    type(psVariable) :: waterPhosphorous, soilOdu, waterOdu
    type(psVariable) :: soilOxygen, waterOxygen
    logical          :: isEqual, isPresent

    rc = ESMF_SUCCESS
    lbnd(:) = 1
    ubnd(:) = 1

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(parentClock, startTime=startTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    verbose = .false.
    if (currTime == startTime) verbose=.true.

    !> Look for all species in export, which is well known as long
    !> as we have only omexdia as an export model

    ! call MOSSCO_StateGet(exportState, fieldList, &
    !   itemSearch='dissolved_oxygen_upward_flux_at_soil_surface', &
    !   fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    ! _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    ! if (fieldCount /= 1) then
    !   write(message,'(A,I1)') 'Expected exactly one complete field for dissolved_oxygen_upward_flux_at_soil_surface, received ',fieldCount
    !   call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    !   rc = ESMF_RC_ARG_BAD
    !   return
    ! else
    !   soilOxygen%field = fieldList(1)
    !   call ESMF_FieldGet(fieldList(1), rank=soilOxygen%rank, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   if (soilOxygen%rank == 1) then
    !     call ESMF_FieldGet(fieldList(1), farrayPtr=soilOxygen%data1)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   elseif (soilOxygen%rank == 2) then
    !     call ESMF_FieldGet(fieldList(1), farrayPtr=soilOxygen%data2)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   endif
    !   call ESMF_AttributeGet(fieldList(1), 'unit', soilOxygen%unit, &
    !     isPresent=isPresent, defaultValue='', rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   allocate(soilOxygen%ubnd(rank))
    !   allocate(soilOxygen%lbnd(rank))
    !
    !   call ESMF_FieldGetBounds(fieldList(1), localDe=0,  exclusiveLBound=soilOxygen%lbnd, &
    !     exclusiveUBound=soilOxygen%ubnd, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   write(message, '(A)') trim(name)//' uses soil oxygen '
    !   call MOSSCO_FieldString(fieldList(1), message)
    !   if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    ! endif
    !
    ! call MOSSCO_StateGet(exportState, fieldList, &
    !   itemSearch='dissolved_reduced_substances_odu_upward_flux_at_soil_surface', &
    !   fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    ! _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    ! if (fieldCount /= 1) then
    !   write(message,'(A,I1)') 'Expected exactly one complete field for dissolved_reduced_substances_odu_upward_flux_at_soil_surface, received ',fieldCount
    !   call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    !   rc = ESMF_RC_ARG_BAD
    !   return
    ! else
    !   soilOdu%field = fieldList(1)
    !   call ESMF_FieldGet(fieldList(1), rank=soilOdu%rank, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   if (soilOdu%rank == 1) then
    !     call ESMF_FieldGet(fieldList(1), farrayPtr=soilOdu%data1)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   elseif (soilOdu%rank == 2) then
    !     call ESMF_FieldGet(fieldList(1), farrayPtr=soilOdu%data2)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   endif
    !   call ESMF_AttributeGet(fieldList(1), 'unit', soilOdu%unit, &
    !     isPresent=isPresent, defaultValue='', rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   allocate(soilOdu%ubnd(rank))
    !   allocate(soilOdu%lbnd(rank))
    !
    !   call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=soilOdu%lbnd, &
    !     exclusiveUBound=soilOdu%ubnd, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   write(message, '(A)') trim(name)//' uses soil ODU '
    !   call MOSSCO_FieldString(fieldList(1), message)
    !   if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    ! endif
    !
    ! !> Find oxygen information in import
    ! if (associated(includeList)) deallocate(includeList)
    ! allocate(includeList(10))
    ! includeList(1)='concentration_of_dissolved_oxygen_at_soil_surface'
    ! includeList(2)='concentration_of_dissolved_oxygen_in_water'
    ! includeList(3)='oxygen_at_soil_surface'
    ! includeList(4)='oxygen_in_water'
    ! includeList(5)='dissolved_oxygen_oxy_at_soil_surface'
    ! includeList(6)='dissolved_oxygen_oxy_in_water'
    ! includeList(7)='hzg_ecosmo_oxy_at_soil_surface'
    ! includeList(8)='hzg_ecosmo_oxy_in_water'
    ! includeList(9)='dissolved_oxygen_at_soil_surface'
    ! includeList(10)='dissolved_oxygen_in_water'
    ! call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
    !   include=includeList, verbose=verbose, owner=trim(name), rc=localrc)
    ! _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    ! if (fieldCount > 0) then
    !   waterOxygen%field = fieldList(1)
    !   call ESMF_FieldGet(fieldList(1), rank=waterOxygen%rank, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   if (waterOxygen%rank == 1) then
    !     call ESMF_FieldGet(fieldList(1), farrayPtr=waterOxygen%data1)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   elseif (waterOxygen%rank == 2) then
    !     call ESMF_FieldGet(fieldList(1), farrayPtr=waterOxygen%data2)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   elseif (waterOxygen%rank == 3) then
    !     call ESMF_FieldGet(fieldList(1), farrayPtr=waterOxygen%data3)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   endif
    !   call ESMF_AttributeGet(fieldList(1), 'unit', waterOxygen%unit, &
    !     isPresent=isPresent, defaultValue='', rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   allocate(waterOxygen%ubnd(rank))
    !   allocate(waterOxygen%lbnd(rank))
    !
    !   call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=waterOxygen%lbnd, &
    !     exclusiveUBound=waterOxygen%ubnd, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   write(message, '(A)') trim(name)//' uses water oxygen '
    !   call MOSSCO_FieldString(fieldList(1), message)
    !   if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    ! endif
    !
    ! if (associated(includeList)) deallocate(includeList)
    ! allocate(includeList(4))
    ! includeList(1) = 'dissolved_reduced_substances_odu_at_soil_surface'
    ! includeList(2) = 'dissolved_reduced_substances_odu_in_water'
    ! includeList(3) = 'dissolved_reduced_substances_at_soil_surface'
    ! includeList(4) = 'dissolved_reduced_substances_in_water'
    ! call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
    !   include=includeList, verbose=verbose, owner=trim(name), rc=localrc)
    ! _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    ! if (fieldCount > 0) then
    !   waterOdu%field = fieldList(1)
    !   call ESMF_FieldGet(fieldList(1), rank=waterOdu%rank, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   if (waterOdu%rank == 1) then
    !     call ESMF_FieldGet(fieldList(1), farrayPtr=waterOdu%data1)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   elseif (waterOdu%rank == 2) then
    !     call ESMF_FieldGet(fieldList(1), farrayPtr=waterOdu%data2)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   elseif (waterOdu%rank == 3) then
    !     call ESMF_FieldGet(fieldList(1), farrayPtr=waterOdu%data3)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !   endif
    !   call ESMF_AttributeGet(fieldList(1), 'unit', waterOdu%unit, &
    !     isPresent=isPresent, defaultValue='', rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   allocate(waterOdu%ubnd(rank))
    !   allocate(waterOdu%lbnd(rank))
    !
    !   call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=waterOdu%lbnd, &
    !     exclusiveUBound=waterOdu%ubnd, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   write(message, '(A)') trim(name)//' uses water ODU '
    !   call MOSSCO_FieldString(fieldList(1), message)
    !   if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    ! endif
    !
    ! !> Separately treat oxy and odu if they appear both on both
    ! !> sides of the connector
    ! if (waterOxygen%rank > 0 .and. waterOdu%rank > 0) then
    !   if (waterOxygen%rank == 3 .and. soilOxygen%rank == 2) then
    !     soilOxygen%data2(soilOxygen%lbnd(1):soilOxygen%ubnd(1), &
    !       soilOxygen%lbnd(2):soilOxygen%ubnd(2)) &
    !       = waterOxygen%data3(waterOxygen%lbnd(1):waterOxygen%ubnd(1), &
    !       waterOxygen%lbnd(2):waterOxygen%ubnd(2),waterOxygen%lbnd(1))
    !   elseif (waterOxygen%rank == 2 .and. soilOxygen%rank == 2) then
    !     soilOxygen%data2(soilOxygen%lbnd(1):soilOxygen%ubnd(1), &
    !       soilOxygen%lbnd(2):soilOxygen%ubnd(2)) &
    !       = waterOxygen%data2(waterOxygen%lbnd(1):waterOxygen%ubnd(1), &
    !       waterOxygen%lbnd(2):waterOxygen%ubnd(2))
    !   elseif (waterOxygen%rank == 2 .and. soilOxygen%rank == 1) then
    !     soilOxygen%data1(soilOxygen%lbnd(1):soilOxygen%ubnd(1)) &
    !       = waterOxygen%data2(waterOxygen%lbnd(1):waterOxygen%ubnd(1), &
    !       waterOxygen%lbnd(2))
    !   elseif (waterOxygen%rank == 1 .and. soilOxygen%rank == 1) then
    !       soilOxygen%data1(soilOxygen%lbnd(1):soilOxygen%ubnd(1)) &
    !       = waterOxygen%data1(waterOxygen%lbnd(1):waterOxygen%ubnd(1))
    !   endif
    !   if (waterOdu%rank == 3 .and. soilOdu%rank == 2) then
    !     soilOdu%data2(soilOdu%lbnd(1):soilOdu%ubnd(1), &
    !       soilOdu%lbnd(2):soilOdu%ubnd(2)) &
    !       = waterOdu%data3(waterOdu%lbnd(1):waterOdu%ubnd(1), &
    !       waterOdu%lbnd(2):waterOdu%ubnd(2),waterOdu%lbnd(1))
    !   elseif (waterOdu%rank == 2 .and. soilOdu%rank == 2) then
    !     soilOdu%data2(soilOdu%lbnd(1):soilOdu%ubnd(1), &
    !       soilOdu%lbnd(2):soilOdu%ubnd(2)) &
    !       = waterOdu%data2(waterOdu%lbnd(1):waterOdu%ubnd(1), &
    !       waterOdu%lbnd(2):waterOdu%ubnd(2))
    !   elseif (waterOdu%rank == 2 .and. soilOdu%rank == 1) then
    !     soilOdu%data1(soilOdu%lbnd(1):soilOdu%ubnd(1)) &
    !       = waterOdu%data2(waterOdu%lbnd(1):waterOdu%ubnd(1), &
    !       waterOdu%lbnd(2))
    !   elseif (waterOdu%rank == 1 .and. soilOdu%rank == 1) then
    !       soilOdu%data1(soilOdu%lbnd(1):soilOdu%ubnd(1)) &
    !       = waterOdu%data1(waterOdu%lbnd(1):waterOdu%ubnd(1))
    !   endif
    ! elseif (waterOxygen%rank > 0) then
    !   !> Only oxygen, not odu in water, thus split negative part
    !   !> off into odu
    !   if (waterOxygen%rank == 3 .and. soilOxygen%rank == 2) then
    !     soilOxygen%data2(soilOxygen%lbnd(1):soilOxygen%ubnd(1), &
    !       soilOxygen%lbnd(2):soilOxygen%ubnd(2)) &
    !       = waterOxygen%data3(waterOxygen%lbnd(1):waterOxygen%ubnd(1), &
    !       waterOxygen%lbnd(2):waterOxygen%ubnd(2),waterOxygen%lbnd(1))
    !   elseif (waterOxygen%rank == 2 .and. soilOxygen%rank == 2) then
    !     soilOxygen%data2(soilOxygen%lbnd(1):soilOxygen%ubnd(1), &
    !       soilOxygen%lbnd(2):soilOxygen%ubnd(2)) &
    !       = waterOxygen%data2(waterOxygen%lbnd(1):waterOxygen%ubnd(1), &
    !       waterOxygen%lbnd(2):waterOxygen%ubnd(2))
    !   elseif (waterOxygen%rank == 2 .and. soilOxygen%rank == 1) then
    !     soilOxygen%data1(soilOxygen%lbnd(1):soilOxygen%ubnd(1)) &
    !       = waterOxygen%data2(waterOxygen%lbnd(1):waterOxygen%ubnd(1), &
    !       waterOxygen%lbnd(2))
    !   elseif (waterOxygen%rank == 1 .and. soilOxygen%rank == 1) then
    !       soilOxygen%data1(soilOxygen%lbnd(1):soilOxygen%ubnd(1)) &
    !       = waterOxygen%data1(waterOxygen%lbnd(1):waterOxygen%ubnd(1))
    !   endif
    !   if (waterOxygen%rank == 3 .and. soilOdu%rank == 2) then
    !     soilOdu%data2(soilOdu%lbnd(1):soilOdu%ubnd(1), &
    !       soilOdu%lbnd(2):soilOdu%ubnd(2)) &
    !       = - waterOxygen%data3(waterOxygen%lbnd(1):waterOxygen%ubnd(1), &
    !       waterOxygen%lbnd(2):waterOxygen%ubnd(2),waterOxygen%lbnd(1))
    !   elseif (waterOxygen%rank == 2 .and. soilOdu%rank == 2) then
    !     soilOdu%data2(soilOdu%lbnd(1):soilOdu%ubnd(1), &
    !       soilOdu%lbnd(2):soilOdu%ubnd(2)) &
    !       = - waterOxygen%data2(waterOxygen%lbnd(1):waterOxygen%ubnd(1), &
    !       waterOxygen%lbnd(2):waterOxygen%ubnd(2))
    !   elseif (waterOxygen%rank == 2 .and. soilOdu%rank == 1) then
    !     soilOdu%data1(soilOdu%lbnd(1):soilOdu%ubnd(1)) &
    !       = - waterOxygen%data2(waterOxygen%lbnd(1):waterOxygen%ubnd(1), &
    !       waterOxygen%lbnd(2))
    !   elseif (waterOxygen%rank == 1 .and. soilOdu%rank == 1) then
    !       soilOdu%data1(soilOdu%lbnd(1):soilOdu%ubnd(1)) &
    !       = - waterOxygen%data1(waterOxygen%lbnd(1):waterOxygen%ubnd(1))
    !   endif
    !   if (soilOdu%rank == 2) then
    !     where(soilOdu%data2(soilOdu%lbnd(1):soilOdu%ubnd(1), &
    !       soilOdu%lbnd(2):soilOdu%ubnd(2)) < 0)
    !       soilOdu%data2(soilOdu%lbnd(1):soilOdu%ubnd(1), &
    !         soilOdu%lbnd(2):soilOdu%ubnd(2)) = 0.0
    !     endwhere
    !   elseif (soilOdu%rank == 1) then
    !     where(soilOdu%data1(soilOdu%lbnd(1):soilOdu%ubnd(1)) < 0)
    !       soilOdu%data1(soilOdu%lbnd(1):soilOdu%ubnd(1)) = 0.0
    !     endwhere
    !   endif
    !   if (soilOxygen%rank == 2) then
    !     where(soilOxygen%data2(soilOxygen%lbnd(1):soilOxygen%ubnd(1), &
    !       soilOxygen%lbnd(2):soilOxygen%ubnd(2)) < 0)
    !       soilOxygen%data2(soilOxygen%lbnd(1):soilOxygen%ubnd(1), &
    !         soilOxygen%lbnd(2):soilOxygen%ubnd(2)) = 0.0
    !     endwhere
    !   elseif (soilOxygen%rank == 1) then
    !     where(soilOxygen%data1(soilOxygen%lbnd(1):soilOxygen%ubnd(1)) < 0)
    !       soilOxygen%data1(soilOxygen%lbnd(1):soilOxygen%ubnd(1)) = 0.0
    !     endwhere
    !   endif
    ! elseif (waterOdu%rank > 0) then
    !   !> Only odu, not oxygen in water, thus split negative part
    !   !> off into oxygen
    !   if (waterOdu%rank == 3 .and. soilOxygen%rank == 2) then
    !     soilOxygen%data2(soilOxygen%lbnd(1):soilOxygen%ubnd(1), &
    !       soilOxygen%lbnd(2):soilOxygen%ubnd(2)) &
    !       = -waterOdu%data3(waterOdu%lbnd(1):waterOdu%ubnd(1), &
    !       waterOdu%lbnd(2):waterOdu%ubnd(2),waterOdu%lbnd(1))
    !   elseif (waterOdu%rank == 2 .and. soilOxygen%rank == 2) then
    !     soilOxygen%data2(soilOxygen%lbnd(1):soilOxygen%ubnd(1), &
    !       soilOxygen%lbnd(2):soilOxygen%ubnd(2)) &
    !       = -waterOdu%data2(waterOdu%lbnd(1):waterOdu%ubnd(1), &
    !       waterOdu%lbnd(2):waterOdu%ubnd(2))
    !   elseif (waterOdu%rank == 2 .and. soilOxygen%rank == 1) then
    !     soilOxygen%data1(soilOxygen%lbnd(1):soilOxygen%ubnd(1)) &
    !       = -waterOdu%data2(waterOdu%lbnd(1):waterOdu%ubnd(1), &
    !       waterOdu%lbnd(2))
    !   elseif (waterOdu%rank == 1 .and. soilOxygen%rank == 1) then
    !       soilOxygen%data1(soilOxygen%lbnd(1):soilOxygen%ubnd(1)) &
    !       = -waterOdu%data1(waterOdu%lbnd(1):waterOdu%ubnd(1))
    !   endif
    !   if (waterOdu%rank == 3 .and. soilOdu%rank == 2) then
    !     soilOdu%data2(soilOdu%lbnd(1):soilOdu%ubnd(1), &
    !       soilOdu%lbnd(2):soilOdu%ubnd(2)) &
    !       = waterOdu%data3(waterOdu%lbnd(1):waterOdu%ubnd(1), &
    !       waterOdu%lbnd(2):waterOdu%ubnd(2),waterOdu%lbnd(1))
    !   elseif (waterOdu%rank == 2 .and. soilOdu%rank == 2) then
    !     soilOdu%data2(soilOdu%lbnd(1):soilOdu%ubnd(1), &
    !       soilOdu%lbnd(2):soilOdu%ubnd(2)) &
    !       = waterOdu%data2(waterOdu%lbnd(1):waterOdu%ubnd(1), &
    !       waterOdu%lbnd(2):waterOdu%ubnd(2))
    !   elseif (waterOdu%rank == 2 .and. soilOdu%rank == 1) then
    !     soilOdu%data1(soilOdu%lbnd(1):soilOdu%ubnd(1)) &
    !       = waterOdu%data2(waterOdu%lbnd(1):waterOdu%ubnd(1), &
    !       waterOdu%lbnd(2))
    !   elseif (waterOdu%rank == 1 .and. soilOdu%rank == 1) then
    !       soilOdu%data1(soilOdu%lbnd(1):soilOdu%ubnd(1)) &
    !       = waterOdu%data1(waterOdu%lbnd(1):waterOdu%ubnd(1))
    !   endif
    !   if (soilOdu%rank == 2) then
    !     where(soilOdu%data2(soilOdu%lbnd(1):soilOdu%ubnd(1), &
    !       soilOdu%lbnd(2):soilOdu%ubnd(2)) < 0)
    !       soilOdu%data2(soilOdu%lbnd(1):soilOdu%ubnd(1), &
    !         soilOdu%lbnd(2):soilOdu%ubnd(2)) = 0.0
    !     endwhere
    !   elseif (soilOdu%rank == 1) then
    !     where(soilOdu%data1(soilOdu%lbnd(1):soilOdu%ubnd(1)) < 0)
    !       soilOdu%data1(soilOdu%lbnd(1):soilOdu%ubnd(1)) = 0.0
    !     endwhere
    !   endif
    !   if (soilOxygen%rank == 2) then
    !     where(soilOxygen%data2(soilOxygen%lbnd(1):soilOxygen%ubnd(1), &
    !       soilOxygen%lbnd(2):soilOxygen%ubnd(2)) < 0)
    !       soilOxygen%data2(soilOxygen%lbnd(1):soilOxygen%ubnd(1), &
    !         soilOxygen%lbnd(2):soilOxygen%ubnd(2)) = 0.0
    !     endwhere
    !   elseif (soilOxygen%rank == 1) then
    !     where(soilOxygen%data1(soilOxygen%lbnd(1):soilOxygen%ubnd(1)) < 0)
    !       soilOxygen%data1(soilOxygen%lbnd(1):soilOxygen%ubnd(1)) = 0.0
    !     endwhere
    !   endif
    ! endif

    ! if (soilOxygen%rank > 0) deallocate(soilOxygen%lbnd, soilOxygen%ubnd)
    ! if (waterOxygen%rank > 0) deallocate(waterOxygen%lbnd, waterOxygen%ubnd)
    ! if (soilOdu%rank > 0) deallocate(soilOdu%lbnd, soilOdu%ubnd)
    ! if (waterOdu%rank > 0) deallocate(waterOdu%lbnd, waterOdu%ubnd)

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
        exportState, (/'practical_salinity_at_soil_surface'/),  &
        verbose=verbose, rc=localrc)
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

      if (rank==3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr3, &
          exclusiveLBound=lbnd(1:3), &
          exclusiveUbound=ubnd(1:3), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, &
          exclusiveLBound=lbnd(1:2), &
          exclusiveUbound=ubnd(1:2), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, &
          exclusiveLBound=lbnd(1:1), exclusiveUbound=ubnd(1:1), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      else
        localrc = ESMF_RC_NOT_IMPL
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

      odurc = ESMF_SUCCESS

      if (rank==3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr32, &
          exclusiveLBound=lbnd(1:3), &
          exclusiveUbound=ubnd(1:3), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr22, &
          exclusiveLBound=lbnd(1:2), &
          exclusiveUbound=ubnd(1:2), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr12, &
          exclusiveLBound=lbnd(1:1), &
          exclusiveUbound=ubnd(1:1), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      itemSearch='dissolved_oxygen_at_soil_surface', verbose=verbose, owner=trim(name), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      ! this is always true for OMexDia

      call ESMF_FieldGet(fieldList(1), rank=exportRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (exportRank==2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (exportRank==1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      else

        localrc = ESMF_RC_NOT_IMPL
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
      else
        write(message,'(A,A,I)') trim(name)//' '//trim(includeList(1)), &
          ' cannot have rank ',rank
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        localrc = ESMF_RC_NOT_IMPL
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
      elseif (associated(farrayPtr1)) then
        farrayPtr1(RANGE1D)  = farrayPtr1(RANGE1D)
        farrayPtr12(RANGE1D) = farrayPtr12(RANGE1D)
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    elseif (odurc == ESMF_SUCCESS) then
      ! importState provides only odu, split negative part to oxy

      if (associated(farrayPtr32)) then
        farrayPtr2(RANGE2D)  =  max(0.0, -farrayPtr32(RANGE2D,lbnd(3)))
        farrayPtr22(RANGE2D) =  max(0.0, farrayPtr32(RANGE2D,lbnd(3)))
      elseif (associated(farrayPtr22)) then
        farrayPtr1(RANGE1D)  = max(0.0, -farrayPtr22(RANGE1D,lbnd(2)))
        farrayPtr12(RANGE1D) =  max(0.0, -farrayPtr22(RANGE1D,lbnd(2)))
      elseif (associated(farrayPtr12)) then
        farrayPtr1(RANGE1D)  = max(0.0, -farrayPtr12(RANGE1D))
        farrayPtr12(RANGE1D) =  max(0.0, -farrayPtr12(RANGE1D))
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

    elseif (oxyrc == ESMF_SUCCESS) then
      ! importState provides only oxy, split negative part to odu

      if (associated(farrayPtr32)) then
        farrayPtr2(RANGE2D)  =  max(0.0, -farrayPtr32(RANGE2D,lbnd(3)))
        farrayPtr22(RANGE2D) =  max(0.0, farrayPtr32(RANGE2D,lbnd(3)))
      elseif (associated(farrayPtr22)) then
        farrayPtr1(RANGE1D)  = max(0.0, -farrayPtr22(RANGE1D,lbnd(2)))
        farrayPtr12(RANGE1D) =  max(0.0, -farrayPtr22(RANGE1D,lbnd(2)))
      elseif (associated(farrayPtr12)) then
        farrayPtr1(RANGE1D)  = max(0.0, -farrayPtr12(RANGE1D))
        farrayPtr12(RANGE1D) =  max(0.0, -farrayPtr12(RANGE1D))
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
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

          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, &
            exclusiveLBound=lbnd(1:1), exclusiveUbound=ubnd(1:1), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldGet(field, farrayPtr=farrayPtr12, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          farrayPtr12(RANGE1D) = farrayPtr1(RANGE1D)
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
      enddo
    endif

    !> Clean up oxygen-related fields
    if (allocated(fieldList)) deallocate(fieldList)
    if (associated(includeList)) deallocate(includeList)
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

    do while (fieldCount > 0)

      call ESMF_FieldGet(fieldList(1), rank=rank, name=fieldName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank==1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detN1, &
          exclusiveLBound=lbnd(1:1), &
          exclusiveUBound=ubnd(1:1), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detN2, &
          exclusiveLBound=lbnd(1:2), &
          exclusiveUBound=ubnd(1:2), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank==3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detN3, &
          exclusiveLBound=lbnd(1:3), &
          exclusiveUBound=ubnd(1:3), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      else
        write(message,'(A,A,I1)') trim(name)//' '//trim(fieldName), &
          ' cannot have rank ',rank
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        exit
      endif

      write(message,'(A)') trim(name)//' with field '//trim(fieldName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      i = index(fieldName, '_in_water')
      if (i>0) then
        fieldName = fieldName(1:i)//'z_velocity_in_water'
      else
        i = index(fieldName,'_at_soil_surface')
        if (i>0) then
          fieldName = fieldName(1:i)//'z_velocity_at_soil_surface'
        endif
      endif

      if (i<1) then
        write(message,'(A)') trim(name)//' misinterpreted '//trim(fieldName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
        itemSearch=trim(fieldName), verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldCount > 0) then
        if (rank==1) then
          call ESMF_FieldGet(fieldList(1), farrayPtr=vdetN1, &
            exclusiveLBound=lbnd(1:1), &
            exclusiveUBound=ubnd(1:1), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        elseif (rank==2) then
          call ESMF_FieldGet(fieldList(1), farrayPtr=vdetN2, &
            exclusiveLBound=lbnd(1:2), &
            exclusiveUBound=ubnd(1:2), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        elseif (rank==3) then
          call ESMF_FieldGet(fieldList(1), farrayPtr=vdetN3, &
            exclusiveLBound=lbnd(1:3), &
            exclusiveUBound=ubnd(1:3), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
      else
        write(message,'(A)') trim(name)//' could not find '//trim(fieldName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
      exit
    enddo ! detritus fieldCount

    ! Allocate CN ratios depending on exportRank
    if (exportRank == 1) then
      allocate(CN_det1(RANGE1D))
      CN_det1(RANGE1D) = 106.0d0/16.0d0
    elseif (exportRank == 2) then
      !write(0,*) lbnd, ubnd
      allocate(CN_det2(RANGE2D))
      CN_det2(RANGE2D) = 106.0d0/16.0d0
    else
      write(message,'(A,I1)') trim(name)//' cannot have rank ',exportRank
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      localrc = ESMF_RC_NOT_IMPL
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
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

      if (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detC1, &
          exclusiveLBound=lbnd(1:1), &
          exclusiveUBound=ubnd(1:1), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detC2, &
          exclusiveLBound=lbnd(1:2), &
          exclusiveUBound=ubnd(1:2), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (rank == 3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=detC3, &
          exclusiveLBound=lbnd(1:3), &
          exclusiveUBound=ubnd(1:3), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    ! Determine detritus C:N ratio (default is Redfield)
    write(message,'(A)') trim(name)//' uses variable C:N ratio'
    if (associated(detN3) .and. associated(detC3) .and. exportRank == 2) then
      CN_det2(RANGE2D) = detC3(RANGE2D,lbnd(3)) / (1E-5 + detN3(RANGE2D,lbnd(3)))
    elseif (associated(detN2) .and. associated(detC2) .and. exportRank == 2) then
      CN_det2(RANGE2D) = detC2(RANGE2D) / (1E-5 + detN2(RANGE2D))
    elseif (associated(detN2) .and. associated(detC2) .and. exportRank == 1) then
      CN_det1(RANGE1D) = detC2(RANGE1D,lbnd(2)) / (1E-5 + detN2(RANGE1D,lbnd(2)))
    elseif (associated(detN1) .and. associated(detC1) .and. exportRank == 1) then
      CN_det1(RANGE1D) = detC1(RANGE1D) / (1E-5 + detN1(RANGE1D))
    else
      write(message,'(A)') trim(name)//' uses constant Redfield C:N ratio 106:16'
    endif
    if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    ! From two endmembers NC_sdet and NC_ldet for semilabile and labile
    ! material, determine partitioning
    if (associated(CN_det2)) then

      allocate(fac_ldet2(RANGE2D), frac_ldet2(RANGE2D), stat=localrc)
      allocate(fac_sdet2(RANGE2D), frac_sdet2(RANGE2D), stat=localrc)
      allocate(fac_env2(RANGE2D), stat=localrc)

      fac_ldet2(RANGE2D)  = (1.0d0-NC_sdet*CN_det2(RANGE2D))/(NC_ldet-NC_sdet)
      frac_ldet2(RANGE2D) = fac_ldet2(RANGE2D) * NC_ldet

      where (fac_ldet2(RANGE2D) .gt. CN_det2(RANGE2D))
        fac_ldet2(RANGE2D)  = CN_det2(RANGE2D)
        frac_ldet2(RANGE2D) = 1.0d0
      endwhere

      where (fac_ldet2(RANGE2D) .lt. 0.0d0)
        fac_ldet2(RANGE2D)  = 0.0d0
        frac_ldet2(RANGE2D) = 0.0d0
      endwhere

      fac_sdet2(RANGE2D) = CN_det2(RANGE2D) - fac_ldet2(RANGE2D)
      frac_sdet2(RANGE2D) = 1.0d0 - frac_ldet2(RANGE2D)
      fac_env2(RANGE2D) = 1.0d0

    elseif (associated(CN_det1)) then

      allocate(fac_ldet1(RANGE1D), frac_ldet1(RANGE1D), stat=localrc)
      allocate(fac_sdet1(RANGE1D), frac_sdet1(RANGE1D), stat=localrc)
      allocate(fac_env1(RANGE1D), stat=localrc)

      fac_ldet1(RANGE1D)  = (1.0d0-NC_sdet*CN_det1(RANGE1D))/(NC_ldet-NC_sdet)
      frac_ldet1(RANGE1D) = fac_ldet1(RANGE1D) * NC_ldet

      where (fac_ldet1(RANGE1D) .gt. CN_det1(RANGE1D))
        fac_ldet1(RANGE1D)  = CN_det1(RANGE1D)
        frac_ldet1(RANGE1D) = 1.0d0
      endwhere

      where (fac_ldet1(RANGE1D) .lt. 0.0d0)
        fac_ldet1(RANGE1D)  = 0.0d0
        frac_ldet1(RANGE1D) = 0.0d0
      endwhere

      fac_sdet1(RANGE1D) = CN_det1(RANGE1D) - fac_ldet1(RANGE1D)
      frac_sdet1(RANGE1D) = 1.0d0 - frac_ldet1(RANGE1D)
      fac_env1(RANGE1D) = 1.0d0

    endif

    deallocate(includeList, stat=localrc)
    allocate(includeList(1), stat=localrc)
    includeList(1) = 'water_depth_at_soil_surface'

    ! get depth from exportState, where the physical model has put its data
    ! @todo get depth information from gotm 'water_depth_at_soil_surface'
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
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    if (fieldCount > 0 .and. half_sedimentation_depth .gt. 1E-3) then
      ! reduce sedimentation due to depth, assuming higher wave erosion in shallow areas
      if (associated(depth1) .and. associated(fac_env1)) then
        fac_env1(RANGE1D) = fac_env1(RANGE1D) * depth1(RANGE1D)**2/(depth1(RANGE1D)**2 + half_sedimentation_depth**2)
      elseif (associated(depth2) .and. associated(fac_env2)) then
        fac_env2(RANGE2D) = fac_env2(RANGE2D) * depth2(RANGE2D)**2/(depth2(RANGE2D)**2 + half_sedimentation_depth**2)
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    deallocate(includeList, stat=localrc)
    allocate(includeList(2), stat=localrc)
    includeList(1) = 'turbulent_kinetic_energy_at_soil_surface'
    includeList(2) = 'turbulent_diffusivity_of_momentum_at_soil_surface'
    !> @todo what about _in_water, is diffusivity (from GOTM) correct?

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
          fac_env1(RANGE1D) = fac_env1(RANGE1D) &
            * half_sedimentation_tke/(farrayPtr1(RANGE1D) + half_sedimentation_tke)
        endif
      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (half_sedimentation_tke .lt. 9E9) then
          fac_env2(RANGE2D) = fac_env2(RANGE2D) &
            * half_sedimentation_tke/(farrayPtr2(RANGE2D) + half_sedimentation_tke)
        endif
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    else
      write(message,'(A)') trim(name)//' does not reduce sedimentation due to turbulence'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    ! ensure minimum sedimentation
    if (associated(fac_env2)) then
      fac_env2(RANGE2D) = fac_env2(RANGE2D) + sinking_factor_min/sinking_factor
    elseif  (associated(fac_env1)) then
      fac_env1(RANGE1D) = fac_env1(RANGE1D) + sinking_factor_min/sinking_factor
    else
      localrc = ESMF_RC_NOT_IMPL
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    ! reduce sedimentation due to detritus-C (assuming higher detN in shallow areas)
    !> @todo this does not work from GOTM yet
    if (associated(detC3) .and. associated(fac_env2)) then
      if (critical_detritus .gt. 1E-3 .and. critical_detritus .lt. 9E9) then
        fac_env2(RANGE2D) = fac_env2(RANGE2D) &
          * 1.0d0/(1.0d0 + (detC3(RANGE2D,lbnd(3))/critical_detritus)**4)
      end if
    elseif (associated(detC2) .and. associated(fac_env1)) then
      if (critical_detritus .gt. 1E-3 .and. critical_detritus .lt. 9E9) then
        fac_env1(RANGE1D) = fac_env1(RANGE1D) &
          * 1.0d0/(1.0d0 + (detC2(RANGE1D,lbnd(2))/critical_detritus)**4)
      end if
    elseif (associated(detC2) .and. associated(fac_env2)) then
      if (critical_detritus .gt. 1E-3 .and. critical_detritus .lt. 9E9) then
        fac_env2(RANGE2D) = fac_env2(RANGE2D) &
          * 1.0d0/(1.0d0 + (detC2(RANGE2D)/critical_detritus)**4)
      end if
    elseif (associated(detC1) .and. associated(fac_env1)) then
      if (critical_detritus .gt. 1E-3 .and. critical_detritus .lt. 9E9) then
        fac_env1(RANGE1D) = fac_env1(RANGE1D) &
          * 1.0d0/(1.0d0 + (detC1(RANGE1D)/critical_detritus)**4)
      end if
    else
      write(message,'(A)') trim(name)//' does not reduce sedimentation due to detritus carbon'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
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
      call ESMF_FieldGet(fieldList(1), rank=exportRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (exportRank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (associated(detN2)) then
          farrayPtr1 = fac_ldet1(RANGE1D) * convertN*detN2(RANGE1D,lbnd(2))
        elseif (associated(detN1)) then
          farrayPtr1 = fac_ldet1(RANGE1D) * convertN*detN1(RANGE1D)
        endif

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (associated(detN2)) then
            farrayPtr1 = sinking_factor * fac_env1(RANGE1D) * detN2(RANGE1D,lbnd(2))
          elseif (associated(detN1)) then
            farrayPtr1 = sinking_factor * fac_env1(RANGE1D) * detN1(RANGE1D)
          endif

        endif

      elseif (exportRank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (associated(detN3)) then
          farrayPtr2 = fac_ldet2(RANGE2D) * convertN*detN3(RANGE2D,lbnd(3))
        elseif (associated(detN2)) then
          farrayPtr2 = fac_ldet2(RANGE2D) * convertN*detN2(RANGE2D)
        endif

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (associated(detN3)) then
            farrayPtr2 = sinking_factor * fac_env2(RANGE2D) * detN3(RANGE2D,lbnd(3))
          elseif (associated(detN2)) then
            farrayPtr2 = sinking_factor * fac_env2(RANGE2D) * detN2(RANGE2D)
          endif

        endif
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    includeList(1) = 'detritus_semilabile_carbon_at_soil_surface'
    includeList(2) = 'detritus_semilabile_carbon_z_velocity_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, fieldCount=fieldCount, &
      include=includeList, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      if (exportRank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (associated(detN2)) then
          farrayPtr1 = fac_sdet1(RANGE1D) * convertN*detN2(RANGE1D,lbnd(2))
        elseif (associated(detN1)) then
          farrayPtr1 = fac_sdet1(RANGE1D) * convertN*detN1(RANGE1D)
        endif

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (associated(detN2)) then
            farrayPtr1 = sinking_factor * fac_env1(RANGE1D) * detN2(RANGE1D,lbnd(2))
          elseif (associated(detN1)) then
            farrayPtr1 = sinking_factor * fac_env1(RANGE1D) * detN1(RANGE1D)
          endif

        endif

      elseif (exportRank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (associated(detN3)) then
          farrayPtr2 = fac_sdet2(RANGE2D) * convertN*detN3(RANGE2D,lbnd(3))
        elseif (associated(detN2)) then
          farrayPtr2 = fac_sdet2(RANGE2D) * convertN*detN2(RANGE2D)
        endif

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (associated(detN3)) then
            farrayPtr2 = sinking_factor * fac_env2(RANGE2D) * detN3(RANGE2D,lbnd(3))
          elseif (associated(detN2)) then
            farrayPtr2 = sinking_factor * fac_env2(RANGE2D) * detN2(RANGE2D)
          endif
        endif
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
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
      call ESMF_FieldGet(fieldList(1), rank=exportRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (exportRank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (associated(detN2)) then
          farrayPtr1 = frac_ldet1(RANGE1D) * convertN*detN2(RANGE1D,lbnd(2))
        elseif (associated(detN1)) then
          farrayPtr1 = frac_ldet1(RANGE1D) * convertN*detN1(RANGE1D)
        endif

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (associated(detN2)) then
            farrayPtr1 = sinking_factor * fac_env1(RANGE1D) * detN2(RANGE1D,lbnd(2))
          elseif (associated(detN1)) then
            farrayPtr1 = sinking_factor * fac_env1(RANGE1D) * detN1(RANGE1D)
          endif

        endif

      elseif (exportRank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (associated(detN3)) then
          farrayPtr2 = frac_ldet2(RANGE2D) * convertN*detN3(RANGE2D,lbnd(3))
        elseif (associated(detN2)) then
          farrayPtr2 = frac_ldet2(RANGE2D) * convertN*detN2(RANGE2D)
        endif

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (associated(detN3)) then
            farrayPtr2 = sinking_factor * fac_env2(RANGE2D) * detN3(RANGE2D,lbnd(3))
          elseif (associated(detN2)) then
            farrayPtr2 = sinking_factor * fac_env2(RANGE2D) * detN2(RANGE2D)
          endif
        endif
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
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

        if (associated(detN2)) then
          farrayPtr1 = frac_sdet1(RANGE1D) * convertN*detN2(RANGE1D,lbnd(2))
        elseif (associated(detN1)) then
          farrayPtr1 = frac_sdet1(RANGE1D) * convertN*detN1(RANGE1D)
        endif

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (associated(detN2)) then
            farrayPtr1 = sinking_factor * fac_env1(RANGE1D) * detN2(RANGE1D,lbnd(2))
          elseif (associated(detN1)) then
            farrayPtr1 = sinking_factor * fac_env1(RANGE1D) * detN1(RANGE1D)
          endif

        endif

      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (associated(detN3)) then
          farrayPtr2 = frac_sdet2(RANGE2D) * convertN*detN3(RANGE2D,lbnd(3))
        elseif (associated(detN2)) then
          farrayPtr2 = frac_sdet2(RANGE2D) * convertN*detN2(RANGE2D)
        endif

        if (fieldCount > 1) then ! for velocity field

          call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (associated(detN3)) then
            farrayPtr2 = sinking_factor * fac_env2(RANGE2D) *detN3(RANGE2D,lbnd(3))
          elseif (associated(detN2)) then
            farrayPtr2 = sinking_factor * fac_env2(RANGE2D) *detN2(RANGE2D)
          endif
        endif
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
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

      call ESMF_FieldGet(fieldList(1), rank=exportRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Assume Redfield at first, later overwrite with actual data,
      !> if this is found in import state
      if (exportRank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (associated(detN2)) then
          farrayPtr1 = 1.0d0/16.0d0 * convertN*detN2(RANGE1D,lbnd(2))
        elseif (associated(detN1)) then
          farrayPtr1 = 1.0d0/16.0d0 * convertN*detN1(RANGE1D)
        endif

      elseif (exportRank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (associated(detN3)) then
          farrayPtr2 = 1.0d0/16.0d0 * convertN*detN3(RANGE2D,lbnd(3))
        elseif (associated(detN2)) then
          farrayPtr2 = 1.0d0/16.0d0 * convertN*detN2(RANGE2D)
        endif
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
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

          if (associated(detP2)) then
            farrayPtr1 = detP2(RANGE1D,lbnd(2))
          elseif (associated(detP1)) then
            farrayPtr1 = detP1(RANGE1D)
          endif

        elseif (rank == 3) then
          call ESMF_FieldGet(fieldList(1), farrayPtr=detP3, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (associated(detP3)) then
            farrayPtr2 = detP3(RANGE2D,lbnd(3))
          elseif (associated(detP2)) then
            farrayPtr2 = detP2(RANGE2D)
          endif
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
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

      if (exportRank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (associated(detN2)) then
          farrayPtr1 = sinking_factor * fac_env1(RANGE1D) *detN2(RANGE1D,lbnd(2))
        elseif (associated(detN1)) then
          farrayPtr1 = sinking_factor * fac_env1(RANGE1D) *detN1(RANGE1D)
        endif

      elseif (exportRank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (associated(detN3)) then
          farrayPtr2 = sinking_factor * fac_env2(RANGE2D) *detN3(RANGE2D,lbnd(3))
        elseif (associated(detN2)) then
          farrayPtr2 = sinking_factor * fac_env2(RANGE2D) *detN2(RANGE2D)
        endif
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      includeList(1) = 'detP_z_velocity_in_water'
      includeList(2) = 'Detritus_Phosphorus_detP_z_velocity_in_water'

      call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
        include=includeList, verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldCount > 0) then

        call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd(1:rank), &
          exclusiveUBound=ubnd(1:rank), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (rank == 1) call ESMF_FieldGet(fieldList(1), farrayPtr=detP1, rc=localrc)
        if (rank == 2) call ESMF_FieldGet(fieldList(1), farrayPtr=detP2, rc=localrc)
        if (rank == 3) call ESMF_FieldGet(fieldList(1), farrayPtr=detP3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (exportRank == 2) then

          if (associated(detP3)) then
            farrayPtr2 = sinking_factor * fac_env2(RANGE2D) * detP3(RANGE2D,lbnd(3))
          elseif (associated(detP2)) then
            farrayPtr2 = sinking_factor * fac_env2(RANGE2D) * detP2(RANGE2D)
          endif

        elseif (exportRank == 1) then

          if (associated(detP2)) then
            farrayPtr1 = sinking_factor * fac_env1(RANGE1D) * detP2(RANGE1D,lbnd(2))
          elseif (associated(detP1)) then
            farrayPtr1 = sinking_factor * fac_env1(RANGE1D) * detP1(RANGE1D)
          endif
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
      endif
    endif

    ! Dissolved inorganic matter, i.e. nitrate, ammonium or DIN
    call MOSSCO_StateGet(importState, fieldList, itemSearch='nitrate_in_water', &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd(1:rank), &
        exclusiveUBound=ubnd(1:rank), rc=localrc)
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
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      write(message,'(A)') trim(name)//' obtains NO3 from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      hasNitrate = .true.
    else
      hasNitrate = .false.
    endif

    deallocate(includeList)
    allocate(includeList(3))
    includeList(1) = 'nutrients_in_water'
    includeList(2) = 'DIN_in_water'
    includeList(3) = 'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water'
    call MOSSCO_StateGet(importState, fieldList, include=includeList, &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd(1:rank), &
        exclusiveUBound=ubnd(1:rank), rc=localrc)
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
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      write(message,'(A)') trim(name)//' obtains DIN from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      hasDIN = .true.
    else
      hasDIN = .false.
    endif

    deallocate(includeList)
    allocate(includeList(2))
    includeList(1) = 'ammonium_in_water'
    includeList(2) = 'dissolved_ammonium_nh3_in_water'
    call MOSSCO_StateGet(importState, fieldList, include=includeList, &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd(1:rank), &
        exclusiveUBound=ubnd(1:rank), rc=localrc)
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
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      write(message,'(A)') trim(name)//' obtains NH4 from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      hasAmmonium = .true.
    else
      hasAmmonium = .false.
    endif


    ! Get mandatory ammonium field from export State and save either
    ! the pelagic ammonium (if present) or ammonium derived from combinations
    ! of NO3 and DIN

    call MOSSCO_StateGet(exportState, fieldList, &
      itemSearch='mole_concentration_of_ammonium_at_soil_surface', &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount /= 1) then
      write(message,'(A,I1)') 'Expected exactly one complete field for mole_concentration_of_ammonium_at_soil_surface, received ',fieldCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_BAD
      return
    endif

    call ESMF_FieldGet(fieldList(1), rank=exportRank, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (exportRank == 1) then

      call ESMF_FieldGet(fieldList(1), localde=0, farrayPtr=farrayPtr1, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (hasAmmonium .and. associated(amm2)) then
        farrayPtr1 = convertN*amm2(RANGE1D,lbnd(2))
      elseif (hasAmmonium .and. associated(amm1)) then
        farrayPtr1 = convertN*amm1(RANGE1D)
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

    elseif (exportRank == 2) then

      call ESMF_FieldGet(fieldList(1), localde=0, farrayPtr=farrayPtr2, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (hasAmmonium .and. associated(amm3)) then
        farrayPtr2 = convertN*amm3(RANGE2D,lbnd(3))
      elseif (hasAmmonium .and. associated(amm2)) then
        farrayPtr2 = convertN*amm2(RANGE2D)
      elseif (hasDIN .and. hasNitrate) then
        if (associated(nit3) .and. associated(din3)) then
          farrayPtr2 = convertN*(din3(RANGE2D,lbnd(3)) - nit3(RANGE2D,lbnd(3)))
        elseif (associated(nit2) .and. associated(din2)) then
          farrayPtr2 = convertN*(din2(RANGE2D) - nit2(RANGE2D))
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
        write(message,'(A)') trim(name)//' calculates NH4 as DIN - NO3'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasDIN) then
        if (associated (din3)) then
          farrayPtr2 = convertN*0.5d0 * din3(RANGE2D,lbnd(3))
        elseif (associated (din2)) then
          farrayPtr2 = convertN*0.5d0 * din2(RANGE2D)
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
        write(message,'(A)') trim(name)//' calculates NH4 as 0.5 * DIN'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasNitrate) then
        if (associated (nit3)) then
          farrayPtr2 = convertN*nit3(RANGE2D,lbnd(3))
        elseif (associated (nit2)) then
          farrayPtr2 = convertN*nit2(RANGE2D)
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
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

    call ESMF_FieldGet(field, rank=exportRank, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (exportRank == 1) then

      call ESMF_FieldGet(fieldList(1), localde=0, farrayPtr=farrayPtr1, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (hasNitrate) then
        if (associated(nit2)) then
          farrayPtr1 = convertN*nit2(RANGE1D,lbnd(2))
        elseif (associated(nit1)) then
          farrayPtr1 = convertN*nit1(RANGE1D)
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
      elseif (hasAmmonium .and. hasDIN) then
        if (associated(din2) .and. associated(amm2)) then
          farrayPtr1 = convertN*(din2(RANGE1D,lbnd(2)) &
            - amm2(RANGE1D,lbnd(2)))
        elseif (associated(din1) .and. associated(amm1)) then
          farrayPtr1 = convertN*(din1(RANGE1D) - amm1(RANGE1D))
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif

        write(message,'(A)') trim(name)//' calculates NO3 = DIN - NH4'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasDIN) then
        if (associated(din2)) then
          farrayPtr1 = convertN*0.5d0 * din2(RANGE1D,lbnd(2))
        elseif (associated(din1)) then
          farrayPtr1 = convertN*0.5d0 * din1(RANGE1D)
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
        write(message,'(A)') trim(name)//' calculates NO3 = 0.5 * DIN'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasAmmonium) then
        if (associated(amm2)) then
          farrayPtr1 = convertN*amm2(RANGE1D,lbnd(2))
        elseif (associated(amm1)) then
          farrayPtr1 = convertN*amm1(RANGE1D)
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
        write(message,'(A)') trim(name)//' calculates NO3 equal to NH4'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        write(message,'(A)') trim(name)//' did not receive any information on nitrogen'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_NOT_FOUND
        return
      end if

    elseif (exportRank == 2) then

      if (hasNitrate) then
        if (associated(nit3)) then
          farrayPtr2 = convertN*nit3(RANGE2D,lbnd(3))
        elseif (associated(nit2)) then
          farrayPtr2 = convertN*nit2(RANGE2D)
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
      elseif (hasAmmonium .and. hasDIN) then
        if (associated(din3).and. associated(amm3)) then
          farrayPtr2 = convertN*(din3(RANGE2D,lbnd(3)) &
           - amm3(RANGE2D,lbnd(3)))
        elseif (associated(din2).and. associated(amm2)) then
          farrayPtr2 = convertN*(din2(RANGE2D) &
           - amm2(RANGE2D))
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
        write(message,'(A)') trim(name)//' calculates NO3 = DIN - NH4'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasDIN) then
        if (associated(din3)) then
          farrayPtr2 = convertN*0.5d0 * din3(RANGE2D,lbnd(3))
        elseif (associated(din2)) then
          farrayPtr2 = convertN*0.5d0 * din2(RANGE2D)
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
        write(message,'(A)') trim(name)//' calculates NO3 = 0.5 * DIN'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      elseif (hasAmmonium) then
        if (associated(amm3)) then
          farrayPtr2 = convertN*amm3(RANGE2D,lbnd(3))
        elseif (associated(amm2)) then
          farrayPtr2 = convertN*amm2(RANGE2D)
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
        write(message,'(A)') trim(name)//' calculates NO3 equal to NH4'
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        write(message,'(A)') trim(name)//' did not receive any information on nitrogen'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_NOT_FOUND
        return
      end if
    else
      localrc = ESMF_RC_NOT_IMPL
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    !> check for DIP, if present, take as is, if not calculate it N-based
    !> with Redfield Stoichiometry

    deallocate(includeList)
    allocate(includeList(3))
    includeList(1) = 'DIP_in_water'
    includeList(2) = 'phosphate_in_water'
    includeList(3) = 'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water'
    call MOSSCO_StateGet(importState, fieldList, include=includeList, &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd(1:rank), &
        exclusiveUBound=ubnd(1:rank), rc=localrc)
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
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      write(message,'(A)') trim(name)//' obtains DIP from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      hasDIP = .true.
    else
      hasDIP = .false.

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
      else
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
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
    else
      localrc = ESMF_RC_NOT_IMPL
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, rc=localrc)
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

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, rc=localrc)
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

    integer(ESMF_KIND_I4)               :: localrc, fieldCount, i
    type(ESMF_Field), allocatable       :: fieldList3(:), fieldList2(:)
    logical                             :: verbose_
    character(len=ESMF_MAXSTR), pointer :: includeList(:)

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(verbose)) then
      verbose_ = verbose
    else
      verbose_ = .false.
    endif
    if (present(kwe)) verbose_ = verbose_

    allocate(includeList(size(importFieldList)))
    do i=1,ubound(importFieldList,1)
      includeList(i) = trim(importFieldList(i))
    enddo

    call MOSSCO_StateGet(importState, fieldList=fieldList3, &
      include=includeList, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), &
      fieldCount=fieldCount, verbose=verbose_, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (associated(includeList)) deallocate(includeList)

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
      return
    endif

    call MOSSCO_FieldReduce(fieldList3(1), fieldList2(1), indexmask=(/1/), &
      owner='pelagic_soil_connector', rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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

    integer(ESMF_KIND_I4)               :: localrc, fieldCount, i
    type(ESMF_Field), allocatable       :: fieldList2(:), fieldList1(:)
    logical                             :: verbose_
    character(len=ESMF_MAXSTR), pointer :: includeList(:)

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(verbose)) then
      verbose_ = verbose
    else
      verbose_ = .false.
    endif
    if (present(kwe)) verbose_ = verbose_

    allocate(includeList(size(importFieldList)))
    do i=1,ubound(importFieldList,1)
      includeList(i) = trim(importFieldList(i))
    enddo

    call MOSSCO_StateGet(importState, fieldList=fieldList2, &
      include=includeList, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), &
      fieldCount=fieldCount, verbose=verbose_, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (associated(includeList)) deallocate(includeList)

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
