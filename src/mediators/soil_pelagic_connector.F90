!> @brief Implementation of an ESMF soil-pelagic coupling
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
#define ESMF_FILENAME "soil_pelagic_connector.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#define RANGE2D lbnd(1):ubnd(1),lbnd(2):ubnd(2)
#define RANGE3D lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)

module soil_pelagic_connector

  use esmf
  use mossco_state
  use mossco_field
  use mossco_component

  implicit none

  private

  real(ESMF_KIND_R8) :: dinflux_const=0.0
  real(ESMF_KIND_R8) :: dipflux_const=-1.
  real(ESMF_KIND_R8) :: NC_ldet=0.20d0
  real(ESMF_KIND_R8) :: NC_sdet=0.04d0
  real(ESMF_KIND_R8) :: convertP=1.0d0
  real(ESMF_KIND_R8) :: convertN=1.0d0

type spVariable
  type(ESMF_Field)            :: field
  real(ESMF_KIND_R8), pointer :: data1(:) => null()
  real(ESMF_KIND_R8), pointer :: data2(:,:) => null()
  !real(ESMF_KIND_R8), pointer :: data3(:,:,:) => null()
  integer(ESMF_KIND_I4)       :: rank = 0
  character(len=ESMF_MAXSTR)  :: unit = ''
  integer(ESMF_KIND_I4)       :: lbnd(2) = 1, ubnd(2) = 1
end type spVariable

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

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    type(ESMF_Field)     :: newfield
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_State)      :: paramState
    type(ESMF_Time)       :: currTime
    logical               :: isPresent
    integer               :: nmlunit=127, localrc

    namelist /soil_pelagic_connector/ dinflux_const,dipflux_const,NC_ldet,NC_sdet,convertN,convertP

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, externalClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !read namelist
    inquire(file=trim(name)//'.nml', exist=isPresent)

    if (isPresent) then
      open(nmlunit,file='soil_pelagic_connector.nml',action='read',status='old')
      read(nmlunit,soil_pelagic_connector)
      close(nmlunit)
    endif

    if (dipflux_const < 0.0) dipflux_const=dinflux_const/16.0d0

    paramState=ESMF_StateCreate(name=trim(name)//'Parameters', rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(paramState, trim(name)//'::dipflux_const', dipflux_const, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(paramState, trim(name)//'::dinflux_const', dinflux_const, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> @todo re-enable once it does not contain itself anymore
    !call ESMF_StateAdd(importState, (/paramState/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"

  subroutine Run(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime, stopTime
    integer(ESMF_KIND_I4)       :: localrc, fieldCount, i
    integer                     :: myrank
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    type(ESMF_Field)            :: field
    integer(ESMF_KIND_R8)       :: advanceCount
    !> @todo read NC_ldet dynamically from fabm model info?  This would not comply with our aim to separate fabm/esmf
    integer(ESMF_KIND_I4)       :: rank, ubnd(2), lbnd(2), itemCount
    logical                     :: verbose
    real(ESMF_KIND_R8), pointer :: farrayPtr2(:,:) => null()
    real(ESMF_KIND_R8), pointer :: farrayPtr1(:) => null()
    logical                     :: hasCarbon, hasNitrogen, hasPhosphorous
    type(ESMF_Field), allocatable       :: importFieldList(:)
    type(ESMF_Field), allocatable       :: fieldList(:)
    character(len=ESMF_MAXSTR), pointer :: includeList(:) => null()

    type(spVariable) :: soilNitrate, soilAmmonium
    type(spVariable) :: soilPhosphorous, waterCarbon, waterNitrogen
    type(spVariable) :: waterNitrate, waterAmmonium, waterNutrient
    type(spVariable) :: waterPhosphorous, soilOdu, waterOdu
    type(spVariable) :: soilOxygen, waterOxygen
    logical          :: isEqual, isPresent

    rc = ESMF_SUCCESS
    call MOSSCO_CompEntry(cplComp, externalClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(externalClock, advanceCount=advanceCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    verbose = .false.
    if (advanceCount == 0) verbose = .true.

    !> Export fields created by OmexDia typically are:
    !> detritus_labile_carbon, detritus_semilabile_carbon, detritus_phosphorus
    !> and their respective z-velocities plus
    !> mole_concentration_of_phosphate, mole_concentration_of_nitrate,
    !> mole_concentration_of_ammonium, dissolved_oxygen, dissolved_reduced_substances
    !> And for all the above the upward-fluxes
    !> This coupler component deals *only* with these 8 upward fluxes

    !> Import fields to ECOSMO are typically:
    !> hzg_ecosmo_no3, hzg_ecosmo_nh4, hzg_ecosmo_pho, hzg_ecosmo_det
    !> hzg_ecosmo_opa, hzg_ecosmo_dom
    !> hzg_ecosmo_sil
    !> hzg_ecosmo_oxy, hzg_ecosmo_dia, hzg_ecosmo_fla, hzg_ecosmo_bg
    !> hzg_ecosmo_microzoo, hzg_ecosmo_mesozoo

    call MOSSCO_StateGet(importState, fieldList, &
      itemSearch='mole_concentration_of_phosphate_upward_flux_at_soil_surface', &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount /= 1) then
      write(message,'(A,I1)') trim(name)//' expected exactly one field for mole_concentration_of_phosphate_upward_flux_at_soil_surface, received ',fieldCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_BAD
      return
    else
      soilPhosphorous%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=soilPhosphorous%rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (soilPhosphorous%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=soilPhosphorous%data1)
      elseif (soilPhosphorous%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=soilPhosphorous%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', soilPhosphorous%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      write(message, '(A)') trim(name)//' uses soil phosphorous '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_StateGet(importState, fieldList, &
      itemSearch='mole_concentration_of_nitrate_upward_flux_at_soil_surface', &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount /= 1) then
      write(message,'(A,I1)') trim(name)//' expected exactly one field for mole_concentration_of_nitrate_upward_flux_at_soil_surface, received ',fieldCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_BAD
      return
    else
      soilNitrate%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=soilNitrate%rank, rc=localrc)
      if (soilNitrate%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=soilNitrate%data1)
      elseif (soilNitrate%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=soilNitrate%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', soilNitrate%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      write(message, '(A)') trim(name)//' uses soil nitrate '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_StateGet(importState, fieldList, &
      itemSearch='mole_concentration_of_ammonium_upward_flux_at_soil_surface', &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount /= 1) then
      write(message,'(A,I1)') trim(name)//' expected exactly one field for mole_concentration_of_ammonium_upward_flux_at_soil_surface, received ',fieldCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_BAD
      return
    else
      soilAmmonium%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=soilAmmonium%rank, rc=localrc)
      if (soilNitrate%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=soilAmmonium%data1)
      elseif (soilNitrate%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=soilAmmonium%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', soilAmmonium%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      write(message, '(A)') trim(name)//' uses soil ammonium '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    !> Now look for nitrogen species in export states, these areas
    !> for MAECS Dissolved_Inorganic_Nitrogen_DIN_nutN, for NPZD nutrients,
    !> for ECOSMO hzg_ecosmo_no3, hzg_ecosmo_nh4

    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(2))
    includeList(1) = 'nitrate_upward_flux_at_soil_surface'
    includeList(2) = 'hzg_ecosmo_no3_upward_flux_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, &
      include=includeList, verbose=verbose, &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      waterNitrate%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=waterNitrate%rank, rc=localrc)
      if (waterNitrate%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterNitrate%data1)
        if (associated(soilNitrate%data1)) waterNitrate%data1 = soilNitrate%data1
      elseif (waterNitrate%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterNitrate%data2)
        if (associated(soilNitrate%data2)) waterNitrate%data2 = soilNitrate%data2
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', waterNitrate%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' uses water nitrate '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      if (soilNitrate%rank > 0) then
        isEqual = .false.
        !call MOSSCO_CheckUnits(soilNitrate%unit, waterNitrate%unit, isEqual, localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message,'(A)') trim(name)//' unit mismatch '
        call MOSSCO_MessageAdd(message, trim(soilNitrate%unit)//' /= ')
        call MOSSCO_MessageAdd(message, trim(waterNitrate%unit))
        if (.not.isEqual) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

        if (waterNitrate%rank == 1) then
          waterNitrate%data1 = soilNitrate%data1
        elseif (waterNitrate%rank == 2) then
          waterNitrate%data2 = soilNitrate%data2
        endif

        write(message,'(A)') trim(name)//' connected'
        call MOSSCO_FieldString(soilNitrate%field, message)
        call MOSSCO_MessageAdd(message,' to ')
        call MOSSCO_FieldString(waterNitrate%field, message)
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    !> @todo remove return
    !return

    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(3))
    includeList(1) = 'ammonium_upward_flux_at_soil_surface'
    includeList(2) = 'dissolved_ammonium_nh3_upward_flux_at_soil_surface'
    includeList(3) = 'hzg_ecosmo_nh4_upward_flux_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, &
      include=includeList, verbose=verbose, &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      waterAmmonium%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=waterAmmonium%rank, rc=localrc)
      if (waterAmmonium%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterAmmonium%data1)
      elseif (waterAmmonium%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterAmmonium%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', waterAmmonium%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' uses water ammonium '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      if (soilAmmonium%rank > 0) then
        isEqual = .false.
        !call MOSSCO_CheckUnits(soilAmmonium%unit, waterAmmonium%unit, isEqual, localrc)
        write(message,'(A)') trim(name)//' unit mismatch '
        call MOSSCO_MessageAdd(message, trim(soilAmmonium%unit)//' /= ')
        call MOSSCO_MessageAdd(message, trim(waterAmmonium%unit)//' /= ')
        if (.not.isEqual) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

        if (waterAmmonium%rank == 1) then
          waterAmmonium%data1 = convertN * soilAmmonium%data1
        elseif (waterAmmonium%rank == 2) then
          waterAmmonium%data2 = convertN * soilAmmonium%data2
        endif
        write(message,'(A)') trim(name)//' connected'
        call MOSSCO_FieldString(soilAmmonium%field, message)
        call MOSSCO_MessageAdd(message,' to ')
        call MOSSCO_FieldString(waterAmmonium%field, message)
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(3))
    includeList(1) = 'nutrients_upward_flux_at_soil_surface'
    includeList(2) = 'DIN_upward_flux_at_soil_surface'
    includeList(3) = 'Dissolved_Inorganic_Nitrogen_DIN_nutN_upward_flux_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, &
      include=includeList, verbose=verbose, &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then
      waterNutrient%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=waterNutrient%rank, rc=localrc)
      if (waterNutrient%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterNutrient%data1)
      elseif (waterNutrient%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterNutrient%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', waterNutrient%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' uses water nutrient '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      if ((soilAmmonium%rank > 0).and.(soilNitrate%rank > 0)) then
        isEqual = .false.
        !call MOSSCO_CheckUnits(soilAmmonium%unit, waterNutrient%unit, isEqual, localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message,'(A)') trim(name)//' unit mismatch '
        call MOSSCO_MessageAdd(message, trim(soilAmmonium%unit)//' /= ')
        call MOSSCO_MessageAdd(message, trim(waterNutrient%unit))
        if (.not.isEqual) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

        !call MOSSCO_CheckUnits(soilNitrate%unit, waterNutrient%unit, isEqual, localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message,'(A)') trim(name)//' unit mismatch '
        call MOSSCO_MessageAdd(message, trim(soilNitrate%unit)//' /= ')
        call MOSSCO_MessageAdd(message, trim(waterNutrient%unit))
        if (.not.isEqual) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

        if (waterNutrient%rank == 1) then
          waterNutrient%data1 = (soilAmmonium%data1 + soilNitrate%data1 &
          !> @todo why only here, not with separate NO3/NH4?
            + dinflux_const/(86400.0*365.0)) * convertN
        elseif (waterNutrient%rank == 2) then
          waterNutrient%data2 = (soilAmmonium%data2 + soilNitrate%data2 &
            + dinflux_const/(86400.0*365.0)) * convertN
        endif
        write(message,'(A)') trim(name)//' connected'
        call MOSSCO_FieldString(soilAmmonium%field, message)
        call MOSSCO_MessageAdd(message,' + ')
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        write(message,'(A)') trim(name)//' + '
        call MOSSCO_FieldString(soilNitrate%field, message)
        call MOSSCO_MessageAdd(message,' to ')
        call MOSSCO_FieldString(waterNutrient%field, message)
        if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
      !> todo nutrients includes P, or doubling of N species if only one is known
    endif

    !> Now on to phosphorous (which we already have for soilPhosphorous)
    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(4))
    includeList(1) = 'DIP_upward_flux_at_soil_surface'
    includeList(2) = 'phosphate_upward_flux_at_soil_surface'
    includeList(3) = 'Dissolved_Inorganic_Phosphorus_DIP_nutP_upward_flux_at_soil_surface'
    includeList(4) = 'hzg_ecosmo_pho_upward_flux_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, &
      include=includeList, verbose=verbose, &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0 ) then
      waterPhosphorous%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=waterPhosphorous%rank, rc=localrc)
      if (waterPhosphorous%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterPhosphorous%data1)
      elseif (waterPhosphorous%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterPhosphorous%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', waterPhosphorous%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' uses water phosphorous '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    endif

    if ((waterPhosphorous%rank > 0) .and. (soilPhosphorous%rank > 0)) then
      isEqual = .false.
      !call MOSSCO_CheckUnits(waterPhosphorous%unit, soilPhosphorous%unit, isEqual, localrc)
      write(message,'(A)') trim(name)//' unit mismatch '
      call MOSSCO_MessageAdd(message, trim(waterPhosphorous%unit)//' /= ')
      call MOSSCO_MessageAdd(message, trim(soilPhosphorous%unit))
      if (.not.isEqual) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

      if (.not.isEqual) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (waterPhosphorous%rank == 1) then
        waterPhosphorous%data1 = convertP * (soilPhosphorous%data1 &
          + dipflux_const/(86400.0*365.0))
      elseif (waterPhosphorous%rank == 2) then
        waterPhosphorous%data2 = convertP * (soilPhosphorous%data2 &
          + dipflux_const/(86400.0*365.0))
      endif
      write(message,'(A)') trim(name)//' connected'
      call MOSSCO_FieldString(soilPhosphorous%field, message)
      call MOSSCO_MessageAdd(message,' to ')
      call MOSSCO_FieldString(waterPhosphorous%field, message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    !> Now on to oxygen and odu
    call MOSSCO_StateGet(importState, fieldList, &
      itemSearch='dissolved_oxygen_upward_flux_at_soil_surface', &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount /= 1) then
      write(message,'(A,I1)') trim(name)//' expected exactly one field for dissolved_oxygen_upward_flux_at_soil_surface, received ',fieldCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_BAD
      return
    else
      soilOxygen%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=soilOxygen%rank, rc=localrc)
      if (soilOxygen%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=soilOxygen%data1)
      elseif (soilOxygen%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=soilOxygen%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', soilOxygen%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      write(message, '(A)') trim(name)//' uses soil oxygen '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_StateGet(importState, fieldList, &
      itemSearch='dissolved_reduced_substances_upward_flux_at_soil_surface', &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount /= 1) then
      write(message,'(A,I1)') trim(name)//' expected exactly one field for dissolved_reduced_substances_upward_flux_at_soil_surface, received ',fieldCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_BAD
      return
    else
      soilOdu%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=soilOdu%rank, rc=localrc)
      if (soilOdu%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=soilOdu%data1)
      elseif (soilOdu%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=soilOdu%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', soilOdu%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      write(message, '(A)') trim(name)//' uses soil ODU '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    !> Oxygen and ODU fields in export
    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(3))
    includeList(1) = 'oxygen_upward_flux_at_soil_surface'
    includeList(2) = 'dissolved_oxygen_oxy_upward_flux_at_soil_surface'
    includeList(3) = 'hzg_ecosmo_oxy_upward_flux_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, &
      include=includeList, verbose=verbose, &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0 ) then
      waterOxygen%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=waterOxygen%rank, rc=localrc)
      if (waterOxygen%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterOxygen%data1)
      elseif (waterOxygen%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterOxygen%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', waterOxygen%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' uses water oxygen '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    endif

    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(1))
    includeList(1) = 'dissolved_reduced_substances_upward_flux_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, &
      include=includeList, verbose=verbose, &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0 ) then
      waterOdu%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=waterOdu%rank, rc=localrc)
      if (waterOdu%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterOdu%data1)
      elseif (waterOdu%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterOdu%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', waterOdu%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' uses water ODU '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    endif

    if ((waterOdu%rank > 0) .and. (waterOxygen%rank > 0)) then

      isEqual = .false.
      !call MOSSCO_CheckUnits(waterOdu%unit, soilOdu%unit, isEqual, localrc)
      write(message,'(A)') trim(name)//' unit mismatch '
      call MOSSCO_MessageAdd(message, trim(waterOdu%unit)//' /= ')
      call MOSSCO_MessageAdd(message, trim(soilOdu%unit))
      if (.not.isEqual) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

      if (soilOdu%rank == 1) then
        waterOdu%data1 = soilOdu%data1
      elseif (soilOdu%rank == 2) then
        waterOdu%data2 = soilOdu%data2
      endif
      write(message,'(A)') trim(name)//' connected'
      call MOSSCO_FieldString(soilOdu%field, message)
      call MOSSCO_MessageAdd(message,' to ')
      call MOSSCO_FieldString(waterOdu%field, message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      !call MOSSCO_CheckUnits(waterOxygen%unit, soilOxygen%unit, isEqual, localrc)
      write(message,'(A)') trim(name)//' unit mismatch '
      call MOSSCO_MessageAdd(message, trim(waterOxygen%unit)//' /= ')
      call MOSSCO_MessageAdd(message, trim(soilOxygen%unit))
      if (.not.isEqual) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

      if (soilOdu%rank == 1) then
        waterOxygen%data1 = soilOxygen%data1
      elseif (soilOdu%rank == 2) then
        waterOxygen%data2 = soilOxygen%data2
      endif
      write(message,'(A)') trim(name)//' connected'
      call MOSSCO_FieldString(soilOxygen%field, message)
      call MOSSCO_MessageAdd(message,' to ')
      call MOSSCO_FieldString(waterOxygen%field, message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    elseif (waterOdu%rank > 0) then
      isEqual = .false.
      !call MOSSCO_CheckUnits(waterOdu%unit, soilOxygen%unit, isEqual, localrc)
      write(message,'(A)') trim(name)//' unit mismatch '
      call MOSSCO_MessageAdd(message, trim(waterOdu%unit)//' /= ')
      call MOSSCO_MessageAdd(message, trim(soilOxygen%unit))
      if (.not.isEqual) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      !call MOSSCO_CheckUnits(waterOdu%unit, soilOdu%unit, isEqual, localrc)

      if (soilOdu%rank == 1) then
        waterOdu%data1 =  soilOdu%data1 - soilOxygen%data1
      elseif (soilOdu%rank == 2) then
        waterOdu%data2 = soilOdu%data2 - soilOxygen%data2
      endif
      write(message,'(A)') trim(name)//' connected'
      call MOSSCO_FieldString(soilOdu%field, message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      write(message,'(A)') trim(name)//' - '
      call MOSSCO_FieldString(soilOxygen%field, message)
      call MOSSCO_MessageAdd(message,' to ')
      call MOSSCO_FieldString(waterOdu%field, message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    elseif (waterOxygen%rank > 0) then
      isEqual = .false.
      !call MOSSCO_CheckUnits(waterOxygen%unit, soilOxygen%unit, isEqual, localrc)
      write(message,'(A)') trim(name)//' unit mismatch '
      call MOSSCO_MessageAdd(message, trim(waterOxygen%unit)//' /= ')
      call MOSSCO_MessageAdd(message, trim(soilOxygen%unit))
      if (.not.isEqual) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      !call MOSSCO_CheckUnits(waterOxygen%unit, soilOdu%unit, isEqual, localrc)

      if (soilOxygen%rank == 1) then
        waterOxygen%data1 =  soilOxygen%data1 - soilOdu%data1
      elseif (soilOxygen%rank == 2) then
        waterOxygen%data2 = soilOxygen%data2 - soilOdu%data2
      endif
      write(message,'(A)') trim(name)//' connected'
      call MOSSCO_FieldString(soilOxygen%field, message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      write(message,'(A)') trim(name)//' - '
      call MOSSCO_FieldString(soilOdu%field, message)
      call MOSSCO_MessageAdd(message,' to ')
      call MOSSCO_FieldString(waterOxygen%field, message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    endif

    !> Detrital nitrogen
    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(3))
    includeList(1) = 'detritus_upward_flux_at_soil_surface'
    includeList(2) = 'detN_upward_flux_at_soil_surface'
    includeList(3) = 'Detritus_Nitrogen_detN_upward_flux_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, &
      include=includeList, &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount == 1) then

      waterNitrogen%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=waterNitrogen%rank, rc=localrc)
      if (waterNitrogen%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterNitrogen%data1)
      elseif (waterNitrogen%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterNitrogen%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', waterNitrogen%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      write(message, '(A)') trim(name)//' uses water nitrogen '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      !> Look for detritus nitrogen species in soil and sum them up to send
      !> them to the pelagic
      if (associated(includeList)) deallocate(includeList)
      allocate(includeList(1))
      includeList(1) = 'detritus*nitrogen_upward_flux_at_soil_surface'

      call MOSSCO_StateGet(importState, importFieldList, fieldCount=fieldCount, &
        include=includeList, verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (waterNitrogen%rank == 1) then
        waterNitrogen%data1 = 0.0
      elseif (waterNitrogen%rank == 2) then
        waterNitrogen%data2 = 0.0
      endif

      do i=1,fieldCount

        call ESMF_FieldGet(importFieldList(i), rank=rank, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (rank /= waterNitrogen%rank) then
          write(message, '(A)') trim(name)//' rank mismatch for water nitrogen'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          rc = ESMF_RC_ARG_BAD
          return
        endif

        call ESMF_FieldGetBounds(importFieldList(i), &
          exclusiveUbound=ubnd(1:rank), exclusiveLBound=lbnd(1:rank), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        !> @todo check bounds

        if (rank == 1) then
          call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          waterNitrogen%data1 = waterNitrogen%data1  + farrayPtr1
        elseif (rank == 2) then
          call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          waterNitrogen%data2 = waterNitrogen%data2  + farrayPtr2
        endif
      enddo

    endif

    !> Now on to detritus, deal with carbon first
    call MOSSCO_StateGet(exportState, fieldList, &
      itemSearch='Detritus_Carbon_detC_upward_flux_at_soil_surface', &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Only if carbon is needed do we consider detritus carbon
    if (fieldCount == 1) then

      waterCarbon%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=waterCarbon%rank, rc=localrc)
      if (waterCarbon%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterCarbon%data1)
      elseif (waterCarbon%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterCarbon%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', waterCarbon%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      write(message, '(A)') trim(name)//' uses water carbon '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      !> Look for detritus carbon species in soil and sum them up to send
      !> them to the pelagic
      if (associated(includeList)) deallocate(includeList)
      allocate(includeList(1))
      includeList(1) = 'detritus*carbon_upward_flux_at_soil_surface'

      call MOSSCO_StateGet(importState, importFieldList, fieldCount=fieldCount, &
        include=includeList, verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (waterCarbon%rank == 1) then
        waterCarbon%data1 = 0.0
      elseif (waterCarbon%rank == 2) then
        waterCarbon%data2 = 0.0
      endif

      do i=1,fieldCount

        call ESMF_FieldGet(importFieldList(i), rank=rank, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (rank /= waterCarbon%rank) then
          write(message, '(A)') trim(name)//' rank mismatch for water carbon'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          rc = ESMF_RC_ARG_BAD
          return
        endif

        call ESMF_FieldGetBounds(importFieldList(i), &
          exclusiveUbound=ubnd(1:rank), exclusiveLBound=lbnd(1:rank), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        !> @todo check bounds

        if (rank == 1) then
          call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          waterCarbon%data1 = waterCarbon%data1  + farrayPtr1
        elseif (rank == 2) then
          call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          waterCarbon%data2 = waterCarbon%data2  + farrayPtr2
        endif
      enddo

    endif


    !> Detrital phosphorous
    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(2))
    includeList(1) = 'detP_upward_flux_at_soil_surface'
    includeList(2) = 'Detritus_Phosphorus_detP_upward_flux_at_soil_surface'

    call MOSSCO_StateGet(exportState, fieldList, &
      include=includeList, &
      fieldCount=fieldCount, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount == 1) then

      waterPhosphorous%field = fieldList(1)
      call ESMF_FieldGet(fieldList(1), rank=waterPhosphorous%rank, rc=localrc)
      if (waterPhosphorous%rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterPhosphorous%data1)
      elseif (waterPhosphorous%rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=waterPhosphorous%data2)
      endif
      call ESMF_AttributeGet(fieldList(1), 'unit', waterPhosphorous%unit, &
        isPresent=isPresent, defaultValue='', rc=localrc)
      write(message, '(A)') trim(name)//' uses water phosphorous '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      !> Look for detritus nitrogen species in soil and sum them up to send
      !> them to the pelagic
      if (associated(includeList)) deallocate(includeList)
      allocate(includeList(1))
      includeList(1) = 'detritus*phosphorous_upward_flux_at_soil_surface'

      call MOSSCO_StateGet(importState, importFieldList, fieldCount=fieldCount, &
        include=includeList, verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (waterPhosphorous%rank == 1) then
        waterPhosphorous%data1 = 0.0
      elseif (waterPhosphorous%rank == 2) then
        waterPhosphorous%data2 = 0.0
      endif

      do i=1,fieldCount

        call ESMF_FieldGet(importFieldList(i), rank=rank, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (rank /= waterPhosphorous%rank) then
          write(message, '(A)') trim(name)//' rank mismatch for water phosphorous'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          rc = ESMF_RC_ARG_BAD
          return
        endif

        call ESMF_FieldGetBounds(importFieldList(i), &
          exclusiveUbound=ubnd(1:rank), exclusiveLBound=lbnd(1:rank), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        !> @todo check bounds

        if (rank == 1) then
          call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr1, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          waterPhosphorous%data1 = waterPhosphorous%data1  + farrayPtr1
        elseif (rank == 2) then
          call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          waterPhosphorous%data2 = waterPhosphorous%data2  + farrayPtr2
        endif
      enddo
    endif


    !> For omexdia_p, get detritus N from detritus C, could be streamlined with variables
    !> gathered above, but left from old code for now
    !> if nitrogen did not match so far, use the carbon flux
    !> as used in the sediment by omexdia for the nitrogen detritus.
    ! if ( (.not. hasNitrogen) .and. associated(detNFlux) ) then
    !
    !   call mossco_state_get(importState,(/'detritus_semilabile_carbon_upward_flux_at_soil_surface'/), &
    !     SDETCflux, verbose=verbose, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   call mossco_state_get(importState,(/'detritus_labile_carbon_upward_flux_at_soil_surface'/), &
    !      LDETCflux, verbose=verbose, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   DETNflux = convertN*(NC_ldet*LDETCflux + NC_sdet*SDETCflux)
    ! endif

    !> @todo For models that lack phosphorous, add this according to Redfield

    call MOSSCO_Reallocate(importFieldList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(cplcomp, importState, exportState, externalclock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer,intent(out)  :: rc

    character(len=ESMF_MAXSTR)  :: name, message, paramName
    type(ESMF_State)            :: paramState
    type(ESMF_Time)             :: currTime
    integer                     :: localrc
    type(ESMF_StateItem_Flag)   :: exportItemType, importItemType

    call MOSSCO_CompEntry(cplComp, externalClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

#if 0
    !! Safely destroy the parameter state if it exists in either import or export states
    paramName=trim(name)//'Parameters'
    call ESMF_StateGet(importState, paramName, itemType=importItemType, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateGet(exportState, paramName, itemType=exportItemType, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if ((exportItemType == ESMF_STATEITEM_STATE) .or. (importItemType == ESMF_STATEITEM_STATE)) then
      if (importItemType == ESMF_STATEITEM_STATE) then
        call ESMF_StateGet(importState, paramName, paramState, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_StateRemove(importState, (/paramName/), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      if (exportItemType == ESMF_STATEITEM_STATE) then
        call ESMF_StateGet(exportState, paramName, paramState, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_StateRemove(importState, (/paramName/), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      call ESMF_StateDestroy(paramState, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif
#endif

    !! Exit the method
    call MOSSCO_CompExit(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Finalize

end module soil_pelagic_connector
