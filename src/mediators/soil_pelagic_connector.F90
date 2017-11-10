!> @brief Implementation of an ESMF soil-pelagic coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015 Helmholtz-Zentrum Geesthacht
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
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DETN=>null(),DIN=>null(),vDETN=>null()
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DIP=>null(),DETP=>null(),vDETP=>null()
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: vDETC=>null(),DETC=>null()
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3=>null()
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: ptr_f2=>null(),val1_f2=>null(),val2_f2=>null()
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: DETNflux=>null(),DETPflux=>null()
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: DETCflux=>null(),DINflux=>null()
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: DIPflux=>null(),OXYflux=>null()
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: ODUflux=>null(),omexDETPflux=>null()
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: SDETCflux=>null(),LDETCflux=>null()
  real(ESMF_KIND_R8) :: dinflux_const=0.0
  real(ESMF_KIND_R8) :: dipflux_const=-1.
  real(ESMF_KIND_R8) :: NC_ldet=0.20d0
  real(ESMF_KIND_R8) :: NC_sdet=0.04d0
  real(ESMF_KIND_R8) :: convertP=1.0d0
  real(ESMF_KIND_R8) :: convertN=1.0d0
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !read namelist
    inquire(file=trim(name)//'.nml', exist=isPresent)

    if (isPresent) then
      open(nmlunit,file='soil_pelagic_connector.nml',action='read',status='old')
      read(nmlunit,soil_pelagic_connector)
      close(nmlunit)
    endif

    if (dipflux_const < 0.0) dipflux_const=dinflux_const/16.0d0

    paramState=ESMF_StateCreate(name=trim(name)//'Parameters', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(paramState, trim(name)//'::dipflux_const', dipflux_const, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(paramState, trim(name)//'::dinflux_const', dinflux_const, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(importState, (/paramState/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"

  subroutine Run(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc
    integer              :: ammrc, nitrc, oxyrc, odurc

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
    logical                     :: hasCarbon, hasNitrogen, hasPhosphorous
    type(ESMF_Field), allocatable       :: importFieldList(:)
    character(len=ESMF_MAXSTR), pointer :: includeList(:) => null()

    rc = ESMF_SUCCESS
    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(externalClock, advanceCount=advanceCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    verbose = .true.
    if (advanceCount > 0) verbose = .false.

    !   DIN flux:
    call mossco_state_get(importState, (/'mole_concentration_of_nitrate_upward_flux_at_soil_surface'/), &
      val1_f2, verbose=verbose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call mossco_state_get(importState, (/'mole_concentration_of_ammonium_upward_flux_at_soil_surface'/), &
      val2_f2, verbose=verbose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call mossco_state_get(exportState, &
             (/'nitrate_upward_flux_at_soil_surface'/), &
             DINflux, ubnd=ubnd, lbnd=lbnd, verbose=verbose, rc=nitrc)
    if (nitrc == 0) DINflux = convertN*val1_f2
    call mossco_state_get(exportState, &
             (/'ammonium_upward_flux_at_soil_surface               ',   &
               'dissolved_ammonium_nh3_upward_flux_at_soil_surface '/), &
             DINflux, ubnd=ubnd, lbnd=lbnd, verbose=verbose, rc=ammrc)
    if (ammrc == 0) DINflux = convertN*val2_f2

    !RH: weak check, needs to be replaced:
    if (nitrc /= 0) then
        call mossco_state_get(exportState,(/ &
              'nutrients_upward_flux_at_soil_surface                            ', &
              'DIN_upward_flux_at_soil_surface                                  ', &
              'Dissolved_Inorganic_Nitrogen_DIN_nutN_upward_flux_at_soil_surface'/), &
              DINflux,ubnd=ubnd,lbnd=lbnd, verbose=verbose, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        DINflux = convertN*(val1_f2 + val2_f2)
        ! add constant boundary flux of DIN (through groundwater, advection, rain
        DINflux = DINflux + convertN*dinflux_const/(86400.0*365.0)
    end if

    !   DIP flux:
    call mossco_state_get(exportState,(/ &
              'DIP_upward_flux_at_soil_surface                                    ', &
              'phosphate_upward_flux_at_soil_surface                              ', &
              'Dissolved_Inorganic_Phosphorus_DIP_nutP_upward_flux_at_soil_surface'/), &
              DIPflux, verbose=verbose, rc=localrc)

    if (localrc == 0)  then
        call mossco_state_get(importState,(/ &
              'mole_concentration_of_phosphate_upward_flux_at_soil_surface'/), &
              val1_f2, verbose=verbose, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        DIPflux = convertP*(val1_f2 + dipflux_const/(86400.0*365.0))
    end if

    ! Detritus fluxes, of carbon, phosphorous and nitrogen, these are
    ! state variables defined by variants of omexdia_p and omexdia_cnp
    !> Deal with carbon first
    hasCarbon = .false.
    call mossco_state_get(exportState,(/ &
      'Detritus_Carbon_detC_upward_flux_at_soil_surface'/),DETCflux, &
      verbose=verbose, rc=localrc)
    if (localrc == ESMF_SUCCESS) then
      if (associated(includeList)) deallocate(includeList)
      allocate(includeList(1))
      includeList(1) = 'detritus*carbon_upward_flux_at_soil_surface'

      call MOSSCO_StateGet(importState, importFieldList, fieldCount=fieldCount, &
        include=includeList, verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldCount > 0) hasCarbon = .true.
      DETCflux(RANGE2D) = 0.0
      do i=1,fieldCount
        call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr2, rc=localrc)
        DETCflux(RANGE2D) = DETCflux(RANGE2D) + farrayPtr2(RANGE2D)
      enddo
    elseif (localrc /= ESMF_RC_NOT_FOUND) then
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    !> Now look for nitrogen
    hasNitrogen = .false.
    call mossco_state_get(exportState,(/ &
      'detritus_upward_flux_at_soil_surface              ', &
      'detN_upward_flux_at_soil_surface                  ', &
      'Detritus_Nitrogen_detN_upward_flux_at_soil_surface'/), &
      DETNflux, verbose=verbose, rc=localrc)
    if (localrc == ESMF_SUCCESS) then
      if (associated(includeList)) deallocate(includeList)
      includeList(1) = 'detritus*nitrogen_upward_flux_at_soil_surface'

      call MOSSCO_StateGet(importState, importFieldList, fieldCount=fieldCount, &
        include=includeList, verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldCount > 0) hasNitrogen = .true.
      DETNflux(RANGE2D) = 0.0
      do i=1,fieldCount
        call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr2, rc=localrc)
        DETNflux(RANGE2D) = DETNflux(RANGE2D) + convertN*farrayPtr2(RANGE2D)
      enddo
    elseif (localrc /= ESMF_RC_NOT_FOUND) then
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    !> Now look for phosphorous
    call mossco_state_get(exportState,(/ &
      'detP_upward_flux_at_soil_surface                    ', &
      'Detritus_Phosphorus_detP_upward_flux_at_soil_surface'/), DETPflux, &
      verbose=verbose, rc=rc)

    hasPhosphorous = .false.
    if (localrc == ESMF_SUCCESS) then

      if (associated(includeList)) deallocate(includeList)
      includeList(1) = 'detritus*phosphorous_upward_flux_at_soil_surface'

      call MOSSCO_StateGet(importState, importFieldList, fieldCount=fieldCount, &
        include=includeList, verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldCount > 0) hasPhosphorous = .true.
      DETPflux(RANGE2D) = 0.0
      do i=1,fieldCount
        call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr2, rc=localrc)
        DETPflux(RANGE2D) = DETPflux(RANGE2D) + convertP*farrayPtr2(RANGE2D)
      enddo
    elseif (localrc /= ESMF_RC_NOT_FOUND) then
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif
    if (associated(includeList)) deallocate(includeList)

    !> For omexdia_p, get detritus N from detritus C, could be streamlined with variables
    !> gathered above, but left from old code for now
    if ( hasCarbon .and. (.not. hasNitrogen) .and. associated(detNFlux) ) then

      call mossco_state_get(importState,(/'detritus_semilabile_carbon_upward_flux_at_soil_surface'/), &
        SDETCflux, verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call mossco_state_get(importState,(/'detritus_labile_carbon_upward_flux_at_soil_surface'/), &
         LDETCflux, verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      DETNflux = convertN*(NC_ldet*LDETCflux + NC_sdet*SDETCflux)
    endif

    !> For models that lack phosphorous, add this according to Redfield
    if ( hasNitrogen .and. (.not. hasPhosphorous) .and. associated(detPFlux) ) then
      DETPflux(RANGE2D) = DETNflux(RANGE2D) / 16.0
    elseif ( hasCarbon .and. (.not. hasPhosphorous) .and. associated(detPFlux) ) then
      DETPflux(RANGE2D) = DETCflux(RANGE2D) / 106.0
    endif

    call MOSSCO_Reallocate(importFieldList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)



      !> oxygen and odu fluxes
      call mossco_state_get(exportState,(/ &
        'oxygen_upward_flux_at_soil_surface               ', &
        'dissolved_oxygen_oxy_upward_flux_at_soil_surface '/),OXYflux, verbose=verbose, rc=oxyrc)
      call mossco_state_get(exportState,(/ &
        'dissolved_reduced_substances_odu_upward_flux_at_soil_surface'/),ODUflux, verbose=verbose, rc=odurc)
      if (oxyrc == 0) then
        call mossco_state_get(importState,(/'dissolved_oxygen_upward_flux_at_soil_surface'/), &
          val1_f2, verbose=verbose, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        OXYflux = val1_f2
      endif
      if ((oxyrc == 0) .or. (odurc == 0)) then
        call mossco_state_get(importState,(/'dissolved_reduced_substances_upward_flux_at_soil_surface'/), &
          val2_f2, verbose=verbose, rc=rc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      if ((oxyrc == 0) .and. (odurc /= 0)) OXYflux = OXYflux - val2_f2
      if (odurc == 0) ODUflux = val2_f2

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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

    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#if 0
    !! Safely destroy the parameter state if it exists in either import or export states
    paramName=trim(name)//'Parameters'
    call ESMF_StateGet(importState, paramName, itemType=importItemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, paramName, itemType=exportItemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if ((exportItemType == ESMF_STATEITEM_STATE) .or. (importItemType == ESMF_STATEITEM_STATE)) then
      if (importItemType == ESMF_STATEITEM_STATE) then
        call ESMF_StateGet(importState, paramName, paramState, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateRemove(importState, (/paramName/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      if (exportItemType == ESMF_STATEITEM_STATE) then
        call ESMF_StateGet(exportState, paramName, paramState, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateRemove(importState, (/paramName/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_StateDestroy(paramState, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
#endif

    !! Exit the method
    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module soil_pelagic_connector
