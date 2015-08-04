!> @brief Implementation of an ESMF soil-pelagic coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Nils Weiher <stu95021@uni-kiel.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "soil_pelagic_mediator.F90"

module soil_pelagic_mediator

    !use mossco_mediator
    use esmf
    use mossco_state
    use mossco_field
    use mossco_component


!> @todo: please check if anything can be outsourced in the general method
!----------------------------------------------------------------------
!------------------- Soil Pelagic Config ------------------------------
!----------------------------------------------------------------------

    !------------------------------------------------------------------
    implicit none

    private
    !MODULE VARS
    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DETN=>null(),DIN=>null(),vDETN=>null()
    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DIP=>null(),DETP=>null(),vDETP=>null()
    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: vDETC=>null(),DETC=>null()
    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3=>null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: ptr_f2=>null(),val1_f2=>null(),val2_f2=>null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: DETNflux=>null(),DETPflux=>null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: DETCflux=>null(),DINflux=>null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: DIPflux=>null(),OXYflux=>null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: ODUflux=>null(),omexDETPflux=>null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: SDETCflux=>null(),fDETCflux=>null()
    real(ESMF_KIND_R8) :: dinflux_const=0.0
    real(ESMF_KIND_R8) :: dipflux_const=-1.
    !------------------------------------------------------------------

    public SetServices

    contains

!----------------------------------------------------------------------
!------------------- Soil Pelagic Routines ----------------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "mcpl_InitializeP0"
!> @subsubsection mcpl_InitializeP0 "User Code in Initialization Phase 0"
!> @brief Implementation of User-Code in Phase 0
!> @details Contains untouched States
!> @param
subroutine mcpl_InitializeP0(cplComp, importState, exportState, parentClock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: parentClock
    integer, intent(out)        :: rc

    !LOCAL VARS

    !------------------------------------------------------------------
    rc = ESMF_SUCCESS

end subroutine mcpl_InitializeP0


!> @todo: please check if anything can be outsourced in the general method
#undef  ESMF_METHOD
#define ESMF_METHOD "mcpl_InitializeP1"
!> @subsubsection mcpl_InitializeP1 "User Code in Initialization Phase 1"
!> @brief Implementation of User-Code in Phase 1
!> @details Contains untouched States
!> @param
subroutine mcpl_InitializeP1(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_State)            :: paramState
    type(ESMF_Time)             :: currTime
    logical                     :: isPresent
    integer                     :: nmlunit=127, localrc
    !------------------------------------------------------------------


    namelist /soil_pelagic_mediator/ dinflux_const,dipflux_const

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, externalClock, name=name, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !read namelist
    inquire(file=trim(name)//'.nml', exist=isPresent)

    if (isPresent) then
      open(nmlunit,file='soil_pelagic_mediator.nml',action='read',status='old')
      read(nmlunit,soil_pelagic_mediator)
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

end subroutine mcpl_InitializeP1


!> @todo: please check if anything can be outsourced in the general method
#undef  ESMF_METHOD
#define ESMF_METHOD "mcpl_Run_pre_recipe"
!> @subsubsection mcpl_Run_pre_recipe "User Code before automatic recipe search"
!> @brief Implementation of User-Code in Run Phase,
!! before automatic recipe search via database
!> @details Contains normalized inventory lists of substances known by database
!> @param
subroutine mcpl_Run_pre_recipe(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS
    integer                     :: ammrc, nitrc, oxyrc, odurc
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime, stopTime
    integer                     :: localrc
    integer                     :: myrank
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    type(ESMF_Field)            :: field
    integer(ESMF_KIND_R8)       :: advanceCount
    !> @todo read NC_fdet dynamically from fabm model info?  This would not comply with our aim to separate fabm/esmf
    real(ESMF_KIND_R8),parameter:: NC_fdet=0.20d0
    real(ESMF_KIND_R8),parameter:: NC_sdet=0.04d0
    integer(ESMF_KIND_I4)       :: rank, ubnd(2), lbnd(2), itemCount
    logical                     :: verbose=.true.
    !------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(externalClock, advanceCount=advanceCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (advanceCount > 0) verbose=.false.

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
    if (nitrc == 0) DINflux = val1_f2
    call mossco_state_get(exportState, &
             (/'ammonium_upward_flux_at_soil_surface               ',   &
               'dissolved_ammonium_nh3_upward_flux_at_soil_surface '/), &
             DINflux, ubnd=ubnd, lbnd=lbnd, verbose=verbose, rc=ammrc)
    if (ammrc == 0) DINflux = val2_f2

    !RH: weak check, needs to be replaced:
    if (nitrc /= 0) then
        call mossco_state_get(exportState,(/ &
              'nutrients_upward_flux_at_soil_surface                            ', &
              'DIN_upward_flux_at_soil_surface                                  ', &
              'Dissolved_Inorganic_Nitrogen_DIN_nutN_upward_flux_at_soil_surface'/), &
              DINflux,ubnd=ubnd,lbnd=lbnd, verbose=verbose, rc=localrc)
        if(localrc/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        DINflux = val1_f2 + val2_f2
        ! add constant boundary flux of DIN (through groundwater, advection, rain
        DINflux = DINflux + dinflux_const/(86400.0*365.0)
    end if

    !   DIP flux:
    call mossco_state_get(exportState,(/ &
              'DIP_upward_flux_at_soil_surface                                    ', &
              'phosphate_upward_flux_at_soil_surface                              ', &
              'Dissolved_Inorganic_Phosphorus_DIP_nutP_upward_flux_at_soil_surface'/), &
              DIPflux, verbose=verbose, rc=rc)
    if (rc == 0)  then
        call mossco_state_get(importState,(/ &
              'mole_concentration_of_phosphate_upward_flux_at_soil_surface'/), &
              val1_f2, verbose=verbose, rc=rc)
         DIPflux = val1_f2 + dipflux_const/(86400.0*365.0)
    end if

      !   Det flux:
    call mossco_state_get(importState,(/'slow_detritus_C_upward_flux_at_soil_surface'/), &
      SDETCflux, verbose=verbose, rc=rc)
    call mossco_state_get(importState,(/'fast_detritus_C_upward_flux_at_soil_surface'/), &
      FDETCflux, verbose=verbose, rc=rc)
    call mossco_state_get(importState,(/'detritus-P_upward_flux_at_soil_surface'/), &
      omexDETPflux, verbose=verbose, rc=rc)

    call mossco_state_get(exportState,(/ &
            'detritus_upward_flux_at_soil_surface              ', &
            'detN_upward_flux_at_soil_surface                  ', &
            'Detritus_Nitrogen_detN_upward_flux_at_soil_surface'/), &
            DETNflux, verbose=verbose, rc=rc)
      if(rc/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      DETNflux = NC_fdet*FDETCflux + NC_sdet*SDETCflux

      !> search for Detritus-C
      call mossco_state_get(exportState,(/ &
         'Detritus_Carbon_detC_upward_flux_at_soil_surface'/),DETCflux, verbose=verbose, rc=rc)
      if (rc == 0) then
         DETCflux = FDETCflux + SDETCflux
      end if

      !> check for Detritus-P and calculate flux either N-based
      !> or as present through the Detritus-P pool
      call mossco_state_get(exportState,(/ &
          'detP_upward_flux_at_soil_surface                    ', &
          'Detritus_Phosphorus_detP_upward_flux_at_soil_surface'/),DETPflux, verbose=verbose, rc=rc)
      if (rc == 0) then
        DETPflux = omexDETPflux
      end if

      !> oxygen and odu fluxes
      call mossco_state_get(exportState,(/ &
        'oxygen_upward_flux_at_soil_surface               ', &
        'dissolved_oxygen_oxy_upward_flux_at_soil_surface '/),OXYflux, verbose=verbose, rc=oxyrc)
      call mossco_state_get(exportState,(/ &
        'dissolved_reduced_substances_odu_upward_flux_at_soil_surface'/),ODUflux, verbose=verbose, rc=odurc)
      call mossco_state_get(importState,(/'dissolved_oxygen_upward_flux_at_soil_surface'/), &
        val1_f2, verbose=verbose, rc=rc)
      call mossco_state_get(importState,(/'dissolved_reduced_substances_upward_flux_at_soil_surface'/), &
        val2_f2, verbose=verbose, rc=rc)
      if (oxyrc == 0) OXYflux = val1_f2
      if ((oxyrc == 0) .and. (odurc /= 0)) OXYflux = OXYflux - val2_f2
      if (odurc == 0) ODUflux = val2_f2

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine mcpl_Run_pre_recipe


#undef  ESMF_METHOD
#define ESMF_METHOD "mcpl_Run_pre_log"
!> @subsubsection mcpl_Run_pre_log "User Code before log creation"
!> @brief Implementation of User-Code in Run Phase,
!! after automatic recipe search, before missing log is created
!> @details Contains inventory lists reduced by found substances
!> @param
subroutine mcpl_Run_pre_log(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS

    !------------------------------------------------------------------

    rc = ESMF_SUCCESS

end subroutine mcpl_Run_pre_log


!> @todo: please check if anything can be outsourced in the general method
#undef  ESMF_METHOD
#define ESMF_METHOD "mcpl_finalize"
!> @subsubsection mcpl_finalize "User Code at end of execution"
!> @brief Implementation of User-Code in Finalize Phase,
!> @details
!> @param
subroutine mcpl_finalize(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS
    type(ESMF_Time)             :: currTime
    integer                     :: localrc
    character(len=ESMF_MAXSTR)  :: name
    !------------------------------------------------------------------

    rc = ESMF_SUCCESS

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

end subroutine mcpl_finalize




!############################################################################################
!############################################################################################
!############################################################################################







!@dev: To be outsourced (see bottom)
!----------------------------------------------------------------------
!------------------- General MOSSCO coupler Routines ------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
!> @subsubsection SetServices "SetServices Method"
!> @brief Specifies entry points for ESMF methods
!> @details Required public ESMF method
!> @param cplComp, rc Component Class and Return Code
subroutine SetServices(cplComp, rc)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    type(ESMF_CplComp)          :: cplComp
    integer, intent(out)        :: rc

    !LOCAL VARS
    integer                     :: localrc
    !------------------------------------------------------------------

    rc = ESMF_SUCCESS

    !>Register phase 0 method for ESMF
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !>Register phase 1 method for ESMF
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !>Register run method for ESMF
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !>Register finalize method for ESMF
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine SetServices


#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
!> @subsubsection InitializeP0 "Initialize Method Phase 0"
!> @brief ESMF preperation method
!> @details Allocate Space, open files, set initial conditions
!> @param cplComp, importState, exportState, parentClock, rc
!! ComponentObject; Incoming Parameters; outgoing parameters; Simulation Time; Return Code
subroutine InitializeP0(cplComp, importState, exportState, parentClock, rc)
    !------------------------------------------------------------------
    implicit none
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: parentClock
    integer, intent(out)        :: rc

    !LOCAL VARS
    integer                     :: localrc
    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    !------------------------------------------------------------------

    rc = ESMF_SUCCESS

    !> Calls incoming couple/grid component method and receives clock
    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Defines number of ESMF Initialization phases according to NUOPC standard,
    !! adds the configuration to the Component
    InitializePhaseMap(1) = "IPDv00p1=1"
    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Calls outgoing couple/grid component method and passes clock
    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
!> @subsubsection InitializeP1 "Initialize Method Phase 1"
!> @brief ESMF preperation method
!> @details Allocate Space, open files, set initial conditions
!> @param cplComp, importState, exportState, parentClock, rc
!! ComponentObject; Incoming Parameters; outgoing parameters; Simulation Time; Return Code
subroutine InitializeP1(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    implicit none

    !INPUTS / OUTPUTS
    type(ESMF_CplComp)          :: cplcomp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS
    type(ESMF_State), target    :: dba_import, dba_export
    type(ESMF_State), pointer   :: dba
    character(len=ESMF_MAXSTR)  :: name
    integer                     :: localrc, dba_rc,i,j
    real                        :: dba_value
    type(ESMF_Time)             :: currTime
    logical                     :: dba_verbose

    character(len=ESMF_MAXSTR), dimension (:), pointer &
                                :: namelist => null()
    !------------------------------------------------------------------

    rc = ESMF_SUCCESS

    !> Call user-code method
    call mcpl_InitializeP1(cplcomp, importState, exportState, externalclock, rc)

    !> @paragraph dba "Database Array States"
    !> @brief Create database array states (dba) for import and export

    !> receive coupler component information
    call MOSSCO_CompEntry(cplComp, externalClock, name=name, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    dba_import=ESMF_StateCreate(name=trim(name)//'Database Array Import', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    dba_export=ESMF_StateCreate(name=trim(name)//'Database Array Export', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Query the Import and Export State for all substances
!***@todo


    !!!Option 1: Search all entries in the database within the States
    call get_substance_list(namelist)

    do i = 1, 2
        !> run import / export database array seriell
        if (i==1) then
            dba => dba_import
        else
            dba => dba_export
        end if

        do j=1, size(namelist)
            !> search value in State

            !call mossco_state_get(importState,(/'test'/), &
            !     verbose=dba_verbose, rc=dba_rc)
        end do

    end do



!        call mossco_state_get(importState, (/'mole_concentration_of_ammonium_upward_flux_at_soil_surface'/), &
!            dba_value, verbose=verbose, rc=dba_rc)
!        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!        !
!        call mossco_state_get(dba,(/namelist(j)/),val, vb, rc=rcs)
!        call mossco_state_get(importState,namelist(j), &
!        !     !value, verbose=verbose, rc=rcs)
!        > @todo: Add further information - amount and unit
!        if (rc==ESMF_SUCCESS) call ESMF_AttributeSet(dba, namelist(j), value, rc=localrc)


!    do (i=1,2)
!        !> Get Import/Export Database Array
!        select case (i)
!            case(1)
!                dba=>dba_import
!            case(2)
!                dba=>dba_export
!        end select
!
!        !>
!        forall(j=lbound(namelist):ubound(namelist))
!            !> Search Substance in State
!            call mossco_state_get(importState,namelist(j), &
!                value, verbose=verbose, rc=rcs)
!
!            if (rc==ESMF_SUCCESS) then
!                call ESMF_AttributeSet(dba, namelist(j), value, rc=localrc)
!            end if
!        end forall
!    end do

    !!!Option 2: Search all entries in State in the database
    !repeat for import/export
    !forall
        !if () then
            !>if known by database add to dba
             !call ESMF_AttributeSet(paramState, trim(name)//'::dipflux_const', dipflux_const, rc=localrc)
             !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
             !   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        !else
            !> if unknown add to logg array
        !end if
    !stop

    !> Complete database Arrays
    call ESMF_StateAdd(importState, (/dba_import/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(exportState, (/dba_export/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @paragraph log "Log"
    !> @brief: Log unknown substances
!***@todo


end subroutine InitializeP1


#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
!> @subsubsection Run "Run Method"
!> @brief ESMF Simulation step execution method
!> @details Executes Transformations between import and export State
!> @param cplComp, importState, exportState, parentClock, rc
!! ComponentObject; Incoming Parameters; outgoing parameters; Simulation Time; Return Code
subroutine Run(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_CplComp)          :: cplcomp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS

    !------------------------------------------------------------------

    rc = ESMF_SUCCESS

    !> Call user-code method
    call mcpl_Run_pre_recipe(cplcomp, importState, exportState, externalclock, rc)

    !> Automatic Recipe Search
    ! @dev: Recursurve search in the database for fitting recipes
    ! Receive equation as string, read and execute the equation

    !> Log Substances found by automatic recipe
!***@todo

    call mcpl_Run_pre_log(cplcomp, importState, exportState, externalclock, rc)

    !> Log missing Substances
!***@todo

end subroutine Run


#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
!> @brief ESMF End of Execution method
!> @details deallocate space, close files, print results
!> @param cplComp, importState, exportState, parentClock, rc
!! ComponentObject; Incoming Parameters; outgoing parameters; Simulation Time; Return Code
subroutine Finalize(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_CplComp)          :: cplcomp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer,intent(out)         :: rc

    !LOCAL VARS
    character(len=ESMF_MAXSTR)  :: name, message, paramName
    type(ESMF_State)            :: paramState
    type(ESMF_Time)             :: currTime
    integer                     :: localrc
    type(ESMF_StateItem_Flag)   :: exportItemType, importItemType
    !------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call mcpl_finalize(cplcomp, importState, exportState, externalclock, rc)

!    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)



!    call MOSSCO_CompExit(cplComp, localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine Finalize

end module soil_pelagic_mediator



!@dev: area for all general coupler routines
!Outsource and 'use' in all Coupler modules
!----------------------------------------------------------------------
!------------------- General MOSSCO coupler Routines ------------------
!----------------------------------------------------------------------
!module mossco_mediator
!
!
!end module mossco_mediator


!####### TEMPLATE #####################################################
!#undef  ESMF_METHOD
!#define ESMF_METHOD "User_Code"
!> @brief
!> @param
!subroutine User_Code()
!    !------------------------------------------------------------------
!    !INPUTS / OUTPUTS
!
!    !LOCAL VARS
!
!    !------------------------------------------------------------------
!
!end subroutine User_Code
