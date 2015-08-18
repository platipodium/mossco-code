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
    use mossco_db


!> @todo: please check if anything can be outsourced in the general method
!----------------------------------------------------------------------
!------------------- Soil Pelagic Config ------------------------------
!----------------------------------------------------------------------

    !------------------------------------------------------------------
    implicit none

    private
    !COUPLER CONFIG
    character(len=ESMF_MAXSTR)                   :: rulesets &
                                                    ="'General', &
                                                     'HZG KW'"

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
!    type(ESMF_State), target    :: dba_import, dba_export
    type(ESMF_State), pointer   :: ste =>null()
    type(ESMF_State)            :: dba_import, dba_export


    character(len=ESMF_MAXSTR)  :: name, substanceName
    integer                     :: localrc, dba_rc,i,j, h
    real(ESMF_KIND_R8), dimension(:), pointer &
                                :: dba_value => null()
    type(ESMF_Time)             :: currTime
    logical                     :: dba_verbose
    type(ESMF_StateItem_Flag)            :: import_itemTypes(:)
    character (len=ESMF_MAXSTR)          :: import_itemNames(:)
    type(ESMF_StateItem_Flag)            :: export_itemTypes(:)
    character (len=ESMF_MAXSTR)          :: export_itemNames(:)
    integer                              :: import_itemCount,
                                            export_itemCount
    character(len=ESMF_MAXSTR),dimension(:,:),allocatable &
                                         :: dba_substances, &
                                            dba_aliases, &
                                            dba_equivalents
    !------------------------------------------------------------------

    rc = ESMF_SUCCESS

    !> Call user-code method
    call mcpl_InitializeP1(cplcomp, importState, exportState, externalclock, rc)

    !> @paragraph dba "Database Arrays"
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

    !> Get all Substances by unique name from database
    call get_substances_list(dba_substances)

    call ESMF_StateGet(dba_import, itemCount=import_itemCount)
    allocate(import_itemNames(import_itemCount))
    allocate(import_itemTypes(import_itemCount))
    allocate(export_itemNames(export_itemCount))
    allocate(export_itemTypes(export_itemCount))

    !> get list and types of all items found in import / export state
    call ESMF_StateGet(importState, itemTypeList=import_itemTypes, itemNameList=import_itemNames, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateGet(exportState, itemTypeList=export_itemTypes, itemNameList=export_itemNames, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Loop all SubstanceName - Appendices Comibnations from the database
    do j=1, size(dba_substances)
        call get_substance_aliases_list(dba_substances(j,1), rulesets,dba_aliases)
            do i=1, (size(dba_aliases)/2)

                !> Search combinations in import
                do h=1, size(import_itemNames)
                    !> If found add them to import inventory
                    if (import_itemNames(h)==dba_aliases(i,2)) then
                    !> @todo: check the TYPE of the found items too
                        call ESMF_AttributeSet(dba_import, dba_aliases(i,1), dba_aliases(i,2), rc=localrc)
                        exit
                    end if
                end do

                !> Search combinations in export
                do h=1, size(export_itemNames)
                    !> If found add them to export inventory
                    if (export_itemNames(h)==dba_aliases(i,2)) then
                        call ESMF_AttributeSet(dba_export, dba_aliases(i,1), dba_aliases(i,2), rc=localrc)
                        exit
                    end if
                end do
            end do
    end do

!    ! Loop all Substances in database
!    do j=1, size(dba_substances)
!        call get_substance_appendices_list(dba_substances(j,1), dba_appendices)
!        ! Loop all appendices for the substance
!        do i=1, size(dba_appendices)
!            ! Retreive set of equivalent name-appendix combinations
!            call get_substance_appendix_aliases_list(dba_substances(j,1), dba_appendices(i,1), rulesets, dba_equivalents)
!            call ESMF_StateGet(importState, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localRc)
!
!            !call mossco_state_get(importState, dba_equivalents, dba_value, rc=dba_rc)
!***********@todo: cast dba_equivalents as 1D array
!
!            if (dba_rc==ESMF_SUCCESS) then
!                call ESMF_AttributeSet(dba_import, dba_substances(j,1) // dba_appendices(i,1), substanceName, rc=localrc)


!***********@todo:: Attributes sind für Parameter, nicht für Pointer!
!***********@todo:: ESMF State gettate

!StateGet: Liste aller Namen und Typen in einem State
!***********ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localRc)
!    do i=1,itemCount
!      if (itemtypeList(i) == ESMF_STATEITEM_FIELD) then
!        write(message,'(A)')  trim(name)//' field'


                !Beschreiben des Export States:
                !Werte unverändert füllen
                !Warnung bei mehrfachem Beschreiben
                !z.B. dissolved_oxygen und oxygen --> Beide mit gleichen Daten füllen aber dies
                !in der Liste / im Log vermerken
!                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!                    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!            end if
!
!            ! Search combinations in export and add them to inventory State
!            !call mossco_state_get(exportState, dba_equivalents, dba_value, rc=dba_rc)
!            if (dba_rc==ESMF_SUCCESS) then
!                call ESMF_AttributeSet(dba_export, dba_substances(j,1) // dba_appendices(i,1), dba_value, rc=localrc)
!                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
!                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!            end if
!        end do
!    end do

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
    !> @brief: Log with found and not found substances

    !> @todo: ESMF log
    call ESMF_StateGet(dba_import, itemTypeList=import_itemTypes, itemNameList=import_itemNames, rc=localRc)
    write(*,*) "Found the following substances in import state:"
    write(*,'(A)') import_itemNames

    call ESMF_StateGet(dba_export, itemTypeList=export_itemTypes, itemNameList=export_itemNames, rc=localRc)
    write(*,*) "Found the following substances in import state:"
    write(*,'(A)') export_itemNames


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
    type(ESMF_State)                       :: dba_import, dba_export
    type(ESMF_StateItem_Flag)              :: import_itemTypes(:), &
                                              export_itemTypes(:)

    real(ESMF_KIND_R8),dimension(:,:),   pointer :: field=>null()

    character(len=ESMF_MAXSTR),dimension(1:3) &
                                           :: substances_import, &
                                              substances_export

    character (len=ESMF_MAXSTR)            :: import_itemNames(:), &
                                              export_itemNames(:), &
                                              val
    character(len=ESMF_MAXSTR),allocatable :: a_imp, a_exp

    integer                                :: import_itemCount, &
                                              export_itemCount, &
                                              c, c_imp, c_exp, i, j, n
    !------------------------------------------------------------------

    rc = ESMF_SUCCESS

    !> Call user-code method
    call mcpl_Run_pre_recipe(cplcomp, importState, exportState, externalclock, rc)

    !> receive coupler component information
    call MOSSCO_CompEntry(cplComp, externalClock, name=name, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @paragraph: automated_copy  "Automated copy of Substances 
    !> @brief: Substances directly found in Import State are copied to the export State

    call mossco_state_get(importState, (/'dba_import'/), dba_import, verbose=verbose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call mossco_state_get(exportState, (/'dba_export'/), dba_export, verbose=verbose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Dimension the Names and Types Arrays for ESMF_StateGet
    call ESMF_StateGet(dba_import, itemCount=import_itemCount)
    allocate(import_itemNames(import_itemCount))
    allocate(import_itemTypes(import_itemCount))
    allocate(export_itemNames(export_itemCount))
    allocate(export_itemTypes(export_itemCount))

    !> Receive the Name and Type lists
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateGet(dba_import, itemTypeList=import_itemTypes, itemNameList=import_itemNames, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateGet(exportState, itemTypeList=export_itemTypes, itemNameList=export_itemNames, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Prepare import array
    c_imp = import_itemCount - sum(index(import_itemNames, "_used"))
    allocate(a_imp((6*c_imp)-1))
    a_imp=repeat("0,0,0,",c_imp)
    substances_import=reshape((/a_imp/,(/c_imp,2/))

    j=0
    !> Read import inventory to array
    do i=1, size(import_itemCount)
        !> Handle Substance Names
        if (index(import_itemNames(i),"_used")==0) then
            j=j+1
            !> Search for used attribute
            call ESMF_AttributeGet(dba_import, import_itemNames(i),"_used", val, rc=localrc)
            if (localrc==ESMF_SUCCESS) then
                used=val
            else
                used=0
            end if
            !> Get Equivalent
            call ESMF_AttributeGet(dba_import, import_itemNames(i), val, rc=localrc)
            !> Save values
            substances_import(j,1) = import_itemNames(i)    !> Substances_import(1) = Substance Name
            substances_import(j,2) = val                    !> Substances_import(2) = Substance Equivalent
            substances_import(j,3) = used                   !> Substances_import(3) = used attribute
        end if
    end do

    !> Prepare export array
    c_exp = export_itemCount - sum(index(export_itemNames, "_used"))
    allocate(a_exp((6*c_exp)-1))
    a_imp=repeat("0,0,0,",c_exp)
    substances_export=reshape((/a_exp/,(/c_exp,2/))
    !> Read export inventory to array

    j=0
    !> Read import inventory to array
    do i=1, size(export_itemCount)
        !> Handle Substance Names
        if (index(export_itemNames(i),"_used")==0) then
            j=j+1
            !> Search for used attribute
            call ESMF_AttributeGet(dba_export, export_itemNames(i),"_used", val, rc=localrc)
            !> Get Equivalent
            call ESMF_AttributeGet(dba_export, export_itemNames(i), val, rc=localrc)
            !> Save values
            substances_export(j,1) = export_itemNames(i)    !> Substances_export(1) = Substance Name
            substances_export(j,2) = val                    !> Substances_export(2) = Substance Equivalent
            substances_export(j,3) = 0                      !> Substances_export(3) = Found attribute
        end if
    end do

    !> Find the lowest used number for the substance
    do i=1, c_exp
        n=2147483647
        do j=1, c_imp
            if ((substances_export(i,1) == substances_import(j,1)) &
                .and. substances_import(j,3)<n ) n=j
        end do

        if (n<2147483647) then
            call mossco_state_get(importState, (//), field, rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
                    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            call ESMF_StateAdd(importState, (/paramState/), rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
                    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            write(*,*) "Found required Substance for export, used ", n, " times:"
            write(*,*) substances_export(i,1), " found as '", substances_import(n,2), "'."
        end if
    end do

    !try to find already used hit

    !> @paragraph: automated_recipe "Automatic Recipe Search"
    !> @brief: Recursurve search in the database for fitting recipes

    ! @dev: Implementation
    ! Receive equation as string, read and execute the equation

    !> @paragraph: final_log "Final log"
    !> @brief: Log found and not found substances

    !> Log missing Substances

    !> @todo: Implementation

    call mcpl_Run_pre_log(cplcomp, importState, exportState, externalclock, rc)

    call MOSSCO_CompExit(cplComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
