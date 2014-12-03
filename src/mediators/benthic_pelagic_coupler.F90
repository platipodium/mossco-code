!> @brief Implementation of an ESMF link coupling
!>
!> This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2014, Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "benthic_pelagic_coupler.F90"

module benthic_pelagic_coupler
    
  use esmf
  use fabm_sediment_component, only : rk
  use mossco_state
  use mossco_component

  implicit none

  private
  type(ESMF_GRID) :: flux_bdy_grid
  type(ESMF_ARRAYSPEC) :: flux_bdy_array
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DETN,DIN,vDETN
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DIP,DETP,vDETP
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: vDETC,DETC
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: ptr_f2,val1_f2,val2_f2
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: DETNflux,DETPflux,DETCflux,DINflux,DIPflux,OXYflux
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: SDETCflux,fDETCflux,omexDETPflux
  real(ESMF_KIND_R8) :: dinflux_const
  real(ESMF_KIND_R8) :: dipflux_const=-1.
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    type(ESMF_Time)       :: currTime
    integer              :: nmlunit=127, localrc
    namelist /benthic_pelagic_coupler/ dinflux_const,dipflux_const

    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !read namelist
    open(nmlunit,file='benthic_pelagic_coupler.nml',action='read',status='old')
    read(nmlunit,benthic_pelagic_coupler)
    close(nmlunit)
    if (dipflux_const < 0.0) dipflux_const=dinflux_const/16.0_rk

    ! create exchange fields
    !> @todo: get grid size from exportState (so far using 1x1 horizontal grid
    call ESMF_ArraySpecSet(flux_bdy_array, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    flux_bdy_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
      maxIndex=(/1,1/), &
      regDecomp=(/1,1/), &
      coordSys=ESMF_COORDSYS_SPH_DEG, &
      indexflag=ESMF_INDEX_GLOBAL,  &
      name="pelagic states grid", &
      coordTypeKind=ESMF_TYPEKIND_R8, &
      coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    ! create coupler fields
    call create_optional_fields_from_names(exportState, (/&
           "nutrients_upward_flux_at_soil_surface                              ", &
           "nitrate_upward_flux_at_soil_surface                                ", &
           "ammonium_upward_flux_at_soil_surface                               ", &
           "phosphate_upward_flux_at_soil_surface                              ", &
           "oxygen_upward_flux_at_soil_surface                                 ", &
           "phosphate_upward_flux_at_soil_surface                              ", &
           "DIN_upward_flux_at_soil_surface                                    ", &
           "DIP_upward_flux_at_soil_surface                                    ", &
           "detN_upward_flux_at_soil_surface                                   ", &
           "detP_upward_flux_at_soil_surface                                   ", &
           "Dissolved_Inorganic_Nitrogen_DIN_nutN_upward_flux_at_soil_surface  ", &
           "Dissolved_Inorganic_Phosphorus_DIP_nutP_upward_flux_at_soil_surface", &
           "detritus_upward_flux_at_soil_surface                               ", &
           "Detritus_Nitrogen_detN_upward_flux_at_soil_surface                 ", &
           "Detritus_Phosphorus_detP_upward_flux_at_soil_surface               ", &
           "Detritus_Carbon_detC_upward_flux_at_soil_surface                   "/),flux_bdy_grid)

    !> allocate temporary arrays
    allocate(DETNflux(1,1))
    allocate(DETPflux(1,1))

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"

  subroutine Run(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc
    integer              :: ammrc,nitrc,oxyrc

    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime, stopTime
    integer                     :: localrc
    integer                     :: myrank
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    type(ESMF_Field)            :: field
    !> @todo read NC_fdet dynamically from fabm model info?  This would not comply with our aim to separate fabm/esmf
    real(ESMF_KIND_R8),parameter    :: NC_fdet=0.20_rk
    real(ESMF_KIND_R8),parameter    :: NC_sdet=0.04_rk

    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !   DIN flux:
      call ESMF_StateGet(importState,trim('mole_concentration_of_nitrate_upward_flux_at_soil_surface'),field,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=val1_f2,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateGet(importState,'mole_concentration_of_ammonium_upward_flux_at_soil_surface',field,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=val2_f2,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call mossco_state_get(exportState,(/'nitrate_upward_flux_at_soil_surface'/),DINflux,rc=nitrc)
      if (nitrc == 0) DINflux = val2_f2
      call mossco_state_get(exportState,(/'ammonium_upward_flux_at_soil_surface'/),DINflux,rc=nitrc)
      if (nitrc == 0) DINflux = val1_f2

      !RH: weak check, needs to be replaced:
      if (nitrc /= 0) then
        call mossco_state_get(exportState,(/ &
              'nutrients_upward_flux_at_soil_surface                            ', &
              'DIN_upward_flux_at_soil_surface                                  ', &
              'Dissolved_Inorganic_Nitrogen_DIN_nutN_upward_flux_at_soil_surface'/),DINflux,rc=rc)
        if(rc/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        DINflux = val1_f2 + val2_f2
        ! add constant boundary flux of DIN (through groundwater, advection, rain
        DINflux = DINflux + dinflux_const/(86400.0*365.0)
      end if

      !   DIP flux:
      call mossco_state_get(exportState,(/ &
              'DIP_upward_flux_at_soil_surface                                    ', &
              'phosphate_upward_flux_at_soil_surface                              ', &
              'Dissolved_Inorganic_Phosphorus_DIP_nutP_upward_flux_at_soil_surface'/),DIPflux,rc=rc)
      if (rc == 0)  then
        call mossco_state_get(importState,(/ &
              'mole_concentration_of_phosphate_upward_flux_at_soil_surface'/),val1_f2,rc=rc)
         DIPflux = val1_f2 + dipflux_const/(86400.0*365.0)
      end if

      !   Det flux:
      call mossco_state_get(importState,(/'slow_detritus_C_upward_flux_at_soil_surface'/),SDETCflux,rc=rc)
      call mossco_state_get(importState,(/'fast_detritus_C_upward_flux_at_soil_surface'/),FDETCflux,rc=rc)
      call mossco_state_get(importState,(/'detritus-P_upward_flux_at_soil_surface'/),omexDETPflux,rc=rc)

      call mossco_state_get(exportState,(/ &
            'detritus_upward_flux_at_soil_surface              ', &
            'detN_upward_flux_at_soil_surface                  ', &
            'Detritus_Nitrogen_detN_upward_flux_at_soil_surface'/),DETNflux,rc=rc)
      if(rc/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      DETNflux = NC_fdet*FDETCflux + NC_sdet*SDETCflux

      !> search for Detritus-C
      call mossco_state_get(exportState,(/ &
         'Detritus_Carbon_detC_upward_flux_at_soil_surface'/),DETCflux,rc=rc)
      if (rc == 0) then
         DETCflux = FDETCflux + SDETCflux
      end if

      !> check for Detritus-P and calculate flux either N-based
      !> or as present through the Detritus-P pool
      call mossco_state_get(exportState,(/ &
          'detP_upward_flux_at_soil_surface                    ', &
          'Detritus_Phosphorus_detP_upward_flux_at_soil_surface'/),DETPflux,rc=rc)
      if (rc == 0) then
        DETPflux = omexDETPflux
      end if

      !> oxygen and odu fluxes
      call mossco_state_get(exportState,(/'oxygen_upward_flux_at_soil_surface'/),OXYflux,rc=rc)
      if (rc == 0) then
        call mossco_state_get(importState,(/'dissolved_oxygen_upward_flux_at_soil_surface'/),val1_f2,rc=rc)
        call mossco_state_get(importState,(/'dissolved_reduced_substances_upward_flux_at_soil_surface'/),val2_f2,rc=rc)
        OXYflux = val1_f2 - val2_f2
      end if

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(cplcomp, importState, exportState, externalclock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer,intent(out)  :: rc
     
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)       :: currTime
    integer :: localrc
    
    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !call ESMF_ArraySpecDestroy(flux_bdy_array, rc)
    call ESMF_GridDestroy(flux_bdy_grid, rc=rc)

    if (associated(DETNflux)) deallocate(DETNflux)
    if (associated(DETPflux)) deallocate(DETPflux)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize


end module benthic_pelagic_coupler

