!> @brief Implementation of an ESMF toplevel coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
module toplevel_component

  use esmf

  ! Registration routines for fabm
  use fabm_sediment_component, only : fabmsed_SetServices => SetServices
  use constant_component, only : constant_SetServices => SetServices
  use gotm_component, only : gotm_SetServices => SetServices
  use fabm_gotm_component, only : fabm_gotm_SetServices => SetServices
  use surfaces_coupler, only: surfaces_SetServices => SetServices

  use mossco_state

  implicit none

  private

  public SetServices

  type(ESMF_CplComp), save  :: surfacesCplComp
  type(ESMF_GridComp),save  :: fabmsedComp, constantComp, gotmComp, fabmgotmComp
  type(ESMF_State),save     :: state,pelagicstate,sedimentstate
  type(ESMF_GRID) :: pelagic_bdy_grid,flux_bdy_grid
  type(ESMF_ARRAYSPEC) :: pelagic_bdy_array,flux_bdy_array
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DETN,DIN,vDETN,DIP,DETC,DETP,vDETC,vDETP
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3,val1_f3,val2_f3
  real(ESMF_KIND_R8),dimension(:,:), pointer :: ptr_f2,val1_f2,val2_f2
  real(ESMF_KIND_R8),dimension(:,:), pointer :: DETNflux,DETCflux,DETPflux,DINflux,DIPflux
  integer :: nvar,nvars_pel,nvars_sed

  contains

  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: temperatureField, field, newfield
    type(ESMF_FieldBundle) :: fieldBundle
    integer               :: petCount, localPet
    real(ESMF_KIND_R8),dimension(:,:),allocatable :: farray

    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)

    ! Create component, call setservices, and create states
    gotmComp = ESMF_GridCompCreate(name="gotmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gotmComp,gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    fabmgotmComp = ESMF_GridCompCreate(name="fabmgotmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(fabmgotmComp,fabm_gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    fabmsedComp = ESMF_GridCompCreate(name="fabmsedComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(fabmsedComp,fabmsed_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    constantComp = ESMF_GridCompCreate(name="constantComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(constantComp,constant_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    surfacesCplComp = ESMF_CplCompCreate(name="surfacesCplComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetServices(surfacesCplComp,surfaces_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
   
    ! States for exchange (only one generic)
    ! GOTM and constant don't have import states, FABM both, sed both
    ! GOTM export goes into FABMGOTM and is filled, then passed on to sediment
    state = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="Exchange state")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    pelagicstate = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="Pelagic state")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    sedimentstate = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="Sediment state")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(constantComp, importState=pelagicState, exportState=pelagicstate,clock=parentClock,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(gotmComp, importState=pelagicstate, exportState=pelagicstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(fabmgotmComp, importState=pelagicstate, exportState=pelagicstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(fabmsedComp, importState=pelagicstate, exportState=sedimentstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StatePrint(pelagicState)

    call ESMF_CplCompInitialize(surfacesCplComp, importState=pelagicstate, exportState=sedimentstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_CplCompInitialize(surfacesCplComp, importState=sedimentState, exportState=pelagicstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StatePrint(pelagicState)

    call ESMF_CplCompInitialize(surfacesCplComp, importState=pelagicstate, exportState=sedimentstate, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


    ! create exchange fields
    call ESMF_ArraySpecSet(flux_bdy_array, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ArraySpecSet(pelagic_bdy_array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    flux_bdy_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/1,1/), &
      regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="sediment fluxes grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    pelagic_bdy_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/1,1,1/), &
      regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="pelagic states grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="dissolved_nitrate", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0d0
    call ESMF_StateAddReplace(pelagicstate,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="dissolved_ammonium", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0d0
    call ESMF_StateAddReplace(pelagicstate,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="dissolved_reduced_substances", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0d0
    call ESMF_StateAddReplace(pelagicstate,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="fast_detritus_C", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0d0
    call ESMF_StateAddReplace(pelagicstate,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="slow_detritus_C", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0d0
    call ESMF_StateAddReplace(pelagicstate,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="detritus-P", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0d0
    call ESMF_StateAddReplace(pelagicstate,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="fast_detritus_C_z_velocity", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 1.0d0
    call ESMF_StateAddReplace(pelagicstate,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="slow_detritus_C_z_velocity", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 1.0d0
    call ESMF_StateAddReplace(pelagicstate,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="detritus-P_z_velocity", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 1.0d0
    call ESMF_StateAddReplace(pelagicstate,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

#if 1
    call mossco_create_upward_flux_fields(pelagicstate, (/&
           "nutrients                              ", &
           "Dissolved_Inorganic_Nitrogen_DIN_nutN  ", &
           "Dissolved_Inorganic_Phosphorus_DIP_nutP", &
           "detritus                               ", &
           "Detritus_Nitrogen_detN                 ", &
           "Detritus_Phosphorus_detP               ", &
           "Detritus_Carbon_detC                   "/),sedimentstate)
#else

    newfield = ESMF_FieldCreate(flux_bdy_grid,flux_bdy_array, &
                       name="nutrients_upward_flux", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f2, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f2 = 0.0d0
    call ESMF_StateAddReplace(sedimentstate,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(flux_bdy_grid,flux_bdy_array, &
                       name="detritus_upward_flux", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f2, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f2 = 0.0d0
    call ESMF_StateAddReplace(sedimentstate,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
#endif

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO)

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc
    type(ESMF_Field)      :: field
    real(ESMF_KIND_R8),parameter    :: sinking_factor=0.3d0 !30% of Det sinks into sediment
    real(ESMF_KIND_R8)    :: CN_det=106.0d0/16.0d0
    !> @todo read NC_fdet dynamically from fabm model info?  This would not comply with our aim to separate fabm/esmf
    real(ESMF_KIND_R8),parameter    :: NC_fdet=0.20d0
    real(ESMF_KIND_R8),parameter    :: NC_sdet=0.04d0
    real(ESMF_KIND_R8)    :: fac_fdet
    real(ESMF_KIND_R8)    :: fac_sdet
    !> fdet + sdet = CN_det*det
    !> NC_fdet*fdet + NC_sdet*sdet = det
    !> fdet = fac_fdet*det
    !> sdet = fac_sdet*det

    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero


    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO)

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))

      !> change state such that gotm-fabm output is mapped to fabmsed fields:
      !   DIN flux:
      call ESMF_StateGet(sedimentstate,trim('dissolved_nitrate_upward_flux'),field,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=val1_f2,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateGet(sedimentstate,'dissolved_ammonium_upward_flux',field,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=val2_f2,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call mossco_state_get(sedimentstate,(/ &
              'nutrients_upward_flux                            ', &
              'Dissolved_Inorganic_Nitrogen_DIN_nutN_upward_flux'/),DINflux,rc=rc)
      DINflux(1,1) = val1_f2(1,1) + val2_f2(1,1)

      !   DIP flux:
      call mossco_state_get(sedimentstate,(/ &
              'Dissolved_Inorganic_Phosphorus_DIP_nutP_upward_flux'/),DIPflux,rc=rc)
      if (rc == 0)  then
        call mossco_state_get(sedimentstate,(/ &
              'dissolved_phosphate_upward_flux'/),val1_f2,rc=rc)
         DIPflux(1,1) = val1_f2(1,1)
      end if

      !   Det flux:
      call mossco_state_get(pelagicstate,(/ &
              'detritus              ', &
              'Detritus_Nitrogen_detN'/),DETN,rc=rc)
      call mossco_state_get(pelagicstate,(/ &
              'detritus_z_velocity              ', &
              'Detritus_Nitrogen_detN_z_velocity'/),vDETN,rc=rc)
      call mossco_state_get(sedimentstate,(/ &
              'detritus_upward_flux              ', &
              'Detritus_Nitrogen_detN_upward_flux'/),DETNflux,rc=rc)
      DETNflux(1,1) = sinking_factor * DETN(1,1,1) * vDETN(1,1,1)

      !> search for Detritus-C, if present, use Detritus C-to-N ratio and apply flux
      call mossco_state_get(pelagicstate,(/'Detritus_Carbon_detC'/),DETC,rc=rc)
      if (rc /= 0) then
         CN_det=106.0d0/16.0d0
      else
         CN_det = DETC(1,1,1)/DETN(1,1,1)
         call mossco_state_get(sedimentstate,(/ &
              'Detritus_Carbon_detC_upward_flux'/),DETCflux,rc=rc)
         DETCflux(1,1) = sinking_factor * DETC(1,1,1) * vDETN(1,1,1)
      end if
      fac_fdet = (1.0d0-NC_sdet*CN_det)/(NC_fdet-NC_sdet)
      fac_sdet = (1.0d0-NC_fdet*CN_det)/(NC_sdet-NC_fdet)

      call ESMF_StateGet(pelagicstate,'fast_detritus_C',field,rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      ptr_f3(1,1,1) = fac_fdet * DETNflux(1,1)
      call ESMF_StateGet(pelagicstate,'slow_detritus_C',field,rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      ptr_f3(1,1,1) = fac_sdet * DETNflux(1,1)
   
      !> check for Detritus-P and calculate flux either N-based
      !> or as present through the Detritus-P pool
      call mossco_state_get(pelagicstate,(/'Detritus_Phosphorus_detP'/),DETP,rc=rc)
      if (rc == 0) then
        call mossco_state_get(pelagicstate,(/ &
              'Detritus_Phosphorus_detP_z_velocity'/),vDETP,rc=rc)
        call mossco_state_get(sedimentstate,(/ &
              'Detritus_Phosphorus_detP_upward_flux'/),DETPflux,rc=rc)
        DETPflux(1,1) = sinking_factor * DETP(1,1,1) * vDETP(1,1,1)
      else
        if (.not.(associated(DETPflux))) allocate(DETPflux(1,1))
        DETPflux(1,1) = 1.0d0/16.0d0 * DETNflux(1,1)
      end if

      call ESMF_StateGet(pelagicstate,'detritus-P',field,rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      ptr_f3(1,1,1) = DETPflux(1,1)
      ! DIM concentrations:
      !  oxygen is coming from constant component, ODU is set to 0.0 in Initialize
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call mossco_state_get(pelagicstate,(/ &
              'nutrients                            ', &
              'Dissolved_Inorganic_Nitrogen_DIN_nutN'/),DIN,rc=rc)
      call ESMF_StateGet(pelagicstate,'dissolved_ammonium',field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f3(1,1,1) = 0.5d0 * DIN(1,1,1)
      call ESMF_StateGet(pelagicstate,'dissolved_nitrate',field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f3(1,1,1) = 0.5d0 * DIN(1,1,1)

      !> check for DIP, if present, take as is, if not calculate it N-based
      call mossco_state_get(pelagicstate,(/'Dissolved_Inorganic_Phosphorus_DIP_nutP'/),DIP,rc=rc)
      if (rc /= 0) then
        if (.not.(associated(DIP))) allocate(DIP(1,1,1))
        DIP(1,1,1) = 1.0d0/16.0d0 * DIN(1,1,1)
      end if
      call ESMF_StateGet(pelagicstate,'dissolved_phosphate',field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f3(1,1,1) = DIP(1,1,1)

      call ESMF_GridCompRun(gotmComp, importState=pelagicstate, exportState=pelagicstate, clock=parentClock, rc=rc)
      call ESMF_GridCompRun(fabmgotmComp, importState=sedimentstate, exportState=pelagicstate, clock=parentClock, rc=rc)
      call ESMF_GridCompRun(fabmsedComp, importState=pelagicstate, exportState=sedimentstate, clock=parentClock, rc=rc)

      call ESMF_ClockAdvance(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    enddo 

    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_INFO)
 
  end subroutine Run

  subroutine mossco_create_upward_flux_fields(inputstate, name, outputstate)
  type(ESMF_State)  :: inputstate,outputstate
  character(len=*),dimension(:) :: name
  type(ESMF_Field)  :: newfield
  integer           :: rc,i,itemcount
  type(ESMF_ArraySpec) :: flux_bdy_array
  type(ESMF_Grid)      :: flux_bdy_grid

  ! create arraySpec and grid for flux fields
  call ESMF_ArraySpecSet(flux_bdy_array, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  flux_bdy_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/1,1/), &
      regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="sediment fluxes grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  do i=1,ubound(name,1)
    ! if name present in inputstate, then create corresponding flux field
    call ESMF_StateGet(inputstate, itemSearch=trim(name(i)), itemCount=itemcount,rc=rc)
    if (itemcount>0) then
      newfield = ESMF_FieldCreate(flux_bdy_grid,flux_bdy_array, &
                       name=trim(name(i))//'_upward_flux', &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f2, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f2 = 0.0d0
      call ESMF_StateAddReplace(outputstate,(/newfield/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end if
  end do
  end subroutine mossco_create_upward_flux_fields  

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4)  :: phase, maxPhaseCount=2
    integer(ESMF_KIND_I4), allocatable  :: phaseCountList(:)
    logical                :: hasPhaseZero


    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)

    call ESMF_GridCompFinalize(fabmgotmComp, exportState=state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(fabmgotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(gotmComp, exportState=state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(fabmsedComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(fabmsedComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(constantComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(constantComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateDestroy(state,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateDestroy(pelagicstate,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateDestroy(sedimentstate,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module toplevel_component
