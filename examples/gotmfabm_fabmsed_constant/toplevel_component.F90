module esmf_toplevel_component

  use esmf

  ! Registration routines for fabm
  use fabm_sediment_component, only : fabmsed_SetServices => SetServices
  use fabm_sediment_component, only : bdys,fluxes,rk
  use constant_component, only : constant_SetServices => SetServices
  use gotm_component, only : gotm_SetServices => SetServices

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),save  :: fabmComp,constantComp, gotmComp
  type(ESMF_State),save     :: fabmExp, fabmImp, gotmExp, gotmImp
  type(ESMF_GRID) :: pelagic_bdy_grid,flux_bdy_grid
  type(ESMF_ARRAYSPEC) :: pelagic_bdy_array,flux_bdy_array
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3,val1_f3,val2_f3
  real(ESMF_KIND_R8),dimension(:,:), pointer :: ptr_f2,val1_f2,val2_f2
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

    call ESMF_LogWrite("Toplevel component initializing ... ",ESMF_LOGMSG_INFO)

    ! Create component, call setservices, and create states
    gotmComp = ESMF_GridCompCreate(name="gotmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gotmComp,gotm_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    fabmComp = ESMF_GridCompCreate(name="fabmComp", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(fabmComp,fabmsed_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    constantComp = ESMF_GridCompCreate(name="constantComp",rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(constantComp,constant_SetServices, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    gotmImp = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="gotmImp")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    gotmExp = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="gotmExp")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    fabmImp = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="fabmImp")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    fabmExp = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED,name="fabmExp")
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(constantComp, importState=gotmExp, exportState=fabmImp,clock=parentClock,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(gotmComp, importState=gotmImp, exportState=fabmImp, clock=parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(fabmComp, importState=fabmImp, exportState=fabmExp, clock=parentClock, rc=rc)

    ! create exchange fields in fabmImp and gotmExp:

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
                       name="hzg_omexdia_p dissolved nitrate", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0_rk
    call ESMF_StateAddReplace(fabmImp,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="hzg_omexdia_p dissolved ammonium", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0_rk
    call ESMF_StateAddReplace(fabmImp,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="hzg_omexdia_p dissolved reduced substances", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0_rk
    call ESMF_StateAddReplace(fabmImp,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="hzg_omexdia_p fast detritus C", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0_rk
    call ESMF_StateAddReplace(fabmImp,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="hzg_omexdia_p slow detritus C", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0_rk
    call ESMF_StateAddReplace(fabmImp,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="hzg_omexdia_p detritus-P", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 0.0_rk
    call ESMF_StateAddReplace(fabmImp,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="hzg_omexdia_p fast detritus C_z_velocity", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 1.0_rk
    call ESMF_StateAddReplace(fabmImp,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="hzg_omexdia_p slow detritus C_z_velocity", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 1.0_rk
    call ESMF_StateAddReplace(fabmImp,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(pelagic_bdy_grid,pelagic_bdy_array, &
                       name="hzg_omexdia_p detritus-P_z_velocity", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f3 = 1.0_rk
    call ESMF_StateAddReplace(fabmImp,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(flux_bdy_grid,flux_bdy_array, &
                       name="gotm_npzd nutrients_upward_flux", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f2, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f2 = 0.0_rk
    call ESMF_StateAddReplace(gotmExp,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    newfield = ESMF_FieldCreate(flux_bdy_grid,flux_bdy_array, &
                       name="gotm_npzd detritus_upward_flux", &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field=newfield, localDe=0, farrayPtr=ptr_f2, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    ptr_f2 = 0.0_rk
    call ESMF_StateAddReplace(gotmExp,(/newfield/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_LogWrite("Toplevel component initialized",ESMF_LOGMSG_INFO)

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc
    type(ESMF_Field)      :: field
    real(ESMF_KIND_R8),parameter    :: sinking_factor=0.3d0 !30% of Det sinks into sediment

    call ESMF_LogWrite("Toplevel component running ... ",ESMF_LOGMSG_INFO)

    do while (.not. ESMF_ClockIsStopTime(parentClock, rc=rc))

      call ESMF_ClockAdvance(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! change fabmImp such that gotm-fabm output is mapped to fabmsed fields:
      !   DIN flux:
      call ESMF_StateGet(fabmExp,trim('hzg_omexdia_p dissolved nitrate_upward_flux'),field,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=val1_f2,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateGet(fabmExp,'hzg_omexdia_p dissolved ammonium_upward_flux',field,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=val2_f2,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateGet(gotmExp,'gotm_npzd nutrients_upward_flux',field,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f2,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f2 = val1_f2 + val2_f2
      !   DetN flux:
      call ESMF_StateGet(fabmImp,'gotm_npzd detritus',field,rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=val1_f3,rc=rc)
      call ESMF_StateGet(fabmImp,'gotm_npzd detritus_z_velocity',field,rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=val2_f3,rc=rc)
      call ESMF_StateGet(gotmExp,'gotm_npzd detritus_upward_flux',field,rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f2,rc=rc)
      ptr_f2 = sinking_factor * val1_f3(:,:,1) * val2_f3(:,:,1)
      call ESMF_StateGet(fabmImp,'hzg_omexdia_p fast detritus C',field,rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      ptr_f3 = sinking_factor * 106.0d0/16.0d0 * 0.5d0 * val1_f3 * val2_f3
      call ESMF_StateGet(fabmImp,'hzg_omexdia_p slow detritus C',field,rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      ptr_f3 = sinking_factor * 106.0d0/16.0d0 * 0.5d0 * val1_f3 * val2_f3
      call ESMF_StateGet(fabmImp,'hzg_omexdia_p detritus-P',field,rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      ptr_f3 = sinking_factor * 1.0d0/16.0d0 * val1_f3 * val2_f3
      ! DIM concentrations:
      !  oxygen is coming from constant component, ODU is set to 0.0 in Initialize
      call ESMF_StateGet(fabmImp,'gotm_npzd nutrients',field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=val1_f3,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateGet(fabmImp,'hzg_omexdia_p dissolved ammonium',field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f3 = 0.5d0 * val1_f3
      call ESMF_StateGet(fabmImp,'hzg_omexdia_p dissolved nitrate',field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f3 = 0.5d0 * val1_f3
      call ESMF_StateGet(fabmImp,'hzg_omexdia_p dissolved phosphate',field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f3,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f3 = 1.0d0/16.0d0 * val1_f3
      
      call ESMF_GridCompRun(gotmComp, importState=gotmExp, exportState=fabmImp, clock=parentClock, rc=rc)
      call ESMF_GridCompRun(fabmComp, importState=fabmImp, exportState=fabmExp, clock=parentClock, rc=rc)

    enddo 

    call ESMF_LogWrite("Toplevel component finished running. ",ESMF_LOGMSG_INFO)
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    call ESMF_LogWrite("Toplevel component finalizing",ESMF_LOGMSG_INFO)

    call ESMF_GridCompFinalize(gotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gotmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(fabmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(fabmComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(constantComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(constantComp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("Toplevel component finalized",ESMF_LOGMSG_INFO)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module esmf_toplevel_component
