module benthic_pelagic_coupler
    
  use esmf
  use fabm_sediment_component, only : rk
  use mossco_state

  implicit none

  private
  type(ESMF_GRID) :: flux_bdy_grid
  type(ESMF_ARRAYSPEC) :: flux_bdy_array
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DETN,DIN,vDETN
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DIP,DETP,vDETP
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: vDETC,DETC
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: ptr_f2,val1_f2,val2_f2
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: DETNflux,DETPflux,DETCflux,DINflux,DIPflux

  public SetServices

  contains

  subroutine SetServices(cplcomp, rc)

    type(ESMF_CplComp)   :: cplcomp
    integer, intent(out) :: rc

    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, Initialize  &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_RUN,    Run   &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_FINALIZE, Finalize &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine SetServices

  subroutine Initialize(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    type(ESMF_Field)     :: newfield
    integer, intent(out) :: rc

    call ESMF_LogWrite("benthic-pelagic coupler initializing", ESMF_LOGMSG_INFO)

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
    !> @todo: check in exportState for required quantities and create those fields
    call mossco_create_upward_flux_fields(exportState, (/&
           "nutrients                              ", &
           "Dissolved_Inorganic_Nitrogen_DIN_nutN  ", &
           "Dissolved_Inorganic_Phosphorus_DIP_nutP", &
           "detritus                               ", &
           "Detritus_Nitrogen_detN                 ", &
           "Detritus_Phosphorus_detP               ", &
           "Detritus_Carbon_detC                   "/),importState)

    !> allocate temporary arrays
    allocate(DETNflux(1,1))
    allocate(DETPflux(1,1))

    call ESMF_LogWrite("benthic-pelagic coupler initialized", ESMF_LOGMSG_INFO)

  end subroutine Initialize


  subroutine Run(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc

    integer                     :: myrank
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    character (len=ESMF_MAXSTR) :: message
    type(ESMF_Field)            :: field
    real(ESMF_KIND_R8),parameter    :: sinking_factor=0.3d0 !> 30% of Det sinks into sediment
    real(ESMF_KIND_R8)    :: CN_det=106.0_rk/16.0_rk
    !> @todo read NC_fdet dynamically from fabm model info?  This would not comply with our aim to separate fabm/esmf
    real(ESMF_KIND_R8),parameter    :: NC_fdet=0.20_rk
    real(ESMF_KIND_R8),parameter    :: NC_sdet=0.04_rk
    real(ESMF_KIND_R8)    :: fac_fdet
    real(ESMF_KIND_R8)    :: fac_sdet
    !> fdet + sdet = CN_det*det
    !> NC_fdet*fdet + NC_sdet*sdet = det
    !> fdet = fac_fdet*det
    !> sdet = fac_sdet*det

      !   DIN flux:
      call ESMF_StateGet(importState,trim('mole_concentration_of_nitrate_upward_flux'),field,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=val1_f2,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateGet(importState,'mole_concentration_of_ammonium_upward_flux',field,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=val2_f2,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call mossco_state_get(exportState,(/ &
              'nutrients_upward_flux                            ', &
              'Dissolved_Inorganic_Nitrogen_DIN_nutN_upward_flux'/),DINflux,rc=rc)
      DINflux(1,1) = val1_f2(1,1) + val2_f2(1,1)

      !   DIP flux:
      call mossco_state_get(exportState,(/ &
              'Dissolved_Inorganic_Phosphorus_DIP_nutP_upward_flux'/),DIPflux,rc=rc)
      if (rc == 0)  then
        call mossco_state_get(importState,(/ &
              'mole_concentration_of_phosphate_upward_flux'/),val1_f2,rc=rc)
         DIPflux(1,1) = val1_f2(1,1)
      end if

      !   Det flux:
      call mossco_state_get(exportState,(/ &
            'detritus              ', &
            'Detritus_Nitrogen_detN'/),DETN,rc=rc)
      call mossco_state_get(exportState,(/ &
            'detritus_z_velocity              ', &
            'Detritus_Nitrogen_detN_z_velocity'/),vDETN,rc=rc)
      DETNflux(1,1) = sinking_factor * DETN(1,1,1) * vDETN(1,1,1)

      !> search for Detritus-C, if present, use Detritus C-to-N ratio and apply flux
      call mossco_state_get(exportState,(/'Detritus_Carbon_detC'/),DETC,rc=rc)
      if (rc == 0) then
         call mossco_state_get(exportState,(/ &
              'Detritus_Carbon_detC_upward_flux'/),DETCflux,rc=rc)
         DETCflux(1,1) = sinking_factor * DETC(1,1,1) * vDETN(1,1,1)
      end if

      !> check for Detritus-P and calculate flux either N-based
      !> or as present through the Detritus-P pool
      call mossco_state_get(exportState,(/'Detritus_Phosphorus_detP'/),DETP,rc=rc)
      if (rc == 0) then
        call mossco_state_get(exportState,(/ &
              'Detritus_Phosphorus_detP_z_velocity'/),vDETP,rc=rc)
        DETPflux(1,1) = sinking_factor * DETP(1,1,1) * vDETP(1,1,1)
      else
        if (.not.(associated(DETPflux))) allocate(DETPflux(1,1))
        DETPflux(1,1) = 1.0d0/16.0d0 * DETNflux(1,1)
      end if


  end subroutine Run

  subroutine Finalize(cplcomp, importState, exportState, externalclock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer,intent(out)  :: rc
     
    call ESMF_LogWrite("pelagic-benthic coupler finalizing", ESMF_LOGMSG_INFO)

    call ESMF_ArraySpecDestroy(flux_bdy_array, rc)
    call ESMF_GridDestroy(flux_bdy_grid, rc=rc)

    if (associated(DETNflux)) deallocate(DETNflux)
    if (associated(DETPflux)) deallocate(DETPflux)

    call ESMF_LogWrite("benthic-pelagic coupler finalized", ESMF_LOGMSG_INFO)
  end subroutine Finalize

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
      ptr_f2 = 0.0_rk
      call ESMF_StateAddReplace(outputstate,(/newfield/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end if
  end do
  end subroutine mossco_create_upward_flux_fields  


end module benthic_pelagic_coupler

