module pelagic_benthic_coupler
    
  use esmf
  use fabm_sediment_component, only : rk
  use mossco_state

  implicit none

  private
  type(ESMF_GRID) :: pelagic_bdy_grid
  type(ESMF_ARRAYSPEC) :: pelagic_bdy_array
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DETN,DIN,vDETN
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DIP=>null(),DETP,vDETP
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: vDETC,DETC
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: nit,amm
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: oxy=>null(),odu=>null()
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: ptr_f2

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

    call ESMF_LogWrite("pelagic-benthic coupler initializing", ESMF_LOGMSG_INFO)

    ! create exchange fields
    !> @todo: get grid size from exportState (so far using 1x1 horizontal grid
    call ESMF_ArraySpecSet(pelagic_bdy_array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    pelagic_bdy_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
      maxIndex=(/1,1/), &
      regDecomp=(/1,1/), &
      coordSys=ESMF_COORDSYS_SPH_DEG, &
      indexflag=ESMF_INDEX_GLOBAL,  &
      name="sediment_surface_boundary_grid", &
      coordTypeKind=ESMF_TYPEKIND_R8, &
      coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    ! create omexdia_p-related fields, if not existing
    call create_required_fields(exportState,pelagic_bdy_grid)

    call ESMF_LogWrite("pelagic-benthic coupler initialized", ESMF_LOGMSG_INFO)

  end subroutine Initialize


  subroutine Run(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc
    integer              :: ammrc,nitrc

    integer                     :: myrank
    integer                     :: i,j,inum,jnum
    integer                     :: lbnd(3)=1
    integer                     :: Clbnd(3),AMMlbnd(3),Plbnd(3)
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    character (len=ESMF_MAXSTR) :: message
    type(ESMF_Field)            :: field
    real(ESMF_KIND_R8),parameter      :: sinking_factor=0.3d0 !> 30% of Det sinks into sediment
    real(ESMF_KIND_R8),dimension(:,:),pointer :: CN_det=>null()
    !> @todo read NC_fdet dynamically from fabm model info?  This would not comply with our aim to separate fabm/esmf
    real(ESMF_KIND_R8),parameter    :: NC_fdet=0.20_rk
    real(ESMF_KIND_R8),parameter    :: NC_sdet=0.04_rk
    real(ESMF_KIND_R8),dimension(:,:),pointer :: fac_fdet
    real(ESMF_KIND_R8),dimension(:,:),pointer :: fac_sdet
    !> fdet + sdet = CN_det*det
    !> NC_fdet*fdet + NC_sdet*sdet = det
    !> fdet = fac_fdet*det
    !> sdet = fac_sdet*det

      ! water temperature:
      call mossco_state_get(importState,(/'temperature_in_water'/),ptr_f3,lbnd=lbnd,rc=rc)
      call mossco_state_get(exportState,(/'temperature_at_soil_surface'/),ptr_f2,rc=rc)
      ptr_f2 = ptr_f3(:,:,lbnd(3))

      ! dissolved_oxygen:
      call mossco_state_get(importState,(/ &
        'oxygen          ', &
        'dissolved_oxygen'/),ptr_f3,lbnd=lbnd,rc=rc)
      call mossco_state_get(exportState,(/'dissolved_oxygen_at_soil_surface'/),ptr_f2,rc=rc)

      inum=ubound(ptr_f3,1)
      jnum=ubound(ptr_f3,2)
      if (.not.associated(oxy)) allocate(oxy(inum,jnum))
      if (.not.associated(odu)) allocate(odu(inum,jnum))

      do i=1,inum
        do j=1,jnum
          oxy = max(0.0d0,ptr_f3(i,j,lbnd(3)))
          odu = max(0.0d0,-ptr_f3(i,j,lbnd(3)))
        end do
      end do
      ptr_f2 = oxy(:,:)


      !   Det flux:
      call mossco_state_get(importState,(/ &
            'detritus              ', &
            'detN                  ', &
            'Detritus_Nitrogen_detN'/),DETN,lbnd=lbnd,rc=rc)
      call mossco_state_get(importState,(/ &
            'detritus_z_velocity              ', &
            'detN_z_velocity                  ', &
            'Detritus_Nitrogen_detN_z_velocity'/),vDETN,rc=rc)

      inum=ubound(DETN,1)
      jnum=ubound(DETN,2)
      if (.not.associated(CN_det)) allocate(CN_det(1:inum,1:jnum))
      if (.not.associated(fac_fdet)) allocate(fac_fdet(1:inum,1:jnum))
      if (.not.associated(fac_sdet)) allocate(fac_sdet(1:inum,1:jnum))
      !> search for Detritus-C, if present, use Detritus C-to-N ratio and apply flux
      call mossco_state_get(importState,(/'Detritus_Carbon_detC'/),DETC,lbnd=Clbnd,rc=rc)
      if (rc /= 0) then
         CN_det=106.0_rk/16.0_rk
      else
         CN_det = DETC(:,:,Clbnd(3))/DETN(:,:,lbnd(3))
      end if
      fac_fdet = (1.0_rk-NC_sdet*CN_det)/(NC_fdet-NC_sdet)
      fac_sdet = (1.0_rk-NC_fdet*CN_det)/(NC_sdet-NC_fdet)

      call ESMF_StateGet(exportState,'fast_detritus_C_at_soil_surface',field,rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f2,rc=rc)
      ptr_f2 = fac_fdet * DETN(:,:,lbnd(3))
      call ESMF_StateGet(exportState,'slow_detritus_C_at_soil_surface',field,rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f2,rc=rc)
      ptr_f2 = fac_sdet * DETN(:,:,lbnd(3))

      call mossco_state_get(exportState,(/'fast_detritus_C_z_velocity_at_soil_surface'/),ptr_f2,rc=rc)
      if (rc==0) ptr_f2 = sinking_factor * vDETN(:,:,lbnd(3))
      call mossco_state_get(exportState,(/'slow_detritus_C_z_velocity_at_soil_surface'/),ptr_f2,rc=rc)
      if (rc==0) ptr_f2 = sinking_factor * vDETN(:,:,lbnd(3))

      !> check for Detritus-P and calculate flux either N-based
      !> or as present through the Detritus-P pool
      call mossco_state_get(exportState,(/'detritus-P_at_soil_surface'/),ptr_f2,rc=rc)
      call mossco_state_get(importState,(/ &
          'detP                    ', &
          'Detritus_Phosphorus_detP'/),DETP,lbnd=Plbnd,rc=rc)
      if (rc == 0) then
        ptr_f2 = DETP(:,:,plbnd(3))
      else
        ptr_f2 = 1.0d0/16.0d0 * DETN(:,:,lbnd(3))
      end if

      call mossco_state_get(exportState,(/'detritus-P_z_velocity_at_soil_surface'/),ptr_f2,rc=rc)
      call mossco_state_get(importState,(/ &
              'detP_z_velocity                    ', &
              'Detritus_Phosphorus_detP_z_velocity'/),vDETP,rc=rc)
      if (rc==0) then
        ptr_f2 = sinking_factor * vDETP(:,:,Plbnd(3))
      else
        ptr_f2 = sinking_factor * vDETN(:,:,lbnd(3))
      end if

      ! DIM concentrations:
      !  oxygen is coming from constant component
      !  set reduced substances to zero
      call mossco_state_get(exportState,(/'dissolved_reduced_substances_at_soil_surface'/),ptr_f2,rc=rc)
      ptr_f2 = odu(:,:)

      call mossco_state_get(importState,(/'nitrate'/),nit,rc=nitrc)
      if (nitrc /= 0) then
        call mossco_state_get(importState,(/ &
              'nutrients                            ', &
              'DIN                                  ', &
              'Dissolved_Inorganic_Nitrogen_DIN_nutN'/),DIN,lbnd=lbnd,rc=rc)
      end if
      call mossco_state_get(importState,(/'ammonium'/),amm,lbnd=AMMlbnd,rc=ammrc)
      
      call ESMF_StateGet(exportState,'mole_concentration_of_ammonium_at_soil_surface',field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f2,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if (ammrc == 0) then
        ptr_f2 = amm(:,:,AMMlbnd(3))
      else
        ptr_f2 = 0.5d0 * DIN(:,:,lbnd(3))
      end if
      call ESMF_StateGet(exportState,'mole_concentration_of_nitrate_at_soil_surface',field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f2,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if (nitrc == 0) then
        ptr_f2 = nit(:,:,lbnd(3))
      else
        ptr_f2 = 0.5d0 * DIN(:,:,lbnd(3))
      end if

      !> check for DIP, if present, take as is, if not calculate it N-based
      call mossco_state_get(importState,(/ &
          'DIP                                    ', &
          'phosphate                              ', &
          'Dissolved_Inorganic_Phosphorus_DIP_nutP'/),DIP,lbnd=Plbnd,rc=rc)
      if (rc /= 0) then
        if (.not.(associated(DIP))) allocate(DIP(1:ubound(DIN,1),1:ubound(DIN,2),1))
        DIP(:,:,1) = 1.0_rk/16.0_rk * DIN(:,:,lbnd(3))
        Plbnd(3)=1
      end if
      call ESMF_StateGet(exportState,'mole_concentration_of_phosphate_at_soil_surface',field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f2,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f2 = DIP(:,:,Plbnd(3))

  end subroutine Run

  subroutine Finalize(cplcomp, importState, exportState, externalclock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer,intent(out)  :: rc
     
    call ESMF_LogWrite("pelagic-benthic coupler finalizing", ESMF_LOGMSG_INFO)

    !call ESMF_ArraySpecDestroy(pelagic_bdy_array, rc)
    call ESMF_GridDestroy(pelagic_bdy_grid, rc=rc)

    call ESMF_LogWrite("pelagic-benthic coupler finalized", ESMF_LOGMSG_INFO)
  end subroutine Finalize


end module pelagic_benthic_coupler

