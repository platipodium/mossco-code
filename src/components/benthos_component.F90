
module benthos_component

  use esmf

  use Macrofauna_interface
  use Microphytobenthos_class

  implicit none

  public :: SetServices

  private


  !! @todo hn: read CF documnetation for correct name

  ! Dimensions (x,y,z)
  real(ESMF_KIND_R8), dimension(:,:,:), pointer :: Effect_of_MPB_on_sediment_erodibility_at_bottom
  real(ESMF_KIND_R8), dimension(:,:,:), pointer :: Effect_of_MPB_on_critical_bed_shearstress
  real(ESMF_KIND_R8), dimension(:,:,:), pointer :: Effect_of_Mbalthica_on_sediment_erodibility_at_bottom
  real(ESMF_KIND_R8), dimension(:,:,:), pointer :: Effect_of_Mbalthica_on_critical_bed_shearstress

  type(ESMF_Field)            :: Microphytobenthos_erodibility,Microphytobenthos_critical_bed_shearstress, &
    &                            Macrofauna_erodibility,Macrofauna_critical_bed_shearstress
  integer                     :: ubnd(3),lbnd(3)


  type (microphytobenthos),save  :: Micro
  type (BioturbationEffect),save :: Total_Bioturb
  real (fp)    :: tau
  real (fp)    :: Erod


contains

  !> Provide an ESMF compliant SetServices routine, which defines
  !! the entry points for Init/Run/Finalize
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  !> Initialize the component
  !!
  !! Allocate memory for boundaries and fluxes, create ESMF fields
  !! and export them
  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: importState, exportState
    type(ESMF_Clock)    :: parentClock
    integer, intent(out)     :: rc

    type(ESMF_Grid)      :: grid
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array)     :: array
    real(ESMF_KIND_R8),dimension(:),pointer :: LonCoord,LatCoord,DepthCoord

    character(len=19) :: timestring
    type(ESMF_Time)   :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8) :: dt
    character(len=80)  :: title
    character(len=256) :: din_variable='',pon_variable=''
    integer(ESMF_KIND_I8) :: nlev

    call ESMF_LogWrite('Initializing benthos effect component',ESMF_LOGMSG_INFO)


    nlev=1

call init_microphyt(Micro)
call set_microphyt(Micro)

call Macrofanua_init(Total_Bioturb)
call Macrofanua_set()

tau = 1.9
Erod = 0.00006

write (*,*) 'Abiotic critical tau =' , tau, 'Abiotic Erodibility = ', Erod
write (*,*)
    !> create grid
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/1,1,1/), &
      regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="Benthos grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !> Create distgrid for arrays
    distGrid =  ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/1,1,1/), &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)

    !call ESMF_GridGet(grid,distgrid=distgrid)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !> create export fields
    allocate(Effect_of_MPB_on_sediment_erodibility_at_bottom(1,1,1))
    Effect_of_MPB_on_sediment_erodibility_at_bottom => Micro%ErodibilityEffect
    array = ESMF_ArrayCreate(distgrid=distgrid, &
      farrayPtr=Effect_of_MPB_on_sediment_erodibility_at_bottom,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    Microphytobenthos_erodibility = ESMF_FieldCreate(grid, array, &
      name="Effect_of_MPB_on_sediment_erodibility_at_bottom", &
      staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    allocate(Effect_of_MPB_on_critical_bed_shearstress(1,1,1))
    Effect_of_MPB_on_critical_bed_shearstress => Micro%TauEffect
    array = ESMF_ArrayCreate(distgrid=distgrid, &
      farrayPtr=Effect_of_MPB_on_critical_bed_shearstress, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    Microphytobenthos_critical_bed_shearstress= ESMF_FieldCreate(grid, array, &
      name="Effect_of_MPB_on_critical_bed_shearstress", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    allocate( Effect_of_Mbalthica_on_sediment_erodibility_at_bottom(1,1,1))
    Effect_of_Mbalthica_on_sediment_erodibility_at_bottom => Total_Bioturb%ErodibilityEffect
    array = ESMF_ArrayCreate(distgrid=distgrid,  &
      farrayPtr=Effect_of_Mbalthica_on_sediment_erodibility_at_bottom, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    Macrofauna_erodibility= ESMF_FieldCreate(grid, array, &
      name="Effect_of_Mbalthica_on_sediment_erodibility_at_bottom", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    allocate(Effect_of_Mbalthica_on_critical_bed_shearstress(1,1,1))
    Effect_of_Mbalthica_on_critical_bed_shearstress => Total_Bioturb%TauEffect
    array = ESMF_ArrayCreate(distgrid=distgrid, &
      farrayPtr=Effect_of_Mbalthica_on_sediment_erodibility_at_bottom, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    Macrofauna_critical_bed_shearstress= ESMF_FieldCreate(grid, array, &
      name="Effect_of_Mbalthica_on_critical_bed_shearstress", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !> set export state
    call ESMF_StateAdd(exportState,(/Microphytobenthos_erodibility/),rc=rc)
    call ESMF_StateAdd(exportState,(/Microphytobenthos_critical_bed_shearstress/),rc=rc)

    call ESMF_StateAdd(exportState,(/Macrofauna_erodibility/),rc=rc)
    call ESMF_StateAdd(exportState,(/Macrofauna_critical_bed_shearstress/),rc=rc)

    call ESMF_FieldPrint (Microphytobenthos_erodibility)
      write (*,*) 'Mircrophy. erodibility effect', Micro%ErodibilityEffect
    call ESMF_FieldPrint (Microphytobenthos_critical_bed_shearstress)
      write (*,*) 'Mircrophy. Tau effect', Micro%TauEffect
    call ESMF_FieldPrint (Macrofauna_erodibility)
      write (*,*) ' Macro. erodibility effect', Total_Bioturb%ErodibilityEffect
    call ESMF_FieldPrint (Macrofauna_critical_bed_shearstress)
      write (*,*) ' Macro. Critical bed Shear stress', Total_Bioturb%TauEffect



    call ESMF_LogWrite('Initialized benthos component',ESMF_LOGMSG_INFO)

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)        :: timestring
    character(len=255)       :: logstring
    type(ESMF_Time)          :: clockTime
    type(ESMF_TimeInterval)  :: timestep
    integer(ESMF_KIND_I8)    :: advancecount
    real(ESMF_KIND_R8)       :: runtimestepcount,dt

    ! Get global clock properties
    call ESMF_TimeSet(clockTime)
    call ESMF_ClockGet(parentClock,currTime=clockTime,AdvanceCount=advancecount,&
      runTimeStepCount=runtimestepcount,timeStep=timestep,rc=rc)
    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
    write (logstring,'(A,I6,A,A)') "Benthos run(",advancecount,") at ",timestring
    call ESMF_LogWrite(trim(logstring), ESMF_LOGMSG_INFO)

    call ESMF_TimeIntervalGet(timestep,s_r8=dt,rc=rc)


#if 0
    ! get import state
    if (forcing_from_coupler) then
      call ESMF_StateGet(importState, "water_temperature", water_temperature_field, rc=rc)
      call ESMF_FieldGet(water_temperature_field, farrayPtr=water_temperature, rc=rc)
      zerod%temp = water_temperature(1,1,1)
    end if

#endif

call run_microphyt(Micro)
call Macrofanua_run(Total_Bioturb)


call ESMF_FieldPrint (Microphytobenthos_erodibility)
   write (*,*) 'Mircrophy. erodibility effect', Micro%ErodibilityEffect
call ESMF_FieldPrint (Microphytobenthos_critical_bed_shearstress)
   write (*,*) 'Mircrophy. Tau effect', Micro%TauEffect
call ESMF_FieldPrint (Macrofauna_erodibility)
   write (*,*) ' Macro. erodibility effect', Total_Bioturb%ErodibilityEffect
call ESMF_FieldPrint (Macrofauna_critical_bed_shearstress)
   write (*,*) ' Macro. Critical bed Shear stress', Total_Bioturb%TauEffect


write (*,*) 'tau (macrofaunau and microphytobenthos) =' ,tau,' Both Biotic Critical bed shear stress effect= ',&
      &   Total_Bioturb%TauEffect, 'Both Biotic erodibility',Total_Bioturb%ErodibilityEffect

write (*,*)


  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc


 call fin_micropyht(Micro)
 call Macrofanua_fin(Total_Bioturb)


  end subroutine Finalize

end module benthos_component
