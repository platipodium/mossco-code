
module benthos_component

  use esmf
  use mossco_component

  use Macrofauna_interface
  use Microphytobenthos_class

  implicit none

  public :: SetServices

  private


  !! @todo hn: read CF documnetation for correct name

  ! Dimensions (x,y,z)
  real(ESMF_KIND_R8), dimension(:,:), pointer :: Effect_of_MPB_on_sediment_erodibility_at_bottom
  real(ESMF_KIND_R8), dimension(:,:), pointer :: Effect_of_MPB_on_critical_bed_shearstress
  real(ESMF_KIND_R8), dimension(:,:), pointer :: Effect_of_Mbalthica_on_sediment_erodibility_at_bottom
  real(ESMF_KIND_R8), dimension(:,:), pointer :: Effect_of_Mbalthica_on_critical_bed_shearstress

  type(ESMF_Field), save      :: Microphytobenthos_erodibility,Microphytobenthos_critical_bed_shearstress, &
    &                            Macrofauna_erodibility,Macrofauna_critical_bed_shearstress
  integer                     :: ubnd(3),lbnd(3)


  type (microphytobenthos) ,save :: Micro
  type (BioturbationEffect),save :: Total_Bioturb
  real (fp)    :: tau
  real (fp)    :: Erod
  integer      :: inum, jnum

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

    type(ESMF_Grid)      :: grid, foreign_grid
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array)     :: array
    type(ESMF_Field)     :: field
    
    real(ESMF_KIND_R8),dimension(:),pointer :: LonCoord,LatCoord,DepthCoord
    character(len=ESMF_MAXSTR) :: foreignGridFieldName
    
    integer , allocatable :: maxIndex(:)
    integer               :: rank

    type(ESMF_Time)   :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8) :: dt
    character(len=80)  :: title
    character(len=256) :: din_variable='',pon_variable=''
    integer(ESMF_KIND_I8) :: nlev

    character(ESMF_MAXSTR):: name, message, timeString
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: currTime
    logical               :: clockIsPresent
    
    rc = ESMF_SUCCESS
     
    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)


    !! get/set grid:
    !! rely on field with name foreignGridFieldName given as attribute and field
    !! in importState
    !! and just take the same grid&distgrid.
    
    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
           value=foreignGridFieldName, defaultValue='none',rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (trim(foreignGridFieldName)=='none') then
     inum=1
     jnum = 1
     ! call ESMF_ArraySpecSet(array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                   maxIndex=(/inum,jnum/), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="benthos grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridAddCoord(grid, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    else
      call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field, grid=foreign_grid, rank=rank, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if (rank<2) then
        write(message,*) 'foreign grid must be of at least rank >= 2'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      end if 
      
      allocate(maxIndex(rank))
        inum=maxIndex(1)
        jnum=maxIndex(2)
      if (rank ==2) then 
        !grid = foreign_Grid    !> ToDO discuss copy or link for grid 
        grid = ESMF_GridCreate(foreign_grid,rc=rc)
       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      elseif (rank == 3) then
 
        call ESMF_GridGet(foreign_grid,staggerloc=ESMF_STAGGERLOC_CENTER,localDE=0, &
               computationalCount=maxIndex,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                   maxIndex=maxIndex(1:2), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="benthos grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=rc)
        inum=maxIndex(1)
        jnum=maxIndex(2)
      !  numlayers=maxIndex(3)
        call ESMF_GridAddCoord(grid, rc=rc)   !> ToDO we need to copy the coordiane from foreign Grid.
        deallocate(maxIndex)
      else
        write(message,*) 'foreign grid must be of rank = 2 or 3'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      end if
    end if


    !> create grid
   ! grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/1,1,1/), &
 !     regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
   !   name="Benthos grid",coordTypeKind=ESMF_TYPEKIND_R8, rc=rc)
   ! if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   ! call ESMF_GridAddCoord(grid, rc=rc)
   ! if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)


    !> Create distgrid for arrays
   !   distgrid =  ESMF_DistGridCreate(minIndex=(/inum,jnum/), maxIndex=(/inum,jnum/), &
   !   indexflag=ESMF_INDEX_GLOBAL, rc=rc)
       call ESMF_GridGet (grid, DistGrid= DistGrid, rc=rc)

       if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

 

    nlev=1

    call init_microphyt(Micro,inum, jnum)
    call set_microphyt(Micro)

    call Macrofanua_init(Total_Bioturb, inum, jnum)
    call Macrofanua_set()

    ! Test parameters (they are not needed here for real calculations)
    tau = 1.9
    Erod = 0.00006

#ifdef DEBUG
    write (*,*) 'Abiotic critical tau =' , tau, 'Abiotic Erodibility = ', Erod
    write (*,*)
#endif


   !> create export fields
    allocate(Effect_of_MPB_on_sediment_erodibility_at_bottom(inum,jnum))
    Effect_of_MPB_on_sediment_erodibility_at_bottom => Micro%ErodibilityEffect

#ifdef DEBUG
    write (*,*) ' Effect_of_MPB_on_sediment_erodibility_at_soil_surface', &
    Effect_of_MPB_on_sediment_erodibility_at_bottom
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=ESMF_INDEX_GLOBAL, &
      farray=Effect_of_MPB_on_sediment_erodibility_at_bottom,rc=rc)

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    Microphytobenthos_erodibility = ESMF_FieldCreate(grid, array, &
      name="Effect_of_MPB_on_sediment_erodibility_at_soil_surface", &
      staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    allocate(Effect_of_MPB_on_critical_bed_shearstress(inum,jnum))

    Effect_of_MPB_on_critical_bed_shearstress => Micro%TauEffect

#ifdef DEBUG
    write (*,*) 'Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface',&
     Effect_of_MPB_on_critical_bed_shearstress
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=ESMF_INDEX_GLOBAL, &
      farray=Effect_of_MPB_on_critical_bed_shearstress, rc=rc)

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    Microphytobenthos_critical_bed_shearstress= ESMF_FieldCreate(grid, array, &
      name="Effect_of_MPB_on_critical_bed_shearstress", rc=rc)

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    allocate( Effect_of_Mbalthica_on_sediment_erodibility_at_bottom(inum,jnum))

    Effect_of_Mbalthica_on_sediment_erodibility_at_bottom => Total_Bioturb%ErodibilityEffect

#ifdef DEBUG
    write (*,*) 'Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface', &
    Effect_of_Mbalthica_on_sediment_erodibility_at_bottom
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=ESMF_INDEX_GLOBAL,  &
      farray=Effect_of_Mbalthica_on_sediment_erodibility_at_bottom, rc=rc)

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    Macrofauna_erodibility= ESMF_FieldCreate(grid, array, &
      name="Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface", rc=rc)

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    allocate(Effect_of_Mbalthica_on_critical_bed_shearstress(inum,jnum))

    Effect_of_Mbalthica_on_critical_bed_shearstress => Total_Bioturb%TauEffect

#ifdef DEBUG
    write (*,*) 'Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface',&
    Effect_of_Mbalthica_on_critical_bed_shearstress
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=ESMF_INDEX_GLOBAL, &
      farray=Effect_of_Mbalthica_on_critical_bed_shearstress, rc=rc)

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    Macrofauna_critical_bed_shearstress= ESMF_FieldCreate(grid, array, &
      name="Effect_of_Mbalthica_on_critical_bed_shearstress", rc=rc)

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !> set export state
    call ESMF_StateAdd(exportState,(/Microphytobenthos_erodibility/),rc=rc)
    call ESMF_StateAdd(exportState,(/Microphytobenthos_critical_bed_shearstress/),rc=rc)

    call ESMF_StateAdd(exportState,(/Macrofauna_erodibility/),rc=rc)
    call ESMF_StateAdd(exportState,(/Macrofauna_critical_bed_shearstress/),rc=rc)

#ifdef DEBUG
    call ESMF_FieldPrint (Microphytobenthos_erodibility)
      write (*,*) 'Mircrophy. erodibility effect', Micro%ErodibilityEffect

    call ESMF_FieldPrint (Microphytobenthos_critical_bed_shearstress)
      write (*,*) 'Mircrophy. Tau effect', Micro%TauEffect

    call ESMF_FieldPrint (Macrofauna_erodibility)
      write (*,*) ' Macro. erodibility effect', Total_Bioturb%ErodibilityEffect

    call ESMF_FieldPrint (Macrofauna_critical_bed_shearstress)
      write (*,*) ' Macro. Critical bed Shear stress', Total_Bioturb%TauEffect
#endif

    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=255)       :: logstring
    type(ESMF_Time)          :: clockTime
    type(ESMF_TimeInterval)  :: timestep
    integer(ESMF_KIND_I8)    :: advancecount
    real(ESMF_KIND_R8)       :: runtimestepcount,dt

     integer               :: petCount, localPet
    character(ESMF_MAXSTR):: name, message, timeString
    logical               :: clockIsPresent
    type(ESMF_Time)       :: currTime
    type(ESMF_Clock)      :: clock
     
    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)


    call ESMF_ClockGet(clock,currTime=currTime, AdvanceCount=advancecount,&
      runTimeStepCount=runtimestepcount,timeStep=timestep, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalGet(timestep,s_r8=dt,rc=rc)


#if 0
    ! get import state
    if (forcing_from_coupler) then
      call ESMF_StateGet(importState, "temperature_in_water", water_temperature_field, rc=rc)
      call ESMF_FieldGet(water_temperature_field, farrayPtr=water_temperature, rc=rc)
      zerod%temp = water_temperature(1,1,1)
    end if

#endif

    call run_microphyt(Micro)
    call Macrofanua_run(Total_Bioturb)


#ifdef DEBUG
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
#endif

    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)

   
    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory    

    call ESMF_ClockDestroy(clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)


  end subroutine Finalize

end module benthos_component
