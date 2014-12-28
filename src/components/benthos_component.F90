!> @brief Implementation of an ESMF component that calculates benthos effects
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, Helmholtz-Zentrum Geesthacht
!> @author Hassan Nasermoaddeli
!> @author Carsten Lemmen
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "benthos_component.F90"

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

  type(ESMF_Field), save         :: Microphytobenthos_erodibility,Microphytobenthos_critical_bed_shearstress, &
    &                               Macrofauna_erodibility,Macrofauna_critical_bed_shearstress
  integer                        :: ubnd(3),lbnd(3)


  type (microphytobenthos) ,save :: Micro
  type (BioturbationEffect),save :: Total_Bioturb
  real (fp)                      :: tau
  real (fp)                      :: Erod
  integer                        :: inum, jnum

contains


#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer              :: localrc

    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)         :: gridComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: parentClock
    integer, intent(out)        :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"

  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)    :: gridComp
    type(ESMF_State)       :: importState, exportState
    type(ESMF_Clock)       :: parentClock
    integer, intent(out)   :: rc

    type(ESMF_Grid)        :: grid, foreign_grid
    type(ESMF_DistGrid)    :: distgrid
    type(ESMF_ArraySpec)   :: arrayspec
    type(ESMF_Array)       :: array
    type(ESMF_Field)       :: field

    real(ESMF_KIND_R8),dimension(:),pointer :: LonCoord,LatCoord,DepthCoord
    character(len=ESMF_MAXSTR) :: foreignGridFieldName

    integer                :: rank, localrc

    type(ESMF_Time)        :: wallTime, clockTime
    type(ESMF_TimeInterval):: timeInterval
    real(ESMF_KIND_R8)     :: dt
    character(len=80)      :: title
    character(len=256)     :: din_variable='',pon_variable=''
    integer(ESMF_KIND_I8)  :: nlev

    character(ESMF_MAXSTR) :: name, message, timeString
    type(ESMF_Clock)       :: clock
    type(ESMF_Time)        :: currTime
    logical                :: clockIsPresent
    type(ESMF_INDEX_Flag)  :: indexFlag

    integer(ESMF_KIND_I4)  :: ubnd2(2), lbnd2(2), ubnd3(3), lbnd3(3)

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! get/set grid:
    !! rely on field with name foreignGridFieldName given as attribute and field
    !! in importState
    !! and just take the same grid&distgrid.

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
           value=foreignGridFieldName, defaultValue='none',rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (trim(foreignGridFieldName)=='none') then
     inum=1
     jnum = 1
     ! call ESMF_ArraySpecSet(array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
      grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                   maxIndex=(/inum,jnum/), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="benthos grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridAddCoord(grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      write(message,*) trim(name)//' uses foreign grid '//trim(foreignGridFieldName)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

      call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field, rank=rank, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (rank<2 .or. rank>3) then
        write(message,*) 'foreign grid must be of rank 2 or 3'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
      end if

      if (rank==2) then
        call ESMF_FieldGet(field, grid=grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGetBounds(field, exclusiveLBound=lbnd2, exclusiveUBound=ubnd2, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        inum=ubnd2(1)-lbnd2(1)+1
        jnum=ubnd2(2)-lbnd2(2)+1
      endif

      if (rank==3) then
        write(message,*) 'foreign grid of rank 3 not yet implemented'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        call ESMF_FieldGet(field, grid=foreign_grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGetBounds(field, exclusiveLBound=lbnd3, exclusiveUBound=ubnd3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        inum=ubnd3(1)-lbnd3(1)+1
        jnum=ubnd3(2)-lbnd3(2)+1

        grid = ESMF_GridCreateNoPeriDim(minIndex=lbnd3(1:2), &
                   maxIndex=ubnd3(1:2), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="benthos grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_GridAddCoord(grid, rc=localrc)   !> ToDO we need to copy the coordiane from foreign Grid.

      endif
    endif

    !> Create distgrid for arrays
    !   distgrid =  ESMF_DistGridCreate(minIndex=(/inum,jnum/), maxIndex=(/inum,jnum/), &
    !   indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
    call ESMF_GridGet(grid, distGrid=distGrid, indexflag=indexflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    nlev=1

    call Micro%initialize(inum, jnum)
    call Micro%set()

    call Macrofanua_init(Total_Bioturb, inum, jnum)
    call Macrofanua_set()

    ! Test parameters (they are not needed here for real calculations)
    tau = 1.9
    Erod = 0.00006

#ifdef DEBUG
    write(0,*) 'Abiotic critical tau =' , tau, 'Abiotic Erodibility = ', Erod
    write(0,*)
#endif

   !> create export fields
    allocate(Effect_of_MPB_on_sediment_erodibility_at_bottom(inum,jnum))
    Effect_of_MPB_on_sediment_erodibility_at_bottom => Micro%ErodibilityEffect

#ifdef DEBUG
    write(0,*) ' Effect_of_MPB_on_sediment_erodibility_at_soil_surface', &
    Effect_of_MPB_on_sediment_erodibility_at_bottom, ubound(Effect_of_MPB_on_sediment_erodibility_at_bottom)
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=indexFlag, &
      farray=Effect_of_MPB_on_sediment_erodibility_at_bottom,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    Microphytobenthos_erodibility = ESMF_FieldCreate(grid, array, &
      name="Effect_of_MPB_on_sediment_erodibility_at_soil_surface", &
      staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(Microphytobenthos_erodibility, 'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(Effect_of_MPB_on_critical_bed_shearstress(inum,jnum))

    Effect_of_MPB_on_critical_bed_shearstress => Micro%TauEffect

#ifdef DEBUG
    write(0,*) 'Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface',&
     Effect_of_MPB_on_critical_bed_shearstress
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=indexFlag, &
      farray=Effect_of_MPB_on_critical_bed_shearstress, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    Microphytobenthos_critical_bed_shearstress= ESMF_FieldCreate(grid, array, &
      name="Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(Microphytobenthos_critical_bed_shearstress, 'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate( Effect_of_Mbalthica_on_sediment_erodibility_at_bottom(inum,jnum))

    Effect_of_Mbalthica_on_sediment_erodibility_at_bottom => Total_Bioturb%ErodibilityEffect

#ifdef DEBUG
    write(0,*) 'Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface', &
    Effect_of_Mbalthica_on_sediment_erodibility_at_bottom
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=indexFlag,  &
      farray=Effect_of_Mbalthica_on_sediment_erodibility_at_bottom, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    Macrofauna_erodibility= ESMF_FieldCreate(grid, array, &
      name="Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(Macrofauna_erodibility, 'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(Effect_of_Mbalthica_on_critical_bed_shearstress(inum,jnum))

    Effect_of_Mbalthica_on_critical_bed_shearstress => Total_Bioturb%TauEffect

#ifdef DEBUG
    write(0,*) 'Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface',&
    Effect_of_Mbalthica_on_critical_bed_shearstress
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=indexFlag, &
      farray=Effect_of_Mbalthica_on_critical_bed_shearstress, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    Macrofauna_critical_bed_shearstress= ESMF_FieldCreate(grid, array, &
      name="Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(Macrofauna_critical_bed_shearstress, 'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> set export state
    call ESMF_StateAdd(exportState,(/Microphytobenthos_erodibility/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(exportState,(/Microphytobenthos_critical_bed_shearstress/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(exportState,(/Macrofauna_erodibility/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(exportState,(/Macrofauna_critical_bed_shearstress/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifdef DEBUG
    call ESMF_FieldPrint (Microphytobenthos_erodibility)
      write(0,*) 'Mircrophy. erodibility effect', Micro%ErodibilityEffect

    call ESMF_FieldPrint (Microphytobenthos_critical_bed_shearstress)
      write(0,*) 'Mircrophy. Tau effect', Micro%TauEffect

    call ESMF_FieldPrint (Macrofauna_erodibility)
      write(0,*) ' Macro. erodibility effect', Total_Bioturb%ErodibilityEffect

    call ESMF_FieldPrint (Macrofauna_critical_bed_shearstress)
      write(0,*) ' Macro. Critical bed Shear stress', Total_Bioturb%TauEffect
#endif

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)    :: gridComp
    type(ESMF_State)       :: importState, exportState
    type(ESMF_Clock)       :: parentClock
    integer, intent(out)   :: rc

    character(len=255)     :: logstring
    type(ESMF_Time)        :: clockTime
    type(ESMF_TimeInterval):: timestep
    integer(ESMF_KIND_I8)  :: advancecount
    real(ESMF_KIND_R8)     :: runtimestepcount,dt

    integer                :: petCount, localPet
    character(ESMF_MAXSTR) :: name, message, timeString
    logical                :: clockIsPresent
    type(ESMF_Time)        :: currTime, stopTime
    type(ESMF_Clock)       :: clock

    integer                :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)


    call ESMF_ClockGet(clock,currTime=currTime, AdvanceCount=advancecount,&
      runTimeStepCount=runtimestepcount,timeStep=timestep, rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalGet(timestep,s_r8=dt,rc=localrc)


#if 0
    ! get import state
    if (forcing_from_coupler) then
      call ESMF_StateGet(importState, "temperature_in_water", water_temperature_field, rc=localrc)
      call ESMF_FieldGet(water_temperature_field, farrayPtr=water_temperature, rc=localrc)
      zerod%temp = water_temperature(1,1,1)
    end if

#endif

    call Micro%run()
    call Macrofanua_run(Total_Bioturb)


#ifdef DEBUG
    call ESMF_FieldPrint (Microphytobenthos_erodibility)

    write(0,*) 'Mircrophy. erodibility effect', Micro%ErodibilityEffect

    call ESMF_FieldPrint (Microphytobenthos_critical_bed_shearstress)

    write(0,*) 'Mircrophy. Tau effect', Micro%TauEffect

    call ESMF_FieldPrint (Macrofauna_erodibility)

    write(0,*) ' Macro. erodibility effect', Total_Bioturb%ErodibilityEffect

    call ESMF_FieldPrint (Macrofauna_critical_bed_shearstress)

    write(0,*) ' Macro. Critical bed Shear stress', Total_Bioturb%TauEffect

    write(0,*) 'tau (macrofaunau and microphytobenthos) =' ,tau,' Both Biotic Critical bed shear stress effect= ',&
      &   Total_Bioturb%TauEffect, 'Both Biotic erodibility',Total_Bioturb%ErodibilityEffect

    write(0,*)
#endif

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
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

    integer :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)


    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory
    deallocate (Effect_of_MPB_on_sediment_erodibility_at_bottom)
    deallocate (Effect_of_MPB_on_critical_bed_shearstress)
    deallocate (Effect_of_Mbalthica_on_sediment_erodibility_at_bottom)
    deallocate (Effect_of_Mbalthica_on_critical_bed_shearstress)

    call ESMF_ClockDestroy(clock, rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


  end subroutine Finalize

end module benthos_component
