!> @brief FABM pelagic ESMF component
!
!> The ESMF/FABM pelagic driver component module provides infrastructure for the
!! MOSSCO pelagic component.
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, 2014, Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#include "fabm_driver.h"

#define _RK4_ 1
#define _ADAPTIVE_EULER_ 2

module fabm_pelagic_component

  use esmf
  use fabm
  use fabm_types
  use mossco_fabm_pelagic
  use solver_library
  use mossco_strings
  use mossco_state
  use mossco_component

  implicit none

  private
 
  real(rk)  :: dt
  real(rk)  :: dt_min=1.0e-8_rk,relative_change_min=-0.9_rk
  integer   :: inum=1,jnum=1
  integer   :: t,tnum,k,n,numlayers
  integer   :: ode_method=1

  type :: type_2d_pointer
    real(rk),dimension(:,:), pointer :: p
  end type

  type :: type_3d_pointer
    real(rk),dimension(:,:,:), pointer :: p
  end type

  real(rk),dimension(:,:,:),pointer            :: diag
  type(type_2d_pointer), dimension(:), pointer :: bfl
 
  type(type_mossco_fabm_pelagic),save :: pel

  public :: SetServices,rk
  
  contains

  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices
  
    subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)
 
    implicit none
  
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)       :: currTime

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=rc)
    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=rc)

    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine InitializeP0

  
  !> Initialize phase 1
  !!
  !! Allocate memory for boundaries and fluxes, create ESMF fields
  !! and export them
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_TimeInterval) :: timeInterval,alarmInterval
    character(len=ESMF_MAXSTR) :: string,fileName,varname
    character(len=ESMF_MAXSTR) :: foreignGridFieldName
    character(len=ESMF_MAXSTR) :: attribute_name
    type(ESMF_Config)     :: config
    type(ESMF_FieldBundle) :: fieldBundle
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    type(ESMF_Field)     :: field,wsfield,concfield,tmpField
    type(ESMF_Array)     :: array
    integer              :: i
    integer              :: rank
    integer, allocatable :: maxIndex(:)
    type(ESMF_DistGrid)  :: distGrid_3d,distGrid_2d
    type(ESMF_Grid)      :: state_grid,horizontal_grid,foreign_grid
    type(ESMF_Mesh)      :: surface_mesh, state_mesh
    type(ESMF_ArraySpec) :: flux_array,state_array
    type(ESMF_StateItem_Flag) :: itemType


    real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3
    real(ESMF_KIND_R8),dimension(:,:,:,:),pointer :: ptr_f4
    real(ESMF_KIND_R8)    :: attribute_r8
    integer(ESMF_KIND_I4) :: fieldcount
    integer(ESMF_KIND_I4) :: lbnd2(2),ubnd2(2),lbnd3(3),ubnd3(3)
    integer(ESMF_KIND_I8) :: tidx
    type(ESMF_Alarm)      :: outputAlarm
  
    character(len=ESMF_MAXSTR) :: timestring, name, message, units
    integer(ESMF_KIND_I4)      :: localPet, petCount, itemCount
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime, startTime, stopTime
    integer(ESMF_KIND_I8)      :: seconds, advanceCount
    type(ESMF_TimeInterval)    :: timeStep
    logical                    :: clockIsPresent
    integer                    :: numElements,numNodes

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Get the time step
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(clock, timeStep=timeInterval, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeIntervalGet(timeInterval,s_r8=dt,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! get/set grid:
    !! rely on field with name foreignGridFieldName given as attribute and field
    !! in importState
    !! and just take the same grid&distgrid.
    !! so far, this is hardcoded to 1,1,numlayers
    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
           value=foreignGridFieldName, defaultValue='none',rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (trim(foreignGridFieldName)=='none') then
      call ESMF_ArraySpecSet(state_array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      state_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), &
                   maxIndex=(/inum,jnum,numlayers/), &
                   regDecomp=(/1,1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="pelagic states grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridAddCoord(state_grid, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    else
      call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field, grid=state_grid, rank=rank, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if (rank == 3) then
        allocate(maxIndex(rank))
        call ESMF_GridGet(state_grid,staggerloc=ESMF_STAGGERLOC_CENTER,localDE=0, &
               computationalCount=maxIndex,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        inum=maxIndex(1)
        jnum=maxIndex(2)
        numlayers=maxIndex(3)
        deallocate(maxIndex)
      else
        write(message,*) 'foreign grid must be of rank = 3'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      end if
    end if
    horizontal_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                   maxIndex=(/inum,jnum/), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="pelagic horizontal grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridAddCoord(horizontal_grid, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
     
    !! Initialize FABM
    pel = mossco_create_fabm_pelagic(inum,jnum,numlayers,dt)

    !! re-allocate state variables
    !! todo: re-set initial values
    if (associated(pel%conc)) deallocate(pel%conc)
    call ESMF_GridGetFieldBounds(state_grid,totalubound=ubnd3,totallbound=lbnd3,rc=rc)
    allocate(pel%conc(lbnd3(1):ubnd3(1),lbnd3(2):ubnd3(2),lbnd3(3):ubnd3(3),1:pel%nvar))
    call pel%update_export_states()

    !! allocate local arrays
    allocate(bfl(pel%nvar))
 
    ! set solver_settings:
    pel%dt_min=dt_min
    pel%relative_change_min=relative_change_min

    ! put concentration array and vertical velocity into export state
    ! it might be enough to do this once in initialize(?)
    do n=1,size(pel%export_states)
      varname = trim(pel%export_states(n)%standard_name)//'_in_water'
      concfield = ESMF_FieldCreate(state_grid,farrayPtr=pel%export_states(n)%conc, &
                       name=trim(varname), &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_AttributeSet(concfield,'units',trim(pel%export_states(n)%units))
      !> add attributes relevant for MOSSCO
      !! mean_particle_diameter and particle density given only,
      !! if property persent
      attribute_name=trim('mean_particle_diameter')
      attribute_r8 = pel%model%info%state_variables(n)%properties%get_real('diameter',default=-99.d0)
      if (attribute_r8 > 0.0d0) &
        call ESMF_AttributeSet(field,attribute_name, attribute_r8)
      attribute_name=trim('particle_density')
      attribute_r8 = pel%model%info%state_variables(n)%properties%get_real('density',default=-99.d0)
      if (attribute_r8 > 0.0d0) &
        call ESMF_AttributeSet(concfield,attribute_name, attribute_r8)
      !> add fabm index in concentration array as "external_index" to be used by other components
      call ESMF_AttributeSet(concfield,'external_index',pel%export_states(n)%fabm_id)

      wsfield = ESMF_FieldCreate(state_grid,typekind=ESMF_TYPEKIND_R8, &
                       name=trim(varname)//'_z_velocity', &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_AttributeSet(wsfield,'units','m/s')
      call ESMF_FieldGet(field=wsfield, localDe=0, farrayPtr=pel%export_states(n)%ws, &
                     totalLBound=lbnd3,totalUBound=ubnd3, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      !> add to state depending on existing items
      call ESMF_StateGet(exportState, trim(varname), itemType, rc=rc)

      if (itemType == ESMF_STATEITEM_NOTFOUND) then
        call ESMF_StateAddReplace(exportState,(/concfield,wsfield/),rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      else if (itemType ==ESMF_STATEITEM_FIELD) then
      !> if field present, remove from state, create bundle, add fields
        call ESMF_StateGet(exportState,trim(varname),field,rc=rc)
        call ESMF_StateRemove(exportState,(/ trim(varname) /),rc=rc)
        fieldBundle = ESMF_FieldBundleCreate(fieldlist=(/field,concfield/), &
                name=trim(varname),   &
                multiflag=.true.,rc=rc)
        call ESMF_StateAddReplace(exportState,(/fieldBundle/),rc=rc)

        call ESMF_StateGet(exportState, &
                trim(varname)//'_z_velocity',field,rc=rc)
        call ESMF_StateRemove(exportState, &
                (/ trim(varname)//'_z_velocity' /),rc=rc)
        fieldBundle = ESMF_FieldBundleCreate(fieldlist=(/field,wsfield/), &
                name=trim(varname)//'_z_velocity',   &
                multiflag=.true.,rc=rc)
        call ESMF_StateAddReplace(exportState,(/fieldBundle/),rc=rc)

      else if(itemType == ESMF_STATEITEM_FIELDBUNDLE) then
      !> if fieldBundle, get the bundle and add field
        call ESMF_StateGet(exportState,trim(varname),fieldBundle,rc=rc)
        call ESMF_FieldBundleAdd(fieldBundle,(/concfield/),multiflag=.true.,rc=rc)
        call ESMF_StateGet(exportState,trim(varname)//'_z_velocity',fieldBundle,rc=rc)
        call ESMF_FieldBundleAdd(fieldBundle,(/wsfield/),multiflag=.true.,rc=rc)
      end if
    end do

    do n=1,size(pel%model%info%diagnostic_variables)
        diag => pel%diagnostic_variables(n)
        field = ESMF_FieldCreate(state_grid,farrayPtr=diag, &
                   name=only_var_name(pel%model%info%diagnostic_variables(n)%long_name)//'_in_water', rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_AttributeSet(field,'units',trim(pel%model%info%diagnostic_variables(n)%units))
        
        call ESMF_StateAddReplace(exportState,(/field/),rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end do

    !! create forcing fields in import State
    do n=1,size(pel%bulk_dependencies)
        field = ESMF_FieldCreate(state_grid, &
               name=trim(pel%bulk_dependencies(n)%name)//'_in_water', &
               typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_AttributeSet(field,'units',trim(pel%bulk_dependencies(n)%units))
        ! add field to state, if not present
        call ESMF_StateAdd(importState,(/field/),rc=rc)
        if(rc /= ESMF_SUCCESS) write(0,*) 'use existing field: ',trim(pel%bulk_dependencies(n)%name)//'_in_water'
        attribute_name=trim(pel%bulk_dependencies(n)%name)//'_in_water'
        call set_item_flags(importState,attribute_name,requiredFlag=.true.,requiredRank=3)
        !! set FABM's pointers to dependencies data,
        !! this probably has to be done only once (here) and not in Run
        call ESMF_StateGet(importState, trim(pel%bulk_dependencies(n)%name)//'_in_water', field=field, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldGet(field=field, farrayPtr=ptr_f3, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call pel%set_environment(pel%bulk_dependencies(n)%name,ptr_bulk=ptr_f3)
    end do

    do n=1,size(pel%horizontal_dependencies)
        field = ESMF_FieldCreate(horizontal_grid, &
               name=trim(pel%horizontal_dependencies(n)%name), &
               typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_AttributeSet(field,'units',trim(pel%bulk_dependencies(n)%units))
        !! add field to state, if not present
        call ESMF_StateAddReplace(importState,(/field/),rc=rc)
        if(rc /= ESMF_SUCCESS) write(0,*) 'use existing field: ',trim(pel%horizontal_dependencies(n)%name)
        attribute_name=trim(pel%horizontal_dependencies(n)%name)
        call set_item_flags(importState,attribute_name,requiredFlag=.true.,requiredRank=2)
        !! set FABM's pointers to dependencies data,
        !! this probably has to be done only once (here) and not in Run
        call ESMF_StateGet(importState, trim(pel%horizontal_dependencies(n)%name), field=field, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldGet(field=field, farrayPtr=ptr_f2, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call pel%set_environment(pel%horizontal_dependencies(n)%name,ptr_horizontal=ptr_f2)
    end do

    !! prepare upward_flux forcing
    do n=1,size(pel%model%state_variables)
      varname = trim(only_var_name(pel%model%state_variables(n)%long_name))//'_upward_flux'
      field = ESMF_FieldCreate(horizontal_grid, &
             name=varname, &
             typekind=ESMF_TYPEKIND_R8, &
             staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      !! initialise with zeros
      call ESMF_FieldGet(field=field, farrayPtr=bfl(n)%p, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      bfl(n)%p = 0.0_rk
      attribute_name=trim(varname)
      call set_item_flags(importState,attribute_name,requiredFlag=.false.,optionalFlag=.true.,requiredRank=3)

      !> add to importState
      call ESMF_StateGet(importState, trim(varname), itemType,rc=rc)
      if (itemType == ESMF_STATEITEM_NOTFOUND) then
        !> is not present, just add field
        call ESMF_StateAddReplace(importState,(/field/),rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      else if (itemType ==ESMF_STATEITEM_FIELD) then
        !> if field present, remove from state, create bundle, add fields
        call ESMF_StateGet(importState,trim(varname),tmpField,rc=rc)
        call ESMF_StateRemove(importState,(/ trim(varname) /),rc=rc)
        fieldBundle = ESMF_FieldBundleCreate(fieldlist=(/tmpField,field/), &
                name=trim(varname),   &
                multiflag=.true.,rc=rc)
        call ESMF_StateAddReplace(importState,(/fieldBundle/),rc=rc)
      else if(itemType == ESMF_STATEITEM_FIELDBUNDLE) then
        !> if fieldBundle, get the bundle and add field
        call ESMF_StateGet(importState,trim(varname),fieldBundle,rc=rc)
        call ESMF_FieldBundleAdd(fieldBundle,(/field/),multiflag=.true.,rc=rc)
      end if
    end do

    !call ESMF_StatePrint(importState)
    !call ESMF_StatePrint(exportState)
    call pel%check_ready()

    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine InitializeP1


  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc
 
    real(ESMF_KIND_R8),pointer,dimension(:,:) :: ptr_f2
    real(ESMF_KIND_R8),pointer,dimension(:,:,:) :: ptr_f3
    integer           :: i,j
    integer(8)     :: t
 
    character(len=ESMF_MAXSTR) :: name
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime
    
    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    ! calculate PAR
    call pel%light()

	  call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do while (.not.ESMF_ClockIsStopTime(clock))
      ! integrate rates
      call ode_solver(pel,dt,ode_method)

      ! integrate bottom upward fluxes
      do n=1,pel%nvar
        pel%conc(:,:,1,n) = pel%conc(:,:,1,n) + bfl(n)%p*dt/pel%layer_height(:,:,1)
      end do

      ! reset concentrations to mininum_value
      do n=1,pel%nvar
        do k=1,pel%knum
        do j=1,pel%jnum
        do i=1,pel%inum
          if (pel%conc(i,j,k,n) .lt. pel%model%info%state_variables(n)%minimum) then
            pel%conc(i,j,k,n) = pel%model%info%state_variables(n)%minimum
          end if
        end do
        end do
        end do
      end do

      call ESMF_ClockGet(clock, advanceCount=t, rc=rc)

      call ESMF_ClockAdvance(clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo

    !> prepare component's export   
    call pel%update_export_states()
 
    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory
    !! 3. Destroy your clock

    !! @todo The clockIsPresent statement does not detect if a clock has been destroyed
    !! previously, thus, we comment the clock destruction code while this has not
    !! been fixed by ESMF
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
		call ESMF_ClockDestroy(clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (associated(bfl)) deallocate(bfl)

    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Finalize

end module fabm_pelagic_component
