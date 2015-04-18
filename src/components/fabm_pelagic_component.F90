!> @brief FABM pelagic ESMF component
!
!> The ESMF/FABM pelagic driver component module provides infrastructure for the
!! MOSSCO pelagic component.
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define _RK4_ 1
#define _ADAPTIVE_EULER_ 2

#define RANGE2D 1:pel%inum,1:pel%jnum
#define RANGE3D RANGE2D,1:pel%knum

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "fabm_pelagic_component.F90"

module fabm_pelagic_component

  use esmf
  use mossco_fabm_pelagic
  use solver_library
  use mossco_strings
  use mossco_state
  use mossco_field
  use mossco_component

  implicit none

  private

  real(rk)  :: dt
  real(rk)  :: dt_min=1.0e-8_rk,relative_change_min=-0.9_rk
  integer   :: inum=1,jnum=1
  integer   :: t,tnum,k,n,numlayers
  integer   :: ode_method=1

  type :: type_2d_pointer
    real(rk),dimension(:,:), pointer :: p=>null()
  end type

  type :: type_3d_pointer
    real(rk),dimension(:,:,:), pointer :: p=>null()
  end type

  real(rk),dimension(:,:,:),pointer            :: diag=>null()
  real(rk),dimension(:,:),pointer              :: diag_hz=>null()
  type(type_2d_pointer), dimension(:), pointer :: bfl=>null()

  type(type_mossco_fabm_pelagic),save :: pel

  public SetServices

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

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
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

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)       :: currTime
    integer(ESMF_KIND_I4) :: localrc

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
#define ESMF_METHOD "Initialise_Advertise"
  subroutine Initialise_Advertise(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc
    type(ESMF_Field)     :: field
    integer(ESMF_KIND_I4) :: localrc

    character(ESMF_MAXSTR) :: name, message

    namelist /fabm_pelagic/ dt,ode_method,dt_min,relative_change_min

    !! read namelist input for control of timestepping
    open(33,file='fabm_pelagic.nml',action='read',status='old')
    read(33,nml=fabm_pelagic)
    close(33)

    !! Initialize FABM
    pel = mossco_create_fabm_pelagic()

    call ESMF_GridCompGet(gridComp, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! put concentration array and vertical velocity into export state
    ! it might be enough to do this once in initialize(?)
    do n=1,size(pel%export_states)
    end do

    !> this will not work, is state_grid contains halo zones
    do n=1,size(pel%model%info%diagnostic_variables)
      field = ESMF_FieldEmptyCreate( &
        name=only_var_name(pel%model%info%diagnostic_variables(n)%long_name)//'_in_water', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'units',trim(pel%model%info%diagnostic_variables(n)%units))
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(name)//' created diagnostic field '
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

      call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

    !! create forcing fields in import State
    do n=1,size(pel%bulk_dependencies)
    end do

    !! create forcing fields in import State
    do n=1,size(pel%horizontal_dependencies)
    end do

    !! prepare upward_flux forcing
    do n=1,size(pel%model%state_variables)
    end do

  end subroutine Initialise_Advertise


  !> Initialize phase 1
  !!
  !! Allocate memory for boundaries and fluxes, create ESMF fields
  !! and export them
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_TimeInterval) :: timeInterval,alarmInterval
    character(len=ESMF_MAXSTR) :: string,fileName,varname,wsname
    character(len=ESMF_MAXSTR) :: foreignGridFieldName
    character(len=ESMF_MAXSTR) :: attribute_name
    type(ESMF_Config)     :: config
    type(ESMF_FieldBundle) :: fieldBundle
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    type(ESMF_Field)     :: field,wsfield,concfield,tmpField
    type(ESMF_Field)     :: restartField
    type(ESMF_Array)     :: array
    integer              :: i,j,k,n
    integer              :: rank
    integer, allocatable :: maxIndex(:)
    type(ESMF_DELayout)  :: delayout
    type(ESMF_DistGrid)  :: distGrid_3d,distGrid_2d
    type(ESMF_Grid)      :: state_grid,horizontal_grid,foreign_grid
    type(ESMF_Mesh)      :: surface_mesh, state_mesh
    type(ESMF_ArraySpec) :: flux_array,state_array
    type(ESMF_StateItem_Flag) :: itemType
    type(ESMF_CoordSys_Flag) :: coordSys
    integer(ESMF_KIND_I4) :: localrc


    real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3
    real(ESMF_KIND_R8),dimension(:,:,:,:),pointer :: ptr_f4
    real(ESMF_KIND_R8)    :: attribute_r8
    real(ESMF_KIND_R8)    :: background_extinction
    integer(ESMF_KIND_I4) :: fieldcount
    integer(ESMF_KIND_I4) :: lbnd2(2),ubnd2(2),lbnd3(3),ubnd3(3)
    integer(ESMF_KIND_I4) :: totallwidth3(3), totaluwidth3(3)
    integer(ESMF_KIND_I4) :: totallwidth2(2), totaluwidth2(2)
    integer(ESMF_KIND_I4) :: totallwidth(3,1), totaluwidth(3,1)
    integer(ESMF_KIND_I8) :: tidx
    type(ESMF_Alarm)      :: outputAlarm

    character(len=ESMF_MAXSTR) :: timestring, name, message, units, esmf_name
    integer(ESMF_KIND_I4)      :: localPet, petCount, itemCount
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime, startTime, stopTime
    integer(ESMF_KIND_I8)      :: seconds, advanceCount
    type(ESMF_TimeInterval)    :: timeStep
    logical                    :: clockIsPresent
    integer                    :: deCount,numElements,numNodes
    integer,dimension(2)       :: distgridToArrayMap
    integer,dimension(3)       :: coordDimCount
    integer,dimension(3,3)     :: coordDimMap
    integer,dimension(:,:)  ,allocatable,target :: minIndexPDe,maxIndexPDe
    integer,dimension(:,:,:),allocatable,target :: deBlockList
    integer                    :: day_of_year, day, seconds_of_day
    logical, dimension(:,:,:), pointer :: mask=>null()
    integer, dimension(:,:,:), pointer :: gridmask=>null()
    real(ESMF_KIND_R8), dimension(:)  , pointer :: coord1d=>null()
    real(ESMF_KIND_R8), dimension(:,:), pointer :: coord2d=>null()
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)

    namelist /fabm_pelagic/ dt,ode_method,dt_min,relative_change_min,background_extinction

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Get the time step
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! get time information
    call ESMF_TimeGet(currTime, dd=day, s=seconds_of_day, &
                      dayOfYear=day_of_year, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! read namelist input for control of timestepping
    open(33,file='fabm_pelagic.nml',action='read',status='old')
    read(33,nml=fabm_pelagic)
    close(33)

    call ESMF_TimeIntervalSet(timeInterval,s_r8=dt,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockSet(clock, timeStep=timeInterval, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! get/set grid:
    !! rely on field with name foreignGridFieldName given as attribute and field
    !! in importState
    !! and just take the same grid&distgrid.
    !! so far, this is hardcoded to 1,1,numlayers
    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
           value=foreignGridFieldName, defaultValue='none',rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (trim(foreignGridFieldName)=='none') then
      call ESMF_ArraySpecSet(state_array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      state_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), &
                   maxIndex=(/inum,jnum,numlayers/), &
                   regDecomp=(/1,1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="pelagic states grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridAddCoord(state_grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      totalLWidth3(:)=0
      totalUWidth3(:)=0
      totalLWidth2(:)=0
      totalUWidth2(:)=0
    else
      call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldGet(field, grid=state_grid, rank=rank, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (rank == 3) then
        allocate(maxIndex(rank))
        call ESMF_GridGet(state_grid,staggerloc=ESMF_STAGGERLOC_CENTER,localDE=0, &
               exclusiveCount=maxIndex,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldGet(field, totalLWidth=totalLWidth, totalUWidth=totalUWidth, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        inum=maxIndex(1)
        jnum=maxIndex(2)
        numlayers=maxIndex(3)
        deallocate(maxIndex)
        totalLWidth3(:)=totalLWidth(:,1)
        totalUWidth3(:)=totalUWidth(:,1)
        totalLWidth2(:)=totalLWidth(1:2,1)
        totalUWidth2(:)=totalUWidth(1:2,1)
      else
        write(message,'(A)') 'foreign grid must be of rank = 3'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      end if
    end if

    call ESMF_GridGet(state_Grid,distgrid=distGrid_3D,        &
                                 coordSys=coordSys,           &
                                 coordDimCount=coordDimCount, &
                                 coordDimMap=coordDimMap)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_DistGridGet(distGrid_3D,delayout=delayout)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_DELayoutGet(delayout,deCount=deCount)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(minIndexPDe(3,deCount))
    allocate(maxIndexPDe(3,deCount))
    allocate(deBlockList(3,2,deCount))

    call ESMF_DistGridGet(distGrid_3D,minIndexPDe=minIndexPDe, &
                                      maxIndexPDe=maxIndexPDe)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    deBlockList(:,1,:) = minIndexPDe
    deBlockList(:,2,:) = maxIndexPDe

    distGrid_2D = ESMF_DistGridCreate(minval(deBlockList(1:2,1,:),2), &
                                      maxval(deBlockList(1:2,2,:),2), &
                                      int(deBlockList(1:2,:,:)),      &
                                      delayout=delayout)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    horizontal_grid = ESMF_GridCreate(distGrid_2D,name="pelagic horizontal grid", &
                                      gridAlign=(/1,1/),                          &
                                      coordSys=coordSys,                          &
                                      coordDimCount=int(coordDimCount(1:2)),      &
                                      coordDimMap=int(coordDimMap(1:2,1:2)),      &
                                      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do n=1,2
      if (coordDimCount(n) .eq. 1) then
        call ESMF_GridGetCoord(state_grid, staggerloc=ESMF_STAGGERLOC_CENTER, coorddim=n, farrayptr=coord1d, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        distgridToArrayMap = 0
        distgridToArrayMap(n) = 1
        array = ESMF_ArrayCreate(distGrid_2D,coord1d,indexflag=ESMF_INDEX_DELOCAL,distgridToArrayMap=distgridToArrayMap, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        call ESMF_GridGetCoord(state_grid, staggerloc=ESMF_STAGGERLOC_CENTER, coorddim=n, farrayptr=coord2d, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        array = ESMF_ArrayCreate(distGrid_2D,coord2d,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      end if
      call ESMF_GridSetCoord(horizontal_grid,n,array=array,staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do


    !! Initialize FABM
    pel = mossco_create_fabm_pelagic()

    ! set background extinction
    pel%background_extinction=background_extinction

    !! re-allocate state variables
    call ESMF_GridGetFieldBounds(state_grid,totalubound=ubnd3,totallbound=lbnd3,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    allocate(pel%conc(1-totalLWidth3(1):inum+totalUWidth3(1), &
                      1-totalLWidth3(2):jnum+totalUWidth3(2), &
                      1-totalLWidth3(3):numlayers+totalUWidth3(3), &
                      1:pel%nvar))
    !! get mask
    allocate(mask(1:inum,1:jnum,1:numlayers))
    mask = .false.
    call ESMF_GridGetItem(state_grid, ESMF_GRIDITEM_MASK, farrayPtr=gridmask, rc=localrc)
    if (localrc == ESMF_SUCCESS) then
      do i=1,inum
        do j=1,jnum
          do k=1,numlayers
            mask(i,j,k) = gridmask(i,j,k)==0
          end do
        end do
      end do
    end if

    call pel%initialize_domain(inum,jnum,numlayers,dt,mask=mask)
    call pel%update_pointers()
    call pel%initialize_concentrations()
    call pel%update_export_states(update_sinking=.false.)

    ! done with mask here
    deallocate(mask); nullify(mask)

    !! allocate local arrays
    allocate(bfl(pel%nvar))

    ! set solver_settings:
    pel%dt_min=dt_min
    pel%relative_change_min=relative_change_min

    ! put concentration array and vertical velocity into export state
    ! it might be enough to do this once in initialize(?)
    do n=1,size(pel%export_states)
      varname = trim(pel%export_states(n)%standard_name)//'_in_water'
      wsname  = trim(pel%export_states(n)%standard_name)//'_z_velocity_in_water'

      concfield = ESMF_FieldCreate(state_grid,farrayPtr=pel%export_states(n)%conc, &
                       name=trim(varname), &
                       totalLWidth=totalLWidth3,totalUWidth=totalUWidth3, &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(concfield,'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !! when memory is allocated, set pel%export_states(n)%conc to the values?

      !> create empty fields for restarts
      restartField = ESMF_FieldEmptyCreate(name=trim(varname),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(restartField,'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(restartField,'units',trim(pel%export_states(n)%units))
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldEmptySet(restartField, state_grid, &
                       staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAddReplace(importState,(/restartField/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> continue with statefield
      call ESMF_AttributeSet(concfield,'units',trim(pel%export_states(n)%units))
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Find all attributes in this state variable and add them to MOSSCO
      itemCount =  pel%model%info%state_variables(n)%properties%size()
      if (itemCount>0) allocate(itemNameList(itemCount))
      call pel%model%info%state_variables(n)%properties%keys(itemNameList)
      do i=1, itemCount
        !>@todo
        !if (pel%model%info%state_variables(n)%properties%get_property(itemNameList(i))%typecode()==1)
        !attribute_r8 = pel%model%info%state_variables(n)%properties%get_real(itemNameList(i), default=-1d30)
        !call ESMF_AttributeSet(concfield,trim(itemNameList(i)), attribute_r8)
        !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo

      if (allocated(itemNameList)) deallocate(itemNameList)

      !> add attributes relevant for MOSSCO
      !! mean_particle_diameter and particle density given only,
      !! if property persent
      !! this section can be removed once the more generic one above works
      attribute_name=trim('mean_particle_diameter')
      attribute_r8 = pel%model%info%state_variables(n)%properties%get_real('diameter',default=-99.d0)
      if (attribute_r8 > 0.0d0) &
        call ESMF_AttributeSet(concfield,attribute_name, attribute_r8)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      attribute_name=trim('particle_density')
      attribute_r8 = pel%model%info%state_variables(n)%properties%get_real('density',default=-99.d0)
      if (attribute_r8 > 0.0d0) &
        call ESMF_AttributeSet(concfield,attribute_name, attribute_r8)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> add fabm index in concentration array as "external_index" to be used by other components
      call ESMF_AttributeSet(concfield,'external_index',pel%export_states(n)%fabm_id)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      wsfield = ESMF_FieldCreate(state_grid,typekind=ESMF_TYPEKIND_R8, &
                       name=trim(wsname), &
                       totalLWidth=totalLWidth3,totalUWidth=totalUWidth3, &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(wsfield,'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> add fabm index in concentration array as "external_index" to be used by other components
      call ESMF_AttributeSet(wsfield,'external_index',pel%export_states(n)%fabm_id)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(wsfield,'units','m/s')
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldGet(field=wsfield, localDe=0, farrayPtr=pel%export_states(n)%ws, &
                     totalLBound=lbnd3,totalUBound=ubnd3, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      pel%export_states(n)%ws = 0.0d0

      write(message,'(A)') trim(name)//' created field '
      call MOSSCO_FieldString(wsfield, message)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

      !> add to state depending on existing items
      call ESMF_StateGet(exportState, trim(varname), itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemType == ESMF_STATEITEM_NOTFOUND) then
        call ESMF_StateAddReplace(exportState,(/concfield,wsfield/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else if (itemType ==ESMF_STATEITEM_FIELD) then
      !> if field present, remove from state, create bundle, add fields
        call ESMF_StateGet(exportState,trim(varname),field,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_StateRemove(exportState,(/ trim(varname) /),rc=localrc)
        fieldBundle = ESMF_FieldBundleCreate(fieldlist=(/field,concfield/), &
                name=trim(varname),   &
                multiflag=.true.,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(fieldBundle,'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_StateAddReplace(exportState,(/fieldBundle/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateGet(exportState, trim(wsname), field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_StateRemove(exportState, (/ trim(wsname) /), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        fieldBundle = ESMF_FieldBundleCreate(fieldlist=(/field,wsfield/), &
                name=trim(wsname),   &
                multiflag=.true.,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_StateAddReplace(exportState,(/fieldBundle/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(fieldBundle,'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      else if(itemType == ESMF_STATEITEM_FIELDBUNDLE) then
      !> if fieldBundle, get the bundle and add field
        call ESMF_StateGet(exportState,trim(varname),fieldBundle,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldBundleAdd(fieldBundle,(/concfield/),multiflag=.true.,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_StateGet(exportState,trim(wsname),fieldBundle,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldBundleAdd(fieldBundle,(/wsfield/),multiflag=.true.,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      end if
    end do

    !> this will not work, is state_grid contains halo zones
    do n=1,size(pel%model%info%diagnostic_variables)
        diag => pel%diagnostic_variables(n)
        !call ESMF_StateGet(exportState, &
        !  name=only_var_name(pel%model%info%diagnostic_variables(n)%long_name)//'_in_water', &
        !  field, rc=localrc)

        !call ESMF_FieldEmptyComplete(field,grid=state_grid,farrayPtr=diag, &
        field = ESMF_FieldCreate(state_grid,farrayPtr=diag, &
                   name=only_var_name(pel%model%info%diagnostic_variables(n)%long_name)//'_in_water', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(field,'units',trim(pel%model%info%diagnostic_variables(n)%units))
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' created diagnostic field '
        call MOSSCO_FieldString(field, message)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

        call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

    !! create forcing fields in import State
    if (associated(pel%bulk_dependencies)) then
      do n=1,size(pel%bulk_dependencies)
        !> check for existing field
        call ESMF_StateGet(importState, trim(pel%bulk_dependencies(n)%name)//'_in_water', itemType,rc=localrc)
        if (itemType == ESMF_STATEITEM_NOTFOUND) then
          field = ESMF_FieldCreate(state_grid, &
                    name=trim(pel%bulk_dependencies(n)%name)//'_in_water', &
                    typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_AttributeSet(field,'units',trim(pel%bulk_dependencies(n)%units))
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_FieldGet(field=field, farrayPtr=ptr_f3, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          ptr_f3 = 0.0_rk

          write(message,'(A)') trim(name)//' created bulk dependency field '
          call MOSSCO_FieldString(field, message)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

          call ESMF_StateAdd(importState,(/field/),rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        else
          call ESMF_StateGet(importState, trim(pel%bulk_dependencies(n)%name)//'_in_water', field, rc=localrc)
          write(message,'(A)') trim(name)//' uses existing bulk dependency field '
          call MOSSCO_FieldString(field, message)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
        end if
        attribute_name=trim(pel%bulk_dependencies(n)%name)//'_in_water'
        call set_item_flags(importState,attribute_name,requiredFlag=.true.,requiredRank=3)
        !! set FABM's pointers to dependencies data,
        !! this probably has to be done only once (here) and not in Run
        call ESMF_StateGet(importState, trim(pel%bulk_dependencies(n)%name)//'_in_water', field=field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldGet(field=field, farrayPtr=ptr_f3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call pel%set_environment(pel%bulk_dependencies(n)%name,ptr_bulk=ptr_f3)
      end do
    end if

    if (associated(pel%horizontal_dependencies)) then
      do n=1,size(pel%horizontal_dependencies)
        !> check for existing field
        if (trim(pel%horizontal_dependencies(n)%name)=='bottom_depth') then
          esmf_name = 'water_depth_at_soil_surface'
        else
          esmf_name = pel%horizontal_dependencies(n)%name
        end if
        call ESMF_StateGet(importState, trim(esmf_name), itemType,rc=localrc)
        if (itemType == ESMF_STATEITEM_NOTFOUND) then

          field = ESMF_FieldCreate(horizontal_grid, &
               name=trim(esmf_name), &
               typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_AttributeSet(field,'units',trim(pel%horizontal_dependencies(n)%units))
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          write(message,'(A)') trim(name)//' created horizontal dependency field '
          call MOSSCO_FieldString(field, message)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

          call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        else
          call ESMF_StateGet(importState, trim(esmf_name), field, rc=localrc)
          write(message,'(A)') trim(name)//' uses existing horizontal dependency field '
          call MOSSCO_FieldString(field, message)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
        end if
        attribute_name=trim(esmf_name)
        call set_item_flags(importState,attribute_name,requiredFlag=.true.,requiredRank=2)
        !! set FABM's pointers to dependencies data,
        !! this probably has to be done only once (here) and not in Run
        call ESMF_StateGet(importState, trim(esmf_name), field=field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldGet(field, farrayPtr=ptr_f2, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (itemType == ESMF_STATEITEM_NOTFOUND) then
        ptr_f2 = 0.0_rk
        end if
        ! check for valid upper bounds of possibly existing array
        if ((ubound(ptr_f2,1).lt.pel%inum).or. &
            (ubound(ptr_f2,2).lt.pel%jnum).or. &
            (lbound(ptr_f2,1).gt.1).or. &
            (lbound(ptr_f2,2).gt.1)) then
          write(message,*) 'upper bounds of possibly existing 2d array for ', &
                           trim(esmf_name), &
                           ' does not fit into domain: ',size(ptr_f2), &
                           'vs.',pel%inum,pel%jnum
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR,rc=localrc)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
        call pel%set_environment(pel%horizontal_dependencies(n)%name,ptr_horizontal=ptr_f2)
      end do
    end if


    !! prepare upward_flux forcing
    do n=1,size(pel%model%state_variables)
      varname = trim(only_var_name(pel%model%state_variables(n)%long_name))//'_upward_flux_at_soil_surface'
      field = ESMF_FieldCreate(horizontal_grid, &
             name=varname, &
             typekind=ESMF_TYPEKIND_R8, &
             staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'external_index',pel%export_states(n)%fabm_id)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !! initialise with zeros
      call ESMF_FieldGet(field=field, farrayPtr=bfl(n)%p, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      bfl(n)%p = 0.0_rk
      attribute_name=trim(varname)
      call set_item_flags(importState,attribute_name,requiredFlag=.false.,optionalFlag=.true.,requiredRank=2)

      write(message,'(A)') trim(name)//' created field '
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

      !> add to importState
      call ESMF_StateGet(importState, itemName=trim(varname), itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemType == ESMF_STATEITEM_NOTFOUND) then
        !> is not present, just add field
        call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else if (itemType ==ESMF_STATEITEM_FIELD) then
        !> if field present, remove from state, create bundle, add fields
        call ESMF_StateGet(importState,trim(varname),tmpField,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_StateRemove(importState,(/ trim(varname) /),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        fieldBundle = ESMF_FieldBundleCreate(fieldlist=(/tmpField,field/), &
                name=trim(varname),   &
                multiflag=.true.,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_StateAddReplace(importState,(/fieldBundle/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else if(itemType == ESMF_STATEITEM_FIELDBUNDLE) then
        !> if fieldBundle, get the bundle and add field
        call ESMF_StateGet(importState,trim(varname),fieldBundle,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldBundleAdd(fieldBundle,(/field/),multiflag=.true.,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      end if
    end do

    !> get z-positions of vertical layer interfaces
    call ESMF_GridGetCoord(state_grid, coordDim=3, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
           farrayPtr=pel%zi, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !call ESMF_StatePrint(importState)
    !call ESMF_StatePrint(exportState)

    !> set global time, such that fabm can calculate initial diagnostics
    call pel%set_time(day_of_year, seconds_of_day)

    !> check consistency of fabm setup
    call pel%check_ready()

    !> now initialize diagnostic variables.
    !! after check_ready FABM's internal pointers are set correctly
    do n=1,size(pel%model%horizontal_diagnostic_variables)
        diag_hz => pel%horizontal_diagnostic_variables(n)
        field = ESMF_FieldCreate(horizontal_grid,farrayPtr=diag_hz, &
          name=only_var_name(pel%model%info%horizontal_diagnostic_variables(n)%long_name)//'_hz', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(field,'units',trim(pel%model%info%horizontal_diagnostic_variables(n)%units))
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' created horizontal diagnostic field '
        call MOSSCO_FieldString(field, message)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

        call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

    !> also update export states again with sinking velocities
    !! todo: this has to go into a second init phase,
    !!       when real forcing is linked. Also diagnostic variables could
    !!       be initialised, while doing a 0-timestep based on initial fields
    !!       and forcing
    call pel%update_export_states(update_sinking=.true.)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1


#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    type(ESMF_Time)            :: currTime
    character(len=ESMF_MAXSTR) :: message, name
    integer(ESMF_KIND_I4)      :: localrc
    type(ESMF_Clock)           :: clock

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> lookup the importState for restart data
    call ReadRestart(gridComp, importState, exportState, parentClock, rc=localrc)

    !> update sinking after restart
    call pel%update_export_states(update_sinking=.true.)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP2


#undef  ESMF_METHOD
#define ESMF_METHOD "update_import_pointers"
  subroutine update_import_pointers(importState)

    implicit none

    type(ESMF_State)          :: importState

    type(ESMF_StateItem_Flag) :: itemType
    type(ESMF_Field)          :: field
    real(ESMF_KIND_R8), dimension(:,:), pointer :: ptr_f2=>null()
    integer                   :: localrc
    character(len=ESMF_MAXSTR):: varname

    ! todo: add bulk dependencies

    ! link horizontal dependencies
    if (associated(pel%horizontal_dependencies)) then
      do n=1,size(pel%horizontal_dependencies)
        !> check for existing field
        call ESMF_StateGet(importState, trim(pel%horizontal_dependencies(n)%name), itemType,rc=localrc)
        if (itemType == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(importState, trim(pel%horizontal_dependencies(n)%name), field=field, rc=localrc)
          call ESMF_FieldGet(field, farrayPtr=ptr_f2, rc=localrc)
          call pel%set_environment(pel%horizontal_dependencies(n)%name,ptr_horizontal=ptr_f2)
        end if
      end do
    end if

#if 0
    !! re-link upward_flux forcing
    do n=1,size(pel%model%state_variables)
      varname = trim(only_var_name(pel%model%state_variables(n)%long_name))//'_upward_flux_at_soil_surface'
      call ESMF_StateGet(importState, trim(varname), itemType,rc=localrc)
      if (itemType == ESMF_STATEITEM_FIELD) then
        !write(0,*) 'field ',trim(varname),' found'
        call ESMF_StateGet(importState, trim(varname), field=field, rc=localrc)
        call ESMF_FieldGet(field, farrayPtr=bfl(n)%p, rc=localrc)
      else
        !write(0,*) 'field ',trim(varname),' not found'
      end if
    end do
#endif

  end subroutine


#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=ESMF_MAXSTR)  :: name,message,varname
    type(ESMF_Time)             :: currTime
    integer                     :: localrc,n

    integer(ESMF_KIND_I4)          :: ubnd(3),lbnd(3),ownshape(3)
    real(ESMF_KIND_R8), pointer    :: ptr_f3(:,:,:)
    type(ESMF_FieldStatus_Flag)    :: fieldstatus
    type(ESMF_StateItem_Flag)      :: itemtype
    type(ESMF_Field)               :: field

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> browse through list of state variables and
    !! copy data from importState fields with same name
    do n=1,size(pel%export_states)
      varname=trim(pel%export_states(n)%standard_name)//'_in_water'
      call ESMF_StateGet(importState, trim(varname), itemType=itemType, rc=localrc)
      if (itemType==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(importState, trim(varname), field=field, rc=localrc)
        call ESMF_FieldGet(field, status=fieldstatus, rc=localrc)
        if (fieldstatus== ESMF_FIELDSTATUS_COMPLETE) then
          call ESMF_FieldGet(field, farrayPtr=ptr_f3, &
               exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
          ownshape = shape(pel%export_states(n)%data)
          if ((ubnd(1)-lbnd(1)+1.ne.ownshape(1)).or. &
              (ubnd(2)-lbnd(2)+1.ne.ownshape(2)).or. &
              (ubnd(3)-lbnd(3)+1.ne.ownshape(3))) then
            write(message,'(A)') trim(name)//' incompatible shape of field'
            call mossco_fieldString(field, message)
            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
            write(message,'(A,4I3,A,4I3)') trim(name)//' own shape', ownshape, ' other shape ', &
              ubnd(:)-lbnd(:)+ (/1,1,1/)
            call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          end if
          pel%export_states(n)%conc = ptr_f3
          write(message,'(A)') trim(name)//' hotstarted field'
          call mossco_fieldString(field, message)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
        else
          write(message,'(A)') trim(name)//' incomplete field'
          call mossco_fieldString(field, message)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        end if
      else
        write(message,'(A)') trim(name)//' skipped hotstart for variable '//trim(varname)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
        call MOSSCO_StateLog(importState)
      end if
    end do

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine ReadRestart

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    real(ESMF_KIND_R8),pointer,dimension(:,:) :: ptr_f2
    real(ESMF_KIND_R8),pointer,dimension(:,:,:) :: ptr_f3

    integer           :: k,n
    integer(8)        :: t
    integer           :: seconds_of_day, day_of_year, day

    character(len=ESMF_MAXSTR) :: name, message, prefix
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime, stopTime
    type(ESMF_TimeInterval)    :: timeStep
    integer(ESMF_KIND_I4)      :: localrc

    type(ESMF_Field)                       :: importField, exportField
    type(ESMF_Field), allocatable          :: exportFieldList(:), importFieldList(:)
    character(ESMF_MAXSTR), allocatable    :: itemNameList(:)
    character(ESMF_MAXSTR)                 :: itemName
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_StateItem_Flag)              :: itemType
    type(ESMF_FieldStatus_Flag)            :: fieldStatus
    integer(ESMF_KIND_I4)                  :: i, j, l, nmatch, itemCount, rank
    integer(ESMF_KIND_I4)                  :: ubnd(2), lbnd(2)
    integer(ESMF_KIND_I8)                  :: advanceCount

    real(ESMF_KIND_R8), pointer            :: farrayPtr3(:,:,:), ratePtr3(:,:,:)
    real(ESMF_KIND_R8), pointer            :: farrayPtr2(:,:), ratePtr2(:,:)

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    ! set global time information
    call ESMF_TimeGet(currTime, dd=day, s=seconds_of_day, &
                      dayOfYear=day_of_year, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call pel%set_time(day_of_year, seconds_of_day)

    ! calculate layer_heights
    call pel%update_grid()

    ! update pointers from import
    call update_import_pointers(importState)

    ! calculate PAR
    call pel%light()

    ! Create a list of matching flux and state
    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(itemNameList(itemCount))
    allocate(itemTypeList(itemCount))
    call ESMF_StateGet(importState, itemNameList=itemNameList, itemTypeList=itemTypeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(exportFieldList(itemCount))
    allocate(importFieldList(itemCount))

    nmatch=0
    do i=1, itemCount
      j=index(itemNameList(i),'_flux_in_water')
      if (j<1) j=index(itemNameList(i),'_flux_at_water_surface')
      if (j<1) j=index(itemNameList(i),'_flux_at_surface')
      if (j<1) j=index(itemNameList(i),'_flux_at_soil_surface')
      if (j<1) cycle

      itemName=itemNameList(i)
      prefix=itemName(1:j-1)
      call ESMF_StateGet(exportState, trim(prefix)//'_in_water', itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> @todo implement for data in fieldbundles, log warning only on first run
      if (itemType == ESMF_STATEITEM_FIELDBUNDLE .and. advanceCount<1) then
        write(message,'(A)') trim(name)//' cannot yet process matching fieldbundle for flux '
        call MOSSCO_FieldString(importField, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      endif

      if (itemType /= ESMF_STATEITEM_FIELD) cycle

      call ESMF_StateGet(exportState, trim(prefix)//'_in_water', exportField, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

     ! call ESMF_AttributeGet(exportField, 'external_index', isPresent=isPresent, rc=localrc)
     ! if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
     !   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

     ! if (.not.isPresent) cycle

     ! call ESMF_AttributeGet(exportField, 'external_index', isPresent=isPresent, rc=localrc)
     ! if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
     !   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateGet(importState, trim(itemName), importField, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      nmatch=nmatch+1
      exportFieldList(nmatch)=exportField
      importFieldList(nmatch)=importField
      itemNameList(nmatch)=itemNameList(i)

      !! Only log successful matching the first time Run() operates
      if (advanceCount<1) then
        write(message,'(A)') trim(name)//' found matching field for flux '
        call MOSSCO_FieldString(importField, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

    enddo

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, stopTime=stopTime, currTime=currTime, timeStep=timeStep, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalGet(timeStep, s_r8=dt, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do while (.not.ESMF_ClockIsStopTime(clock))

      call ESMF_ClockGet(clock, currTime=currTime, advanceCount=t, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (currTime + timeStep > stopTime) then
        timeStep=stopTime-currTime
        call ESMF_TimeIntervalGet(timeStep, s_r8=dt, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      ! integrate rates
      call ode_solver(pel,dt,ode_method)

      ! integrate bottom upward fluxes
      ! todo: this does not work with the link coupler, yet. the bfl(:)%p pointers
      !       have to be updated from importState here in Run
      do n=1,pel%nvar
        pel%conc(RANGE2D,1,n) = pel%conc(RANGE2D,1,n) + bfl(n)%p(RANGE2D)*dt/pel%layer_height(RANGE2D,1)
      end do

      do i=1, nmatch

        write(message,'(A)') trim(name)//' add flux field '
        call MOSSCO_FieldString(importFieldList(i), message)

        write(message,'(A)') trim(message)//' to field '
        call MOSSCO_FieldString(exportFieldList(i), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        call ESMF_FieldGet(exportFieldList(i), farrayPtr=farrayPtr3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGet(importFieldList(i), rank=rank, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (rank==2) then
          call ESMF_FieldGetBounds(importFieldList(i), exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldGet(importFieldList(i), farrayPtr=ratePtr2, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !> If it is a vertically averaged (2D-) flux (expected unit mmol m**-3)
          if (index(itemNameList(i),'_flux_in_water')>0) then
            do k=1,pel%knum
              where(ratePtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2))>0)
                farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),k) = farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),k)  &
                                                              + ratePtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) * dt
              endwhere
            enddo
          !> Otherwise it is a surface (2D-) flux (expected unit mmol m**-2), that needs
          !> to be converted to volume concentration by division with layer_height
          elseif (index(itemNameList(i),'_flux_at_surface')>0 .or. &
                  index(itemNameList(i),'_flux_at_water_surface')>0) then
            where (ratePtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2))>0)
              farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),pel%knum) = farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),pel%knum) &
                + ratePtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) * dt / pel%layer_height(lbnd(1):ubnd(1),lbnd(2):ubnd(2),pel%knum)
            endwhere
          elseif (index(itemNameList(i),'_flux_at_soil_surface')>0) then
            where(ratePtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2))>0)
              farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),1) = farrayPtr3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),1) &
                + ratePtr2(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) * dt / pel%layer_height(lbnd(1):ubnd(1),lbnd(2):ubnd(2),1)
            endwhere
          else
            write (message,'(A)') trim(name)//' could not locate/add flux field'
            call MOSSCO_FieldString(importFieldList(i),message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif
        elseif (rank==3) then
          call ESMF_FieldGet(importFieldList(i), farrayPtr=ratePtr3, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          where(ratePtr3(:,:,:)>0)
            farrayPtr3(:,:,:) = farrayPtr3(:,:,:)  + ratePtr3(:,:,:) * dt
          endwhere
        endif
      enddo

      !call add_fluxes(importState, dt=dt, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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

      ! link fabm state
      call pel%update_pointers()
      call pel%update_expressions()

      call ESMF_ClockAdvance(clock, timeStep=timeStep, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    if (allocated(exportFieldList)) deallocate(exportFieldList)
    if (allocated(importFieldList)) deallocate(importFieldList)
    if (allocated(itemNameList))    deallocate(itemNameList)
    if (allocated(itemTypeList))    deallocate(itemTypeList)

    !> prepare component's export
    call pel%update_export_states()

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    integer(ESMF_KIND_I4)   :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory
    !! 3. Destroy your clock

    !! @todo The clockIsPresent statement does not detect if a clock has been destroyed
    !! previously, thus, we comment the clock destruction code while this has not
    !! been fixed by ESMF
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (associated(bfl)) deallocate(bfl)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module fabm_pelagic_component
