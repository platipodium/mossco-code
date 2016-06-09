!> @brief FABM benthic ESMF component
!
!> The ESMF/FABM benthic driver component module provides infrastructure for the
!! MOSSCO benthic component.
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015, 2016 Helmholtz-Zentrum Geesthacht
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

#define RANGE2D 1:ben%inum,1:ben%jnum
#define RANGE3D RANGE2D,1:ben%knum

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "fabm_benthic_component.F90"

module fabm_benthic_component

  use esmf
  use mossco_fabm_benthic
  use solver_library
  use mossco_strings
  use mossco_state
  use mossco_field
  use mossco_component
  use mossco_grid

  implicit none

  private

  real(rk)  :: dt
  real(rk)  :: dt_min=1.0e-8_rk,relative_change_min=-0.9_rk
  integer   :: inum=1,jnum=1
  integer   :: t,tnum,n
  integer, parameter :: numlayers = 1
  integer   :: ode_method=1

  type :: type_2d_pointer
    real(rk),dimension(:,:), pointer :: p=>null()
  end type

  type :: type_3d_pointer
    real(rk),dimension(:,:,:), pointer :: p=>null()
  end type

  real(rk),dimension(:,:),pointer              :: diag=>null()
  !type(type_2d_pointer), dimension(:), pointer :: bfl=>null()

  type(type_mossco_fabm_benthic),save :: ben

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

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "Initialise_Advertise"
  subroutine Initialise_Advertise(gridComp, importState, exportState, parentClock, rc)
    use fabm_types, only: output_none
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc
    type(ESMF_Field)     :: field
    integer(ESMF_KIND_I4) :: localrc, i
    character(ESMF_MAXSTR), dimension(10,2) :: stringList
    character(ESMF_MAXSTR) :: convention, purpose

    character(ESMF_MAXSTR) :: name, message

    namelist /fabm_benthic/ dt,ode_method,dt_min,relative_change_min

    !! read namelist input for control of timestepping
    open(33,file='fabm_benthic.nml',action='read',status='old')
    read(33,nml=fabm_benthic)
    close(33)

    call ESMF_AttributeSet(exportState, trim(name)//'::dt', dt, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(exportState, trim(name)//'::ode_method', ode_method, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(exportState, trim(name)//'::dt_min', dt_min, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(exportState, trim(name)//'::relative_change_min', relative_change_min, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Write CIM attributes
      convention = 'CIM 1.5'
      purpose = 'ModelComp'
      stringList(1,1)='ShortName';     stringList(1,2)='FABMben'
      stringList(2,1)='LongName';      stringList(2,2)='Framework for Adaptive Biogeochemical Models, benthic'
      stringList(3,1)='Description';   stringList(3,2)='The benthic implementation of FABM'
      stringList(4,1)='ReleaseDate';   stringList(4,2)='unknown'
      stringList(5,1)='ModelType';     stringList(4,2)='ocean'

      call ESMF_AttributeAdd(gridComp, convention=convention, purpose=purpose, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      do i=1,5
        call ESMF_AttributeSet(gridComp, trim(stringList(i,1)), trim(stringList(i,2)), &
          convention=convention, purpose=purpose, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo

      !> Write Responsible party ISO 19115 attributes
      convention = 'ISO 19115'
      purpose    = 'RespParty'
      stringList(1,1)='Name';               stringList(1,2)='Carsten Lemmen'
      stringList(2,1)='Abbreviation';       stringList(2,2)='cl'
      stringList(3,1)='PhysicalAddress';    stringList(3,2)='Helmholtz-Zentrum Geesthacht'
      stringList(4,1)='EmailAddress';       stringList(4,2)='carsten.lemmen@hzg.de'
      stringList(5,1)='ResponsiblePartyRole';   stringList(6,2)='http://www.hzg.de'
      stringList(6,1)='URL';                stringList(5,2)='Contact'

      do i=1,6
        call ESMF_AttributeSet(gridComp, trim(stringList(i,1)), trim(stringList(i,2)), &
          convention=convention, purpose=purpose, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo

      !> Write Citation ISO 19115 attributes
      convention = 'ISO 19115'
      purpose    = 'Citation'
      stringList(1,1)='ShortTitle';     stringList(1,2)='Lemmen et al. (unpublished)'
      stringList(2,1)='LongTitle';      stringList(2,2)='Lemmen et al. (unpublished)'
      stringList(3,1)='Date';           stringList(3,2)='unpublished'
      stringList(4,1)='PresentationForm';   stringList(4,2)='source-code documented'
      stringList(5,1)='DOI';            stringList(5,2)='not assigned'
      stringList(6,1)='URL';            stringList(6,2)='not available'

      do i=1,6
        call ESMF_AttributeSet(gridComp, trim(stringList(i,1)), trim(stringList(i,2)), &
          convention=convention, purpose=purpose, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo

    !! Initialize FABM
    ben = mossco_create_fabm_benthic()

    call ESMF_GridCompGet(gridComp, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! put concentration array and vertical velocity into export state
    ! it might be enough to do this once in initialize(?)
    do n=1,size(ben%export_states)
    end do

    !> this will not work, if state_grid contains halo zones
    do n=1,size(ben%model%diagnostic_variables)
      if (ben%model%diagnostic_variables(n)%output /= output_none) then
        field = ESMF_FieldEmptyCreate( &
          name=only_var_name(ben%model%diagnostic_variables(n)%long_name)//'_at_soil_surface', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AttributeSet(field,'units',trim(ben%model%diagnostic_variables(n)%units))
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' created diagnostic field '
        call MOSSCO_FieldString(field, message, rc=localrc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

        call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      end if
    end do

    !! create forcing fields in import State
    do n=1,size(ben%horizontal_dependencies)
    end do

    !! prepare upward_flux forcing
    do n=1,size(ben%model%state_variables)
    end do

  end subroutine Initialise_Advertise


  !> Initialize phase 1
  !!
  !! Create ESMF fields and export them
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_TimeInterval) :: timeInterval
    character(len=ESMF_MAXSTR) :: string, fileName, varname
    character(len=ESMF_MAXSTR) :: foreignGridFieldName
    character(len=ESMF_MAXSTR) :: attribute_name
    type(ESMF_Config)     :: config
    type(ESMF_FieldBundle) :: fieldBundle
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    type(ESMF_Field)     :: field, concfield, tmpField
    type(ESMF_Field)     :: restartField, areaField
    type(ESMF_Array)     :: array, array_corner
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

    real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2=>null()
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3=>null()
    real(ESMF_KIND_R8),dimension(:,:,:,:),pointer :: ptr_f4=>null()
    real(ESMF_KIND_R8)    :: attribute_r8
    real(ESMF_KIND_R8)    :: background_extinction=0.13
    real(ESMF_KIND_R8)    :: albedo_const=0.78
    integer(ESMF_KIND_I4) :: fieldcount
    integer(ESMF_KIND_I4) :: lbnd2(2),ubnd2(2),lbnd3(3),ubnd3(3)
    integer(ESMF_KIND_I4) :: totallwidth3(3,1), totaluwidth3(3,1)
    integer(ESMF_KIND_I4) :: totallwidth2(2,1), totaluwidth2(2,1)
    integer(ESMF_KIND_I8) :: tidx

    character(len=ESMF_MAXSTR) :: timestring, name, message, units, esmf_name
    integer(ESMF_KIND_I4)      :: localPet, petCount, itemCount
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime, startTime, stopTime
    integer(ESMF_KIND_I8)      :: seconds, advanceCount
    type(ESMF_TimeInterval)    :: timeStep
    logical                    :: clockIsPresent, isPresent
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
    character(len=ESMF_MAXSTR)  :: fabm_nml='fabm_benthic.nml'

    namelist /fabm_benthic/ dt,ode_method,dt_min,relative_change_min, &
                            background_extinction, albedo_const,fabm_nml

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Get the time step
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! get time information
    call ESMF_TimeGet(currTime, dd=day, s=seconds_of_day, &
                      dayOfYear=day_of_year, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! read namelist input for control of timestepping
    inquire(file=trim(name)//'.nml', exist = isPresent)
    if (isPresent) then
      open(33,file=trim(name)//'.nml', action='read', status='old')
    else
      inquire(file='fabm_benthic.nml', exist = isPresent)
      if (.not.isPresent) then
        write(message,'(A)') trim(name)//' could not find required namelist file fabm_benthic.nml'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
      open(33,file='fabm_benthic.nml', action='read', status='old')
    endif
    read(33,nml=fabm_benthic)
    close(33)

    call ESMF_TimeIntervalSet(timeInterval,s_r8=dt,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockSet(clock, timeStep=timeInterval, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! get/set grid:
    !! rely on field with name foreignGridFieldName given as attribute and field
    !! in importState
    !! and just take the same grid&distgrid.
    !! so far, this is hardcoded to 1,1,numlayers
    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
           value=foreignGridFieldName, defaultValue='none',rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (trim(foreignGridFieldName)=='none') then
      call ESMF_ArraySpecSet(state_array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      state_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), &
                   maxIndex=(/inum,jnum,numlayers/), &
                   regDecomp=(/1,1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="benthic states grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridAddCoord(state_grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      totalLWidth3(:,1)=0
      totalUWidth3(:,1)=0
      totalLWidth2(:,1)=0
      totalUWidth2(:,1)=0
    else
      call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field, grid=state_grid, rank=rank, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      allocate(maxIndex(rank))
      call ESMF_GridGet(state_grid,staggerloc=ESMF_STAGGERLOC_CENTER,localDE=0, &
             exclusiveCount=maxIndex,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (rank == 3) call ESMF_FieldGet(field, totalLWidth=totalLWidth3, totalUWidth=totalUWidth3, rc=localrc)
      if (rank == 2) call ESMF_FieldGet(field, totalLWidth=totalLWidth2, totalUWidth=totalUWidth2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      inum=maxIndex(1)
      jnum=maxIndex(2)

      if (rank == 3) then
        !@todo: check numlayers==maxIndex(3)
        totalLWidth2(:,1)=totalLWidth3(1:2,1)
        totalUWidth2(:,1)=totalUWidth3(1:2,1)
      elseif (rank == 2) then
        totalLWidth3(3,1)=1
        totalUWidth3(3,1)=1
        horizontal_grid=state_grid
        state_grid = MOSSCO_GridCreateFromOtherGrid(horizontal_grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        write(message,'(A)') trim(name)//' uses experimental feature with rank 2 foreign grid.  Expect trouble.'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
      else
        write(message,'(A)') 'foreign grid must be of rank 2 or 3'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      end if
      deallocate(maxIndex)
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    deBlockList(:,1,:) = minIndexPDe
    deBlockList(:,2,:) = maxIndexPDe

    distGrid_2D = ESMF_DistGridCreate(minval(deBlockList(1:2,1,:),2), &
                                      maxval(deBlockList(1:2,2,:),2), &
                                      int(deBlockList(1:2,:,:)),      &
                                      delayout=delayout)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    horizontal_grid = ESMF_GridCreate(distGrid_2D,name="benthic horizontal grid", &
                                      gridAlign=(/1,1/),                          &
                                      coordSys=coordSys,                          &
                                      coordDimCount=int(coordDimCount(1:2)),      &
                                      coordDimMap=int(coordDimMap(1:2,1:2)),      &
                                      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do n=1, 2
      if (coordDimCount(n) .eq. 1) then
        call ESMF_GridGetCoord(state_grid, staggerloc=ESMF_STAGGERLOC_CENTER, coorddim=n, farrayptr=coord1d, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        distgridToArrayMap = 0
        distgridToArrayMap(n) = 1

        array = ESMF_ArrayCreate(distGrid_2D,coord1d,indexflag=ESMF_INDEX_DELOCAL,distgridToArrayMap=distgridToArrayMap, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#if 0
        call ESMF_GridGetCoord(state_grid, staggerloc=ESMF_STAGGERLOC_CORNER, coorddim=n, farrayptr=coord1d, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc))  then !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,*) 'corner coordinates not available'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        else
          array_corner = ESMF_ArrayCreate(distGrid_2D,coord1d,indexflag=ESMF_INDEX_DELOCAL,distgridToArrayMap=distgridToArrayMap, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        end if
#endif
      else
        call ESMF_GridGetCoord(state_grid, staggerloc=ESMF_STAGGERLOC_CENTER, coorddim=n, farrayptr=coord2d, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        array = ESMF_ArrayCreate(distGrid_2D,coord2d,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#if 0
        call ESMF_GridGetCoord(state_grid, staggerloc=ESMF_STAGGERLOC_CORNER, coorddim=n, farrayptr=coord2d, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,*) 'corner coordinates not available'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        else
          array_corner = ESMF_ArrayCreate(distGrid_2D,coord2d,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        end if
#endif
      end if
      call ESMF_GridSetCoord(horizontal_grid,n,array=array,staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call ESMF_GridSetCoord(horizontal_grid,n,array=array_corner,staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

    !! Initialize FABM
    inquire(file=trim(fabm_nml), exist=isPresent)
    if (.not.isPresent) then
      write(message,'(A)') trim(name)//' could not find required namelist file '//trim(fabm_nml)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    ben = mossco_create_fabm_benthic(fabm_nml)

    !! re-allocate state variables
    call ESMF_GridGetFieldBounds(state_grid,totalubound=ubnd3,totallbound=lbnd3,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    allocate(ben%conc(1-totalLWidth3(1,1):inum+totalUWidth3(1,1), &
                      1-totalLWidth3(2,1):jnum+totalUWidth3(2,1), &
                      1-totalLWidth3(3,1):numlayers+totalUWidth3(3,1), &
                      1:ben%nvar))
    !! get mask
    allocate(mask(1-totalLWidth3(1,1):inum+totalUWidth3(1,1), &
                  1-totalLWidth3(2,1):jnum+totalUWidth3(2,1), &
                  1-totalLWidth3(3,1):numlayers+totalUWidth3(3,1)))
    mask = .false.
    allocate(ben%is_openboundary(1-totalLWidth3(1,1):inum+totalUWidth3(1,1), &
                                 1-totalLWidth3(2,1):jnum+totalUWidth3(2,1), &
                                 1-totalLWidth3(3,1):numlayers+totalUWidth3(3,1)))
    ben%is_openboundary = .false.
    if (.not.(associated(ben%is_openboundary_hz))) &
      allocate(ben%is_openboundary_hz(1-totalLWidth3(1,1):inum+totalUWidth3(1,1), &
                                      1-totalLWidth3(2,1):jnum+totalUWidth3(2,1)))
    ben%is_openboundary_hz = .false.

    call ESMF_GridGetItem(state_grid, ESMF_GRIDITEM_MASK, farrayPtr=gridmask, rc=localrc)
    if (localrc == ESMF_SUCCESS) then
      mask = ( gridmask.le.0 ) !>@todo: mask where gridmask /= 1
      ben%is_openboundary = ( gridmask > 1 )
      ben%is_openboundary_hz = ben%is_openboundary(:,:,1)
    end if

    !! add cell area to horizontal grid
    call ESMF_GridAddItem(horizontal_grid, itemflag=ESMF_GRIDITEM_AREA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridGetItem(horizontal_grid, itemflag=ESMF_GRIDITEM_AREA, staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=ben%column_area, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! get area from 3d grid
    call ESMF_GridGetItem(state_grid, itemflag=ESMF_GRIDITEM_AREA, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, farrayPtr=ptr_f3, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      ben%column_area = 1.0d0
      write(message,*) 'cannot find vcenter area in grid, set area to 1.0 and assume fluxes_in_water to come as mass per m2 per s'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
    else
      ! use layer 1 cell area.
      ! the indexing fits to GETM layout that starts at i=1-totalLWidth(1)
      ben%column_area = ptr_f3(1:inum,1:jnum,1)
      ! @todo: if no area in state_grid, calculate based on corner coordinates
    end if

    call ben%initialize_domain(inum,jnum,dt,mask=mask(1:inum,1:jnum,1:numlayers))
    !> set Albedo from namelist
    !!@todo: may come 2d from import State
    ben%albedo = albedo_const

    call ben%update_pointers()
    call ben%initialize_concentrations()
    call ben%update_export_states()

    ! done with mask here
    deallocate(mask); nullify(mask)

    ! set solver_settings:
    ben%dt_min=dt_min
    ben%relative_change_min=relative_change_min

    ! put concentration array and vertical velocity into export state
    ! it might be enough to do this once in initialize(?)
    do n=1,size(ben%export_states)
      varname = trim(ben%export_states(n)%standard_name)//'_at_soil_surface'

      concfield = ESMF_FieldCreate(state_grid,farrayPtr=ben%export_states(n)%conc, &
                       name=trim(varname), &
                       totalLWidth=totalLWidth3(:,1),totalUWidth=totalUWidth3(:,1), &
                       staggerloc=ESMF_STAGGERLOC_CENTER,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(concfield,'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !! when memory is allocated, set ben%export_states(n)%conc to the values?

      !> create empty fields for restarts
      restartField = ESMF_FieldEmptyCreate(name=trim(varname),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(restartField,'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(restartField,'units',trim(ben%export_states(n)%units))
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldEmptySet(restartField, state_grid, &
        staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAddReplace(importState,(/restartField/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> continue with statefield
      call ESMF_AttributeSet(concfield,'units',trim(ben%export_states(n)%units))
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> Find all attributes in this state variable and add them to MOSSCO
      itemCount =  ben%model%state_variables_ben(n)%properties%size()

      call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ben%model%state_variables_ben(n)%properties%keys(itemNameList)
      do i=1, itemCount
        !>@todo
        !if (ben%model%state_variables_ben(n)%properties%get_property(itemNameList(i))%typecode()==1)
        !attribute_r8 = ben%model%state_variables_ben(n)%properties%get_real(itemNameList(i), default=-1d30)
        !call ESMF_AttributeSet(concfield,trim(itemNameList(i)), attribute_r8)
        !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo

      call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> add attributes relevant for MOSSCO
      !! mean_particle_diameter and particle density given only,
      !! if property persent
      !! this section can be removed once the more generic one above works
      attribute_name=trim('mean_particle_diameter')
      attribute_r8 = ben%model%state_variables_ben(n)%properties%get_real('diameter',default=-99.d0)
      if (attribute_r8 > 0.0d0) &
        call ESMF_AttributeSet(concfield,attribute_name, attribute_r8)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      attribute_name=trim('particle_density')
      attribute_r8 = ben%model%state_variables_ben(n)%properties%get_real('density',default=-99.d0)
      if (attribute_r8 > 0.0d0) &
        call ESMF_AttributeSet(concfield,attribute_name, attribute_r8)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> set maximum value for zero-gradient boundary condition limiter
      if (trim(varname)=='Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water') then
        call ESMF_AttributeSet(concfield,'hackmax', 0.8d0)
        call ESMF_AttributeSet(concfield,'hackmaxmin', 0.2d0)
        call ESMF_LogWrite('  use maximum boundary value of 0.8 for '//trim(varname),ESMF_LOGMSG_WARNING)
      end if

      if (trim(varname)=='Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water') then
        call ESMF_AttributeSet(concfield,'hackmax', 8.0d0)
        call ESMF_AttributeSet(concfield,'hackmaxmin', 2.0d0)
        call ESMF_LogWrite('  use maximum boundary value of 8.0 for '//trim(varname),ESMF_LOGMSG_WARNING)
      end if

      !> add fabm index in concentration array as "external_index" to be used by other components
      call ESMF_AttributeSet(concfield,'external_index', int(ben%export_states(n)%fabm_id,ESMF_KIND_I8))
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> add to state depending on existing items
      call ESMF_StateGet(exportState, trim(varname), itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemType == ESMF_STATEITEM_NOTFOUND) then
        call ESMF_StateAddReplace(exportState,(/concfield/),rc=localrc)
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
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateAddReplace(exportState,(/fieldBundle/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      else if(itemType == ESMF_STATEITEM_FIELDBUNDLE) then
      !> if fieldBundle, get the bundle and add field
        call ESMF_StateGet(exportState,trim(varname),fieldBundle,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleAdd(fieldBundle,(/concfield/),multiflag=.true.,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      end if
    end do

    !> this will not work, is state_grid contains halo zones
    do n=1,size(ben%model%diagnostic_variables)
      diag => ben%diagnostic_variables(n)
      if (associated(diag)) then
        !call ESMF_StateGet(exportState, &
        !  name=only_var_name(ben%model%diagnostic_variables(n)%long_name)//'_in_water', &
        !  field, rc=localrc)

        !call ESMF_FieldEmptyComplete(field,grid=state_grid,farrayPtr=diag, &
        field = ESMF_FieldCreate(state_grid,farrayPtr=diag, &
                   name=only_var_name(ben%model%diagnostic_variables(n)%long_name)//'_in_water', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(field,'units',trim(ben%model%diagnostic_variables(n)%units))
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' created diagnostic field '
        call MOSSCO_FieldString(field, message, rc=localrc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

        call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      end if
    end do

    !! create forcing fields in import State
    if (associated(ben%bulk_dependencies)) then
      do n=1,size(ben%bulk_dependencies)
        !> check for existing field
        call ESMF_StateGet(importState, trim(ben%bulk_dependencies(n)%name)//'_in_water', itemType,rc=localrc)
        if (itemType == ESMF_STATEITEM_NOTFOUND) then
          field = ESMF_FieldCreate(state_grid, &
                    name=trim(ben%bulk_dependencies(n)%name)//'_in_water', &
                    typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_AttributeSet(field,'units',trim(ben%bulk_dependencies(n)%units))
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_FieldGet(field=field, farrayPtr=ptr_f3, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          ptr_f3 = 0.0_rk

          write(message,'(A)') trim(name)//' created bulk dependency field '
          call MOSSCO_FieldString(field, message, rc=localrc)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

          call ESMF_StateAdd(importState,(/field/),rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        else
          call ESMF_StateGet(importState, trim(ben%bulk_dependencies(n)%name)//'_in_water', field, rc=localrc)
          write(message,'(A)') trim(name)//' uses existing bulk dependency field '
          call MOSSCO_FieldString(field, message, rc=localrc)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
        end if
        attribute_name=trim(ben%bulk_dependencies(n)%name)//'_in_water'
        call set_item_flags(importState,attribute_name,requiredFlag=.true.,requiredRank=3)
        !! set FABM's pointers to dependencies data,
        !! this probably has to be done only once (here) and not in Run
        call ESMF_StateGet(importState, trim(ben%bulk_dependencies(n)%name)//'_in_water', field=field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldGet(field=field, farrayPtr=ptr_f3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ben%set_environment(ben%bulk_dependencies(n)%name,ptr_bulk=ptr_f3)
      end do
    end if

    if (associated(ben%horizontal_dependencies)) then
      do n=1,size(ben%horizontal_dependencies)
        !> check for existing field
        if (trim(ben%horizontal_dependencies(n)%name)=='bottom_depth') then
          esmf_name = 'water_depth_at_soil_surface'
        else
          esmf_name = ben%horizontal_dependencies(n)%name(1:ESMF_MAXSTR)
        end if
        call ESMF_StateGet(importState, trim(esmf_name), itemType,rc=localrc)
        if (itemType == ESMF_STATEITEM_NOTFOUND) then

          field = ESMF_FieldCreate(horizontal_grid, &
               name=trim(esmf_name), &
               typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_AttributeSet(field,'units',trim(ben%horizontal_dependencies(n)%units))
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          write(message,'(A)') trim(name)//' created horizontal dependency field '
          call MOSSCO_FieldString(field, message, rc=localrc)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

          call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        else
          call ESMF_StateGet(importState, trim(esmf_name), field, rc=localrc)
          write(message,'(A)') trim(name)//' uses existing horizontal dependency field '
          call MOSSCO_FieldString(field, message, rc=localrc)
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
        if ((ubound(ptr_f2,1).lt.ben%inum).or. &
            (ubound(ptr_f2,2).lt.ben%jnum).or. &
            (lbound(ptr_f2,1).gt.1).or. &
            (lbound(ptr_f2,2).gt.1)) then
          write(message,*) 'upper bounds of possibly existing 2d array for ', &
                           trim(esmf_name), &
                           ' does not fit into domain: ',size(ptr_f2), &
                           'vs.',ben%inum,ben%jnum
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR,rc=localrc)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
        call ben%set_environment(ben%horizontal_dependencies(n)%name,ptr_horizontal=ptr_f2)
      end do
    end if

    !! prepare upward_flux forcing
    !> todo: this should be moved to fieldBundle to allow multiple fluxes
    do n=1,size(ben%model%state_variables_ben)
      varname = trim(only_var_name(ben%model%state_variables_ben(n)%long_name))//'_upward_flux_at_soil_surface'
      field = ESMF_FieldCreate(horizontal_grid, &
             name=varname, &
             typekind=ESMF_TYPEKIND_R8, &
             staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'external_index', int(ben%export_states(n)%fabm_id,ESMF_KIND_I8))
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !! initialise with zeros
      !call ESMF_FieldGet(field=field, farrayPtr=bfl(n)%p, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !bfl(n)%p = 0.0_rk
      attribute_name=trim(varname)
      call set_item_flags(importState,attribute_name,requiredFlag=.false.,optionalFlag=.true.,requiredRank=2)

      write(message,'(A)') trim(name)//' created field '
      call MOSSCO_FieldString(field, message, rc=localrc)
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
           farrayPtr=ben%zi, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> create river runoff field in import State
#if 1
    field = ESMF_FieldEmptyCreate(name="volume_flux_in_water")
    !> @todo: make field gridset. BUT: since fabm_benthic creates its own horizontal_grid the
    !! check for equal grids requires magic in the link coupler to check for conformal grids.
#else
    field = ESMF_FieldCreate(horizontal_grid, name="volume_flux_in_water", staggerloc=ESMF_STAGGERLOC_CENTER, &
              typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(field,'units','m3.s-1',rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldGet(field, farrayPtr=ben%volume_flux, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    ! initialize volume flux with 0.0 (to be filled in the importState
    ben%volume_flux = 0.0d0
#endif
    call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !call ESMF_StatePrint(importState)
    !call ESMF_StatePrint(exportState)

    !> set global time, such that fabm can calculate initial diagnostics
    call ben%set_time(day_of_year, seconds_of_day)

    !> check consistency of fabm setup
    call ben%check_ready()

    !> also update export states again
    call ben%update_export_states()

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

    type(ESMF_Field), allocatable  :: fieldList(:)
    type(ESMF_Time)            :: currTime
    character(len=ESMF_MAXSTR) :: message, name
    integer(ESMF_KIND_I4)      :: localrc, fieldCount
    type(ESMF_Clock)           :: clock

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> lookup the importState for restart data
    !call ReadRestart(gridComp, importState, exportState, parentClock, rc=localrc)

    !> get volume_flux pointer
    call MOSSCO_StateGetFieldList(importState, fieldList, itemSearch='volume_flux_in_water', &
      fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount == 1) then
      call ESMF_FieldGet(fieldList(1), farrayPtr=ben%volume_flux, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      ben%volume_flux=>null()
    end if

    !> update  after restart
    call ben%update_export_states()

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP2

#undef  ESMF_METHOD
#define ESMF_METHOD "update_import_pointers"
  subroutine update_import_pointers(importState)

    implicit none

    type(ESMF_State)          :: importState

    type(ESMF_StateItem_Flag) :: itemType
    type(ESMF_Field)          :: field
    real(ESMF_KIND_R8), dimension(:,:), pointer   :: ptr_f2=>null()
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: ptr_f3=>null()
    integer                   :: localrc
    character(len=ESMF_MAXSTR):: varname

    ! link bulk dependencies
    if (associated(ben%bulk_dependencies)) then
      do n=1,size(ben%bulk_dependencies)
        !> check for existing field
        call ESMF_StateGet(importState, trim(ben%bulk_dependencies(n)%name)//'_in_water', itemType,rc=localrc)
        if (itemType == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(importState, trim(ben%bulk_dependencies(n)%name)//'_in_water', field=field, rc=localrc)
          call ESMF_FieldGet(field, farrayPtr=ptr_f3, rc=localrc)
          call ben%set_environment(ben%bulk_dependencies(n)%name,ptr_bulk=ptr_f3)
        end if
      end do
    end if

    ! link horizontal dependencies
    if (associated(ben%horizontal_dependencies)) then
      do n=1,size(ben%horizontal_dependencies)
        !> check for existing field
        call ESMF_StateGet(importState, trim(ben%horizontal_dependencies(n)%name), itemType,rc=localrc)
        if (itemType == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(importState, trim(ben%horizontal_dependencies(n)%name), field=field, rc=localrc)
          call ESMF_FieldGet(field, farrayPtr=ptr_f2, rc=localrc)
          call ben%set_environment(ben%horizontal_dependencies(n)%name,ptr_horizontal=ptr_f2)
        end if
      end do
    end if

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
    integer                     :: localrc, n, rank, k

    integer(ESMF_KIND_I4)          :: ubnd(3), lbnd(3), exportUbnd(3), exportLBnd(3)
    real(ESMF_KIND_R8), pointer    :: ptr_f3(:,:,:), exportPtr(:,:,:)
    type(ESMF_FieldStatus_Flag)    :: fieldstatus
    type(ESMF_StateItem_Flag)      :: itemtype
    type(ESMF_Field)               :: field, exportField
    type(ESMF_FieldBundle)         :: fieldBundle
    integer(ESMF_KIND_I8)          :: external_index
    integer(ESMF_KIND_I4)          :: fieldCount
    type(ESMF_Field),dimension(:),allocatable :: fieldList
    logical                        :: foundItem=.false.

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> browse through list of state variables and
    !! copy data from importState fields with same name
    do n=1,size(ben%export_states)

      varname=trim(ben%export_states(n)%standard_name)//'_in_water'
      call ESMF_StateGet(importState, trim(varname), itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
        foundItem=.false.
        call ESMF_StateGet(importState, trim(varname), fieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldBundleGet(fieldBundle, fieldName=trim(varname), &
          fieldCount=fieldCount, rc=localrc)

        if (fieldCount == 0) then
          write(message,'(A)') trim(name)//' empty fieldBundle, skipped hotstart for variable '//trim(varname)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
          cycle
        end if
        call MOSSCO_Reallocate(fieldList, fieldCount, rc=localrc)
        call ESMF_FieldBundleGet(fieldBundle, &
            fieldName=trim(varname), fieldList = fieldList, rc=localrc)

        do k=1,fieldCount
          call ESMF_AttributeGet(fieldList(k), name='external_index', &
                 value=external_index, &
                 defaultValue=int(-1,ESMF_KIND_I8),rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          ! only use field, if external_index matches own index
          if (external_index == int(ben%export_states(n)%fabm_id,ESMF_KIND_I8)) then
            field = fieldList(k)
            foundItem=.true.
            exit
          end if
        end do

        if (foundItem) then
          call RestartConcFromField(n,field)
        else
          write(message,'(A)') trim(name)//' skipped hotstart for variable '//trim(varname)
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
          cycle
        end if

      else if (itemType == ESMF_STATEITEM_FIELD) then

        call ESMF_StateGet(importState, trim(varname), field=field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call RestartConcFromField(n,field)
      end if

    end do

    !> update  after restart
    call ben%update_export_states()

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine ReadRestart

  subroutine RestartConcFromField(n,field)
    implicit none
    integer, intent(in)            :: n
    type(ESMF_Field), intent(in)   :: field
    integer                        :: rc
    integer                        :: rank, localrc
    character(len=ESMF_MAXSTR)     :: message, name, varname
    integer(ESMF_KIND_I4)          :: ubnd(3), lbnd(3), exportUbnd(3)
    real(ESMF_KIND_R8), pointer    :: ptr_f3(:,:,:)
    type(ESMF_FieldStatus_Flag)    :: fieldstatus

    name="fabm_benthic"
    rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, status=fieldstatus, name=varname, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldstatus /= ESMF_FIELDSTATUS_COMPLETE) then

      write(message,'(A)') trim(name)//' skipped hotstart for variable '//trim(varname)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      write(message,'(A)') trim(name)//' incomplete field '
      call mossco_fieldString(field, message)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
      return

    endif

    call ESMF_FieldGet(field, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rank /= 3) then

      write(message,'(A)') trim(name)//' skipped hotstart for variable '//trim(varname)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      write(message,'(A)') trim(name)//' expected rank 3 but got field '
      call mossco_fieldString(field, message)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
      return

    endif

    call ESMF_FieldGet(field, farrayPtr=ptr_f3, exclusiveUbound=ubnd, exclusiveLbound=lbnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Need to get shape from exportState field of same name to constrain the indices of the conc field
    !! so far, use convention of fabm_benthic, that exclusive bounds start at
    !! index 1

    ben%export_states(n)%conc(1:(ubnd(1)-lbnd(1)+1),1:(ubnd(2)-lbnd(2)+1),1:(ubnd(3)-lbnd(3)+1)) &
        = ptr_f3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3))
    write(message,'(A)') trim(name)//' hotstarted field'
    call mossco_fieldString(field, message)
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

  end subroutine RestartConcFromField

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    real(ESMF_KIND_R8),pointer,dimension(:,:) :: ptr_f2=>null()
    real(ESMF_KIND_R8),pointer,dimension(:,:,:) :: ptr_f3=>null()

    integer           :: n
    integer(8)        :: t
    integer           :: seconds_of_day, day_of_year, day

    character(len=ESMF_MAXSTR) :: name, message, prefix
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime, stopTime
    type(ESMF_TimeInterval)    :: timeStep
    integer(ESMF_KIND_I4)      :: localrc

    type(ESMF_Field)                       :: field
    character(len=ESMF_MAXSTR)             :: varname
    type(ESMF_Field)                       :: importField, exportField
    type(ESMF_FieldBundle)                 :: importFieldBundle, exportFieldBundle
    type(ESMF_Field), allocatable          :: exportFieldList(:), importFieldList(:)
    type(ESMF_Field), allocatable          :: fieldList(:)
    type(ESMF_Field), allocatable          :: exportBundleFieldList(:), importBundleFieldList(:)
    character(ESMF_MAXSTR), allocatable    :: itemNameList(:)
    character(ESMF_MAXSTR)                 :: itemName
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_StateItem_Flag)              :: itemType
    type(ESMF_FieldStatus_Flag)            :: fieldStatus
    integer(ESMF_KIND_I4)                  :: exportFieldCount
    integer(ESMF_KIND_I4)                  :: i, j, k, l, nmatch, itemCount, rank, fieldCount
    integer(ESMF_KIND_I4)                  :: ubnd(2), lbnd(2), ubnd3(3), lbnd3(3)
    integer(ESMF_KIND_I8)                  :: advanceCount

    real(ESMF_KIND_R8), pointer            :: farrayPtr3(:,:,:), ratePtr3(:,:,:)
    real(ESMF_KIND_R8), pointer            :: farrayPtr2(:,:), ratePtr2(:,:)
    character(ESMF_MAXSTR), dimension(3), parameter :: &
      suffixList = (/'_flux_at_water_surface','_flux_at_soil_surface ','_flux_at_surface      '/)

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
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

    call ben%set_time(day_of_year, seconds_of_day)

    ! calculate layer_heights (removed from driver)
    !call ben%update_grid()

    ! update pointers from import
    call update_import_pointers(importState)

    ! update internal pointers of fabm_benthic_driver
    call ben%update_pointers()

    ! Create a list of fields  in the export state that have matching fluxes
    ! in the import state
    call ESMF_StateGet(exportState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, itemNameList=itemNameList, itemTypeList=itemTypeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! Build a list of fields in the exportState that have the _in_water suffix and that
    ! have one or more associated fluxes (with allowed suffixex specified in
    ! suffixList) in the importState, preallocate this exportFieldList with
    ! itemCount
    call MOSSCO_Reallocate(exportFieldList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(importFieldList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    nmatch = 0
    do i = 1, itemCount

      !write(message,'(A)') trim(name)//' searches match for item '//trim(itemNameList(i))
      !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      ! Skip everything that is not field or fieldBundle
      if (itemTypeList(i) /= ESMF_STATEITEM_FIELD &
        .and. itemTypeList(i) /= ESMF_STATEITEM_FIELDBUNDLE) cycle

      ! Search for the prefix (i.e. the name without _in_water suffix), if not
      ! found, then skip this item
      j = index(itemNameList(i),'_in_water')
      if (j<2) cycle
      itemName = trim(itemNameList(i))
      prefix = itemName(1:j-1)

      if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then

        do k = 1, size(suffixList)

          call MOSSCO_StateGetFieldList(importState, fieldList, itemSearch=trim(prefix)//trim(suffixList(k)), &
            fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (fieldCount == 0) cycle

          nmatch = nmatch + fieldCount
          if (ubound(exportFieldList, 1) < nmatch) then

            call MOSSCO_Reallocate(exportFieldList, nmatch * 2, keep=.true., rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call MOSSCO_Reallocate(importFieldList, nmatch * 2, keep=.true., rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          endif

          call ESMF_StateGet(exportState, trim(itemName), field, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          exportFieldList(nmatch-fieldCount+1:nmatch) = field
          importFieldList(nmatch-fieldCount+1:nmatch) = fieldList(1:fieldCount)

        enddo

      elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then

        call ESMF_StateGet(exportState, trim(itemName), exportFieldBundle, rc=localrc)
        call ESMF_FieldBundleGet(exportFieldBundle, fieldName=trim(itemName), &
          fieldCount=exportFieldCount, rc=localrc)

        if (exportFieldCount == 0) cycle

        do k = 1, size(suffixList)
          call MOSSCO_StateGetFieldList(importState, fieldList, itemSearch=trim(prefix)//trim(suffixList(k)), &
            fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
          if (fieldCount /= 1 .and. fieldCount /= exportFieldCount ) cycle

          nmatch = nmatch + fieldCount
          if (ubound(exportFieldList,1) < nmatch) then
            call MOSSCO_Reallocate(exportFieldList, nmatch * 2, rc=localrc)
            call MOSSCO_Reallocate(importFieldList, nmatch * 2, rc=localrc)
          endif

          importFieldList(nmatch-fieldCount+1:nmatch) = fieldList
          call MOSSCO_Reallocate(fieldList, exportFieldCount, keep=.false., rc=localrc)
          call ESMF_FieldBundleGet(exportFieldBundle, fieldName=trim(itemName), &
            fieldList = fieldList, rc=localrc)
          exportFieldList(nmatch-fieldCount+1:nmatch) = fieldList

        enddo
      endif
    enddo

    !! Only log successful matching the first time Run() operates
    if (advanceCount<1) then
      do i = 1, nmatch
        write(message,'(A)') trim(name)//' matching'
        call MOSSCO_FieldString(exportFieldList(i), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        write(message,'(A)') '  obtains flux'
        call MOSSCO_FieldString(importFieldList(i), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo
    endif

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
      call ode_solver(ben,dt,ode_method)

      ! integrate bottom upward fluxes
      ! todo: this does not work with the link coupler, yet. the bfl(:)%p pointers
      !       have to be updated from importState here in Run
      if (any((ben%layer_height(RANGE2D,1) <= 0).and.(.not.ben%mask(RANGE2D,1)))) then
        write(message,'(A)') '  non-positive layer height detected'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      !where (ben%layer_height(RANGE2D,1) > 0)
      !  do n=1,ben%nvar
      !    ben%conc(RANGE2D,1,n) = ben%conc(RANGE2D,1,n) + bfl(n)%p(RANGE2D)*dt/ben%layer_height(RANGE2D,1)
      !  end do
      !endwhere

      !> vertically homogeneous boundary conditions
      !>@todo vertically resolved boundary conditions need regridding
      if (associated(ben%is_openboundary_hz)) then
      do n=1,ben%nvar
        varname = trim(ben%export_states(n)%standard_name)
        call ESMF_StateGet(importState, trim(varname)//'_boundary_value_hz', itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (itemType == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(importState, trim(varname)//'_boundary_value_hz', field, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_FieldGet(field, farrayPtr=ratePtr2, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_FieldGetBounds(field, exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          ! overwrite concentrations at the boundary if present
          !>@todo check bounds
          do k=1,ben%knum
            where (ben%is_openboundary_hz(RANGE2D))
              ben%conc(RANGE2D,k,n) = ratePtr2(RANGE2D)
            end where
          end do
        else
          ! no field found
          cycle
          !do k=1,ben%knum
          !where (ben%is_openboundary_hz(RANGE2D))
          !  ben%conc(RANGE2D,k,n) = 1.234
          !end where
          !end do
        end if
      end do
      end if

      call integrate_flux_in_water(ben, importState)

      do i=1, nmatch
        write(message,'(A)') trim(name)//' add flux field '
        call MOSSCO_FieldString(importFieldList(i), message)

        call MOSSCO_MessageAdd(message,' to field ')
        call MOSSCO_FieldString(exportFieldList(i), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        call ESMF_FieldGet(exportFieldList(i), farrayPtr=farrayPtr3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGet(importFieldList(i), rank=rank, name=itemName, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (rank==2) then
          call ESMF_FieldGetBounds(importFieldList(i), exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldGet(importFieldList(i), farrayPtr=ratePtr2, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !> If it is a vertically integrated (2D-) flux (expected unit mmol s**-1)
          !! it is handled by integrate_fluxes_in_water

          !> If is a surface (2D-) flux (expected unit mmol m**-2 d**-1), it needs
          !> to be converted to volume concentration by division with layer_height
          if (index(itemName,'_flux_at_surface')>0 .or. &
                  index(itemName,'_flux_at_water_surface')>0) then
            farrayPtr3(RANGE2D,ben%knum) = farrayPtr3(RANGE2D,ben%knum) + ratePtr2(RANGE2D) * dt / ben%layer_height(RANGE2D,ben%knum)
          elseif (index(itemName,'_flux_at_soil_surface')>0) then
            !> @todo Skip if  .not.ben%mask(RANGE2D,k)
            ! if (all(ratePtr2(RANGE2D) == 0.0 .or. ben%mask(RANGE2D,1)) cycle
            ! Avoid overshoot of negative fluxes within a timestep
            ! where (farrayPtr3(RANGE2D,1) + ratePtr2(RANGE2D) * dt / ben%layer_height(RANGE2D,1) > 0)
              farrayPtr3(RANGE2D,1) = farrayPtr3(RANGE2D,1) + ratePtr2(RANGE2D) * dt / ben%layer_height(RANGE2D,1)
            !endwhere
            write (message,'(A,ES10.3,A)') trim(name)//' added ',maxval(ratePtr2(RANGE2D) * dt / ben%layer_height(RANGE2D,1)),' from '
            call MOSSCO_FieldString(importFieldList(i),message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
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

          farrayPtr3(RANGE3D) = farrayPtr3(RANGE3D)  + ratePtr3(RANGE3D) * dt
        endif
      enddo

      ! clip concentrations that are below minimum
      call ben%clip_below_minimum()

      ! time integration of diagnostic variables
      call ben%integrate_diagnostic_variables(dt)

      ! link fabm state
      call ben%update_pointers()

      call ESMF_ClockAdvance(clock, timeStep=timeStep, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    call MOSSCO_Reallocate(exportFieldList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(importFieldList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> prepare component's export
    call ben%update_export_states()

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

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory

    !if (associated(bfl)) deallocate(bfl)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

  subroutine integrate_flux_in_water(ben, importState)
    type(ESMF_State)               :: importState
    type(type_mossco_fabm_benthic) :: ben
    type(ESMF_Field)               :: field
    type(ESMF_Field),allocatable   :: fieldList(:), tempList(:)
    type(ESMF_FieldBundle)         :: fieldBundle
    type(ESMF_StateItem_FLAG)      :: itemtype
    integer(ESMF_KIND_I4)          :: n,i,j,k,m, localrc, rc, fieldCount
    integer(ESMF_KIND_I8)          :: external_index
    integer(kind=ESMF_KIND_I4)     :: ubnd(2),lbnd(2),ubnd3(3),lbnd3(3), rank
    character(len=ESMF_MAXSTR)     :: message, varname
    real(ESMF_KIND_R8), pointer    :: ratePtr2(:,:), ratePtr3(:,:,:)

    ! calculate total water depth
    if (.not.(associated(ben%cell_per_column_volume))) then
      ubnd3 = ubound(ben%layer_height)
      lbnd3 = lbound(ben%layer_height)
      allocate(ben%cell_per_column_volume(lbnd3(1):ubnd3(1),lbnd3(2):ubnd3(2),lbnd3(3):ubnd3(3)))
      ben%cell_per_column_volume = 0.0d0
    end if

    do k=1,ben%knum
      if (any((ben%layer_height(RANGE2D,k) <= 0).and.(.not.ben%mask(RANGE2D,k)))) then
        write(message,'(A)') '  non-positive layer height detected'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      where (.not.ben%mask(RANGE2D,k))
      ben%cell_per_column_volume(RANGE2D,k) = 1.0d0 / &
        (sum(ben%layer_height(RANGE3D),dim=3)*ben%column_area(RANGE2D))
      endwhere
    enddo

    do n=1,ben%nvar
      varname = trim(ben%export_states(n)%standard_name)

      call MOSSCO_StateGetFieldList(importState, fieldList, &
        itemSearch=trim(varname)//'_flux_at_soil_surface', fieldCount=fieldCount, &
        fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldCount == 0) cycle
      if (fieldCount > 1) then

        call MOSSCO_Reallocate(tempList, fieldCount, rc=localrc)

        ! filter out all fields with non-matching external_index
        m = 0
        do k=1, fieldCount
          call ESMF_AttributeGet(fieldList(k), name='external_index', &
                 value=external_index, defaultValue=int(-1, ESMF_KIND_I8),rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          ! only use field, if external_index matches own index
          if (external_index /= int(ben%export_states(n)%fabm_id,ESMF_KIND_I8) &
            .and. external_index > -1) cycle
          m = m + 1
          tempList(m) = fieldlist(k)
        end do
        fieldCount = m
        if (fieldCount == 0) cycle

        call MOSSCO_Reallocate(fieldList, fieldCount, keep=.false., rc=localrc)
        fieldList(:) = tempList(1:m)
        call MOSSCO_Reallocate(tempList, fieldCount, keep=.false., rc=localrc)

        ! filter out all fields with no external_index if one exists
        m = 0
        do k=1, fieldCount
          call ESMF_AttributeGet(fieldList(k), name='external_index', &
                 value=external_index, defaultValue=int(-1, ESMF_KIND_I8),rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          ! only use field, if external_index matches own index
          if (external_index /= int(ben%export_states(n)%fabm_id,ESMF_KIND_I8)) cycle
          m = m + 1
          tempList(m) = fieldlist(k)
        end do

        if (m > 0) then ! found exactly matching external index
          fieldCount = m
          call MOSSCO_Reallocate(fieldList, fieldCount, keep=.false., rc=localrc)
          fieldList(:) = tempList(1:m)
        endif
        call MOSSCO_Reallocate(tempList, 0, keep=.false., rc=localrc)
      endif

      do i=1, fieldCount
        field = fieldList(i)
        call ESMF_FieldGet(field, rank=rank, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') '  integrating '
        call MOSSCO_FieldString(field, message, rc=localrc)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        !> vertically homogeneous flux in water (e.g. rivers)
        if (rank == 2) then
          call ESMF_FieldGet(field, farrayPtr=ratePtr2, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldGetBounds(field, exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          do k=1,ben%knum
            !> addition of mass
            ben%conc(RANGE2D,k,n) = ben%conc(RANGE2D,k,n) &
                + dt * ratePtr2(RANGE2D) * ben%cell_per_column_volume(RANGE2D,k)
          end do

        elseif (rank == 3) then
          call ESMF_FieldGet(field, farrayPtr=ratePtr3, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !call ESMF_FieldGetBounds(field, exclusiveUBound=ubnd3, exclusiveLBound=lbnd3, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          ben%conc(RANGE3D,n) = ben%conc(RANGE3D,n) &
                + dt * ratePtr3(RANGE3D)
          !* ben%cell_per_column_volume(RANGE3D)
          !write(0,'(A,ES10.3,A,ES10.3)') 'Max 3D integrating ', &
          !  maxval(-dt * ratePtr3(RANGE3D)), &
          ! ' of ', maxval(ben%conc(RANGE3D,n))
        end if
      end do
    enddo

  end subroutine integrate_flux_in_water

end module fabm_benthic_component
