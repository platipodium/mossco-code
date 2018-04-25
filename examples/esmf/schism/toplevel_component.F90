!> @brief Implementation of an ESMF toplevel coupling
!>
!> @copyright 2018 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @license Apache License 2.0

!
#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "toplevel_component.F90"
module toplevel_component

  use esmf
  use mossco_variable_types
  use mossco_state
  use mossco_component

  use link_connector, only : link_connector_SetServices => SetServices
  use regrid_coupler, only : regrid_coupler_SetServices => SetServices
  use schism_esmf_component, only : schism_SetServices => SetServices
  use netcdf_input_component, only : netcdf_input_SetServices => SetServices
  use grid_component, only : grid_SetServices => SetServices
  use netcdf_component, only : netcdf_SetServices => SetServices

  implicit none

  private

  public SetServices

  type(ESMF_GridComp),dimension(:),save, allocatable :: gridCompList
  type(ESMF_CplComp),dimension(:), save, allocatable :: cplCompList
  type(ESMF_State), dimension(:),  save, allocatable :: gridExportStateList, gridImportStateList
  type(ESMF_Alarm), dimension(:),  save, allocatable :: cplAlarmList
  type(ESMF_Clock), dimension(:),  save, allocatable :: gridCompClockList, cplCompClockList
  type(ESMF_Clock), save                             :: controlClock
  character(len=ESMF_MAXSTR), dimension(:), save, allocatable :: gridCompNameList, cplCompNameList, cplNames

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    character(len=ESMF_MAXSTR)  :: myName
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=myName, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
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

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: startTime, currTime
    type(ESMF_Time)         :: ringTime, time
    type(ESMF_TimeInterval) :: alarmInterval

    integer(ESMF_KIND_I4)  :: numGridComp, numCplComp, petCount
    integer(ESMF_KIND_I4)  :: alarmCount, numCplAlarm, i, localrc, localPet
    type(ESMF_Alarm), dimension(:), allocatable :: alarmList !> @todo shoudl this be a pointer?
    character(ESMF_MAXSTR) :: myName, message, childName, alarmName
    type(ESMF_Alarm)       :: childAlarm
    type(ESMF_Clock)       :: childClock
    type(ESMF_Clock)       :: clock !> This component's internal clock
    logical                :: clockIsPresent
    integer(ESMF_KIND_I4), allocatable :: petList(:)
    type(ESMF_VM)          :: vm
    type(ESMF_Log)         :: stateLog

    integer(ESMF_KIND_I4)  :: phase, phaseCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: gridCompPhaseCountList,CplCompPhaseCountList
    logical, allocatable   :: GridCompHasPhaseZeroList(:)
    logical                :: hasPhaseZero
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9

    integer(ESMF_KIND_I4), allocatable      :: intValueList(:)
    character(len=ESMF_MAXSTR), allocatable :: charValueList(:)
    character(len=ESMF_MAXSTR)              :: stringList(6,2)
    type(ESMF_AttPack)     :: attPack
    character(len=ESMF_MAXSTR) :: convention, purpose

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=myName, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Create a control clock that is later used for deteriming timesteps to
    !! individual calls of coupled components
    !! this is a global variable, might change later
    controlClock = ESMF_ClockCreate(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockSet(controlClock, name=trim(myName)//'Control')
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_VmGet(vm, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Add a CIM component attribute package to this component
    convention='CIM 1.5'
    purpose='ModelComp'

    call ESMF_AttributeAdd(gridComp, convention=convention, purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'ShortName', 'toplevel', convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'LongName', 'toplevel', convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'ModelType', 'framework', convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, name='simulation_title', value=message, defaultvalue='Untitled', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'SimulationShortName', message, convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'SimulationLongName', message, convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'SimulationRationale', 'Modular coupled system simulation', convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, name='simulation_start', value=message, defaultvalue='Untitled', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'SimulationStartDate', message, convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, name='simulation_stop', value=message, defaultvalue='Untitled', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'SimulationDuration', message, convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(I4)') petCount
    call ESMF_AttributeSet(gridComp, 'SimulationNumberOfProcessingElements', message, convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    purpose='Platform'
    call ESMF_AttributeGetAttPack(gridComp, convention, purpose, attpack=attpack, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'MachineName', 'unknown', convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Allocate the fields for all gridded components and their names
    numGridComp = 5
    allocate(gridCompList(numGridComp), stat=localrc)
    allocate(gridCompClockList(numGridComp), stat=localrc)
    allocate(gridCompNameList(numGridComp), stat=localrc)
    allocate(gridImportStateList(numGridComp), stat=localrc)
    allocate(gridExportStateList(numGridComp), stat=localrc)

    gridCompNameList(1) = 'wind_output'
    gridCompNameList(2) = 'schism_output'
    gridCompNameList(3) = 'grid_input'
    gridCompNameList(4) = 'wind_input'
    gridCompNameList(5) = 'schism'

    !! Create all gridded components, and create import and export states for these

    allocate(petList(petCount), stat=localrc)
    do i=1,petCount
      petList(i)=i-1
    enddo

    do i = 1, numGridComp
      gridCompClockList(i) = ESMF_ClockCreate(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockSet(gridCompClockList(i), name=trim(gridCompNameList(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    gridCompList(1) = ESMF_GridCompCreate(name=trim(gridCompNameList(1)),  &
      petList=petList, clock=gridCompClockList(1), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    gridCompList(2) = ESMF_GridCompCreate(name=trim(gridCompNameList(2)),  &
      petList=petList, clock=gridCompClockList(2), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    gridCompList(3) = ESMF_GridCompCreate(name=trim(gridCompNameList(3)),  &
      petList=petList, clock=gridCompClockList(3), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    gridCompList(4) = ESMF_GridCompCreate(name=trim(gridCompNameList(4)),  &
      petList=petList, clock=gridCompClockList(4), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    gridCompList(5) = ESMF_GridCompCreate(name=trim(gridCompNameList(5)),  &
      petList=petList, clock=gridCompClockList(5), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, numGridComp
      gridExportStateList(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
        name=trim(gridCompNameList(i))//'Export')
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Export', ESMF_LOGMSG_INFO)
      call ESMF_StateReconcile(gridExportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridImportStateList(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
        name=trim(gridCompNameList(i))//'Import')
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Import', ESMF_LOGMSG_INFO)
      call ESMF_StateReconcile(gridImportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo
    !! Adding meta information to output component wind_output

    !>@todo find out why attributeSet does not work

    do i=1, numGridComp
      if (i<10) then
        write(message,'(A,I1)') 'gridded_component_', i
      else
        write(message,'(A,I2)') 'gridded_component_', i
      endif
        !call ESMF_AttributeSet(importState(1), trim(message), trim(gridCompNameList(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo
    !! Adding meta information to output component schism_output

    !>@todo find out why attributeSet does not work

    do i=1, numGridComp
      if (i<10) then
        write(message,'(A,I1)') 'gridded_component_', i
      else
        write(message,'(A,I2)') 'gridded_component_', i
      endif
        !call ESMF_AttributeSet(importState(2), trim(message), trim(gridCompNameList(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    !! Now register all setServices routines for the gridded components
    call ESMF_GridCompSetServices(gridCompList(1), netcdf_SetServices, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gridCompList(2), netcdf_SetServices, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gridCompList(3), grid_SetServices, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gridCompList(4), netcdf_input_SetServices, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gridCompList(5), schism_SetServices, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Allocate the fields for all coupler components and their names
    numCplComp = 2
    allocate(cplCompList(numCplComp), stat=localrc)
    allocate(cplCompNameList(numCplComp), stat=localrc)
    allocate(cplCompClockList(numCplComp), stat=localrc)
    cplCompNameList(1) = 'link_connector'
    cplCompNameList(2) = 'regrid'


    do i = 1, numCplComp
      cplCompClockList(i) = ESMF_ClockCreate(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockSet(cplCompClockList(i), name=trim(cplCompNameList(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      cplCompList(i) = ESMF_CplCompCreate(name=trim(cplCompNameList(i)), clock=cplCompClockList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    call ESMF_CplCompSetServices(cplCompList(1), link_connector_SetServices, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetServices(cplCompList(2), regrid_coupler_SetServices, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Initialize all components, both cpl and grid components, do this
    !! in the order specified by dependencies/couplings
    !! Also, try to find coupling/dependency specific export/import states in
    !! the initialization

    !! Establish number of phases and zero phase for all components
    !! @> todo this interface will likely change in the future and will
    !! be integrated with GridCompGet

    allocate(GridCompHasPhaseZeroList(numGridComp), stat=localrc)
    allocate(gridCompPhaseCountList(numGridComp), stat=localrc)

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_INITIALIZE, &
        phaseCount=phaseCount, phaseZeroFlag=hasPhaseZero, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridCompPhaseCountList(i)=phaseCount
      gridCompHasPhaseZeroList(i)=hasPhaseZero
    enddo

    !! Go through all phase 0 if components have it
    do i = 1,numGridcomp
      if (.not.GridCompHasPhaseZeroList(i)) cycle
      call ESMF_GridCompInitialize(gridCompList(i), exportState=gridExportStateList(i), phase=0, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!> @todo expect the Attribute InitializePhaseMap in this state, this attribute
      !! contains information on the phases defined in the component.
    enddo

    allocate(CplCompPhaseCountList(numCplComp), stat=localrc)
    cplCompPhaseCountList(:)=1

    !!> The code below is not working in ESMF 6, thus not executed for now
    !do i = 1, numCplComp
    !  call ESMF_CplCompGetEPPhaseCount(cplCompList(i), ESMF_METHOD_INITIALIZE, &
    !    phaseCount=CplCompPhaseCountList(i), phaseZeroFlag=hasPhaseZero, rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !  if (.not.hasPhaseZero) cycle
!     TODO: clock provided during Create() seems to be not recognized?!
      !call ESMF_CplCompInitialize(cplCompList(i), phase=0, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!> @todo expect the Attribute InitializePhaseMap in this state, this attribute
      !! contains information on the phases defined in the component.
    !end do

    !! Declare all dependencies
    call ESMF_AttributeSet(gridImportStateList(4), name="foreign_grid_field_name", value="grid_input", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(charValueList(1), intValueList(1))
    charValueList(1) = 'grid_input'
    intValueList (1) = 3
    call ESMF_AttributeSet(gridImportStateList(4), name="depends_on", valueList=charValueList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridImportStateList(4), name="depends_on_id", valueList=intValueList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    deallocate(charValueList)
    deallocate(intValueList)

    !! Go through all phases:
    !! IPDv00p1 = phase 1: Advertise Fields in import and export States. These can be
    !!   empty fields that are later completed with FieldEmptyComplete
    !! IPDv00p2 = phase 2: Realize Fields (that have not been completed in phase 1)

    do phase = 1,2

      !! Initializing wind_output
      if (gridCompPhaseCountList( 1)>= phase) then
        call ESMF_GridCompInitialize(gridCompList(1), importState=gridImportStateList(1), &
          exportState=gridExportStateList(1), clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridImportStateList(1), &
        !   clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridExportStateList(1), &
        !   clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif

      !! Initializing schism_output
      if (gridCompPhaseCountList( 2)>= phase) then
        call ESMF_GridCompInitialize(gridCompList(2), importState=gridImportStateList(2), &
          exportState=gridExportStateList(2), clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridImportStateList(2), &
        !   clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridExportStateList(2), &
        !   clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif

      !! Initializing grid_input
      if (gridCompPhaseCountList( 3)>= phase) then
        call ESMF_GridCompInitialize(gridCompList(3), importState=gridImportStateList(3), &
          exportState=gridExportStateList(3), clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridImportStateList(3), &
        !   clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridExportStateList(3), &
        !   clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif

      !! Initializing wind_input
      !! linking grid_inputExport to wind_inputImport
      write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(3))//"Export to "//trim(gridCompNameList(4))//"Import"
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(3), &
        exportState=gridImportStateList(4), clock=clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_LogFlush()
      if (gridCompPhaseCountList( 4)>= phase) then
        call ESMF_GridCompInitialize(gridCompList(4), importState=gridImportStateList(4), &
          exportState=gridExportStateList(4), clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridImportStateList(4), &
        !   clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridExportStateList(4), &
        !   clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif

      !! Initializing schism
      if (gridCompPhaseCountList( 5)>= phase) then
        call ESMF_GridCompInitialize(gridCompList(5), importState=gridImportStateList(5), &
          exportState=gridExportStateList(5), clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridImportStateList(5), &
        !   clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridExportStateList(5), &
        !   clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif

      !! Linking
      !! linking wind_output and wind_input
      if (gridCompPhaseCountList( 1)>= phase .or. gridCompPhaseCountList( 4)>= phase) then
        !! linking wind_inputExport to wind_outputImport
        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(4))//"Export to "//trim(gridCompNameList(1))//"Import"
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(4), &
          exportState=gridImportStateList(1), clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !! linking wind_outputImport to wind_inputExport
        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(1))//"Import to "//trim(gridCompNameList(4))//"Export"
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        call ESMF_CplCompInitialize(cplCompList(1), importState=gridImportStateList(1), &
          exportState=gridExportStateList(4), clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif

      !! linking schism_output and schism
      if (gridCompPhaseCountList( 2)>= phase .or. gridCompPhaseCountList( 5)>= phase) then
        !! linking schismExport to schism_outputImport
        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(5))//"Export to "//trim(gridCompNameList(2))//"Import"
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(5), &
          exportState=gridImportStateList(2), clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !! linking schism_outputImport to schismExport
        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(2))//"Import to "//trim(gridCompNameList(5))//"Export"
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        call ESMF_CplCompInitialize(cplCompList(1), importState=gridImportStateList(2), &
          exportState=gridExportStateList(5), clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif

      !! linking grid_input and wind_input
      if (gridCompPhaseCountList( 3)>= phase .or. gridCompPhaseCountList( 4)>= phase) then
        !! linking grid_inputExport to wind_inputImport
        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(3))//"Export to "//trim(gridCompNameList(4))//"Import"
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(3), &
          exportState=gridImportStateList(4), clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !! linking wind_inputImport to grid_inputExport
        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(4))//"Import to "//trim(gridCompNameList(3))//"Export"
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        call ESMF_CplCompInitialize(cplCompList(1), importState=gridImportStateList(4), &
          exportState=gridExportStateList(3), clock=clock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif

      !! linking wind_input and schism
      if (gridCompPhaseCountList( 4)>= phase .or. gridCompPhaseCountList( 5)>= phase) then
      endif

      !! calling init of regrid
      !! connecting wind_inputExport to schismImport
      if (gridCompPhaseCountList( 4)>= phase .or. gridCompPhaseCountList( 5)>= phase) then
      if (cplCompPhaseCountList( 2)>= phase) then
        write(message,"(A,I1,A)") trim(myName)//" "//trim(gridCompNameList(4))//"Export=>"//trim(cplCompNameList(2))//"(initP",phase,")=>"//trim(gridCompNameList(5))//"Import"
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        !call MOSSCO_StateLog(gridExportStateList(4), rc=localrc)
        call ESMF_CplCompInitialize(cplCompList(2), importState=gridExportStateList(4), &
          exportState=gridImportStateList(5), clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !call MOSSCO_Log(gridImportStateList(5), rc=localrc)
      endif
      endif
    enddo  ! of loop over Initialize phases

    !> Link attributes of exportState of the topLevel component (which contains metadata)
    !> to the netcdf component's import state
    call ESMF_AttributeLink(importState, gridImportStateList(1), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeLink(gridComp, gridImportStateList(1), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompInitialize(cplCompList(1), importState=importState, &
      exportState=gridImportStateList(1), clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_StateLog(exportState, rc=localrc)
    call MOSSCO_StateLog(gridImportStateList(1), rc=localrc)
    call MOSSCO_CompLog(gridComp, rc=localrc)
    !> Link attributes of exportState of the topLevel component (which contains metadata)
    !> to the netcdf component's import state
    call ESMF_AttributeLink(importState, gridImportStateList(2), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeLink(gridComp, gridImportStateList(2), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_CplCompInitialize(cplCompList(1), importState=importState, &
      exportState=gridImportStateList(2), clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_StateLog(exportState, rc=localrc)
    call MOSSCO_StateLog(gridImportStateList(2), rc=localrc)
    call MOSSCO_CompLog(gridComp, rc=localrc)

    !> Go through all components and log their import and export states

    call ESMF_GridCompGet(gridComp, localPet=localPet, rc=localrc)

    if (localPet==0) then
      call ESMF_AttributeGet(importState, name='simulation_title', value=message, defaultvalue='Untitled', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_LogOpen(stateLog,'states_'//trim(message), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_LogWrite('====== Status at end of child initialization ======', ESMF_LOGMSG_INFO, log=stateLog)

      do i=1,numGridComp
        call ESMF_LogWrite('====== '//trim(gridCompNameList(i))//' ======', ESMF_LOGMSG_INFO, log=stateLog)

        call MOSSCO_CompLog(gridCompList(i), log=stateLog, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_StateLog(gridImportStateList(i), log=stateLog, deep=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_StateLog(gridExportStateList(i), log=stateLog, deep=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo

      do i=1,numCplComp
        call ESMF_LogWrite('====== '//trim(cplCompNameList(i))//' ======', ESMF_LOGMSG_INFO, log=stateLog)

        call MOSSCO_CompLog(cplCompList(i), log=stateLog, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo

      call ESMF_LogClose(stateLog)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
    !! ReadRestarting wind_output with data from wind_input
    call ESMF_GridCompReadRestart(gridCompList(1), importState=gridExportStateList(4), &
      exportState=gridExportStateList(1), clock=clock, phase=1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! ReadRestarting schism with data from wind_input
    call ESMF_GridCompReadRestart(gridCompList(5), importState=gridExportStateList(4), &
      exportState=gridExportStateList(5), clock=clock, phase=1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! End of ReadRestart


    do i=1, numGridComp
      call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Import', ESMF_LOGMSG_INFO)
      call ESMF_StateReconcile(state=gridImportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Export', ESMF_LOGMSG_INFO)
      call ESMF_StateReconcile(state=gridExportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    !!> Check all states for remaining incomplete fields
    !!>@todo find segfault this is causing
    !call ESMF_LogWrite(trim(myName)//' listing all import and export states', ESMF_LOGMSG_INFO)

    do i=1, numGridComp
      !call MOSSCO_StateCheckFields(gridImportStateList(i), rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call MOSSCO_StateCheckFields(gridExportStateList(i), rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call MOSSCO_StateLog(gridImportStateList(i))
      !call MOSSCO_StateLog(gridExportStateList(i))
   enddo

    !> Go through all components and log their import and export states
    call ESMF_LogWrite('====== Status at end of child readrestarting ======', ESMF_LOGMSG_INFO, log=stateLog)

    !do i=1,numGridComp
    !  call ESMF_LogWrite('====== States of '//trim(gridCompNameList(i))//' ======', ESMF_LOGMSG_INFO, log=stateLog)
    !  call MOSSCO_StateLog(gridImportStateList(i))
    !  call MOSSCO_StateLog(gridExportStateList(i))
    !enddo
    numCplAlarm = 4
    if (allocated(cplAlarmList)) deallocate(cplAlarmList)
    if (allocated(cplNames)) deallocate(cplNames)
    allocate(cplAlarmList(numCplAlarm))
    allocate(cplNames(numCplAlarm))

    !! The default coupler for all cplAlarms is the 'link' connector
    cplNames(:) = 'link'

    !! For other explicitly given couplings, specify connectors
    cplNames(2)='regrid'

    !! Set the coupling alarm starting from start time of local clock
    call ESMF_ClockGet(clock,startTime=startTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalSet(alarmInterval, startTime, yy=99999 ,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    cplAlarmList(1)=ESMF_AlarmCreate(clock=clock,ringTime=startTime+alarmInterval, &
      ringInterval=alarmInterval, name='grid_input--wind_input--cplAlarm', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Copy this alarm to all children as well
    do i=1,numGridComp
      call ESMF_GridCompGet(gridCompList(i),name=childName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (trim(childName)=='grid_input' .or. trim(childName)=='wind_input') then
        call ESMF_GridCompGet(gridCompList(i), clockIsPresent=clockIsPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (clockIsPresent) then
          call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        else
          call ESMF_LOGWRITE(trim(myName)//' creates clock for '//trim(childName)//', this should have been done by the component.', &
            ESMF_LOGMSG_WARNING)

          childClock=ESMF_ClockCreate(clock=clock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_GridCompSet(gridCompList(i),clock=childClock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        childAlarm=ESMF_AlarmCreate(cplAlarmList(1), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AlarmSet(childAlarm, clock=childClock)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif
    enddo
        call ESMF_TimeIntervalSet(alarmInterval, startTime, m=30 ,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    cplAlarmList(2)=ESMF_AlarmCreate(clock=clock,ringTime=startTime+alarmInterval, &
      ringInterval=alarmInterval, name='wind_input--schism--cplAlarm', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Copy this alarm to all children as well
    do i=1,numGridComp
      call ESMF_GridCompGet(gridCompList(i),name=childName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (trim(childName)=='wind_input' .or. trim(childName)=='schism') then
        call ESMF_GridCompGet(gridCompList(i), clockIsPresent=clockIsPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (clockIsPresent) then
          call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        else
          call ESMF_LOGWRITE(trim(myName)//' creates clock for '//trim(childName)//', this should have been done by the component.', &
            ESMF_LOGMSG_WARNING)

          childClock=ESMF_ClockCreate(clock=clock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_GridCompSet(gridCompList(i),clock=childClock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        childAlarm=ESMF_AlarmCreate(cplAlarmList(2), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AlarmSet(childAlarm, clock=childClock)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif
    enddo
        call ESMF_TimeIntervalSet(alarmInterval, startTime, m=30 ,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    cplAlarmList(3)=ESMF_AlarmCreate(clock=clock,ringTime=startTime+alarmInterval, &
      ringInterval=alarmInterval, name='wind_input--wind_output--cplAlarm', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Copy this alarm to all children as well
    do i=1,numGridComp
      call ESMF_GridCompGet(gridCompList(i),name=childName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (trim(childName)=='wind_input' .or. trim(childName)=='wind_output') then
        call ESMF_GridCompGet(gridCompList(i), clockIsPresent=clockIsPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (clockIsPresent) then
          call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        else
          call ESMF_LOGWRITE(trim(myName)//' creates clock for '//trim(childName)//', this should have been done by the component.', &
            ESMF_LOGMSG_WARNING)

          childClock=ESMF_ClockCreate(clock=clock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_GridCompSet(gridCompList(i),clock=childClock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        childAlarm=ESMF_AlarmCreate(cplAlarmList(3), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AlarmSet(childAlarm, clock=childClock)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif
    enddo
        call ESMF_TimeIntervalSet(alarmInterval, startTime, m=30 ,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    cplAlarmList(4)=ESMF_AlarmCreate(clock=clock,ringTime=startTime+alarmInterval, &
      ringInterval=alarmInterval, name='schism--schism_output--cplAlarm', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Copy this alarm to all children as well
    do i=1,numGridComp
      call ESMF_GridCompGet(gridCompList(i),name=childName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (trim(childName)=='schism' .or. trim(childName)=='schism_output') then
        call ESMF_GridCompGet(gridCompList(i), clockIsPresent=clockIsPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (clockIsPresent) then
          call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        else
          call ESMF_LOGWRITE(trim(myName)//' creates clock for '//trim(childName)//', this should have been done by the component.', &
            ESMF_LOGMSG_WARNING)

          childClock=ESMF_ClockCreate(clock=clock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_GridCompSet(gridCompList(i),clock=childClock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        childAlarm=ESMF_AlarmCreate(cplAlarmList(4), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AlarmSet(childAlarm, clock=childClock)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif
    enddo

    !! Set the default ringTime to the stopTime of local clock, then get all Alarms
    !! from local clock into alarmList, find those that contain the string "cplAlarm"
    !! and look for the earliest ringtime in all coupling alarms.  Save that in the
    !! ringTime
    call ESMF_ClockGet(clock, stopTime=ringTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_ALL, alarmCount=alarmCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(alarmList)) deallocate(alarmList)
    allocate(alarmList(alarmCount))

    call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmList=alarmList,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,ubound(alarmList,1)
      call ESMF_AlarmGet(alarmList(i), ringTime=time, name=alarmName, rc=localrc)

      call ESMF_TimeGet(time,timeStringISOFrac=timestring)
      !write(message,'(A)') trim(myName)//' alarm '//trim(alarmName)//' rings at '//trim(timestring)
      !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      if (index(trim(alarmName),'cplAlarm') < 1) cycle
      if (time<ringTime) ringTime=time
    enddo
    if (allocated(alarmList)) deallocate(alarmList)

    !! Set the timestep such that it corresponds to the time until the
    !! first ringing alarm, log that time
    call ESMF_ClockGet(clock,currTime=currTime,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockSet(clock,timeStep=ringTime-currTime,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, name=childName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(myName)//' '//trim(childName)//' alarms ring next at '//trim(timestring)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    stringList(1,1)='Name';               stringList(1,2)='Carsten Lemmen'
    stringList(2,1)='Abbreviation';       stringList(2,2)='cl'
    stringList(3,1)='PhysicalAddress';    stringList(3,2)='Helmholtz-Zentrum Geesthacht'
    stringList(4,1)='EmailAddress';       stringList(4,2)='carsten.lemmen@hzg.de'
    stringList(5,1)='ResponsiblePartyRole';   stringList(5,2)='Contact'
    stringList(6,1)='URL';   stringList(6,2)='http://www.hzg.de'

    !> @todo te following code throws attribute warnings in ESMF7, this needs
    !> to be investigated and is disabled for now.

#if ESMF_VERSION_MAJOR > 7
    !> Write Responsible party ISO 19115 attributes
    convention = 'ISO 19115'
    purpose    = 'RespParty'

    do i=1,6
      call ESMF_AttributeSet(gridComp, trim(stringList(i,1)), trim(stringList(i,2)), &
        convention=convention, purpose=purpose, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    !> Write Citation ISO 19115 attributes
    convention = 'ISO 19115'
    purpose    = 'Citation'
    stringList(1,1)='ShortTitle';     stringList(1,2)='Lemmen et al. (2013)'
    stringList(2,1)='LongTitle';      stringList(2,2)='  '
    stringList(3,1)='Date';           stringList(3,2)='2013'
    stringList(4,1)='PresentationForm';   stringList(4,2)='Workshop report'
    stringList(5,1)='DOI';            stringList(5,2)='not assigned'
    stringList(6,1)='URL';            stringList(6,2)='http://www.kfki.de/files/kfki-aktuell/0/13-2-DE.pdf'

    !do i=1,6
    !  call ESMF_AttributeSet(gridComp, trim(stringList(i,1)), trim(stringList(i,2)), &
    !    convention=convention, purpose=purpose, rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !enddo

    call ESMF_AttributeWrite(gridComp, convention='CIM 1.5', purpose='ModelComp', &
      attwriteflag=ESMF_ATTWRITE_XML, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
      if (rc .ne. ESMF_RC_LIB_NOT_PRESENT) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    do i=1,numGridComp
      call ESMF_AttributeWrite(gridCompList(i), 'CIM 1.5', 'ModelComp', &
        attwriteflag=ESMF_ATTWRITE_XML, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
        if (rc .ne. ESMF_RC_LIB_NOT_PRESENT) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo
#endif

    !! Populate toplevel state with metadata on simulation
    call MOSSCO_StatePopulateAttributes(exportState, parentClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)



    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Flush the log at the end of Initialize()
    call ESMF_LogFlush(rc=localrc)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: timestring, cplName, myName, childName
    type(ESMF_Time)            :: stopTime, currTime, ringTime, time
    type(ESMF_TimeInterval)    :: timeInterval, ringInterval
    integer(ESMF_KIND_I8)      :: i, j, k, l, advanceCount
    integer(ESMF_KIND_I4)      :: alarmCount
    integer(ESMF_KIND_I4)      :: numGridComp, numCplComp
    integer(ESMF_KIND_I4)      :: hours, minutes, seconds, localPet
    type(ESMF_Log)             :: stateLog

    type(ESMF_Alarm), dimension(:), allocatable :: alarmList
    type(ESMF_Clock)        :: childClock, myClock
    logical                 :: clockIsPresent
    type(ESMF_State)        :: impState, expState
    integer(ESMF_KIND_I4)   :: localrc

    character(len=ESMF_MAXSTR) :: message, compName, alarmName, name1, name2

    integer(ESMF_KIND_I4)  :: phase, phaseCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: gridCompPhaseCountList,CplCompPhaseCountList
    logical, allocatable   :: GridCompHasPhaseZeroList(:)
    logical                :: hasPhaseZero
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=myName, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=myClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    numGridComp=ubound(gridCompList,1)-lbound(gridCompList,1)+1
    numCplComp =ubound(cplCompList ,1)-lbound(cplCompList ,1)+1

    !! Establish number of phases and zero phase for all components
    !! @> todo this interface will likely change in the future and will
    !! be integrated with GridCompGet

    allocate(GridCompHasPhaseZeroList(numGridComp))
    allocate(gridCompPhaseCountList(numGridComp))

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_RUN, &
        phaseCount=phaseCount, phaseZeroFlag=hasPhaseZero, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridCompPhaseCountList(i)=phaseCount
      GridCompHasPhaseZeroList(i)=hasPhaseZero
    enddo

    allocate(CplCompPhaseCountList(numCplComp))
    !!> @todo reenable if ESMF new enough
    CplCompPHaseCountList(:)=1

    !do i = 1, numCplComp
    !  call ESMF_CplCompGetEPPhaseCount(cplCompList(i), ESMF_METHOD_RUN, &
    !    phaseCount=CplCompPhaseCountList(i), phaseZeroFlag=hasPhaseZero, rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !  if (.not.hasPhaseZero) cycle
    !enddo

    call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
      alarmCount=alarmCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Run until the clock's stoptime is reached
    do

      call ESMF_ClockGet(myClock,currTime=currTime, stopTime=stopTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (currTime>stopTime) then
        call ESMF_LogWrite(trim(myName)//' clock out of scope', ESMF_LOGMSG_ERROR)
        call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
      endif

      !! Loop through all components and check whether their clock is currently at the
      !! same time as my own clock's currTime, if yes, then run the respective couplers
      do i=1,numGridComp
        !! Determine for each child the clock
        call ESMF_GridCompGet(gridCompList(i),name=compName, clockIsPresent=clockIsPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (.not.clockIsPresent) then
          call ESMF_LogWrite(trim(myName)//' required clock not found in '//trim(compName), ESMF_LOGMSG_ERROR)
          call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
        endif

        call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_ClockGet(childClock,currTime=time, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc))  &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        ! write(message,'(A)') trim(myName)//' '//trim(compName)//' now at '//trim(timestring)
        !  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE)

        if (time>currTime) cycle

        !! Find all the alarms in this child and call all the couplers that
        !! have ringing alarms at this stage

        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmCount=alarmCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (alarmCount==0) then
          timeInterval=stopTime-currTime
          cycle
        endif

        if (allocated(alarmList)) deallocate(alarmList)
        allocate(alarmList(alarmCount))

        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
           alarmList=alarmList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do j=1,alarmCount
          call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=ringTime, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !! Skip this alarm if it is not a cplAlarm
          if (index(trim(alarmName),'cplAlarm') < 1) cycle

          !! Skip this alarm if it is inbound of this component
          if (trim(alarmName(1:index(alarmName,'--')-1))/=trim(compName)) cycle

          !! Skip this alarm if it is not ringing now
          !if (ringTime > currTime) cycle

          call ESMF_TimeGet(ringTime,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !write(message,'(A)') trim(myName)//' '//trim(compName)//' '//trim(alarmName)//' rings at '//trim(timeString)
          !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

          name1=trim(alarmName(1:index(alarmName,'--')-1))
          name2=trim(alarmName(index(alarmName,'--')+2:index(alarmName,'--cplAlarm')-1))

          do k=1,ubound(cplAlarmList,1)
            if (cplAlarmList(k) == alarmList(j)) then
              cplName = trim(cplNames(k))
              exit
            endif
          enddo

          ! Catch a possible memory leak problem when k overflows
          if (k > ubound(cplAlarmList,1)) then
            write(message,'(A)') 'You have some memory corruption, good luck searching ...'
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif

          write(message,'(A)') trim(myName)//' '//trim(timeString)//' '//trim(name1)//' ->'
          if (trim(cplName) /= 'link') then
            write(message,'(A)') trim(message)//' '//trim(cplName)//' ->'
          else
            write(message,'(A)') trim(message)//' ('//trim(cplName)//') ->'
          endif
          write(message,'(A)') trim(message)//' '//trim(name2)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

          !call ESMF_GridCompGet(gridCompList(i), exportState=impState, rc=localrc)
          impState=gridExportStateList(i)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !! Search the gridCompList for other's name
          do k=1, ubound(gridCompList,1)
              call ESMF_GridCompGet(gridCompList(k), name=childName, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
              if (trim(childName)==trim(name2)) exit
          enddo

          if (trim(childName) /= trim(name2)) then
            write(message,'(A)') trim(myName)//' did not find component '//trim(name2)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
          endif

          !! Search the cplCompList for cplName
          do l=1, ubound(cplCompNameList,1)
              !write(0,*) l,trim(cplCompNameList(l))//' ?= '//trim(cplName)//'_connector'
              if (trim(cplCompNameList(l))==trim(cplName)//'_connector') exit
              if (trim(cplCompNameList(l))==trim(cplName)//'_mediator') exit
              if (trim(cplCompNameList(l))==trim(cplName)) exit
          enddo

          !! Exit if the name of the connector/mediator was not found
          if (l > ubound(cplCompNameList,1)) then
            write(message,'(A)') trim(myName)//' could not match '//trim(cplName)//' and '//trim(cplCompNameList(l))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
          endif

          !call ESMF_GridCompGet(gridCompList(k), importState=expState, rc=localrc)
          expState=gridImportStateList(k)
          !> @todo the following is a hack and makes the nudge_connector special by
          !> receiving two export States (the latter to manipulate)
          if (trim(cplCompNameList(l)) == 'nudge_connector') expState=gridExportStateList(k)

          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A)') trim(myName)//' '//trim(timeString)//' calling '//trim(cplCompNameList(l))

          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

          call ESMF_CplCompRun(cplCompList(l), importState=impState, &
            exportState=expState, clock=controlClock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_LogFlush()

        enddo
      enddo

#if 0
      !! Obtain all currently ringing Alarms, and run the components associated with these alarms
      !! When no alarms are ringing anymore, then obtain the minimum of the nextRinging alarms and advance
      !! myself with that timeStep

      call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_RINGING, &
        alarmCount=alarmCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (alarmCount>0) then
        if (allocated(alarmList)) deallocate(alarmList)
        allocate(alarmList(alarmCount))
        call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_RINGING, &
          alarmList=alarmList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_TimeGet(time,timeStringISOFrac=timeString)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        write(message,'(A,I2,A)') trim(myName)//'  '//trim(timeString)//' has',alarmCount,' ringing alarms'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      endif

      do j=1,alarmCount
        call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=time, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (index(trim(alarmName),'cplAlarm')<1) cycle

        call ESMF_TimeGet(time,timeStringISOFrac=timeString)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        write(message,'(A)') trim(myName)//'  '//trim(alarmName)//' is ringing now at '//trim(timestring)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      enddo

      call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_NEXTRINGING, &
        alarmCount=alarmCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (alarmCount>0) then
        if (allocated(alarmList)) deallocate(alarmList)
        allocate(alarmList(alarmCount))
      endif
#endif

      !! Loop through all components and check whether their clock is currently at the
      !! same time as my own clock's currTime, if yes, then run the component and advance it's time
      !! until the next coupling Alarm of this component
      do i=1,numGridComp
        !! Determine for each child the clock
        call ESMF_GridCompGet(gridCompList(i),name=compName, clockIsPresent=clockIsPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (.not.clockIsPresent) then
          call ESMF_LogWrite(trim(myName)//' required clock not found in '//trim(compName), ESMF_LOGMSG_ERROR)
          call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
        endif

        call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_ClockGet(childClock,currTime=time, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_TimeGet(time,timeStringISOFrac=timeString)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (time>currTime) then
          call ESMF_TimeGet(time,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !write(message,'(A)') trim(myName)//' '//trim(compName)//' now at '//trim(timestring)//', but'
          !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)

          call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !write(message,'(A)') trim(myName)//' now at '//trim(timestring)//', cycling ...'
          !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)

          cycle
        endif

        !! Find the child's alarm list, get the interval to the next ringing alarm
        !! and run the component for the interval until that alarm

        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmCount=alarmCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (alarmCount==0) then
          !! This case seems problematic and causing the non-monotonic time warning in netcdf
          call ESMF_LogWrite('No alarm found in '//trim(compName), ESMF_LOGMSG_WARNING)
          timeInterval=stopTime-currTime
        else
          if (allocated(alarmList)) deallocate(alarmList)
          allocate(alarmList(alarmCount))
          call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
             alarmList=alarmList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

        !! Set the default ringTime to the stopTime of local clock, then get all Alarms
        !! from local clock into alarmList, find those that contain the string "cplAlarm"
        !! and look for the earliest ringtime in all coupling alarms.  Save that in the
        !! ringTime
        call ESMF_ClockGet(myClock, stopTime=ringTime, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do j=1,alarmCount
          call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=time, &
            ringInterval=ringInterval, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (index(trim(alarmName),'cplAlarm')<1) cycle

          call ESMF_TimeGet(time,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !write(message,'(A)') trim(myName)//' '//trim(compName)//' '//trim(alarmName)//' rings at '//trim(timestring)
          !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

          !! This might be problematic for components that need to run multiple times from multiple alarms
          !! For a process model the currTime+ringInterval should be taken if it advances it's own clock
          !! For a non-process model (i.e. output), the clock should only be advanced if all of it's current
          !! alarms are switched off ...
          if (time==currTime) ringTime=currTime+ringInterval
          if (time<ringTime) ringTime=time
        enddo

        !call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=localrc)
        !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        !write(message,'(A)') trim(myName)//' setting child''s stopTime to'//trim(timeString)
        !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc);

!       TODO: do not modify childClock
!             (components need to inquire stopTime not from their own clock!)
        call ESMF_ClockSet(childClock, stopTime=ringTime, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_ClockGet(childClock, timeStep=timeInterval, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (timeInterval>ringTime-currTime) then
          !call ESMF_ClockSet(childClock, timeStep=ringTime-currTime, rc=localrc)
          !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          !call ESMF_LogWrite(trim(myName)//"  must be implemented in "//trim(compName),ESMF_LOGMSG_WARNING)
        endif

        !! Change the controlClock with updated currTime and timeStep (if not zero)
        if (ringTime < currTime) then
          write(message,'(A)') trim(myName)//' should not run components with negative timestep'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING, rc=localrc)
          !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        elseif (ringTime > currTime) then
          timeInterval=ringTime-currTime

          call ESMF_ClockSet(controlClock, currTime=currTime, timeStep=timeInterval, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_TimeIntervalGet(timeInterval, h=hours, m=minutes, s=seconds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          write(message,'(A,A,I5.5,A,I2.2,A,I2.2,A)') trim(myName)//' '//trim(timeString)//' calling '//trim(compName), &
            ' to run for ', hours, ':', minutes, ':', seconds, ' hours'
        else
          write(message,'(A,A,I5.5,A,I2.2,A,I2.2,A)') trim(myName)//' '//trim(timeString)//' calling '//trim(compName), &
            ' to run without stepping forward'
        endif
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc)
        call ESMF_ClockSet(controlClock, currTime=currTime,  rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


        !! Loop over all run phases, disregarding any action that could be taken between
        !! phases
        do phase=1,gridCompPhaseCountList(i)
          !call MOSSCO_GridCompFieldsTable(gridCompList(i), importState=gridImportStateList(i), exportState=gridExportStateList(i),rc=localrc)
          call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Import', ESMF_LOGMSG_INFO)
          call ESMF_StateReconcile(gridImportStateList(i), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Export', ESMF_LOGMSG_INFO)
          call ESMF_StateReconcile(gridExportStateList(i), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_GridCompRun(gridCompList(i),importState=gridImportStateList(i),&
            exportState=gridExportStateList(i), clock=controlClock, phase=phase, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          !call MOSSCO_GridCompFieldsTable(gridCompList(i), importState=gridImportStateList(i), exportState=gridExportStateList(i),rc=localrc)
          !call ESMF_LogFlush()
        enddo

        call ESMF_ClockGet(childClock, currTime=time, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (time == currTime) then
          !! This child component did not advance its clock in its Run() routine
          !! We do that here
          call ESMF_LogWrite(trim(myName)//' '//trim(compName)//' did not advance its clock',ESMF_LOGMSG_WARNING)
          !call ESMF_LogWrite("... but this assumption is weird - skipping further action!",ESMF_LOGMSG_WARNING)

          !call ESMF_ClockAdvance(childClock, timeStep=timeInterval, rc=localrc)
          !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      enddo

      !! Now that all child components have been started, find out the minimum time
      !! to the next coupling and use this as a time step for my own clock Advance

      call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
        alarmCount=alarmCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (alarmCount==0) then
        !call ESMF_LogWrite('No alarm found in '//trim(myName), ESMF_LOGMSG_WARNING)
        timeInterval=stopTime-currTime
      else
        if (allocated(alarmList)) deallocate(alarmList)
        allocate(alarmList(alarmCount))

        call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmList=alarmList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AlarmGet(alarmList(1), ringTime=ringTime, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do j=2,alarmCount
          call ESMF_AlarmGet(alarmList(j), ringTime=time, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (time<ringTime) ringTime=time
        enddo

        timeInterval=ringTime-currTime
      endif

      !> Log current and next ring time
      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(myName)//' '//trim(timeString)//' '//trim(myName)//' stepping to'
      call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(message)//' '//trim(timeString)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc)

      call ESMF_ClockGet(myClock, advanceCount=advanceCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridCompGet(gridComp, localPet=localPet, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> If advanceCount==1 then go through all components and log their import and export states

      if (localPet==0 .and. advanceCount==0) then
        call ESMF_AttributeGet(importState, name='simulation_title', value=message, defaultvalue='Untitled', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_LogOpen(stateLog,'states_'//trim(message), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_LogWrite('====== Status at end of first run loop  ======', ESMF_LOGMSG_INFO, log=stateLog)

        do i=1,numGridComp
          call ESMF_LogWrite('====== States of '//trim(gridCompNameList(i))//' ======', ESMF_LOGMSG_INFO, log=stateLog)
          call MOSSCO_StateLog(gridImportStateList(i), deep=.true., log=stateLog, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call MOSSCO_StateLog(gridExportStateList(i), deep=.true., log=stateLog, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        enddo

        call ESMF_LogWrite('====== States of '//trim(myName)//' ======', ESMF_LOGMSG_INFO, log=stateLog)
        call MOSSCO_StateLog(importState, log=stateLog, deep=.true.,  rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_StateLog(exportState, log=stateLog, deep=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_LogClose(stateLog)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      !> Set new time interval and advance clock, stop if end of
      !! simulation reached
      call ESMF_ClockSet(myClock, timeStep=timeInterval, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockAdvance(myClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (ESMF_ClockIsStopTime(myClock, rc=localrc)) exit
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    enddo

    !! Running final netcdf output coupling wind_input to wind_output
    call ESMF_CplCompRun(cplCompList(1), importState=gridImportStateList(4), &
      exportState=gridExportStateList(1), clock=controlClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do phase=1,gridCompPhaseCountList(1)
      call ESMF_GridCompRun(gridCompList(1), importState=gridImportStateList(1), &
        exportState=gridExportStateList(1), clock=controlClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    !! Running final netcdf output coupling schism to schism_output
    call ESMF_CplCompRun(cplCompList(1), importState=gridImportStateList(5), &
      exportState=gridExportStateList(2), clock=controlClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do phase=1,gridCompPhaseCountList(2)
      call ESMF_GridCompRun(gridCompList(2), importState=gridImportStateList(2), &
        exportState=gridExportStateList(2), clock=controlClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I8)   :: i
    integer(ESMF_KIND_I4)   :: numGridComp, numCplComp, localrc
    character(ESMF_MAXSTR)  :: myName
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    integer(ESMF_KIND_I4)  :: phase, phaseCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: gridCompPhaseCountList,CplCompPhaseCountList
    logical, allocatable   :: GridCompHasPhaseZeroList(:)
    logical                :: hasPhaseZero, isPresent
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=myName, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Establish number of phases and zero phase for all components
    !! @> todo this interface will likely change in the future and will
    !! be integrated with GridCompGet

    numGridComp=size(gridCompList)
    numCplComp=size(cplCompList)

    allocate(GridCompHasPhaseZeroList(numGridComp))
    allocate(gridCompPhaseCountList(numGridComp))

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_FINALIZE, &
        phaseCount=phaseCount, phaseZeroFlag=hasPhaseZero, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridCompPhaseCountList(i)=phaseCount
      GridCompHasPhaseZeroList(i)=hasPhaseZero
    enddo

    allocate(CplCompPhaseCountList(numCplComp))
    !> @todo reenable if implemented in ESMF versions
    CplCompPhaseCountList(:)=1

    !do i = 1, numCplComp
    !  call ESMF_CplCompGetEPPhaseCount(cplCompList(i), ESMF_METHOD_FINALIZE, &
    !    phaseCount=CplCompPhaseCountList(i), phaseZeroFlag=hasPhaseZero, rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !  if (.not. hasPhaseZero) cycle
    !enddo

    do i=1,ubound(cplCompList,1)
      call ESMF_CplCompFinalize(cplCompList(i), clock=clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    do i=1,ubound(gridCompList,1)
      do phase=1,gridCompPhaseCountList(i)
        !call ESMF_LogWrite(trim(myName)//' tells '//trim(gridCompNameList(i))//' to finalize', ESMF_LOGMSG_INFO)
        call ESMF_GridCompFinalize(gridCompList(i), importState=gridImportStateList(i), exportState= &
          gridExportStateList(i), clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo
    enddo

    do i=1,ubound(gridCompList,1)
      !!@todo destroy any remaining fields/arrays in states
      call MOSSCO_DestroyOwn(gridExportStateList(i), owner=trim(myName),  rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_DestroyOwn(gridImportStateList(i), owner=trim(myName),  rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    call ESMF_GridCompDestroy(gridCompList(1), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gridCompList(2), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gridCompList(3), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gridCompList(4), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gridCompList(5), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompDestroy(cplCompList(1), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompDestroy(cplCompList(2), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(gridCompClockList)) deallocate(gridCompClockList)
    if (allocated(gridCompList)) deallocate(gridCompList)
    if (allocated(cplCompList))  deallocate(cplCompList)
    if (allocated(gridExportStateList)) deallocate(gridExportStateList)
    if (allocated(gridImportStateList)) deallocate(gridImportStateList)
    if (allocated(cplAlarmList)) deallocate(cplAlarmList)

    call ESMF_ClockDestroy(controlClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module toplevel_component
