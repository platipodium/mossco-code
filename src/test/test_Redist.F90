!> @file test_Redist.F90
!! @brief explores Redist capability on concurrent PET performance
!! @author Carsten Lemmen
!!
!! runs 1e6 times the ESMF routine FieldGet to retrieve the
!! array pointer from an ESMF_Field. The performance is much
!! improved in ESMF v.6.3beta4+

program test_redist

use ESMF
use test_component, only : SetServices

implicit none

type(ESMF_GridComp)  :: gridComp1, gridComp2
type(ESMF_State)     :: importState, exportState, state
type(ESMF_Grid)      :: grid
type(ESMF_Field)     :: srcField, dstField, field
type(ESMF_VM)        :: vm
type(ESMF_RouteHandle) :: routeHandle
type(ESMF_Clock)     :: clock
type(ESMF_Time)      :: startTime
type(ESMF_TimeInterval) :: timeInterval

integer(ESMF_KIND_I4)  :: petCount, localPet
integer(ESMF_KIND_I4), pointer, dimension(:,:) :: farrayPtr

integer              :: rc

call ESMF_Initialize(vm=vm, defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=rc)
call ESMF_VmGet(vm, petCount=petCount, localPet=localPet, rc=rc)
call ESMF_TimeSet(startTime, yy=2001, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
call ESMF_TimeIntervalSet(timeInterval, d=1, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
clock = ESMF_ClockCreate(startTime=startTime,timeStep=timeInterval, &
  stopTime=startTime+100*timeInterval, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

gridComp1 = ESMF_GridCompCreate(name='Comp 1', petList=(/0/), clock=clock, rc=rc)
gridComp2 = ESMF_GridCompCreate(name='Comp 2', petList=(/petCount-1/), clock=clock, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

importState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
  name='ImportState', rc=rc)
exportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
  name='ExportState', rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
call ESMF_GridCompSetServices(gridComp1, SetServices, rc=rc)
call ESMF_GridCompSetServices(gridComp1, SetServices, rc=rc)
call ESMF_GridCompSetServices(gridComp1, SetServices, rc=rc)
call ESMF_GridCompSetServices(gridComp2, SetServices, rc=rc)
call ESMF_GridCompSetServices(gridComp2, SetServices, rc=rc)
call ESMF_GridCompSetServices(gridComp2, SetServices, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_GridCompInitialize(gridComp1, importState=importState, &
  exportState=exportState,rc=rc)
call ESMF_GridCompInitialize(gridComp2, importState=exportState, &
  exportState=importState,rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

! Emulate behaviour of component 1 running on PET 0
if (localPet==0) then
  call ESMF_GridCompGet(gridComp1, exportState=state, rc=rc)
  grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
  regDecomp=(/2,2/), name="grid", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_I4, name='pet+1', rc=rc) 
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
  farrayPtr(:,:)=1
  call ESMF_StateAdd(state, (/field/), rc=rc)  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
endif

if (localPet==petCount-1) then
  call ESMF_GridCompGet(gridComp2, exportState=state, rc=rc)
  grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
  regDecomp=(/2,2/), name="grid", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_I4, name='pet+1', rc=rc) 
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
  farrayPtr(:,:)=2
  call ESMF_StateAdd(state, (/field/), rc=rc)  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
endif


if (localPet==petCount-1) then
  call ESMF_GridCompGet(gridComp2, exportState=state, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
  call ESMF_StateGet(state, 'pet+1', field=field, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
  call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
  write(0,'(10I1)') farrayPtr(1:10,1)
endif


call ESMF_StateReconcile(exportState)
call ESMF_StateReconcile(importState)

! Redist the data from one onto two
call ESMF_StateGet(exportState, 'pet+1', field=srcField, rc=rc)
call ESMF_StateGet(importState, 'pet+1', field=dstField, rc=rc)

call ESMF_FieldRedistStore(srcField, dstField, routeHandle=routeHandle, rc=rc)
call ESMF_FieldRedist(srcField, dstField, routeHandle=routeHandle, rc=rc)
call ESMF_FieldRedistRelease(routeHandle=routeHandle, rc=rc)

if (localPet==petCount-1) then
  call ESMF_GridCompGet(gridComp2, exportState=state, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
  call ESMF_StateGet(state, 'pet+1', field=field, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
  call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
  write(0,'(10I1)') farrayPtr(1:10,1)
endif


call ESMF_Finalize()

end program

