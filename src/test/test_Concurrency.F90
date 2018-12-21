!> @file test_Concurrency.F90
!! @brief Tests concurrent execution of the default and info components
!! @author Carsten Lemmen
!!

program test_Concurrency

use esmf
use default_component, only : default_SetServices => SetServices
use info_component,     only : info_SetServices => SetServices

integer                   :: rc
type(ESMF_State)          :: importState, exportState
type(ESMF_GridComp)       :: defaultComp, infoComp
type(ESMF_Time)           :: startTime, stopTime
type(ESMF_Clock)          :: clock
type(ESMF_VM)             :: vm
integer(ESMF_KIND_I4)     :: petCount, localPet
integer(ESMF_KIND_I4), allocatable, dimension(:) :: petList

call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, vm=vm)

call ESMF_TimeSet(startTime,yy=2002,mm=1,dd=1)
call ESMF_TimeSet(stopTime, yy=2003,mm=1,dd=1)
clock = ESMF_ClockCreate(timeStep=stopTime-startTime, startTime=startTime, stopTime=stopTime, & 
  name="clock", rc=rc)

!! Create States, register and initialize components
importState=ESMF_StateCreate(name='import', stateintent=ESMF_STATEINTENT_UNSPECIFIED, rc=rc)
exportState=ESMF_StateCreate(name='export', stateintent=ESMF_STATEINTENT_UNSPECIFIED, rc=rc)

!! Create components on different PET elements
call ESMF_VmGet(vm, petCount=petCount, rc=rc)

allocate(petList(petCount-1))
do i=1, petCount
  petList(i)=i-1
enddo
defaultComp=ESMF_GridCompCreate(name='default', clock=clock, petList=(/0/))
infoComp=ESMF_GridCompCreate(name='info', clock=clock, petList=petList)

call ESMF_GridCompSetServices(defaultComp, default_SetServices, rc=rc)
call ESMF_GridCompSetServices(infoComp, info_SetServices, rc=rc)

call ESMF_GridCompInitialize(defaultComp, importState=importState, exportState=exportState, clock=clock, &
rc=rc)
call ESMF_GridCompInitialize(defaultComp, importState=exportState, exportState=importState, clock=clock, &
rc=rc)

call ESMF_GridCompRun(defaultComp, importState=importState, exportState=exportState, clock=clock, &
rc=rc)
call ESMF_GridCompRun(infoComp, importState=exportState, exportState=importState, clock=clock, &
rc=rc)

call ESMF_GridCompFinalize(defaultComp, importState=importState, exportState=exportState, clock=clock, &
rc=rc)
call ESMF_GridCompFinalize(infoComp, importState=exportState, exportState=importState, clock=clock, &
rc=rc)

!call ESMF_ClockDestroy(clock,rc=rc)
call ESMF_Finalize()

end program
