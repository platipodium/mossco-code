!> @file test_ClockDestroy.F90
!! @brief Tests the function of ESMF_ClockDestroy 
!! @author Carsten Lemmen
!! @author Ulrich Körner
!!

program test_ClockDestroy

use esmf
use empty_component, only: SetServices

implicit none

integer                   :: rc
type(ESMF_Clock)          :: clock
type(ESMF_State)          :: istate, estate
type(ESMF_GridComp),allocatable       :: comps(:)
type(ESMF_Time)           :: clockTime, startTime, stopTime
character(ESMF_MAXSTR)    :: timestring
logical                   :: clockIsPresent

call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN)

call ESMF_TimeSet(startTime,yy=2002,mm=1,dd=1,h=0,m=0,s=0)
call ESMF_TimeSet(stopTime, yy=2003,mm=1,dd=1,h=0,m=0,s=0)
call ESMF_TimeSet(clockTime,rc=rc)


allocate(comps(2))

clock = ESMF_ClockCreate(timeStep=stopTime-startTime, startTime=startTime, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

istate = ESMF_StateCreate(rc=rc)
estate = ESMF_StateCreate(rc=rc)

comps(1) = ESMF_GridCompCreate(clock=clock, rc=rc)
comps(2) = ESMF_GridCompCreate(clock=clock, rc=rc)


call ESMF_GridCompSetServices(comps(1), SetServices, rc=rc)
call ESMF_GridCompSetServices(comps(2), SetServices, rc=rc)


call ESMF_GridCompInitialize(comps(1), importState=iState, exportState=eState, clock=clock)
call ESMF_GridCompInitialize(comps(2), importState=iState, exportState=eState, clock=clock)


call ESMF_GridCompGet(comps(1), clockIsPresent=clockIsPresent)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
write(0,*) clockIsPresent

call ESMF_ClockValidate(clock)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
! Now call Finalize(), which should internally call clockDestroy for this component's clock
!call ESMF_GridCompFinalize(comps(1), importState=iState, exportState=eState, clock=clock)

! We expect that after the Finalize() call, there is no clock anymore
call ESMF_GridCompGet(comps(1), clockIsPresent=clockIsPresent)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
write(0,*) clockIsPresent

call ESMF_ClockValidate(clock, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_LogWrite('Clock not valid', ESMF_LOGMSG_WARNING)
call ESMF_ClockDestroy(clock, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
call ESMF_GridCompGet(comps(1), clockIsPresent=clockIsPresent)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
call ESMF_ClockValidate(clock)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


! Next line gives a segfault, since the Finalize() call tries to destroy the (already 
! destroyed) clock
!call ESMF_GridCompFinalize(comps(2), importState=iState, exportState=eState, clock=clock)

call ESMF_GridCompDestroy(comps(1))
call ESMF_Finalize()

end program
