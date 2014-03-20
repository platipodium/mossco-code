!> @file test_Clock.F90
!! @brief Tests the speed of the clock
!! @author Richard Hofmeister
!!
!! test the performance of the ESMF_Clock related routines. In the test,
!! 0.5e6 ESMF-controlled timesteps are iterated and an additional 
!! plain loop, which iterates through a running integer and per timestep
!! calls ESMF_Clock routines to get the current time.
!! The ESMF iteration is found to be 3x slower. 

program test_Clock

use esmf

integer                   :: rc
integer(kind=8)           :: i,n,nn
integer(kind=8)           :: tic1,tic2,tic3,toc1,toc2,toc3,gettimetick
integer(kind=8)           :: alarmtick,totaltick,inittick,looptick,systemclock,ticks_per_sec
logical                   :: isstop
type(ESMF_Clock)          :: clock,secondclock
type(ESMF_Time)           :: clockTime, startTime, stopTime
type(ESMF_TimeInterval)   :: timeStepIntv,timeStepIntv2, alarmIntv
character(ESMF_MAXSTR)    :: timestring
type(ESMF_Alarm)          :: someAlarm
real(kind=8)              :: tickrate

inittick=0
totaltick=0
alarmtick=0
looptick=0
gettimetick=0
nn=0

call system_clock(tic1)
call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN)

call ESMF_TimeSet(startTime,yy=2002,mm=1,dd=1,h=0,m=0,s=0)
call ESMF_TimeSet(stopTime, yy=2003,mm=1,dd=1,h=0,m=0,s=0)
call ESMF_TimeSet(clockTime,rc=rc)
call ESMF_TimeIntervalSet(timeStepIntv, s_r8=real(60.,kind=ESMF_KIND_R8), rc=rc)
call ESMF_TimeIntervalSet(timeStepIntv2, s_r8=real(30.,kind=ESMF_KIND_R8), rc=rc)
call ESMF_TimeIntervalSet(alarmIntv, s_r8=real(86400.,kind=ESMF_KIND_R8), rc=rc)

clock = ESMF_ClockCreate(timeStep=timeStepIntv, startTime=startTime, stopTime=stopTime, & 
     name="Parent clock")

secondclock = ESMF_ClockCreate(clock)
call ESMF_ClockSet(secondclock,timeStep=timeStepIntv2)

someAlarm = ESMF_AlarmCreate(clock=clock,name="some alarm", &
      ringTime=startTime+alarmIntv,ringInterval=alarmIntv,rc=rc)

call system_clock(toc1)
inittick=toc1-tic1

call system_clock(tic1)
do while (.not. ESMF_ClockIsStopTime(clock))
#if 1
  call system_clock(tic3)
  call ESMF_ClockGet(clock,currTime=clockTime, advanceCount=n, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! just ask for the current time
  call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call system_clock(toc3)
  gettimetick=gettimetick + toc3-tic3

  ! call the loop-related ESMF routines again on second clock
  call system_clock(tic3)

  if (.not.(ESMF_ClockIsStopTime(secondclock))) call ESMF_ClockAdvance(secondclock,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)  

  call system_clock(toc3)
  looptick=looptick+toc3-tic3


  call system_clock(tic2)

  if (ESMF_AlarmIsRinging(someAlarm)) then
    call ESMF_AlarmRingerOff(someAlarm,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  endif

  call system_clock(toc2)
  alarmtick = alarmtick + toc2-tic2
#endif

  call ESMF_ClockAdvance(clock, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
end do
call system_clock(toc1)
totaltick=toc1-tic1

call system_clock( count=systemclock, count_rate=ticks_per_sec)
tick_rate = 1.0d0/ticks_per_sec

write(0,*) ' finished',n,'timesteps'
write(0,*) '----------------------------------------------'
write(0,*) ' initialisation [s]:',inittick*tick_rate
write(0,*) ' whole loop [s]:',totaltick*tick_rate
write(0,*) ' alarm [s]:',alarmtick*tick_rate
write(0,*) ' ClockIsStop and ClockAdvance [s]:',looptick*tick_rate
write(0,*) ' ClockGet and TimeGet [s]:',gettimetick*tick_rate

call system_clock(tic1)
do i=1,n
#if 1
#if 1
  call ESMF_ClockGet(clock,currTime=clockTime, advanceCount=nn, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! just ask for the current time
  call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

  if (.not.(ESMF_ClockIsStopTime(secondclock))) call ESMF_ClockAdvance(secondclock,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)  

  if (ESMF_AlarmIsRinging(someAlarm)) then
    call ESMF_AlarmRingerOff(someAlarm,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  endif
#endif
! add a stupid operation for a check without ESMF calls
if (mod(nn,7) .eq. 0) nn=nn+1

end do
call system_clock(toc1)
write(0,*) '----------------------------------------------'
write(0,*) ' plain loop:',(toc1-tic1)*tick_rate

call ESMF_ClockDestroy(clock,rc=rc)

call ESMF_Finalize()

end program
