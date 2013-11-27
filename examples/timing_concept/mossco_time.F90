module mossco_time

use esmf

implicit none

!interface MOSSCO_ClockGetTimeStepToNextAlarm
!  module procedure MOSSCO_ClockGetTimeStepToNextAlarm_components
!  module procedure MOSSCO_ClockGetTimeStepToNextAlarm_all
!end interface MOSSCO_ClockGetTimeStepToNextAlarm


contains


subroutine MOSSCO_ClockSetTimeStepByAlarms(clock, rc)
  type(ESMF_Clock), intent(inout) :: clock
  integer(ESMF_KIND_I4), intent(out), optional :: rc

  type(ESMF_TimeInterval) :: timeInterval

	call MOSSCO_ClockGetTimeStepToNextAlarm(clock, timeInterval, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
  call ESMF_ClockSet(clock, timeStep=timeInterval, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
end subroutine MOSSCO_ClockSetTimeStepByAlarms    


!> This subroutine searches a clock's alarms and returns the time interval to the next 
!! ringing alarm
subroutine MOSSCO_ClockGetTimeStepToNextAlarm_all(clock, timeInterval, rc)
  type (ESMF_Clock), intent(in) :: clock
  type (ESMF_TimeInterval), intent(out) :: timeInterval
  integer(ESMF_KIND_I4), intent(out), optional :: rc

  type (ESMF_GridComp), dimension(:), pointer :: component
end subroutine MOSSCO_ClockGetTimeStepToNextAlarm_all

!> This subroutine searches a clock's alarms and returns the time interval to the next 
!! ringing alarm
subroutine MOSSCO_ClockGetTimeStepToNextAlarm_components(clock, timeInterval, component, rc)
  type (ESMF_Clock), intent(in) :: clock
  type (ESMF_TimeInterval), intent(out) :: timeInterval
  integer(ESMF_KIND_I4), intent(out), optional :: rc
  type (ESMF_GridComp), dimension(:), intent(in) :: component

  type(ESMF_Time)         :: ringTime, time, currentTime
  type(ESMF_Alarm), dimension(:), allocatable :: alarmList
  integer(ESMF_KIND_I4) :: n,i

  call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmCount=n,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  allocate(alarmList(n))
  
  call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmList=alarmList,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
  n=size(alarmList)
  if (n>0) then
    call ESMF_AlarmGet(alarmList(1),ringTime=time,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  endif
      
  do i=2,n
    call ESMF_AlarmGet(alarmList(i),ringTime=ringTime,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (ringtime<time) time=ringTime
  enddo
  
  if (allocated(alarmList)) deallocate(alarmList)
  
  call ESMF_ClockGet(clock,currTime=currentTime,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  timeInterval=time - currentTime

end subroutine  MOSSCO_ClockGetTimeStepToNextAlarm_components

end module mossco_time