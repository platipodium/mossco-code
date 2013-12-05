module mossco_time

use esmf

implicit none

interface MOSSCO_ClockGetTimeStepToNextAlarm
  module procedure MOSSCO_ClockGetTimeStepToNextAlarm_componentname
  module procedure MOSSCO_ClockGetTimeStepToNextAlarm_all
end interface MOSSCO_ClockGetTimeStepToNextAlarm

character(len=1), parameter :: MOSSCO_CPL_SEPARATOR = ':'

contains

subroutine MOSSCO_ClockSetTimeStepByAlarms(clock, rc)
  type(ESMF_Clock), intent(inout) :: clock
  integer(ESMF_KIND_I4), intent(out), optional :: rc

  type(ESMF_TimeInterval) :: timeInterval

	call MOSSCO_ClockGetTimeStepToNextAlarm(clock, timeInterval, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
  call ESMF_ClockSet(clock, timeStep=timeInterval, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
  return
end subroutine MOSSCO_ClockSetTimeStepByAlarms    


!> This subroutine searches all of a clock's alarms and returns the time 
!! interval to the next ringing alarm
subroutine MOSSCO_ClockGetTimeStepToNextAlarm_all(clock, timeInterval, rc)
  type (ESMF_Clock), intent(in) :: clock
  type (ESMF_TimeInterval), intent(out) :: timeInterval
  integer(ESMF_KIND_I4), intent(out), optional :: rc

  call MOSSCO_ClockGetTimeStepToNextAlarm_componentname(clock, 'global', timeInterval, rc)
  return
  
end subroutine MOSSCO_ClockGetTimeStepToNextAlarm_all

!> This subroutine searches only some of a clock's  alarms and returns the time 
!! interval to the next ringing alarm, the selection is based on the components
subroutine MOSSCO_ClockGetTimeStepToNextAlarm_componentname(clock, componentName, timeInterval, rc)
  type (ESMF_Clock), intent(in) :: clock
  type (ESMF_TimeInterval), intent(out) :: timeInterval
  integer(ESMF_KIND_I4), intent(out), optional :: rc
  character (len=*), intent(in) :: componentname

  type(ESMF_Time)         :: ringTime, time, currentTime
  type(ESMF_Alarm), dimension(:), allocatable :: alarmList
  integer(ESMF_KIND_I4) :: n,i, hours
  character (ESMF_MAXSTR) :: name, message, ringName, timeString1, timeString2

  call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmCount=n,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  allocate(alarmList(n))
  
  call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmList=alarmList,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
  call ESMF_ClockGet(clock,stopTime=time, name=ringName, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  do i=1,n
    call ESMF_AlarmGet(alarmList(i),ringTime=ringTime,name=name,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    !call ESMF_AlarmPrint(alarmList(i))
        
    if (index(trim(componentname),'global').eq.0) then
      if (index(trim(name),trim(componentName)).eq.0) then 
        cycle
      endif
    endif
    
    if (ringtime<time) then
      time=ringTime
      ringName=name
    endif
  enddo
  
  if (allocated(alarmList)) deallocate(alarmList)
  
  call ESMF_ClockGet(clock,currTime=currentTime,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)  
  call ESMF_TimeGet(currentTime,timeString=timeString1)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)  
  call ESMF_TimeGet(time,timeString=timeString2)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)  

  if (time<currentTime) then
    write(message,'(A)')  trim(timeString1)//': negative time step to '//trim(timestring2)
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR, rc=rc)
    call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  endif
  
  timeInterval=time - currentTime
  call ESMF_TimeIntervalGet(timeInterval, h=hours, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)  
     
  write(message,'(A,A)') trim(timeString1)//': next coupling '//trim(componentName), &
     ' ('//trim(ringName)//')' 
  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)  

  write(message,'(A,I5,A)') '    in ',hours, ' hours. (at '//trim(timeString2)//')'  
  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)  

  return
  
end subroutine  MOSSCO_ClockGetTimeStepToNextAlarm_componentname

end module mossco_time