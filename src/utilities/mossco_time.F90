!> @brief Implementation of time utilities
!>
!> This computer program is part of MOSSCO.
!> @copyright 2021-2023 Helmholtz-Zentrum hereon GmbH
!> @copyright 2014-2021 Helmholtz-Zentrum Geesthacht GmbH
!> @author Carsten Lemmen <carsten.lemmen@hereon.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "mossco_time.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module mossco_time

use esmf

implicit none

interface MOSSCO_ClockGetTimeStepToNextAlarm
  module procedure MOSSCO_ClockGetTimeStepToNextAlarm_componentname
  module procedure MOSSCO_ClockGetTimeStepToNextAlarm_all
end interface MOSSCO_ClockGetTimeStepToNextAlarm

character(len=1), parameter :: MOSSCO_CPL_SEPARATOR = ':'

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ClockSetTimeStepByAlarms"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ClockGetTimeStepToNextAlarm_all"
subroutine MOSSCO_ClockGetTimeStepToNextAlarm_all(clock, timeInterval, rc)
  type (ESMF_Clock), intent(in) :: clock
  type (ESMF_TimeInterval), intent(out) :: timeInterval
  integer(ESMF_KIND_I4), intent(out), optional :: rc

  call MOSSCO_ClockGetTimeStepToNextAlarm_componentname(clock, 'global', timeInterval, rc)
  return

end subroutine MOSSCO_ClockGetTimeStepToNextAlarm_all

!> This subroutine searches only some of a clock's  alarms and returns the time
!! interval to the next ringing alarm, the selection is based on the components
#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ClockGetTimeStepToNextAlarm_componentname"
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

  !write(message,'(A,A)') '  '//trim(timeString1)//': next coupling '//trim(componentName), &
  !   ' ('//trim(ringName)//')'
  !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO, rc=rc)

  !write(message,'(A,I5,A)') '  in ',hours, ' hours. (at '//trim(timeString2)//')'
  !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO, rc=rc)

  return

end subroutine  MOSSCO_ClockGetTimeStepToNextAlarm_componentname

!> @description Takes an ISO 8601 time string and converts this
!> to an ESMF Time object.  This implementation
!> also extends the ISO 8601 strict definition by allowing less
!> digits per item in a time string.
!> @todo make more robust and accept all valid cases
!> @todo more gracefully handle no calendar provided
!
! Date 2020-01-10
! Date and time in UTC	2020-01-10T13:12:31+00:00
! 2020-01-10T13:12:31Z
! 20200110T131231Z
! Week	2020-W02
! Date with week number	2020-W02-5
! Ordinal date	2020-010
! Extension 2020-1-10T13:1:31
!
#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_TimeSet"
subroutine MOSSCO_TimeSet(time, datetimestring, rc)

  character(len=*), intent(in)          :: datetimestring
  type(ESMF_Time), intent(inout)        :: time
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  integer(ESMF_KIND_I4) :: localrc=0, rc_=0, i=0
  integer               :: yy=1, mm=1, dd=1, h=0, m=0, s=0
  character(len=10)     :: dateString=''
  character(len=10)     :: timeString=''
  character(len=ESMF_MAXSTR) :: message=''

  rc_ = ESMF_SUCCESS

  ! If 'T' or ' ' are found within string, it consists of a date part and a time
  ! part, which can be separately handled
  i = index(datetimestring, 'T')
  if (i<1) i=index(datetimestring(1:len_trim(datetimestring)), ' ')
  if (i>1 .and. i<11) then
    write(datestring,'(A)') trim(adjustl(datetimestring(1:i-1)))
    write(timestring,'(A)') trim(adjustl(datetimestring(&
      i+1:min(len_trim(datetimestring),i+11))))
  else
    write(datestring,'(A)') trim(adjustl(datetimestring(&
      1:min(len_trim(datetimestring),10))))
  endif
  write(timestring,'(A)') trim(adjustl(datetimestring(i+1:len_trim(datetimestring))))

  i=index(datestring, 'W')
  if (i>0) then
    localrc = ESMF_RC_NOT_IMPL
    write(message, '(A)') 'Calendar week format not implemented for date '//trim(datestring)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
  endif

  i=index(datestring, '-')
  if (i<1) then
    if (len_trim(datestring) >= 4)  read(datestring(1:4),'(i4)', iostat=localrc) yy
    if (len_trim(datestring) >= 7)  read(datestring(6:7),'(i2)', iostat=localrc) mm
    if (len_trim(datestring) >= 10) read(datestring(9:10),'(i2)', iostat=localrc) dd
  else
    if (i==5) read(datestring(1:4),'(i4)', iostat=localrc) yy
    if (i==4) read(datestring(1:3),'(i3)', iostat=localrc) yy
    if (i==3) read(datestring(1:2),'(i2)', iostat=localrc) yy
    if (i==2) read(datestring(1:1),'(i1)', iostat=localrc) yy
    message=''
    message(1:len_trim(datestring)-i+1) = datestring(i+1:len_trim(datestring))
    write(datestring,'(A)') trim(message)
    i=index(datestring, '-')
  endif

  if (i>0) then ! a second hyphen was found
    if (i==3) read(datestring(1:2),'(i2)', iostat=localrc) mm
    if (i==2) read(datestring(1:1),'(i1)', iostat=localrc) mm
    message=''
    message(1:len_trim(datestring)-i+1) = datestring(i+1:len_trim(datestring))
    write(datestring,'(A)') trim(message)
    if (len_trim(datestring) == 2) read(datestring(1:2),'(i2)', iostat=localrc) dd
    if (len_trim(datestring) == 1) read(datestring(1:1),'(i1)', iostat=localrc) dd
  endif

  ! Deal with time now
  i=index(timestring, ':')
  if (i<1) then
    if (len_trim(timestring) > 1)  read(timestring(1:2),'(i4)', iostat=localrc) h
    if (len_trim(timestring) > 3)  read(timestring(3:4),'(i2)', iostat=localrc) m
    if (len_trim(timestring) > 5)  read(timestring(4:5),'(i2)', iostat=localrc) s
  else
    if (i==3) read(timestring(1:2),'(i2)', iostat=localrc) h
    if (i==2) read(timestring(1:1),'(i1)', iostat=localrc) h
    message=''
    message(1:len_trim(timestring)-i+1) = timestring(i+1:len_trim(timestring))
    write(timestring,'(A)') trim(message)
    i=index(timestring, ':')
  endif

  if (i>0) then ! a second colon was found
    if (i==3) read(timestring(1:2),'(i2)', iostat=localrc) m
    if (i==2) read(timestring(1:1),'(i1)', iostat=localrc) m
    message=''
    message(1:len_trim(timestring)-i+1) = timestring(i+1:len_trim(timestring))
    write(timestring,'(A)') trim(message)
    if (len_trim(timestring) == 1) read(timestring(1:2),'(i2)', iostat=localrc) s
    if (len_trim(timestring) == 2) read(timestring(1:1),'(i1)', iostat=localrc) s
  endif

  ! There is no year zero
  if (yy == 0) yy=1
  if (mm == 0) mm=1
  if (dd == 0) dd=1

  call ESMF_TimeSet(time, yy=yy, mm=mm, dd=dd, h=h,m =m, s=s, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  if (present(rc)) rc=rc_
  return

end subroutine MOSSCO_TimeSet

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_TimeIntervalSet"
subroutine MOSSCO_TimeIntervalSet(timeInterval, timeString, kwe, rc)

  type(ESMF_TimeInterval), intent(out)         :: timeInterval
  character(len=*), intent(in)                 :: timeString
  logical, intent(in), optional                :: kwe
  integer(ESMF_KIND_I4), intent(out), optional :: rc

  integer(ESMF_KIND_I4)        :: localrc, rc_
  integer(ESMF_KIND_I4)        :: yy=0,mm=0,d=0,h=0,m=0,s=0, i, n
  character(len=ESMF_MAXSTR)   :: unit, message, string

  rc_=ESMF_SUCCESS
  if (present(kwe)) rc=rc

  string = adjustl(timeString)
  n = len_trim(string)
  i = index(trim(string), ' ', back=.true.)

  if (i > 1 .and. i < n) then
    write(unit, '(A)') string(i+1:n)
  else
    write(unit,'(A)') 'd'
  endif
  unit=adjustl(unit)

  if (i < 2) then
    d = 1
    write(message,'(A)') '  used default time interval of 1 day'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
  else

    select case(trim(unit))
    case ('yy', 'year', 'years')
      read(string(1:i-1),*) yy
    case ('mm', 'month', 'months')
      read(string(1:i-1),*) mm
    case ('d', 'day', 'days')
      read(string(1:i-1),*) d
    case ('h', 'hour', 'hours')
      read(string(1:i-1),*) h
    case ('m', 'minute', 'minutes')
      read(string(1:i-1),*) m
    case ('s', 'second', 'seconds')
      read(string(1:i-1),*) s
    case default
      write(message,'(A)') '  obtained unknown unit '//trim(unit)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      if (present(rc)) rc=ESMF_RC_ARG_BAD
      return
    endselect
  endif

  call ESMF_TimeIntervalSet(timeInterval,yy=yy,mm=mm,d=d,h=h,m=m,s=s, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (present(rc)) rc=rc_
  return

end subroutine MOSSCO_TimeIntervalSet

!> Actually, this should be an extension of ESMF_TimeSet
#undef  ESMF_METHOD
#define ESMF_METHOD "timeString2ESMF_Time"
subroutine timeString2ESMF_Time(timestring,time)
  character(len=*), intent(in) :: timestring
  type(ESMF_Time), intent(out) :: time

  integer :: yy,mm,dd,h,m,s, rc_, localrc

  read(timestring(1:4),'(i4)') yy
  read(timestring(6:7),'(i2)') mm
  read(timestring(9:10),'(i2)') dd
  read(timestring(12:13),'(i2)') h
  read(timestring(15:16),'(i2)') m
  read(timestring(18:19),'(i2)') s

  call ESMF_LogWrite('This method is deprecated, please use MOSSCO_TimeSet', ESMF_LOGMSG_WARNING)

  call ESMF_TimeSet(time,yy=yy,mm=mm,dd=dd,h=h,m=m,s=s, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine timeString2ESMF_Time

subroutine timeString2ISOTimeString(timeString, isoString, rc)

  character(len=*), intent(in)                 :: timeString
  character(len=ESMF_MAXSTR), intent(out)      :: isoString
  integer(ESMF_KIND_I4), optional, intent(out) :: rc

  integer(ESMF_KIND_I4)          :: rc_, i, y, m, d, hh, mm
  character(len=ESMF_MAXSTR)     :: string
  real(ESMF_KIND_R8)             :: ss

  rc_ = ESMF_SUCCESS

  ! Make sure that this is in ISO format, i.e. YYYY-MM-DDThh:mm:ss
  ! Some implementations do not write 4 (or 2) digits single digit components.

  y=1
  m=1
  d=1
  hh=0
  mm=0
  ss=0.0D0

  string=trim(adjustl(timeString))

  do while (.true.)
    i = index(string,'-')
    if (i>1) then
      read(string(1:i-1),*) y
    else
      read(string(1:4),'(I4)') y
      i=4
    endif

    if (i>=len_trim(string)) exit
    string=string(i+1:len_trim(string))

    i = index(string,'-')
    if (i>1) then
      read(string(1:i-1),*) m
    else
      read(string(1:2),*) m
      i=2
    endif

    if (i>=len_trim(string)) exit
    string=string(i+1:len_trim(string))

    i = index(string,'T')
    if (i<1) i = index(string,' ')
    if (i>1) then
      read(string(1:i-1),*) d
    else
      read(string(1:2),*) d
      i=2
    endif

    if (i>=len_trim(string)) exit
    string=string(i+1:len_trim(string))

    i = index(string,':')
    if (i>1) then
      read(string(1:i-1),*) hh
    else
      read(string(1:2),*) hh
      i=2
    endif

    if (i>=len_trim(string)) exit
    string=string(i+1:len_trim(string))

    i = index(string,':')
    if (i>1) then
      read(string(1:i-1),*) mm
    else
      read(string(1:2),*) mm
      i=2
    endif

    if (i>=len_trim(string)) exit

    read(string(i+1:len_trim(string)),*) ss
    exit

  enddo
  write(isoString,'(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)') y,'-',m,'-',d,'T', &
    hh,':',mm,':',int(ss)

  if (present(rc)) rc = rc_
  return

end subroutine timeString2ISOTimeString


end module mossco_time
