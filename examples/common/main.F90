!> @brief Main routine for calling coupled systems
!> @file main.F90
!!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, HZG
!> @author Knut Klingbeil, IOW
!> @author Richard Hofmeister, HZG
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "main.F90"

#undef  ESMF_METHOD
#define ESMF_METHOD "main"
program main

  use esmf
  use toplevel_component, only: SetServices
  use mossco_time
  use mossco_strings

  implicit none

  type(ESMF_Time)            :: time1, time2, startTime, stopTime
  type(ESMF_TimeInterval)    :: runDuration
  integer                    :: localrc, rc,nmlunit=2013
  double precision           :: seconds
  character(len=40)          :: timestring
  character(len=40)          :: start='2000-01-01 00:00:00'
  character(len=40)          :: stop='2000-01-05 00:00:00'
  character(len=40)          :: title='Untitled'
  type(ESMF_GridComp)        :: topComp
  type(ESMF_State)           :: topState ! for import and export, empty
  type(ESMF_Clock)           :: mainClock,topClock
  type(ESMF_VM)              :: vm
  integer(ESMF_KIND_I4)      :: iostat, localPet, petCount
  logical                    :: ClockIsPresent
  character(len=ESMF_MAXSTR) :: message, formatstring


!> Read the namelist `mossco_run.nml`and evaluate three parameters:
!> 1. `start`: the start date of the simulation in YYYY-MM-DD hh:mm:ss format
!> 2. `stop` : the stop date of the simulation in the same format
!> 3. `title`: the title of the simulation.
!>
!> If this file is not present, then the default simulation with title "Untitled"
!> will be executed for the time 2000-01-01 00:00:00 to 2000-01-05 00:00:00

  namelist /mossco_run/ title,start,stop

  !@todo use inquire to test for existence

  ! Read mossco_run.nml
  open(nmlunit,file='mossco_run.nml',status='old',action='read',iostat=iostat)
  if (iostat .eq. 0) then
    read(nmlunit,nml=mossco_run)
    close(nmlunit)
  end if

!> @todo implement alternative reading of mossco.config
!> preferential over namelist

  ! substitute slash and space characters in title string
  call replace_character(title,'/','-')
  call replace_character(title,' ','_')

  ! Initialize ESMF, get resources, and log this
  call ESMF_Initialize(defaultLogFileName=trim(title),rc=localrc,&
    logkindflag=ESMF_LOGKIND_MULTI,defaultCalKind=ESMF_CALKIND_GREGORIAN,&
    vm=vm)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !call ESMF_LogSet(flush=.false., logMsgList=(/ESMF_LOGMSG_ERROR, ESMF_LOGMSG_WARNING/),rc=localrc)
  call ESMF_LogSet(flush=.true., logMsgList=(/ESMF_LOGMSG_ALL/),rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  write(message,'(A)')  'MOSSCO '//trim(title)//" coupled system starts"
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  write(formatstring,'(A)') '(A,'//intformat(localPet)//',A,'//intformat(petCount)//')'
  write(message,formatstring) 'Creating multiple logs, this is processor ',localPet,' of ', petCount
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

! Get the wall clock starting time
  call ESMF_TimeSet(time1, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeSyncToRealTime(time1,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeGet(time1,timeStringISOFrac=timestring, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  write(message,'(A)')  "Program starts at wall clock "//trim(timestring)
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

! Create and initialize a clock from mossco_run.nml
  call timeString2ESMF_Time(start,startTime)
  call timeString2ESMF_Time(stop,stopTime)
  runDuration = stopTime - startTime

  mainClock = ESMF_ClockCreate(timeStep=runDuration, startTime=startTime, stopTime=stopTime, &
    name=trim(title), rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!> @todo feature request sent to ESMF for supporting clock attributes (not implemented)
!	call ESMF_AttributeSet(mainClock, 'creator', trim(name), rc=localrc)
!  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeGet(startTime,timeStringISOFrac=timestring, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("Simulation starts at "//timestring, ESMF_LOGMSG_INFO)

  call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("Simulation ends at "//timestring, ESMF_LOGMSG_INFO)

! Create toplevel component and call its setservices routines, if namelist was successfully read, then copy the
! main clock to the toplevel (child) clock.  If no time information from namelist, then let the toplevel component
! read the time and pass it back to main clock
  if (iostat .eq. 0) then
    topClock = ESMF_ClockCreate(mainClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockSet(topClock,name="toplevel", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    topComp = ESMF_GridCompCreate(name="toplevel",clock=topClock,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  else
    topComp = ESMF_GridCompCreate(name="toplevel", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  endif

  call ESMF_GridCompSetServices(topComp,SetServices,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  topState = ESMF_StateCreate(name="topState",rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompGet(topComp,clockIsPresent=ClockIsPresent, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(topComp,importState=topState,exportState=topState,clock=mainClock, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (iostat .ne. 0) then
    call ESMF_GridCompGet(topComp,clockIsPresent=ClockIsPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (clockIsPresent) then
      call ESMF_GridCompGet(topComp,clock=topClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockGet(topClock,startTime=startTime, stopTime=stopTime,runDuration=runDuration, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockSet(mainClock,startTime=startTime, stopTime=stopTime,timeStep=runDuration, &
                  currTime=startTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
  end if

  call ESMF_GridCompRun(topComp,importState=topState,exportState=topState,clock=mainClock,rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

! Destroy toplevel component and clean up
  call ESMF_GridCompFinalize(topComp,importState=topState,exportState=topState,clock=mainClock,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(topComp,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("All ESMF components destroyed", ESMF_LOGMSG_INFO)

  call ESMF_TimeSet(time2,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeSyncToRealTime(time2,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeGet(time2,timeStringISOFrac=timestring, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeIntervalGet(time2-time1,s_r8=seconds, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  write(message,'(A,ES10.4,A)') trim(title)//' needed ',seconds,' seconds to run'
  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
  call ESMF_LogWrite('MOSSCO '//trim(title)//' finished at wall clock '//timestring,ESMF_LOGMSG_INFO)

  call ESMF_StateDestroy(topState,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!> @todo What about topClocks that were created locally?
  call ESMF_ClockDestroy(mainClock,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_Finalize(rc=localrc,endflag=ESMF_END_NORMAL)

end program main
