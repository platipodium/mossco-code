!> @brief Main routine for calling coupled systems
!> @file main.F90
!!
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Knut Klingbeil <knut.klingbeil@io-warnemuende.de>
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>
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
  type(ESMF_TimeInterval)    :: runDuration, timeStep
  integer                    :: localrc, rc,nmlunit=2013
  double precision           :: seconds
  character(len=40)          :: timestring, logKind='multi', name='main'
  character(len=40)          :: start='2000-01-01 00:00:00'
  character(len=40)          :: stop='2000-01-05 00:00:00'
  character(len=ESMF_MAXSTR) :: title='Untitled'
  type(ESMF_GridComp)        :: topComp
  type(ESMF_State)           :: topState ! for import and export, empty
  type(ESMF_Clock)           :: mainClock,topClock
  type(ESMF_VM)              :: vm
  integer(ESMF_KIND_I4)      :: localPet, petCount
  logical                    :: ClockIsPresent
  character(len=ESMF_MAXSTR) :: message, formatstring
  type(ESMF_LogMsg_Flag), allocatable :: logMsgList(:)
  type(ESMF_LogKind_Flag)    :: logKindFlag
  logical                    :: fileIsPresent, labelIsPresent
  type(ESMF_Config)          :: config
  character(len=ESMF_MAXSTR) :: configFileName='mossco.cfg'
  character(len=ESMF_MAXSTR) :: logLevel='all'
  logical                    :: logFlush=.false.

!> Read the namelist `mossco_run.nml`and evaluate five parameters:
!> 1. `start`: the start date of the simulation in YYYY-MM-DD hh:mm:ss format
!> 2. `stop` : the stop date of the simulation in the same format
!> 3. `title`: the title of the simulation.
!> 4. `logkind`: an ESMF LOGKIND, multi | single | none, default is multi
!> 5. `loglevel`: an ESMF LOGMSGFLAG, none | error | warning | all, default is all
!> 6. `logflush`: a logical, .false. | .true. , default is .true.
!>
!> If this file is not present, then the default simulation with title "Untitled"
!> will be executed for the time 2000-01-01 00:00:00 to 2000-01-05 00:00:00

  namelist /mossco_run/ title,start,stop,logkind,loglevel,logflush

  configfilename='mossco.cfg'
  inquire(file=trim(configfilename), exist=fileIsPresent)

  if (.not.fileIsPresent) then
    configfilename='mossco_run.nml'
    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (fileIsPresent) then
      open(nmlunit,file=trim(configfilename),status='old',action='read',iostat=localrc)
      if (localrc .eq. 0) then
        read(nmlunit,nml=mossco_run)
        close(nmlunit)
      end if
    endif
  endif

  !> @todo assess how to read the title before calling Initialize()
  ! substitute slash and space characters in title string
  call replace_character(title,'/','-')
  call replace_character(title,' ','_')

  !> Find out what kind of log to write, the default is MULTI
  if (logKind == 'none') then
    logKindFlag=ESMF_LOGKIND_NONE
  elseif (logKind == 'single') then
    logKindFlag=ESMF_LOGKIND_SINGLE
  else
    logKindFlag=ESMF_LOGKIND_MULTI
  endif

  ! Initialize ESMF, get resources, and log this to a file beginning with PET
  if (logKindFlag==ESMF_LOGKIND_SINGLE) then
    call ESMF_Initialize(defaultLogFileName='PET.'//trim(title), rc=localrc, &
      logkindflag=logKindFlag,defaultCalKind=ESMF_CALKIND_GREGORIAN, vm=vm)
  else
    call ESMF_Initialize(defaultLogFileName=trim(title), rc=localrc, &
      logkindflag=logKindFlag,defaultCalKind=ESMF_CALKIND_GREGORIAN, vm=vm)
  endif
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  config = ESMF_ConfigCreate(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  ! Read parameters from mossco.cfg (overriding those in the namelist, except for the
  ! logKindFlag, which cannot be set after ESMF_Initialize for the default log
  configfilename='mossco.cfg'
  inquire(file=trim(configfilename), exist=fileIsPresent)

  if (fileIsPresent) then

    write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ConfigFindLabel(config, label='title:', isPresent=labelIsPresent, rc = localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (labelIsPresent) then
      call ESMF_ConfigGetAttribute(config, title, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' title: '//trim(title)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_ConfigFindLabel(config, label='start:', isPresent=labelIsPresent, rc = localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (labelIsPresent) then
      call ESMF_ConfigGetAttribute(config, start, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' start: '//trim(start)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_ConfigFindLabel(config, label='stop:', isPresent=labelIsPresent, rc = localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (labelIsPresent) then
      call ESMF_ConfigGetAttribute(config, stop, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' stop: '//trim(stop)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_ConfigFindLabel(config, label='logflush:', isPresent=labelIsPresent, rc = localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (labelIsPresent) then
      call ESMF_ConfigGetAttribute(config, logflush, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A,L)')  trim(name)//' found in file '//trim(configFileName)//' logflush: ',logflush
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_ConfigFindLabel(config, label='loglevel:', isPresent=labelIsPresent, rc = localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (labelIsPresent) then
      call ESMF_ConfigGetAttribute(config, loglevel, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' loglevel: '//trim(loglevel)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif
  endif

  !> Find out what level of log to write, the default is all, overwrite previous decision

  if (logLevel == 'none') then
    allocate(logMsgList(1))
    logMsgList=(/ESMF_LOGMSG_NONE/)
  elseif (logLevel == 'error') then
    allocate(logMsgList(1))
    logMsgList=(/ESMF_LOGMSG_ERROR/)
  elseif (logLevel == 'warning') then
    allocate(logMsgList(2))
    logMsgList=(/ESMF_LOGMSG_ERROR, ESMF_LOGMSG_WARNING/)
  elseif (logLevel == 'info') then
    allocate(logMsgList(3))
    logMsgList=(/ESMF_LOGMSG_ERROR, ESMF_LOGMSG_WARNING, ESMF_LOGMSG_INFO/)
  elseif (logLevel == 'trace') then
    allocate(logMsgList(1))
    logMsgList=(/ESMF_LOGMSG_ERROR, ESMF_LOGMSG_WARNING, ESMF_LOGMSG_TRACE/)
  elseif (logLevel == 'default') then
    allocate(logMsgList(1))
    logMsgList=(/ESMF_LOGMSG_ERROR, ESMF_LOGMSG_WARNING, ESMF_LOGMSG_TRACE/)
  else
    allocate(logMsgList(4))
    logMsgList=(/ESMF_LOGMSG_ERROR, ESMF_LOGMSG_WARNING, ESMF_LOGMSG_TRACE, ESMF_LOGMSG_INFO/)
  endif

  call ESMF_LogSet(logMsgList=logMsgList, flush=logFlush, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (allocated(logMsgList)) deallocate(logMsgList)

  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  write(message,'(A)')  'MOSSCO '//trim(title)//" coupled system starts"
  if (localPet==0 .or. logKindFlag==ESMF_LOGKIND_MULTI) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  write(formatstring,'(A)') '(A,'//intformat(localPet)//',A,'//intformat(petCount)//')'
  write(message,formatstring) 'Creating multiple logs, this is processor ',localPet,' of ', petCount
  if (logKindFlag==ESMF_LOGKIND_MULTI) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

 ! Get the wall clock starting time
  call ESMF_TimeSet(time1, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeSyncToRealTime(time1,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeGet(time1,timeStringISOFrac=timestring, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  if (localPet==0 .or. logKindFlag==ESMF_LOGKIND_MULTI) write(message,'(A)')  "Program starts at wall clock "//trim(timestring)
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  ! Create and initialize a clock from mossco_run.nml
  call MOSSCO_TimeSet(startTime, start, localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call MOSSCO_TimeSet(stopTime, stop, localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  runDuration = stopTime - startTime

  mainClock = ESMF_ClockCreate(timeStep=runDuration, startTime=startTime, stopTime=stopTime, &
    name=trim(title), rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!> @todo feature request sent to ESMF for supporting clock attributes (not implemented)
!	call ESMF_AttributeSet(mainClock, 'creator', trim(name), rc=localrc)
!  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeGet(startTime,timeStringISOFrac=timestring, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  if (localPet==0 .or. logKindFlag==ESMF_LOGKIND_MULTI) call ESMF_LogWrite("Simulation starts at "//timestring, ESMF_LOGMSG_INFO)

  call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  if (localPet==0 .or. logKindFlag==ESMF_LOGKIND_MULTI) call ESMF_LogWrite("Simulation ends at "//timestring, ESMF_LOGMSG_INFO)

! Create toplevel component and call its setservices routines, if namelist was successfully read, then copy the
! main clock to the toplevel (child) clock.  If no time information from namelist, then let the toplevel component
! read the time and pass it back to main clock
  if (fileIsPresent) then
    topClock = ESMF_ClockCreate(mainClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockSet(topClock,name="toplevel", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    topComp = ESMF_GridCompCreate(name="toplevel",clock=topClock,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  else
    topComp = ESMF_GridCompCreate(name="toplevel", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  endif

  call ESMF_GridCompSet(topComp, config=config, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(topComp,SetServices,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  topState = ESMF_StateCreate(name="topState",rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !> Add all configuration options as attributes to state
  call ESMF_AttributeSet(topState, 'simulation_title', trim(title), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_AttributeSet(topState, 'simulation_start', trim(start), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_AttributeSet(topState, 'simulation_stop', trim(stop), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_AttributeSet(topState, 'simulation_log_kind', trim(logKind), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompGet(topComp,clockIsPresent=ClockIsPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(topComp,importState=topState,exportState=topState,clock=mainClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  if (.not.fileIsPresent) then
    call ESMF_GridCompGet(topComp,clockIsPresent=ClockIsPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (clockIsPresent) then
      call ESMF_GridCompGet(topComp,clock=topClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockGet(topClock,startTime=startTime, stopTime=stopTime,runDuration=runDuration, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockSet(mainClock,startTime=startTime, stopTime=stopTime,timeStep=runDuration, &
                  currTime=startTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
  end if


  !> @todo create simulation attribute package for CIM and write this to XML if XERCES is set, otherwise write
  !> tab-delimited info

  call ESMF_GridCompRun(topComp, importState=topState, exportState=topState, clock=mainClock, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  ! Destroy toplevel component and clean up
  call ESMF_GridCompFinalize(topComp, importState=topState, exportState=topState, clock=mainClock, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !> @todo  The following line was commented, as it produces a segfault
  call ESMF_GridCompDestroy(topComp,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("All ESMF components destroyed", ESMF_LOGMSG_INFO)

  call ESMF_TimeSet(time2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeSyncToRealTime(time2, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeGet(time2, timeStringISOFrac=timestring, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  timeStep=time2 - time1

  call ESMF_TimeIntervalGet(timeStep, s_r8=seconds, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  write(message,'(A,ES10.4,A)') trim(title)//' needed ',seconds,' seconds to run'
  if (localPet==0 .or. logKindFlag==ESMF_LOGKIND_MULTI) call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
  if (localPet==0 .or. logKindFlag==ESMF_LOGKIND_MULTI) &
    call ESMF_LogWrite('MOSSCO '//trim(title)//' finished at wall clock '//timestring,ESMF_LOGMSG_INFO)


  call ESMF_StateDestroy(topState,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_ClockDestroy(mainClock,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !call ESMF_ConfigDestroy(config,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_Finalize(rc=localrc,endflag=ESMF_END_NORMAL)

end program main
