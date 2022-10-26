!> @brief Main routine for calling coupled systems
!> @file main.F90
!!
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2021-2022 Helmholtz-Zentrum Hereon
!> @copyright Copyright (C) 2013-2021 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Knut Klingbeil <knut.klingbeil@io-warnemuende.de>
!> @author Richard Hofmeister
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

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
  double precision           :: seconds, runseconds
  character(len=40)          :: timestring, name='main'
  character(len=40)          :: start='2000-01-01 00:00:00'
  character(len=40)          :: stop='2000-01-05 00:00:00'
  character(len=ESMF_MAXSTR) :: title='Untitled'
  type(ESMF_GridComp)        :: topComp
  type(ESMF_State)           :: topState ! for import and export, empty
  type(ESMF_Clock)           :: mainClock,topClock
  type(ESMF_VM)              :: vm
  integer(ESMF_KIND_I4)      :: localPet, petCount,argc,i
  logical                    :: ClockIsPresent
  character(len=ESMF_MAXSTR) :: message, formatstring, pidString, logKind
  type(ESMF_LogMsg_Flag), allocatable :: logMsgList(:)
  type(ESMF_LogKind_Flag)    :: logKindFlag
  logical                    :: fileIsPresent, labelIsPresent
  type(ESMF_Config)          :: config
  character(len=ESMF_MAXSTR) :: configFileName
  character(len=ESMF_MAXSTR) :: logLevel='all'
  character(len=ESMF_MAXSTR) :: logLevelZero='not_given'
  logical                    :: logFlush=.false.

  integer(ESMF_KIND_I8)      :: system_clock_start, system_clock_stop, system_clock_max
  integer(ESMF_KIND_I8)      :: system_clock_rate, system_clock_duration
  character(len=ESMF_MAXSTR), allocatable :: configFileNameList(:), argValueList(:)

   type(ESMF_Info)           :: info

!> Read the namelist `mossco_run.nml`and evaluate five parameters:
!> 1. `start`: the start date of the simulation in YYYY-MM-DD hh:mm:ss format
!> 2. `stop` : the stop date of the simulation in the same format
!> 3. `title`: the title of the simulation.
!> 5. `loglevel`: an ESMF LOGMSGFLAG, none | error | warning | all | one  default is all
!> 6. `logflush`: a logical, .false. | .true. , default is .true.
!> 5. `loglevelzero`: an ESMF LOGMSGFLAG for the first PET
!>
!> If this file is not present, then the default simulation with title "Untitled"
!> will be executed for the time 2000-01-01 00:00:00 to 2000-01-05 00:00:00

  !> @todo logKind is deprecated, to be removed from nml
  namelist /mossco_run/ title,start,stop,logkind,loglevel,logflush,loglevelzero

  !> Predefine possible config file names, if one is given
  !> on the command line as an argument, then this will be
  !> inserted in position 1
  allocate(configFileNameList(4), stat=localrc)
  configFileNameList(1:2) = 'mossco.cfg'
  configFileNameList(3)   = 'mossco.nml'
  configFileNameList(4  ) = 'mossco_run.nml'

  !> Parse command line arguments
  argc = command_argument_count()
  allocate(argValueList(argc + 1), stat=localrc)
  do i = 0, argc
    call get_command_argument(i, argValueList(i+1))
    !write(0,'(A,I1.1,A,I1.1,A)') 'Command argument ',i,' of ',argc, &
    !  ' is "'//trim(argValueList(i+1))//'"'
  enddo

  !> Positional second argument could be the name of a config file
  if (argc > 0) then
    configFileNameList(1)=trim(argValueList(2))
    inquire(file=configFileNameList(1), exist=fileIsPresent)
    if (.not.fileIsPresent) then
      write(0, '(A)') 'Fatal error. Could not find '//trim(configFileNameList(1))
      stop
    endif
  endif

  !> Try finding config file names and parse them for the title
  !> of the simulation, needed for initializing ESMF

  do i=1, ubound(configFileNameList,1)
    configFileName = configfilenameList(i)
    inquire(file=configFileName, exist=fileIsPresent)
    if (fileIsPresent) exit
  enddo

  !write(0,'(4(A,X))') (trim(configFileNameList(i)), i=1,4)

  title = adjustl(trim(argValueList(1)))

  !> .nml can be read before ESMF is Initialized
  if (fileIsPresent .and. index(configFileName, '.nml') > 1) then

    open(nmlunit,file=trim(configfilename), status='old', action='read', iostat=localrc)
    if (localrc /= ESMF_SUCCESS) then
      write(0, '(A)') 'Fatal problem reading namelist from '//trim(configFileName)
      stop
    endif

    read(nmlunit, nml=mossco_run, iostat=localrc)
    close(nmlunit)
    if (localrc /= ESMF_SUCCESS) then
      write(0, '(A)') 'Fatal problem reading namelist from '//trim(configFileName)
      stop
    endif
  endif

  !> Get the process id for tagging the PET log
#ifndef NO_ISO_FORTRAN_ENV
  write(pidString,'(I20)') getpid()
#endif

  !> @todo assess how to read the title before calling Initialize()
  ! substitute slash and space characters in title string
  call replace_character(title,'/','-')
  call replace_character(title,' ','_')
  !write(title,'(A,A)') trim(title), '-'//trim(adjustl(pidString))

  !> Find out what kind of log to write, the default is MULTI
  logKindFlag=ESMF_LOGKIND_MULTI

  ! Initialize ESMF, get resources, and log this to a file beginning with PET
  call ESMF_Initialize(defaultLogFileName=trim(title), rc=localrc, &
    defaultCalKind=ESMF_CALKIND_GREGORIAN )
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  config = ESMF_ConfigCreate(rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  do i=1, ubound(argValueList,1)
    write(message,'(A,I1.1,A)') 'MOSSCO command line argument ',i-1, &
      ' is "'//trim(argValueList(i))//'"'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
  enddo

  if (.not. fileIsPresent) then
    write(message,'(A)') 'No configuration file was provided or could be read'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  !> Read the remaining parameters from the ESMF resource file
  ! or read ESMF resource files ending in .rc or .cfg
  elseif (index(configFileName, '.cfg') > 1 .or. &
    index(configFileName, '.rc') > 1) then

    write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ConfigFindLabel(config, label='title:', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (labelIsPresent) then
      call ESMF_ConfigGetAttribute(config, title, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' title: '//trim(title)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_ConfigFindLabel(config, label='start:', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (labelIsPresent) then
      call ESMF_ConfigGetAttribute(config, start, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' start: '//trim(start)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_ConfigFindLabel(config, label='stop:', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (labelIsPresent) then
      call ESMF_ConfigGetAttribute(config, stop, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' stop: '//trim(stop)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_ConfigFindLabel(config, label='logflush:', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (labelIsPresent) then
      call ESMF_ConfigGetAttribute(config, logflush, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A,L)')  trim(name)//' found in file '//trim(configFileName)//' logflush: ',logflush
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_ConfigFindLabel(config, label='loglevel:', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (labelIsPresent) then
      call ESMF_ConfigGetAttribute(config, loglevel, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' loglevel: '//trim(loglevel)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_ConfigFindLabel(config, label='loglevelzero:', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (labelIsPresent) then
      call ESMF_ConfigGetAttribute(config, loglevelzero, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' loglevelzero: '//trim(loglevelzero)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif
  else
    write(message,'(A)') 'Read namelist mossco_run from '//trim(configFileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  !> Read the remaining parameters from the ESMF resource file
  endif

  deallocate(argValueList, stat=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  deallocate(configFileNameList, stat=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_VMGetGlobal(vm, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  if (trim(logLevelZero) == 'not_given') logLevelZero = logLevel
  if (localPet == 0) logLevel = trim(logLevelzero)
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
    allocate(logMsgList(3))
    logMsgList=(/ESMF_LOGMSG_ERROR, ESMF_LOGMSG_WARNING, ESMF_LOGMSG_TRACE/)
  elseif (logLevel == 'default') then
    allocate(logMsgList(3))
    logMsgList=(/ESMF_LOGMSG_ERROR, ESMF_LOGMSG_WARNING, ESMF_LOGMSG_TRACE/)
  else
    allocate(logMsgList(4))
    logMsgList=(/ESMF_LOGMSG_ERROR, ESMF_LOGMSG_WARNING, ESMF_LOGMSG_TRACE, ESMF_LOGMSG_INFO/)
  endif

  call ESMF_LogSet(logMsgList=logMsgList, flush=logFlush, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  if (allocated(logMsgList)) deallocate(logMsgList, stat=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  write(message,'(A)')  'MOSSCO '//trim(title)//" coupled system starts"
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  write(formatstring,'(A)') '(A,'//intformat(petCount)//',A,'//intformat(petCount)//')'
  write(message,formatstring) 'MOSSCO PET ',localPet,' of ', petCount
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  ! Initialize the system clock (which measures CPU time)
  call system_clock(count_rate=system_clock_rate)
  call system_clock(count_max=system_clock_max)
  call system_clock(system_clock_start)

  ! Get the wall clock starting time
  call ESMF_TimeSet(time1, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_TimeSyncToRealTime(time1,rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_TimeGet(time1,timeStringISOFrac=timestring, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  write(message,'(A)')  'MOSSCO starts at wall clock '//trim(timestring)
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  ! Create and initialize a clock from mossco_run.nml
  call MOSSCO_TimeSet(startTime, start, localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call MOSSCO_TimeSet(stopTime, stop, localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  if (startTime >= stopTime) then
    call ESMF_TimeGet(startTime, timeStringISOFrac=timestring, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    write(message,'(A)') trim(timestring)//' cannot be greater/equal to '

    call ESMF_TimeGet(stopTime, timeStringISOFrac=timestring, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    write(message,'(A)') trim(message)//' '//trim(timestring)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    localrc = ESMF_RC_ARG_BAD
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  endif

  runDuration = stopTime - startTime

  mainClock = ESMF_ClockCreate(timeStep=runDuration, startTime=startTime, &
    stopTime=stopTime, name=trim(title), rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

!> @todo feature request sent to ESMF for supporting clock attributes (not implemented)
!	call ESMF_AttributeSet(mainClock, 'creator', trim(name), rc=localrc)
!  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeGet(startTime,timeStringISOFrac=timestring, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_LogWrite("Simulation starts at "//timestring, ESMF_LOGMSG_INFO)

  call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_LogWrite("Simulation ends at "//timestring, ESMF_LOGMSG_INFO)

  write(formatString,'(A)') '(A,'//intformat(digits(1.0_ESMF_KIND_R8))//',A)'
  write(message,formatString) 'ESMF double precision (KIND_R8) has ', &
    digits(1.0_ESMF_KIND_R8),' significant digits'
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  write(formatString,'(A)') '(A,'//intformat(digits(1.0_ESMF_KIND_R4))//',A)'
  write(message,formatString) 'ESMF single precision (KIND_R4) has ', &
    digits(1.0_ESMF_KIND_R4),' significant digits'
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  write(formatString,'(A,I3.3,A,I3.3,A)') '(A,'//intformat(digits(1_ESMF_KIND_I8))//',A,I', &
    ceiling(log10(2.0**digits(1_ESMF_KIND_I8))),'.', &
    ceiling(log10(2.0**digits(1_ESMF_KIND_I8))),').'

  write(message,formatString) 'ESMF long (KIND_I8) has ', &
    digits(1_ESMF_KIND_I8),'  digits, largest is ',huge(1_ESMF_KIND_I8)
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  write(formatString,'(A,I3.3,A,I3.3,A)') '(A,'//intformat(digits(1_ESMF_KIND_I4))//',A,I', &
    ceiling(log10(2.0**digits(1_ESMF_KIND_I4))),'.', &
    ceiling(log10(2.0**digits(1_ESMF_KIND_I4))),').'
  write(message,formatString) 'ESMF short (KIND_I4) has ', &
    digits(1_ESMF_KIND_I4),' digits, largest is ',huge(1_ESMF_KIND_I4)
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

! Create toplevel component and call its setservices routines, if namelist was successfully read, then copy the
! main clock to the toplevel (child) clock.  If no time information from namelist, then let the toplevel component
! read the time and pass it back to main clock
  if (fileIsPresent) then
    topClock = ESMF_ClockCreate(mainClock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockSet(topClock, name="toplevel", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    topComp = ESMF_GridCompCreate(name="toplevel", clock=topClock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  else
    topComp = ESMF_GridCompCreate(name="toplevel", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  endif

  call ESMF_GridCompSet(topComp, config=config, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_GridCompSetServices(topComp,SetServices,rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  topState = ESMF_StateCreate(name="topState",rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  !> Add all configuration options as attributes to state
  call ESMF_InfoGetFromHost(topComp, info, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_InfoSet(info, 'simulation_title', trim(title), rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_InfoSet(info, 'simulation_start', trim(start), rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_InfoSet(info, 'simulation_stop', trim(stop), rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_GridCompGet(topComp,clockIsPresent=ClockIsPresent, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_GridCompInitialize(topComp,importState=topState,exportState=topState,clock=mainClock, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  if (.not.fileIsPresent) then
    call ESMF_GridCompGet(topComp,clockIsPresent=ClockIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (clockIsPresent) then
      call ESMF_GridCompGet(topComp,clock=topClock, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ClockGet(topClock,startTime=startTime, stopTime=stopTime,runDuration=runDuration, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ClockSet(mainClock,startTime=startTime, stopTime=stopTime,timeStep=runDuration, &
        currTime=startTime, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif
  end if

  !> @todo create simulation attribute package for CIM and write this to XML if XERCES is set, otherwise write
  !> tab-delimited info

  call ESMF_VmBarrier(vm, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_GridCompRun(topComp, importState=topState, exportState=topState, clock=mainClock, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_VmBarrier(vm, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  ! Destroy toplevel component and clean up
  call ESMF_GridCompFinalize(topComp, importState=topState, exportState=topState, clock=mainClock, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_VmBarrier(vm, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  !> @todo  The following line was commented, as it produces a segfault
  call ESMF_GridCompDestroy(topComp,rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_LogWrite("All ESMF components destroyed", ESMF_LOGMSG_INFO)

  call system_clock(system_clock_stop)

  call ESMF_TimeSet(time2, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_TimeSyncToRealTime(time2, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_TimeGet(time2, timeStringISOFrac=timestring, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  timeStep=time2 - time1

  call ESMF_TimeIntervalGet(timeStep, s_r8=seconds, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  write(message,'(A,ES10.3,A)') trim(title)//' needed ',seconds,' seconds to run'
  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

  call ESMF_TimeIntervalGet(runduration, s_r8=runseconds, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  if (seconds > 0) then
    write(message,'(A,ES10.3,A)') trim(title)//' total speedup is ',runseconds/seconds
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    write(message,'(A,ES10.3,A)') trim(title)//' speedup per CPU is ',runseconds/seconds/petCount
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
  endif

  system_clock_duration = (system_clock_stop - system_clock_start) / system_clock_rate
  write(message,'(A,ES10.3,A)') trim(title)//' CPU time ',dble(system_clock_duration),' seconds'

  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
  call ESMF_LogWrite('MOSSCO '//trim(title)//' finished at wall clock '//timestring,ESMF_LOGMSG_INFO)

  call ESMF_StateDestroy(topState,rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_ClockDestroy(mainClock,rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  !call ESMF_ConfigDestroy(config,rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_Finalize(rc=localrc,endflag=ESMF_END_NORMAL)

end program main
