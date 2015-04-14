!> @brief Implementation of an ESMF netcdf output component
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "netcdf_component.F90"

module netcdf_component

  use esmf
  use mossco_variable_types
  use mossco_netcdf
  use mossco_strings
  use mossco_component
  use mossco_field
  use mossco_state

  implicit none
  private

  type(type_mossco_netcdf)   :: nc !> @todo should this be an array?

  public :: SetServices

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer              :: localrc

    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: name, configFileName, fileName, message
    character(len=ESMF_MAXSTR) :: filterPatternExclude
    type(ESMF_Time)            :: currTime
    integer(ESMF_KIND_I4)      :: localrc, j, n
    logical                    :: isPresent, fileIsPresent, labelIsPresent, configIsPresent
    type(ESMF_Config)          :: config
    character(len=ESMF_MAXSTR), allocatable :: includePatternList(:), excludePatternList(:)
    character(len=4096)        :: excludeAttributeString, includeAttributeString

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Check whether there is a config file with the same name as this component
    !! If yes, load it.
    call ESMF_GridCompGet(gridComp, configIsPresent=configIsPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (configIsPresent) then
      call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    configfilename=trim(name)//'.cfg'
    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (fileIsPresent) then

      write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      !> @todo deal with already existing config
      config = ESMF_ConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigFindLabel(config, label='filename:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(fileName,'(A)') trim(name)
      if (labelIsPresent) then
        call ESMF_ConfigGetAttribute(config, fileName, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' filename: '//trim(fileName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call ESMF_AttributeSet(importState, 'filename', trim(fileName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigFindLabel(config, label='exclude:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      excludeAttributeString=''
      if (labelIsPresent) then
        n=ESMF_ConfigGetLen(config, label='exclude:', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (n>0) allocate(excludePatternList(n))

        call ESMF_ConfigFindLabel(config, label='exclude:', rc = localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do j=1,n
          call ESMF_ConfigGetAttribute(config, value=excludePatternList(j), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         ! if (len_trim(excludePatternList(j))<1) cycle

          call MOSSCO_CleanPattern(excludePatternList(j), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' exclude: '//trim(excludePatternList(j))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          if (len_trim(excludeAttributeString)<1) then
            write(excludeAttributeString,'(A)') trim(excludePatternList(j))
          else
            write(excludeAttributeString,'(A)') trim(excludeAttributeString)//', '//trim(excludePatternList(j))
          endif
        enddo
        if (allocated(excludePatternList)) deallocate(excludePatternList)

        call ESMF_AttributeSet(importState, 'filter_pattern_exclude', trim(excludeAttributeString), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_ConfigFindLabel(config, label='include:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      includeAttributeString=''
      if (labelIsPresent) then
        n=ESMF_ConfigGetLen(config, label='include:', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,I2.2,A)')  trim(name)//' found ',n,' include patterns'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        if (n>0) allocate(includePatternList(n))

        call ESMF_ConfigFindLabel(config, label='include:', rc = localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do j=1,n
          call ESMF_ConfigGetAttribute(config, value=includePatternList(j), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !call MOSSCO_CleanPattern(includePatternList(j), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' include: '//trim(includePatternList(j))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          if (len_trim(includeAttributeString)<1) then
            write(includeAttributeString,'(A)') trim(includePatternList(j))
          else
            write(includeAttributeString,'(A)') trim(includeAttributeString)//', '//trim(includePatternList(j))
          endif
        enddo
        if (allocated(includePatternList)) deallocate(includePatternList)
        call ESMF_AttributeSet(importState, 'filter_pattern_include', trim(includeAttributeString), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    endif

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    rc=ESMF_SUCCESS

    !> Here omes your restart code, which in the simplest case copies
    !> values from all fields in importState to those in exportState

  end subroutine ReadRestart


#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: currTime, currentTime, ringTime, time, refTime,startTime, stopTime
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I8)   :: i, j, n, advanceCount
    real(ESMF_KIND_R8)      :: seconds
    integer(ESMF_KIND_I4)   :: itemCount, timeSlice, localPet, fieldCount, ii, petCount
    integer(ESMF_KIND_I4)   :: localDeCount, localrc
    integer(ESMF_KIND_I4), dimension(:), allocatable :: totalUBound, totalLBound
    type(ESMF_StateItem_Flag), allocatable, dimension(:) :: itemTypeList
    type(ESMF_Field)        :: field
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    type(ESMF_Array)        :: array
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_ArrayBundle)  :: arrayBundle
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: itemNameList
    character(len=ESMF_MAXSTR) :: fieldName
    character(len=3)        :: numberstring
    type(ESMF_Clock)        :: clock
    logical                 :: clockIsPresent, isMatch, isPresent
    character(len=ESMF_MAXSTR) :: form

    character(len=ESMF_MAXSTR) :: message, fileName, name, numString, timeUnit
    type(ESMF_FileStatus_Flag) :: fileStatus=ESMF_FILESTATUS_REPLACE
    type(ESMF_IOFmt_Flag)      :: ioFmt
    character(len=ESMF_MAXSTR) :: filterPatternExclude
    character(len=ESMF_MAXSTR), allocatable :: includePatternList(:), excludePatternList(:)
    character(len=4096)        :: excludeAttributeString, includeAttributeString

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, petCount=petCount, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, refTime=refTime, startTime=startTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, name='filename', value=fileName, &
      defaultValue=trim(name)//'.nc', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    j=index(fileName,'.nc',back=.true.)
    if (j < len_trim(fileName)-2) fileName=trim(fileName)//'.nc'

    if (petCount>1) then
      write(form,'(A)')  '(A,'//trim(intformat(int(petCount-1,kind=8)))//',A)'
      write(fileName,form) filename(1:index(filename,'.nc')-1)//'.',localPet,'.nc'
    endif

    call ESMF_AttributeGet(importState, name='filter_pattern_exclude', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(importState, name='filter_pattern_exclude', value=excludeAttributeString, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (advanceCount<1) &
        call ESMF_LogWrite(trim(name)//' found exclude pattern '//trim(excludeAttributeString), ESMF_LOGMSG_INFO)

      !if (len_trim(excludeAttributeString)<1) exit
      n=1
      do i=1,len_trim(excludeAttributeString)
        if (excludeAttributeString(i:i)==',') n=n+1
      enddo

      !write(message,'(A,I1,A)') trim(name)//' filter_pattern_exclude(1:',n,') = "'//trim(excludeAttributeString)//'"'
      !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      allocate(excludePatternList(n))
      do i=1,n
        j=index(excludeAttributeString,',')
        if (j>0) then
          excludePatternList(i)=excludeAttributeString(1:j-1)
        else
          excludePatternList(i)=trim(excludeAttributeString)
        endif
        write(excludeAttributeString,'(A)') excludeAttributeString(j+2:len_trim(excludeAttributeString))
        write(message,'(A,I1,A)') trim(name)//' filter_pattern_exclude(',i,') = "'//trim(excludePatternList(i))//'"'
        if (advanceCount<1) &
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo

    endif

    call ESMF_AttributeGet(importState, name='filter_pattern_include', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(importState, name='filter_pattern_include', value=includeAttributeString, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (advanceCount < 1) &
        call ESMF_LogWrite(trim(name)//' found include pattern '//trim(includeAttributeString), ESMF_LOGMSG_INFO)

      n=1
      do i=1,len_trim(includeAttributeString)
        if (includeAttributeString(i:i)==',') n=n+1
      enddo

      allocate(includePatternList(n))
      do i=1,n
        j=index(includeAttributeString,',')
        if (j>0) then
          includePatternList(i)=trim(adjustl(includeAttributeString(1:j-1)))
        else
          includePatternList(i)=trim(adjustl(includeAttributeString))
        endif
        write(includeAttributeString,'(A)') includeAttributeString(j+2:len_trim(includeAttributeString))
        write(message,'(A,I1,A)') trim(name)//' filter_pattern_include(',i,') = "'//trim(includePatternList(i))//'"'
        if (advanceCount < 1) &
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo
    endif

    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (advanceCount<huge(timeSlice)) then
      timeSlice=int(advanceCount, ESMF_KIND_I4)
    else
      write(message,'(A)') 'Cannot use this advanceCount for a netcdf timeSlice, failed to convert long int to int'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (itemcount>0) then
      if (.not.allocated(itemTypeList)) allocate(itemTypeList(itemCount))
      if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))

      call ESMF_StateGet(importState, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_TimeGet(refTime, timeStringISOFrac=timeString, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      write(timeUnit,'(A)') 'seconds since '//timeString(1:10)//' '//timestring(12:len_trim(timestring))

      call ESMF_TimeIntervalGet(currTime-refTime, s_r8=seconds, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (currTime == startTime) then
        nc = mossco_netcdfCreate(fileName, timeUnit=timeUnit, rc=localrc)
      else
        nc = mossco_netcdfOpen(fileName, timeUnit=timeUnit, rc=localrc)
      end if

      call nc%update()
      call nc%update_variables()

      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call nc%add_timestep(seconds, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call nc%update()
      call nc%update_variables()

      do i=1,itemCount

        if (allocated(excludePatternList)) then
          isMatch = .false.
          do j=1, ubound(excludePatternList,1)
            call MOSSCO_StringMatch(trim(itemNameList(i)), trim(excludePatternList(j)), isMatch, localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            if (isMatch .and. advanceCount < 1) then
              write(message,'(A)') trim(name)//' excluded '//trim(itemNameList(i))//' from exclude pattern '//trim(excludePatternList(j))
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
              exit
            endif
          enddo
          if (isMatch) cycle
        endif

        if (allocated(includePatternList)) then
          isMatch = .false.
          do j=1, ubound(includePatternList,1)
            call MOSSCO_StringMatch(trim(itemNameList(i)), trim(includePatternList(j)), isMatch, localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            if (isMatch .and. advanceCount < 1) then
              write(message,'(A)') trim(name)//' included '//trim(itemNameList(i))
              call MOSSCO_MESSAGEAdd(message,' from include pattern '//trim(includePatternList(j)))
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
              exit
            endif
          enddo
          if (.not.isMatch) cycle
        endif

        if (advanceCount < 1) then
          write(message,'(A)') trim(name)//' will write'
          call MOSSCO_MessageAdd(message,' '//itemNameList(i))
          call MOSSCO_MessageAdd(message,' to file ')
          call MOSSCO_MessageAdd(message,' '//fileName)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif

        if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(importState, trim(itemNameList(i)), field, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_FieldGet(field, localDeCount=localDeCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (localDeCount>0) call nc%put_variable(field)

        elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
          call ESMF_StateGet(importState, trim(itemNameList(i)), fieldBundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_FieldBundleGet(fieldBundle,fieldCount=fieldCount,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          allocate(fieldList(fieldCount))
          call ESMF_FieldBundleGet(fieldBundle,fieldList=fieldList,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !! go through list of fields and put fields into netcdf using field name and number
          do ii=1,size(fieldList)
            call ESMF_FieldGet(fieldList(ii),name=fieldName,rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call MOSSCO_StringMatch(trim(itemNameList(i)), filterPatternExclude, isMatch, localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            if (isMatch) cycle

            write(numberstring,'(I0.3)') ii

            call ESMF_FieldGet(fieldList(ii), localDeCount=localDeCount)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            if (localDeCount>0)call nc%put_variable(fieldList(ii),name=trim(fieldName)//'_'//numberstring)

          end do
          deallocate(fieldList)
        elseif (advanceCount < 1) then
          write(message,'(A)') trim(name)//' does not save item '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        endif
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !> Remove from import state the written field
        call ESMF_StateRemove(importState, (/ trim(itemNameList(i))/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      enddo

      if (allocated(itemTypeList)) deallocate(itemTypeList)
      if (allocated(itemNameList)) deallocate(itemNameList)

      call nc%close()
    endif

    if (allocated(includePatternList)) deallocate(includePatternList)
    if (allocated(excludePatternList)) deallocate(excludePatternList)

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (stopTime>currTime) then
      call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    integer(ESMF_KIND_I4)   :: localrc

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module netcdf_component
