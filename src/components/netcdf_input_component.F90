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
#define ESMF_FILENAME "netcdf_input_component.F90"

module netcdf_input_component

  use esmf
  use mossco_variable_types
  use mossco_netcdf
  use mossco_field
  use mossco_strings
  use mossco_component
  use mossco_state
  use mossco_time
  use mossco_grid

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

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
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

    character(len=10)           :: InitializePhaseMap(2)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"
    InitializePhaseMap(2) = "IPDv00p2=2"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0


  !> Initialize the component
  !!
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: timestring, message, name, fileName
    character(len=ESMF_MAXSTR) :: foreignGridFieldName, form
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timeInterval, timeStep
    integer(ESMF_KIND_I4)      :: petCount, localPet, localRc
    integer(ESMF_KIND_I8)      :: advanceCount
    type(ESMF_Clock)           :: clock

    logical                    :: isPresent, fileIsPresent, labelIsPresent, hasGrid
    type(ESMF_Grid)            :: grid2, grid3, grid, varGrid
    type(ESMF_Field)           :: field
    character(len=ESMF_MAXSTR) :: configFileName, timeUnit, itemName, petFileName, gridName
    type(ESMF_Config)          :: config

    integer(ESMF_KIND_I4)      :: itemCount, i, j, timeid, itime, udimid, n
    integer(ESMF_KIND_I4)      :: fieldRank, gridRank
    type(ESMF_Time)            :: refTime, time
    real(ESMF_KIND_R8)         :: seconds
    type(ESMF_Field), allocatable :: fieldList(:)
    integer(ESMF_KIND_I4), allocatable    :: ungriddedUbnd(:), ungriddedLbnd(:)
    character(len=ESMF_MAXSTR), allocatable :: aliasList(:,:)
    character(len=4096)        :: aliasString

    rc = ESMF_SUCCESS
    hasGrid = .false.

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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

      if (labelIsPresent) then
        call ESMF_ConfigGetAttribute(config, fileName, rc=localrc, default=trim(name))
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)')  trim(name)//' found in file '
        call MOSSCO_MessageAdd(message,trim(configFileName)//' filename: '//trim(fileName))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call ESMF_AttributeGet(importState, 'filename', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isPresent) then
        call ESMF_AttributeGet(importState, 'filename', value=fileName, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      elseif (labelIsPresent) then
        call ESMF_AttributeSet(importState, 'filename', trim(fileName), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_ConfigFindLabel(config, label='grid:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (labelIsPresent) then
        call ESMF_ConfigGetAttribute(config, foreignGridFieldName, rc=localrc, default='none')
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' grid: '//trim(fileName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call ESMF_AttributeGet(importState, 'foreign_grid_field_name', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (isPresent) then
        call ESMF_AttributeGet(importState, 'foreign_grid_field_name', value=foreignGridFieldName, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      elseif (labelIsPresent) then
        call ESMF_AttributeSet(importState, 'foreign_grid_field_name', trim(foreignGridFieldName), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_ConfigFindLabel(config, label='alias:', isPresent=labelIsPresent, rc = localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (labelIsPresent) then
        n=ESMF_ConfigGetLen(config, label='alias:', rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        aliasString=''

        if (n>0 .and. mod(n,2) == 0) then

          allocate(aliasList(n/2,2))

          call ESMF_ConfigFindLabel(config, label='alias:', rc = localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          do j=1, n/2
            call ESMF_ConfigGetAttribute(config, value=aliasList(j,1), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            call ESMF_ConfigGetAttribute(config, value=aliasList(j,2), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
              call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            write(message,'(A)')  trim(name)//' found in file '//trim(configFileName)//' alias: '//trim(aliasList(j,1))
            call MOSSCO_MessageAdd(message,' = '//trim(aliasList(j,2)))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

            if (len_trim(aliasString)<1) then
              write(aliasString,'(A)') trim(aliasList(j,1))//'='//trim(aliasList(j,2))
            else
              write(aliasString,'(A)') ', '//trim(aliasList(j,1))//'='//trim(aliasList(j,2))
            endif
          enddo
          if (allocated(aliasList)) deallocate(aliasList)

          call ESMF_AttributeSet(importState, 'alias_list', trim(aliasString), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      endif

      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_AttributeGet(importState, 'filename', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      write(message,'(A)') trim(name)//' received no filename to read from'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      call MOSSCO_CompExit(gridComp)
      return
    endif

    call ESMF_AttributeGet(importState, 'filename', value=fileName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ! For multiprocessor applications, try to read cpu-specific input first, default
    ! back to overall file
    if (petCount>0) then
      write(form,'(A)')  '(A,'//trim(intformat(int(petCount-1,kind=8)))//',A)'
      write(petFileName,trim(form)) filename(1:len_trim(filename)-2),localPet,'.nc'
      inquire(file=trim(petFileName), exist=isPresent)
      if (isPresent) fileName=trim(petFileName)
    endif

    inquire(file=trim(fileName), exist=isPresent)

    if (.not.isPresent) then
      write(message,'(A)') trim(name)//' cannot read file '//trim(fileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)
    endif

    write(message,'(A)')  trim(name)//' reading file '//trim(fileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_GridCompGet(gridComp, gridIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      write(message,'(A)') trim(name)//' found grid in component'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)

      call ESMF_GridCompGet(gridComp, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)
      hasGrid=.true.
    endif

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
      isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
        value=foreignGridFieldName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (trim(foreignGridFieldName) /= 'none') then
        call ESMF_StateGet(importState,  trim(foreignGridFieldName), field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message, '(A)') trim(name)//' obtains grid from field'
        call MOSSCO_FieldString(field,message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        call ESMF_FieldGet(field, grid=grid, rank=fieldRank, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        hasGrid=.true.
      endif
    else
      write(message,'(A)') trim(name)//' not implemented without foreign_grid'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      !rc = ESMF_RC_NOT_IMPL
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (.not.hasGrid) then
      write(message,'(A)') trim(name)//' not implemented without grid or foreign_grid'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      !rc = ESMF_RC_NOT_IMPL
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_GridGet(grid, rank=gridRank, name=gridName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Check for ungridded dimensions
    if (fieldRank>gridRank) then
      allocate(ungriddedUbnd(fieldRank-gridRank))
      allocate(ungriddedLbnd(fieldRank-gridRank))
      call ESMF_FieldGet(field, ungriddedLBound=ungriddedLbnd, ungriddedUbound=ungriddedUbnd, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (gridRank == 2) then
      grid2 = grid
      grid3 = MOSSCO_GridCreateFromOtherGrid(grid2, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    elseif (gridRank == 3) then
      grid3 = grid
      grid2 = MOSSCO_GridCreateFromOtherGrid(grid3, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      write(message,'(A)') trim(name)//' cannot use grid with rank<2 or >3 for foreign_grid'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    nc = MOSSCO_NetcdfOpen(trim(fileName), mode='r', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call nc%update_variables()
    call nc%update()
    itemCount = nc%nvars

    ! Get time information from time variable if present and log the time span available
    itime=1
    timeid=0
    do i=1, itemCount
      if (trim(nc%variables(i)%name) == 'time' .or. trim(nc%variables(i)%standard_name) == 'time') then
        timeid=i
        write(message,'(A)')  trim(name)//' found time variable'
        write(message,'(A,I3,A,I1,A)') trim(message)//' ', &
          nc%variables(i)%varid,' rank ',nc%variables(i)%rank,' units='//trim(nc%variables(i)%units)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        exit
      endif
    enddo

    if (timeid>0) then
      call ESMF_ClockGet(clock, currTime=currTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      timeunit=trim(nc%variables(timeid)%units)
      udimid=nc%variables(timeid)%dimids(1)
      i=index(timeunit,'since ')
      if (i>0) then
        call MOSSCO_TimeSet(refTime, timeunit(i+6:len_trim(timeunit)), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        timeunit=timeunit(1:i-1)
      endif

      if (trim(timeUnit) == 'seconds') then
        call ESMF_TimeIntervalGet(currTime-refTime, s_r8=seconds, rc=rc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        write(message,'(A)')  trim(name)//' not implemented: unit for time is '//trim(timeUnit)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        seconds=0.0
      endif

      ! todo find time index (default is one)
      ! allocate(time(nc%variables(timid)%dimlens(1)))
      ! localrc=nf90_var_get(nc%ncid, timeid, time)
    else
      udimid=-1
    endif

    if (itemCount>0) allocate(fieldList(itemCount))

    do i=1, itemCount
      if (trim(nc%variables(i)%name) == 'time') cycle
      if (trim(nc%variables(i)%name) == 'lat') cycle
      if (trim(nc%variables(i)%name) == 'lon') cycle
      write(message,'(A)') trim(name)//' found item "'
      call MOSSCO_MessageAdd(message, trim(nc%variables(i)%standard_name)//'"')
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      if (nc%variables(i)%rank < 2) then
        write(message,'(A)') trim(name)//' reading of rank < 2 variables not implemented'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        cycle
      endif

      write(message,'(A,I3,A,I1,A)') trim(name)//' id = ', &
         nc%variables(i)%varid,', rank = ',nc%variables(i)%rank,' units = "'//trim(nc%variables(i)%units)//'"'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      itemName=trim(nc%variables(i)%standard_name)
      if (len_trim(itemName)<1) itemName=trim(nc%variables(i)%standard_name)
      if (len_trim(itemName)<1) cycle

      fieldList(i) = ESMF_FieldEmptyCreate(name=trim(itemName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !call nc%gridget(varGrid, nc%variables(i), localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (hasGrid) then

        !! Make sure varRank>=fieldRank>=gridRank

        if (any(nc%variables(i)%dimids==udimid)) then
          if (fieldRank /= nc%variables(i)%rank-1) then
            write(message,'(A,I1)') trim(name)//' mismatch from'
            call MOSSCO_MessageAdd(message,' '//trim(nc%name)//'::'//trim(nc%variables(i)%name))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            write(message,'(A,I1,A)') '  rank ',nc%variables(i)%rank-1,' /= field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)

            cycle
            !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif
        else
          if (fieldRank /= nc%variables(i)%rank) then
            write(message,'(A,I1)') trim(name)//' mismatch from'
            call MOSSCO_MessageAdd(message,' '//trim(nc%name)//'::'//trim(nc%variables(i)%name))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            write(message,'(A,I1,A)') '  rank ',nc%variables(i)%rank-1,' /= field '
            call MOSSCO_FieldString(field, message)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)

            cycle
            !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif
        endif

        !> @todo: test if varGrid conforms to grid
        if (gridRank==2) then
          call ESMF_FieldEmptySet(fieldList(i), grid2, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
        elseif (gridRank==3) then
          call ESMF_FieldEmptySet(fieldList(i), grid3, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
        else
          write(message,'(A)') trim(name)//' not implemented with gridrank <2 or >3'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          rc = ESMF_RC_NOT_IMPL
        endif
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        write(message,'(A)') trim(name)//' not implemented without foreign grid'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_NOT_IMPL
        return
      endif

      if (fieldRank>gridRank) then
        call ESMF_FieldEmptyComplete(fieldList(i), typekind=ESMF_TYPEKIND_R8, ungriddedLBound= &
          ungriddedLbnd, ungriddedUbound=ungriddedUbnd, rc=localrc)
      else
        call ESMF_FieldEmptyComplete(fieldList(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      endif
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call nc%getvar(fieldList(i), nc%variables(i), itime=itime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeGet(fieldList(i), 'creator', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (.not.isPresent) then
        call ESMF_AttributeSet(fieldList(i), 'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      write(message, '(A)') trim(name)//' created field'
      call MOSSCO_FieldString(fieldList(i), message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAdd(exportState, (/fieldList(i)/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    enddo

    if (allocated(ungriddedUbnd)) deallocate(ungriddedUbnd)
    if (allocated(ungriddedLbnd)) deallocate(ungriddedLbnd)

    if (allocated(fieldList)) deallocate(fieldList)

    call nc%close(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp)
    return

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: timestring, message, name, fileName
    character(len=ESMF_MAXSTR) :: foreignGridFieldName, form
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timeInterval, timeStep
    integer(ESMF_KIND_I4)      :: petCount, localPet, localRc
    integer(ESMF_KIND_I8)      :: advanceCount
    type(ESMF_Clock)           :: clock

    logical                    :: isPresent, fileIsPresent, labelIsPresent, hasGrid
    type(ESMF_Grid)            :: grid2, grid3, varGrid, importGrid, exportGrid
    type(ESMF_Field)           :: field, importField, exportField
    character(len=ESMF_MAXSTR) :: configFileName, timeUnit, itemName, petFileName
    type(ESMF_Config)          :: config

    integer(ESMF_KIND_I4)      :: itemCount, i, j, timeid, itime, udimid, gridRank, rank
    type(ESMF_Time)            :: refTime, time
    real(ESMF_KIND_R8)         :: seconds
    type(ESMF_Field), allocatable :: fieldList(:)
    type(ESMF_StateItem_Flag)  :: itemType

    rc = ESMF_SUCCESS
    hasGrid = .false.

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#if 0
    call ESMF_AttributeGet(importState, 'filename', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      write(message,'(A)') trim(name)//' received no filename to read from'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      call MOSSCO_CompExit(gridComp)
      return
    endif

    call ESMF_AttributeGet(importState, 'filename', value=fileName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    nc = MOSSCO_NetcdfOpen(trim(fileName), mode='r', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call nc%update_variables()
    call nc%update()
    itemCount = nc%nvars

    ! Get time information from time variable if present and log the time span available
    itime=1
    timeid=0
    do i=1, itemCount
      if (trim(nc%variables(i)%name) == 'time' .or. trim(nc%variables(i)%standard_name) == 'time') then
        timeid=i
        write(message,'(A)')  trim(name)//' found time variable'
        write(message,'(A,I3,A,I1,A)') trim(message)//' ', &
          nc%variables(i)%varid,' rank ',nc%variables(i)%rank,' units='//trim(nc%variables(i)%units)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        exit
      endif
    enddo

    if (timeid>0) then
      call ESMF_ClockGet(clock, currTime=currTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      timeunit=trim(nc%variables(timeid)%units)
      udimid=nc%variables(timeid)%dimids(1)
      i=index(timeunit,'since ')
      if (i>0) then
        call MOSSCO_TimeSet(refTime, timeunit(i+6:len_trim(timeunit)), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        timeunit=timeunit(1:i-1)
      endif

      if (trim(timeUnit) == 'seconds') then
        call ESMF_TimeIntervalGet(currTime-refTime, s_r8=seconds, rc=rc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        write(message,'(A)')  trim(name)//' not implemented: unit for time is '//trim(timeUnit)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        seconds=0.0
      endif

      ! todo find time index (default is one)
      ! allocate(time(nc%variables(timid)%dimlens(1)))
      ! localrc=nf90_var_get(nc%ncid, timeid, time)
    else
      udimid=-1
    endif

    if (itemCount>0) allocate(fieldList(itemCount))

    do i=1, itemCount

      itemName=trim(nc%variables(i)%standard_name)
      call ESMF_StateGet(exportState, itemName, itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemType /= ESMF_STATEITEM_FIELD) cycle

      call ESMF_StateGet(importState, itemName, importField, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateGet(exportState, itemName, exportField, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

			write(message, '(A)') trim(name)//' contains import field'
			call MOSSCO_FieldString(importField, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

			write(message, '(A)') trim(name)//' contains export field'
			call MOSSCO_FieldString(importField, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_FieldEmptySet(fieldList(i), grid3, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)

      call ESMF_FieldEmptyComplete(fieldList(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call nc%getvar(fieldList(i), nc%variables(i), itime=itime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeGet(fieldList(i), 'creator', isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (.not.isPresent) then
        call ESMF_AttributeSet(fieldList(i), 'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      !call ESMF_StateAdd(exportState, (/fieldList(i)/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    if (allocated(fieldList)) deallocate(fieldList)

    call nc%close(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#endif

    call MOSSCO_CompExit(gridComp)
    return

  end subroutine InitializeP2

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
    type(ESMF_TimeInterval) :: timeStep
    integer(ESMF_KIND_I8)   :: i, j, advanceCount
    real(ESMF_KIND_R8)      :: seconds
    integer(ESMF_KIND_I4)   :: itemCount, timeSlice, localPet, fieldCount, ii, petCount
    integer(ESMF_KIND_I4)   :: localDeCount, n
    type(ESMF_StateItem_Flag), allocatable, dimension(:) :: itemTypeList
    type(ESMF_Field)        :: field
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: itemNameList
    character(len=ESMF_MAXSTR) :: fieldName, fileName
    type(ESMF_Clock)        :: clock
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(type_mossco_netcdf_variable), pointer    :: var => null()

    character(len=ESMF_MAXSTR) :: message, name
    integer(ESMF_KIND_I4)      :: localrc, itime
    logical                    :: isPresent
    character(len=ESMF_MAXSTR), allocatable :: aliasList(:,:)
    character(len=4096)        :: aliasString

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, 'filename', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
      write(message,'(A)') trim(name)//' received no filename to read from'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      call MOSSCO_CompExit(gridComp)
      return
    endif

    call ESMF_AttributeGet(importState, 'filename', value=fileName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    nc = MOSSCO_NetcdfOpen(trim(fileName), mode='r', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call nc%update_variables()
    call nc%update()

    if (nc%nvars==0) then
      write(message,'(A)') trim(name)//' contains no variables to read.'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      call MOSSCO_CompExit(gridComp)
      return
    endif

    call ESMF_StateGet(exportState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      if (.not.allocated(itemTypeList)) allocate(itemTypeList(itemCount))
      if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))

      call ESMF_StateGet(exportState, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call nc%timeIndex(currTime, itime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, name='alias_string', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(importState, name='aliasString', value=aliasString, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (advanceCount<1) &
        call ESMF_LogWrite(trim(name)//' found aliasses '//trim(aliasString), ESMF_LOGMSG_INFO)

      n=1
      do i=1,len_trim(aliasString)
        if (aliasString(i:i)==',') n=n+1
      enddo

      if (n>0) allocate(aliasList(n,2))
      do i=1,n
        j=index(aliasString,'=')
        aliasList(i,1)=aliasString(1:j-1)

        write(aliasString,'(A)') aliasString(j+1:len_trim(aliasString))
        j=index(aliasString,',')
        if (j>0) then
          aliasList(i,2)=aliasString(1:j-1)
        else
          aliasList(i,2)=trim(aliasString)
        endif

        write(message,'(A,I1,A)') trim(name)//' alias(',i,') = "'//trim(aliasList(i,1))
        call MOSSCO_MessageAdd(message, '='//trim(aliasList(i,2))//'"')
        if (advanceCount<1) &
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo

    endif

    !> Go through list of export variables and fill their pointers with values from the file
    do i=1, itemCount

      if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) cycle

      call ESMF_StateGet(exportState, trim(itemNameList(i)), field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) cycle

      call ESMF_FieldGet(field, localDeCount=localDeCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (localDeCount < 1) cycle

      if (allocated(aliasList)) then
        do j=1,ubound(aliasList,1)
          if (trim(itemNameList(i)) == trim(aliasList(j,2))) itemNameList(i)=trim(aliasList(j,2))
        enddo
      endif

      var => nc%getvarvar(trim(itemNameList(i)))
      if (.not.associated(var)) cycle


      call nc%getvar(field, var, itime=int(itime, kind=ESMF_KIND_I4), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    enddo

    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)

    call nc%close()

    !! This component has no do loop over an internal timestep, it is advanced with the
    !! timestep written into its local clock from a parent component
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock, currTime=currTime, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    timeStep=stopTime-currTime
    if (stopTime>currTime) then
      call ESMF_ClockAdvance(clock, timeStep=timeStep, rc=localrc)
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

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module netcdf_input_component
