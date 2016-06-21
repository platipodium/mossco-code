!> @brief Implementation of an ESMF component that delivers constant data fields
!
!> @import none
!> @export all variables that are located in a file read by this component
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015, 2016 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "constant_grid_component.F90"

module constant_grid_component

  use esmf
  use mossco_variable_types
  use mossco_field
  use mossco_state

  implicit none

  private
  public SetServices

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"
    InitializePhaseMap(2) = "IPDv00p1=2"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=ESMF_MAXSTR)     :: name, message, line
    type(ESMF_Alarm)      :: alarm
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: time
    type(ESMF_TimeInterval) :: timeInterval, alarmInterval

    integer(ESMF_KIND_I4) :: nexport,lbnd(3),ubnd(3),farray_shape(3)
    integer(ESMF_KIND_I4) :: i,j,k
    type(ESMF_Field), dimension(:), allocatable :: exportField
    type(ESMF_Grid)                             :: grid2, grid3
    type(ESMF_DistGrid)                         :: distgrid
    type(ESMF_ArraySpec)                        :: arrayspec2, arraySpec3
    real(ESMF_KIND_R8), pointer :: farrayPtr3(:,:,:), farrayPtr2(:,:)
    character(len=ESMF_MAXSTR)                  :: varname
    integer, parameter                          :: fileunit=21
    logical                                     :: file_readable=.true., clockIsPresent
    integer(ESMF_KIND_I4)                       :: start

    character(len=ESMF_MAXSTR)                  :: timeString, unitString
    type(ESMF_Time)                             :: currTime
    real(ESMF_KIND_R8)                          :: floatValue
    integer(ESMF_KIND_I4), dimension(2)  :: computationalUBound2, computationalLBound2
    integer(ESMF_KIND_I4), dimension(3)  :: computationalUBound3, computationalLBound3
    integer(ESMF_KIND_I4)                :: localDeCount2, localDeCount3

    rc = ESMF_SUCCESS

    !! Check whether there is already a clock (it might have been set
    !! with a prior ESMF_gridCompCreate() call.  If not, then create
    !! a local clock as a clone of the parent clock, and associate it
    !! with this component.  Finally, set the name of the local clock
    call ESMF_GridCompGet(gridComp, name=name, clockIsPresent=clockIsPresent, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (clockIsPresent) then
      call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    else
      clock = ESMF_ClockCreate(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompSet(gridComp, clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock, name=trim(name)//' clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Log the call to this function
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing phase 1 ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    allocate(variable_items)
    cur_item => variable_items
    cur_item%next => variable_items

    !> open constant_grid.dat
    !! @todo: read filename from configuration namelist/yaml
    open(fileunit,file=trim(name)//'.dat',iostat=rc, action='read', status='old')
    if (rc == 0) then
      write(message,'(A)') trim(name)//' reads from file '//trim(name)//'.dat'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    else
      open(fileunit,file='constant_grid.dat',iostat=rc, action='read', status='old')
      if (rc == 0) then
        write(message,'(A)') trim(name)//' reads from file constant_grid.dat'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      else
        open(fileunit,file='constant_grid_component.dat',iostat=rc, action='read', status='old')
        if (rc == 0) then
          write(message,'(A)') trim(name)//' reads from file constant_grid_component.dat.  This feature is deprecated.'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        else
          write(message,'(A)') trim(name)//' could not open file for reading.'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
          file_readable=.false.
        endif
      endif
    endif

    if (rc ==0 ) then
      do
        !> read constant_grid.dat line by line, maybe add rank later
        !! format of each line is:
        !!   some_standard_name  12.345
        read(fileunit,'(A)', iostat=rc) line
        if (rc /= 0) exit
        line=adjustl(line)
        if (len_trim(line)==0) cycle
        if (line(1:1)=='#' .or. line(1:1)=='%' .or. line(1:1)=='#') cycle

        read(line,*,iostat=rc) varname
        if (rc /= 0) cycle
        line=adjustl(line(len_trim(varname)+1:))
        if (len_trim(line)>0) then
          read(line,*,iostat=rc) floatValue
          if (rc /= 0) cycle
        else
          floatValue=0.0D0
        endif
        line=adjustl(line(len_trim(varname)+1:))
        if (len_trim(line)>0) then
          read(line,*,iostat=rc) unitString
          if (rc /= 0) cycle
        else
          unitString=''
        endif

        !> add item to list of constants
        allocate(cur_item%next)
        cur_item => cur_item%next
        cur_item%standard_name=trim(varname)
        cur_item%rank = 3
        cur_item%value = floatValue
        cur_item%units = unitString
        start=1
        do while (index(cur_item%standard_name(start:),'_at_')>0)
          cur_item%rank=cur_item%rank - 1
          start = index(cur_item%standard_name(start:),'_at_')+1
        enddo

        if ((cur_item%rank == 3 .and. localDeCount3>0) &
          .or.(cur_item%rank == 2 .and. localDeCount2>0)) then
          write(0,*) 'constant_grid_component: create field ', &
              trim(varname),' =',cur_item%value
          write(message,'(A,I1,A,ES9.2)') trim(name)//' created field '//trim(varname)// &
            ' rank(',cur_item%rank,'), value ',cur_item%value
          call ESMF_LogWrite(message,ESMF_LOGMSG_INFO)
        endif
        nullify(cur_item%next)
      end do
    close(fileunit)
    end if
!5   continue


    !> now go through list, create empty fields and add to exportState
    cur_item => variable_items%next
    if (file_readable) then
      do
        cur_item%field = ESMF_FieldEmptyCreate(name=cur_item%standard_name, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(cur_item%field, 'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (len_trim(cur_item%units)>0) then
          call ESMF_AttributeSet(cur_item%field,'units',trim(cur_item%units))
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif

        call ESMF_AttributeSet(cur_item%field,'rank',cur_item%rank)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call ESMF_AttributeSet(cur_item%field,'default_value',cur_item%value)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call ESMF_StateAddReplace(exportState,(/cur_item%field/),rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)


        if (associated(cur_item%next)) then
          cur_item => cur_item%next
        else
          exit
        end if
      end do
    endif

    write(message,'(A)') trim(timestring)//' '//trim(name)//' initialized phase 1.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=ESMF_MAXSTR)     :: name, message, line
    type(ESMF_Alarm)      :: alarm
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: time
    type(ESMF_TimeInterval) :: timeInterval, alarmInterval

    integer(ESMF_KIND_I4) :: nexport,lbnd(3),ubnd(3),farray_shape(3)
    integer(ESMF_KIND_I4) :: i,j,k
    type(ESMF_Field), dimension(:), allocatable :: exportField
    type(ESMF_Grid)                             :: grid2, grid3, grid
    type(ESMF_DistGrid)                         :: distgrid
    type(ESMF_ArraySpec)                        :: arrayspec2, arraySpec3
    real(ESMF_KIND_R8), pointer :: farrayPtr3(:,:,:), farrayPtr2(:,:)
    character(len=ESMF_MAXSTR)                  :: varname
    integer, parameter                          :: fileunit=21
    logical                                     :: file_readable=.true., clockIsPresent
    integer(ESMF_KIND_I4)                       :: start

    character(len=ESMF_MAXSTR)                  :: timeString, unitString
    type(ESMF_Time)                             :: currTime
    real(ESMF_KIND_R8)                          :: floatValue
    integer(ESMF_KIND_I4), dimension(2)  :: computationalUBound2, computationalLBound2
    integer(ESMF_KIND_I4), dimension(3)  :: computationalUBound3, computationalLBound3
    integer(ESMF_KIND_I4)                :: localDeCount2, localDeCount3

    character(ESMF_MAXSTR)               :: foreignGridFieldName, attributeName
    integer(ESMF_KIND_I4)                :: localDe, coordDim
    integer(ESMF_KIND_I4), allocatable   :: maxIndex(:)

    type(ESMF_FieldStatus_Flag)          :: fieldStatus
    type(ESMF_StateItem_Flag)            :: itemType
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    character(ESMF_MAXSTR)               :: itemName
    character(ESMF_MAXSTR), allocatable  :: itemNameList(:)

    integer(ESMF_KIND_I4)                :: fieldRank, itemCount, rank
    integer                              :: intValue
    real(ESMF_KIND_R8)                   :: real8value
    type(ESMF_Field)                     :: field

    rc = ESMF_SUCCESS

    !! Log the call to this function
    call ESMF_GridCompGet(gridComp, name=name, clock=clock, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
           value=foreignGridFieldName, defaultValue='none',rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (trim(foreignGridFieldName)=='none') then
      write(message,'(A)') 'This routine needs a foreignGrid.'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, rc=rc)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field, grid=grid, rank=rank, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (rank<2 .or. rank>3) then
      write(message,'(A)') 'Foreign grid must be of rank = 2 or 3'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    allocate(maxIndex(rank))
    call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, localDE=0, &
        computationalCount=maxIndex, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (rank==3) then
      grid3=ESMF_GridCreate(grid, name="constant_grid_3d", rc=rc)
	    grid2=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/maxIndex(1),maxIndex(2)/), &
         regDecomp=(/1,1/), coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL,  &
           name="constant_grid_2d", coordTypeKind=ESMF_TYPEKIND_R8, coordDep1=(/1/), &
           coorddep2=(/2/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_GridAddCoord(grid2, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_AttributeSet(grid2,'creator',trim(name))
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    else
      grid2=ESMF_GridCreate(grid, name="constant_grid_2d", rc=rc)
	    grid3=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), maxIndex=(/maxIndex(1),maxIndex(2),1/), &
         regDecomp=(/1,1,1/), coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL,  &
           name="constant_grid_3d", coordTypeKind=ESMF_TYPEKIND_R8, coordDep1=(/1/), &
          coorddep2=(/2/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_GridAddCoord(grid3, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_AttributeSet(grid3,'creator',trim(name))
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    deallocate(maxIndex)

    call ESMF_GridGet(grid3, localDeCount=localDeCount3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_GridGet(grid2, localDeCount=localDeCount3, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (localDeCount3 /= localDeCount2) then
        write(message,'(A)') 'LocalDeCount must be equal between 2D and 3D grid'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    do localDe=0,localDeCount3-1

      do coordDim=1,2
        call ESMF_GridGetCoord(grid3, coordDim=coordDim, localDE=localDe, farrayPtr=farrayPtr3, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        call ESMF_GridGetCoord(grid2, coordDim=coordDim, localDE=localDe, farrayPtr=farrayPtr2, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        if (rank==3) then ! Copy coords from 3d to 2d Grid (take first index)
				   farrayPtr2(:,:)=farrayPtr3(:,:,1)
        else
           farrayPtr3(:,:,1)=farrayPtr2(:,:)
        endif
      enddo
    enddo

    call ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
    allocate(itemTypeList(itemCount))
    allocate(itemNameList(itemCount))

    do i=1,itemCount
      if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) cycle

      call ESMF_StateGet(exportState, itemNameList(i), field, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field, status=fieldStatus, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

			call ESMF_AttributeGet(field, name='rank',value=fieldRank, defaultValue=1, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      if (fieldRank==1) then
        write(message,'(A)') 'Rank attribute is missing in field, must be 2 or 3.'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      endif

			call ESMF_AttributeGet(field, name='default_value', value=intValue, defaultValue=-99, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

			if (fieldStatus==ESMF_FIELDSTATUS_EMPTY) then
			  if (rank==2) then
				  call ESMF_FieldEmptySet(field, grid2, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
        else
				  call ESMF_FieldEmptySet(field, grid3, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
        endif
      endif

      call ESMF_FieldGet(field, status=fieldStatus, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

			if (fieldStatus==ESMF_FIELDSTATUS_GRIDSET) then
			  call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
			endif

      call ESMF_FieldGet(field, status=fieldStatus, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

			if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
        write(message,'(A)') 'Field is not complete.'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      endif

      if (rank==2) then
        call ESMF_FieldGet(field, farrayPtr=farrayPtr2, rc=rc)
			  farrayPtr2(:,:) = real(intValue,kind=ESMF_KIND_R8)
      else
        call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=rc)
			  farrayPtr3(:,:,:) = real(intValue,kind=ESMF_KIND_R8)
      endif

    enddo
    !! Finally, log the successful completion of this function
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initialized'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

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

    !> @todo implement this routine

  end subroutine ReadRestart

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)     :: gridComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock
    integer, intent(out)    :: rc

    integer(ESMF_KIND_I8)   :: advanceCount
    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    type(ESMF_TimeInterval) :: timeInterval

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (.not.clockIsPresent) then
      clock = parentClock
    else
      call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    call ESMF_ClockGet(clock,currTime=currTime, advanceCount=advanceCount, &
      timeStep=timeInterval, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,I8)') trim(timestring)//' '//trim(name)//' running step ',advanceCount
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    !> Here comes your own run code
    !! In particular, this should contain
    !! 1. Checking for fields in your import State and mapping the values/pointers
    !!    in these import fields to your model's internal data.  Be aware that
    !!    oftentimes the import state you get here is an export from an entirely different
    !!    ESMF component.  In particular, you cannot rely on your import state to be
    !!    the same as your Initialize() routines import state.

    if (clockIsPresent) then
      do while (.not. ESMF_ClockIsStopTime(clock, rc=rc))

      !! Your own code continued:
      !! 2. Calling a single (or even multiple) internal of your model

        call ESMF_ClockAdvance(clock, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      enddo
    endif

    !! 3. You should not have to do anything with the export state, because the mapping
    !!    between your internal model's data and the exported fields has already been
    !!    done in the Initialize() routine.  In MOSSCO, this is recommended practices, but
    !!    don't rely on this.

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finished running.'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=rc)

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

    !> Obtain information on the component, especially whether there is a local
    !! clock to obtain the time from and to later destroy
    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (.not.clockIsPresent) then
      clock=parentClock
    else
      call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    !> Get the time and log it
    call ESMF_ClockGet(clock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' finalizing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finalized'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE)

  end subroutine Finalize

end module constant_grid_component
