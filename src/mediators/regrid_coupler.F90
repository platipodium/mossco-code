!> @brief Implementation of an ESMF coupling between different grids
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2017 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "regrid_coupler.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module regrid_coupler

  use esmf
  use mossco_state
  use mossco_field
  use mossco_component
  use mossco_config

  implicit none

  private

  public SetServices

  type type_mossco_fields_handle
    type(ESMF_RouteHandle) :: routehandle
    type(ESMF_Field) :: srcField, dstField ! should these be pointers?
    type(ESMF_State) :: srcState, dstState ! should these be pointers?
    type(type_mossco_fields_handle), pointer :: next=>null()
  end type

  class(type_mossco_fields_handle), allocatable, target :: fieldsHandle

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(cplcomp, rc)

    type(ESMF_CplComp)   :: cplcomp
    integer, intent(out) :: rc

    integer :: localrc

    rc = ESMF_SUCCESS

    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "Initialize"
  subroutine Initialize(cplcomp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentclock
    integer, intent(out) :: rc

    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: petCount, localPet, i, itemCount
    character(len=ESMF_MAXSTR)  :: message, name, timeString, exportName, importName
    character(len=ESMF_MAXSTR)  :: exportFieldName, importFieldName
    type(ESMF_RouteHandle)      :: routeHandle
    class(type_mossco_fields_handle), pointer :: currHandle=>null()
    type(ESMF_Field)            :: importField, exportField
    integer                     :: localrc

    integer(ESMF_KIND_I4)       :: rank, localDeCount
    type(ESMF_FieldStatus_Flag) :: status
    type(ESMF_Mesh)             :: mesh
    type(ESMF_Grid)             :: grid, externalGrid
    type(ESMF_LocStream)        :: locstream
    type(ESMF_GeomType_Flag)    :: geomType
    character(ESMF_MAXSTR)      :: geomName, gridFileName
    integer                     :: numOwnedNodes, dimCount
    integer(ESMF_KIND_I4)       :: keycount, matchIndex, importFieldCount
    integer(ESMF_KIND_I4)       :: exportFieldCount
    logical                     :: gridIsPresent, isPresent

    type(ESMF_Field), allocatable :: importFieldList(:)
    type(ESMF_Field), allocatable :: exportFieldList(:)

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Read (optionally) the associated config file and configure
    !> external target grid, include and exclude patterns
    call read_config(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeGet(cplComp, 'grid_filename',  &
      isPresent=gridIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> The get_FieldList call returns a list of ESMF_COMPLETE fields in a
    !>  state, including lists that previously were located within
    !> fieldBundles; this subroutine also considers exclusion/inclusion
    !> patterns defined in the config file
    call get_FieldList(cplComp, importState, importFieldList, verbose=.true., &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    importFieldCount = 0
    if (allocated(importFieldList)) then
      importFieldCount = ubound(importFieldList,1)
    endif

    if (importFieldCount < 1) then
      write(message,'(A)') trim(name)//' no couplable items in '//trim(importName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    endif

    if (gridIsPresent) then

      call ESMF_AttributeGet(cplComp, 'grid_filename',  gridFileName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> @todo how to deal with decomposition?
      !> First try SCRIP format
      externalGrid = ESMF_GridCreate(filename=trim(gridFileName), &
        fileFormat=ESMF_FILEFORMAT_SCRIP, isSphere=.false., rc=localrc)

      if (localrc /= ESMF_SUCCESS) then
        externalGrid = ESMF_GridCreate(filename=trim(gridFileName), fileFormat=ESMF_FILEFORMAT_GRIDSPEC, &
          isSphere=.false., rc=localrc)
      endif

      if (localrc /= ESMF_SUCCESS) then
        write(message, '(A)') trim(name)//' could not read file '//trim(gridFileName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif


    if (gridIsPresent) then
      !> @todo create with this grid all states in exportState
      allocate(exportFieldList(importFieldCount))

      do i=1, importFieldCount

        call MOSSCO_FieldInFieldsHandle(importFieldList(i), fieldsHandle, &
          isPresent=isPresent, handle=currHandle, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (isPresent) then
          exportFieldList(i) = currHandle%dstfield
          cycle
        endif

        importField = importFieldList(i)

        call ESMF_FieldGet(importField, name=importFieldName, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        exportField = ESMF_FieldEmptyCreate(name=trim(importFieldName), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldEmptySet(exportField, grid=externalGrid, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_StateAddReplace(exportState, (/exportField/), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call MOSSCO_FieldCopy(exportField, importField, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AttributeSet(exportField, 'creator', trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message, '(A)') trim(name)//' created '
        call MOSSCO_FieldString(exportField, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        exportFieldList(i) = exportField
      enddo
    else
      call get_FieldList(cplComp, exportState, exportFieldList, verbose=.true., rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    if (allocated(exportFieldList)) exportFieldCount = ubound(exportFieldList,1)

    !do i=1,exportFieldCount
    !  write(message, '(A)') trim(name)//' export item  '
    !  call MOSSCO_FieldString(exportFieldList(i), message)
    !  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    !enddo

    call ESMF_StateGet(exportState, name=exportName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateGet(importState, name=importName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Search for all fields that are present in both import and export state,
    !! for each combination of fields
    !! - if they are defined on different grids, create a route handle and
    !!   name it with the name of the two grids for identification (todo)

    do i=1, importFieldCount

      importField = importFieldList(i)
      call ESMF_FieldGet(importField, name=importFieldName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' trying to match '
      call MOSSCO_FieldString(importField, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      !> look for matching field in exportState, this matching is performed
      !> on the field name and a maximum of the attributes
      call MOSSCO_FieldMatchFields(importFieldList(i), exportFieldList, &
        index=matchIndex, owner=trim(name), verbose=.true., rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (matchIndex<1) cycle

      exportField = exportFieldList(matchIndex)

      write(message,'(A)') trim(name)//' matched  '
      call MOSSCO_FieldString(exportField, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_FieldGet(exportField, name=exportFieldName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (importField == exportField) then
        !> @todo add a zero-action routehandle for those fields that area
        !> already on the proper export grid
        write(message,'(A)') trim(name)//' skipped  '//trim(importFieldName) &
          //' (already the same in '//trim(exportName)//')'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      ! grid match to see fields are on the same grid

      call ESMF_FieldGet(importField, localDeCount=localDeCount, rank=rank, &
        geomType=geomType, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (geomType == ESMF_GEOMTYPE_GRID) then

          call ESMF_FieldGet(importField, grid=grid, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_GridGet(grid, dimCount=dimCount, rank=rank, name=geomName, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A,I1,A,I1,A,I1)') trim(name)//' grid '//trim(geomName)//' rank ',rank,' dimensions ',dimCount

      elseif (geomType == ESMF_GEOMTYPE_MESH) then

          call ESMF_FieldGet(importField, mesh=mesh, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_MeshGet(mesh, numOwnedNodes=numOwnedNodes, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A,I5,A)') trim(name)//' mesh with ',numOwnedNodes,' nodes'

      elseif (geomType == ESMF_GEOMTYPE_LOCSTREAM) then

          call ESMF_FieldGet(importField, locstream=locstream, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_LocStreamGet(locstream, keycount=keycount, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A,I5,A)') trim(name)//' locstream with ',keycount,' keycount'

      else

          write(message,'(A)') trim(name)//' other geomtype, skipped'
          cycle
      endif

      call ESMF_FieldGet(exportField, status=status, localDeCount=localDeCount, rank=rank, &
        geomType=geomType, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (geomType == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(exportField, grid=grid, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_GridGet(grid, dimCount=dimCount, rank=rank, name=geomName, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A,I1,A,I1,A,I1)') trim(message)//' --> grid '//trim(geomName)//' rank ',rank,' dimensions ',dimCount

      elseif (geomType == ESMF_GEOMTYPE_MESH) then

        call ESMF_FieldGet(exportField, mesh=mesh, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_MeshGet(mesh, numOwnedNodes=numOwnedNodes, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A,I5,A)') trim(message)//' --> mesh with ',numOwnedNodes,' nodes.'

      elseif (geomType == ESMF_GEOMTYPE_LOCSTREAM) then

          call ESMF_FieldGet(exportField, locstream=locstream, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_LocStreamGet(locstream, keycount=keycount, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A,I5,A)') trim(name)//' locstream with ',keycount,' keys'

      else
        write(message,'(A)') trim(name)//' --> other geomtype skipped'
        cycle
      endif

      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      ! look to see if this field pair already has a routehandle
      !! search for the correct routeHandle
      if (.not.associated(currHandle)) then
        allocate(fieldsHandle)
      endif

      currHandle=>fieldsHandle
      do while (associated(currHandle%next))
        if (.not.((currHandle%srcField==importField).and.(currHandle%dstField==exportField))) &
          currHandle=>currHandle%next
      enddo
      ! this field pair has not already been "handled"
      if (.not. associated(currHandle%next)) then

        write(message,'(A)') trim(name)//' field '//trim(importFieldName) &
          //' creating routeHandle'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        !> @todo this call is problematic and throws an error
        !call ESMF_FieldRegridStore(srcField=importField, dstField=exportField,&
        !  routeHandle=routehandle, regridmethod=ESMF_REGRIDMETHOD_BILINEAR, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message,'(A)') trim(name)//' field '//trim(importFieldName) &
          //' created routeHandle'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          !! ESMF_FieldRegrid.F90:2018 ESMF_FieldRegridGetIwts Invalid argument
          !! - - can't currently regrid a grid       that contains a DE of width less than 2

        allocate (currHandle%next)
        currHandle=>currHandle%next

        currHandle%srcField=importField
        currHandle%dstField=exportField
        currHandle%srcState=importState
        currHandle%dstState=exportState
        currHandle%routeHandle=routeHandle

      ! this field pair has already been "handled", continue!
      else
        write(message,'(A)') trim(name)//' field '//trim(importFieldName) &
          //' has already been handled, skipping.'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

        ! generic list cycling code..
        ! do while(associated(currHandle%next))
        !   currHandle=>currHandle%next
        ! enddo

    enddo

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Initialize

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(cplcomp, importState, exportState, parentclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentclock
    integer, intent(out) :: rc

    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: petCount, localPet, i, itemCount
    character(len=ESMF_MAXSTR)  :: message, name, timeString, exportName, importName
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMf_StateItem_Flag)   :: itemType
    class(type_mossco_fields_handle), pointer :: currHandle=>null()
    type(ESMF_Field)            :: importField, exportField
    type(ESMF_RouteHandle)      :: routeHandle
    integer :: localrc

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateGet(exportState, name=exportName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateGet(importState, itemCount=itemCount, name=importName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (itemCount>0) then
      allocate(itemNameList(itemCount))
      allocate(itemTypeList(itemCount))

      call ESMF_StateGet(importState, itemNameList=itemNameList, &
        itemTypeList=itemTypeList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    else
      write(message,'(A)') trim(name)//' no couplable fields in '//trim(importName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    endif

    do i=1,itemCount
      if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) then
        write(message,'(A)') trim(name)//' skipped non-field item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      call ESMF_StateGet(exportState, itemName=itemNameList(i), itemType=itemType, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (itemType==ESMF_STATEITEM_NOTFOUND) then
        write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
          //' (not in '//trim(exportName)//')'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      elseif (itemType/=ESMF_STATEITEM_FIELD) then
        write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
          //' (not a field in '//trim(exportName)//')'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      call ESMF_StateGet(importState, itemNameList(i), importField, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_StateGet(exportState, itemNameList(i), exportField, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (importField==exportField) then
        write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
          //' (already the same in '//trim(exportName)//')'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      !! search for the correct routeHandle
      currHandle=>fieldsHandle
      do while (associated(currHandle%next))
        if (.not.((currHandle%srcField==importField).and.(currHandle%dstField==exportField))) &
          currHandle=>currHandle%next
      enddo
      routeHandle=currHandle%routeHandle

      call ESMF_FieldRegrid(srcField=importField, dstField=exportField,&
        routeHandle=routehandle, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      !! ESMF_FieldRegrid.F90:2018 ESMF_FieldRegridGetIwts Invalid argument
      !! - - can't currently regrid a grid       that contains a DE of width less than 2

    enddo

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc
    integer :: localrc

    class(type_mossco_fields_handle), pointer :: currHandle=>null()

    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (allocated(fieldsHandle)) then
      currHandle=>fieldsHandle
      do while (associated(currHandle%next))
        currHandle=>currHandle%next
        deallocate(fieldsHandle)
      enddo
    endif

    if (clockIsPresent) call ESMF_ClockDestroy(clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "get_FieldList"

  subroutine get_FieldList(cplComp, state, fieldList, kwe, verbose, rc)

    implicit none

    type(ESMF_CplComp), intent(inout)  :: cplComp
    type(ESMF_State), intent(inout)    :: state
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional                    :: verbose
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)             :: rc_, localrc, fieldcount
    logical                           :: configIsPresent, configFileIsPresent
    type(ESMF_Config)                 :: config
    character(len=ESMF_MAXSTR), pointer :: filterExcludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer :: filterIncludeList(:) => null()
    logical                           :: verbose_

    rc_ = ESMF_SUCCESS
    verbose_ = .false.

    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    if (present(verbose)) verbose_ = verbose

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_exclude', filterExcludeList, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_include', filterIncludeList, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (allocated (fieldList)) deallocate(fieldList)

    call MOSSCO_StateGet(state, fieldList, fieldCount=fieldCount, &
        fieldStatus=ESMF_FIELDSTATUS_COMPLETE, include=filterIncludeList, &
        exclude=filterExcludeList, verbose=verbose_, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine get_FieldList

#undef  ESMF_METHOD
#define ESMF_METHOD "read_config"

  subroutine read_config(cplComp, kwe, rc)

    implicit none

    type(ESMF_CplComp), intent(inout)    :: cplComp
    type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)             :: rc_, localRc
    character(len=ESMF_MAXSTR)        :: configFileName, srcGridFileName, message
    character(len=ESMF_MAXSTR)        :: cplCompName, dstGridFileName
    logical                           :: labelIsPresent, fileIsPresent
    logical                           :: configIsPresent, configFileIsPresent
    type(ESMF_Config)                 :: config
    character(len=ESMF_MAXSTR), pointer :: filterExcludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer :: filterIncludeList(:) => null()

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_CplCompGet(cplComp, configIsPresent=configIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (configIsPresent) then
      call ESMF_CplCompGet(cplComp, configIsPresent=configIsPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    else
      config = ESMF_ConfigCreate(rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_CplCompSet(cplComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call ESMF_CplCompGet(cplComp, configFileIsPresent=configFileIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_CplCompGet(cplComp, name=cplCompName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (configFileIsPresent) then
      call ESMF_CplCompGet(cplComp, configFile=configFileName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    else
      configFileName=trim(cplCompName)//'.cfg'
    endif

    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (.not. fileIsPresent) return

    write(message,'(A)')  trim(cplCompName)//' reads configuration from '//trim(configFileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='grid', value=dstGridFileName, &
      defaultValue=trim(cplCompName)//'_grid.nc', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A)') trim(cplCompName)// ' found config item grid = '//trim(dstGridFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(cplComp, 'grid_filename', trim(dstGridFileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    !> Find out whether the label was specified.  If yes, then require
    !> the file to be present, and return if not found
    inquire(file=trim(dstGridFileName), exist=fileIsPresent)

    if (labelIsPresent .and..not. fileIsPresent) then
      write(message, '(A)') trim(cplCompName)//' cannot find '//trim(dstGridFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      if (present(rc)) then
        rc = ESMF_RC_NOT_FOUND
        return
      else
        localrc = ESMF_RC_FILE_OPEN
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      endif
    endif

    call MOSSCO_ConfigGet(config, 'exclude', filterExcludeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (associated(filterExcludeList)) then
      call MOSSCO_AttributeSet(cplComp, 'filter_pattern_exclude', filterExcludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      deallocate(filterExcludeList)

      call MOSSCO_AttributeGet(cplComp, 'filter_pattern_exclude', filterExcludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(cplCompName)//' uses exclude patterns:'
      call MOSSCO_MessageAddListPtr(message, filterExcludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    endif

    call MOSSCO_ConfigGet(config, 'include', filterIncludeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (associated(filterIncludeList)) then
      call MOSSCO_AttributeSet(cplComp, 'filter_pattern_include', filterIncludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      deallocate(filterIncludeList)

      call MOSSCO_AttributeGet(cplComp, 'filter_pattern_include', filterIncludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)') trim(cplCompName)//' uses include patterns:'
      call MOSSCO_MessageAddListPtr(message, filterIncludeList, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

  end subroutine read_config

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldInFieldsHandle"
  subroutine MOSSCO_FieldInFieldsHandle(field, fieldsHandle, kwe, isPresent, &
    handle, rc)

    type(ESMF_Field), intent(in)                      :: field
    class(type_mossco_fields_handle), intent(in), allocatable, target  :: fieldsHandle
    type(ESMF_KeyWordEnforcer), intent(in), optional  :: kwe
    logical, intent(out), optional                    :: isPresent
    type(type_mossco_fields_handle), pointer, optional, intent(out) :: handle
    integer(ESMF_KIND_I4), intent(out), optional      :: rc

    integer(ESMF_KIND_I4)             :: rc_, localrc
    logical                           :: isPresent_
    type(type_mossco_fields_handle), pointer :: currHandle=>null()
    character(len=ESMF_MAXSTR)               :: message

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    if (present(handle)) handle => null()

    if (.not.allocated(fieldsHandle)) then
      if (present(isPresent)) then
        isPresent = .false.
      elseif (present(rc)) then
        rc = ESMF_RC_NOT_FOUND
      else
        localrc = ESMF_RC_NOT_FOUND
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      endif
      return
    endif

    currHandle => fieldsHandle

    do while (.true.)

      call MOSSCO_FieldString(currHandle%srcField, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)

      if (currHandle%srcField == field) then
        if (present(isPresent)) isPresent = .true.
        if (present(handle)) handle = currHandle
        return
      endif

      if (.not.associated(currHandle%next)) exit
      currHandle => currHandle%next
    enddo

    if (present(isPresent)) then
      isPresent = .false.
    elseif (present(rc)) then
      rc = ESMF_RC_NOT_FOUND
    else
      localrc = ESMF_RC_NOT_FOUND
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

  end subroutine  MOSSCO_FieldInFieldsHandle

end module regrid_coupler

#undef UNITTESTS
#ifdef UNITTESTS
#undef  ESMF_METHOD
#define ESMF_METHOD "test"
program test

  use esmf
  use regrid_coupler, only : SetServices

  implicit none

  type(ESMF_State) :: states(2)
  type(ESMF_Field) :: fields(9)
  type(ESMF_Grid)  :: grid34,grid55,grid565
  integer(ESMF_KIND_I4) :: i, rc
  type(ESMF_CplComp) :: coupler
  type(ESMF_Clock)   :: clock
  type(ESMF_TimeInterval) :: timeInterval
  type(ESMF_Time)         :: time
  integer :: localrc

  call ESMF_Initialize(defaultLogFileName="test_regrid_coupler", &
    logkindflag=ESMF_LOGKIND_MULTI,defaultCalKind=ESMF_CALKIND_GREGORIAN)

  !! Initialize
  do i=1,2
   states(i)=ESMF_StateCreate()
  enddo

  call ESMF_TimeSet(time, yy=2014)
  call ESMF_TimeSyncToRealTime(time,rc=localrc)
  call ESMF_TimeIntervalSet(timeInterval, d=1)
  clock=ESMF_ClockCreate(timeInterval, time)

  grid34=ESMF_GridCreate(maxIndex=(/3,4/))
  grid55=ESMF_GridCreate(maxIndex=(/5,5/))
  grid565=ESMF_GridCreate(maxIndex=(/5,6,5/))


  fields(1)=ESMF_FieldCreate(grid34, typekind=ESMF_TYPEKIND_R8, name="field1")
  fields(2)=ESMF_FieldCreate(grid34, typekind=ESMF_TYPEKIND_R8, name="field2")
  fields(3)=ESMF_FieldCreate(grid34, typekind=ESMF_TYPEKIND_R8, name="field3")
  fields(4)=ESMF_FieldCreate(grid55, typekind=ESMF_TYPEKIND_R8, name="field4")
  fields(5)=ESMF_FieldCreate(grid565, typekind=ESMF_TYPEKIND_R8, name="field5")

  !! Identical field
  fields(6)=ESMF_FieldCreate(grid34, typekind=ESMF_TYPEKIND_R8, name="field1")

  !! Same name, different grid, same rank
  fields(7)=ESMF_FieldCreate(grid55, typekind=ESMF_TYPEKIND_R8, name="field2")

  !! Same name, same grid, different type
  fields(8)=ESMF_FieldCreate(grid34, typekind=ESMF_TYPEKIND_I8, name="field3")

  !! Same name, different rank
  fields(9)=ESMF_FieldCreate(grid565, typekind=ESMF_TYPEKIND_I8, name="field4")


  coupler=ESMF_CplCompCreate(name="coupler")
  call ESMF_CplCompSetServices(coupler, SetServices, rc=localrc)

  !! Run tests

  call ESMF_StateAdd(states(1),fields(1:5))
  call ESMF_StateAdd(states(2),fields(6:9))

  call ESMF_StatePrint(states(1))
  call ESMF_StatePrint(states(2))

  call ESMF_CplCompInitialize(coupler, importState=states(1), exportState=states(2), clock=clock)
  !call ESMF_CplCompRun(coupler, importState=states(1), exportState=states(2))

  !! Cleanup
  do i=1,ubound(fields,1)
    call ESMF_FieldDestroy(fields(i))
  enddo

  do i=1,ubound(states,1)
    call ESMF_StateDestroy(states(i))
  enddo

  call ESMF_GridDestroy(grid34)
  call ESMF_GridDestroy(grid55)
  call ESMF_GridDestroy(grid565)

  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end program test
#endif
