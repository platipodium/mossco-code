!> @brief Implementation of an ESMF coupling between different grids
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2017, 2018 Helmholtz-Zentrum Geesthacht
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
#define _MOSSCO_LINE_ call ESMF_LogWrite('',ESMF_LOGMSG_WARNING, ESMF_CONTEXT)

module regrid_coupler

  use esmf
  use mossco_state
  use mossco_field
  use mossco_component
  use mossco_config

  implicit none

  private

  public SetServices

  type type_mossco_routes
    type(ESMF_RouteHandle)        :: routehandle
    type(ESMF_RegridMethod_Flag)  :: regridMethod
    type(ESMF_Field)              :: srcField, dstField
    type(ESMF_Grid)               :: srcGrid, dstGrid
    type(ESMF_Locstream)          :: srcLocstream, dstLocstream
    type(ESMF_Mesh)               :: srcMesh, dstMesh
    type(type_mossco_routes), pointer :: next=>null()

  end type

  ! This is a module-globale variable that is accessible
  ! across all instances of this coupler.  This way, existing
  ! routehandles can be shared across instances
  type(type_mossco_routes), allocatable, target :: Routes

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
    integer(ESMF_KIND_I4)       :: petCount, localPet, i, itemCount, m
    character(len=ESMF_MAXSTR)  :: message, name, timeString, exportName, importName
    character(len=ESMF_MAXSTR)  :: exportFieldName, importFieldName
    type(ESMF_RouteHandle)      :: routeHandle
    type(type_mossco_routes), pointer :: fieldRoute=>null()
    type(type_mossco_routes), pointer :: geomRoute=>null()
    type(ESMF_Field)            :: importField, exportField
    integer                     :: localrc

    integer(ESMF_KIND_I4)       :: rank, localDeCount
    type(ESMF_FieldStatus_Flag) :: status
    type(ESMF_Mesh)             :: importMesh, exportMesh
    type(ESMF_Grid)             :: externalGrid, importGrid, exportGrid
    type(ESMF_LocStream)        :: importLocstream, exportLocstream
    type(ESMF_GeomType_Flag)    :: importGeomType, exportGeomType
    character(ESMF_MAXSTR)      :: importGeomName, exportGeomName, gridFileName
    integer                     :: numOwnedNodes, dimCount
    integer(ESMF_KIND_I4)       :: keycount, matchIndex, importFieldCount
    integer(ESMF_KIND_I4)       :: exportFieldCount
    logical                     :: gridIsPresent, isPresent

    type(ESMF_Field), allocatable :: importFieldList(:)
    type(ESMF_Field), allocatable :: exportFieldList(:)
    character(len=ESMF_MAXSTR)    :: gridFileFormatString = 'SCRIP'
    type(ESMF_RegridMethod_Flag)  :: regridMethod, currentMethod

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

    call ESMF_StateGet(importState, name=importName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (importFieldCount < 1) then
      write(message,'(A)') trim(name)//' no couplable items in '//trim(importName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    endif

    if (gridIsPresent) then

      call ESMF_AttributeGet(cplComp, 'grid_filename',  gridFileName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeGet(cplComp, 'grid_file_format',  gridFileFormatString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (trim(gridFileFormatString) == 'SCRIP') then
        externalGrid = ESMF_GridCreate(filename=trim(gridFileName), &
          fileFormat=ESMF_FILEFORMAT_SCRIP, isSphere=.false., rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      elseif (trim(gridFileFormatString) == 'GRIDSPEC') then
        externalGrid = ESMF_GridCreate(filename=trim(gridFileName), fileFormat=ESMF_FILEFORMAT_GRIDSPEC, &
          isSphere=.false., rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      else
        write(message, '(A)') trim(name)//' unknown file format '//trim(gridFileFormatString)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    if (gridIsPresent) then

      allocate(exportFieldList(importFieldCount))

      do i=1, importFieldCount

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
        geomType=importGeomType, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (importGeomType == ESMF_GEOMTYPE_GRID) then

          call ESMF_FieldGet(importField, grid=importGrid, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_GridGet(importGrid, dimCount=dimCount, rank=rank, name=importGeomName, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A,I1,A,I1,A,I1)') trim(name)//' grid '//trim(importGeomName)//' rank ',rank,' dimensions ',dimCount

      elseif (importGeomType == ESMF_GEOMTYPE_MESH) then

          call ESMF_FieldGet(importField, mesh=importMesh, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_MeshGet(importMesh, numOwnedNodes=numOwnedNodes, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A,I5,A)') trim(name)//' mesh with ',numOwnedNodes,' nodes'
          importGeomName='anonymousMesh'

      elseif (importGeomType == ESMF_GEOMTYPE_LOCSTREAM) then

          call ESMF_FieldGet(importField, locstream=importLocstream, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_LocStreamGet(importLocstream, keycount=keycount, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A,I5,A)') trim(name)//' locstream with ',keycount,' keycount'
          importGeomName='anonymousLocStream'

      else

          write(message,'(A)') trim(name)//' other geomtype, skipped'
          cycle
      endif

      call ESMF_FieldGet(exportField, status=status, localDeCount=localDeCount, rank=rank, &
        geomType=exportGeomType, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (exportGeomType == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(exportField, grid=exportGrid, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_GridGet(exportGrid, dimCount=dimCount, rank=rank, name=exportGeomName, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message,'(A,I1,A,I1,A,I1)') trim(message)//' --> grid '//trim(exportGeomName)//' rank ',rank,' dimensions ',dimCount

      elseif (exportGeomType == ESMF_GEOMTYPE_MESH) then

        call ESMF_FieldGet(exportField, mesh=exportMesh, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_MeshGet(exportMesh, numOwnedNodes=numOwnedNodes, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        importGeomName='anonymousMesh'
        write(message,'(A,I5,A)') trim(message)//' --> mesh with ',numOwnedNodes,' nodes.'

      elseif (exportGeomType == ESMF_GEOMTYPE_LOCSTREAM) then

          call ESMF_FieldGet(exportField, locstream=exportLocstream, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_LocStreamGet(exportLocstream, keycount=keycount, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          importGeomName='anonymousLocStream'
          write(message,'(A,I5,A)') trim(name)//' locstream with ',keycount,' keys'

      else
        write(message,'(A)') trim(name)//' --> other geomtype skipped'
        cycle
      endif
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      ! look to see if this field pair already has a routehandle
      !! search for the correct routeHandle
      if (.not.associated(fieldRoute)) then
        allocate(Routes)
      endif

      fieldRoute=>Routes

      !> @todo loop over multiple methods, at least the DTOS
      do m = 1,2

        if ( m==1 ) currentMethod = ESMF_REGRIDMETHOD_BILINEAR
        if ( m==2 ) currentMethod = ESMF_REGRIDMETHOD_NEAREST_DTOS !better STOD?

      !> Field pair / Method matching
      do while (associated(fieldRoute%next))
        fieldRoute=>fieldRoute%next
        if (fieldRoute%srcField == importField  &
          .and. fieldRoute%dstField == exportField &
          .and. fieldRoute%regridMethod == currentMethod) exit
      enddo

      ! this field pair has not already been "handled"
      if (.not. associated(fieldRoute%next)) then

        ! See whether the GeomPair has already been handled and use
        ! that routhandle if found, otherwise create new routehandle
        geomRoute => Routes

        do while (associated(geomRoute%next))
          geomRoute=>geomRoute%next
          if ( (regridMethod == geomRoute%regridMethod) .and. ( &
            (importGeomType == ESMF_GEOMTYPE_GRID .and. geomRoute%srcGrid==importGrid) &
            .or. (importGeomType == ESMF_GEOMTYPE_MESH .and. geomRoute%srcMesh==importMesh) &
            .or. (importGeomType == ESMF_GEOMTYPE_LOCSTREAM .and. geomRoute%srcLocstream==importLocstream) &
          ) .and. ( &
            (exportGeomType == ESMF_GEOMTYPE_GRID .and. geomRoute%srcGrid==exportGrid) &
            .or. (exportGeomType == ESMF_GEOMTYPE_MESH .and. geomRoute%srcMesh==exportMesh) &
            .or. (exportGeomType == ESMF_GEOMTYPE_LOCSTREAM .and. geomRoute%srcLocstream==exportLocstream) &
          )) then

            write(message,'(A)') trim(name)//' field '//trim(importFieldName) &
              //' has route '//trim(importGeomName)//' --> '//trim(exportGeomName)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

            exit
          endif
        enddo

        ! This geom pair has not been routed
        if (.not. associated(geomRoute%next)) then

          write(message,'(A)') trim(name)//' field '//trim(importFieldName) &
            //' creating routeHandle'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          !> @todo this needs to consider masks!
          call ESMF_FieldRegridStore(srcField=importField, dstField=exportField, &
            !filename="weights.nc", &!routeHandle=routehandle, &
            srcMaskValues=(/1,2/), dstMaskValues=(/1,2/), &
            routeHandle=routehandle, &
            regridmethod=currentMethod, &
            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          !call ESMF_FieldSMMStore(srcField=importField, dstField=exportField, &
          !  filename="weights.nc", routehandle=routehandle, rc=localrc)

          write(message,'(A)') trim(name)//' field '//trim(importFieldName) &
            //' created routeHandle'

        else
          routeHandle = geomRoute%routehandle
          write(message,'(A)') trim(name)//' field '//trim(importFieldName) &
            //' reused routeHandle'
        endif
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          !! ESMF_FieldRegrid.F90:2018 ESMF_FieldRegridGetIwts Invalid argument
          !! - - can't currently regrid a grid       that contains a DE of width less than 2

        allocate (fieldRoute%next)
        fieldRoute=>fieldRoute%next

        if (importGeomType == ESMF_GEOMTYPE_GRID) fieldRoute%srcGrid=importGrid
        if (exportGeomType == ESMF_GEOMTYPE_GRID) fieldRoute%dstGrid=exportGrid
        if (importGeomType == ESMF_GEOMTYPE_MESH) fieldRoute%srcMesh=importMesh
        if (exportGeomType == ESMF_GEOMTYPE_MESH) fieldRoute%dstMesh=exportMesh
        if (importGeomType == ESMF_GEOMTYPE_LOCSTREAM) fieldRoute%srcLocStream=importLocStream
        if (exportGeomType == ESMF_GEOMTYPE_LOCSTREAM) fieldRoute%dstLocStream=exportLocStream
        fieldRoute%srcField=importField
        fieldRoute%dstField=exportField
        fieldRoute%routeHandle=routeHandle
        fieldRoute%regridMethod=currentMethod

        !@todo RouteHandlePrint creates a SIGILL Illegal instruction error
        !call ESMF_RouteHandlePrint(routehandle, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      ! this field pair has already been "handled", continue!
      else
        write(message,'(A)') trim(name)//' field '//trim(importFieldName) &
          //' has already been handled, skipping.'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

        ! generic list cycling code..
        ! do while(associated(currentRoute%next))
        !   currentRoute=>currentRoute%next
        ! enddo
      enddo ! loop over methods
    enddo  ! loop over fields

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
    integer(ESMF_KIND_I4)       :: petCount, localPet, i, m, importFieldCount
    character(len=ESMF_MAXSTR)  :: message, name, regridMethodString
    type(type_mossco_routes), pointer :: currentRoute=>null()
    integer                       :: localrc, rank
    logical                       :: isPresent
    type(ESMF_Field), allocatable :: importFieldList(:)
    type(ESMF_Field)              :: exportField
    type(ESMF_RouteHandle)        :: routeHandle
    type(ESMF_RegridMethod_Flag)  :: regridMethod
    real(ESMF_KIND_R8), pointer   :: farrayPtr2near(:,:) => null()
    real(ESMF_KIND_R8), pointer   :: farrayPtr3near(:,:,:) => null()
    real(ESMF_KIND_R8), pointer   :: farrayPtr2(:,:) => null()
    real(ESMF_KIND_R8), pointer   :: farrayPtr3(:,:,:) => null()
    integer(ESMF_KIND_I4), allocatable :: ubnd(:), lbnd(:)

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call get_FieldList(cplComp, importState, importFieldList, verbose=.false., &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    importFieldCount = 0
    if (allocated(importFieldList)) importFieldCount = ubound(importFieldList,1)

    do i=1, importFieldCount

      ! Do the regridding twice, once for a nearest methods (need to choose one, for edges)
      ! and later one for the actual desired one.  Then, console both regridded fields
      ! to have a complete data set on destination grid
      regridMethod = ESMF_REGRIDMETHOD_NEAREST_DTOS
      regridMethodString = 'nearest_dtos'

      call MOSSCO_FieldInRoutes(Routes, importFieldList(i), regridMethod, &
        isPresent=isPresent, dstField=exportField, routeHandle=routeHandle, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (.not.isPresent) then
        write(message,'(A)') trim(name)//' found no route '//trim(regridMethodString)//' regrid '
        call MOSSCO_FieldString(importFieldList(i), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        cycle
      endif

      write(message,'(A)') trim(name)//' '//trim(regridMethodString)//' regrid '
      call MOSSCO_FieldString(importFieldList(i), message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      write(message,'(A)') trim(name)//' onto '
      call MOSSCO_FieldString(exportField, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_FieldRegrid(srcField=importFieldList(i), dstField=exportField,&
        routeHandle=routehandle, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(exportField, rank=rank, rc=localrc)
      allocate(ubnd(rank))
      allocate(lbnd(rank))

      if (rank == 2) then
        call ESMF_FieldGet(exportField, farrayPtr=farrayPtr2near, &
          exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
      elseif (rank == 3) then
        call ESMF_FieldGet(exportField, farrayPtr=farrayPtr3near, &
          exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
      endif

      regridMethod = ESMF_REGRIDMETHOD_BILINEAR ! Choose method from config later
      regridMethodString = 'bilinear'

      call MOSSCO_FieldInRoutes(Routes, importFieldList(i), regridMethod, &
        isPresent=isPresent, dstField=exportField, routeHandle=routeHandle, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (.not.isPresent) then
        write(message,'(A)') trim(name)//' found no route '//trim(regridMethodString)//' regrid '
        call MOSSCO_FieldString(importFieldList(i), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        cycle
      endif

      write(message,'(A)') trim(name)//' '//trim(regridMethodString)//' regrid '
      call MOSSCO_FieldString(importFieldList(i), message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      write(message,'(A)') trim(name)//' onto '
      call MOSSCO_FieldString(exportField, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_FieldRegrid(srcField=importFieldList(i), dstField=exportField,&
        routeHandle=routehandle, zeroregion=ESMF_REGION_SELECT, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 2) then
        call ESMF_FieldGet(exportField, farrayPtr=farrayPtr2, &
          exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
      elseif (rank == 3) then
        call ESMF_FieldGet(exportField, farrayPtr=farrayPtr3, &
          exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
      endif

      !> @todo update values in farrayPtr with values from farrayPtrNear,
      !> where this is necessary.  Need to consider mask.

    enddo ! Loop over fields

    nullify(farrayPtr2)
    nullify(farrayPtr3)
    nullify(farrayPtr2near)
    nullify(farrayPtr3near)

    deallocate(ubnd)
    deallocate(lbnd)

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

    type(type_mossco_routes), pointer :: currentRoute=>null()
    type(type_mossco_routes), pointer :: oldHandle=>null()

    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (allocated(Routes)) then
      currentRoute => Routes
      do while (associated(currentRoute%next))
        oldHandle => currentRoute
        currentRoute=>currentRoute%next
        !if (associated(oldHandle)) deallocate(oldHandle)
      enddo
      !if (associated(currentRoute)) deallocate(currentRoute)
    endif

    !call ESMF_CplCompGet(cplComp, clockIsPresent=clockIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !if (clockIsPresent) call ESMF_ClockDestroy(clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "get_FieldList"

  subroutine get_FieldList(cplComp, state, fieldList, kwe, verbose, rc)

    implicit none

    type(ESMF_CplComp), intent(inout)                :: cplComp
    type(ESMF_State), intent(inout)                  :: state
    type(ESMF_Field), allocatable, dimension(:)      :: fieldList
    type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional                    :: verbose
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc, fieldcount
    logical                             :: configIsPresent, configFileIsPresent
    type(ESMF_Config)                   :: config
    character(len=ESMF_MAXSTR), pointer :: filterExcludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer :: filterIncludeList(:) => null()
    logical                             :: verbose_

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

    character(len=ESMF_MAXSTR)        :: gridFileFormatString = 'SCRIP'

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_CplCompGet(cplComp, configIsPresent=configIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (configIsPresent) then
      call ESMF_CplCompGet(cplComp, config=config, rc=localrc)
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

    call MOSSCO_ConfigGet(config, label='format', value=gridFileFormatString, &
      defaultValue='SCRIP', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A)') trim(cplCompName)// ' found config item format = '//trim(gridFileFormatString)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(cplComp, 'grid_file_format', trim(gridFileFormatString), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_ConfigGet(config, label='grid', value=dstGridFileName, &
      defaultValue=trim(cplCompName)//'_grid.nc', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A)') trim(cplCompName)// ' found config item grid = '//trim(dstGridFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(cplComp, 'grid_filename', trim(dstGridFileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      call ESMF_AttributeSet(cplComp, 'grid_file_format', trim(gridFileFormatString), rc=localrc)
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
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (associated(filterExcludeList)) then
      call MOSSCO_AttributeSet(cplComp, 'filter_pattern_exclude', filterExcludeList, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      deallocate(filterExcludeList)
    else
      call ESMF_AttributeSet(cplComp, 'filter_pattern_exclude', value='none', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_exclude', filterExcludeList, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    write(message,'(A)') trim(cplCompName)//' uses exclude patterns:'
    call MOSSCO_MessageAddListPtr(message, filterExcludeList, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call MOSSCO_ConfigGet(config, 'include', filterIncludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (associated(filterIncludeList)) then
      call MOSSCO_AttributeSet(cplComp, 'filter_pattern_include', filterIncludeList, localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      deallocate(filterIncludeList)
    else
      call ESMF_AttributeSet(cplComp, 'filter_pattern_include', value='*', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_include', filterIncludeList, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    write(message,'(A)') trim(cplCompName)//' uses include patterns:'
    call MOSSCO_MessageAddListPtr(message, filterIncludeList, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  end subroutine read_config

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldInRoutes"
  subroutine MOSSCO_FieldInRoutes(Routes, srcField, regridMethod, kwe, isPresent, &
    routeHandle, dstField, rc)

    type(type_mossco_routes), target                  :: Routes
    type(ESMF_RegridMethod_Flag), intent(in)          :: regridMethod
    type(ESMF_Field), intent(in)                      :: srcField
    type(ESMF_Field), intent(out), optional           :: dstField
    type(ESMF_RouteHandle), intent(out), optional     :: routeHandle
    type(ESMF_KeyWordEnforcer), intent(in), optional  :: kwe
    logical, intent(out), optional                    :: isPresent
    integer(ESMF_KIND_I4), intent(out), optional      :: rc

    integer(ESMF_KIND_I4)             :: rc_, localrc
    logical                           :: isPresent_
    type(type_mossco_routes), pointer :: currentRoute=>null()
    character(len=ESMF_MAXSTR)               :: message

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    currentRoute => Routes

    do while (associated(currentRoute%next))

      currentRoute => currentRoute%next
      message=''
      call MOSSCO_FieldString(currentRoute%srcField, message)
      !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)

      if (currentRoute%srcField == srcField .and. regridMethod==currentRoute%regridMethod) then
        if (present(isPresent)) isPresent = .true.
        if (present(routeHandle)) routeHandle=currentRoute%routeHandle
        if (present(dstField)) dstField=currentRoute%dstField
        return
      endif
    enddo

    if (present(isPresent)) then
      isPresent = .false.
    elseif (present(rc)) then
      rc = ESMF_RC_NOT_FOUND
    else
      localrc = ESMF_RC_NOT_FOUND
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

  end subroutine  MOSSCO_FieldInRoutes

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GeomPairInRoutes"
  subroutine MOSSCO_GeomPairInRoutes(routes, regridMethod, kwe, isPresent, &
    srcGrid, dstGrid, srcMesh, dstMesh, srcLocstream, dstLocstream, &
    routeHandle, srcField, dstField, rc)

    type(type_mossco_routes), target                  :: routes
    type(ESMF_RegridMethod_Flag), intent(in)          :: regridMethod

    type(ESMF_Grid), intent(in), optional             :: srcGrid, dstGrid
    type(ESMF_Mesh), intent(in), optional             :: srcMesh, dstMesh
    type(ESMF_LocStream), intent(in), optional        :: srcLocstream, dstLocStream
    type(ESMF_Field), intent(out), optional           :: srcField, dstField
    type(ESMF_RouteHandle), intent(out), optional     :: routeHandle
    type(ESMF_KeyWordEnforcer), intent(in), optional  :: kwe
    logical, intent(out), optional                    :: isPresent
    integer(ESMF_KIND_I4), intent(out), optional      :: rc

    integer(ESMF_KIND_I4)                    :: rc_, localrc, i
    logical                                  :: isPresent_
    type(type_mossco_routes), pointer        :: currentRoute=>null()
    character(len=ESMF_MAXSTR)               :: message

    message = ''
    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    currentRoute => Routes

    i=0
    if (present(srcGrid)) i=i+1
    if (present(srcMesh)) i=i+1
    if (present(srcLocStream)) i=i+1
    if (i /= 1) then
      write(message,'(A)') '-- obtained wrong number of source geoms, need exactly one'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      localrc = ESMF_RC_ARG_BAD
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    i=0
    if (present(dstGrid)) i=i+1
    if (present(dstMesh)) i=i+1
    if (present(dstLocStream)) i=i+1
    if (i /= 1) then
      write(message,'(A)') '-- obtained wrong number of destination geoms, need exactly one'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
      localrc = ESMF_RC_ARG_BAD
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    do while (associated(currentRoute%next))

      currentRoute => currentRoute%next
      if (regridMethod /= currentRoute%regridMethod) cycle

      if (present(srcGrid) .and. currentRoute%srcGrid /= srcGrid) cycle
      if (present(srcMesh) .and. currentRoute%srcMesh /= srcMesh) cycle
      if (present(srcLocStream) .and. currentRoute%srcLocStream /= srcLocStream) cycle

      if (present(dstGrid) .and. currentRoute%dstGrid /= dstGrid) cycle
      if (present(dstMesh) .and. currentRoute%dstMesh /= dstMesh) cycle
      if (present(dstLocStream) .and. currentRoute%dstLocStream /= dstLocStream) cycle

      if (present(isPresent))  isPresent = .true.
      if (present(routeHandle)) routeHandle=currentRoute%routeHandle
      return

    enddo

    if (present(isPresent)) then
      isPresent = .false.
    elseif (present(rc)) then
      rc = ESMF_RC_NOT_FOUND
    else
      localrc = ESMF_RC_NOT_FOUND
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

  end subroutine MOSSCO_GeomPairInRoutes

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
