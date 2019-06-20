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

#define RANGE1D lbnd(1):ubnd(1)
#define RANGE2D RANGE1D,lbnd(2):ubnd(2)
#define RANGE3D RANGE2D,lbnd(3):ubnd(3)
#define RANGE4D RANGE3D,lbnd(4):ubnd(4)

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#define _MOSSCO_LINE_ call ESMF_LogWrite('',ESMF_LOGMSG_WARNING, ESMF_CONTEXT)

module regrid_coupler

  use esmf
  use mossco_state
  use mossco_field
  use mossco_component
  use mossco_config
  use mossco_netcdf
  use mossco_locstream
  use mossco_routehandle
  use mossco_attribute
  use mossco_grid
  use mossco_strings
  use mossco_geom

  implicit none

  private

  public SetServices

  type type_mossco_routes
    integer(ESMF_KIND_I4)         :: id=-1
    character(len=ESMF_MAXSTR)    :: creator=''
    character(len=ESMF_MAXSTR)    :: name=''
    type(ESMF_RouteHandle)        :: routeHandle
    type(ESMF_RegridMethod_Flag)  :: regridMethod
    type(ESMF_Grid)               :: srcGrid, dstGrid
    type(ESMF_Locstream)          :: srcLocstream, dstLocstream
    type(ESMF_Mesh)               :: srcMesh, dstMesh
    type(ESMF_XGrid)              :: srcXGrid, dstXGrid
    type(ESMF_GeomType_Flag)      :: srcGeomType, dstGeomType
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

    use ESMF_PointListMod

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentclock
    integer, intent(out) :: rc

    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: petCount, localPet, i, j=0, itemCount, m, k
    character(len=ESMF_MAXSTR)  :: message, name, timeString, exportName, importName
    character(len=ESMF_MAXSTR)  :: exportFieldName, importFieldName
    type(ESMF_RouteHandle)      :: routeHandle
    type(type_mossco_routes), pointer :: geomRoute=>null()
    type(ESMF_Field)            :: importField, exportField
    integer                     :: localrc

    integer(ESMF_KIND_I4)       :: rank, localDeCount
    type(ESMF_FieldStatus_Flag) :: status
    type(ESMF_Mesh)             :: importMesh, exportMesh, externalMesh
    type(ESMF_Grid)             :: externalGrid, importGrid, exportGrid
    type(ESMF_LocStream)        :: importLocstream, exportLocstream, externalLocStream
    type(ESMF_GeomType_Flag)    :: importGeomType, exportGeomType
    character(ESMF_MAXSTR)      :: importGeomName, exportGeomName, geomFileName
    integer                     :: numOwnedNodes, dimCount
    integer(ESMF_KIND_I4)       :: keycount, matchIndex, importFieldCount
    integer(ESMF_KIND_I4)       :: exportFieldCount, unmappedCount
    logical                     :: geomIsPresent, hasMaskVariable, isPresent

    type(ESMF_Field), allocatable :: importFieldList(:)
    type(ESMF_Field), allocatable :: exportFieldList(:)
    character(len=ESMF_MAXSTR)    :: geomFileFormatString = 'SCRIP'
    character(len=ESMF_MAXSTR)    :: geomBaseFileName
    character(len=ESMF_MAXSTR)    :: mask_variable='mask'
    character(len=ESMF_MAXSTR)    :: geomTypeString = 'GRID'
    character(len=ESMF_MAXSTR)    :: regridMethodString = 'bilinear'
    type(ESMF_RegridMethod_Flag)  :: regridMethod
    integer(ESMF_KIND_I4),pointer :: unmappedDstList(:) => null()
    type(ESMF_PointList)          :: pointList

    type(ESMF_Mesh)               :: mesh
    type(ESMF_Grid)               :: grid
    type(ESMF_LocStream)          :: locStream
    type(ESMF_XGrid)              :: xgrid
    real(ESMF_KIND_R8), pointer   :: factorList(:) => null()
    integer(ESMF_KIND_I4), allocatable :: dstMaskValues(:), srcMaskValues(:)

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> Read (optionally) the associated config file and configure
    !> external target grid, include and exclude patterns
    call read_config(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeGet(cplComp, 'geom_filename',  &
      isPresent=geomIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeGet(cplComp, 'regrid_method',  &
      regridMethodString, defaultValue='bilinear', rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_RegridMethod(regridMethod, regridMethodString, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> The get_FieldList call returns a list of ESMF_COMPLETE fields in a
    !>  state, including lists that previously were located within
    !> fieldBundles; this subroutine also considers exclusion/inclusion
    !> patterns defined in the config file
    call get_FieldList(cplComp, importState, importFieldList, verbose=.true., &
      fieldCount=importFieldCount, rc=localrc)
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

    if (geomIsPresent) then

      call ESMF_AttributeGet(cplComp, 'mask_variable',  &
        isPresent=hasMaskVariable, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (hasMaskVariable) then
        call ESMF_AttributeGet(cplComp, 'mask_variable',  &
          mask_variable, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      call ESMF_AttributeGet(cplComp, 'geom_filename',  geomFileName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      ! Remove extension (.nc)
      i=index(geomFileName,'.',back=.true.)
      geomBaseFileName = geomFileName(1:i-1)

      call ESMF_AttributeGet(cplComp, 'geom_file_format',  geomFileFormatString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeGet(cplComp, 'geom_file_type',  geomTypeString, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Convert strings to standard format strings
      call MOSSCO_CleanGeomFormatString(geomFileFormatString, rc=localrc)

      if (trim(geomFileFormatString) == 'SCRIP' .and. geomTypeString == 'GRID') then
        externalGrid = ESMF_GridCreate(filename=trim(geomFileName), &
          fileFormat=ESMF_FILEFORMAT_SCRIP, isSphere=.false., rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_GridWriteVTK(externalGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          filename=trim(geomBaseFileName), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message, '(A)') trim(name)//' created grid from SCRIP '//trim(geomFileName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      elseif (trim(geomFileFormatString) == 'GRIDSPEC') then
        if (hasMaskVariable) then

          externalGrid = ESMF_GridCreate(filename=trim(geomFileName), fileFormat=ESMF_FILEFORMAT_GRIDSPEC, &
            isSphere=.false., addmask=.true., varname=trim(mask_variable), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          write(message, '(A)') trim(name)//' created grid WITH mask ' //trim(mask_variable)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        else
          externalGrid = ESMF_GridCreate(filename=trim(geomFileName), fileFormat=ESMF_FILEFORMAT_GRIDSPEC, &
            isSphere=.false., rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          write(message, '(A)') trim(name)//' created grid WITHOUT mask '
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif

        call ESMF_GridWriteVTK(externalGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          filename=trim(geomBaseFileName), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message, '(A)') trim(name)//' created grid from CF '//trim(geomFileName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        ! if (hasMaskVariable) then
        !   call ESMF_AttributeGet(cplComp, 'mask_variable',  &
        !     mask_variable, rc=localrc)
        !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        !
        !   call ESMF_GridGet(externalGrid, rank=rank, rc=localrc)
        !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        ! endif
        !
        ! if (hasMaskVariable .and. rank==2) then
        !   call MOSSCO_GridAddMaskFromVariable(externalGrid, trim(gridFileName), &
        !     trim(mask_variable), owner=trim(name), rc=localrc)
        !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        !   write(message, '(A)') trim(name)//' added grid mask from '//trim(mask_variable)
        !   call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        ! endif

      elseif (trim(geomFileFormatString) == 'UGRID' .and. geomTypeString == 'MESH') then
        if (hasMaskVariable) then
          write(message,'(A)') trim(name)//' does not implement mask variable for UGRID'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
        !   externalMesh = ESMF_MeshCreateFromFile(trim(geomFileName), &
        !     fileformat=ESMF_FILEFORMAT_UGRID, convertToDual=.false., &
        !     maskFlag=ESMF_MESHLOC_ELEMENT, varname=trim(mask_variable),  rc=localrc)
        !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        ! else
        endif
        externalMesh = ESMF_MeshCreate(trim(geomFileName), &
          fileformat=ESMF_FILEFORMAT_UGRID, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_MeshWrite(externalMesh, trim(geomBaseFileName), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message, '(A)') trim(name)//' created from UGRID '//trim(geomFileName)
        call MOSSCO_GeomString(externalMesh, message)

        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      elseif (trim(geomFileFormatString) == 'ESMF'  .and. geomTypeString == 'MESH') then
        externalMesh = ESMF_MeshCreate(trim(geomFileName), &
          fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_MeshWrite(externalMesh, trim(geomBaseFileName), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message, '(A)') trim(name)//' created ESMF '//trim(geomFileName)
        call MOSSCO_GeomString(externalMesh, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      elseif (trim(geomFileFormatString) == 'SCRIP'  .and. geomTypeString == 'MESH') then
        externalMesh = ESMF_MeshCreate(trim(geomFileName), &
          fileformat=ESMF_FILEFORMAT_SCRIP, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_MeshWrite(externalMesh, trim(geomBaseFileName), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message, '(A)') trim(name)//' created from SCRIP '//trim(geomFileName)
        call MOSSCO_GeomString(externalMesh, message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      elseif (geomTypeString == 'LOCSTREAM') then
        ! Private name: call using ESMF_LocStreamCreate()
        !       function ESMF_LocStreamCreateFromFile(filename, &
        !            fileformat, varname, indexflag, centerflag, name, rc)
        ! For a grid in ESMF or UGRID format, it can use center coordinates
        ! or  corner coordinates. For SCRIP only center coordinates.

        if (trim(geomFileFormatString) == 'SCRIP') then
          externalLocStream = ESMF_LocStreamCreate(filename=trim(geomFileName), &
            fileformat=ESMF_FILEFORMAT_SCRIP, name=trim(geomFileName), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        elseif (trim(geomFileFormatString) == 'UGRID' .and. hasMaskVariable) then
          externalLocStream = ESMF_LocStreamCreate(filename=trim(geomFileName), &
            fileformat=ESMF_FILEFORMAT_UGRID, varname=trim(mask_variable), &
            name=trim(geomFileName), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        elseif (trim(geomFileFormatString) == 'UGRID') then
          externalLocStream = ESMF_LocStreamCreate(filename=trim(geomFileName), &
            fileformat=ESMF_FILEFORMAT_UGRID, name=trim(geomFileName), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        elseif (trim(geomFileFormatString) == 'ESMF') then
          externalLocStream = ESMF_LocStreamCreate(filename=trim(geomFileName), &
            fileformat=ESMF_FILEFORMAT_UGRID, name=trim(geomFileName), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        else
          write(message, '(A,A)') trim(name)//' unknown file format/type ' &
            ,trim(geomFileFormatString)//'/'//trim(geomTypeString)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif

        pointList=ESMF_PointListCreate(externalLocStream, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_PointListWriteVTK(pointList, trim(geomBaseFileName), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_PointListDestroy(pointList, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message, '(A)') trim(name)//' created locstream from '//trim(geomFileFormatString)//' '//trim(geomFileName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      else
        write(message, '(A,A)') trim(name)//' unknown file format/type ' &
          ,trim(geomFileFormatString)//'/'//trim(geomTypeString)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    call MOSSCO_AttributeGet(cplComp, label='src_mask', &
      list=srcMaskValues, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    write(*,*) 'srcMask=', srcMaskValues

    call MOSSCO_AttributeGet(cplComp, label='dst_mask', &
      list=dstMaskValues, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    write(*,*) 'dstMask=', dstMaskValues

    if (geomIsPresent) then

      allocate(exportFieldList(importFieldCount))

      do i=1, importFieldCount

        importField = importFieldList(i)

        call ESMF_FieldGet(importField, name=importFieldName, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        exportField = ESMF_FieldEmptyCreate(name=trim(importFieldName), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (trim(geomTypeString) == 'MESH') then
          call ESMF_FieldEmptySet(exportField, mesh=externalMesh, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        elseif (trim(geomTypeString) == 'GRID') then
          call ESMF_FieldEmptySet(exportField, grid=externalGrid, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        elseif (trim(geomTypeString) == 'LOCSTREAM') then
          call ESMF_FieldEmptySet(exportField, locstream=externalLocStream, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif

        !> Copy all attributes from the importField  and complete
        !> the new field with same typeKind
        call MOSSCO_FieldCopy(exportField, importField, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call MOSSCO_FieldInitialize(exportField, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AttributeSet(exportField, 'creator', trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_StateGet(exportState, itemSearch=trim(importFieldName), &
          itemCount=itemCount, rc=localrc)

        if (itemCount == 0) then
          write(message, '(A)') trim(name)//' created '
          call MOSSCO_FieldString(exportField, message)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        else
          !write(message, '(A)') trim(name)//' replaced '
          !call MOSSCO_FieldString(importField, message)
          !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          write(message, '(A)') trim(name)//' replace created '
          call MOSSCO_FieldString(exportField, message)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif

        call ESMF_StateAddReplace(exportState, (/exportField/), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        exportFieldList(i) = exportField
      enddo
    else ! if geomIsPresent
      call get_FieldList(cplComp, exportState, exportFieldList, verbose=.true., &
        fieldCount=exportFieldCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    if (allocated(exportFieldList)) exportFieldCount = ubound(exportFieldList,1)

    do i=1,exportFieldCount
      write(message, '(A)') trim(name)//' export item  '
      call MOSSCO_FieldString(exportFieldList(i), message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

    call ESMF_StateGet(exportState, name=exportName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateGet(importState, name=importName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)


    !> Search for all fields that are present in both import and export state,
    !! for each combination of fields
    !! - if they are defined on different grids, create a route handle and
    !!   name it with the name of the two grids for identification (todo)

    do i=1, importFieldCount

      if (.not.allocated(Routes)) then
        allocate(Routes)
        j=-1
      endif

      geomRoute=>Routes

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

      if (associated(geomRoute)) then

        call MOSSCO_RouteFindFieldPair(geomRoute, regridMethod, &
          srcField=importFieldList(i), dstField=exportField, &
          isPresent=isPresent, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      endif

      if (isPresent) then
        write(message,'(A)') trim(name)//' reuses route '
        call MOSSCO_RouteString(geomRoute, message, rc=localrc)
        call MOSSCO_MessageAdd(message, ' for fields')
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        write(message,'(A)') trim(name)//' regridding from '
        call MOSSCO_FieldString(importFieldList(i), message, rc=localrc)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        write(message,'(A)') trim(name)//' regridding to '
        call MOSSCO_FieldString(exportField, message, rc=localrc)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        cycle
      endif

      !>@todo Handle identical fields and fields on identical grids

      call ESMF_FieldGet(exportField, name=exportFieldName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(importField, geomType=importGeomType, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(exportField, geomType=exportGeomType, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Advance to the end of the route to create a new one
      geomRoute => Routes
      do while (associated(geomRoute%next))
        geomRoute=>geomRoute%next
      enddo

      write(message,'(A)') trim(name)//' field '//trim(importFieldName) &
        //' creates new route'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

#if ESMF_VERSION_MAJOR > 6 && ESMF_VERSION_MINOR > 0
      !> @todo this needs to consider masks!
      call ESMF_FieldRegridStore(srcField=importField, dstField=exportField, &
        srcMaskValues=srcMaskValues, dstMaskValues=dstMaskValues, &
        routeHandle=routehandle, &
        regridmethod=regridMethod, factorList=factorList, &
        extrapMethod=ESMF_EXTRAPMETHOD_NEAREST_IDAVG, &
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
        ! unmappedDstList=unmappedDstList, & ! ESMF internal error
            rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
#else
      !> @todo this needs to consider masks!
      call ESMF_FieldRegridStore(srcField=importField, dstField=exportField, &
        srcMaskValues=srcMaskValues, dstMaskValues=dstMaskValues, &
        routeHandle=routehandle, &
        regridmethod=regridMethod, &
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
        ! unmappedDstList=unmappedDstList, & ! ESMF internal error
        rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
#endif

      if (associated(unmappedDstList) .and. ubound(unmappedDstList,1) > 0) then
        write(message, '(A)') trim(name)//' has unmapped destination points '
        write(message,*) trim(message), unmappedDstList(:)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
        nullify(unmappedDstList)
      endif

      if (associated(factorList)) then
        if (ubound(factorList,1) < 1) then
          write(message,'(A)') trim(name)//' could not find weights'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          write(message,'(A)') trim(name)//' hint: try to specify src_mask or dst_mask in configuration file'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          cycle
        endif
        !write(*,'(A,20(X,F4.2))') 'factorList=',factorList(1:20)
      endif

      !call ESMF_FieldSMMStore(srcField=importField, dstField=exportField, &
      !  filename="weights.nc", routehandle=routehandle, rc=localrc)

      !! ESMF_FieldRegrid.F90:2018 ESMF_FieldRegridGetIwts Invalid argument
      !! - - can't currently regrid a grid       that contains a DE of width less than 2

      allocate (geomRoute%next)
      geomRoute=>geomRoute%next
      j=j+1

      if (importGeomType == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(importField, grid=grid, rc=localrc)
        geomRoute%srcGrid=grid
      elseif (importGeomType == ESMF_GEOMTYPE_MESH) then
        call ESMF_FieldGet(importField, mesh=mesh, rc=localrc)
        geomRoute%srcMesh=mesh
      elseif (importGeomType == ESMF_GEOMTYPE_LOCSTREAM) then
        call ESMF_FieldGet(importField, locstream=locStream, rc=localrc)
        geomRoute%srcLocStream=locStream
      elseif (importGeomType == ESMF_GEOMTYPE_XGRID) then
        call ESMF_FieldGet(importField, xgrid=xgrid, rc=localrc)
        geomRoute%srcXGrid=xgrid
      endif

      if (exportGeomType == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(exportField, grid=grid, rc=localrc)
        geomRoute%dstGrid=grid
      elseif (exportGeomType == ESMF_GEOMTYPE_MESH) then
        call ESMF_FieldGet(exportField, mesh=mesh, rc=localrc)
        geomRoute%dstMesh=mesh
      elseif (exportGeomType == ESMF_GEOMTYPE_LOCSTREAM) then
        call ESMF_FieldGet(exportField, locstream=locStream, rc=localrc)
        geomRoute%dstLocStream=locStream
      elseif (exportGeomType == ESMF_GEOMTYPE_XGRID) then
        call ESMF_FieldGet(exportField, xgrid=xgrid, rc=localrc)
        geomRoute%dstXGrid=xgrid
      endif

      geomRoute%routeHandle  = routeHandle
      geomRoute%regridMethod = regridMethod
      geomRoute%srcGeomType  = importGeomType
      geomRoute%dstGeomType  = exportGeomType
      write(geomRoute%creator,'(A)') trim(name)
      geomRoute%id = j

      write(message,'(A)') trim(name)//' created route '
      call MOSSCO_RouteString(geomRoute, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)


    enddo  ! loop over fields

    if (allocated(srcMaskValues)) deallocate(srcMaskValues)
    if (allocated(dstMaskValues)) deallocate(dstMaskValues)

    call MOSSCO_CompExit(cplComp, rc=localrc)
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

    type(ESMF_Time)             :: currTime, startTime
    integer(ESMF_KIND_I4)       :: petCount, localPet, i, m, j, k, matchIndex
    integer(ESMF_KIND_I4)       :: fieldCount, importFieldCount, exportFieldCount
    character(len=ESMF_MAXSTR)  :: message, name
    character(len=ESMF_MAXSTR)  :: regridMethodString
    type(type_mossco_routes), pointer :: currentRoute=>null()
    integer                       :: localrc, rank
    logical                       :: isPresent
    type(ESMF_Field), allocatable, target :: importFieldList(:), fieldList(:), exportFieldList(:)
    type(ESMF_Field)              :: exportField
    type(ESMF_RouteHandle)        :: routeHandle
    type(ESMF_RegridMethod_Flag)  :: regridMethod, currentMethod
    real(ESMF_KIND_R8), allocatable  :: farrayPtr1near(:)
    real(ESMF_KIND_R8), allocatable  :: farrayPtr2near(:,:)
    real(ESMF_KIND_R8), allocatable  :: farrayPtr3near(:,:,:)
    real(ESMF_KIND_R8), pointer      :: farrayPtr1(:) => null()
    real(ESMF_KIND_R8), pointer      :: farrayPtr2(:,:) => null()
    real(ESMF_KIND_R8), pointer      :: farrayPtr3(:,:,:) => null()
    integer(ESMF_KIND_I4), allocatable :: ubnd(:), lbnd(:)
    type(ESMF_Clock)                   :: clock
    type(ESMF_GeomType_Flag)      :: importGeomType, exportGeomType
    type(ESMF_Grid)               :: grid
    type(ESMF_XGrid)              :: xgrid
    type(ESMF_Mesh)               :: mesh
    type(ESMF_LocStream)          :: locstream
    character(len=ESMF_MAXSTR), pointer :: includeList(:) => null()

    rc = ESMF_SUCCESS
    rank = 0

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> If there are no routes, then don't do anything
    if (.not.allocated(Routes)) then
      if (startTime == currTime ) then
        write(message,'(A)') trim(name)//' has no routes'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call MOSSCO_CompExit(cplComp, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      return
    endif

    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(clock, startTime=startTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> At first time Run(), print a table of all available routes, note
    !> that this contains routes from all instances of this component, not
    !> only the ones created by this one, as Routes is a module global variable
    i = 0
    if (startTime == currTime ) then
      currentRoute => Routes
      do while(associated(currentRoute%next))
        currentRoute => currentRoute%next
        write(message,'(A)') trim(name)//' has route'
        call MOSSCO_RouteString(currentRoute, message, rc=localrc)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        i = i + 1
      enddo
    endif

    call ESMF_AttributeGet(cplComp, 'regrid_method',  &
      regridMethodString, defaultValue='bilinear', rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_RegridMethod(regridMethod, regridMethodString, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call get_FieldList(cplComp, importState, importFieldList, verbose=.false., &
      fieldCount=importFieldCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (allocated(importFieldList)) importFieldcount=ubound(importFieldList,1)

    call get_FieldList(cplComp, exportState, exportFieldList, verbose=.false., &
      fieldCount=exportFieldCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (allocated(exportFieldList)) exportFieldcount=ubound(exportFieldList,1)

    if (importFieldCount < 1) then
      !write(*,*) trim(name)//' no import fields at ',__LINE__
      return
    endif

    if (associated(includeList)) deallocate(includeList)
    allocate(includeList(1)) ! to hold field name for exportState matching

    do i=1, importFieldCount

      !> look for matching field in exportState, this matching is performed
      !> on the field name and a maximum of the attributes
      !> @todo exclude creator and coordinates attributes
      call MOSSCO_FieldMatchFields(importFieldList(i), exportFieldList, &
        index=matchIndex, owner=trim(name), verbose=.true., rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (matchIndex<1) then
        write(message,'(A)') trim(name)//' failed to find match for'
        call MOSSCO_FieldString(importFieldList(i), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
        !write(*,*) trim(name)//' match fail at ',__LINE__
        !write(*,*) exportFieldList(1:exportFieldCount)
        call MOSSCO_StateLog(exportState)
        cycle
      endif

      exportField = exportFieldList(matchIndex)

      currentRoute => Routes

      call MOSSCO_RouteFindFieldPair(currentRoute, regridMethod, &
          srcField=importFieldList(i), dstField=exportField, &
          isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (.not.associated(currentRoute)) then
          write(message,'(A)') trim(name)//' cannot find route for fields'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          write(message,'(A)') trim(name)//' regridding from '
          call MOSSCO_FieldString(importFieldList(i), message, rc=localrc)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          write(message,'(A)') trim(name)//' regridding to '
          call MOSSCO_FieldString(exportField, message, rc=localrc)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

          !write(*,*) __LINE__
          cycle
      else
          write(message,'(A)') trim(name)//' uses route '
          call MOSSCO_RouteString(currentRoute, message, rc=localrc)
      endif

      call MOSSCO_MessageAdd(message, ' for fields')
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      write(message,'(A)') trim(name)//' regridding from '
      call MOSSCO_FieldString(importFieldList(i), message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      write(message,'(A)') trim(name)//' regridding to '
      call MOSSCO_FieldString(exportField, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_FieldRegrid(srcField=importFieldList(i), dstField=exportField,&
          routeHandle=currentRoute%routehandle, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (.true.) then

        call ESMF_FieldGet(importFieldList(i), rank=rank, rc=localrc)
        if (allocated(ubnd)) deallocate(ubnd)
        if (allocated(lbnd)) deallocate(lbnd)
        allocate(ubnd(rank))
        allocate(lbnd(rank))

        if (rank == 1) then
            call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr1, &
              exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
            !write(*,'(A,10(X,F4.2))') 'srcPtr1=',farrayPtr1(1:10)
        elseif (rank == 2) then
            call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr2, &
              exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
            !write(*,'(A,10(X,F4.2))') 'srcPtr2=',farrayPtr2(3:4,4:8)
        elseif (rank == 3) then
            call ESMF_FieldGet(importFieldList(i), farrayPtr=farrayPtr3, &
              exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
            !write(*,'(A,12(X,F4.2))') 'srcPtr3=',farrayPtr3(3:4,4:5,3:5)
        endif

        call ESMF_FieldGet(exportField, rank=rank, rc=localrc)
        if (allocated(ubnd)) deallocate(ubnd)
        if (allocated(lbnd)) deallocate(lbnd)
        allocate(ubnd(rank))
        allocate(lbnd(rank))

        if (rank == 1) then
            call ESMF_FieldGet(exportField, farrayPtr=farrayPtr1, &
              exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
            !write(*,'(A,10(X,F4.2))') 'dstPtr1=',farrayPtr1(1:10)
        elseif (rank == 2) then
            call ESMF_FieldGet(exportField, farrayPtr=farrayPtr2, &
              exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
            !write(*,'(A,10(X,F4.2))') 'dstPtr2=',farrayPtr2(3:4,4:8)
        elseif (rank == 3) then
            call ESMF_FieldGet(exportField, farrayPtr=farrayPtr3, &
              exclusiveLbound=lbnd, exclusiveUbound=ubnd, rc=localrc)
            !write(*,'(A,12(X,F4.2))') 'dstPtr3=',farrayPtr3(3:4,4:5,3:5)
        endif

      endif

      !> edgeMethod is  deprecated.  If we consider to reintroduce
      !> this, we should test for (regridMethod == edgeMethod) and cycle
      !> then apply edgeMethod with
      !> call ESMF_FieldRegrid(srcField=importFieldList(i), dstField=exportField,&
      !>  routeHandle=currentRoute%routehandle, zeroregion=ESMF_REGION_SELECT, rc=localrc)
      !> and merge the prior and later fields.
      !>   where ( farrayPtr1(RANGE1D) /= farrayPtr1(RANGE1D) .and. &
      !>      farrayPtr1near(RANGE1D) == farrayPtr1near(RANGE1D))
      !>      farrayPtr1(RANGE1D) = farrayPtr1near(RANGE1D)
      !>    endwhere

    enddo ! i=1, importFieldCount

    if (associated(includeList)) deallocate(includeList)

    nullify(farrayPtr1)
    nullify(farrayPtr2)
    nullify(farrayPtr3)
    if (allocated(farrayPtr1near)) deallocate(farrayPtr1near)
    if (allocated(farrayPtr2near)) deallocate(farrayPtr2near)
    if (allocated(farrayPtr3near)) deallocate(farrayPtr3near)

    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(lbnd)) deallocate(lbnd)

    call MOSSCO_CompExit(cplComp, rc=localrc)
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

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, rc=localrc)
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

    call MOSSCO_CompExit(cplComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "get_FieldList"

  subroutine get_FieldList(cplComp, state, fieldList, kwe, fieldCount, verbose, rc)

    implicit none

    type(ESMF_CplComp), intent(inout)                :: cplComp
    type(ESMF_State), intent(inout)                  :: state
    type(ESMF_Field), allocatable, dimension(:)      :: fieldList
    type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
    logical, intent(in), optional                    :: verbose
    integer(ESMF_KIND_I4), intent(out), optional     :: fieldCount
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc, fieldcount_
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

    call MOSSCO_StateGet(state, fieldList, fieldCount=fieldCount_, &
        fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), include=filterIncludeList, &
        exclude=filterExcludeList, verbose=verbose_, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (present(fieldCount)) fieldCount = fieldCount_

  end subroutine get_FieldList

#undef  ESMF_METHOD
#define ESMF_METHOD "read_config"

  subroutine read_config(cplComp, kwe, rc)

    implicit none

    type(ESMF_CplComp), intent(inout)    :: cplComp
    type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)             :: rc_, localRc
    character(len=ESMF_MAXSTR)        :: configFileName, srcgeomFileName, message
    character(len=ESMF_MAXSTR)        :: cplCompName, dstgeomFileName
    logical                           :: labelIsPresent, fileIsPresent
    logical                           :: configIsPresent, configFileIsPresent
    type(ESMF_Config)                 :: config
    character(len=ESMF_MAXSTR), pointer :: filterExcludeList(:) => null()
    character(len=ESMF_MAXSTR), pointer :: filterIncludeList(:) => null()
    character(len=ESMF_MAXSTR)        :: edgeMethodString, regridMethodString

    character(len=ESMF_MAXSTR)         :: geomFileFormatString = 'SCRIP'
    character(len=ESMF_MAXSTR)         :: geomTypeString = 'GRID'
    character(len=ESMF_MAXSTR)         :: mask_variable
    logical                            :: extrapolate = .true.
    integer(ESMF_KIND_I4), allocatable :: maskValues(:)

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

    call MOSSCO_ConfigGet(config, label='format', value=geomFileFormatString, &
      defaultValue='SCRIP', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A)') trim(cplCompName)// ' found config item format = '//trim(geomFileFormatString)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_ConfigGet(config, label='type', value=geomTypeString, &
      defaultValue='GRID', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A)') trim(cplCompName)// ' found config item type = '//trim(geomFileFormatString)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_ConfigGet(config, label='grid', value=dstgeomFileName, &
      defaultValue=trim(cplCompName)//'_grid.nc', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A)') trim(cplCompName)// ' found config item grid = '//trim(dstgeomFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(cplComp, 'geom_filename', trim(dstgeomFileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      call ESMF_AttributeSet(cplComp, 'geom_file_format', trim(geomFileFormatString), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      !> Overrides from file format
      if (trim(geomFileFormatString) == 'GRIDSPEC' .and. labelIsPresent) then
        geomTypeString='GRID'
      elseif (.not.labelIsPresent .and. (trim(geomFileFormatString) == 'ESMF' &
        .or. trim(geomFileFormatString) == 'UGRID')) then
        geomTypeString='MESH'
      elseif (trim(geomTypeString) == 'GRID' .and. (trim(geomFileFormatString) == 'ESMF' &
        .or. trim(geomFileFormatString) == 'UGRID')) then
        geomTypeString='MESH'
      endif

      call ESMF_AttributeSet(cplComp, 'geom_file_type', trim(geomTypeString), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    endif

    call MOSSCO_ConfigGet(config, label='mask', value=mask_variable, &
      defaultValue='mask', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (labelIsPresent) then
      write(message,'(A)') trim(cplCompName)//' found mask = '//trim(mask_variable)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(cplComp, 'mask_variable', trim(mask_variable), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_ConfigGet(config, label='method', value=regridMethodString, &
      defaultValue='bilinear', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (labelIsPresent) then
      write(message,'(A)') trim(cplCompName)//' found method = '//trim(regridMethodString)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(cplComp, 'regrid_method', trim(regridMethodString), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_ConfigGet(config, label='edge_method', value=edgeMethodString, &
    defaultValue='stod', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (labelIsPresent) then
      write(message,'(A)') trim(cplCompName)//' found edge_method = '//trim(edgeMethodString)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(cplComp, 'edge_method', trim(edgeMethodString), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    ! call MOSSCO_ConfigGet(config, label='extrapolate', value=extrapolate, &
    ! defaultValue=.true., isPresent=labelIsPresent, rc = localrc)
    ! _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    ! if (labelIsPresent) then
    !   write(message,'(A,L1)') trim(cplCompName)//' found extrapolate = ',extrapolate
    !   call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    !   call MOSSCO_AttributeSet(cplComp, 'extrapolate', extrapolate, rc=localrc)
    !   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    ! endif

    !> Find out whether the label was specified.  If yes, then require
    !> the file to be present, and return if not found
    inquire(file=trim(dstgeomFileName), exist=fileIsPresent)

    if (labelIsPresent .and..not. fileIsPresent) then
      write(message, '(A)') trim(cplCompName)//' cannot find '//trim(dstgeomFileName)
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

    call MOSSCO_ConfigGet(config, label='srcMask', value=maskValues, &
      isPresent=labelIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      !write(message,'(A,I2)') trim(cplCompName)// ' found config item srcMask= !',maskValues
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_AttributeSet(cplComp, label='src_mask', list=maskValues, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='dstMask', value=maskValues, &
      isPresent=labelIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      !write(message,'(A,I2)') trim(cplCompName)// ' found config item dstMask= ',maskValues
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_AttributeSet(cplComp, label='dst_mask', list=maskValues, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  end subroutine read_config

#undef ESMF_METHOD
#define ESMF_METHOD "MOSSCO_RouteFindFieldPair"
  subroutine MOSSCO_RouteFindFieldPair(routePtr, regridMethod, srcField, dstField, &
    kwe, isPresent, owner, rc)

    type(type_mossco_routes), intent(inout), pointer  :: routePtr
    type(ESMF_RegridMethod_Flag), intent(in)          :: regridMethod
    type(ESMF_Field), intent(in)                      :: srcField, dstField

    type(ESMF_KeyWordEnforcer), intent(in), optional  :: kwe
    character(len=*), intent(in), optional            :: owner
    logical, intent(out), optional                    :: isPresent
    integer(ESMF_KIND_I4), intent(out), optional      :: rc

    integer(ESMF_KIND_I4)                    :: rc_, localrc, i
    logical                                  :: isPresent_
    character(len=ESMF_MAXSTR)               :: message, owner_

    type(ESMF_Grid)      :: srcGrid, dstGrid
    type(ESMF_XGrid)     :: srcXGrid, dstXGrid
    type(ESMF_LocStream) :: srcLocStream, dstLocStream
    type(ESMF_Mesh)      :: srcMesh, dstMesh
    type(ESMF_GeomType_Flag) :: srcGeomType, dstGeomType

    message = ''
    owner_ = '--'
    rc_ = ESMF_SUCCESS
    isPresent_ = .false.
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    if (present(owner)) call MOSSCO_StringCopy(owner_, owner, rc=localrc)
    if (.not.associated(routePtr)) return

    call ESMF_FieldGet(srcField, geomType=srcGeomType, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (srcGeomType == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(srcField, grid=srcGrid, rc=localrc)
    elseif (srcGeomType == ESMF_GEOMTYPE_MESH) then
      call ESMF_FieldGet(srcField, mesh=srcMesh, rc=localrc)
    elseif (srcGeomType == ESMF_GEOMTYPE_LOCSTREAM) then
      call ESMF_FieldGet(srcField, locstream=srcLocstream, rc=localrc)
    elseif (srcGeomType == ESMF_GEOMTYPE_XGRID) then
      call ESMF_FieldGet(srcField, xgrid=srcXGrid, rc=localrc)
    endif
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_FieldGet(dstField, geomType=dstGeomType, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (dstGeomType == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(dstField, grid=dstGrid, rc=localrc)
    elseif (dstGeomType == ESMF_GEOMTYPE_MESH) then
      call ESMF_FieldGet(dstField, mesh=dstMesh, rc=localrc)
    elseif (dstGeomType == ESMF_GEOMTYPE_LOCSTREAM) then
      call ESMF_FieldGet(dstField, locstream=dstLocstream, rc=localrc)
    elseif (dstGeomType == ESMF_GEOMTYPE_XGRID) then
      call ESMF_FieldGet(dstField, xgrid=dstXGrid, rc=localrc)
    endif
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    do while (associated(routePtr%next))
      routePtr => routePtr%next

      if (routePtr%regridMethod /= regridMethod) cycle
      if (routePtr%srcGeomType /= srcGeomType) cycle
      if (routePtr%dstGeomType /= dstGeomType) cycle

      if (srcGeomType == ESMF_GEOMTYPE_GRID) then
        if (routePtr%srcGrid /= srcGrid) cycle
      elseif (srcGeomType == ESMF_GEOMTYPE_MESH) then
        if (routePtr%srcMesh /= srcMesh) cycle
      elseif (srcGeomType == ESMF_GEOMTYPE_LOCSTREAM) then
        if (routePtr%srcLocStream /= srcLocStream) cycle
      elseif (srcGeomType == ESMF_GEOMTYPE_XGRID) then
        if (routePtr%srcXGrid /= srcXGrid) cycle
      endif

      if (dstGeomType == ESMF_GEOMTYPE_GRID) then
        if (routePtr%dstGrid /= dstGrid) cycle
      elseif (dstGeomType == ESMF_GEOMTYPE_MESH) then
        if (routePtr%dstMesh /= dstMesh) cycle
      elseif (dstGeomType == ESMF_GEOMTYPE_LOCSTREAM) then
        if (routePtr%dstLocStream /= dstLocStream) cycle
      elseif (dstGeomType == ESMF_GEOMTYPE_XGRID) then
        if (routePtr%dstXGrid /= dstXGrid) cycle
      endif

      isPresent_ = .true.
      exit
    enddo

    if (.not.isPresent_) nullify(routePtr)
    if (present(isPresent)) isPresent = isPresent_

  end subroutine MOSSCO_RouteFindFieldPair

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

#undef ESMF_METHOD
#define ESMF_METHOD MOSSCO_RouteString

recursive subroutine MOSSCO_RouteString(route, string, kwe, rc)

  type(type_mossco_routes), pointer, intent(in) :: route
  character(len=*), intent(inout)               :: string
  type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
  integer(ESMF_KIND_I4), intent(out), optional     :: rc

  integer(ESMF_KIND_I4)           :: localrc, rc_
  character(len=ESMF_MAXSTR)      :: formatString

  rc_ = ESMF_SUCCESS
  if (present(kwe)) rc_ = ESMF_SUCCESS
  if (present(rc))  rc = rc_
  if (.not.associated(route)) return

  write(formatString,'(A)') '(A,'//trim(intformat(route%id))//')'
  write(string,formatString) trim(string)//' ',route%id

  if (len_trim(route%creator)>0) then
    call MOSSCO_MessageAdd(string,' ['//trim(route%creator)//']')
  endif

  if (route%srcGeomType == ESMF_GEOMTYPE_GRID) then
    call MOSSCO_GeomString(route%srcGrid, string, rc=localrc)
  elseif (route%srcGeomType == ESMF_GEOMTYPE_XGRID) then
    call MOSSCO_MessageAdd(string,' (xgrid) ', rc=localrc)
  elseif (route%srcGeomType == ESMF_GEOMTYPE_MESH) then
    call MOSSCO_GeomString(route%srcMesh, string, rc=localrc)
  elseif (route%srcGeomType == ESMF_GEOMTYPE_LOCSTREAM) then
    call MOSSCO_GeomString(route%srcLocStream, string, rc=localrc)
  else
    call MOSSCO_MessageAdd(string,' (unknown) ', rc=localrc)
  endif
  !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  call MOSSCO_MessageAdd(string,' --')
  if (route%regridMethod == ESMF_REGRIDMETHOD_BILINEAR) then
    call MOSSCO_MessageAdd(string,'BILIN--> ', rc=localrc)
  elseif (route%regridMethod == ESMF_REGRIDMETHOD_NEAREST_DTOS) then
    call MOSSCO_MessageAdd(string,'NDTOS--> ', rc=localrc)
  elseif (route%regridMethod == ESMF_REGRIDMETHOD_NEAREST_STOD) then
    call MOSSCO_MessageAdd(string,'NSTOD--> ', rc=localrc)
  elseif (route%regridMethod == ESMF_REGRIDMETHOD_PATCH) then
    call MOSSCO_MessageAdd(string,'PATCH--> ', rc=localrc)
  elseif (route%regridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
    call MOSSCO_MessageAdd(string,'CONSV--> ', rc=localrc)
  else
    call MOSSCO_MessageAdd(string,'OTHER--> ', rc=localrc)
  endif
  if (route%dstGeomType == ESMF_GEOMTYPE_GRID) then
    call MOSSCO_GeomString(route%dstGrid, string, rc=localrc)
  elseif (route%dstGeomType == ESMF_GEOMTYPE_XGRID) then
    call MOSSCO_MessageAdd(string,' (xgrid) ', rc=localrc)
  elseif (route%dstGeomType == ESMF_GEOMTYPE_MESH) then
    call MOSSCO_GeomString(route%dstMesh, string, rc=localrc)
  elseif (route%dstGeomType == ESMF_GEOMTYPE_LOCSTREAM) then
    call MOSSCO_GeomString(route%dstLocStream, string, rc=localrc)
  else
    call MOSSCO_MessageAdd(string,' (unknown) ', rc=localrc)
  endif

end subroutine MOSSCO_RouteString

#undef ESMF_METHOD
#define ESMF_METHOD MOSSCO_RegridMethod

recursive subroutine MOSSCO_RegridMethod(regridMethod, string, kwe, rc)

  type(ESMF_RegridMethod_Flag), intent(out)   :: regridMethod
  character(len=*), intent(in)                :: string
  type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
  integer(ESMF_KIND_I4), optional, intent(out)     :: rc

  integer(ESMF_KIND_I4)        :: rc_
  character(ESMF_MAXSTR)       :: message

  if (present(kwe)) rc_ = ESMF_SUCCESS
  if (present(rc))  rc = ESMF_SUCCESS

  select case (trim(string))
  case ('bilinear', 'BILINEAR', 'ESMF_REGRIDMETHOD_BILINEAR')
    regridMethod = ESMF_REGRIDMETHOD_BILINEAR
  case ('dtos', 'DTOS', 'ESMF_REGRIDMETHOD_NEAREST_DTOS', 'nearest_dtos')
    regridMethod = ESMF_REGRIDMETHOD_NEAREST_DTOS
  case ('stod', 'STOD', 'ESMF_REGRIDMETHOD_NEAREST_STOD', 'nearest_stod')
    regridMethod = ESMF_REGRIDMETHOD_NEAREST_STOD
  case ('patch', 'PATCH', 'ESMF_REGRIDMETHOD_PATCH')
    regridMethod = ESMF_REGRIDMETHOD_PATCH
  case ('conserve', 'CONSERVE', 'ESMF_REGRIDMETHOD_CONSERVE')
    regridMethod = ESMF_REGRIDMETHOD_CONSERVE
  case default
    write(message,'(A)') '-- cannot interpret string '//trim(string)//' as regrid method.'
    !> @todo what's wrong here: call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
    if (present(rc))  rc = ESMF_RC_NOT_FOUND
  end select

end subroutine MOSSCO_RegridMethod

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
